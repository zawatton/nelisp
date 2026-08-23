;;; nelisp-m365-auth.el --- OAuth device-code auth for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Delegated OAuth against the Microsoft identity platform v2.0 endpoint
;; for *personal* Microsoft accounts.  The `consumers' tenant is what
;; makes this a Microsoft 365 Personal connector rather than a work or
;; school one: `common' would also admit Entra ID organisational
;; accounts, and `organizations' would admit only those.
;;
;; The device-code grant is used instead of an authorization-code
;; redirect because the runtime cannot listen on a socket, so there is
;; nowhere for a redirect to land.  Device code needs no listener: the
;; user types a code into a browser on any device.
;;
;; Sign-in is split across two calls.  A single blocking call would have
;; to hold an MCP request open for as long as the person takes to reach
;; a browser, so `nelisp-m365-auth-begin' returns the code immediately
;; and `nelisp-m365-auth-poll' does bounded polling afterwards, resuming
;; from the pending grant if it needs to be called again.

;;; Code:

(require 'nelisp-m365-compat)
(require 'nelisp-m365-curl)

(defvar nelisp-m365-client-id nil
  "Microsoft Entra application (client) id for this connector.
The application must be registered as a public client with personal
Microsoft accounts among its supported account types.  Set from the
generated bootstrap file; there is no default and no embedded id.")

(defvar nelisp-m365-tenant "consumers"
  "Microsoft identity tenant segment.
`consumers' restricts sign-in to personal Microsoft accounts, which is
what this connector targets.  `common' would also accept work and
school accounts.")

(defvar nelisp-m365-scopes
  '("offline_access"
    "User.Read"
    "Mail.Read"
    "Calendars.Read"
    "Files.Read"
    "Contacts.Read"
    "Tasks.Read"
    "Notes.Read")
  "Delegated scopes requested at sign-in.
All read-only apart from `offline_access', which is what buys the
refresh token.  The v2.0 endpoint consents dynamically, so scopes may be
added here without re-registering the application -- the next sign-in
just shows a fresh consent screen.")

(defvar nelisp-m365-token-cache-file nil
  "Absolute path of the JSON file holding the token cache.
Set from the generated bootstrap file.")

(defvar nelisp-m365-auth-poll-budget 90
  "Seconds `nelisp-m365-auth-poll' waits before returning `pending'.
Kept below a typical MCP client request timeout; the pending grant
survives, so the caller can simply poll again.")

(define-error 'nelisp-m365-auth-error "nelisp-m365 authentication error")

(defvar nelisp-m365-auth--pending nil
  "Device grant awaiting completion, as an alist, or nil.
Holds `device_code', `interval' and `expires_at' between
`nelisp-m365-auth-begin' and `nelisp-m365-auth-poll'.  Mirrored to disk
so a server restart between the two calls does not strand the user with
a code that no longer resolves to anything.")

(defvar nelisp-m365-auth--token nil
  "In-memory copy of the token cache alist, or nil when not loaded.")

;;; Endpoints ---------------------------------------------------------

(defun nelisp-m365-auth--endpoint (suffix)
  "Return the identity endpoint URL for SUFFIX under the active tenant."
  (concat "https://login.microsoftonline.com/"
          nelisp-m365-tenant "/oauth2/v2.0/" suffix))

(defun nelisp-m365-auth--scope-string ()
  "Return `nelisp-m365-scopes' as a space-separated string."
  (string-join nelisp-m365-scopes " "))

(defun nelisp-m365-auth--require-client-id ()
  "Return the configured client id, or signal when it is unset."
  (or nelisp-m365-client-id
      (signal 'nelisp-m365-auth-error
              (list "nelisp-m365-client-id is unset; see the package README for Entra app registration"))))

(defun nelisp-m365-auth--post-form (url pairs)
  "POST PAIRS to URL as form data and return the parsed JSON body.
Both success and error responses parse; the caller inspects the payload."
  (let* ((resp (nelisp-m365-curl-request
                "POST" url
                :headers (list (cons "Content-Type"
                                     "application/x-www-form-urlencoded")
                               (cons "Accept" "application/json"))
                :body (nelisp-m365-compat-form-encode pairs)))
         (body (plist-get resp :body)))
    (if (or (null body) (equal body ""))
        (signal 'nelisp-m365-auth-error
                (list (format "empty response from %s (HTTP %s)"
                              url (plist-get resp :status))))
      (nelisp-m365-compat-json-parse body))))

;;; Token cache -------------------------------------------------------

(defun nelisp-m365-auth--cache-path ()
  "Return the token cache path, or signal when it is unset."
  (or nelisp-m365-token-cache-file
      (signal 'nelisp-m365-auth-error
              (list "nelisp-m365-token-cache-file is unset"))))

(defun nelisp-m365-auth-load-token ()
  "Return the cached token alist, reading it from disk on first use."
  (or nelisp-m365-auth--token
      (let ((text (nelisp-m365-compat-read-file (nelisp-m365-auth--cache-path))))
        (setq nelisp-m365-auth--token
              (and text
                   (condition-case nil
                       (nelisp-m365-compat-json-parse text)
                     (error nil)))))))

(defun nelisp-m365-auth--save-token (alist)
  "Persist ALIST as the token cache and return it."
  (setq nelisp-m365-auth--token alist)
  (nelisp-m365-compat-write-file
   (nelisp-m365-auth--cache-path)
   (nelisp-m365-compat-json-encode alist)
   t)
  alist)

(defun nelisp-m365-auth-forget ()
  "Delete the token cache, both on disk and in memory."
  (setq nelisp-m365-auth--token nil)
  (condition-case nil (delete-file (nelisp-m365-auth--cache-path)) (error nil))
  t)

;;; Pending device grant ------------------------------------------------

(defun nelisp-m365-auth--pending-path ()
  "Return the path holding a device grant between begin and poll."
  (concat (nelisp-m365-auth--cache-path) ".pending"))

(defun nelisp-m365-auth--save-pending (alist)
  "Persist ALIST as the pending device grant and return it."
  (setq nelisp-m365-auth--pending alist)
  (nelisp-m365-compat-write-file
   (nelisp-m365-auth--pending-path)
   (nelisp-m365-compat-json-encode alist)
   t)
  alist)

(defun nelisp-m365-auth--load-pending ()
  "Return the pending device grant, reading it from disk when needed."
  (or nelisp-m365-auth--pending
      (let ((text (nelisp-m365-compat-read-file
                   (nelisp-m365-auth--pending-path))))
        (setq nelisp-m365-auth--pending
              (and text
                   (condition-case nil
                       (nelisp-m365-compat-json-parse text)
                     (error nil)))))))

(defun nelisp-m365-auth--clear-pending ()
  "Drop the pending device grant from memory and disk."
  (setq nelisp-m365-auth--pending nil)
  (condition-case nil
      (delete-file (nelisp-m365-auth--pending-path))
    (error nil))
  nil)

(defun nelisp-m365-auth--token-from-response (json previous)
  "Build a cache alist from token endpoint JSON.
PREVIOUS supplies the refresh token when the response omits one, which
the endpoint is permitted to do on refresh."
  (let* ((access (cdr (assoc "access_token" json)))
         (refresh (or (cdr (assoc "refresh_token" json))
                      (cdr (assoc "refresh_token" previous))))
         (expires-in (or (cdr (assoc "expires_in" json)) 3600))
         (now (nelisp-m365-compat-now)))
    (list (cons "access_token" access)
          (cons "refresh_token" refresh)
          (cons "scope" (cdr (assoc "scope" json)))
          (cons "token_type" (or (cdr (assoc "token_type" json)) "Bearer"))
          ;; Integers only: `nelisp-json-encode' rejects floats, and this
          ;; alist is written straight to the token cache.
          (cons "expires_at" (truncate (+ now expires-in)))
          (cons "obtained_at" now))))

;;; Device-code sign-in ------------------------------------------------

(defun nelisp-m365-auth--open-browser (uri)
  "Best-effort: open URI in the user's browser.
Silent on failure -- the caller always shows the URL as text too, and a
headless host is a normal way to run this."
  (let ((argv (if (nelisp-m365-compat-windows-p)
                  (list "cmd.exe" "/c" "start" "" uri)
                ;; Under WSL a Windows browser is still the useful one,
                ;; and powershell.exe is on PATH there.
                (list "/bin/sh" "-c"
                      (concat "command -v xdg-open >/dev/null && xdg-open '" uri
                              "' || powershell.exe -NoProfile -Command Start-Process '"
                              uri "'")))))
    (condition-case nil (nelisp-m365-compat-run-program argv) (error nil))
    nil))

(defun nelisp-m365-auth-begin (&optional open-browser)
  "Start the device-code grant and return the user-facing instructions.

Returns an alist with `user_code', `verification_uri', `expires_in' and
`message'.  The grant is held in `nelisp-m365-auth--pending' until
`nelisp-m365-auth-poll' completes or it expires.  With OPEN-BROWSER
non-nil, also try to launch a browser at the verification URI."
  (let* ((client-id (nelisp-m365-auth--require-client-id))
         (json (nelisp-m365-auth--post-form
                (nelisp-m365-auth--endpoint "devicecode")
                (list (cons "client_id" client-id)
                      (cons "scope" (nelisp-m365-auth--scope-string)))))
         (device-code (cdr (assoc "device_code" json)))
         (user-code (cdr (assoc "user_code" json)))
         (uri (or (cdr (assoc "verification_uri" json))
                  "https://microsoft.com/devicelogin"))
         (interval (or (cdr (assoc "interval" json)) 5))
         (expires-in (or (cdr (assoc "expires_in" json)) 900)))
    (unless device-code
      (signal 'nelisp-m365-auth-error
              (list (or (cdr (assoc "error_description" json))
                        "device code request failed"))))
    (nelisp-m365-auth--save-pending
     (list (cons "device_code" device-code)
           (cons "interval" interval)
           (cons "expires_at" (+ (nelisp-m365-compat-now) expires-in))))
    (when open-browser
      (nelisp-m365-auth--open-browser uri))
    (list (cons "user_code" user-code)
          (cons "verification_uri" uri)
          (cons "expires_in" expires-in)
          (cons "message"
                (format "Open %s and enter the code %s, then call m365_authenticate_finish."
                        uri user-code)))))

(defun nelisp-m365-auth-poll ()
  "Poll the pending device grant until it resolves or the budget runs out.

Returns (\"status\" . \"connected\") with account details on success, or
\(\"status\" . \"pending\") when the person has not finished yet -- in
which case calling this again resumes the same grant."
  (let ((pending (nelisp-m365-auth--load-pending)))
    (unless pending
      (signal 'nelisp-m365-auth-error
              (list "no sign-in is in progress; call m365_authenticate first")))
    (let* ((client-id (nelisp-m365-auth--require-client-id))
           (device-code (cdr (assoc "device_code" pending)))
           (interval (or (cdr (assoc "interval" pending)) 5))
           (grant-expires (or (cdr (assoc "expires_at" pending)) 0))
           (deadline (+ (nelisp-m365-compat-now) nelisp-m365-auth-poll-budget))
           (result nil))
      (while (not result)
        (let* ((json (nelisp-m365-auth--post-form
                      (nelisp-m365-auth--endpoint "token")
                      (list (cons "grant_type"
                                  "urn:ietf:params:oauth:grant-type:device_code")
                            (cons "client_id" client-id)
                            (cons "device_code" device-code))))
               (err (cdr (assoc "error" json))))
          (cond
           ((null err)
            (let ((token (nelisp-m365-auth--token-from-response json nil)))
              (nelisp-m365-auth--save-token token)
              (nelisp-m365-auth--clear-pending)
              (setq result
                    (list (cons "status" "connected")
                          (cons "scope" (cdr (assoc "scope" token)))
                          (cons "expires_at"
                                (nelisp-m365-compat-iso8601-utc
                                 (cdr (assoc "expires_at" token))))))))
           ((equal err "authorization_pending")
            (if (>= (nelisp-m365-compat-now) (min deadline grant-expires))
                (let ((expired (>= (nelisp-m365-compat-now) grant-expires)))
                  (when expired (nelisp-m365-auth--clear-pending))
                  (setq result
                        (list (cons "status" (if expired "expired" "pending"))
                              (cons "message"
                                    (if expired
                                        "the code expired; start again with m365_authenticate"
                                      "sign-in not finished; call m365_authenticate_finish again")))))
              (nelisp-m365-compat-sleep interval)))
           ((equal err "slow_down")
            ;; The endpoint asks for a longer gap; honour it for the
            ;; rest of this grant rather than just once.
            (setq interval (+ interval 5))
            (nelisp-m365-compat-sleep interval))
           (t
            (nelisp-m365-auth--clear-pending)
            (signal 'nelisp-m365-auth-error
                    (list (or (cdr (assoc "error_description" json)) err)))))))
      result)))

;;; Access tokens ------------------------------------------------------

(defun nelisp-m365-auth-refresh ()
  "Exchange the cached refresh token for a new access token."
  (let* ((previous (nelisp-m365-auth-load-token))
         (refresh (cdr (assoc "refresh_token" previous))))
    (unless refresh
      (signal 'nelisp-m365-auth-error
              (list "not signed in; call m365_authenticate first")))
    (let* ((json (nelisp-m365-auth--post-form
                  (nelisp-m365-auth--endpoint "token")
                  (list (cons "grant_type" "refresh_token")
                        (cons "client_id" (nelisp-m365-auth--require-client-id))
                        (cons "refresh_token" refresh)
                        (cons "scope" (nelisp-m365-auth--scope-string)))))
           (err (cdr (assoc "error" json))))
      (when err
        ;; A dead refresh token cannot be recovered from, and leaving it
        ;; on disk would make every later call fail the same way.
        (nelisp-m365-auth-forget)
        (signal 'nelisp-m365-auth-error
                (list (format "%s; sign in again with m365_authenticate"
                              (or (cdr (assoc "error_description" json)) err)))))
      (nelisp-m365-auth--save-token
       (nelisp-m365-auth--token-from-response json previous)))))

(defun nelisp-m365-auth-access-token (&optional force-refresh)
  "Return a usable access token, refreshing when needed.
With FORCE-REFRESH non-nil, refresh even if the cached token still looks
valid -- used after a 401, where the server disagrees with the clock."
  (let* ((token (nelisp-m365-auth-load-token))
         (access (cdr (assoc "access_token" token)))
         (expires-at (cdr (assoc "expires_at" token)))
         (now (nelisp-m365-compat-now)))
    (cond
     (force-refresh
      (cdr (assoc "access_token" (nelisp-m365-auth-refresh))))
     ((null token)
      (signal 'nelisp-m365-auth-error
              (list "not signed in; call m365_authenticate first")))
     ;; 120s of skew, and treat a missing expiry as expired: the Windows
     ;; build has no clock, so `expires_at' can be meaningless there.
     ((and access (numberp expires-at) (> expires-at (+ now 120)))
      access)
     (t
      (cdr (assoc "access_token" (nelisp-m365-auth-refresh)))))))

(defun nelisp-m365-auth-status ()
  "Return a connection-status alist without contacting the network."
  (let ((token (nelisp-m365-auth-load-token)))
    ;; Booleans go through `nelisp-m365-compat-json-bool': a bare nil
    ;; encodes as JSON null, and "connected": null reads as unknown
    ;; rather than as not-signed-in.
    (list (cons "connected"
                (nelisp-m365-compat-json-bool
                 (and token (cdr (assoc "refresh_token" token)))))
          (cons "tenant" nelisp-m365-tenant)
          (cons "client_id_configured"
                (nelisp-m365-compat-json-bool nelisp-m365-client-id))
          (cons "requested_scopes" nelisp-m365-scopes)
          (cons "granted_scope" (and token (cdr (assoc "scope" token))))
          (cons "access_token_expires_at"
                (let ((exp (and token (cdr (assoc "expires_at" token)))))
                  (and (numberp exp) (nelisp-m365-compat-iso8601-utc exp))))
          (cons "token_cache" nelisp-m365-token-cache-file))))

(provide 'nelisp-m365-auth)

;;; nelisp-m365-auth.el ends here
