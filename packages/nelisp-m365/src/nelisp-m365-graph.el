;;; nelisp-m365-graph.el --- Microsoft Graph client for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small Microsoft Graph v1.0 client over the curl transport.  It owns
;; three things the tool layer should not have to repeat: attaching a
;; live access token (refreshing once when the server says the token is
;; stale, which is more reliable than trusting the local clock),
;; honouring 429 throttling, and following `@odata.nextLink' so callers
;; see a single list instead of a page.
;;
;; Only the consumer-visible slice of Graph is reachable with a personal
;; Microsoft account: mail, calendar, contacts, OneDrive, To Do and
;; OneNote.  Teams, SharePoint, Planner and the `/search/query' entity
;; types are organisational-only and will answer 403 or 404 no matter
;; what scopes are consented, so nothing here calls them.

;;; Code:

(require 'nelisp-m365-compat)
(require 'nelisp-m365-curl)
(require 'nelisp-m365-auth)

(defconst nelisp-m365-graph-root "https://graph.microsoft.com/v1.0"
  "Base URL of the Microsoft Graph v1.0 endpoint.")

(defvar nelisp-m365-graph-max-pages 10
  "Maximum number of `@odata.nextLink' hops in one paged read.
A guard against a caller asking for an unbounded mailbox scan; the tool
layer reports when it stops early.")

(defvar nelisp-m365-graph-max-retries 3
  "How many times a throttled (429) request is retried.")

(define-error 'nelisp-m365-graph-error "Microsoft Graph error")

;;; Query strings ------------------------------------------------------

(defun nelisp-m365-graph-query (params)
  "Return PARAMS as a query string, or an empty string when all are nil.

PARAMS is an alist of (NAME . VALUE).  Entries with a nil or empty value
are dropped, which lets callers pass optional OData options
unconditionally.  Names are emitted literally so the leading `$' of
`$select' and friends stays readable in logs; values are percent-encoded."
  (let ((parts nil))
    (dolist (p params)
      (let ((v (cdr p)))
        (when (and v (not (equal v "")))
          (push (concat (car p) "="
                        (nelisp-m365-compat-url-encode
                         (if (stringp v) v (format "%s" v))))
                parts))))
    (if parts (concat "?" (string-join (nreverse parts) "&")) "")))

(defun nelisp-m365-graph-escape-odata (value)
  "Escape VALUE for use inside an OData single-quoted string literal."
  (string-join (nelisp-m365-compat-split-all value "'") "''"))

(defun nelisp-m365-graph--url (path)
  "Return the absolute Graph URL for PATH.
PATH may already be absolute, which is how `@odata.nextLink' arrives."
  (if (string-prefix-p "http" path)
      path
    (concat nelisp-m365-graph-root path)))

;;; Requests -----------------------------------------------------------

(defun nelisp-m365-graph--error (status body)
  "Signal a Graph error built from STATUS and the parsed BODY."
  (let* ((err (cdr (assoc "error" body)))
         (code (or (cdr (assoc "code" err)) ""))
         (message (or (cdr (assoc "message" err)) "request failed")))
    (signal 'nelisp-m365-graph-error
            (list (format "Microsoft Graph %s%s: %s"
                          status
                          (if (equal code "") "" (format " (%s)" code))
                          message)))))

(defun nelisp-m365-graph--parse-body (body)
  "Parse BODY as JSON, returning nil for an empty payload."
  (if (or (null body) (equal (string-trim body) ""))
      nil
    (condition-case nil
        (nelisp-m365-compat-json-parse body)
      (error nil))))

(defun nelisp-m365-graph-request (method path &rest options)
  "Send METHOD to Graph PATH and return the parsed JSON body.

OPTIONS is a plist:
  :headers  extra request headers
  :body     request body string
  :timeout  seconds before curl aborts

A 401 triggers exactly one forced token refresh and retry, because the
server is a better authority on token validity than the local clock --
and the Windows build has no clock at all.  A 429 is retried up to
`nelisp-m365-graph-max-retries' times, honouring `Retry-After'."
  (let ((url (nelisp-m365-graph--url path))
        (headers (append (plist-get options :headers)
                         (list (cons "Accept" "application/json"))))
        (refreshed nil)
        (retries 0)
        (result nil)
        (done nil))
    (while (not done)
      (let* ((token (nelisp-m365-auth-access-token refreshed))
             (resp (nelisp-m365-curl-request
                    method url
                    :bearer token
                    :headers headers
                    :body (plist-get options :body)
                    :body-file (plist-get options :body-file)
                    :upload-file (plist-get options :upload-file)
                    :timeout (plist-get options :timeout)))
             (status (plist-get resp :status))
             (body (nelisp-m365-graph--parse-body (plist-get resp :body))))
        (cond
         ((and (>= status 200) (< status 300))
          (setq result body done t))
         ((and (equal status 401) (not refreshed))
          (setq refreshed t))
         ((and (equal status 429) (< retries nelisp-m365-graph-max-retries))
          (setq retries (1+ retries))
          (let* ((hdr (cdr (assoc "retry-after" (plist-get resp :headers))))
                 (wait (if hdr (string-to-number hdr) 0)))
            (nelisp-m365-compat-sleep (if (> wait 0) wait (* 2 retries)))))
         (t
          (nelisp-m365-graph--error status body)))))
    result))

(defun nelisp-m365-graph-get (path &rest options)
  "GET Graph PATH and return the parsed JSON body."
  (apply #'nelisp-m365-graph-request "GET" path options))

(defun nelisp-m365-graph-collection (path limit &rest options)
  "GET Graph PATH and return up to LIMIT items from the collection.

Follows `@odata.nextLink' until LIMIT items are collected or
`nelisp-m365-graph-max-pages' hops have been made.  Returns a plist
\(:items LIST :truncated BOOL); `:truncated' is non-nil when more
results exist than were returned, so the tool layer can say so rather
than silently implying the list is complete."
  (let ((items nil)
        (count 0)
        (pages 0)
        (next path)
        (truncated nil))
    (while (and next (< count limit) (< pages nelisp-m365-graph-max-pages))
      (setq pages (1+ pages))
      (let* ((body (apply #'nelisp-m365-graph-get next options))
             (value (nelisp-m365-compat-to-list (cdr (assoc "value" body)))))
        (dolist (item value)
          (when (< count limit)
            (push item items)
            (setq count (1+ count))))
        (setq next (cdr (assoc "@odata.nextLink" body)))
        (when (and next (>= count limit))
          (setq truncated t))))
    (when (and next (>= pages nelisp-m365-graph-max-pages))
      (setq truncated t))
    (list :items (nreverse items) :truncated truncated)))

(defun nelisp-m365-graph--with-json-body (value fn)
  "Write VALUE as JSON to a temporary file and call FN with its path.
The file is removed afterwards.  Request bodies go through a file rather
than the command line because they carry Japanese subjects and bodies,
and because a mail with an attachment is megabytes of base64."
  (let ((file (make-temp-file "nelisp-m365-body-" nil ".json")))
    (unwind-protect
        (progn
          ;; Escaped to pure ASCII: the runtime's `write-region' compares
          ;; bytes written against characters given and signals when they
          ;; differ, so a body containing Japanese cannot be written at
          ;; all otherwise.
          (nelisp-m365-compat-write-file
           file (nelisp-m365-compat-json-encode-ascii value))
          (funcall fn file))
      (condition-case nil (delete-file file) (error nil)))))

(defun nelisp-m365-graph-send-json (method path value &rest options)
  "Send VALUE as a JSON body to Graph PATH with METHOD.
Returns the parsed response body, or nil for a 204."
  (nelisp-m365-graph--with-json-body
   value
   (lambda (file)
     (apply #'nelisp-m365-graph-request method path
            :body-file file
            :headers (cons (cons "Content-Type" "application/json")
                           (plist-get options :headers))
            options))))

(defun nelisp-m365-graph-post (path value &rest options)
  "POST VALUE as JSON to Graph PATH."
  (apply #'nelisp-m365-graph-send-json "POST" path value options))

(defun nelisp-m365-graph-patch (path value &rest options)
  "PATCH Graph PATH with VALUE as JSON."
  (apply #'nelisp-m365-graph-send-json "PATCH" path value options))

(defun nelisp-m365-graph-delete (path &rest options)
  "DELETE Graph PATH."
  (apply #'nelisp-m365-graph-request "DELETE" path options))

(defun nelisp-m365-graph-upload (path source &rest options)
  "PUT the contents of the local file SOURCE to Graph PATH.
curl streams the file, so its bytes never pass through the runtime --
which matters because every string here is UTF-8 and a binary file would
not survive the trip."
  (apply #'nelisp-m365-graph-request "PUT" path
         :upload-file source
         :headers (cons (cons "Content-Type" "application/octet-stream")
                        (plist-get options :headers))
         options))

(defun nelisp-m365-graph-download (path dest)
  "Download Graph PATH to the local file DEST and return its HTTP status.
Used for driveItem content, which answers with a redirect to pre-signed
storage."
  (nelisp-m365-curl-download (nelisp-m365-graph--url path) dest
                             :bearer (nelisp-m365-auth-access-token)))

;;; Shaping helpers -----------------------------------------------------

(defun nelisp-m365-graph-pick (object keys)
  "Return the subset of alist OBJECT named by KEYS, dropping absent ones.
Graph rows carry far more fields than a model needs to see; trimming
them at the client keeps tool output small."
  (let ((out nil))
    (dolist (k keys)
      (let ((cell (assoc k object)))
        (when cell (push cell out))))
    (nreverse out)))

(defun nelisp-m365-graph-address (recipient)
  "Return the email address string inside a Graph RECIPIENT object."
  (cdr (assoc "address" (cdr (assoc "emailAddress" recipient)))))

(defun nelisp-m365-graph-addresses (recipients)
  "Return the list of address strings for a Graph RECIPIENTS array.
RECIPIENTS arrives as a vector from the parser; a list is accepted too."
  (let ((out nil))
    (dolist (r (nelisp-m365-compat-to-list recipients))
      (let ((a (nelisp-m365-graph-address r)))
        (when a (push a out))))
    (nreverse out)))

(provide 'nelisp-m365-graph)

;;; nelisp-m365-graph.el ends here
