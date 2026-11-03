;;; emacs-http-hardened-path-test.el --- HTTP + crypto integration -*- lexical-binding: t; -*-

;; Proves the hardening primitives compose into one request path: an
;; emacs-http server whose handler uses anvil-crypto for Stripe webhook
;; verification, stateless signed session tokens, and a CSPRNG session
;; id -- the "security built in from day one" shape the web-product plan
;; calls for.  Everything is exercised over a real socket with curl.
;;
;; anvil-crypto lives in the sibling anvil.el repo; the test loads it
;; from ANVIL_CRYPTO (or the default sibling path) and skips if absent.

(require 'ert)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path
               (expand-file-name "../packages/nelisp-emacs-io/lisp" here)))
(require 'emacs-http)

(defvar emacs-http-hardened-test--crypto
  (or (getenv "ANVIL_CRYPTO")
      (let ((here (file-name-directory (or load-file-name buffer-file-name))))
        (expand-file-name "../../anvil.el/anvil-crypto.el" here)))
  "Path to anvil-crypto.el (sibling repo).")

(defun emacs-http-hardened-test--load-crypto ()
  (if (file-exists-p emacs-http-hardened-test--crypto)
      (progn (load emacs-http-hardened-test--crypto nil t) t)
    nil))

;;;; --- the hardened demo handler -----------------------------------------
;;
;; POST /webhook  : verify a Stripe-Signature over the raw body; a valid,
;;                  in-window signature -> 200, else 400.
;; POST /login    : issue a signed session token in a Set-Cookie.
;; GET  /me       : read the sid cookie, verify the token -> 200 payload
;;                  or 401.

(defconst emacs-http-hardened-test--webhook-secret "whsec_demo")
(defconst emacs-http-hardened-test--session-secret "sess_demo")

(defun emacs-http-hardened-test--cookie (request name)
  "Return cookie NAME from REQUEST's Cookie header, or nil."
  (let ((cookie (emacs-http-request-header request "cookie")))
    (when cookie
      (cl-loop for pair in (split-string cookie ";[ ]*")
               for kv = (split-string pair "=")
               when (string= (car kv) name)
               return (cadr kv)))))

(defun emacs-http-hardened-test--handler (request)
  (let ((path (plist-get request :path))
        (method (plist-get request :method)))
    (cond
     ((and (string= method "POST") (string= path "/webhook"))
      (if (anvil-crypto-stripe-verify
           (plist-get request :body)
           (or (emacs-http-request-header request "stripe-signature") "")
           emacs-http-hardened-test--webhook-secret
           :now (string-to-number
                 (or (emacs-http-request-header request "x-test-now") "0")))
          (list :status 200 :body "webhook ok")
        (list :status 400 :body "bad signature")))
     ((and (string= method "POST") (string= path "/login"))
      ;; a real login checks credentials; here just mint a session that
      ;; carries a CSPRNG session id + the user, signed and expiring
      (let* ((sid (anvil-crypto-random-token 16))
             (payload (format "sid=%s;user=alice" sid))
             (token (anvil-crypto-sign-token
                     payload emacs-http-hardened-test--session-secret
                     :ttl 3600)))
        (list :status 200
              :headers (list (cons "Set-Cookie"
                                   (format "sid=%s; HttpOnly; SameSite=Strict"
                                           token)))
              :body "logged in")))
     ((and (string= method "GET") (string= path "/me"))
      (let* ((tok (emacs-http-hardened-test--cookie request "sid"))
             (payload (and tok
                           (anvil-crypto-verify-token
                            tok emacs-http-hardened-test--session-secret))))
        (if payload
            (list :status 200 :body payload)
          (list :status 401 :body "unauthorized"))))
     (t (list :status 404 :body "not found")))))

;;;; --- helpers -----------------------------------------------------------

(defun emacs-http-hardened-test--free-port ()
  (let* ((p (make-network-process :name "hp-probe" :family 'ipv4
                                  :host 'local :service t :server t))
         (port (process-contact p :service)))
    (delete-process p)
    port))

(defun emacs-http-hardened-test--curl (port path &rest args)
  "Async curl to PORT/PATH with ARGS, pumping the event loop."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (url (format "http://127.0.0.1:%d%s" port path))
           (proc (apply #'start-process "hp-curl" buf
                        "curl" "-s" (append args (list url)))))
      (set-process-sentinel proc #'ignore)
      (let ((deadline (+ (float-time) 10)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (buffer-string))))

;;;; --- the integration test ----------------------------------------------

(ert-deftest emacs-http-hardened-path ()
  (unless (emacs-http-hardened-test--load-crypto)
    (ert-skip (format "anvil-crypto not found at %s"
                      emacs-http-hardened-test--crypto)))
  (unless (executable-find "curl")
    (ert-skip "curl not available"))
  (let* ((port (emacs-http-hardened-test--free-port))
         (server (emacs-http-serve
                  port #'emacs-http-hardened-test--handler))
         (payload "{\"type\":\"checkout.session.completed\"}")
         (ts "1720900000")
         (good-sig (anvil-crypto-hmac-sha256
                    emacs-http-hardened-test--webhook-secret
                    (concat ts "." payload))))
    (unwind-protect
        (progn
          ;; --- webhook: valid signature accepted ---
          (should (string=
                   (emacs-http-hardened-test--curl
                    port "/webhook"
                    "-o" "/dev/null" "-w" "%{http_code}"
                    "-H" (format "Stripe-Signature: t=%s,v1=%s" ts good-sig)
                    "-H" (format "X-Test-Now: %s" ts)
                    "--data-binary" payload)
                   "200"))
          ;; --- webhook: tampered body rejected ---
          (should (string=
                   (emacs-http-hardened-test--curl
                    port "/webhook"
                    "-o" "/dev/null" "-w" "%{http_code}"
                    "-H" (format "Stripe-Signature: t=%s,v1=%s" ts good-sig)
                    "-H" (format "X-Test-Now: %s" ts)
                    "--data-binary" "{\"type\":\"tampered\"}")
                   "400"))
          ;; --- webhook: missing signature rejected ---
          (should (string=
                   (emacs-http-hardened-test--curl
                    port "/webhook"
                    "-o" "/dev/null" "-w" "%{http_code}"
                    "--data-binary" payload)
                   "400"))
          ;; --- session: login sets a signed cookie ---
          (let* ((raw (emacs-http-hardened-test--curl
                       port "/login" "-i" "-X" "POST"))
                 ;; curl 8.x styles -i header names with ANSI bold; strip it
                 (headers (replace-regexp-in-string "\e\\[[0-9;]*m" "" raw))
                 (cookie (and (string-match
                               "[Ss]et-[Cc]ookie: sid=\\([^;]+\\)" headers)
                              (match-string 1 headers))))
            (should cookie)
            ;; /me with the real cookie -> 200 and the session payload
            (let ((me (emacs-http-hardened-test--curl
                       port "/me" "-H" (format "Cookie: sid=%s" cookie))))
              (should (string-match-p "user=alice" me)))
            ;; /me with a tampered cookie -> 401
            (should (string=
                     (emacs-http-hardened-test--curl
                      port "/me" "-o" "/dev/null" "-w" "%{http_code}"
                      "-H" (format "Cookie: sid=%sX" cookie))
                     "401")))
          ;; --- /me with no cookie -> 401 ---
          (should (string=
                   (emacs-http-hardened-test--curl
                    port "/me" "-o" "/dev/null" "-w" "%{http_code}")
                   "401")))
      (emacs-http-stop server))))

;;; emacs-http-hardened-path-test.el ends here
