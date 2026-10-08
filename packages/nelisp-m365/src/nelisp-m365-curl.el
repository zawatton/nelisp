;;; nelisp-m365-curl.el --- HTTP transport for nelisp-m365  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The NeLisp standalone runtime has no socket primitives at all --
;; `make-network-process' and `open-network-stream' are both void -- so
;; every HTTP request in this package is a curl subprocess.  curl also
;; owns TLS, which keeps a certificate stack out of the runtime.
;;
;; Responses are read back through `-D -' plus `-o -', which puts the
;; header block and the body on the same stdout stream.  Both CRLF and LF
;; headers are accepted without normalizing the response body.
;;
;; Request bodies use `--data-binary @FILE' to avoid command-line quoting
;; and size limits.  The Windows launcher supplies a private native temp
;; directory; inline bodies are removed on both success and failure.

;;; Code:

(require 'nelisp-m365-compat)

(defconst nelisp-m365-curl-default-timeout 30
  "Seconds a single HTTP request may take before curl gives up.")

(define-error 'nelisp-m365-http-error "nelisp-m365 HTTP error")

(defun nelisp-m365-curl--header-args (headers)
  "Return curl -H arguments for the HEADERS alist."
  (let ((args nil))
    (dolist (h headers)
      (push "-H" args)
      (push (concat (car h) ": " (cdr h)) args))
    (nreverse args)))

(defun nelisp-m365-curl--status-of (line)
  "Return the integer status code in status LINE, or nil.
LINE looks like \"HTTP/1.1 200 OK\".  Regexp capture groups are
unreadable on the standalone runtime, so this splits on spaces."
  (let* ((after-proto (nelisp-m365-compat-split-once line " "))
         (rest (and after-proto (cdr after-proto)))
         (code-part (and rest
                         (let ((split (nelisp-m365-compat-split-once rest " ")))
                           (if split (car split) rest)))))
    (and code-part
         (let ((n (string-to-number code-part)))
           (and (> n 0) n)))))

(defun nelisp-m365-curl--parse-header-block (block)
  "Parse a raw header BLOCK into (STATUS . HEADERS-ALIST).
Header names are downcased.  Returns nil when BLOCK has no status line."
  (let* ((lines (nelisp-m365-compat-split-all
                 (string-join (nelisp-m365-compat-split-all block "\r\n") "\n") "\n"))
         (status (and lines (nelisp-m365-curl--status-of (car lines))))
         (headers nil))
    (when status
      (dolist (line (cdr lines))
        (let ((split (nelisp-m365-compat-split-once line ":")))
          (when split
            (push (cons (downcase (string-trim (car split)))
                        (string-trim (cdr split)))
                  headers))))
      (cons status (nreverse headers)))))

(defun nelisp-m365-curl--split-response (text)
  "Split raw curl output TEXT into (STATUS HEADERS BODY).
curl emits one header block per response, so a 1xx continuation or a
followed redirect leaves several stacked ahead of the body; the last one
wins."
  (let ((rest text)
        (status nil)
        (headers nil)
        (done nil))
    (while (not done)
      (let ((split (and (string-prefix-p "HTTP/" rest)
                        (or (nelisp-m365-compat-split-once rest "\r\n\r\n")
                            (nelisp-m365-compat-split-once rest "\n\n")))))
        (if (not split)
            (setq done t)
          (let ((parsed (nelisp-m365-curl--parse-header-block (car split))))
            (if (not parsed)
                (setq done t)
              (setq status (car parsed)
                    headers (cdr parsed)
                    rest (cdr split)))))))
    (list status headers rest)))

(defun nelisp-m365-curl-request (method url &rest options)
  "Perform an HTTP METHOD request to URL through curl.

OPTIONS is a plist:
  :headers      alist of extra request headers
  :bearer       OAuth access token, added as an Authorization header
  :body         request body string, sent with `--data-binary'
  :body-file    path whose contents are the body, sent with
                `--data-binary @FILE'.  Use this rather than :body for
                anything non-ASCII or large: it keeps the payload off
                the command line, where encoding and length limits are
                the platform's business rather than ours
  :upload-file  path sent with `--upload-file', for a raw PUT
  :timeout      seconds before curl aborts (default
                `nelisp-m365-curl-default-timeout')

Return a plist (:status CODE :headers ALIST :body STRING).  Signal
`nelisp-m365-http-error' when curl itself fails; a non-2xx HTTP response
is returned normally so the caller can read the error payload."
  (let* ((curl (nelisp-m365-compat-curl-program))
         (timeout (or (plist-get options :timeout)
                      nelisp-m365-curl-default-timeout))
         (bearer (plist-get options :bearer))
         (body (plist-get options :body))
         (headers (plist-get options :headers)))
    (unless curl
      (signal 'nelisp-m365-http-error
              (list "curl executable not found; install curl or add its path to nelisp-m365-compat--curl-candidates")))
    (when bearer
      (setq headers (cons (cons "Authorization" (concat "Bearer " bearer))
                          headers)))
    (when (and body (plist-get options :body-file))
      (error "Specify either :body or :body-file"))
    (let* ((temp-body (and body (make-temp-file "nelisp-m365-request-")))
           (body-file (or temp-body (plist-get options :body-file)))
           (upload-file (plist-get options :upload-file))
           (argv (append
                  (list curl "-sS" "-D" "-" "-o" "-"
                        "--max-time" (number-to-string timeout)
                        "-X" (upcase method))
                  (nelisp-m365-curl--header-args headers)
                  (when body-file (list "--data-binary" (concat "@" body-file)))
                  (when upload-file (list "--upload-file" upload-file))
                  (list url)))
           (res nil))
      (unwind-protect
          (progn
            (when temp-body
              (nelisp-m365-compat-write-file temp-body body t))
            (setq res (nelisp-m365-compat-run-program argv))
            (unless res
              (signal 'nelisp-m365-http-error (list "curl produced no result")))
            (unless (equal (car res) 0)
              (signal 'nelisp-m365-http-error
                      (list (format "curl exited %s for %s" (car res) url))))
            (let ((parts (nelisp-m365-curl--split-response (cdr res))))
              (unless (nth 0 parts)
                (signal 'nelisp-m365-http-error
                        (list (format "malformed HTTP response from %s" url))))
              (list :status (nth 0 parts)
                    :headers (nth 1 parts)
                    :body (nth 2 parts))))
        (when temp-body (delete-file temp-body))))))

(defun nelisp-m365-curl-download (url dest &rest options)
  "Download URL to the file DEST, following redirects.
OPTIONS accepts :bearer, :headers and :timeout as in
`nelisp-m365-curl-request'.  Return the HTTP status code.  Used for
OneDrive item content, which answers with a redirect to a pre-signed
storage URL; the Authorization header is deliberately not replayed to
the redirect target."
  (let* ((curl (nelisp-m365-compat-curl-program))
         (timeout (or (plist-get options :timeout)
                      nelisp-m365-curl-default-timeout))
         (bearer (plist-get options :bearer))
         (headers (plist-get options :headers)))
    (unless curl
      (signal 'nelisp-m365-http-error (list "curl executable not found")))
    (when bearer
      (setq headers (cons (cons "Authorization" (concat "Bearer " bearer))
                          headers)))
    (let* ((argv (append
                  (list curl "-sS" "-L" "-w" "%{http_code}" "-o" dest
                        "--max-time" (number-to-string timeout))
                  (nelisp-m365-curl--header-args headers)
                  (list url)))
           (res (nelisp-m365-compat-run-program argv)))
      (unless (and res (equal (car res) 0))
        (signal 'nelisp-m365-http-error
                (list (format "curl download failed (exit %s) for %s" (car res) url))))
      (string-to-number (string-trim (cdr res))))))

(provide 'nelisp-m365-curl)

;;; nelisp-m365-curl.el ends here
