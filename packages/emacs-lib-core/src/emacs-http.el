;;; emacs-http.el --- minimal HTTP/1.1 server layer over make-network-process -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; The HTTP protocol layer the socket substrate was missing.  The real
;; TCP accept/serve substrate already exists (`make-network-process'
;; with `:server', backed by the syscall FFI in `emacs-network-ffi.el'
;; / `emacs-process-events.el'); what did not exist was a way to speak
;; HTTP over it.  This module is that layer, and only that layer:
;;
;;   pure, no sockets (fully unit-testable):
;;     `emacs-http-parse-request'   raw bytes -> request plist (or partial)
;;     `emacs-http-request-header'  case-insensitive header lookup
;;     `emacs-http-build-response'  status + headers + body -> raw bytes
;;
;;   adapter, over make-network-process:
;;     `emacs-http-serve'           bind a port, dispatch each complete
;;                                  request to a handler, write its reply
;;     `emacs-http-stop'            tear a server down
;;
;; A handler takes the request plist and returns a response plist
;; (:status :headers :body); the serve loop turns that into bytes and
;; closes the connection (HTTP/1.0-style Connection: close, which is all
;; the web-product substrate needs and keeps per-connection state
;; trivial).  Requests are accumulated per-connection until Content-Length
;; bytes have arrived, so a body split across TCP segments still parses.
;;
;; Dual-target: pure Elisp on top of `make-network-process', which is
;; native under host Emacs and polyfilled under NeLisp standalone.  A
;; `emacs-http-max-request-bytes' cap bounds memory per connection
;; (input-size hardening).

;;; Code:

(require 'cl-lib)

(defvar emacs-http-max-request-bytes (* 1024 1024)
  "Reject a request whose accumulated bytes exceed this cap.
Bounds per-connection memory; a request over the cap gets a 413 and the
connection is closed.")

;;;; --- pure: request parsing ---------------------------------------------

(defun emacs-http--split-first (string sep)
  "Split STRING on the first occurrence of SEP into a cons (HEAD . TAIL).
Return nil when SEP is absent."
  (let ((i (string-search sep string)))
    (when i
      (cons (substring string 0 i)
            (substring string (+ i (length sep)))))))

(defun emacs-http-parse-request (raw)
  "Parse RAW HTTP request bytes into a request plist, or a partial marker.
On a complete request return a plist:
  (:method :path :query :version :headers ALIST :body STRING :complete t)
When the head is not yet fully received return (:complete nil :need :head).
When the head is present but the Content-Length body is not fully
received return (:complete nil :need :body).
On a malformed request line return (:complete nil :error \"...\")."
  (let ((sep (emacs-http--split-first raw "\r\n\r\n")))
    (if (not sep)
        (list :complete nil :need :head)
      (let* ((head (car sep))
             (rest (cdr sep))
             (lines (split-string head "\r\n"))
             (request-line (car lines))
             (header-lines (cdr lines))
             (rl (split-string request-line " " t)))
        (if (/= (length rl) 3)
            (list :complete nil :error "bad request line")
          (let* ((method (nth 0 rl))
                 (target (nth 1 rl))
                 (version (nth 2 rl))
                 (pq (emacs-http--split-first target "?"))
                 (path (if pq (car pq) target))
                 (query (if pq (cdr pq) ""))
                 (headers nil))
            (dolist (h header-lines)
              (let ((kv (emacs-http--split-first h ":")))
                (when kv
                  (push (cons (downcase (string-trim (car kv)))
                              (string-trim (cdr kv)))
                        headers))))
            (setq headers (nreverse headers))
            (let* ((clen-str (cdr (assoc "content-length" headers)))
                   (clen (if clen-str (string-to-number clen-str) 0)))
              (if (< (length rest) clen)
                  (list :complete nil :need :body)
                (list :method method :path path :query query
                      :version version :headers headers
                      :body (substring rest 0 clen)
                      :complete t)))))))))

(defun emacs-http-request-header (request name)
  "Return header NAME (case-insensitive) from REQUEST, or nil."
  (cdr (assoc (downcase name) (plist-get request :headers))))

;;;; --- pure: response building -------------------------------------------

(defconst emacs-http--status-text
  '((200 . "OK") (201 . "Created") (204 . "No Content")
    (301 . "Moved Permanently") (302 . "Found") (304 . "Not Modified")
    (400 . "Bad Request") (401 . "Unauthorized") (403 . "Forbidden")
    (404 . "Not Found") (405 . "Method Not Allowed")
    (413 . "Payload Too Large") (429 . "Too Many Requests")
    (500 . "Internal Server Error") (503 . "Service Unavailable"))
  "Reason phrases for the status codes the substrate emits.")

(cl-defun emacs-http-build-response (&key (status 200) headers body)
  "Build a raw HTTP/1.1 response string.
STATUS is a code, HEADERS an alist of (NAME . VALUE), BODY a string.
Content-Length and Connection: close are added automatically; the
caller's headers win on conflict."
  (let* ((body (or body ""))
         (reason (or (cdr (assq status emacs-http--status-text)) "Status"))
         (have (lambda (n) (assoc-string n headers t)))
         (hdrs (copy-sequence headers)))
    (unless (funcall have "Content-Length")
      (push (cons "Content-Length" (number-to-string (string-bytes body))) hdrs))
    (unless (funcall have "Connection")
      (push (cons "Connection" "close") hdrs))
    (concat
     (format "HTTP/1.1 %d %s\r\n" status reason)
     (mapconcat (lambda (h) (format "%s: %s\r\n" (car h) (cdr h))) hdrs "")
     "\r\n"
     body)))

;;;; --- adapter: serve loop over make-network-process ---------------------

(defun emacs-http--dispatch (proc handler)
  "Parse PROC's accumulated buffer; when complete, run HANDLER and reply."
  (let* ((raw (or (process-get proc :http-buf) ""))
         (parsed (emacs-http-parse-request raw)))
    (cond
     ((plist-get parsed :error)
      (emacs-http--reply proc (emacs-http-build-response
                               :status 400 :body "bad request")))
     ((not (plist-get parsed :complete))
      ;; keep waiting for more bytes (head or body)
      nil)
     (t
      (let ((resp (condition-case err
                      (funcall handler parsed)
                    (error (list :status 500
                                 :body (format "handler error: %S" err))))))
        (emacs-http--reply
         proc
         (emacs-http-build-response
          :status (or (plist-get resp :status) 200)
          :headers (plist-get resp :headers)
          :body (or (plist-get resp :body) ""))))))))

(defun emacs-http--reply (proc response-string)
  "Send RESPONSE-STRING to PROC and close the connection.
Depends only on `process-send-string' + `delete-process', which operate
on the network-connection fd directly.  It deliberately does NOT use
`process-live-p' or `process-send-eof': under the NeLisp standalone
reader those resolve to a different process subsystem's impl (or an
unloaded one), and gating the write on them silently drops the response
\(the connection closes with an empty reply).  Content-Length delimits
the body, so no half-close is needed for the client to know it is done."
  (condition-case nil
      (process-send-string proc response-string)
    (error nil))
  (ignore-errors (delete-process proc)))

(defun emacs-http--make-filter (handler)
  "Return a process filter that accumulates bytes and dispatches to HANDLER."
  (lambda (proc data)
    (let ((buf (concat (or (process-get proc :http-buf) "") data)))
      (if (> (length buf) emacs-http-max-request-bytes)
          (emacs-http--reply proc (emacs-http-build-response
                                   :status 413 :body "request too large"))
        (process-put proc :http-buf buf)
        (emacs-http--dispatch proc handler)))))

(cl-defun emacs-http-serve (port handler &key (host 'local) name)
  "Start an HTTP server on HOST:PORT dispatching to HANDLER.
HANDLER is called with a request plist and returns a response plist
\(:status :headers :body).  HOST is `local' (127.0.0.1, default), `any',
or an address string.  Returns the server process; stop it with
`emacs-http-stop'."
  (make-network-process
   :name (or name (format "emacs-http:%d" port))
   :family 'ipv4
   :host host
   :service port
   :server t
   :coding 'binary
   :filter (emacs-http--make-filter handler)))

(defun emacs-http-stop (server)
  "Stop an HTTP SERVER started by `emacs-http-serve'."
  (when (process-live-p server)
    (delete-process server)))

(provide 'emacs-http)
;;; emacs-http.el ends here
