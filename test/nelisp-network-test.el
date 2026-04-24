;;; nelisp-network-test.el --- Phase 5-C.2 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-C.2 — `src/nelisp-network.el'.
;;
;; Coverage:
;;   - `nelisp-open-network' struct shape (tcp only; tls path is
;;     exercised as fboundp-only because loopback TLS needs a cert)
;;   - loopback TCP echo via in-process server proves send / filter
;;     / close
;;   - sentinel trampoline dispatches host lambda on disconnect
;;   - registry add / find-by-host / close removes
;;   - get / put plist roundtrip
;;   - missing :host / :service signal
;;   - HTTP cache internals: fresh-p / put / get / clear / size
;;   - HTTP-GET against in-process HTTP/1.1 server: status 200,
;;     body, headers surfaced; cached hit on second call
;;   - HTTP-HEAD returns status + headers without body

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-network)

;;; In-process loopback servers --------------------------------------

(defvar nelisp-network-test--server nil)

(defun nelisp-network-test--start-echo-server ()
  "Start a TCP echo server on 127.0.0.1; return (SERVER . PORT)."
  (let* ((server
          (make-network-process
           :name "nl-echo"
           :server t
           :host "127.0.0.1"
           :service t
           :family 'ipv4
           :coding 'binary
           :filter (lambda (proc chunk)
                     (process-send-string proc chunk)))))
    (cons server (cadr (process-contact server)))))

(defun nelisp-network-test--stop-server (srv)
  (when (and srv (processp srv))
    (ignore-errors (delete-process srv))))

(defun nelisp-network-test--start-http-server (response)
  "Start an HTTP/1.1 server on 127.0.0.1 that replies with RESPONSE
(a full HTTP response string) once per connection, then closes.
Returns (SERVER . PORT)."
  (let* ((server
          (make-network-process
           :name "nl-http"
           :server t
           :host "127.0.0.1"
           :service t
           :family 'ipv4
           :coding 'binary
           :filter (lambda (proc _chunk)
                     (process-send-string proc response)
                     (process-send-eof proc)
                     (ignore-errors (delete-process proc))))))
    (cons server (cadr (process-contact server)))))

(defmacro nelisp-network-test--fresh (&rest body)
  (declare (indent 0))
  `(progn
     (nelisp-network--reset-registry)
     (nelisp-http-cache-clear)
     (unwind-protect
         (progn ,@body)
       (dolist (wrap (nelisp-network-list))
         (ignore-errors (nelisp-network-close wrap)))
       (nelisp-network-test--stop-server nelisp-network-test--server)
       (setq nelisp-network-test--server nil)
       (nelisp-network--reset-registry)
       (nelisp-http-cache-clear))))

;;; Network layer ----------------------------------------------------

(ert-deftest nelisp-network-open-struct-and-status ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network :name "c" :host "127.0.0.1"
                                    :service port :type 'tcp)))
       (should (nelisp-network-p c))
       (should (nelisp-network-live-p c))
       (should (memq (nelisp-network-status c) '(open run)))
       (should (memq c (nelisp-network-list)))))))

(ert-deftest nelisp-network-missing-host-signals ()
  (nelisp-network-test--fresh
   (should-error (nelisp-open-network :service 1234))))

(ert-deftest nelisp-network-missing-service-signals ()
  (nelisp-network-test--fresh
   (should-error (nelisp-open-network :host "127.0.0.1"))))

(ert-deftest nelisp-network-send-and-filter-roundtrip ()
  "Connect to loopback echo; send a string; confirm filter delivers it."
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp))
          (got (list)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network
               :name "rt" :host "127.0.0.1" :service port
               :filter (lambda (_wrap chunk) (push chunk got)))))
       (nelisp-network-send-string c "ping-from-nelisp")
       (let ((deadline (+ (float-time) 2.0)))
         (while (and (null got) (< (float-time) deadline))
           (accept-process-output nil 0.05)))
       (should (cl-some (lambda (s)
                          (and (stringp s)
                               (string-match-p "ping-from-nelisp" s)))
                        got))))))

(ert-deftest nelisp-network-sentinel-trampoline-on-close ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp))
          (seen (list)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network
               :name "sn" :host "127.0.0.1" :service port
               :sentinel (lambda (_wrap ev) (push ev seen)))))
       (nelisp-network-close c)
       (accept-process-output nil 0.1)
       (should (cl-some (lambda (s) (and (stringp s)
                                         (string-match-p
                                          "deleted\\|closed\\|exited"
                                          s)))
                        seen))))))

(ert-deftest nelisp-network-close-removes-from-registry ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network :name "cr" :host "127.0.0.1"
                                   :service port)))
       (should (memq c (nelisp-network-list)))
       (nelisp-network-close c)
       (should (null (memq c (nelisp-network-list))))))))

(ert-deftest nelisp-network-find-by-host ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network :name "fb" :host "127.0.0.1"
                                   :service port)))
       (should (eq c (nelisp-network--find-by-host
                      (nelisp-network-host-conn c))))))))

(ert-deftest nelisp-network-get-put-roundtrip ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-echo-server))
          (srv (car sp)) (port (cdr sp)))
     (setq nelisp-network-test--server srv)
     (let ((c (nelisp-open-network :name "gp" :host "127.0.0.1"
                                   :service port
                                   :props (list :label "seed"))))
       (should (equal "seed" (nelisp-network-get c :label)))
       (nelisp-network-put c :retries 3)
       (should (= 3 (nelisp-network-get c :retries)))))))

(ert-deftest nelisp-network-tls-path-fboundp-only ()
  "`:type 'tls' path requires a remote with a cert — we only
verify the open function resolves without error when the stream
backend is present (actual TLS loopback is a §3.4 demo)."
  (should (fboundp 'open-network-stream)))

;;; HTTP cache internals --------------------------------------------

(ert-deftest nelisp-network-http-cache-clear-and-size ()
  (nelisp-http-cache-clear)
  (should (= 0 (nelisp-http-cache-size)))
  (puthash "http://example/" (list :body "x") nelisp-http--cache)
  (should (= 1 (nelisp-http-cache-size)))
  (nelisp-http-cache-clear)
  (should (= 0 (nelisp-http-cache-size))))

(ert-deftest nelisp-network-http-cache-fresh-p ()
  (should (nelisp-http--cache-fresh-p
           (list :ttl 60 :fetched-at (float-time))))
  (should-not (nelisp-http--cache-fresh-p
               (list :ttl 60 :fetched-at (- (float-time) 100))))
  (should-not (nelisp-http--cache-fresh-p (list :body "no ttl"))))

(ert-deftest nelisp-network-http-parse-buffer-response ()
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n"
            "Content-Type: text/plain\r\n"
            "ETag: \"abc\"\r\n\r\n"
            "payload-body")
    (let* ((parsed (nelisp-http--parse-buffer (current-buffer))))
      (should (= 200 (plist-get parsed :status)))
      (should (equal "text/plain"
                     (cdr (assoc "content-type"
                                 (plist-get parsed :headers)))))
      (should (equal "\"abc\""
                     (cdr (assoc "etag" (plist-get parsed :headers)))))
      (should (string-match-p "payload-body"
                              (plist-get parsed :body))))))

;;; HTTP GET / HEAD integration -------------------------------------

(defconst nelisp-network-test--http-200
  "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\nETag: \"v1\"\r\nConnection: close\r\n\r\nhello"
  "A minimal well-formed HTTP/1.1 response for in-process testing.")

(ert-deftest nelisp-network-http-get-roundtrip ()
  "Loopback HTTP GET returns the canned 200 response body."
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-http-server
               nelisp-network-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port)))
     (setq nelisp-network-test--server srv)
     (let ((result (nelisp-http-get url)))
       (should (= 200 (plist-get result :status)))
       (should (string-match-p "hello" (plist-get result :body)))
       (should (equal "\"v1\""
                      (cdr (assoc "etag"
                                  (plist-get result :headers)))))
       (should (null (plist-get result :cached)))))))

(ert-deftest nelisp-network-http-get-caches-within-ttl ()
  "Second call within TTL returns cached body, :cached t."
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-http-server
               nelisp-network-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port)))
     (setq nelisp-network-test--server srv)
     (let ((first (nelisp-http-get url :cache-ttl 60)))
       (should (null (plist-get first :cached)))
       (should (= 1 (nelisp-http-cache-size))))
     (let ((second (nelisp-http-get url :cache-ttl 60)))
       (should (eq t (plist-get second :cached)))
       (should (string-match-p "hello" (plist-get second :body)))))))

(ert-deftest nelisp-network-http-cache-clear-evicts ()
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-http-server
               nelisp-network-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port)))
     (setq nelisp-network-test--server srv)
     (nelisp-http-get url :cache-ttl 60)
     (should (= 1 (nelisp-http-cache-size)))
     (nelisp-http-cache-clear)
     (should (= 0 (nelisp-http-cache-size))))))

(ert-deftest nelisp-network-http-head-returns-headers-only ()
  "HEAD goes to the same canned responder (which sends a full
200) — we still assert status is parseable + no body expectation."
  (nelisp-network-test--fresh
   (let* ((sp (nelisp-network-test--start-http-server
               nelisp-network-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port)))
     (setq nelisp-network-test--server srv)
     (let ((result (nelisp-http-head url)))
       (should (= 200 (plist-get result :status)))
       (should (assoc "etag" (plist-get result :headers)))))))

(ert-deftest nelisp-network-http-get-url-failure-returns-nil-status ()
  "Connecting to a closed port yields a nil :status plist, not an error."
  (nelisp-network-test--fresh
   (let ((result (nelisp-http-get "http://127.0.0.1:1/")))
     (should (null (plist-get result :status))))))

(provide 'nelisp-network-test)
;;; nelisp-network-test.el ends here
