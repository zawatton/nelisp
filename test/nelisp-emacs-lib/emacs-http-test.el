;;; emacs-http-test.el --- Tests for emacs-http -*- lexical-binding: t; -*-

(require 'ert)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path
               (expand-file-name "../packages/nelisp-emacs-io/lisp" here)))
(require 'emacs-http)

;;;; --- pure: request parsing ---------------------------------------------

(ert-deftest emacs-http-parse-get ()
  (let ((r (emacs-http-parse-request
            "GET /hello?x=1&y=2 HTTP/1.1\r\nHost: localhost\r\nAccept: */*\r\n\r\n")))
    (should (plist-get r :complete))
    (should (string= (plist-get r :method) "GET"))
    (should (string= (plist-get r :path) "/hello"))
    (should (string= (plist-get r :query) "x=1&y=2"))
    (should (string= (emacs-http-request-header r "host") "localhost"))
    (should (string= (emacs-http-request-header r "ACCEPT") "*/*"))
    (should (string= (plist-get r :body) ""))))

(ert-deftest emacs-http-parse-post-body ()
  (let ((r (emacs-http-parse-request
            "POST /webhook HTTP/1.1\r\nContent-Length: 11\r\n\r\nhello world")))
    (should (plist-get r :complete))
    (should (string= (plist-get r :method) "POST"))
    (should (string= (plist-get r :body) "hello world"))))

(ert-deftest emacs-http-parse-incomplete-head ()
  (let ((r (emacs-http-parse-request "GET / HTTP/1.1\r\nHost: x")))
    (should-not (plist-get r :complete))
    (should (eq (plist-get r :need) :head))))

(ert-deftest emacs-http-parse-incomplete-body ()
  ;; head complete, but only 3 of 11 body bytes arrived
  (let ((r (emacs-http-parse-request
            "POST /w HTTP/1.1\r\nContent-Length: 11\r\n\r\nhel")))
    (should-not (plist-get r :complete))
    (should (eq (plist-get r :need) :body))))

(ert-deftest emacs-http-parse-bad-request-line ()
  (let ((r (emacs-http-parse-request "GARBAGE\r\n\r\n")))
    (should-not (plist-get r :complete))
    (should (plist-get r :error))))

;;;; --- pure: response building -------------------------------------------

(ert-deftest emacs-http-build-basic ()
  (let ((s (emacs-http-build-response :status 200 :body "hi")))
    (should (string-prefix-p "HTTP/1.1 200 OK\r\n" s))
    (should (string-match-p "Content-Length: 2\r\n" s))
    (should (string-match-p "Connection: close\r\n" s))
    (should (string-suffix-p "\r\n\r\nhi" s))))

(ert-deftest emacs-http-build-content-length-is-bytes ()
  ;; multibyte body: Content-Length must be the UTF-8 byte count, not chars
  (let ((s (emacs-http-build-response :status 200 :body "あ")))
    (should (string-match-p "Content-Length: 3\r\n" s))))

(ert-deftest emacs-http-build-custom-status-and-header ()
  (let ((s (emacs-http-build-response
            :status 404 :headers '(("Content-Type" . "text/plain")) :body "no")))
    (should (string-prefix-p "HTTP/1.1 404 Not Found\r\n" s))
    (should (string-match-p "Content-Type: text/plain\r\n" s))))

;;;; --- adapter: real socket roundtrip via curl ---------------------------

(defun emacs-http-test--curl ()
  (executable-find "curl"))

(defun emacs-http-test--free-port ()
  "Grab an ephemeral port by opening then closing a server socket."
  (let* ((p (make-network-process :name "emacs-http-test-probe"
                                  :family 'ipv4 :host 'local
                                  :service t :server t))
         (port (process-contact p :service)))
    (delete-process p)
    port))

(defun emacs-http-test--curl-async (port path &rest args)
  "Run curl against http://127.0.0.1:PORT/PATH with ARGS asynchronously.
The server and client live in the same single-threaded Emacs, so curl
must run async while `accept-process-output' drives the server's accept
and filter callbacks.  Returns curl's captured stdout."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           (url (format "http://127.0.0.1:%d%s" port path))
           (proc (apply #'start-process "emacs-http-test-curl" buf
                        "curl" "-s" (append args (list url)))))
      ;; keep the default sentinel from inserting a "Process ... finished"
      ;; status line into the capture buffer
      (set-process-sentinel proc #'ignore)
      (let ((deadline (+ (float-time) 10)))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (buffer-string))))

(ert-deftest emacs-http-serve-roundtrip ()
  "Start a real server, hit it with curl, get the handler's reply."
  (unless (emacs-http-test--curl)
    (ert-skip "curl not available"))
  (let* ((port (emacs-http-test--free-port))
         (server
          (emacs-http-serve
           port
           (lambda (req)
             (cond
              ((string= (plist-get req :path) "/echo")
               (list :status 200
                     :headers '(("Content-Type" . "text/plain"))
                     :body (concat "you said: " (plist-get req :body))))
              (t (list :status 404 :body "nope")))))))
    (unwind-protect
        (progn
          ;; GET the 404 branch (write http_code to stdout)
          (should (string=
                   (emacs-http-test--curl-async
                    port "/missing" "-o" "/dev/null" "-w" "%{http_code}")
                   "404"))
          ;; POST /echo returns the body
          (should (string=
                   (emacs-http-test--curl-async
                    port "/echo" "--data-binary" "ping")
                   "you said: ping")))
      (emacs-http-stop server))))

;;; emacs-http-test.el ends here
