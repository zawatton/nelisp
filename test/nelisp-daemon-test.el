;;; nelisp-daemon-test.el --- Phase 5-C.4 ERT -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT suite for Phase 5-C.4 — `examples/nelisp-daemon/main.el'.
;;
;; Coverage:
;;   - daemon start / stop lifecycle (collector actor spawn, watch
;;     registry cleanup)
;;   - pure dispatcher: initialize / tools/list / unknown method
;;   - JSON-RPC wire wrapper: request parse, response serialize
;;   - tools/call http-get against an in-process HTTP/1.1 loopback
;;     server (reusing the §3.2 pattern)
;;   - tools/call watch registers + tools/call watch-events drains
;;     a synthetic file-change event (batch inotify caveat per §3.3)
;;   - tools/call shutdown flips :running to nil
;;   - 3-layer integration: one test that stands up the daemon,
;;     exercises HTTP, registers a watch, injects a file-change,
;;     then drains events — proves process/network/file-notify
;;     plumbing composes under a single dispatcher

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'nelisp-actor)
(require 'nelisp-file-notify)

;; main.el is not on the default -L path; resolve it relative to the
;; repo root (CWD of `make test').
(let ((main (expand-file-name "examples/nelisp-daemon/main.el")))
  (when (file-exists-p main) (load main nil t)))

;;; In-process HTTP responder (reused from §3.2 pattern) -------------

(defvar nelisp-daemon-test--server nil)

(defconst nelisp-daemon-test--http-200
  "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 5\r\nETag: \"v1\"\r\nConnection: close\r\n\r\nhello"
  "Minimal canned HTTP/1.1 response used by integration tests.")

(defun nelisp-daemon-test--start-http-server (response)
  (let ((srv
         (make-network-process
          :name "nl-daemon-http"
          :server t :host "127.0.0.1" :service t
          :family 'ipv4 :coding 'binary
          :filter (lambda (proc _chunk)
                    (process-send-string proc response)
                    (process-send-eof proc)
                    (ignore-errors (delete-process proc))))))
    (cons srv (cadr (process-contact srv)))))

(defun nelisp-daemon-test--stop-server (srv)
  (when (and srv (processp srv))
    (ignore-errors (delete-process srv))))

(defmacro nelisp-daemon-test--fresh (&rest body)
  (declare (indent 0))
  `(progn
     (setq nelisp-daemon-test--server nil)
     (nelisp-daemon-stop)
     (nelisp-http-cache-clear)
     (nelisp-file-notify--reset-registry)
     (unwind-protect
         (progn ,@body)
       (nelisp-daemon-stop)
       (nelisp-daemon-test--stop-server nelisp-daemon-test--server)
       (setq nelisp-daemon-test--server nil)
       (nelisp-http-cache-clear)
       (nelisp-file-notify--reset-registry))))

;;; Lifecycle ---------------------------------------------------------

(ert-deftest nelisp-daemon-start-returns-collector-actor ()
  (nelisp-daemon-test--fresh
   (let ((c (nelisp-daemon-start)))
     (should (nelisp-actor-p c))
     (should (not (memq (nelisp-actor-status c) '(:dead :crashed))))
     (should (eq c (plist-get nelisp-daemon--state :collector))))))

(ert-deftest nelisp-daemon-stop-clears-state ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (nelisp-daemon-stop)
   (should (null nelisp-daemon--state))))

(ert-deftest nelisp-daemon-start-is-idempotent ()
  (nelisp-daemon-test--fresh
   (let ((c1 (nelisp-daemon-start))
         (c2 (nelisp-daemon-start)))
     (should (not (eq c1 c2)))
     (should (eq c2 (plist-get nelisp-daemon--state :collector))))))

;;; Pure dispatcher ---------------------------------------------------

(ert-deftest nelisp-daemon-dispatch-initialize ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let ((r (nelisp-daemon-dispatch "initialize" nil)))
     (should (plist-get r :serverInfo))
     (should (equal "nelisp-daemon-demo"
                    (plist-get (plist-get r :serverInfo) :name))))))

(ert-deftest nelisp-daemon-dispatch-tools-list ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((r (nelisp-daemon-dispatch "tools/list" nil))
          (names (mapcar (lambda (t0) (plist-get t0 :name))
                         (plist-get r :tools))))
     (should (member "http-get" names))
     (should (member "watch" names))
     (should (member "watch-events" names))
     (should (member "shutdown" names)))))

(ert-deftest nelisp-daemon-dispatch-unknown-method ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let ((r (nelisp-daemon-dispatch "bogus" nil)))
     (should (plist-get r :error)))))

(ert-deftest nelisp-daemon-dispatch-unknown-tool ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let ((r (nelisp-daemon-dispatch
             "tools/call"
             (list (cons 'name "does-not-exist")
                   (cons 'arguments nil)))))
     (should (plist-get r :error)))))

;;; JSON-RPC round trip ----------------------------------------------

(ert-deftest nelisp-daemon-call-json-initialize-round-trip ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")
          (resp (nelisp-daemon-call-json req))
          (parsed (json-read-from-string resp))
          (result (alist-get 'result parsed)))
     (should (equal "2.0" (alist-get 'jsonrpc parsed)))
     (should (equal 1 (alist-get 'id parsed)))
     (should result)
     (should (equal "nelisp-daemon-demo"
                    (alist-get 'name (alist-get 'serverInfo result)))))))

(ert-deftest nelisp-daemon-call-json-tools-list-round-trip ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/list\"}")
          (resp (nelisp-daemon-call-json req))
          (parsed (json-read-from-string resp))
          (result (alist-get 'result parsed))
          (tools (alist-get 'tools result))
          (names (mapcar (lambda (tt) (alist-get 'name tt))
                         tools)))
     (should (equal 7 (alist-get 'id parsed)))
     (should (member "http-get" names))
     (should (member "watch" names)))))

;;; HTTP tool vs loopback --------------------------------------------

(ert-deftest nelisp-daemon-tool-http-get-loopback-hello ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((sp (nelisp-daemon-test--start-http-server
               nelisp-daemon-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port))
          (r (nelisp-daemon-dispatch
              "tools/call"
              (list (cons 'name "http-get")
                    (cons 'arguments
                          (list (cons 'url url)))))))
     (setq nelisp-daemon-test--server srv)
     (should (= 200 (plist-get r :status)))
     (should (string-match-p "hello" (plist-get r :body))))))

(ert-deftest nelisp-daemon-tool-http-get-via-json-round-trip ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((sp (nelisp-daemon-test--start-http-server
               nelisp-daemon-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port))
          (req (format
                "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"http-get\",\"arguments\":{\"url\":\"%s\"}}}"
                url))
          (resp (nelisp-daemon-call-json req))
          (parsed (json-read-from-string resp))
          (result (alist-get 'result parsed)))
     (setq nelisp-daemon-test--server srv)
     (should (= 200 (alist-get 'status result)))
     (should (string-match-p "hello" (alist-get 'body result))))))

;;; Watch tool + synthetic file-change -------------------------------

(ert-deftest nelisp-daemon-tool-watch-registers-wrap ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((dir (file-name-as-directory (make-temp-file "nl-daemon-" t)))
          (r (nelisp-daemon-dispatch
              "tools/call"
              (list (cons 'name "watch")
                    (cons 'arguments (list (cons 'dir dir)))))))
     (unwind-protect
         (progn
           (should (equal dir (plist-get r :watching)))
           (should (assoc dir (plist-get nelisp-daemon--state :watches)))
           (should (nelisp-file-notify-list)))
       (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-daemon-watch-events-drains-collector ()
  (nelisp-daemon-test--fresh
   (let* ((_ (nelisp-daemon-start))
          (dir (file-name-as-directory (make-temp-file "nl-daemon-" t))))
     (unwind-protect
         (let* ((watch-result
                 (nelisp-daemon-dispatch
                  "tools/call"
                  (list (cons 'name "watch")
                        (cons 'arguments (list (cons 'dir dir))))))
                (wrap (cdr (assoc
                            dir (plist-get nelisp-daemon--state :watches)))))
           (should (plist-get watch-result :ok))
           ;; Inject a synthetic host-shaped file-notify event per §3.3.
           (nelisp-file-notify--trampoline
            (list (nelisp-file-notify-watch-host-desc wrap)
                  'created (expand-file-name "a.txt" dir)))
           ;; Drain collector, then ask via tool call.
           (nelisp-actor-run-until-idle)
           (let ((events-result
                  (nelisp-daemon-dispatch
                   "tools/call"
                   (list (cons 'name "watch-events")
                         (cons 'arguments nil)))))
             (should (= 1 (plist-get events-result :count)))
             (let ((first (car (plist-get events-result :events))))
               (should (eq 'created (plist-get first :action)))
               (should (string-match-p "a\\.txt"
                                       (plist-get first :file))))))
       (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-daemon-watch-events-empty-when-idle ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let ((r (nelisp-daemon-dispatch
             "tools/call"
             (list (cons 'name "watch-events")
                   (cons 'arguments nil)))))
     (should (= 0 (plist-get r :count)))
     (should (null (plist-get r :events))))))

;;; Shutdown + stop ---------------------------------------------------

(ert-deftest nelisp-daemon-shutdown-tool-flips-running ()
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (should (plist-get nelisp-daemon--state :running))
   (nelisp-daemon-dispatch
    "tools/call"
    (list (cons 'name "shutdown") (cons 'arguments nil)))
   (should (not (plist-get nelisp-daemon--state :running)))))

(ert-deftest nelisp-daemon-stop-removes-watches ()
  (nelisp-daemon-test--fresh
   (let* ((_ (nelisp-daemon-start))
          (dir (file-name-as-directory (make-temp-file "nl-daemon-" t))))
     (unwind-protect
         (progn
           (nelisp-daemon-dispatch
            "tools/call"
            (list (cons 'name "watch")
                  (cons 'arguments (list (cons 'dir dir)))))
           (should (nelisp-file-notify-list))
           (nelisp-daemon-stop)
           (should (null (nelisp-file-notify-list))))
       (ignore-errors (delete-directory dir t))))))

;;; 3-layer integration ----------------------------------------------

(ert-deftest nelisp-daemon-three-layer-integration ()
  "process/actor + network + file-notify all exercised under one
dispatcher path.  This is the §3.4 flagship test."
  (nelisp-daemon-test--fresh
   (nelisp-daemon-start)
   (let* ((sp (nelisp-daemon-test--start-http-server
               nelisp-daemon-test--http-200))
          (srv (car sp)) (port (cdr sp))
          (url (format "http://127.0.0.1:%d/" port))
          (dir (file-name-as-directory (make-temp-file "nl-daemon-" t))))
     (setq nelisp-daemon-test--server srv)
     (unwind-protect
         (progn
           ;; Layer 1: initialize + tools/list response shape.
           (let ((init (nelisp-daemon-dispatch "initialize" nil)))
             (should (plist-get init :serverInfo)))
           ;; Layer 2: HTTP round trip.
           (let ((http (nelisp-daemon-dispatch
                        "tools/call"
                        (list (cons 'name "http-get")
                              (cons 'arguments
                                    (list (cons 'url url)))))))
             (should (= 200 (plist-get http :status)))
             (should (string-match-p "hello" (plist-get http :body))))
           ;; Layer 3: file-notify via actor event pipeline.
           (let* ((watch-result
                   (nelisp-daemon-dispatch
                    "tools/call"
                    (list (cons 'name "watch")
                          (cons 'arguments (list (cons 'dir dir))))))
                  (wrap (cdr (assoc dir
                                    (plist-get nelisp-daemon--state
                                               :watches)))))
             (should (plist-get watch-result :ok))
             (nelisp-file-notify--trampoline
              (list (nelisp-file-notify-watch-host-desc wrap)
                    'created (expand-file-name "x.log" dir)))
             (nelisp-actor-run-until-idle)
             (let ((events (nelisp-daemon-dispatch
                            "tools/call"
                            (list (cons 'name "watch-events")
                                  (cons 'arguments nil)))))
               (should (= 1 (plist-get events :count)))
               (should (eq 'created
                           (plist-get (car (plist-get events :events))
                                      :action))))))
       (ignore-errors (delete-directory dir t))))))

(provide 'nelisp-daemon-test)
;;; nelisp-daemon-test.el ends here
