;;; nelisp-tools-test.el --- Tests for Phase 5-E §3.3 initial tools -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT coverage for `src/nelisp-tools.el'.  Each test resets the tool
;; registry and re-invokes `nelisp-tools-register-all' so the suite
;; is deterministic regardless of which other test file ran first.
;; Tool handlers are exercised directly (not through JSON-RPC) except
;; where end-to-end dispatch matters.

;;; Code:

(require 'ert)
(require 'json)
(require 'nelisp-server)
(require 'nelisp-tools)

;;; Helpers ----------------------------------------------------------

(defmacro nelisp-tools-test--with-tools (&rest body)
  "Clear the tool registry, re-register Phase 5-E initial tools,
run BODY with server started, tear everything down afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (clrhash nelisp-server--tool-registry)
         (nelisp-tools-register-all)
         (nelisp-server-start)
         ,@body)
     (nelisp-server-stop)
     (clrhash nelisp-server--tool-registry)
     (nelisp-tools--data-store-reset nil)))

(defun nelisp-tools-test--handler (name)
  "Return the handler lambda stored for tool NAME."
  (plist-get (gethash name nelisp-server--tool-registry) :handler))

;;; Registration smoke ---------------------------------------------

(ert-deftest nelisp-tools-test-all-registered ()
  (nelisp-tools-test--with-tools
    (dolist (n '("file-read" "file-outline" "git-log" "git-status"
                 "http-get" "data-get-path" "data-set-path"))
      (should (gethash n nelisp-server--tool-registry)))))

(ert-deftest nelisp-tools-test-tools-list-surface ()
  "tools/list returns every registered tool with a description."
  (nelisp-tools-test--with-tools
    (let* ((result (nelisp-server-dispatch "tools/list" nil))
           (names (mapcar (lambda (spec) (plist-get spec :name))
                          (plist-get result :tools))))
      (dolist (expected '("file-read" "file-outline" "git-log"
                          "git-status" "http-get"
                          "data-get-path" "data-set-path"))
        (should (member expected names))))))

;;; file-read -------------------------------------------------------

(ert-deftest nelisp-tools-test-file-read-roundtrip ()
  (nelisp-tools-test--with-tools
    (let* ((tmp (make-temp-file "nelisp-tools-"))
           (body "hello\nnelisp phase5e.3\n"))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (let ((result (funcall (nelisp-tools-test--handler "file-read")
                                   `((path . ,tmp)))))
              (should (string= body (plist-get result :content)))
              (should (string= tmp (plist-get result :path)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-read-missing-path ()
  (nelisp-tools-test--with-tools
    (should-error
     (funcall (nelisp-tools-test--handler "file-read") nil))))

(ert-deftest nelisp-tools-test-file-read-via-dispatch ()
  "End-to-end tools/call through dispatch, not handler direct."
  (nelisp-tools-test--with-tools
    (let* ((tmp (make-temp-file "nelisp-tools-"))
           (body "dispatch e2e\n"))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (let* ((result (nelisp-server-dispatch
                            "tools/call"
                            `((name . "file-read")
                              (arguments . ((path . ,tmp))))))
                   (content (plist-get result :content)))
              (should (string-match-p "dispatch e2e"
                                      (plist-get (car content) :text)))))
        (delete-file tmp)))))

;;; file-outline ----------------------------------------------------

(ert-deftest nelisp-tools-test-file-outline-elisp ()
  (nelisp-tools-test--with-tools
    (let* ((tmp (make-temp-file "nelisp-tools-" nil ".el"))
           (body (concat "(defun foo () 1)\n"
                         "(defvar bar 2)\n"
                         "(defmacro baz () '(3))\n"
                         "(defconst qux 4)\n")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (let* ((result (funcall (nelisp-tools-test--handler "file-outline")
                                    `((path . ,tmp))))
                   (outline (plist-get result :outline))
                   (names (mapcar (lambda (e) (plist-get e :name)) outline)))
              (should (= 4 (plist-get result :count)))
              (should (equal '("foo" "bar" "baz" "qux") names))
              (should (string= "el" (plist-get result :format)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-outline-org ()
  (nelisp-tools-test--with-tools
    (let* ((tmp (make-temp-file "nelisp-tools-" nil ".org"))
           (body "* A\n** A1\n* B\n*** B1a\n"))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (let* ((result (funcall (nelisp-tools-test--handler "file-outline")
                                    `((path . ,tmp))))
                   (outline (plist-get result :outline))
                   (levels (mapcar (lambda (e) (plist-get e :level)) outline))
                   (titles (mapcar (lambda (e) (plist-get e :title)) outline)))
              (should (equal '(1 2 1 3) levels))
              (should (equal '("A" "A1" "B" "B1a") titles))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-outline-markdown ()
  (nelisp-tools-test--with-tools
    (let* ((tmp (make-temp-file "nelisp-tools-" nil ".md"))
           (body "# Top\n## Sub\n# Top2\n"))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert body))
            (let* ((result (funcall (nelisp-tools-test--handler "file-outline")
                                    `((path . ,tmp))))
                   (outline (plist-get result :outline)))
              (should (= 3 (plist-get result :count)))
              (should (equal '(1 2 1)
                             (mapcar (lambda (e) (plist-get e :level))
                                     outline)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-outline-unsupported-ext ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-tools-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "plain"))
            (should-error
             (funcall (nelisp-tools-test--handler "file-outline")
                      `((path . ,tmp)))))
        (delete-file tmp)))))

;;; git-log / git-status -------------------------------------------

(ert-deftest nelisp-tools-test-git-log-shape ()
  "Runs `git log' in the nelisp repo (the test process' cwd).  The
test repo always has commits, so we only assert on the output
shape: non-empty string, exit 0, limit echoed back, every line
starts with a short SHA prefix."
  (nelisp-tools-test--with-tools
    (let* ((result (funcall (nelisp-tools-test--handler "git-log")
                            '((limit . 3)))))
      (should (= 0 (plist-get result :exit)))
      (should (= 3 (plist-get result :limit)))
      (let ((log (plist-get result :log)))
        (should (stringp log))
        (should (not (string-empty-p log)))
        ;; Every non-empty line begins with a short hex SHA + space.
        (dolist (line (split-string log "\n" t))
          (should (string-match-p "^[0-9a-f]\\{7,\\} " line)))))))

(ert-deftest nelisp-tools-test-git-log-default-limit ()
  (nelisp-tools-test--with-tools
    (let ((result (funcall (nelisp-tools-test--handler "git-log") nil)))
      (should (= 10 (plist-get result :limit))))))

(ert-deftest nelisp-tools-test-git-status-exit-zero ()
  "`git status' in a valid repo exits 0; output may be empty or not."
  (nelisp-tools-test--with-tools
    (let ((result (funcall (nelisp-tools-test--handler "git-status") nil)))
      (should (= 0 (plist-get result :exit)))
      (should (stringp (plist-get result :status))))))

;;; http-get --------------------------------------------------------

(ert-deftest nelisp-tools-test-http-get-loopback ()
  "Spawn an in-process HTTP/1.1 responder (per Phase 5-C.2 pattern),
GET via the http-get tool, assert :status 200 + body."
  (nelisp-tools-test--with-tools
    (let* ((port 0)
           (body "hello from http-get tool")
           (server (make-network-process
                    :name "nelisp-tools-http-srv"
                    :server t
                    :host 'local
                    :service port
                    :family 'ipv4
                    :filter
                    (lambda (proc _input)
                      (let* ((reply (format (concat "HTTP/1.1 200 OK\r\n"
                                                    "Content-Type: text/plain\r\n"
                                                    "Content-Length: %d\r\n\r\n"
                                                    "%s")
                                            (length body) body)))
                        (process-send-string proc reply)
                        (ignore-errors (delete-process proc))))))
           (port-num (process-contact server :service))
           (url (format "http://127.0.0.1:%d/" port-num)))
      (unwind-protect
          (let ((result (funcall (nelisp-tools-test--handler "http-get")
                                 `((url . ,url)))))
            (should (= 200 (plist-get result :status)))
            (should (string-match-p "hello from http-get tool"
                                    (plist-get result :body))))
        (delete-process server)
        (ignore-errors (nelisp-http-cache-clear))))))

(ert-deftest nelisp-tools-test-http-get-missing-url ()
  (nelisp-tools-test--with-tools
    (should-error
     (funcall (nelisp-tools-test--handler "http-get") nil))))

;;; data-get-path / data-set-path ----------------------------------

(ert-deftest nelisp-tools-test-data-set-then-get ()
  (nelisp-tools-test--with-tools
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "foo") (value . 42)))
    (let ((got (funcall (nelisp-tools-test--handler "data-get-path")
                        '((path . "foo")))))
      (should (= 42 (plist-get got :value)))
      (should (string= "foo" (plist-get got :path))))))

(ert-deftest nelisp-tools-test-data-nested-path ()
  (nelisp-tools-test--with-tools
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "a.b.c") (value . "deep")))
    (let ((got (funcall (nelisp-tools-test--handler "data-get-path")
                        '((path . "a.b.c")))))
      (should (string= "deep" (plist-get got :value))))))

(ert-deftest nelisp-tools-test-data-get-missing-returns-nil ()
  "Unset paths return nil in :value (distinguish from error)."
  (nelisp-tools-test--with-tools
    (let ((got (funcall (nelisp-tools-test--handler "data-get-path")
                        '((path . "absent.key")))))
      (should (null (plist-get got :value))))))

(ert-deftest nelisp-tools-test-data-set-overwrites-sibling-preserve ()
  "Writing a.b does not stomp a.c."
  (nelisp-tools-test--with-tools
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "a.b") (value . 1)))
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "a.c") (value . 2)))
    (should (= 1 (plist-get (funcall (nelisp-tools-test--handler "data-get-path")
                                      '((path . "a.b")))
                            :value)))
    (should (= 2 (plist-get (funcall (nelisp-tools-test--handler "data-get-path")
                                      '((path . "a.c")))
                            :value)))))

(ert-deftest nelisp-tools-test-data-invalid-path-errors ()
  (nelisp-tools-test--with-tools
    (should-error
     (funcall (nelisp-tools-test--handler "data-get-path")
              '((path . ""))))
    (should-error
     (funcall (nelisp-tools-test--handler "data-get-path")
              '((path . "a..b"))))))

(provide 'nelisp-tools-test)

;;; nelisp-tools-test.el ends here
