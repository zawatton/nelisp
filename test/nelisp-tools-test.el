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
                 "http-get" "data-get-path" "data-set-path"
                 ;; Phase 5-F.1.2
                 "state-set" "state-get" "state-delete"
                 "state-list-ns" "state-list-keys" "state-count"
                 ;; Phase 5-F.2 (Doc 19)
                 "file-replace-string" "file-insert-at-line"
                 "file-delete-lines" "file-append" "file-read-snippet"
                 "git-diff-names" "git-head-sha"
                 "git-branch-current" "git-repo-root"
                 "data-list-keys" "data-delete-path"
                 ;; Phase 5-F.3.1 (Doc 20)
                 "org-read-outline-tree"))
      (should (gethash n nelisp-server--tool-registry)))))

(ert-deftest nelisp-tools-test-tools-list-surface ()
  "tools/list returns every registered tool with a description."
  (nelisp-tools-test--with-tools
    (let* ((result (nelisp-server-dispatch "tools/list" nil))
           (names (mapcar (lambda (spec) (plist-get spec :name))
                          (plist-get result :tools))))
      (dolist (expected '("file-read" "file-outline" "git-log"
                          "git-status" "http-get"
                          "data-get-path" "data-set-path"
                          "state-set" "state-get" "state-delete"
                          "state-list-ns" "state-list-keys" "state-count"
                          "file-replace-string" "file-insert-at-line"
                          "file-delete-lines" "file-append" "file-read-snippet"
                          "git-diff-names" "git-head-sha"
                          "git-branch-current" "git-repo-root"
                          "data-list-keys" "data-delete-path"
                          "org-read-outline-tree"))
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

;;; state-* (Phase 5-F.1.2) ----------------------------------------

(defmacro nelisp-tools-test--with-state-tools (&rest body)
  "Combine tool registration + fresh temp DB for state-* tests."
  (declare (indent 0) (debug t))
  `(let* ((tmpdir (make-temp-file "nelisp-tools-state-" t))
          (nelisp-state-db-path
           (expand-file-name "test-state.db" tmpdir)))
     (unwind-protect
         (progn
           (nelisp-state--close)
           (nelisp-tools-test--with-tools
             ,@body))
       (nelisp-state--close)
       (when (file-directory-p tmpdir)
         (delete-directory tmpdir t)))))

(ert-deftest nelisp-tools-test-state-set-get-roundtrip ()
  "state-set → state-get で value が round trip する (MCP 経由)。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (let ((set-r (funcall (nelisp-tools-test--handler "state-set")
                          '((key . "k") (value . 42)))))
      (should (eq t (plist-get set-r :stored))))
    (let ((get-r (funcall (nelisp-tools-test--handler "state-get")
                          '((key . "k")))))
      (should (= 42 (plist-get get-r :value))))))

(ert-deftest nelisp-tools-test-state-ns-isolation ()
  ":ns パラメータで namespace 境界が保たれる。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k") (value . "a-val") (ns . "a")))
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k") (value . "b-val") (ns . "b")))
    (let ((ra (funcall (nelisp-tools-test--handler "state-get")
                       '((key . "k") (ns . "a"))))
          (rb (funcall (nelisp-tools-test--handler "state-get")
                       '((key . "k") (ns . "b")))))
      (should (string= "a-val" (plist-get ra :value)))
      (should (string= "b-val" (plist-get rb :value))))))

(ert-deftest nelisp-tools-test-state-delete-returns-flag ()
  "state-delete: hit → :deleted t、miss → :deleted nil。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k") (value . 1)))
    (let ((r1 (funcall (nelisp-tools-test--handler "state-delete")
                       '((key . "k")))))
      (should (eq t (plist-get r1 :deleted))))
    (let ((r2 (funcall (nelisp-tools-test--handler "state-delete")
                       '((key . "k")))))
      (should-not (plist-get r2 :deleted)))))

(ert-deftest nelisp-tools-test-state-list-ns-and-keys ()
  "state-list-ns は sorted namespaces、state-list-keys は :ns 必須 + :limit。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k1") (value . 1) (ns . "zeta")))
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k2") (value . 2) (ns . "alpha")))
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k3") (value . 3) (ns . "alpha")))
    (let ((lr (funcall (nelisp-tools-test--handler "state-list-ns") nil)))
      (should (equal '("alpha" "zeta") (plist-get lr :namespaces))))
    (let ((kr (funcall (nelisp-tools-test--handler "state-list-keys")
                       '((ns . "alpha")))))
      (should (equal '("k2" "k3") (plist-get kr :keys))))
    (let ((kr (funcall (nelisp-tools-test--handler "state-list-keys")
                       '((ns . "alpha") (limit . 1)))))
      (should (equal '("k2") (plist-get kr :keys))))))

(ert-deftest nelisp-tools-test-state-count-total-and-ns ()
  "state-count は ns 省略で total、ns 指定で namespace 限定。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k1") (value . 1) (ns . "a")))
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k2") (value . 2) (ns . "a")))
    (funcall (nelisp-tools-test--handler "state-set")
             '((key . "k3") (value . 3) (ns . "b")))
    (let ((rt (funcall (nelisp-tools-test--handler "state-count") nil)))
      (should (= 3 (plist-get rt :count))))
    (let ((ra (funcall (nelisp-tools-test--handler "state-count")
                       '((ns . "a")))))
      (should (= 2 (plist-get ra :count))))))

(ert-deftest nelisp-tools-test-state-via-dispatch ()
  "End-to-end: tools/call name=state-set → state-get で value が戻る。"
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (nelisp-tools-test--with-state-tools
    (nelisp-server-dispatch
     "tools/call"
     '((name . "state-set")
       (arguments . ((key . "via-dispatch") (value . "ok")))))
    (let* ((result (nelisp-server-dispatch
                    "tools/call"
                    '((name . "state-get")
                      (arguments . ((key . "via-dispatch"))))))
           (content (plist-get result :content)))
      (should (string-match-p "\"ok\"\\|ok"
                              (or (plist-get (car content) :text) ""))))))

;;; Phase 5-F.2.1 file ops (Doc 19) -------------------------------

(ert-deftest nelisp-tools-test-file-replace-string-counts ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-replace-")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "foo bar foo baz foo"))
            (let ((r (funcall (nelisp-tools-test--handler "file-replace-string")
                              `((path . ,tmp) (old . "foo") (new . "FOO")))))
              (should (= 3 (plist-get r :replaced))))
            (with-temp-buffer
              (insert-file-contents tmp)
              (should (string= "FOO bar FOO baz FOO" (buffer-string)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-replace-string-empty-old-errors ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-replace-")))
      (unwind-protect
          (should-error
           (funcall (nelisp-tools-test--handler "file-replace-string")
                    `((path . ,tmp) (old . "") (new . "x"))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-insert-at-line-prepends ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-insert-")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "line2\nline3\n"))
            (funcall (nelisp-tools-test--handler "file-insert-at-line")
                     `((path . ,tmp) (line . 1) (content . "line1")))
            (with-temp-buffer
              (insert-file-contents tmp)
              (should (string= "line1\nline2\nline3\n" (buffer-string)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-delete-lines-removes-range ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-delete-")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
            (let ((r (funcall (nelisp-tools-test--handler "file-delete-lines")
                              `((path . ,tmp) (line . 2) (count . 2)))))
              (should (= 2 (plist-get r :deleted))))
            (with-temp-buffer
              (insert-file-contents tmp)
              (should (string= "a\nd\ne\n" (buffer-string)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-append-adds-content ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-append-")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "head\n"))
            (let ((r (funcall (nelisp-tools-test--handler "file-append")
                              `((path . ,tmp) (content . "tail\n")))))
              (should (= 5 (plist-get r :appended))))
            (with-temp-buffer
              (insert-file-contents tmp)
              (should (string= "head\ntail\n" (buffer-string)))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-file-read-snippet-window ()
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-snippet-")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (dotimes (i 10) (insert (format "line%d\n" (1+ i)))))
            (let ((r (funcall (nelisp-tools-test--handler "file-read-snippet")
                              `((path . ,tmp) (line . 5) (window . 4)))))
              ;; window 4 → half=2、line=5 なので start=3 / end=7
              (should (= 3 (plist-get r :start)))
              (should (= 7 (plist-get r :end)))
              (should (= 10 (plist-get r :total-lines)))
              (should (string-match-p "line3\nline4\nline5\nline6\nline7"
                                      (plist-get r :content)))))
        (delete-file tmp)))))

;;; Phase 5-F.2.2 git ops (Doc 19) ---------------------------------

(ert-deftest nelisp-tools-test-git-head-sha-shape ()
  "git-head-sha returns 40-char hex SHA when run inside this repo."
  (nelisp-tools-test--with-tools
    (let* ((r (funcall (nelisp-tools-test--handler "git-head-sha") nil))
           (sha (plist-get r :sha)))
      (should (= 0 (plist-get r :exit)))
      (should (= 40 (length sha)))
      (should (string-match-p "\\`[0-9a-f]+\\'" sha)))))

(ert-deftest nelisp-tools-test-git-branch-current-non-empty ()
  (nelisp-tools-test--with-tools
    (let* ((r (funcall (nelisp-tools-test--handler "git-branch-current") nil))
           (br (plist-get r :branch)))
      (should (= 0 (plist-get r :exit)))
      (should (stringp br))
      (should (> (length br) 0)))))

(ert-deftest nelisp-tools-test-git-repo-root-absolute ()
  (skip-unless (executable-find "git"))
  (skip-unless (file-directory-p (expand-file-name ".git" default-directory)))
  (nelisp-tools-test--with-tools
    (let* ((r (funcall (nelisp-tools-test--handler "git-repo-root") nil))
           (root (plist-get r :root)))
      (should (= 0 (plist-get r :exit)))
      (skip-unless (and (stringp root) (not (string-empty-p root))))
      (should (file-name-absolute-p root))
      (should (file-directory-p root)))))

(ert-deftest nelisp-tools-test-git-diff-names-list-shape ()
  "git-diff-names returns :files as a list (possibly empty)."
  (nelisp-tools-test--with-tools
    (let* ((r (funcall (nelisp-tools-test--handler "git-diff-names") nil))
           (files (plist-get r :files)))
      (should (= 0 (plist-get r :exit)))
      (should (listp files)))))

;;; Phase 5-F.2.3 data ops (Doc 19) --------------------------------

(ert-deftest nelisp-tools-test-data-list-keys-top-and-nested ()
  (nelisp-tools-test--with-tools
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "a.b") (value . 1)))
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "a.c") (value . 2)))
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "x") (value . 9)))
    (let ((r (funcall (nelisp-tools-test--handler "data-list-keys") nil)))
      (should (member ":a" (plist-get r :keys)))
      (should (member ":x" (plist-get r :keys))))
    (let ((r (funcall (nelisp-tools-test--handler "data-list-keys")
                      '((path . "a")))))
      (should (equal '(":b" ":c")
                     (sort (copy-sequence (plist-get r :keys))
                           #'string<))))))

(ert-deftest nelisp-tools-test-data-delete-path-unsets-value ()
  (nelisp-tools-test--with-tools
    (funcall (nelisp-tools-test--handler "data-set-path")
             '((path . "k") (value . "v")))
    (should (string= "v" (plist-get (funcall (nelisp-tools-test--handler "data-get-path")
                                              '((path . "k")))
                                     :value)))
    (funcall (nelisp-tools-test--handler "data-delete-path")
             '((path . "k")))
    (should-not (plist-get (funcall (nelisp-tools-test--handler "data-get-path")
                                     '((path . "k")))
                           :value))))

;;; Phase 5-F.3.1 org-read-outline-tree (Doc 20) ------------------

(ert-deftest nelisp-tools-test-org-tree-flat-3-headlines ()
  "Flat 3 level-1 headlines produce 3 root nodes with empty children."
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-org-tree-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert "* Alpha\n")
              (insert "* Beta\n")
              (insert "* Gamma\n"))
            (let* ((r (funcall (nelisp-tools-test--handler "org-read-outline-tree")
                               `((path . ,tmp))))
                   (tree (plist-get r :tree)))
              (should (= 3 (plist-get r :total-headlines)))
              (should (= 3 (length tree)))
              (should (string= "Alpha" (plist-get (nth 0 tree) :title)))
              (should-not (plist-get (nth 0 tree) :children))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-org-tree-nested-2-levels ()
  "Level 2 headlines become children of preceding level 1."
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-org-tree-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert "* Top\n")
              (insert "** Sub-A\n")
              (insert "** Sub-B\n")
              (insert "* Top2\n"))
            (let* ((r (funcall (nelisp-tools-test--handler "org-read-outline-tree")
                               `((path . ,tmp))))
                   (tree (plist-get r :tree)))
              (should (= 4 (plist-get r :total-headlines)))
              (should (= 2 (length tree)))
              (let* ((top (nth 0 tree))
                     (kids (plist-get top :children)))
                (should (string= "Top" (plist-get top :title)))
                (should (= 2 (length kids)))
                (should (string= "Sub-A" (plist-get (nth 0 kids) :title)))
                (should (string= "Sub-B" (plist-get (nth 1 kids) :title))))
              (should-not (plist-get (nth 1 tree) :children))))
        (delete-file tmp)))))

(ert-deftest nelisp-tools-test-org-tree-rejects-non-org-extension ()
  "Non-.org extension raises before file is read."
  (nelisp-tools-test--with-tools
    (let ((tmp (make-temp-file "nelisp-org-tree-" nil ".md")))
      (unwind-protect
          (should-error
           (funcall (nelisp-tools-test--handler "org-read-outline-tree")
                    `((path . ,tmp))))
        (delete-file tmp)))))

(provide 'nelisp-tools-test)

;;; nelisp-tools-test.el ends here
