;;; nelisp-sys-test.el --- ERT smoke tests for nelisp-sys -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Package-level smoke tests.  Per-stage behavior is tested in the
;; sibling nelisp-sys-<topic>-test.el files.  These tests must keep
;; passing after extraction (Doc 130 testing strategy).

;;; Code:

(require 'ert)
(require 'nelisp-sys)
(require 'nelisp-sys-adapter-nelisp)

(ert-deftest nelisp-sys-loads ()
  "The package aggregator loads and exposes a version string."
  (should (featurep 'nelisp-sys))
  (should (stringp (nelisp-sys-version)))
  (should (string-match-p "\\." (nelisp-sys-version))))

(ert-deftest nelisp-sys-error-is-defined ()
  "The package error symbol exists and is an `error' subtype."
  (should (get 'nelisp-sys-error 'error-conditions))
  (should (memq 'error (get 'nelisp-sys-error 'error-conditions))))

(ert-deftest nelisp-sys-adapter-availability-is-boolean ()
  "The adapter availability predicate returns a definite boolean."
  (let ((v (nelisp-sys-adapter-available-p)))
    (should (memq v '(nil t)))))

(ert-deftest nelisp-sys-getenv-reads-process-environment ()
  "Doc 44 getenv helper reads NAME from the active environment."
  (let ((process-environment
         (cons "NELISP_SYS_TEST_ENV=ok" process-environment)))
    (should (equal (nelisp-sys-getenv "NELISP_SYS_TEST_ENV") "ok"))
    (should (null (nelisp-sys-getenv "NELISP_SYS_TEST_MISSING")))))

(ert-deftest nelisp-sys-executable-find-searches-path ()
  "Doc 44 executable resolver searches PATH and checks X_OK."
  ;; POSIX-only: the probe is an extension-less `#!/bin/sh' script made
  ;; executable via chmod #o755 — on the Windows CI runner there is no
  ;; X_OK mode bit and only PATHEXT suffixes count as executable, so
  ;; the resolver (correctly) does not find it there.
  (skip-unless (memq system-type '(gnu/linux darwin berkeley-unix)))
  (let* ((dir (make-temp-file "nelisp-sys-path-" t))
         (exe (expand-file-name "tool" dir))
         (process-environment (list (concat "PATH=" dir))))
    (unwind-protect
        (progn
          (write-region "#!/bin/sh\nexit 0\n" nil exe nil 'silent)
          (set-file-modes exe #o755)
          (should (equal (nelisp-sys-executable-find "tool") exe))
          (should (equal (nelisp-sys-executable-find exe) exe))
          (should (null (nelisp-sys-executable-find "missing"))))
      (delete-directory dir t))))

(ert-deftest nelisp-sys-startup-metadata-host-fallbacks ()
  "Doc 44 startup metadata names are exposed in hosted mode."
  (should (integerp (nelisp-startup-argc)))
  (should (listp (nelisp-startup-argv)))
  (should (listp (nelisp-startup-envp)))
  (should (= (nelisp-sys-startup-argc) (nelisp-startup-argc))))

(ert-deftest nelisp-sys-portable-syscall-number-hides-platform-nrs ()
  "Portable syscall lookup keeps raw target numbers behind nelisp-sys."
  (should (= (nelisp-sys-syscall-number 'fork 'gnu/linux) 57))
  (should (= (nelisp-sys-syscall-number "wait4" 'gnu/linux) 61))
  (should (= (nelisp-sys-syscall-number 'fork 'darwin) 2))
  (should (= (nelisp-sys-syscall-number 'wait4 'darwin) 7))
  (should-error (nelisp-sys-syscall-number 'fork 'windows)
                :type 'nelisp-sys-error))

(ert-deftest nelisp-sys-portable-syscall-dispatches-through-runtime-binding ()
  "Symbolic syscall invocation resolves the number before calling runtime.
`nelisp-sys-syscall' resolves NAME through the CURRENT platform's
table (kill = 62 on gnu/linux but 37 on darwin), so the expected
number must be looked up per-host instead of hardcoding the Linux
value.  Skipped on platforms without a syscall table (windows)."
  (skip-unless (assq (nelisp-sys-current-platform)
                     nelisp-sys--portable-syscall-table))
  (let ((kill-nr (nelisp-sys-syscall-number 'kill))
        seen)
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest args) (setq seen args) 123)))
      (should (= (nelisp-sys-syscall 'kill 44 15) 123))
      (should (equal seen (list kill-nr 44 15 0 0 0 0))))))

(ert-deftest nelisp-sys-cstring-helpers-forward-to-os-layer ()
  "Doc 44 C string helpers allocate through the OS substrate."
  (let (freed)
    (clrhash nelisp-sys--cstring-array-owners)
    (cl-letf (((symbol-function 'nelisp-sys--require-os) (lambda (_op) t))
              ((symbol-function 'nelisp-os--alloc-cstring)
               (lambda (s) (should (equal s "abc")) 55))
              ((symbol-function 'nelisp-os--build-cstr-array)
               (lambda (strings)
                 (should (equal strings '("prog" "arg")))
                 (cons 100 '(11 12))))
              ((symbol-function 'nelisp-os--free-cstr-array)
               (lambda (pair) (setq freed pair))))
      (should (= (nelisp-sys-cstring "abc") 55))
      (should (= (nelisp-sys-cstring-array '("prog" "arg")) 100))
      (nelisp-sys-free-cstring-array 100)
      (should (equal freed (cons 100 '(11 12))))
      (should (null (gethash 100 nelisp-sys--cstring-array-owners))))))

(ert-deftest nelisp-sys-os-wrappers-forward-and-copy-bytes ()
  "Doc 44 L1 wrappers connect to existing `nelisp-os-*' primitives."
  (let (writes close-seen execve-call exit-code)
    (cl-letf (((symbol-function 'nelisp-sys--require-os) (lambda (_op) t))
              ((symbol-function 'nelisp-os-open)
               (lambda (path flags mode)
                 (should (equal (list path flags mode)
                                '("out" 577 420)))
                 7))
              ((symbol-function 'nelisp-os-read)
               (lambda (fd len)
                 (should (equal (list fd len) '(7 3)))
                 "abc"))
              ((symbol-function 'ptr-write-u8)
               (lambda (ptr off byte)
                 (push (list ptr off byte) writes)
                 byte))
              ((symbol-function 'ptr-read-u8)
               (lambda (_ptr off) (+ ?x off)))
              ((symbol-function 'nelisp-os-write)
               (lambda (fd str)
                 (should (equal (list fd str) '(8 "xyz")))
                 3))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd) (setq close-seen fd) nil))
              ((symbol-function 'nelisp-os-exit)
               (lambda (code) (setq exit-code code) :exited))
              ((symbol-function 'nelisp-os-dup2)
               (lambda (old new) (should (equal (list old new) '(3 4))) 4))
              ((symbol-function 'nelisp-os-fork) (lambda () 1234))
              ((symbol-function 'nelisp-os-execve)
               (lambda (path argv envp)
                 (setq execve-call (list path argv envp))
                 :no-return))
              ((symbol-function 'nelisp-os-wait)
               (lambda (pid options)
                 (should (equal (list pid options) '(1234 0)))
                 (cons 1234 #x2a00)))
              ((symbol-function 'nelisp-os-pipe)
               (lambda () (cons 9 10))))
      (should (= (nelisp-sys-open "out" 577 #o644) 7))
      (should (= (nelisp-sys-read 7 200 3) 3))
      (should (equal (nreverse writes)
                     '((200 0 97) (200 1 98) (200 2 99))))
      (should (= (nelisp-sys-write 8 300 3) 3))
      (should (= (nelisp-sys-close 7) 0))
      (should (= close-seen 7))
      (should (eq (nelisp-sys-exit 127) :exited))
      (should (= exit-code 127))
      (should (= (nelisp-sys-dup2 3 4) 4))
      (should (= (nelisp-sys-fork) 1234))
      (should (eq (nelisp-sys-execve "/bin/echo" '("echo") '("A=B"))
                  :no-return))
      (should (equal execve-call '("/bin/echo" ("echo") ("A=B"))))
      (should (= (nelisp-sys-waitpid 1234 0) #x2a00))
      (should (equal (nelisp-sys-pipe) '(9 . 10))))))

;;; M0 increment 1 — filesystem primitive tests ----------------------

;; These tests exercise the host-fallback paths (no `nelisp--syscall'
;; binding).  They validate correctness on a regular Emacs and also
;; confirm that the standalone NeLisp binary path is reached when
;; `nelisp--syscall' IS bound (tested by the standalone test runner).

(ert-deftest nelisp-sys-chdir-changes-default-directory ()
  "chdir host fallback sets `default-directory' and returns 0."
  (skip-unless (not (fboundp 'nelisp--syscall)))
  (let* ((tmp (make-temp-file "nelisp-sys-chdir-" t))
         (orig default-directory))
    (unwind-protect
        (progn
          (should (= (nelisp-sys-chdir tmp) 0))
          (should (equal (file-name-as-directory tmp) default-directory)))
      (setq default-directory orig)
      (delete-directory tmp))))

(ert-deftest nelisp-sys-chdir-signals-on-nonexistent ()
  "chdir signals `nelisp-sys-error' for a nonexistent path."
  (skip-unless (not (fboundp 'nelisp--syscall)))
  (should-error (nelisp-sys-chdir "/this/path/does/not/exist/nelisp-test")
                :type 'nelisp-sys-error))

(ert-deftest nelisp-sys-chdir-type-check ()
  "chdir signals `wrong-type-argument' for non-string input."
  (should-error (nelisp-sys-chdir 42) :type 'wrong-type-argument))

(ert-deftest nelisp-sys-getcwd-returns-string ()
  "getcwd returns the current working directory as a string (host only).
Skipped on standalone: getcwd is deferred (M0 increment 2)."
  ;; getcwd signals on standalone until a buffer syscall builtin is wired.
  (skip-unless (not (fboundp 'nelisp--syscall-path-int)))
  (let ((cwd (nelisp-sys-getcwd)))
    (should (stringp cwd))
    (should (> (length cwd) 0))))

(ert-deftest nelisp-sys-chdir-then-getcwd ()
  "chdir followed by getcwd returns the new directory (host fallback path)."
  ;; getcwd is deferred on standalone; skip when standalone builtins present.
  (skip-unless (not (fboundp 'nelisp--syscall-path-int)))
  (let* ((tmp (make-temp-file "nelisp-sys-cg-" t))
         (orig default-directory))
    (unwind-protect
        (progn
          (nelisp-sys-chdir tmp)
          (let ((cwd (nelisp-sys-getcwd)))
            ;; Both paths should resolve to the same real path; use
            ;; file-truename to handle any symlinks in /tmp.
            (should (equal (file-truename cwd)
                           (file-truename tmp)))))
      (setq default-directory orig)
      (delete-directory tmp))))

(ert-deftest nelisp-sys-symlink-creates-link ()
  "symlink creates a symbolic link that file-symlink-p can confirm (host only).
Skipped on standalone: symlink is deferred (M0 increment 2)."
  ;; symlink signals on standalone until a 2-path syscall builtin is wired.
  (skip-unless (not (fboundp 'nelisp--syscall-path-int)))
  (let* ((tmp (make-temp-file "nelisp-sys-sl-" t))
         (target (expand-file-name "target.txt" tmp))
         (link   (expand-file-name "link.txt" tmp)))
    (unwind-protect
        (progn
          (write-region "hello" nil target nil 'silent)
          (should (= (nelisp-sys-symlink target link) 0))
          (should (file-symlink-p link))
          (should (equal (file-truename link) (file-truename target))))
      (delete-directory tmp t))))

(ert-deftest nelisp-sys-symlink-type-checks ()
  "symlink signals `wrong-type-argument' for non-string arguments."
  (should-error (nelisp-sys-symlink 1 "link") :type 'wrong-type-argument)
  (should-error (nelisp-sys-symlink "target" 2) :type 'wrong-type-argument))

(ert-deftest nelisp-sys-rmdir-removes-empty-dir ()
  "rmdir removes an empty directory and it no longer exists afterwards."
  (let* ((parent (make-temp-file "nelisp-sys-rd-" t))
         (subdir (expand-file-name "empty" parent)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (should (file-directory-p subdir))
          (should (= (nelisp-sys-rmdir subdir) 0))
          (should (not (file-exists-p subdir))))
      (when (file-directory-p parent)
        (delete-directory parent t)))))

(ert-deftest nelisp-sys-rmdir-type-check ()
  "rmdir signals `wrong-type-argument' for non-string input."
  (should-error (nelisp-sys-rmdir 99) :type 'wrong-type-argument))

(ert-deftest nelisp-sys-mkdtemp-creates-unique-dirs ()
  "mkdtemp creates a new directory that exists and is unique across calls."
  (let ((dir1 (nelisp-sys-mkdtemp temporary-file-directory))
        (dir2 (nelisp-sys-mkdtemp temporary-file-directory)))
    (unwind-protect
        (progn
          (should (stringp dir1))
          (should (file-directory-p dir1))
          (should (stringp dir2))
          (should (file-directory-p dir2))
          (should (not (equal dir1 dir2))))
      (when (file-directory-p dir1) (delete-directory dir1))
      (when (file-directory-p dir2) (delete-directory dir2)))))

(ert-deftest nelisp-sys-mkdtemp-type-check ()
  "mkdtemp signals `wrong-type-argument' for non-string template."
  (should-error (nelisp-sys-mkdtemp nil) :type 'wrong-type-argument))

(ert-deftest nelisp-sys-getcwd-no-trailing-slash ()
  "getcwd result has no trailing slash (POSIX getcwd(2) convention, host only).
Skipped on standalone: getcwd is deferred (M0 increment 2)."
  ;; getcwd signals on standalone until a buffer syscall builtin is wired.
  (skip-unless (not (fboundp 'nelisp--syscall-path-int)))
  (let ((cwd (nelisp-sys-getcwd)))
    (should (stringp cwd))
    ;; Root \"/\" is the only legitimate trailing-slash case; skip it.
    (unless (equal cwd "/")
      (should (not (string-suffix-p "/" cwd))))))

(ert-deftest nelisp-sys-portable-syscall-table-has-new-entries ()
  "Syscall table contains the M0 increment-1 entries for gnu/linux."
  (should (= (nelisp-sys-syscall-number 'chdir 'gnu/linux) 80))
  (should (= (nelisp-sys-syscall-number 'symlink 'gnu/linux) 88))
  (should (= (nelisp-sys-syscall-number 'rmdir 'gnu/linux) 84))
  (should (= (nelisp-sys-syscall-number 'getcwd 'gnu/linux) 79))
  (should (= (nelisp-sys-syscall-number 'chdir 'darwin) 12))
  (should (= (nelisp-sys-syscall-number 'symlink 'darwin) 57))
  (should (= (nelisp-sys-syscall-number 'rmdir 'darwin) 137))
  ;; getcwd is intentionally absent on Darwin.
  (should-error (nelisp-sys-syscall-number 'getcwd 'darwin)
                :type 'nelisp-sys-error))

;;; nelisp-sys-test.el ends here
