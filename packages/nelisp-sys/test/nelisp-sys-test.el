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

;;; nelisp-sys-test.el ends here
