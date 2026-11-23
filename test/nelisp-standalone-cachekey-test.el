;;; nelisp-standalone-cachekey-test.el --- incremental build key tests -*- lexical-binding: t; -*-

;; Unit-level tests for the standalone unit-cache key, entry verification
;; and reproducible-identity helpers.  No build is run.
;; Run: emacs --batch -Q -L scripts -L lisp -L src -l test/nelisp-standalone-cachekey-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'nelisp-version)
(require 'nelisp-standalone-cachekey)

(defconst nelisp-cachekey-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defmacro nelisp-cachekey-test--with-env (bindings &rest body)
  "Run BODY with `process-environment' extended by BINDINGS (VAR . VALUE or VAR)."
  `(let ((process-environment
          (append (mapcar (lambda (b) (if (consp b) (format "%s=%s" (car b) (cdr b)) b))
                          ',bindings)
                  process-environment)))
     ,@body))

(ert-deftest nelisp-cachekey-version-pinned-ignores-git ()
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) (NELISP_VERSION . "pin-1")
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root) "pin-1"))
    (should (equal (nelisp-build-source-revision nelisp-cachekey-test--root)
                   "0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-pinned-no-describe ()
  "Without NELISP_VERSION the pinned build still never calls git describe."
  (nelisp-cachekey-test--with-env
      ((NELISP_REPRODUCIBLE_BUILD . 1) "NELISP_VERSION="
       (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (should (equal (nelisp-build-version nelisp-cachekey-test--root)
                   "unknown-0123456789abcdef0123456789abcdef01234567"))))

(ert-deftest nelisp-cachekey-version-unpinned-negative-control ()
  "Negative control: without the flag the source revision is the git HEAD."
  (skip-unless (executable-find "git"))
  (nelisp-cachekey-test--with-env
      ("NELISP_REPRODUCIBLE_BUILD=" (NELISP_SOURCE_REVISION . "0123456789abcdef0123456789abcdef01234567"))
    (let ((rev (nelisp-build-source-revision nelisp-cachekey-test--root)))
      (skip-unless rev)
      (should-not (equal rev "0123456789abcdef0123456789abcdef01234567")))))

;;;; Step 3: key completeness.

(defun nelisp-cachekey-test--real-closure ()
  (let ((load-path (mapcar (lambda (d) (expand-file-name d nelisp-cachekey-test--root))
                           '("lisp" "src" "scripts"))))
    (nelisp-standalone-cachekey-require-closure
     nelisp-standalone-cachekey-root-features nelisp-cachekey-test--root load-path)))

(defun nelisp-cachekey-test--names (files) (mapcar #'file-name-nondirectory files))

(ert-deftest nelisp-cachekey-closure-has-former-holes ()
  (let ((names (nelisp-cachekey-test--names (nelisp-cachekey-test--real-closure))))
    (dolist (n '("nelisp-asm-x86_64.el" "nelisp-asm-arm64.el" "nelisp-sexp-layout.el"
                 "nelisp-cc-fxir-rows.el" "nelisp-cc-fxir-effects.el"
                 "nelisp-aot-compiler.el" "nelisp-cc-runtime.el"))
      (should (member n names)))
    ;; Only Emacs stdlib features may stay unresolved (they live outside the repo).
    (should-not (seq-some (lambda (n) (string-prefix-p "?nelisp" n)) names))))

(ert-deftest nelisp-cachekey-old-dep-list-misses-the-assembler ()
  "Negative control: the pre-audit list (8 libraries, no closure) lacks them."
  (let* ((load-path (mapcar (lambda (d) (expand-file-name d nelisp-cachekey-test--root))
                            '("lisp" "src" "scripts")))
         (old (delq nil (mapcar #'locate-library
                                '("nelisp-aot-compiler" "nelisp-standalone-arena-rewrite"
                                  "nelisp-static-linker" "nelisp-elf-write" "nelisp-cc-runtime"
                                  "nelisp-standalone-argframe.el" "nelisp-heap-accessors"
                                  "nelisp-cc-fxir-native")))))
    (should-not (member "nelisp-asm-x86_64.el" (nelisp-cachekey-test--names old)))
    (should-not (member "nelisp-sexp-layout.el" (nelisp-cachekey-test--names old)))
    (should-not (member "nelisp-cc-fxir-rows.el" (nelisp-cachekey-test--names old)))))

(defmacro nelisp-cachekey-test--with-tree (&rest body)
  "Run BODY with `dir' bound to a temp repo: lisp/a.el requires b, c (c is .elc-shadowed)."
  `(let* ((dir (make-temp-file "ck-tree-" t))
          (lisp (expand-file-name "lisp" dir)))
     (unwind-protect
         (progn
           (make-directory lisp)
           (with-temp-file (expand-file-name "a.el" lisp) (insert "(require 'b)\n(defun a () 1)\n"))
           (with-temp-file (expand-file-name "b.el" lisp) (insert "(defun b () 1)\n"))
           (with-temp-file (expand-file-name "c.el" lisp) (insert "(defun c () 1)\n"))
           (with-temp-file (expand-file-name "c.elc" lisp) (insert "stale-bytecode"))
           ,@body)
       (delete-directory dir t))))

(defun nelisp-cachekey-test--digest (dir roots)
  (let ((load-path (list (expand-file-name "lisp" dir))))
    (nelisp-standalone-cachekey-files-digest
     (nelisp-standalone-cachekey-require-closure roots dir load-path))))

(ert-deftest nelisp-cachekey-transitive-edit-changes-digest ()
  (nelisp-cachekey-test--with-tree
   (let ((d0 (nelisp-cachekey-test--digest dir '("a"))))
     (should (equal d0 (nelisp-cachekey-test--digest dir '("a"))))
     (with-temp-file (expand-file-name "lisp/b.el" dir) (insert "(defun b () 2)\n"))
     (should-not (equal d0 (nelisp-cachekey-test--digest dir '("a")))))))

(ert-deftest nelisp-cachekey-hashes-el-not-elc ()
  (nelisp-cachekey-test--with-tree
   (let ((d0 (nelisp-cachekey-test--digest dir '("c"))))
     ;; Editing the .el the loader runs must re-key even though a .elc exists.
     (with-temp-file (expand-file-name "lisp/c.el" dir) (insert "(defun c () 2)\n"))
     (should-not (equal d0 (nelisp-cachekey-test--digest dir '("c"))))
     ;; Negative control: touching only the stale .elc must NOT re-key.
     (let ((d1 (nelisp-cachekey-test--digest dir '("c"))))
       (with-temp-file (expand-file-name "lisp/c.elc" dir) (insert "other-stale-bytecode"))
       (should (equal d1 (nelisp-cachekey-test--digest dir '("c"))))))))

(ert-deftest nelisp-cachekey-missing-feature-is-recorded ()
  (nelisp-cachekey-test--with-tree
   (should (member "?nope" (let ((load-path (list (expand-file-name "lisp" dir))))
                             (nelisp-standalone-cachekey-require-closure '("nope") dir load-path))))))

(ert-deftest nelisp-cachekey-settings-follow-env ()
  (let ((base (nelisp-standalone-cachekey-settings-string (nelisp-standalone-cachekey-env-alist))))
    (dolist (var '("NELISP_STACKMAP" "NELISP_ALIGNHOT_PERTURB" "NC2_LAZY" "NELISP_LINUX_ARENA_SIZE"))
      (should (member var nelisp-standalone-cachekey-env-names))
      (let ((process-environment (cons (format "%s=1" var) process-environment)))
        (should-not (equal base (nelisp-standalone-cachekey-settings-string
                                 (nelisp-standalone-cachekey-env-alist))))))
    ;; Negative control: an unrelated variable does not move the key.
    (let ((process-environment (cons "UNRELATED_VAR=1" process-environment)))
      (should (equal base (nelisp-standalone-cachekey-settings-string
                           (nelisp-standalone-cachekey-env-alist)))))
    ;; Host Emacs version and live settings are part of the text.
    (should (string-match-p (regexp-quote emacs-version) base))
    (should-not (equal base (nelisp-standalone-cachekey-settings-string
                             (nelisp-standalone-cachekey-env-alist '((regalloc . t))))))))

(provide 'nelisp-standalone-cachekey-test)
;;; nelisp-standalone-cachekey-test.el ends here
