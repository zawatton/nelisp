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

;;;; Step 4: self-verifying entries.

(defmacro nelisp-cachekey-test--with-dir (&rest body)
  `(let ((dir (make-temp-file "ck-cache-" t)))
     (unwind-protect (progn ,@body) (delete-directory dir t))))

(defconst nelisp-cachekey-test--key "0123456789abcdef0123456789abcdef01234567")
(defconst nelisp-cachekey-test--key2 "fedcba9876543210fedcba9876543210fedcba98")

(defun nelisp-cachekey-test--slurp (f)
  (with-temp-buffer (let ((coding-system-for-read 'utf-8-unix)) (insert-file-contents f)) (buffer-string)))
(defun nelisp-cachekey-test--spit (f text)
  (let ((coding-system-for-write 'utf-8-unix)) (with-temp-file f (insert text))))

(ert-deftest nelisp-cachekey-entry-roundtrip ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "u.o.K.unit" dir)))
     (nelisp-standalone-cachekey-entry-write f nelisp-cachekey-test--key "(:sections \"a\\n\")")
     (should (equal (nelisp-standalone-cachekey-entry-read f nelisp-cachekey-test--key)
                    '(t . "(:sections \"a\\n\")")))
     ;; No temp file is left behind.
     (should (equal (directory-files dir nil "\\.tmp\\'") nil)))))

(ert-deftest nelisp-cachekey-entry-rejects-corruption ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "u.unit" dir)) (k nelisp-cachekey-test--key))
     (nelisp-standalone-cachekey-entry-write f k "payload-1234")
     (let ((good (nelisp-cachekey-test--slurp f)))
       ;; Same size, one byte flipped: caught by the sha256, not the size.
       (nelisp-cachekey-test--spit f (replace-regexp-in-string "payload-1234" "payload-1235" good))
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . sha)))
       ;; Truncated payload.
       (nelisp-cachekey-test--spit f (substring good 0 (- (length good) 3)))
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . size)))
       ;; Header-less (old format) file.
       (nelisp-cachekey-test--spit f "(:sections nil)")
       (should (equal (nelisp-standalone-cachekey-entry-read f k) '(nil . header)))
       ;; Negative control: the intact entry still verifies.
       (nelisp-cachekey-test--spit f good)
       (should (car (nelisp-standalone-cachekey-entry-read f k)))
       ;; Missing file (another lane's prune): a miss, not an error.
       (should (equal (nelisp-standalone-cachekey-entry-read (expand-file-name "none" dir) k)
                      '(nil . missing)))))))

(ert-deftest nelisp-cachekey-entry-rejects-poisoned-key ()
  "An entry copied under another key's file name (the stale-hit failure) is refused."
  (nelisp-cachekey-test--with-dir
   (let ((old (expand-file-name "u.OLD.unit" dir)) (new (expand-file-name "u.NEW.unit" dir)))
     (nelisp-standalone-cachekey-entry-write old nelisp-cachekey-test--key "old-code")
     (copy-file old new)
     (should (equal (nelisp-standalone-cachekey-entry-read new nelisp-cachekey-test--key2)
                    '(nil . key)))
     ;; Negative control: with verification off the poison is accepted, which
     ;; is what the full-build gate uses to prove it can go RED.
     (let ((process-environment (cons "NELISP_STANDALONE_UNIT_CACHE_VERIFY=0" process-environment)))
       (should (equal (nelisp-standalone-cachekey-entry-read new nelisp-cachekey-test--key2)
                      '(t . "old-code")))))))

(ert-deftest nelisp-cachekey-atomic-write-unique-temp-names ()
  (nelisp-cachekey-test--with-dir
   (let ((f (expand-file-name "x.unit" dir)) (names nil))
     (cl-letf (((symbol-function 'rename-file)
                (lambda (from to &rest _) (push from names) (copy-file from to t) (delete-file from))))
       (dotimes (_ 20) (nelisp-standalone-cachekey-atomic-write f "t")))
     ;; pid + random bits: 20 writes use (almost surely) distinct names.
     (should (> (length (delete-dups (copy-sequence names))) 15))
     (should (cl-every (lambda (n) (string-match-p (format "\\.%d\\.[0-9a-f]+\\.tmp\\'" (emacs-pid)) n)) names)))))

(ert-deftest nelisp-cachekey-prune-only-in-private-dirs ()
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_DIR" nil)
    (setenv "NELISP_STANDALONE_UNIT_CACHE_NOPRUNE" nil)
    (setenv "NELISP_STANDALONE_UNIT_CACHE_PRUNE" nil)
    (should (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_DIR" "/some/shared")
    (should-not (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_PRUNE" "1")
    (should (nelisp-standalone-cachekey-prune-p))
    (setenv "NELISP_STANDALONE_UNIT_CACHE_NOPRUNE" "1")
    (should-not (nelisp-standalone-cachekey-prune-p))))

(provide 'nelisp-standalone-cachekey-test)
;;; nelisp-standalone-cachekey-test.el ends here
