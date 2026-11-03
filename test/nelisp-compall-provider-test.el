;;; nelisp-compall-provider-test.el --- generated provider eligibility -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defconst compall-provider-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
;; Read just the bounded helper and budget; loading the generator runs its CLI.
(with-temp-buffer
  (insert-file-contents (expand-file-name "tools/ai/compall-gen.el" compall-provider-test-root))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (memq (nth 1 form) '(compall-gen--embedded-bytecode-providers compall-gen-meta-budget))
            (eval form t))))
    (end-of-file nil)))
(ert-deftest compall-provider-canonical-budget ()
  (should (= compall-gen-meta-budget 350000)))
(ert-deftest compall-provider-actual-assembly ()
  (let* ((default-directory compall-provider-test-root)
         (names (compall-gen--embedded-bytecode-providers)))
    (should (= (length names) 73))
    (should (memq 'nelisp--tf-fdiv names))
    (should-not (memq 'set-match-data names))))
(ert-deftest compall-provider-guards-and-negative-controls ()
  (let* ((root (make-temp-file "compall-providers-" t))
         (default-directory (file-name-as-directory root))
         (path (expand-file-name "scripts/nelisp-stdlib-prelude.el")))
    (unwind-protect
        (progn
          (make-directory "scripts")
          (with-temp-file path
            (insert ";;; BEGIN GENERATED fixture\n")
            (prin1 `(unless (fboundp 'guarded)
                      (fset 'guarded ,(byte-compile '(lambda (x) x)))) (current-buffer))
            (insert "\n(fset 'source-only (lambda () 1))\n")
            (insert "'(fset 'quoted #[0 \"\\300\\207\" [nil] 1])\n")
            (insert ";;; END GENERATED fixture\n"))
          (should (equal (compall-gen--embedded-bytecode-providers) '(guarded)))
          (with-temp-file path (insert ";;; BEGIN GENERATED missing-end\n"))
          (should-error (compall-gen--embedded-bytecode-providers)))
      (delete-directory root t))))
(ert-run-tests-batch-and-exit)
