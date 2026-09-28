;;; nelisp-autoload-transaction-test.el --- Autoload transaction parity -*- lexical-binding: t; -*-

(defvar nelisp-autoload-transaction-test--fixture-root
  "./test/fixtures/nelisp-autoload/")
(defvar nelisp-autoload-transaction-test--failures nil)

(defun nelisp-autoload-transaction-test--check (name thunk expected)
  (let ((actual (condition-case err
                    (funcall thunk)
                  (error (list 'signal (car err))))))
    (if (equal actual expected)
        (princ (format "PASS %s\n" name))
      (setq nelisp-autoload-transaction-test--failures
            (cons name nelisp-autoload-transaction-test--failures))
      (princ (format "FAIL %s expected=%S actual=%S\n"
                     name expected actual)))))

(defun nelisp-autoload-transaction-test--file (name)
  (expand-file-name
   (concat nelisp-autoload-transaction-test--fixture-root name)))

(nelisp-autoload-transaction-test--check
 "explicit-success"
 (lambda ()
   (autoload 'nelisp-autoload-success-target
             (nelisp-autoload-transaction-test--file "success.el"))
   (let ((function
          (autoload-do-load
           (symbol-function 'nelisp-autoload-success-target)
           'nelisp-autoload-success-target)))
     (list (funcall function 4)
           (autoloadp (symbol-function 'nelisp-autoload-success-target))
           (featurep 'nelisp-autoload-success-feature))))
 '(5 nil t))

(nelisp-autoload-transaction-test--check
 "macro-only-skip"
 (lambda ()
   (autoload 'nelisp-autoload-macro-mismatch-target
             (nelisp-autoload-transaction-test--file "macro-mismatch.el"))
   (let* ((before (symbol-function 'nelisp-autoload-macro-mismatch-target))
          (after (autoload-do-load before
                                   'nelisp-autoload-macro-mismatch-target
                                   'macro)))
     (list (equal before after)
           (equal before (symbol-function 'nelisp-autoload-macro-mismatch-target))
           (featurep 'nelisp-autoload-macro-mismatch-feature))))
 '(t t nil))

(nelisp-autoload-transaction-test--check
 "missing-target-commits-load"
 (lambda ()
   (autoload 'nelisp-autoload-missing-target
             (nelisp-autoload-transaction-test--file "missing-target.el"))
   (let* ((before (symbol-function 'nelisp-autoload-missing-target))
          (failed (condition-case nil
                      (progn (autoload-do-load before 'nelisp-autoload-missing-target)
                             nil)
                    (error t))))
     (list failed
           (equal before (symbol-function 'nelisp-autoload-missing-target))
           (fboundp 'nelisp-autoload-missing-side-effect)
           (featurep 'nelisp-autoload-missing-feature))))
 '(t t t t))

(nelisp-autoload-transaction-test--check
 "defalias-and-provide-rollback"
 (lambda ()
   (fset 'nelisp-autoload-rollback-existing (lambda () 'original))
   (autoload 'nelisp-autoload-rollback-target
             (nelisp-autoload-transaction-test--file "rollback-error.el"))
   (condition-case nil
       (autoload-do-load (symbol-function 'nelisp-autoload-rollback-target)
                         'nelisp-autoload-rollback-target)
     (error nil))
   (list (nelisp-autoload-rollback-existing)
         (featurep 'nelisp-autoload-rollback-feature)
         (fboundp 'nelisp-autoload-rollback-new)))
 '(original nil t))

(nelisp-autoload-transaction-test--check
 "raw-fset-is-not-queued"
 (lambda ()
   (fset 'nelisp-autoload-fset-existing (lambda () 'original))
   (autoload 'nelisp-autoload-fset-target
             (nelisp-autoload-transaction-test--file "fset-error.el"))
   (condition-case nil
       (autoload-do-load (symbol-function 'nelisp-autoload-fset-target)
                         'nelisp-autoload-fset-target)
     (error nil))
   (list (nelisp-autoload-fset-existing)
         (featurep 'nelisp-autoload-fset-feature)))
 '(changed nil))

(nelisp-autoload-transaction-test--check
 "nested-child-before-parent-provide"
 (lambda ()
   (fset 'nelisp-autoload-nested-after-outer (lambda () 'original))
   (autoload 'nelisp-autoload-nested-after-target
             (nelisp-autoload-transaction-test--file "nested-after.el"))
   (condition-case nil
       (autoload-do-load (symbol-function 'nelisp-autoload-nested-after-target)
                         'nelisp-autoload-nested-after-target)
     (error nil))
   (list (nelisp-autoload-nested-after-outer)
         (featurep 'nelisp-autoload-nested-after-outer-feature)
         (nelisp-autoload-nested-after-inner)
         (featurep 'nelisp-autoload-nested-after-inner-feature)))
 '(original nil inner t))

(nelisp-autoload-transaction-test--check
 "nested-parent-provide-before-child"
 (lambda ()
   (autoload 'nelisp-autoload-nested-before-target
             (nelisp-autoload-transaction-test--file "nested-before.el"))
   (condition-case nil
       (autoload-do-load (symbol-function 'nelisp-autoload-nested-before-target)
                         'nelisp-autoload-nested-before-target)
     (error nil))
   (list (featurep 'nelisp-autoload-nested-before-outer-feature)
         (featurep 'nelisp-autoload-nested-before-inner-feature)
         (fboundp 'nelisp-autoload-nested-before-inner)))
 '(nil nil t))

(nelisp-autoload-transaction-test--check
 "ordinary-load-remains-nontransactional"
 (lambda ()
   (fset 'nelisp-autoload-ordinary-load-existing (lambda () 'original))
   (condition-case nil
       (load (nelisp-autoload-transaction-test--file "ordinary-load-error.el"))
     (error nil))
   (list (nelisp-autoload-ordinary-load-existing)
         (featurep 'nelisp-autoload-ordinary-load-feature)
         (fboundp 'nelisp-autoload-ordinary-load-new)))
 '(changed t t))

(unless (null nelisp-autoload-transaction-test--failures)
  (error "Autoload transaction failures: %S"
         (nreverse nelisp-autoload-transaction-test--failures)))

(princ "ALL AUTOLOAD TRANSACTION CASES PASSED\n")

;;; nelisp-autoload-transaction-test.el ends here
