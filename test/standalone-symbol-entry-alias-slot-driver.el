;;; standalone-symbol-entry-alias-slot-driver.el --- Private record slot probe -*- lexical-binding: t; -*-

(let* ((target (make-symbol "alias-target"))
       (entry (nelisp-env--make-symbol-entry 17 'fn '(p v) t target))
       (copy (copy-sequence entry))
       (legacy (nelisp--make-record 'symbol-entry 1 2 nil nil)))
  (unless (and (nelisp-env--symbol-entry-p entry)
               (= (length entry) 6)
               (eq (nelisp-env--symbol-entry-alias-target entry) target)
               (= (nelisp-env--symbol-entry-value entry) 17)
               (eq (nelisp-env--symbol-entry-function entry) 'fn)
               (equal (nelisp-env--symbol-entry-plist entry) '(p v))
               (nelisp-env--symbol-entry-constant-p entry)
               (eq (nelisp-env--symbol-entry-alias-target copy) target)
               (eq (nelisp-env--symbol-entry-set-alias-target entry nil) nil)
               (null (nelisp-env--symbol-entry-alias-target entry))
               (eq (nelisp-env--symbol-entry-alias-target copy) target)
               (null (nelisp-env--symbol-entry-alias-target legacy))
               (null (nelisp-env--symbol-entry-set-alias-target legacy 'target))
               (= (length legacy) 5))
    (error "private alias slot probe failed")))
(dolist (bad (list nil (nelisp--make-record 'other 1 2 3 4 5)))
  (unless (and (condition-case nil
                   (progn (nelisp-env--symbol-entry-alias-target bad) nil)
                 (wrong-type-argument t))
               (condition-case nil
                   (progn (nelisp-env--symbol-entry-set-alias-target bad 'target) nil)
                 (wrong-type-argument t)))
    (error "alias slot accepted an invalid record")))
(princ "SYMBOL-ENTRY-ALIAS-SLOT-PASS\n")
nil

;;; standalone-symbol-entry-alias-slot-driver.el ends here
