;;; standalone-gnu-variable-alias-callable-driver.el --- CALL2/CALL3 parity -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun nelisp-test-variable-alias-call-function (symbol)
  "Call the GNU ELC function named SYMBOL in the active runtime."
  (if (boundp 'emacs-version)
      (funcall symbol)
    (nelisp-bc-run (gethash symbol nelisp--functions) nil)))

(defun nelisp-test-variable-alias-get (symbol)
  (if (boundp 'emacs-version)
      (symbol-value symbol)
    (nelisp-variable-get symbol)))

(defun nelisp-test-variable-alias-set (symbol value)
  (if (boundp 'emacs-version)
      (set symbol value)
    (nelisp-variable-put symbol value)))

(defun nelisp-test-variable-alias-boundp (symbol)
  (if (boundp 'emacs-version)
      (boundp symbol)
    (nelisp-variable-boundp symbol)))

(defun nelisp-test-variable-alias-canonical (symbol)
  (if (boundp 'emacs-version)
      (indirect-variable symbol)
    (nelisp-variable-canonical-symbol symbol)))

(defun nelisp-test-variable-alias-callable-smoke ()
  (let ((source (getenv "NELISP_ALIAS_SOURCE"))
        (elc (getenv "NELISP_ALIAS_ELC")))
    (unless (and (stringp source) (not (file-exists-p source))
                 (stringp elc) (file-readable-p elc))
      (error "alias-callable: source-free ELC prerequisites failed"))
    (if (boundp 'emacs-version)
        (progn
          (let ((load-verbose nil)) (load elc nil t t))
          (unless (and
                   (byte-code-function-p (symbol-function 'nelisp-va-call-defvaralias-2))
                   (byte-code-function-p (symbol-function 'nelisp-va-call-defvaralias-3))
                   (memq 34 (append (aref (symbol-function 'nelisp-va-call-defvaralias-2) 1)
                                    nil))
                   (memq 35 (append (aref (symbol-function 'nelisp-va-call-defvaralias-3) 1)
                                    nil)))
            (error "alias-callable: ELC lacks genuine CALL2/CALL3 instructions")))
      (require 'nelisp-stdlib-defvaralias-callable)
      (nelisp-stdlib-defvaralias-install-callable)
      (unless (and (fboundp 'nelisp-variable-install-alias)
                   (nelisp-bcl-p (gethash 'defvaralias nelisp--functions))
                   (equal (gethash 'defvaralias
                                   (nelisp-gnu-bytecode-vm--live-call-signatures))
                          '(2 3)))
        (error "alias-callable: authenticated installer/callable is unavailable"))
      (nelisp-gnu-bytecode-vm-load-file elc))
    (princ "alias-callable: elc-loaded\n")
    (put 'nelisp-va-old-value 'nelisp-va-test-property 'old-plist)
    (put 'nelisp-va-base-value 'nelisp-va-test-property 'base-plist)
    (unless (eq (nelisp-test-variable-alias-call-function
                 'nelisp-va-call-defvaralias-2)
                'nelisp-va-base-value)
      (error "alias-callable: CALL2 target return mismatch"))
    (princ "alias-callable: call2-return-target\n")
    (unless (and (eq (nelisp-test-variable-alias-call-function
                      'nelisp-va-call-defvaralias-3)
                     'nelisp-va-base-value)
                 (equal (get 'nelisp-va-doc-value 'variable-documentation)
                        "alias doc"))
      (error "alias-callable: CALL3 target/docstring mismatch"))
    (princ "alias-callable: call3-return-target-and-doc\n")
    (unless (and (eq (nelisp-test-variable-alias-call-function
                      'nelisp-va-old-value) 'old-function)
                 (eq (nelisp-test-variable-alias-call-function
                      'nelisp-va-base-value) 'base-function)
                 (eq (get 'nelisp-va-old-value 'nelisp-va-test-property) 'old-plist)
                 (eq (get 'nelisp-va-base-value 'nelisp-va-test-property) 'base-plist))
      (error "alias-callable: variable alias changed function/plist cells"))
    (princ "alias-callable: independent-function-and-plist\n")
    (nelisp-test-variable-alias-set 'nelisp-va-old-value 'through-alias)
    (nelisp-test-variable-alias-call-function 'nelisp-va-call-chain)
    (unless (and (eq (nelisp-test-variable-alias-get 'nelisp-va-base-value)
                     'through-alias)
                 (eq (nelisp-test-variable-alias-get 'nelisp-va-chain-value)
                     'through-alias))
      (error "alias-callable: chained alias read/write mismatch"))
    (princ "alias-callable: variable-chain-read-write\n")
    (let* ((host-p (boundp 'emacs-version))
           (old-cell (if host-p
                         (symbol-function 'symbol-value)
                       (gethash 'symbol-value nelisp--functions)))
           (poison (if host-p
                       (lambda (_symbol) 'poisoned-public-cell)
                     (nelisp-bc-run
                      (nelisp-bc-compile
                       '(lambda (_symbol) 'poisoned-public-cell)))))
           (completed nil))
      (unless (and (if host-p (functionp old-cell) (nelisp-bcl-p old-cell))
                   (nelisp-test-variable-alias-call-function
                    'nelisp-va-call-symbol-value))
        (error "alias-callable: symbol-value intrinsic setup failed"))
      (unwind-protect
          (progn
            (if host-p
                (fset 'symbol-value poison)
              (puthash 'symbol-value poison nelisp--functions))
            (unless (eq (nelisp-test-variable-alias-call-function
                         'nelisp-va-call-symbol-value)
                        'through-alias)
              (error "alias-callable: ELC symbol-value opcode followed public redefinition"))
            (setq completed t))
        (if host-p
            (fset 'symbol-value old-cell)
          (puthash 'symbol-value old-cell nelisp--functions)))
      (unless completed
        (error "alias-callable: symbol-value intrinsic check did not complete")))
    (princ "alias-callable: symbol-value-intrinsic-stable\n")
    (let ((unbound-result
           (condition-case err
               (list 'returned (nelisp-test-variable-alias-call-function
                                'nelisp-va-call-symbol-value-unbound))
             (error (list (car err) (cdr err)))))
          (nonsymbol-result
           (condition-case err
               (list 'returned (nelisp-test-variable-alias-call-function
                                'nelisp-va-call-symbol-value-nonsymbol))
             (error (list (car err) (cdr err)))))
          (unbound-expected
           (if (boundp 'emacs-version)
               '(void-variable (nelisp-va-never-bound-7f31))
             '(void-variable (nelisp-va-never-bound-7f31))))
          (nonsymbol-expected '(wrong-type-argument (symbolp 17))))
      (unless (equal unbound-result unbound-expected)
        (error "alias-callable: opcode74 unbound condition/payload mismatch: %S"
               unbound-result))
      (unless (equal nonsymbol-result nonsymbol-expected)
        (error "alias-callable: opcode74 non-symbol condition/payload mismatch: %S"
               nonsymbol-result))
      (princ "alias-callable: symbol-value-intrinsic-errors\n"))
    (unless (equal (nelisp-test-variable-alias-call-function
                    'nelisp-va-call-dynamic-alias)
                   '(dynamic-value . dynamic-value))
      (error "alias-callable: dynamic alias value mismatch"))
    (unless (eq (nelisp-test-variable-alias-get 'nelisp-va-base-value)
                'through-alias)
      (error "alias-callable: dynamic alias binding was not restored"))
    (princ "alias-callable: dynamic-let-restored\n")
    (let ((rejected
           (condition-case nil
               (progn (nelisp-test-variable-alias-call-function
                       'nelisp-va-call-cycle) nil)
             (error t))))
      (unless (and rejected
                   (eq (nelisp-test-variable-alias-canonical 'nelisp-va-old-value)
                       'nelisp-va-base-value)
                   (eq (nelisp-test-variable-alias-get 'nelisp-va-base-value)
                       'through-alias))
      (error "alias-callable: prospective cycle was not refused atomically")))
    (princ "alias-callable: cycle-refusal-atomic\n")
    (unless (eq (nelisp-test-variable-alias-call-function
                 'nelisp-va-call-makunbound-alias)
                'nelisp-va-old-value)
      (error "alias-callable: makunbound return mismatch"))
    (unless (and (not (nelisp-test-variable-alias-boundp 'nelisp-va-old-value))
                 (nelisp-test-variable-alias-boundp 'nelisp-va-base-value)
                 (eq (nelisp-test-variable-alias-canonical 'nelisp-va-old-value)
                     'nelisp-va-old-value)
                 (eq (nelisp-test-variable-alias-get 'nelisp-va-base-value)
                     'through-alias))
      (error "alias-callable: makunbound did not detach alias"))
    (princ "alias-callable: makunbound-detaches\n")
    (princ "alias-callable: PASS\n")
    t))

(unless (nelisp-test-variable-alias-callable-smoke)
  (error "alias-callable: smoke returned nil"))
