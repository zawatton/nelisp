;;; nelisp-stdlib-defvaralias-callable.el --- callable variable aliases -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Callable `defvaralias' frontend for standalone NeLisp.  The VM installer
;; changes only variable-alias metadata; function cells remain independent.

;;; Code:

(require 'nelisp-gnu-bytecode-vm)
(require 'nelisp-bytecode)

(defconst nelisp-stdlib-defvaralias--bcl-form
  '(lambda (symbol target &optional docstring)
     (if (symbolp symbol)
         nil
       (signal 'wrong-type-argument (list 'symbolp symbol)))
     (if (symbolp target)
         nil
       (signal 'wrong-type-argument (list 'symbolp target)))
     (if (or (null docstring) (stringp docstring))
         nil
       (signal 'wrong-type-argument (list 'stringp docstring)))
     (let ((result (nelisp-variable-install-alias symbol target)))
       (if docstring
           (put symbol 'variable-documentation docstring)
         nil)
       result))
  "Source form compiled to the callable `defvaralias' BCL.")

(defconst nelisp-stdlib-defvaralias--vm-boundp-form
  '(lambda (symbol) (nelisp-variable-boundp symbol)))
(defconst nelisp-stdlib-defvaralias--vm-symbol-value-form
  '(lambda (symbol)
     (if (nelisp-variable-boundp symbol)
         (nelisp-variable-get symbol)
       (signal 'void-variable (list symbol)))))
(defconst nelisp-stdlib-defvaralias--vm-makunbound-form
  '(lambda (symbol) (nelisp-variable-makunbound symbol)))
(defconst nelisp-stdlib-defvaralias--vm-set-form
  '(lambda (symbol value) (nelisp-variable-put symbol value)))

(defun nelisp-stdlib-defvaralias-host-call (symbol target &optional docstring)
  "Alias variable SYMBOL to TARGET and return TARGET.
DOCSTRING, when non-nil, becomes SYMBOL's variable documentation.
The VM installer owns alias graph validation and mutation."
  (unless (symbolp symbol)
    (signal 'wrong-type-argument (list 'symbolp symbol)))
  (unless (symbolp target)
    (signal 'wrong-type-argument (list 'symbolp target)))
  (unless (or (null docstring) (stringp docstring))
    (signal 'wrong-type-argument (list 'stringp docstring)))
  (unless (fboundp 'nelisp-variable-install-alias)
    (signal 'nelisp-variable-alias-error
            (list :reason 'installer-unavailable symbol target)))
  (let ((result (nelisp-variable-install-alias symbol target)))
    (when docstring (put symbol 'variable-documentation docstring))
    result))

(defconst nelisp-stdlib-defvaralias--unbound
  (make-symbol "nelisp-stdlib-defvaralias-unbound"))

(defun nelisp-stdlib-defvaralias-host-vm-boundp (symbol)
  "Return whether SYMBOL has a value in the GNU byte-code VM store."
  (nelisp-variable-boundp symbol))

(defun nelisp-stdlib-defvaralias-host-vm-symbol-value (symbol)
  "Return SYMBOL's value from the GNU byte-code VM store."
  (let ((value (nelisp-variable-get symbol
                                    nelisp-stdlib-defvaralias--unbound)))
    (if (eq value nelisp-stdlib-defvaralias--unbound)
        (signal 'void-variable (list symbol))
      value)))

(defun nelisp-stdlib-defvaralias-host-vm-makunbound (symbol)
  "Detach an alias and unbind SYMBOL in the GNU byte-code VM store."
  (nelisp-variable-makunbound symbol)
  symbol)

(defun nelisp-stdlib-defvaralias-install-callable ()
  "Compile and install the VM `defvaralias' and variable adapter BCLs."
  (unless (fboundp 'nelisp-gnu-bytecode-vm-install-callable-bundle)
    (signal 'nelisp-variable-alias-error (list :reason 'function-store-unavailable)))
  (let* ((callable (nelisp-bc-run
                    (nelisp-bc-compile nelisp-stdlib-defvaralias--bcl-form)))
         (vm-boundp (nelisp-bc-run
                     (nelisp-bc-compile
                      nelisp-stdlib-defvaralias--vm-boundp-form)))
         (vm-symbol-value (nelisp-bc-run
                           (nelisp-bc-compile
                            nelisp-stdlib-defvaralias--vm-symbol-value-form)))
         (vm-makunbound (nelisp-bc-run
                         (nelisp-bc-compile
                          nelisp-stdlib-defvaralias--vm-makunbound-form)))
         (vm-set (nelisp-bc-run
                  (nelisp-bc-compile nelisp-stdlib-defvaralias--vm-set-form))))
    (unless (and (nelisp-bcl-p callable) (nelisp-bcl-p vm-boundp)
                 (nelisp-bcl-p vm-symbol-value) (nelisp-bcl-p vm-makunbound)
                 (nelisp-bcl-p vm-set))
      (signal 'nelisp-variable-alias-error (list :reason 'callable-bcl-compilation-failed)))
    (nelisp-gnu-bytecode-vm-install-callable-bundle
     (list (list 'defvaralias callable '(2 3))
           (list 'boundp vm-boundp '(1))
           (list 'symbol-value vm-symbol-value '(1))
           (list 'makunbound vm-makunbound '(1))
           (list 'set vm-set '(2))))
    'defvaralias))

(provide 'nelisp-stdlib-defvaralias-callable)
;;; nelisp-stdlib-defvaralias-callable.el ends here
