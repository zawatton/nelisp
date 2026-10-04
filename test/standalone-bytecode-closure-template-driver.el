;;; standalone-bytecode-closure-template-driver.el --- Closure template smoke -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-compiler)

(defun nelisp-closure-template-smoke--elc-forms (path)
  "Read PATH as compiled forms without loading or evaluating it."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (goto-char (point-min))
    (unless (looking-at ";ELC")
      (error "Not an ELC file: %s" path))
    (forward-line 1)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file (nreverse forms))))))

(defun nelisp-closure-template-smoke--byte-code-functions (object)
  "Collect materialized byte-code templates nested in unread OBJECT data."
  (cond
   ((byte-code-function-p object) (list object))
   ((consp object)
    (append
     (nelisp-closure-template-smoke--byte-code-functions (car object))
     (nelisp-closure-template-smoke--byte-code-functions (cdr object))))
   ((and (vectorp object) (not (stringp object)))
    (apply #'append
           (mapcar #'nelisp-closure-template-smoke--byte-code-functions
                   (append object nil))))
   (t nil)))

(let* ((elc (getenv "NELISP_CLOSURE_TEMPLATE_ELC"))
       (artifact (getenv "NELISP_CLOSURE_TEMPLATE_ARTIFACT"))
       (functions
        (apply #'append
               (mapcar #'nelisp-closure-template-smoke--byte-code-functions
                       (nelisp-closure-template-smoke--elc-forms elc))))
       (function
        (cl-find-if
         (lambda (candidate)
           (nelisp-bytecode-compiler-input--closure-template-descriptor-p
            (and (> (length candidate) 4) (aref candidate 4))))
         functions))
       (input (and function (nelisp-bytecode-compiler-input-build function)))
       (native-result
        (and function
             (nelisp-bytecode-native-compiler-build
              function artifact 'nl_bc_closure_template_smoke))))
  (unless (and function
               (not (fboundp 'closure-template-fixture-function))
               (not (boundp 'closure-template-fixture-top-level-effect))
               (nelisp-bytecode-compiler-input--closure-template-descriptor-p
                (plist-get input :closure-template-descriptor))
               (eq (plist-get input :capture-values-available) nil)
               (not (plist-get input :captured-values))
               (not (eq (plist-get input :status) 'malformed))
               (eq (plist-get native-result :status) 'unsupported)
               (not (file-exists-p artifact)))
    (error "Closure template refusal smoke failed: input=%S native=%S"
           input native-result))
  (princ "STANDALONE_CLOSURE_TEMPLATE_PUBLIC_REFUSAL_PASS\n"))

;;; standalone-bytecode-closure-template-driver.el ends here
