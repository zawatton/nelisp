;;; nelisp-fxir-native-install-tail.el --- hooks for native prelude closures  -*- lexical-binding: t -*-
;; Second half of the install pass (see nelisp-fxir-native-install.el): every
;; later registration of a listed name is checked too (a function defined
;; again with the same fxir runs native code again), then the records compiled
;; before this pass are converted.
(when (and (boundp 'nelisp-nc--table) nelisp-nc--table (fboundp 'nelisp--fx-active-p)
           (nelisp--fx-active-p) (not (fboundp 'nelisp-nc--orig-register-fn)))
  (fset 'nelisp-nc--orig-register-fn (symbol-function 'nelisp--fx-register-fn))
  (fset 'nelisp--fx-register-fn
        (lambda (name fn &optional force)
          (prog1 (nelisp-nc--orig-register-fn name fn force)
            (when (and name (assq name nelisp-nc--table))
              (condition-case nil (nelisp-nc--convert name fn) (error nil))))))
  (dolist (row nelisp-nc--table)
    (let ((name (car row)))
      (when (and (fboundp name) (not (memq :mutant row)))
        (condition-case nil (nelisp-nc--convert name (symbol-function name)) (error nil))))))
