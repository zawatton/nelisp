;;; binding-locus-1.el --- Public binding owner transitions -*- lexical-binding: t; -*-

(variable-binding-locus
 (variable-binding-locus (make-symbol "unbound-owner"))
 (let ((symbol (make-symbol "global-owner")))
   (set symbol 42)
   (variable-binding-locus symbol))
 (let ((symbol (make-symbol "local-owner")))
   (set symbol 42)
   (with-temp-buffer
     (let ((before (variable-binding-locus symbol)))
       (make-local-variable symbol)
       (let ((during (eq (current-buffer) (variable-binding-locus symbol))))
         (kill-local-variable symbol)
         (list before during (variable-binding-locus symbol))))))
 (let ((symbol (make-symbol "isolated-owner")))
   (set symbol 42)
   (with-temp-buffer
     (make-local-variable symbol)
     (let ((outer (current-buffer)))
       (list (eq outer (variable-binding-locus symbol))
             (with-temp-buffer (variable-binding-locus symbol))
             (eq outer (variable-binding-locus symbol))))))
 (let ((symbol (make-symbol "explicit-owner")))
   (set symbol 42)
   (with-temp-buffer
     (make-local-variable symbol)
     (let ((outer (current-buffer)))
       (with-temp-buffer
         (list (eq outer (current-buffer))
               (local-variable-p symbol outer)
               (local-variable-p symbol (current-buffer))
               (local-variable-p symbol))))))
 (let ((symbol (make-symbol "swapped-owner")))
   (set symbol 10)
   (with-temp-buffer
     (make-local-variable symbol)
     (set symbol 20)
     (let ((before (symbol-value symbol))
           (inner (with-temp-buffer
                    (list (symbol-value symbol) (variable-binding-locus symbol)))))
       (list before inner (symbol-value symbol)))))
 (let ((symbol (make-symbol "restored-owner")))
   (set symbol 10)
   (condition-case nil
       (with-temp-buffer
         (make-local-variable symbol)
         (set symbol 20)
         (error "owner unwind"))
     (error nil))
   (list (symbol-value symbol) (variable-binding-locus symbol)))
 (condition-case err (variable-binding-locus 1) (error err))
 (condition-case err (variable-binding-locus) (error err))
 (condition-case err (variable-binding-locus 'a 'b) (error err)))
