;;; buffer-owner-kill-1.el --- Public buffer destruction and local restoration -*- lexical-binding: t; -*-

(kill-buffer
 (let ((symbol (make-symbol "kill-active-owner")))
   (set symbol 10)
   (with-temp-buffer
     (let ((target (generate-new-buffer " *kill-active-owner*")))
       (with-current-buffer target
         (make-local-variable symbol)
         (set symbol 20)
         (let ((result (kill-buffer)))
           (list result (buffer-live-p target) (symbol-value symbol)
                 (variable-binding-locus symbol)))))))
 (let ((symbol (make-symbol "kill-inactive-owner")))
   (set symbol 10)
   (with-temp-buffer
     (let ((target (generate-new-buffer " *kill-inactive-owner*")))
       (with-current-buffer target
         (make-local-variable symbol)
         (set symbol 20))
       (list (symbol-value symbol) (kill-buffer target)
             (symbol-value symbol) (variable-binding-locus symbol)))))
 (condition-case err (kill-buffer 42) (error err))
 (condition-case err (kill-buffer nil nil) (error err)))
