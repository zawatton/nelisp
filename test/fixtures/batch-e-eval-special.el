;;; batch-e-eval-special.el --- with-syntax-table and with-temp-buffer, GNU 31.1 golden  -*- lexical-binding: t; -*-

;; Both macros must honour their arguments in every configuration
;; (lisp/nelisp-stdlib-eval-special.el defines bootstrap versions).

(defun be--row (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case e (funcall thunk) (error (list 'error (car e)))))))

(be--row "with-syntax-table-uses-table"
         (lambda ()
           (with-temp-buffer
             (let ((table (make-syntax-table)))
               (modify-syntax-entry ?x "." table)
               (list (with-syntax-table table (string (char-syntax ?x)))
                     (string (char-syntax ?x))
                     (with-syntax-table table (eq (syntax-table) table))
                     (condition-case nil (with-syntax-table table (error "boom")) (error 'caught))
                     (string (char-syntax ?x)))))))
(be--row "with-temp-buffer"
         (lambda ()
           (let ((outer (current-buffer)) inner)
             (list (with-temp-buffer
                     (setq inner (current-buffer))
                     (insert "abc")
                     (list (not (eq inner outer)) (buffer-string)))
                   (buffer-live-p inner)
                   (eq (current-buffer) outer)
                   (condition-case nil (with-temp-buffer (setq inner (current-buffer)) (error "boom"))
                     (error (list 'caught (buffer-live-p inner))))))))
