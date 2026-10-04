;;; save-excursion-owner-1.el --- Shared restoration and moving point -*- lexical-binding: t; -*-

(set-buffer
 (with-temp-buffer
   (insert "abcdef")
   (goto-char 4)
   (list (save-excursion (goto-char 1) 42) (point)))
 (with-temp-buffer
   (insert "abcdef")
   (goto-char 4)
   (save-excursion (goto-char 1) (insert "XX"))
   (list (point) (buffer-string)))
 (with-temp-buffer
   (insert "abcdef")
   (goto-char 4)
   (save-excursion (insert "XX"))
   (list (point) (buffer-string)))
 (let ((symbol (make-symbol "excursion-local-owner")))
   (set symbol 10)
   (with-temp-buffer
     (make-local-variable symbol)
     (set symbol 20)
     (let ((outer (current-buffer))
           (target (generate-new-buffer " *excursion-other*")))
       (unwind-protect
           (list (save-excursion
                   (set-buffer target)
                   (list (symbol-value symbol) (variable-binding-locus symbol)))
                 (eq outer (current-buffer)) (symbol-value symbol))
         (kill-buffer target)))))
 (let ((symbol (make-symbol "excursion-error-owner")))
   (set symbol 10)
   (with-temp-buffer
     (make-local-variable symbol)
     (set symbol 20)
     (insert "abc")
     (goto-char 2)
     (let ((outer (current-buffer))
           (target (generate-new-buffer " *excursion-error*")))
       (unwind-protect
           (progn
             (condition-case nil
                 (save-excursion (set-buffer target) (error "unwind excursion"))
               (error nil))
             (list (eq outer (current-buffer)) (symbol-value symbol) (point)))
         (kill-buffer target)))))
 (with-temp-buffer
   (insert "abcdef")
   (goto-char 4)
   (list (save-excursion
           (goto-char 2)
           (list (save-excursion (goto-char 1) (point)) (point)))
         (point)))
 (let ((target (generate-new-buffer " *excursion-deleted*")))
   (with-current-buffer target
     (save-excursion (kill-buffer target))
     (list (buffer-live-p target) (buffer-live-p (current-buffer))))))
