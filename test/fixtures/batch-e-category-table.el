;;; batch-e-category-table.el --- with-category-table, GNU 31.1 golden  -*- lexical-binding: t; -*-

;; GNU international/mule.el `with-category-table': BODY runs with TABLE as
;; the current buffer's category table; the old table is restored on exit,
;; normal or not, in the buffer that was current; the value is BODY's.

(defun be--row (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case e (funcall thunk) (error (list 'error (car e)))))))

(be--row "with-category-table"
         (lambda ()
           (with-temp-buffer
             (let* ((before (category-table))
                    (table (make-category-table))
                    (cat (get-unused-category table)))
               (define-category cat "batch-e" table)
               (list (with-category-table table
                       (list (eq (category-table) table) (category-docstring cat)))
                     (eq (category-table) before)
                     (category-docstring cat before)
                     (condition-case nil (with-category-table table (error "boom"))
                       (error 'caught))
                     (eq (category-table) before)
                     (with-category-table table 42))))))
(be--row "restores-in-original-buffer"
         (lambda ()
           (let ((a (generate-new-buffer " be-cat-a")) (b (generate-new-buffer " be-cat-b"))
                 (table (make-category-table)))
             (unwind-protect
                 (with-current-buffer a
                   (let ((before (category-table)))
                     (with-category-table table (set-buffer b))
                     (list (eq (current-buffer) b)
                           (with-current-buffer a (eq (category-table) before))
                           (with-current-buffer b (eq (category-table) table)))))
               (kill-buffer a) (kill-buffer b)))))
