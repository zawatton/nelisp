;;; par-alloc-io-bytecode.el --- Raw bytecode for cons tests -*- lexical-binding: t; -*-

(defun par-alloc-io--vm (op branch value)
  ;; CAR/CDR, goto-if-(non)nil 6, constant 0 / return, constant 1 / return.
  (let ((code (make-byte-code 257 (unibyte-string op branch 6 0 192 135 193 135)
                             [:fall-through :taken] 1)))
    (condition-case err
        (funcall code value)
      (wrong-type-argument (list (car err) (cadr err))))))

(prin1
 (list :forward
       (par-alloc-io--vm 64 (if (getenv "PAR_ALLOC_IO_REVERSE") 132 131) '(nil))
       (par-alloc-io--vm 64 131 '(t))
       (par-alloc-io--vm 64 132 nil)
       (par-alloc-io--vm 64 132 '(1))
       (par-alloc-io--vm 65 131 '(head))
       (par-alloc-io--vm 65 132 '(head . tail))
       (par-alloc-io--vm 65 132 'atom)))
(terpri)

;; Backward conditional to a shared return.  The original argument remains
;; below the tested CDR so either branch can return it.
(let ((code (make-byte-code 257 (unibyte-string 130 4 0 135 137 65 131 3 0 135)
                           [] 2)))
  (prin1 (list :backward (funcall code '(one)) (funcall code '(one two))))
  (terpri))

;; GNU compiles a cadr to two consecutive tagged reads.  If the CDR
;; produces an atom, CAR still signals with that atom in the error data.
(let ((code (make-byte-code 257 (unibyte-string 65 64 135) [] 1)))
  (prin1 (list :chain (funcall code '(one two)) (funcall code '(one))
               (condition-case err
                   (funcall code '(one . atom))
                 (wrong-type-argument err))))
  (terpri))
