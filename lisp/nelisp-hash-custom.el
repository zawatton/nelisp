;;; nelisp-hash-custom.el --- Lisp custom hash table tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; The standalone's native table owns storage and the standard three tests.
;; Custom tests use an ordinary Lisp scan; no handwritten native is added.
(when (and (fboundp 'nelisp--eval-source-string) (not (fboundp 'define-hash-table-test)))
  (let ((tests nil) (installed nil)
        (make (symbol-function 'make-hash-table))
        (lookup (symbol-function 'gethash))
        (store (symbol-function 'puthash))
        (remove (symbol-function 'remhash)) (scan (symbol-function 'maphash))
        (tablep (symbol-function 'hash-table-p))
        (same (symbol-function 'eq)))
    (cl-labels
     ((definition (name) (cdr (assq name tests)))
      (custom (table)
        ;; Only the key-test name lives in the table.  Its registered
        ;; callbacks never occupy capacity/weakness metadata, and no
        ;; registry keeps otherwise unreachable tables alive.
        (and (funcall tablep table) (> (length (car table)) 2)
             (let ((name (aref (car table) 2)))
               (and (not (memq name '(eq eql equal))) (definition name)))))
      (find-key (key table meta missing)
        (let ((found missing))
          (funcall (aref meta 2) key)
          (funcall scan (lambda (stored _value)
                          (when (and (funcall same found missing)
                                     (funcall (aref meta 1) stored key))
                            (setq found stored))) table)
          found))
      (install ()
        ;; Preserve C-core hot paths until a custom test is requested.
        (unless installed
	  (fset 'gethash
		(lambda (key table &optional default)
		  (let ((meta (custom table)))
                    (if (not meta) (funcall lookup key table default)
                      (let* ((missing (cons nil nil)) (stored (find-key key table meta missing)))
			(if (funcall same stored missing) default (funcall lookup stored table default)))))))
	  (fset 'puthash
		(lambda (&rest args)
		  (unless (= (length args) 3)
                    (signal 'wrong-number-of-arguments (list 'puthash (length args))))
		  (let* ((key (car args)) (value (cadr args)) (table (caddr args))
			 (meta (custom table)))
                    (if (not meta) (funcall store key value table)
                      (let* ((missing (cons nil nil)) (stored (find-key key table meta missing)))
			(funcall store (if (funcall same stored missing) key stored) value table))))))
	  (fset 'remhash
		(lambda (&rest args)
		  (unless (= (length args) 2)
                    (signal 'wrong-number-of-arguments (list 'remhash (length args))))
		  (let* ((key (car args)) (table (cadr args)) (meta (custom table)))
                    (if (not meta) (funcall remove key table)
                      (let* ((missing (cons nil nil)) (stored (find-key key table meta missing)))
			(unless (funcall same stored missing) (funcall remove stored table)))))))
          (setq installed t))))
     (fset 'define-hash-table-test
           (lambda (name test hash)
             (unless (symbolp name)
               (signal 'wrong-type-argument (list 'symbolp name)))
             (unless (and (functionp test) (functionp hash))
               (signal 'wrong-type-argument
                       (list 'functionp (if (functionp test) hash test))))
             (put name 'hash-table-test (cons test hash))
             (let ((pair (vector name
                                 (if (symbolp test) (symbol-function test) test)
                                 (if (symbolp hash) (symbol-function hash) hash))))
               (setq tests (cons (cons name pair) tests)))
             (unless (memq name '(eq eql equal)) (install))
             name))
     (fset 'make-hash-table
           (lambda (&rest keys)
             (let* ((name (plist-get keys :test)) (pair (definition name)))
               (if (or (not pair) (memq name '(eq eql equal))) (apply make keys)
                 (let* ((normalized (copy-sequence keys))
                        (_ (setq normalized (plist-put normalized :test 'equal)))
                        (table (apply make normalized)))
                   (aset (car table) 2 name)
                   table)))))
     )))
(provide 'nelisp-hash-custom)
