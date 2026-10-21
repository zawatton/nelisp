;;; batch-e-syntax-buffer.el --- buffer syntax tables, GNU 31.1 golden  -*- lexical-binding: t; -*-

;; A buffer's syntax table is buffer state: GNU releases it with the buffer,
;; and a buffer that never ran `set-syntax-table' uses
;; buffer_defaults.syntax_table (buffer.c reset_buffer_local_variables).
;; NeLisp keeps the per-buffer table in `nelisp--syntax-buffer-tables' and
;; the default in `nelisp--buffer-defaults-syntax-table'; rows print only
;; values GNU 31.1 also prints, so the runtime-only checks answer as GNU
;; does when the names do not exist.

(defun be--row (name thunk)
  (princ (format "%s => %S\n" name
                 (condition-case e (funcall thunk) (error (list 'error (car e)))))))

(defun be--probe ()
  (list (string (char-syntax ?\;)) (string (char-syntax ?\')) (string (char-syntax ?-))
        (string (char-syntax ?\n))))

(defmacro be--with-default (table &rest body)
  "Run BODY in a fresh buffer whose syntax table is the default TABLE.
NeLisp: through `nelisp--buffer-defaults-syntax-table'.  GNU: the buffer's
table is set to TABLE, which is what a TABLE default gives a new buffer."
  `(if (boundp 'nelisp--buffer-defaults-syntax-table)
       (let ((nelisp--buffer-defaults-syntax-table ,table))
         (with-temp-buffer ,@body))
     (with-temp-buffer (set-syntax-table ,table) ,@body)))

;; The library's default: notes-65 sets the variable to the standard table,
;; and its oracle expects GNU's fresh-buffer answer.
(be--row "default-standard-table"
         (lambda () (be--with-default (standard-syntax-table)
                      (cons (eq (syntax-table) (standard-syntax-table)) (be--probe)))))
(be--row "default-standard-oracle"
         (lambda () (be--with-default (standard-syntax-table)
                      (list (eq (syntax-table) (standard-syntax-table))
                            (string (char-syntax ?\;)) (string (char-syntax ?\'))
                            (string (char-syntax ?_))))))
;; The bare runtime's default (Doc 204 P3): emacs-lisp-mode-syntax-table.
(be--row "default-emacs-lisp-table"
         (lambda () (be--with-default emacs-lisp-mode-syntax-table
                      (cons (eq (syntax-table) emacs-lisp-mode-syntax-table) (be--probe)))))
(be--row "own-table-wins-over-default"
         (lambda ()
           (let ((own (make-syntax-table)))
             (modify-syntax-entry ?- "." own)
             (be--with-default (standard-syntax-table)
               (set-syntax-table own)
               (list (eq (syntax-table) own) (string (char-syntax ?-)))))))
;; kill-buffer releases the buffer's table.
(be--row "kill-buffer-releases-table"
         (lambda ()
           (let ((b (generate-new-buffer " be-kill")) (own (make-syntax-table)))
             (with-current-buffer b (set-syntax-table own))
             (kill-buffer b)
             (list (buffer-live-p b)
                   (if (boundp 'nelisp--syntax-buffer-tables)
                       (null (gethash b nelisp--syntax-buffer-tables))
                     t)))))
(be--row "forget-buffer-reverts-to-default"
         (lambda ()
           (let ((b (generate-new-buffer " be-forget")) (own (make-syntax-table)))
             (unwind-protect
                 (progn
                   (with-current-buffer b (set-syntax-table own))
                   (if (fboundp 'nelisp--syntax-forget-buffer)
                       (progn (nelisp--syntax-forget-buffer b)
                              (with-current-buffer b (not (eq (syntax-table) own))))
                     t))
               (kill-buffer b)))))
