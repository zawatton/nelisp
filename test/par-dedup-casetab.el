;;; par-dedup-casetab.el --- Native case-table copy parity -*- lexical-binding: t; -*-

(let* ((make-table (if (boundp 'nelisp-casetab--native-make)
                       nelisp-casetab--native-make
                     #'make-char-table))
       (table (funcall make-table 'case-table nil))
       (parent (funcall make-table 'case-table nil))
       (upper (funcall make-table 'case-table nil)))
  (set-char-table-range table ?Λ ?λ)
  (set-char-table-range parent ?Ω ?ω)
  (set-char-table-parent table parent)
  (set-char-table-extra-slot table 0 upper)
  (let ((copy (if (fboundp 'nelisp-casetab--copy-char-table)
                  (nelisp-casetab--copy-char-table table)
                (copy-sequence table))))
    (prin1 (list 'native-case-table-copy
                 (char-table-subtype copy)
                 (eq (char-table-parent copy) parent)
                 (eq (char-table-extra-slot copy 0) upper)
                 (aref copy ?Λ)
                 (aref copy ?Ω)))
    (terpri)))
