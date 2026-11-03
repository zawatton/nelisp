;;; standalone-char-table-map-borrowed-range.el --- Genuine borrowed range behavior -*- lexical-binding: t; -*-
(let ((table (make-char-table 'keymap nil)) retained snapshot)
  (set-char-table-range table '(30 . 32) 'range-command)
  (map-char-table
   (lambda (event definition)
     (when (eq definition 'range-command)
       (setq retained event snapshot (copy-tree event)))) table)
  (unless (and (equal snapshot '(30 . 32))
               (equal retained '(33 . 4194303)))
    (error "Direct character-table borrowed range parity differs")))
(princ "CHAR-TABLE-BORROWED-RANGE-PASS\n")
