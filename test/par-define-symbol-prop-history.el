;;; par-define-symbol-prop-history.el --- property registration -*- lexical-binding: t; -*-

(let ((current-load-list '(existing))
      (a (make-symbol "a"))
      (b (make-symbol "b")))
  (define-symbol-prop a 'par-property 1)
  (define-symbol-prop b 'par-property 2)
  (define-symbol-prop a 'par-property 3)
  (define-symbol-prop a 'par-other-property 4)
  (prin1 (list (get a 'par-property) (get b 'par-property)
               current-load-list))
  (terpri))

(let ((file (make-temp-file "par-symbol-prop-" nil ".el"
                            ";;; -*- lexical-binding: t; -*-\n(define-symbol-prop 'par-loaded-symbol 'par-loaded-property 42)\n")))
  (unwind-protect
      (progn
        (load file nil t t)
        (prin1 (list (get 'par-loaded-symbol 'par-loaded-property)
                     (equal (symbol-file 'par-loaded-symbol 'par-loaded-property) file)))
        (terpri))
    (delete-file file)))
