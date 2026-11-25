;;; par-init-case-fold.el --- Case translation during init searches -*- lexical-binding: t; -*-

(with-temp-buffer
  (let ((case-fold-search t)
        (names '("par-init-fold-A" "par-init-fold-b" "par-init-fold-excluded")))
    (dolist (name names)
      (intern name))
    (let ((pattern "\\`PAR-INIT-FOLD-\\([AB]\\)\\'"))
      (prin1
       (list 'apropos
             (mapcar #'symbol-name
                     (apropos-internal
                      pattern
                      (lambda (symbol)
                        (equal (match-string 1 (symbol-name symbol)) "A"))))
             (string-match-p pattern "par-init-fold-b")))
      (terpri)))
  (let ((table (copy-case-table (current-case-table)))
        (case-fold-search t)
        (pattern "\\`q\\(Z\\)\\'"))
    (set-case-syntax-pair ?X ?q table)
    (set-case-table table)
    (prin1 (list 'custom-case
                 (string-match pattern "Xz")
                 (match-data)
                 (string-match-p pattern "xz")
                 (match-data)
                 (let ((case-fold-search nil)) (string-match-p pattern "Xz"))))
    (terpri))
  (set-case-table (standard-case-table))
  (let ((case-fold-search t))
    (prin1 (list 'standard-case
                 (string-match "\\`qz\\'" "Xz")
                 (string-match "\\`qz\\'" "QZ")
                 (string-match "\\`ä\\'" "Ä")))
    (terpri)))
