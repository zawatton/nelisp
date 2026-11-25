;;; par-load-named-unicode.el --- Named escapes on a cold load -*- lexical-binding: t; -*-

(defvar par-load-named-unicode--value nil)

(let ((file (make-temp-file "par-named-unicode-" nil ".el")))
  (unwind-protect
      (progn
        (with-temp-file file
          (insert ";;; -*- lexical-binding: t; -*-\n"
                  "(setq par-load-named-unicode--value\n"
                  "      (list ?\\N{ZERO WIDTH SPACE} ?\\N{BELL (BEL)}\n"
                  "            (aref \"\\N{GREEK SMALL LETTER LAMBDA}\" 0)))\n"))
        (let ((start (float-time)))
          (load file nil t t)
          (prin1 (list 'named-escapes par-load-named-unicode--value
                       (< (- (float-time) start) 5.0)))
          (terpri))
        (let ((ucs-names (make-hash-table :test 'equal)))
          (puthash "PARITY CUSTOM NAME" ?x ucs-names)
          (with-temp-file file
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(setq par-load-named-unicode--value ?\\N{PARITY CUSTOM NAME})\n"))
          (load file nil t t)
          (prin1 (list 'custom-name par-load-named-unicode--value))
          (terpri)))
    (delete-file file)))
