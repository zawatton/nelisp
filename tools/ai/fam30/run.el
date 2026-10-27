;;; -*- lexical-binding: t; -*-
;; fam30 parity corpus runner: prints one line per probe; compared with GNU 31.1 output.
(let ((d (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "src.el" d) nil t)
  (load (expand-file-name "bc.el" d) nil t))
(defmacro f30-p (name form) `(princ (format "%s %S\n" ,name (condition-case e ,form (error (list 'error (car e)))))))
(dolist (suffix '("" "-bc"))
  (dolist (f '(f30-sr-nested f30-sr-throw f30-sr-error f30-sr-edit f30-sr-edit2 f30-sr-switch f30-sr-kill f30-sr-value
               f30-sr-unnarrowed-widen f30-al-bc f30-al-err))
    (let ((fn (intern (concat (symbol-name f) suffix))))
      (f30-p (format "%s" fn) (funcall fn))))
  (let ((fn (intern (concat "f30-sr-plain" suffix)))) (f30-p fn (funcall fn 200)))
  (let ((fn (intern (concat "f30-sr-loop-narrow" suffix)))) (f30-p fn (funcall fn 100))))
;; upcase / downcase / capitalize
(dolist (s '("" "abc" "hello world" "αβγδεζηθικλμνξοπρστυφχψω straße ǆ" "ǅ ǆ Ǆ" "ﬁnance ﬀ ŉ ǰ ΐ" "ΑΣ ΑΣΑ σ ς" "ß" "İi ıI" "ᾀ ᾈ"
              "mixed Ünï Cödé 日本語 abc" "x́y"))
  (princ (format "up %S %S %S %S %S\n" s (upcase s) (downcase s) (capitalize s) (upcase-initials s))))
(dolist (c '(?a ?z ?ß ?ǆ ?σ ?ς ?ÿ ?ᾀ #x10428 ?A ?0 ?İ))
  (princ (format "upc %S %S %S\n" c (upcase c) (downcase c))))
(princ (format "ub %S %S\n" (upcase (string-to-unibyte "abc\xe9")) (downcase "ABC")))
(princ (format "md %S\n" (progn (string-match "b+" "abbbc") (upcase "αβγ straße") (match-data))))
(princ (format "up-err %S\n" (condition-case e (upcase 'a) (error (car e)))))
(kill-emacs 0)
