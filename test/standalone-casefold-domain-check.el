;;; standalone-casefold-domain-check.el --- Case-fold search across case tables -*- lexical-binding: t -*-

;; GNU oracle: emacs -Q --batch --load test/standalone-casefold-domain-check.el
;; NeLisp: nelisp --load test/standalone-casefold-domain-check.el
;;
;; Each row is TABLE/FOLD/CASE and `string-match''s answer.  The tables are
;; the standard one, a copy of it (a canon that is not the pristine one but
;; maps every character the same), a custom table, and copies whose canon
;; is edited at one character: inside ASCII (x -> :), in 128..255 (e-acute
;; -> newline, e-acute -> E-acute, MICRO SIGN -> itself as the library canon
;; has it), above (lambda -> a) and at a raw-byte character.  The subjects
;; cover unibyte strings with raw bytes, multibyte ASCII and non-ASCII
;; strings, and the patterns cover literals, sets, classes, `.', back
;; references and a unibyte raw-byte pattern, so the rows separate the
;; search that may run under the built-in fold from the one that must
;; translate through the table.
(defvar cfp-tramp
  "\\`/\\(?:\\(-\\|[[:alnum:]]\\{2,\\}\\):\\)?\\([^/|:]+\\)?:\\([^/]*\\)\\'")
(defun cfp-row (name thunk)
  (princ (format "%s %S\n" name (condition-case err (funcall thunk) (error (car err))))))
(defun cfp-standard () (standard-case-table))
(defun cfp-copy () (copy-case-table (standard-case-table)))
(defun cfp-custom ()
  (let ((table (make-char-table 'case-table nil)))
    (set-case-syntax-pair 65 955 table)
    (set-case-syntax-pair 66 98 table)
    table))
(defun cfp-canon-edit (pairs)
  "A copied standard table whose canon maps each (FROM . TO) in PAIRS."
  (lambda ()
    (let ((table (copy-case-table (standard-case-table))))
      (set-case-table table)
      (let ((canon (char-table-extra-slot table 1)))
        (dolist (pair pairs) (set-char-table-range canon (car pair) (cdr pair))))
      table)))
(defvar cfp-tables
  (list (cons "std" #'cfp-standard) (cons "copy" #'cfp-copy) (cons "custom" #'cfp-custom)
        (cons "lambda-a" (cfp-canon-edit '((955 . 97))))
        (cons "eacute-nl" (cfp-canon-edit '((233 . 10))))
        (cons "eacute-E" (cfp-canon-edit '((233 . 201))))
        (cons "x-colon" (cfp-canon-edit '((120 . 58))))
        (cons "micro-self" (cfp-canon-edit '((181 . 181))))
        (cons "raw-byte" (cfp-canon-edit '((#x3fffc9 . #x3fffe9))))))
(defvar cfp-cases
  (list
   (list "tramp-uni" cfp-tramp (unibyte-string 47 104 111 109 101 47 117 47 102 46 101 108))
   (list "tramp-colon" cfp-tramp (unibyte-string 47 115 115 104 58 104 111 115 116 58 47 120))
   (list "tramp-x" cfp-tramp (unibyte-string 47 115 115 104 120 104 111 115 116 120 47 120))
   (list "lit-a-lambda" "a" (string 955))
   (list "lit-lambda-xa-mb" (string 955) (string-to-multibyte "xa"))
   (list "lit-lambda-xa-uni" (string 955) (unibyte-string 120 97))
   (list "set-lambda-xa-mb" (string 91 955 93) (string-to-multibyte "xa"))
   (list "lit-A-lambda" "A" (string 955))
   (list "lit-ab-uni" "ab" (unibyte-string 120 65 66 120))
   (list "lit-ab-mb" "ab" (string-to-multibyte "xABx"))
   (list "set-a-lambda" "[a]" (string 955))
   (list "nset-a-lambda" "[^a]" (string 955))
   (list "class-alpha-lambda" "[[:alpha:]]x" (string 955 120))
   (list "class-alnum-raw" "[[:alnum:]]" (unibyte-string 192))
   (list "class-upper-uni" "[[:upper:]]" (unibyte-string 120 97))
   (list "range-a-z-uni" "[a-z]+" (unibyte-string 49 65 66 67))
   (list "dot-raw" "x." (unibyte-string 120 233))
   (list "dot-mb" "x." (string 120 233))
   (list "backref-raw" "\\(.\\)\\1" (unibyte-string 201 233))
   (list "backref-mb" "\\(.\\)\\1" (string 201 233))
   (list "backref-micro-raw" "\\(.\\)\\1" (unibyte-string 181 181))
   (list "backref-micro-mb" "\\(.\\)\\1" (string 181 956))
   (list "dot-micro-raw" "x.y" (unibyte-string 120 181 121))
   (list "rawpat-raw" (unibyte-string 192) (unibyte-string 224))
   (list "rawpat-ascii" (unibyte-string 120 192) (unibyte-string 88 192))
   (list "colon-x-uni" ":" (unibyte-string 97 120 98))
   (list "colon-x-mb" ":" (string-to-multibyte "axb"))
   (list "bol-the" "^the" (unibyte-string 84 104 101 32 120))
   (list "word-uni" "\\w+" (unibyte-string 33 65 98 33))))
(dolist (tb cfp-tables)
  ;; One table per buffer: building a case table's canon is not under test.
  (with-temp-buffer
    (set-case-table (funcall (cdr tb)))
    (dolist (fold '(t nil))
      (dolist (c cfp-cases)
        (cfp-row (format "%s/%s/%s" (car tb) (if fold "t" "nil") (car c))
                 (lambda () (let ((case-fold-search fold))
                              (string-match (nth 1 c) (nth 2 c)))))))))
(princ "CFP-DONE\n")
(kill-emacs 0)
