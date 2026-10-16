;;; standalone-reader-string-escape-check.el --- native reader string escapes as GNU reads them -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-reader-string-escape-check.el
;;
;; The native reader's prescan (`bf_read_one_string_escape_supported_p')
;; admitted only two-digit \x, so every string with \x above #xFF, \u, \U or
;; \N{U+X} went to the interpreted reader.  Icon-font data files
;; (nerd-icons, all-the-icons) are one quoted alist of such strings: a 289 KB
;; form took 23.8 s there and stopped at excessive-lisp-nesting.  The lexer's
;; shared escape scanner already decodes them.
;;
;; Each input below was read with GNU Emacs 31.1 `read-from-string'.  The
;; native reader must return GNU's value, end and multibyteness, or decline
;; (nil) where GNU signals or where the escape is left to the interpreted
;; reader (character names, raw bytes mixed with characters, #x3FFF80+).
;; Prints one STRING-ESCAPE line and exits non-zero on failure.

(defvar string-escape-check--cases
  ;; (INPUT EXPECTED) with EXPECTED (VALUE END MULTIBYTE-P) or `decline'.
  (list (list "\"\\xf0a86\"" (list (string #xf0a86) 9 t))
        (list "\"\\xe90c\"" (list (string #xe90c) 8 t))
        (list "\"\\x100\"" (list (string #x100) 7 t))
        (list "\"\\x041\"" (list "A" 7 nil))
        (list "\"\\x0e9\"" (list (string #xe9) 7 t))
        (list "\"\\x41\"" (list "A" 6 nil))
        (list "\"\\u00e9\"" (list (string #xe9) 8 t))
        (list "\"\\u0041\"" (list "A" 8 nil))
        (list "\"a\\u00e9b\"" (list (string ?a #xe9 ?b) 10 t))
        (list "\"\\U0001F600\"" (list (string #x1f600) 12 t))
        (list "\"\\u12345\"" (list (string #x1234 ?5) 9 t))
        (list "\"\\N{U+E9}\"" (list (string #xe9) 10 t))
        (list "\"\\N{U+1F600}\"" (list (string #x1f600) 13 t))
        (list "\"\\xf0a86\\\" x\"" (list (concat (string #xf0a86) "\" x") 13 t))
        (list "\"\\U00110000\"" 'decline)
        (list "\"\\u12\"" 'decline)
        (list "\"\\N{U+110000}\"" 'decline)
        (list "\"\\N{LATIN SMALL LETTER E WITH ACUTE}\"" 'decline)
        (list "\"\\xe9\\u00e9\"" 'decline)
        (list "\"\\x3fffff\"" 'decline)
        (list "\"\\x400000\"" 'decline)
        (list "\"\\x\"" 'decline)))

(let ((failures nil))
  (dolist (c string-escape-check--cases)
    (let* ((input (car c))
           (got (condition-case err (nelisp--read-all-from-string-native input 0 (length input))
                  (error (list 'error err))))
           (want (nth 1 c))
           (ok (if (eq want 'decline)
                   (null got)
                 (and (consp got) (stringp (car got))
                      (equal (car got) (nth 0 want))
                      (equal (cdr got) (nth 1 want))
                      (eq (and (multibyte-string-p (car got)) t) (nth 2 want))))))
      (unless ok (push (list input got want) failures))))
  ;; A form shaped like an icon data file: one quoted alist of \x strings.
  (let* ((n 3000)
         (src (concat "(defvar icon-alist '("
                      (mapconcat (lambda (i) (format "(\"i%d\" . \"\\x%x\")" i (+ #xf0000 i)))
                                 (number-sequence 1 n) " ")
                      "))"))
         (got (nelisp--read-all-from-string-native src 0 (length src))))
    (unless (and (consp got) (= (cdr got) (length src))
                 (= (length (nth 1 (nth 2 (car got)))) n)
                 (equal (cdr (car (last (nth 1 (nth 2 (car got))))))
                        (string (+ #xf0000 n))))
      (push (list 'icon-alist (and got (cdr got))) failures)))
  (dolist (f (reverse failures))
    (princ (format "  MISMATCH %S\n" f)))
  (princ (format "STRING-ESCAPE cases=%d failures=%d %s\n"
                 (1+ (length string-escape-check--cases)) (length failures)
                 (if failures "FAIL" "PASS")))
  (kill-emacs (if failures 1 0)))

;;; standalone-reader-string-escape-check.el ends here
