;;; standalone-string-search-check.el --- string-search agrees with GNU -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-string-search-check.el
;;
;; A multibyte haystack sent `string-search' to an interpreted walk that
;; allocated a substring and compared it at every position: seconds on a
;; 700 KB SVG holding one non-ASCII character.  The native scanner now takes
;; multibyte strings too, converting only START and the result between char
;; and byte positions.  Each case is what GNU Emacs 31.1 answers, errors
;; included.  Prints one STRING-SEARCH line and exits non-zero on failure.

(defconst string-search-cases
  '(("b" "abc") ("z" "abc") ("" "abc") ("" "abc" 2) ("" "abc" 3) ("c" "abc" 2)
    ("c" "abc" 3) ("abc" "abc") ("abcd" "abc") ("a" "") ("" "")
    ("φ" "3φ 6.6kV") ("6" "3φ 6.6kV") ("kV" "3φ 6.6kV" 3) ("φ" "3φ φ" 2)
    ("日本" "こんにちは日本語") ("語" "日本語日本語" 3) ("x" "日本語" 3)
    ("\"" " d=\"M 1,2\" φ") ("é" "café") ("e" "café") ("ab" "日ab日ab" 2)
    ;; errors
    ("a" "abc" 4) ("a" "abc" -1) ("a" "日本" 3) ("a" "abc" 1.0)
    (1 "abc") ("a" abc)))

(defvar string-search-failures nil)

(let ((want '(1 nil 0 2 3 2 nil 0 nil nil 0 1 3 6 3 5 5 nil 3 3 nil 4 (ERR (args-out-of-range 4)) (ERR (args-out-of-range -1)) (ERR (args-out-of-range 3)) (ERR (wrong-type-argument fixnump 1.0)) (ERR (wrong-type-argument stringp 1)) (ERR (wrong-type-argument stringp abc)))))
  (let ((i 0) (cases string-search-cases))
    (while cases
      (let ((got (condition-case e (apply #'string-search (car cases)) (error (list 'ERR e)))))
        (unless (equal got (car want))
          (push (list (car cases) got (car want)) string-search-failures)))
      (setq cases (cdr cases) want (cdr want) i (1+ i)))))

;; A long multibyte haystack: positions must stay char indices.
(let* ((line "    <path d=\"M 1,2\" id=\"p\" /> 3φ\n")
       (text (apply #'concat (make-list 2000 line)))
       (pos 0) (count 0) (last nil))
  (while (setq pos (string-search " d=\"" text pos))
    (setq count (1+ count) last pos pos (1+ pos)))
  (unless (and (= count 2000) (= last (+ (* 1999 (length line)) 9)))
    (push (list 'long-multibyte count last) string-search-failures)))

(dolist (f (reverse string-search-failures)) (princ (format "  MISMATCH %S\n" f)))
(princ (format "STRING-SEARCH failures=%d %s\n" (length string-search-failures)
               (if string-search-failures "FAIL" "PASS")))
(kill-emacs (if string-search-failures 1 0))

;;; standalone-string-search-check.el ends here
