;;; standalone-reader-char-literal-check.el --- `?' literals read as in GNU Emacs -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-reader-char-literal-check.el
;;
;; GNU `read_char_literal' reads `? ' and `?<TAB>' as 32 and 9 whatever
;; follows (rx writes `(? blank)'; tramp has two such forms), and requires
;; any other literal to be followed by a control character, a space or one
;; of " ' ; ( ) [ ] # ? ` , . -- anything else is (invalid-read-syntax "?").
;; A bare `?' at end of input is end-of-file.
;;
;; Every expected value below was read with GNU Emacs 31.1 `read-from-string'.
;; Each input goes through three paths:
;; - `read-from-string', the native reader with the interpreted fallback;
;; - `nelisp--rd-one', the interpreted reader alone;
;; - `nelisp--read-all-from-string-native', the native reader alone, which
;;   must return the GNU value for valid input and decline (nil) otherwise.
;;
;; Prints one CHAR-LITERAL line and exits non-zero on failure.

(defun char-literal-check--input (spec)
  "SPEC is a string, or a list of strings and character codes."
  (if (stringp spec) spec
    (mapconcat (lambda (part) (if (stringp part) part (string part))) spec "")))

(defvar char-literal-check--cases
  ;; (INPUT EXPECTED); EXPECTED is (FORM . END) or (error SYMBOL [DATA]).
  `(("(? blank)" ((32 blank) . 9))
    (("(?" 9 " blank)") ((9 blank) . 10))
    (("(?" 10 " x)") ((10 x) . 6))
    ("? x" (32 . 2))
    ("(list ? x ?y)" ((list 32 x 121) . 13))
    ("[? a]" ([32 a] . 5))
    (("(?" 9 "x)") ((9 x) . 5))
    ("?a" (97 . 2))
    ("?a)" (97 . 2))
    ("?a.b" (97 . 2))
    ("?a#" (97 . 2))
    ("(?a?b)" ((97 98) . 6))
    ("?a'b" (97 . 2))
    ("?a`b" (97 . 2))
    ("?a,b" (97 . 2))
    ("?a;c" (97 . 2))
    (("?a" 12 "b") (97 . 2))
    (("?" 12 " ") (12 . 2))
    ("?\\s-a" (8388705 . 5))
    ("?\\C-a" (1 . 5))
    ("?\\^?" (127 . 4))
    ("?\\(" (40 . 3))
    ("?(" (40 . 2))
    ("(?\\s x)" ((32 x) . 7))
    ("?\\s" (32 . 3))
    ("?ab" (error invalid-read-syntax "?"))
    ("?\\sx" (error invalid-read-syntax "?"))
    (("?" 13 "x") (error invalid-read-syntax "?"))
    ("?" (error end-of-file))))

(defun char-literal-check--call (fn input)
  (condition-case err (funcall fn input)
    (error (cons 'error err))))

(defun char-literal-check--ok-p (got expected)
  (if (eq (car expected) 'error)
      (and (eq (car-safe got) 'error)
           (eq (nth 1 got) (nth 1 expected))
           (or (null (nthcdr 2 expected))
               (equal (nthcdr 2 got) (nthcdr 2 expected))))
    (equal got expected)))

(let ((failures nil) (checked 0))
  (dolist (case char-literal-check--cases)
    (let* ((input (char-literal-check--input (car case)))
           (expected (nth 1 case))
           (n (length input))
           (paths
            (list (cons 'read-from-string
                        (char-literal-check--call #'read-from-string input))
                  (cons 'rd-one
                        (char-literal-check--call
                         (lambda (s) (nelisp--rd-one s 0 n t)) input))
                  (cons 'native
                        (and (fboundp 'nelisp--read-all-from-string-native)
                             (char-literal-check--call
                              (lambda (s) (nelisp--read-all-from-string-native s 0 n))
                              input))))))
      (dolist (path paths)
        (setq checked (1+ checked))
        (let ((got (cdr path)))
          (unless (if (eq (car path) 'native)
                      ;; The native reader declines what it does not accept.
                      (if (eq (car expected) 'error) (null got)
                        (or (null got) (equal got expected)))
                    (char-literal-check--ok-p got expected))
            (push (list (car path) input got expected) failures))))
      ;; Valid input must not need the interpreted fallback.
      (unless (or (eq (car expected) 'error)
                  (not (fboundp 'nelisp--read-all-from-string-native))
                  (equal (char-literal-check--call
                          (lambda (s) (nelisp--read-all-from-string-native s 0 n))
                          input)
                         expected))
        (push (list 'native-declined input) failures))))
  (dolist (f (reverse failures))
    (princ (format "  MISMATCH %S\n" f)))
  (princ (format "CHAR-LITERAL checked=%d failures=%d %s\n"
                 checked (length failures) (if failures "FAIL" "PASS")))
  (kill-emacs (if failures 1 0)))

;;; standalone-reader-char-literal-check.el ends here
