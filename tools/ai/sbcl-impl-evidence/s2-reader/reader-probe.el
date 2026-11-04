;;; reader-probe.el --- differential reader probe  -*- lexical-binding: t -*-
(defvar rp-inputs
  '("(a b (c . d) . e)" "(1 . 2)" "[1 2 [3] (4)]" "\"a\\nb\\t\\\"q\\\" \\x41\\ z\"" "\"日本語λ\""
    "?a" "?\\n" "?\\C-a" "?\\M-a" "?\\^?" "?\\x41" "?日"
    "1e10" ".5" "-0.0" "1.5e-3" "+1" "-1" "1." "+.5e2" "1.0e+INF" "0.0e+NaN"
    "123456789012345678901234567890" "-99999999999999999999" "#x1F" "#b101" "#o17" "#24r1k"
    "#'f" "'x" "`(a ,b ,@c)" "(quote x)" "#s(foo 1 2)" "#s(hash-table data (a 1))"
    "a\\ b" "##" "foo\\(bar" ":kw" ":" "#:x" "#:" "nil" "t" "()" "(nil)" "\\,a" "a;b\n"
    "; comment\n (x ; c\n y)" "#_f" "#&5\"\\1\"" "(a . (b . (c)))" "?\\(" "?\\s" "(?a ?b)"
    "\"\\u00e9\\N{LATIN SMALL LETTER A}\"" "\"a\\\nb\"" "#1=(a . #1#)" "[a #1=b #1#]"
    "(" ")" "(a . )" "#" "\"abc" "" "   " "'" "1 2" "(a) (b)" "a.b" "1+" "-" "+" ".")) 
(dolist (s rp-inputs)
  (princ (format "%S => " s))
  (princ (condition-case e
             (let* ((print-circle t) (r (read-from-string s)))
               (format "%S @%d | %s" (car r) (cdr r)
                       (condition-case e2 (with-temp-buffer (insert s) (goto-char 1)
                                                            (let ((print-circle t)) (format "%S pt=%d" (read (current-buffer)) (point))))
                         (error (format "buf-err %S" e2)))))
           (error (format "ERR %S" e))))
  (terpri))
