;;; corpus.el --- reader parity corpus (labels, \N{NAME}, radix integers, modifiers)  -*- lexical-binding: t -*-
;; Run identically on GNU Emacs and on a NeLisp binary; the output lines must agree.
;;   emacs -Q --batch -l corpus.el          (GNU)
;;   nelisp-dynamic --load corpus.el --     (NeLisp)
;; One line per case and mode: SOURCE <TAB> MODE <TAB> RESULT.  MODE is rfs
;; (`read-from-string'), str (`read' on a string) or buf (`read' on a buffer).

(defvar reader1--sources nil "Alist (GROUP . SOURCE-STRINGS) in definition order.")
(defvar reader1--group 'labels)
(defmacro reader1--add (&rest srcs)
  `(reader1--push ',srcs))
(defun reader1--push (srcs)
  (setq reader1--sources
        (append reader1--sources (mapcar (lambda (s) (cons reader1--group s)) srcs))))

;; ---- #N= / #N# labels, including redefinition of a label number --------
(setq reader1--group 'labels)
(reader1--add
 "(f '#1=(a . #1#) '#1=(b . #1#))"
 "(#1=(a) #1# #1=(b) #1#)"
 "(#1=(a #1#) #1=(b #1#) #1#)"
 "(#1=(a #2=(c #2#) #1#) #2#)"
 "#1=(a #1=(b #1#) #1#)"
 "#1=(a #1=(b #1#))"
 "(#1=[x #1#] #1=[y #1#])"
 "(#1=(a) #2=(b) #1# #2# #1=(c) #1# #2#)"
 "#1=(#1# . #1#)"
 "#1=#1#"
 "#1=#2="
 "(#1# #1=a)"
 "#1=(a . #1#)"
 "(#1=a #1#)"
 "(#1=\"s\" #1# #1=\"t\" #1#)"
 "#10=(1 2 . #10#)"
 "(#1=(a) #1=(b) #1#)"
 "((#1=(a . #1#)) (#1=(b . #1#)))"
 "#s(foo #1=(a #1#) #1=(b #1#))"
 "(#12345=(a #12345#) #12345=(b #12345#))")

;; ---- \N{NAME} / \N{U+XXXX} named characters ---------------------------
(setq reader1--group 'names)
(reader1--add
 "?\\N{LATIN SMALL LETTER A WITH GRAVE}"
 "?\\N{latin small letter a with grave}"
 "?\\N{Latin Small Letter A}"
 "?\\N{SYLOTI  NAGRI LETTER \n DHO}"
 "?\\N{BED}"
 "?\\N{U+BED}"
 "?\\N{U+e0}"
 "?\\N{VARIATION SELECTOR-1}"
 "?\\N{VARIATION SELECTOR-16}"
 "?\\N{VARIATION SELECTOR-17}"
 "?\\N{VARIATION SELECTOR-256}"
 "?\\N{VARIATION SELECTOR-257}"
 "?\\N{CJK COMPATIBILITY IDEOGRAPH-F900}"
 "?\\N{CJK COMPATIBILITY IDEOGRAPH-FAD9}"
 "?\\N{CJK COMPATIBILITY IDEOGRAPH-2F800}"
 "?\\N{CJK COMPATIBILITY IDEOGRAPH-2FA1D}"
 "?\\N{cjk compatibility ideograph-f900}"
 "?\\N{CJK COMPATIBILITY IDEOGRAPH-FA6E}"
 "?\\N{CJK UNIFIED IDEOGRAPH-4E00}"
 "?\\N{HANGUL SYLLABLE GA}"
 "?\\N{GREEK SMALL LETTER LAMDA}"
 "?\\N{GREEK SMALL LETTER LAMBDA}"
 "?\\N{LINE FEED (LF)}"
 "?\\N{BELL (BEL)}"
 "?\\N{NULL}"
 "?\\N{SPACE}"
 "?\\N{EMOJI MODIFIER FITZPATRICK TYPE-3}"
 "?\\N{HEAVY BLACK HEART}"
 "?\\N{LATIN CAPITAL LETTER A WITH DOT ABOVE}"
 "?\\N{DOES NOT EXIST}"
 "?\\N{}"
 "?\\N"
 "?\\Nx"
 "?\\N{U+110000}"
 "?\\N{U+D800}"
 "?\\N{U+DFFF}"
 "?\\N{U+-0041}"
 "?\\N{U++0041}"
 "?\\N{U+}"
 "?\\N{U+zz}"
 "?\\N{\u00e9}"
 "?\\N{LATIN"
 "?\\N{ LATIN SMALL LETTER A }"
 "\"a\\N{SYLOTI NAGRI  LETTER DHO}b\""
 "\"\\N{LATIN SMALL LETTER E WITH ACUTE}\""
 "\"\\N{U+E9}\""
 "\"\\N{U+41}\\N{U+42}\""
 "\"\\N{BOGUS NAME}\""
 "\"\\N{U+D800}\""
 "\"x\\N{LATIN SMALL LETTER A}\\N{LATIN SMALL LETTER B}y\""
 "(?\\N{LATIN SMALL LETTER A} ?\\N{LATIN SMALL LETTER B})"
 "?\\C-\\N{LATIN SMALL LETTER A}"
 "?\\M-\\N{LATIN SMALL LETTER A}")

;; ---- #x #o #b #NNr integers (fixnum edge, bignum, errors) --------------
(setq reader1--group 'radix)
(reader1--add
 "#xff" "#XFF" "#o17" "#O17" "#b101" "#B101" "#x-ff" "#x+ff" "#x0" "#x-0" "#x000ff"
 "#xfffffffffffffff" "#x1fffffffffffffff" "#x2000000000000000" "#x3fffffffffffffff"
 "#x4000000000000000" "#xffffffffffffffff" "#xfffffffffffffffff"
 "#xfffffffffffffffffffffffffffff" "#x-fffffffffffffffffffffffffffff"
 "#x00000000000000000000000000000001" "#x0000000000000000000000000000000fffffffffffffffffff"
 "#o1777777777777777777777" "#o17777777777777777777777777" "#o-17777777777777777777777777"
 "#b1111111111111111111111111111111111111111111111111111111111111"
 "#b11111111111111111111111111111111111111111111111111111111111111111111"
 "#b-11111111111111111111111111111111111111111111111111111111111111111111"
 "#xFfFfFfFfFfFfFfFfFfFf" "#36rzzzzzzzzzzzzzzzz" "#24r1k" "#2r101" "#10r-12" "#36rZZ"
 "#2r1111111111111111111111111111111111111111111111111111111111111111"
 "#16r123456789abcdef0123456789abcdef"
 "(#xffffffffffffffffffff #xff)" "(#xffffffffffffffffffff . #xff)" "[#xffffffffffffffffffff]"
 "#x" "#b" "#o" "#X" "#24r" "#2r2" "#b12" "#b102" "#o8" "#xfg" "#xg" "#x-" "#x+" "#37r1" "#1r1" "#0r1" "#36r"
 "#x1_0" "#xf.f" "#xf)" "(#xf)" "(#xf g)" "#xff1 " "#xff;c" "#xf'a" "#xf\"s\"" "#x1e3" "#xf,x" "(#xf . #xe)")

;; ---- string and character modifier escapes ----------------------------
(setq reader1--group 'mods)
(let ((prefixes '("\\M-" "\\S-" "\\H-" "\\A-" "\\s-" "\\C-" "\\^"))
      (targets '("a" "A" "1" "?" " " "@" "[" "\\a" "\\n" "\\x41" "\\351" "\\C-a" "\\M-b" "\\^?" "\\s" "\\d" "\\u00e9" "\\\\" "\\\"" "z" "_" "%"))
      out)
  (dolist (a prefixes)
    (dolist (tg targets) (push (concat "\"" a tg "\"") out))
    (dolist (b prefixes)
      (dolist (tg '("a" "A" "\\n" "?" " " "\\351"))
        (push (concat "\"" a b tg "\"") out))))
  (reader1--push (nreverse out)))
(reader1--add "\"\\M-\\M-\\M-\\M-\\M-\\M-\\M-\\M-a\"" "\"\\M-\\M-\\M-\\M-\\M-\\M-\\M-\\M-\\C-\\C-a\"" "?\\M-\\M-\\M-a" "?\\C-\\M-\\S-a")

;; ---- other reader behaviours near the changed code --------------------
(setq reader1--group 'misc)
(reader1--add
 "?\\C-\\M-\u00e9" "?\\C-\u00e9" "?\\C-\\C-c" "?\\C-\\M-\\C-c" "?\\M-\\C-\\C-c" "?\\C-\\C-\\M-c"
 "?\\M-\\S-\\H-\\A-\\C-\\s-x" "?\\C-\u0157" "(#xff . #xe)" "[#b1 #o7 #x-f]"
 "(a . b)" "(a .\u00A0b)" "(a .(b))" "(a . 'b)" "(a .\u00A0)" "?\\\n x" "\"a\\\nb\"" "?\\  x" "?\\^?" "\"\\x41\\x42\"" "(1 . 2 3)" "(a b . )" "#" "#@" "#'x" "#:x" "##")

(defun reader1--show (v)
  (let ((print-circle t) (print-escape-newlines t))
    (cond ((stringp v) (format "S%S%S" (if (multibyte-string-p v) 'M 'U) v))
          (t (format "%S" v)))))
(defun reader1--err (e)
  (if (eq (car e) 'invalid-read-syntax) (list (car e) (cadr e)) (list (car e))))
(defun reader1--run-one (src mode)
  (condition-case e
      (let ((print-circle t))
        (reader1--show
         (pcase mode
           ('rfs (read-from-string src))
           ('str (read src))
           ('buf (with-temp-buffer (insert src) (goto-char (point-min))
                                   (let ((v (read (current-buffer)))) (list v (point))))))))
    (error (format "ERR %S" (reader1--err e)))))
(defun reader1--key (src)
  "SRC printed with every non-ASCII character as \\xHEX, so the two printers cannot disagree on it."
  (let ((q (format "%S" src)) (out nil))
    (dolist (c (append q nil))
      (if (> c 127) (push (format "\\x%x" c) out) (push (char-to-string c) out)))
    (apply #'concat (nreverse out))))
(defun reader1--main ()
  (let ((want (getenv "READER1_SET")))
    (dolist (entry reader1--sources)
      (when (or (null want) (string= want "") (string= want "all")
                (string= want (symbol-name (car entry))))
        (dolist (mode '(rfs str buf))
          (princ (format "%s\t%s\t%s\n" (reader1--key (cdr entry)) mode
                         (reader1--run-one (cdr entry) mode))))))))
(reader1--main)
