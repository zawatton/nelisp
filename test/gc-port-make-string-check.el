;;; gc-port-make-string-check.el --- make-string against GNU: lengths, bytes, multibyteness, contents  -*- lexical-binding: t; -*-

;; GNU's Fmake_string allocates the exact size and fills it (memset for a single-byte character, the
;; character's bytes repeated otherwise).  This prints, for each case, what the result is made of; the
;; checker (tools/ai/gc-port-make-string.py) runs it on GNU Emacs and on the NeLisp binaries and
;; requires identical output.  Unibyte, multibyte and forced-multibyte ASCII, every encoded width
;; (1 to 5 bytes and the raw-byte characters), empty, one, a word, a word and a bit, and large strings,
;; mutation after creation, and the error cases.
(defun gcms-desc (s)
  (let ((n (length s)))
    (format "len=%d bytes=%d multi=%S first=%S last=%S sum=%d"
            n (string-bytes s) (multibyte-string-p s)
            (if (> n 0) (aref s 0) nil) (if (> n 0) (aref s (1- n)) nil)
            (let ((i 0) (a 0)) (while (< i n) (setq a (+ a (aref s i)) i (1+ i))) a))))
(defmacro gcms-case (label form)
  `(princ (format "%s: %s\n" ,label
                  (condition-case e (let ((v ,form)) (if (stringp v) (gcms-desc v) (format "%S" v)))
                    (error (format "ERR %S" (car e)))))))
(dolist (n '(0 1 2 3 7 8 9 15 16 17 31 64 128 129 1000 4099))
  (gcms-case (format "ascii %d" n) (make-string n ?x))
  (gcms-case (format "ascii forced multibyte %d" n) (make-string n ?x t))
  (gcms-case (format "zero byte %d" n) (make-string n 0))
  (gcms-case (format "del %d" n) (make-string n 127)))
(dolist (c (list #x80 #xE9 #xFF #x100 #x7FF #x800 #x3042 #xFFFF #x10000 #x1F600 #x10FFFF #x1FFFFF #x200000
                 #x3FFF7F #x3FFF80 #x3FFFFF))
  (dolist (n '(0 1 2 5 64))
    (gcms-case (format "char %X x %d" c n) (make-string n c))))
(gcms-case "multibyte arg nil" (make-string 3 ?a nil))
(gcms-case "multibyte arg t" (make-string 3 ?a t))
(gcms-case "mutate unibyte" (let ((s (make-string 4 ?a))) (aset s 1 ?b) (aset s 3 ?é) (list (gcms-desc s) (append s nil))))
(gcms-case "mutate multibyte" (let ((s (make-string 4 ?あ))) (aset s 2 ?z) (list (gcms-desc s) (append s nil))))
(gcms-case "equal ascii" (equal (make-string 3 ?a) "aaa"))
(gcms-case "equal multibyte" (equal (make-string 3 ?é) "ééé"))
(gcms-case "concat" (concat (make-string 2 ?a) (make-string 2 ?あ) (make-string 2 ?b)))
(gcms-case "substring" (substring (make-string 10 ?é) 3 7))
(gcms-case "string-to-list" (string-to-list (make-string 5 ?é)))
(gcms-case "upcase" (upcase (make-string 6 ?q)))
(gcms-case "string=" (string= (make-string 5 ?x) "xxxxx"))
(gcms-case "string<" (string< (make-string 5 ?x) (make-string 5 ?y)))
(gcms-case "negative length" (make-string -1 ?a))
(gcms-case "float length" (make-string 1.5 ?a))
(gcms-case "string length" (make-string "3" ?a))
(gcms-case "nil length" (make-string nil ?a))
(gcms-case "string char" (make-string 3 "a"))
(gcms-case "negative char" (make-string 3 -1))
(gcms-case "too large char" (make-string 3 #x400000))
(gcms-case "float char" (make-string 3 1.5))
(gcms-case "no args" (make-string))
(gcms-case "one arg" (make-string 3))
(gcms-case "four args" (make-string 3 ?a t t))
nil
