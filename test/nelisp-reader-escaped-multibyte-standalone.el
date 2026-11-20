;;; nelisp-reader-escaped-multibyte-standalone.el --- ?\<multibyte> -*- lexical-binding: t; -*-
;; Runs unchanged on GNU Emacs and the NeLisp binary; the stdout streams must
;; be byte-identical.  A backslash before a multibyte character is that
;; character: `?\‘' is #x2018.  The native lexer returned the lead byte
;; (226), which made GNU's elisp-mode.el unreadable (`?\‘' followed by stray
;; continuation bytes: invalid-read-syntax).
(defmacro rdm-case (name str)
  `(princ (format "RDM %s %S\n" ,name
                  (condition-case e (read ,str) (error (list 'err (car e)))))))
(rdm-case "lsq" "?\\‘")
(rdm-case "e-acute" "?\\é")
(rdm-case "cjk" "?\\あ")
(rdm-case "astral" "?\\😀")
(rdm-case "plain" "?‘")
(rdm-case "ascii-esc" "?\\(")
(rdm-case "named" "?\\a")
(rdm-case "in-list" "(?\\‘ ?\\’)")
(rdm-case "string" "\"\\é\"")
(princ (format "RDM src-char %S\n" (list ?\‘ ?\’ ?\é)))
(princ (format "RDM src-string %S\n" (list "\é" "a\あb")))
(princ "RDM-DONE\n")
