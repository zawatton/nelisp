;;; -*- lexical-binding: t; -*-
;; String literals mixing raw-byte escapes (\NNN, \xNN in 128..255) with
;; multibyte characters.  GNU's read_string_literal makes such a literal
;; multibyte and each raw byte the BYTE8 character #x3FFF00+B; a literal with
;; raw bytes only stays unibyte.  The literals below are read by the file
;; reader (this file) and again by `read-from-string'.  thai.el has one
;; ("...‌\200D]").  Spec = GNU 31.1 (par-read-byte8.gnu).
(defmacro row (n f) `(princ (format "%-22s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defun rb-desc (s) (list (string-to-list s) (multibyte-string-p s) (string-bytes s)))
(row "B1 thai u then octal" (rb-desc "[̤‌\200D]"))
(row "B2 octal then u" (rb-desc "\200éx"))
(row "B3 octal then char" (rb-desc "\200é"))
(row "B4 char then octal" (rb-desc "é\200"))
(row "B5 hex then u" (rb-desc "\xe9あ"))
(row "B6 u then hex" (rb-desc "あ\xff"))
(row "B7 raw only" (rb-desc "a\200\377"))
(row "B8 octal >255" (rb-desc "\200\400"))
(row "B9 several raw" (rb-desc "\201あ\202い\377"))
(row "B10 read-from-string" (rb-desc (car (read-from-string "\"[\\u0324\\200D]\""))))
(row "B11 read-from-string 2" (rb-desc (car (read-from-string "\"\\200\\u00e9\""))))
(row "B12 char literal" (list ?\200 ?\xff ?é))
