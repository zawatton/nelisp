;;; idna-mapping.el --- lightweight IDNA mapping table  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; A compact replacement for the generated GNU Emacs IDNA mapping table.
;; Callers such as textsec.el use `elt' directly, so the table is a
;; full Unicode scalar vector with nil defaults and representative
;; disallowed/mapped entries installed.

;;; Code:

(defconst idna-mapping--max-code-point #x110000
  "One past the highest Unicode scalar value.")

(defvar idna-mapping-table
  (let ((table (make-vector idna-mapping--max-code-point nil))
        (i 0))
    ;; C0 controls and DEL are disallowed in IDNA.
    (while (< i #x20)
      (aset table i t)
      (setq i (1+ i)))
    (aset table #x7f t)
    ;; ASCII uppercase maps to lowercase.
    (setq i ?A)
    (while (<= i ?Z)
      (aset table i (char-to-string (+ ?a (- i ?A))))
      (setq i (1+ i)))
    ;; Representative ignored/default-ignorable code points.
    (dolist (char '(#x00ad #x034f #x061c #x180b #x180c #x180d
                    #x200b #x200c #x200d #x2060 #xfeff))
      (aset table char 'ignored))
    ;; Representative compatibility mappings from the generated table.
    (dolist (entry '((#x00aa . "a")
                     (#x00b5 . "μ")
                     (#x00b9 . "1")
                     (#x00ba . "o")
                     (#x00bc . "1⁄4")
                     (#x00bd . "1⁄2")
                     (#x00be . "3⁄4")
                     (#x00c0 . "à")
                     (#x00c1 . "á")
                     (#x00c6 . "æ")
                     (#x0130 . "i̇")
                     (#x0132 . "ij")
                     (#x017f . "s")
                     (#x212a . "k")
                     (#x212b . "å")))
      (aset table (car entry) (cdr entry)))
    table)
  "Lightweight IDNA mapping vector.
nil means no special mapping in this compact table.  t means
disallowed, strings are mapped output, and `ignored' means the
character is dropped.")

(provide 'idna-mapping)

;;; idna-mapping.el ends here
