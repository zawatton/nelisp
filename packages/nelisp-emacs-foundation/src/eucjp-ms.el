;;; eucjp-ms.el --- lightweight eucJP-ms translation tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Compact compatibility facade for GNU Emacs'
;; international/eucjp-ms.el.  It registers the standard
;; eucjp-ms-decode/eucjp-ms-encode translation-table names with a
;; representative subset, keeping the large generated table out of
;; startup until runtime images can bake it cheaply.

;;; Code:

(require 'emacs-translation-table)

(defconst eucjp-ms--jisx0208-to-unicode
  '((#xada1 . #x2460)
    (#xada2 . #x2461)
    (#xada3 . #x2462)
    (#xada4 . #x2463)
    (#xada5 . #x2464)
    (#xadb5 . #x2160)
    (#xadb6 . #x2161)
    (#xadb7 . #x2162)
    (#xadc0 . #x3349)
    (#xadc1 . #x3314)
    (#xadd0 . #x339c)
    (#xadd1 . #x339d)
    (#xade0 . #x301d)
    (#xade1 . #x301f)
    (#xade2 . #x2116)
    (#xadf0 . #x2252)
    (#xadf1 . #x2261)
    (#xadf2 . #x222b)
    (#xadf3 . #x222e))
  "Representative eucJP-ms JIS X 0208 extension mappings.")

(defconst eucjp-ms--private-use-to-unicode
  '((#xf5a1 . #xe000)
    (#xf5a2 . #xe001)
    (#xf5a3 . #xe002)
    (#xf6a1 . #xe05e)
    (#xf7a1 . #xe0bc)
    (#xf8a1 . #xe11a)
    (#xf9a1 . #xe178)
    (#xfaa1 . #xe1d6)
    (#xfba1 . #xe234)
    (#xfca1 . #xe292)
    (#xfda1 . #xe2f0)
    (#xfea1 . #xe6fa))
  "Representative eucJP-ms private-use extension mappings.")

(defun eucjp-ms--jis-code (euc-code)
  "Return the 7-bit JIS code represented by EUC-CODE."
  (logand euc-code #x7f7f))

(defun eucjp-ms--decode-map ()
  "Return lightweight eucJP-ms decode map."
  (let (out)
    (dolist (entry eucjp-ms--jisx0208-to-unicode)
      (let ((char (decode-char 'japanese-jisx0208
                               (eucjp-ms--jis-code (car entry)))))
        (when char
          (push (cons char (cdr entry)) out))))
    (dolist (entry eucjp-ms--private-use-to-unicode)
      (push entry out))
    (nreverse out)))

(defun eucjp-ms--reverse-map (map)
  "Return reverse of MAP."
  (let (out)
    (dolist (entry map (nreverse out))
      (push (cons (cdr entry) (car entry)) out))))

(let ((map (eucjp-ms--decode-map)))
  (define-translation-table 'eucjp-ms-decode map)
  (define-translation-table 'eucjp-ms-encode (eucjp-ms--reverse-map map)))

(provide 'eucjp-ms)

;;; eucjp-ms.el ends here
