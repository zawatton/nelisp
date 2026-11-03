;;; cp51932.el --- lightweight CP51932 translation tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Compact compatibility facade for GNU Emacs'
;; international/cp51932.el.  The full generated table is intentionally
;; not loaded during the Class-A lane; representative NEC/IBM extension
;; mappings are registered with the same translation-table names.

;;; Code:

(require 'emacs-translation-table)

(defconst cp51932--jisx0208-to-unicode
  '((#x2d21 . #x2460)
    (#x2d22 . #x2461)
    (#x2d23 . #x2462)
    (#x2d24 . #x2463)
    (#x2d25 . #x2464)
    (#x2d35 . #x2160)
    (#x2d36 . #x2161)
    (#x2d37 . #x2162)
    (#x2d40 . #x3349)
    (#x2d41 . #x3314)
    (#x2d50 . #x339c)
    (#x2d51 . #x339d)
    (#x2d60 . #x301d)
    (#x2d61 . #x301f)
    (#x2d62 . #x2116)
    (#x2d70 . #x2252)
    (#x2d71 . #x2261)
    (#x2d72 . #x222b)
    (#x2d73 . #x222e)
    (#x7c71 . #x2170)
    (#x7c72 . #x2171)
    (#x7c7b . #xffe2)
    (#x7c7c . #xffe4))
  "Representative CP51932 JIS X 0208 extension mappings.")

(defun cp51932--decode-key (jis-code)
  "Return the character key for JIS-CODE."
  (decode-char 'japanese-jisx0208 jis-code))

(defun cp51932--decode-map ()
  "Return lightweight CP51932 decode map."
  (let (out)
    (dolist (entry cp51932--jisx0208-to-unicode (nreverse out))
      (let ((char (cp51932--decode-key (car entry))))
        (when char
          (push (cons char (cdr entry)) out))))))

(defun cp51932--reverse-map (map)
  "Return reverse of MAP."
  (let (out)
    (dolist (entry map (nreverse out))
      (push (cons (cdr entry) (car entry)) out))))

(let ((map (cp51932--decode-map)))
  (define-translation-table 'cp51932-decode map)
  (define-translation-table 'cp51932-encode (cp51932--reverse-map map)))

(provide 'cp51932)

;;; cp51932.el ends here
