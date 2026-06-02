;;; fontset.el --- lightweight fontset data and helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Compact compatibility facade for GNU Emacs' international/fontset.el.
;; The full file builds platform fontsets and calls C fontset primitives.
;; This module keeps the data and helper API needed by terminal/window
;; facades without forcing GUI font discovery during startup.

;;; Code:

(defvar font-encoding-alist
  '(("iso8859-1$" . iso-8859-1)
    ("iso8859-15$" . iso-8859-15)
    ("ascii-0$" . ascii)
    ("jisx0208" . japanese-jisx0208)
    ("jisx0201" . jisx0201)
    ("jisx0212" . japanese-jisx0212)
    ("jisx0213.2000-1" . japanese-jisx0213-1)
    ("jisx0213.2000-2" . japanese-jisx0213-2)
    ("gb2312.1980" . chinese-gb2312)
    ("gbk" . chinese-gbk)
    ("ksx1001" . korean-ksc5601)
    ("ksc5601.1987" . korean-ksc5601)
    ("big5" . big5)
    ("unicode-bmp" . (unicode-bmp . nil))
    ("iso10646-1$" . (unicode-bmp . nil)))
  "Alist of font name patterns and matching charset symbols.")

(defvar font-encoding-charset-alist
  '((latin-iso8859-1 . iso-8859-1)
    (latin-iso8859-15 . iso-8859-15)
    (latin-jisx0201 . jisx0201)
    (katakana-jisx0201 . jisx0201)
    (chinese-big5-1 . big5)
    (chinese-big5-2 . big5)
    (tibetan . unicode-bmp))
  "Alist mapping font encodings to charsets.")

(defvar script-representative-chars
  '((latin ?A ?Z ?a ?z #x00c0 #x0100)
    (greek #x03a9)
    (cyrillic #x042f)
    (hebrew #x05d0)
    (arabic #x0628 #x06c1)
    (devanagari #x0915)
    (thai #x0e17)
    (symbol . [#x201c #x2200 #x2500])
    (cjk-misc #x300e #xff0c #x300a #xff09 #x5b50)
    (kana #x304b)
    (bopomofo #x3105)
    (han #x5b57)
    (hangul #xac00)
    (emoji #x1f600 #x1f680))
  "Representative characters for common scripts.")

(defvar otf-script-alist
  '((latin . "latn")
    (greek . "grek")
    (cyrillic . "cyrl")
    (hebrew . "hebr")
    (arabic . "arab")
    (devanagari . "deva")
    (thai . "thai")
    (kana . "kana")
    (han . "hani")
    (hangul . "hang")
    (emoji . "Zsye"))
  "Representative script to OpenType tag mapping.")

(defvar fontset-alias-alist nil
  "Alist of fontset aliases.")

(defconst xlfd-regexp-family-subnum 0)
(defconst xlfd-regexp-weight-subnum 1)
(defconst xlfd-regexp-slant-subnum 2)
(defconst xlfd-regexp-swidth-subnum 3)
(defconst xlfd-regexp-adstyle-subnum 4)
(defconst xlfd-regexp-pixelsize-subnum 5)
(defconst xlfd-regexp-pointsize-subnum 6)
(defconst xlfd-regexp-resx-subnum 7)
(defconst xlfd-regexp-resy-subnum 8)
(defconst xlfd-regexp-spacing-subnum 9)
(defconst xlfd-regexp-avgwidth-subnum 10)
(defconst xlfd-regexp-registry-subnum 11)

(defconst xlfd-tight-regexp
  "\\`-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*\\'")

(defun fontset--empty-field-p (field)
  "Return non-nil when FIELD is an XLFD wildcard field."
  (or (null field)
      (string-match-p "\\`[*-]+\\'" field)))

(defun x-decompose-font-name (pattern)
  "Decompose XLFD PATTERN into a 12-element vector, or nil."
  (let ((parts (and (stringp pattern)
                    (split-string pattern "-" nil))))
    (when (and (= (length parts) 15)
               (string= (car parts) ""))
      (let ((fields (make-vector 12 nil)))
        (aset fields 0 (concat (nth 1 parts) "-" (nth 2 parts)))
        (dotimes (i 10)
          (aset fields (1+ i) (nth (+ i 3) parts)))
        (aset fields 11 (concat (nth 13 parts) "-" (nth 14 parts)))
        (dotimes (i 12)
          (let ((field (aref fields i)))
            (when (fontset--empty-field-p field)
              (aset fields i nil))))
        fields))))

(defun x-compose-font-name (fields &optional _reduce)
  "Compose an XLFD font name from 12-element vector FIELDS."
  (concat "-" (mapconcat (lambda (field) (or field "*"))
                         (append fields nil) "-")))

(defun set-font-encoding (pattern charset)
  "Set PATTERN's font encoding to CHARSET."
  (let ((slot (assoc pattern font-encoding-alist)))
    (if slot
        (setcdr slot charset)
      (push (cons pattern charset) font-encoding-alist)))
  charset)

(defun fontset--list ()
  "Return known lightweight fontsets."
  (mapcar #'cdr fontset-alias-alist))

(defun fontset--query (pattern &optional _regexpp)
  "Return PATTERN when it is a valid fontset name."
  (cond
   ((fontset-name-p pattern) pattern)
   ((assoc pattern fontset-alias-alist) (cdr (assoc pattern fontset-alias-alist)))
   (t nil)))

(unless (fboundp 'fontset-list)
  (defalias 'fontset-list #'fontset--list))

(unless (fboundp 'query-fontset)
  (defalias 'query-fontset #'fontset--query))

(defun fontset-name-p (fontset)
  "Return non-nil if FONTSET is a valid lightweight fontset name."
  (or (and (stringp fontset)
           (let ((fields (x-decompose-font-name fontset)))
             (and fields
                  (let ((registry (aref fields xlfd-regexp-registry-subnum)))
                    (and registry
                         (string-match-p "\\`fontset-" registry))))))
      (and (stringp fontset)
           (consp (rassoc fontset fontset-alias-alist)))))

(defun fontset-plain-name (fontset)
  "Return a concise display name for FONTSET."
  (let ((resolved (fontset--query fontset)))
    (unless resolved
      (error "Invalid fontset: %s" fontset))
    (let ((fields (x-decompose-font-name resolved)))
      (if fields
          (or (aref fields xlfd-regexp-registry-subnum)
              (aref fields xlfd-regexp-family-subnum)
              resolved)
        resolved))))

(defun generate-fontset-menu ()
  "Return a lightweight fontset menu."
  (let (items)
    (dolist (fontset (fontset--list))
      (unless (or (string-match-p "fontset-default\\'" fontset)
                  (string-match-p "fontset-auto[0-9]+\\'" fontset))
        (push (list (fontset-plain-name fontset) fontset) items)))
    (cons "Fontset"
          (sort items (lambda (a b) (string< (car a) (car b)))))))

(defvar standard-fontset-spec
  "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-standard"
  "Default lightweight fontset spec.")

(defun setup-default-fontset ()
  "Install a default lightweight fontset alias."
  (add-to-list 'fontset-alias-alist
               (cons "fontset-standard" standard-fontset-spec))
  standard-fontset-spec)

(defun create-default-fontset ()
  "Create the default lightweight fontset."
  (setup-default-fontset))

(provide 'fontset)

;;; fontset.el ends here
