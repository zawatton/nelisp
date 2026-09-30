;;; emacs-cc-font-2.el --- font C-core compatibility -*- lexical-binding: t; -*-

(defun emacs-cc-font-2--font-spec-p (x)
  (and (consp x) (eq (car x) 'emacs-cc-font-2--font-spec)))

(unless (fboundp 'font-put)
  (defun font-put (font prop val)
    "Set one property of FONT: give property KEY value VAL."
    (unless (emacs-cc-font-2--font-spec-p font)
      (signal 'wrong-type-argument (list 'font-spec font)))
    (let ((tail (cddr font)))
      (if (plist-member tail prop)
          (plist-put tail prop val)
        (setcdr (cdr font) (cons prop (cons val tail))))
      val)))
(unless (fboundp 'font-shape-gstring)
  (defun font-shape-gstring (gstring direction)
    "Shape the glyph-string GSTRING subject to bidi DIRECTION."
    (ignore direction)
    (signal 'error (list "Invalid glyph-string: "
                         (if (and (consp gstring) (null (cdr gstring))
                                  (symbolp (car gstring)))
                             (car gstring)
                           (or gstring ""))))))
(unless (fboundp 'font-spec)
  (defun font-spec (&rest rest)
    "Return a newly created font-spec with arguments as properties."
    (unless (zerop (% (length rest) 2)) (signal 'wrong-number-of-arguments (list 'font-spec (length rest))))
    (let ((x (cons 'emacs-cc-font-2--font-spec (cons nil rest)))) x)))
(unless (fboundp 'font-variation-glyphs)
  (defun font-variation-glyphs (font-object character)
    "Return a list of variation glyphs for CHARACTER in FONT-OBJECT."
    (ignore character) (signal 'wrong-type-argument (list 'font-object font-object))))
(unless (fboundp 'font-xlfd-name)
  (defun font-xlfd-name (font &optional fold-wildcards long-xlfds)
    "Return XLFD name of FONT."
    (ignore fold-wildcards long-xlfds) (signal 'wrong-type-argument (list 'font font))))
(unless (fboundp 'frame-font-cache)
  (defun frame-font-cache (&optional frame)
    "Return FRAME's font cache.  Mainly used for debugging."
    (or frame
        (and (fboundp 'selected-frame) (selected-frame))
        (and (fboundp 'frame-list) (car (frame-list))))))
(unless (fboundp 'internal-char-font)
  (defun internal-char-font (position &optional ch)
    "For internal use only."
    (ignore ch)
    (let ((minimum (if (fboundp 'point-min) (point-min) 1))
          (maximum (if (fboundp 'point-max) (point-max) 1)))
      (signal 'args-out-of-range (list position minimum maximum)))))
(unless (fboundp 'list-fonts)
  (defun list-fonts (font-spec &optional frame num prefer)
    "List available fonts matching FONT-SPEC on FRAME."
    (ignore frame num prefer) (unless (emacs-cc-font-2--font-spec-p font-spec) (signal 'wrong-type-argument (list 'font-spec font-spec))) nil))
(unless (fboundp 'open-font)
  (defun open-font (font-entity &optional size frame)
    "Open FONT-ENTITY."
    (ignore size frame) (signal 'wrong-type-argument (list 'font-entity font-entity))))
(unless (fboundp 'query-font)
  (defun query-font (font-object)
    "Return information about FONT-OBJECT."
    (signal 'wrong-type-argument (list 'font-object font-object))))

(provide 'emacs-cc-font-2)
