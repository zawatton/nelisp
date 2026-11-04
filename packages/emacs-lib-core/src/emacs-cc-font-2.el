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
(defun emacs-cc-font-2--gstring-p (gstring)
  "Return non-nil if GSTRING has the glyph-string structure."
  (and (vectorp gstring) (>= (length gstring) 2)
       (let ((header (aref gstring 0))
             (id (aref gstring 1)))
         (and (vectorp header) (>= (length header) 2)
              (let ((font (aref header 0)))
                (or (and (symbolp font) (coding-system-p font))
                    (and (or (not (vectorp font)) (> (length font) 0))
                         (fontp font 'font-object))))
              (or (null id) (and (integerp id) (>= id 0)))
              (let ((i 1) (valid t))
                (while (and valid (< i (length header)))
                  (let ((ch (aref header i)))
                    (setq valid (and (integerp ch) (>= ch 0))))
                  (setq i (1+ i)))
                valid)
              (let ((i 2) (valid t))
                ;; A nil glyph terminates the used portion of the vector.
                (while (and valid (< i (length gstring))
                            (aref gstring i))
                  (let ((glyph (aref gstring i)))
                    (setq valid (and (vectorp glyph) (= (length glyph) 10))))
                  (setq i (1+ i)))
                valid)))))

(unless (fboundp 'font-shape-gstring)
  (defun font-shape-gstring (gstring direction)
    "Shape the glyph-string GSTRING subject to bidi DIRECTION."
    (ignore direction)
    (unless (emacs-cc-font-2--gstring-p gstring)
      (signal 'error (cons "Invalid glyph-string: "
                           (if (proper-list-p gstring)
                               gstring
                             (list gstring)))))
    (if (aref gstring 1)
        gstring
      (let ((font (aref (aref gstring 0) 0)))
        (unless (fontp font 'font-object)
          (signal 'wrong-type-argument (list 'font-object font)))
        ;; The standalone font layer has no driver capable of shaping.
        nil))))
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
    (setq frame (or frame (selected-frame)))
    (unless (frame-live-p frame)
      (signal 'wrong-type-argument (list 'frame-live-p frame)))
    ;; Terminal frames have no font drivers and hence no font cache.
    nil))
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
