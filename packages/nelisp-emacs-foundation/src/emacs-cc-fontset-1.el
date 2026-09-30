;;; emacs-cc-fontset-1.el -*- lexical-binding: t; -*-

;; Batch-mode compatibility for the fontset C primitives.

(unless (fboundp 'fontset-font)
  (defun fontset-font (name ch &optional all)
    "Return a font name pattern for character CH in fontset NAME.
If NAME is t, find a pattern in the default fontset.
If NAME is nil, find a pattern in the fontset of the selected frame.

The value has the form (FAMILY . REGISTRY), where FAMILY is a font
family name and REGISTRY is a font registry name.  This is actually
the first font name pattern for CH in the fontset or in the default
fontset.

If the 2nd optional arg ALL is non-nil, return a list of all font name
patterns."
    (ignore all)
    (unless (characterp ch)
      (signal 'wrong-type-argument (list 'characterp ch)))
    (cond ((and (stringp name) (not (equal name "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")))
           (error "Fontset ‘%s’ does not exist" name))
          ((null name) (error "Can’t use fontsets in non-GUI frames"))
          (t nil))))

(unless (fboundp 'fontset-info)
  (defun fontset-info (fontset &optional frame)
    "Return information about a fontset FONTSET on frame FRAME.

FONTSET is a fontset name string, nil for the fontset of FRAME, or t
for the default fontset.  FRAME nil means the selected frame."
    (ignore fontset frame)
    (error "Window system is not in use or not initialized")))

(unless (fboundp 'fontset-list)
  (defun fontset-list ()
    "Return a list of all defined fontset names."
    '("-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")))

(unless (fboundp 'new-fontset)
  (defun new-fontset (name fontlist)
    "Create a new fontset NAME from font information in FONTLIST."
    (unless (stringp name)
      (signal 'wrong-type-argument (list 'stringp name)))
    (ignore fontlist)
    (error "Fontset name must be in XLFD format")))

(unless (fboundp 'query-fontset)
  (defun query-fontset (pattern &optional regexpp)
    "Return the name of a fontset that matches PATTERN.
The value is nil if there is no matching fontset.
PATTERN can contain `*' or `?' as a wildcard
just as X font name matching algorithm allows.
If REGEXPP is non-nil, PATTERN is a regular expression."
    (ignore pattern regexpp)
    (error "Window system is not in use or not initialized")))

(unless (fboundp 'set-fontset-font)
  (defun set-fontset-font (fontset characters font-spec &optional frame add)
    "Modify FONTSET to use font specification in FONT-SPEC for displaying CHARACTERS."
    (ignore font-spec frame add)
    (cond ((and (stringp fontset) (not (equal fontset "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-default")))
           (error "Fontset ‘%s’ does not exist" fontset))
          ((null fontset) (error "Can’t use fontsets in non-GUI frames"))
          ((null characters) nil)
          ((characterp characters)
           (if (< characters 128)
               (error "Can’t set a font for partial ASCII range") nil))
          ((consp characters)
           (if (and (characterp (car characters)) (<= (car characters) 127))
               (error "Can’t set a font for partial ASCII range") nil))
          ((symbolp characters)
           (error "Invalid script or charset name: %s" characters))
          (t (signal 'wrong-type-argument (list 'characterp characters))))))

(provide 'emacs-cc-fontset-1)
