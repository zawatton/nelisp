;;; emacs-cc-xsettings-1.el --- xsettings C primitives -*- lexical-binding: t; -*-

(defvar tool-bar-style)

(unless (fboundp 'font-get-system-font)
  (defun font-get-system-font ()
    "Get the system default fixed width font.
The font is returned as either a font-spec or font name."
    ;; GNU batch builds without a window system have no system font.
    (when (display-graphic-p)
      (frame-parameter (selected-frame) 'font))))

(unless (fboundp 'font-get-system-normal-font)
  (defun font-get-system-normal-font ()
    "Get the system default application font.
The font is returned as either a font-spec or font name."
    ;; GNU batch builds without a window system have no system font.
    (when (display-graphic-p)
      (frame-parameter (selected-frame) 'font))))

(unless (fboundp 'tool-bar-get-system-style)
  (defun tool-bar-get-system-style ()
    "Get the system tool bar style.
If no system tool bar style is known, return `tool-bar-style' if set to a
known style.  Otherwise return image."
    (if (and (boundp 'tool-bar-style)
             (memq (symbol-value 'tool-bar-style) '(image text both)))
        (symbol-value 'tool-bar-style)
      'image)))

(provide 'emacs-cc-xsettings-1)
