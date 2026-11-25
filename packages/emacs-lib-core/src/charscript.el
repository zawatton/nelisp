;;; charscript.el --- Load the GNU script data  -*- lexical-binding: t; -*-

;; GNU src/character.c:syms_of_character sets up the table before loadup
;; evaluates charscript.el.  Category 5 comes from international/characters.el.
(put 'char-script-table 'char-table-extra-slots 1)
(defvar char-script-table)
(setq char-script-table (make-char-table 'char-script-table))
(unless (category-docstring ?5)
  (define-category ?5 "symbol"))
(load (expand-file-name
       "emacs-lisp/international/charscript.el"
       (or (and (boundp 'nelisp-emacs-vendor-root) nelisp-emacs-vendor-root)
           (expand-file-name "../../../vendor"
                             (file-name-directory load-file-name))))
      nil 'no-message t t)

;;; charscript.el ends here
