;;; charprop.el --- Load the generated GNU Unicode property registry -*- lexical-binding: t; -*-

;;; Code:

(require 'emacs-cc-chartab-1)

;; The standalone reader represents the generated #^[...] literal as a
;; vector.  Import that layout before GNU mule-cmds validates the table.
(defun emacs-charprop--define (original name table &optional docstring)
  (when (and (vectorp table) (= (length table) 73)
             (eq (aref table 2) 'char-code-property-table))
    (setq table (emacs-cc-chartab-1--read-table table)))
  (funcall original name table docstring))

(when (emacs-char-table--standalone-p)
  (unless (advice-member-p #'emacs-charprop--define 'define-char-code-property)
    (advice-add 'define-char-code-property :around #'emacs-charprop--define)))

(unless (featurep 'charprop)
  (load "international/charprop.el" nil 'no-message t t))

;;; charprop.el ends here
