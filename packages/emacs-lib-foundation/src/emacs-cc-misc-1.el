;;; emacs-cc-misc-1.el --- tree-sitter tracking predicates -*- lexical-binding: t; -*-

;; GNU Emacs builds without tree-sitter have no parser or line/column
;; tracking state.  These fallbacks preserve the argument checks and report
;; the disabled state used by such builds.

(unless (fboundp 'treesit-parser-tracking-line-column-p)
  (defun treesit-parser-tracking-line-column-p (arg1)
    "Return whether ARG1 tracks tree-sitter line and column positions."
    (unless (and (fboundp 'treesit-parser-p) (treesit-parser-p arg1))
      (signal 'wrong-type-argument (list 'treesit-parser-p arg1)))
    nil))

(unless (fboundp 'treesit-tracking-line-column-p)
  (defun treesit-tracking-line-column-p (&optional arg1)
    "Return whether ARG1 tracks tree-sitter line and column positions."
    (let ((buffer (or arg1 (current-buffer))))
      (unless (bufferp buffer)
        (signal 'wrong-type-argument (list 'bufferp buffer)))
      nil)))

(provide 'emacs-cc-misc-1)
