;;; standalone-eqhash-check.el --- Pinned GNU hash semantics -*- lexical-binding: t; -*-

;; Ordinary hash regression cases. Weak-GC semantics remain in the full
;; tools/ai/eqhash-semantics.el gate pending the external GNU GC port.
;; The adjacent .expected file is pinned GNU output for this same selection.
(defvar eqhash--ordinary-only t)
(load (expand-file-name "../tools/ai/eqhash-semantics.el"
                        (file-name-directory load-file-name)) nil t)
(kill-emacs 0)

;;; standalone-eqhash-check.el ends here
