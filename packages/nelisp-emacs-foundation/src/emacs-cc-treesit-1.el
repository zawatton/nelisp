;;; emacs-cc-treesit-1.el --- Tree-sitter C primitive fallbacks -*- lexical-binding: t; -*-

(defvar emacs-cc-treesit-1--linecol-state nil)
(when (fboundp 'make-variable-buffer-local)
  (make-variable-buffer-local 'emacs-cc-treesit-1--linecol-state))

(unless (fboundp 'treesit-compiled-query-p)
  (defun treesit-compiled-query-p (object)
    "Return t if OBJECT is a compiled tree-sitter query."
    (ignore object)
    nil))

(unless (fboundp 'treesit-grammar-location)
  (defun treesit-grammar-location (language)
    "Return the absolute file name of the grammar file for LANGUAGE."
    (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language)))
    nil))

(unless (fboundp 'treesit-induce-sparse-tree)
  (defun treesit-induce-sparse-tree (root predicate &optional process-fn depth)
    "Create a sparse tree of ROOT's subtree."
    (ignore predicate process-fn depth)
    (unless (and (fboundp 'treesit-node-p) (treesit-node-p root))
      (signal 'wrong-type-argument (list 'treesit-node-p root)))
    nil))

(unless (fboundp 'treesit-language-abi-version)
  (defun treesit-language-abi-version (&optional language)
    "Return the ABI version of the tree-sitter grammar for LANGUAGE."
    (when language
      (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language))))
    nil))

(unless (fboundp 'treesit-language-available-p)
  (defun treesit-language-available-p (language &optional detail)
    "Return non-nil if LANGUAGE exists and is loadable."
    (unless (symbolp language) (signal 'wrong-type-argument (list 'symbolp language)))
    (if detail (cons nil (list 'treesit-load-language-error language)) nil)))

(unless (fboundp 'treesit-library-abi-version)
  (defun treesit-library-abi-version (&optional min-compatible)
    "Return the language ABI version of the tree-sitter library."
    (if min-compatible 13 14)))

(unless (fboundp 'treesit--linecol-at)
  (defun treesit--linecol-at (pos)
    "Test buffer-local linecol cache and return line and column at POS."
    (unless (or (integerp pos) (and (markerp pos) (marker-position pos)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p pos)))
    (save-excursion
      (goto-char pos)
      (cons (1- (line-number-at-pos))
            (if (= (line-number-at-pos) 1)
                (1+ (current-column))
              (current-column))))))

(unless (fboundp 'treesit--linecol-cache)
  (defun treesit--linecol-cache ()
    "Return the buffer-local linecol cache for debugging."
    (let ((state (or emacs-cc-treesit-1--linecol-state '(0 0 0))))
      (list :line (nth 0 state) :col (nth 1 state) :bytepos (nth 2 state)))))

(unless (fboundp 'treesit--linecol-cache-set)
  (defun treesit--linecol-cache-set (line col bytepos)
    "Set the linecol cache for the current buffer."
    (setq emacs-cc-treesit-1--linecol-state (list line col bytepos))))

(unless (fboundp 'treesit-node-check)
  (defun treesit-node-check (node property)
    "Return non-nil if NODE has PROPERTY."
    (unless (or (null node) (and (fboundp 'treesit-node-p) (treesit-node-p node)))
      (signal 'wrong-type-argument (list 'treesit-node-p node)))
    nil))

(unless (fboundp 'treesit-node-child-by-field-name)
  (defun treesit-node-child-by-field-name (node field-name)
    "Return the child of NODE with FIELD-NAME."
    (if (null node)
        nil
      (unless (and (fboundp 'treesit-node-p) (treesit-node-p node))
        (signal 'wrong-type-argument (list 'treesit-node-p node)))
      (unless (stringp field-name) (signal 'wrong-type-argument (list 'stringp field-name)))
      nil)))

(unless (fboundp 'treesit-node-child-count)
  (defun treesit-node-child-count (node &optional named)
    "Return the number of children of NODE."
    (ignore named)
    (unless (or (null node) (and (fboundp 'treesit-node-p) (treesit-node-p node)))
      (signal 'wrong-type-argument (list 'treesit-node-p node)))
    nil))

(provide 'emacs-cc-treesit-1)
