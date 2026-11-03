;;; emacs-cc-treesit-2.el --- tree-sitter node primitives -*- lexical-binding: t; -*-

;; In builds without the external tree-sitter library, no node objects can
;; exist.  Preserve nil behavior and GNU's node type checks in that build.

(defun emacs-cc-treesit-2--check-node (node)
  "Signal GNU's type error unless NODE is nil or a tree-sitter node."
  (when (and node (not (treesit-node-p node)))
    (signal 'wrong-type-argument (list 'treesit-node-p node))))

(unless (fboundp 'treesit-node-child)
  (defun treesit-node-child (node n &optional named)
    "Return the Nth child of NODE, or nil when no tree-sitter node exists."
    (ignore n named)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-descendant-for-range)
  (defun treesit-node-descendant-for-range (node beg end &optional named)
    "Return the smallest node covering BEG to END, or nil without tree-sitter."
    (ignore beg end named)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-end)
  (defun treesit-node-end (node)
    "Return NODE's end position, or nil when NODE is nil."
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-eq)
  (defun treesit-node-eq (node1 node2)
    "Return non-nil when NODE1 and NODE2 refer to the same tree-sitter node."
    (when (and node1 node2)
      (emacs-cc-treesit-2--check-node node1)
      (emacs-cc-treesit-2--check-node node2))
    (and node1 node2 (eq node1 node2))))

(unless (fboundp 'treesit-node-field-name-for-child)
  (defun treesit-node-field-name-for-child (node n)
    "Return the field name of child N of NODE, or nil without tree-sitter."
    (ignore n)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-first-child-for-pos)
  (defun treesit-node-first-child-for-pos (node pos &optional named)
    "Return NODE's first child extending beyond POS, or nil without tree-sitter."
    (ignore pos named)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-match-p)
  (defun treesit-node-match-p (node predicate &optional ignore-missing)
    "Return whether NODE matches PREDICATE, or nil without tree-sitter."
    (ignore predicate ignore-missing)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-next-sibling)
  (defun treesit-node-next-sibling (node &optional named)
    "Return NODE's next sibling, or nil without tree-sitter."
    (ignore named)
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-parent)
  (defun treesit-node-parent (node)
    "Return NODE's immediate parent, or nil without tree-sitter."
    (emacs-cc-treesit-2--check-node node)
    nil))

(unless (fboundp 'treesit-node-parser)
  (defun treesit-node-parser (node)
    "Return the parser to which NODE belongs."
    (unless (and node (treesit-node-p node))
      (signal 'wrong-type-argument (list 'treesit-node-p node)))
    nil))

(unless (fboundp 'treesit-node-p)
  (defun treesit-node-p (object)
    "Return t if OBJECT is a tree-sitter node."
    (ignore object)
    nil))

(unless (fboundp 'treesit-node-prev-sibling)
  (defun treesit-node-prev-sibling (node &optional named)
    "Return NODE's previous sibling, or nil without tree-sitter."
    (ignore named)
    (emacs-cc-treesit-2--check-node node)
    nil))

(provide 'emacs-cc-treesit-2)
