;;; emacs-cc-treesit-5.el --- tree-sitter query primitives -*- lexical-binding: t; -*-

(require 'emacs-cc-treesit-1)

(defun emacs-cc-treesit-5--pattern (x)
  (cond
   ((null x) "")
   ((stringp x) (prin1-to-string x))
   ((symbolp x)
    (let ((name (symbol-name x)))
      (cond ((string= name ":?") "?") ((string= name ":*") "*")
            ((string= name ":+") "+") ((string= name ":equal") "=")
            ((string= name ":eq?") "#eq?") ((string= name ":match") "#match?")
            ((string= name ":match?") "#match?")
            ((string= name ":anchor") ".")
            ((string-prefix-p "@" name) name)
            ((string-suffix-p ":" name) name)
            (t name))))
   ((vectorp x) (concat "[" (mapconcat #'emacs-cc-treesit-5--pattern
                                        (append x nil) " ") "]"))
   ((consp x)
    (concat "(" (mapconcat #'emacs-cc-treesit-5--pattern x " ") ")"))
   (t (signal 'wrong-type-argument (list 'sequencep x)))))

(defun emacs-cc-treesit-5--render (x)
  (cond ((stringp x) (mapconcat #'number-to-string (string-to-list x) " "))
        ((or (listp x) (vectorp x))
         (mapconcat #'emacs-cc-treesit-5--pattern
                    (if (vectorp x) (append x nil) x) " "))
        (t (signal 'wrong-type-argument (list 'sequencep x)))))

(unless (fboundp 'treesit-query-expand)
  (defun treesit-query-expand (query)
    "Expand sexp QUERY to its string form."
    (unless (or (stringp query) (listp query) (vectorp query))
      (signal 'wrong-type-argument (list 'sequencep query)))
    (emacs-cc-treesit-5--render query)))

(unless (fboundp 'treesit-query-language)
  (defun treesit-query-language (query)
    "Return the language of QUERY. QUERY has to be a compiled query."
    (unless (treesit-compiled-query-p query)
      (signal 'wrong-type-argument (list 'treesit-compiled-query-p query)))
    (aref query 1)))

(unless (fboundp 'treesit-query-p)
  (defun treesit-query-p (object)
    "Return t if OBJECT is a generic tree-sitter query."
    (or (stringp object) (consp object) (treesit-compiled-query-p object))))

(unless (fboundp 'treesit-query-source)
  (defun treesit-query-source (query)
    "Return the (string or sexp) source of QUERY. QUERY has to be a compiled query."
    (unless (treesit-compiled-query-p query)
      (signal 'wrong-type-argument (list 'treesit-compiled-query-p query)))
    (aref query 2)))

(unless (fboundp 'treesit-search-forward)
  (defun treesit-search-forward (start predicate &optional backward all)
    "Search for node matching PREDICATE in the parse tree of START."
    (signal 'wrong-type-argument (list 'treesit-node-p start))))

(unless (fboundp 'treesit-search-subtree)
  (defun treesit-search-subtree (node predicate &optional backward all depth)
    "Traverse the parse tree of NODE depth-first using PREDICATE."
    (signal 'wrong-type-argument (list 'treesit-node-p node))))

(unless (fboundp 'treesit-subtree-stat)
  (defun treesit-subtree-stat (node)
    "Return information about the subtree of NODE."
    (signal 'wrong-type-argument (list 'treesit-node-p node))))

(provide 'emacs-cc-treesit-5)
