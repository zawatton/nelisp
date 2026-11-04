;;; emacs-cc-treesit-4.el --- tree-sitter C primitive fallbacks -*- lexical-binding: t; -*-

(require 'emacs-cc-treesit-5)

(defun emacs-cc-treesit-4--parser-error (parser)
  (signal 'wrong-type-argument (list 'treesit-parser-p parser)))

(unless (fboundp 'treesit-parser-notifiers)
  (defun treesit-parser-notifiers (parser)
    "Return the list of after-change notifier functions for PARSER."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parser-p)
  (defun treesit-parser-p (object)
    "Return t if OBJECT is a tree-sitter parser."
    (and object nil)))
(unless (fboundp 'treesit-parser-remove-notifier)
  (defun treesit-parser-remove-notifier (parser function)
    "Remove FUNCTION from PARSER's after-change notifiers."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parser-root-node)
  (defun treesit-parser-root-node (parser)
    "Return the root node of PARSER."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parser-set-embed-level)
  (defun treesit-parser-set-embed-level (parser level)
    "Set the embed level for PARSER to LEVEL."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parser-set-included-ranges)
  (defun treesit-parser-set-included-ranges (parser ranges)
    "Limit PARSER to RANGES."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parser-tag)
  (defun treesit-parser-tag (parser)
    "Return PARSER's tag."
    (emacs-cc-treesit-4--parser-error parser)))
(unless (fboundp 'treesit-parse-string)
  (defun treesit-parse-string (string language)
    "Parse STRING using a parser for LANGUAGE."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (unless (symbolp language)
      (signal 'wrong-type-argument (list 'symbolp language)))
    (emacs-cc-treesit-load-language language)
    (signal 'treesit-error '("Parsing with installed grammars is not implemented"))))

(unless (fboundp 'treesit-pattern-expand)
  (defun treesit-pattern-expand (pattern)
    "Expand PATTERN to its string form."
    (cond ((eq pattern :anchor) ".")
          ((eq pattern :?) "?") ((eq pattern :*) "*") ((eq pattern :+) "+")
          ((eq pattern :equal) "#eq?") ((eq pattern :match) "#match?")
          ((stringp pattern) (prin1-to-string pattern))
          ((vectorp pattern)
           (concat "[" (mapconcat #'treesit-pattern-expand (append pattern nil) " ") "]"))
          ((consp pattern)
           (concat "(" (mapconcat #'treesit-pattern-expand pattern " ") ")"))
          ((symbolp pattern)
           (let ((name (symbol-name pattern)))
             (cond ((string-match "\\`\\(.+\\):\\'" name)
                    (concat (match-string 1 name) ":"))
                   ((string-prefix-p "@" name) name)
                   ((string= name "_") "_")
                   ((string-prefix-p ":" name) (signal 'void-variable (list pattern)))
                   (t name))))
          (t (signal 'wrong-type-argument (list 'treesit-pattern-p pattern))))))

(unless (fboundp 'treesit-query-capture)
  (defun treesit-query-capture (node query &optional beg end node-only grouped)
    "Query NODE with patterns in QUERY."
    (unless (treesit-query-p query)
      (signal 'wrong-type-argument (list 'treesit-query-p query)))
    (unless (or (null node) (symbolp node))
      (signal 'wrong-type-argument
              (list '(or treesit-node-p treesit-parser-p symbolp) node)))
    (emacs-cc-treesit-load-language node)
    (signal 'treesit-error '("Parsing with installed grammars is not implemented"))))
(unless (fboundp 'treesit-query-compile)
  (defun treesit-query-compile (language query &optional eager)
    "Compile QUERY to a compiled query."
    (unless (treesit-query-p query)
      (signal 'wrong-type-argument (list 'treesit-query-p query)))
    (unless (symbolp language)
      (signal 'wrong-type-argument (list 'symbolp language)))
    (when eager
      (emacs-cc-treesit-load-language
       (if (treesit-compiled-query-p query) (treesit-query-language query) language))
      (signal 'treesit-error '("Queries with installed grammars are not implemented")))
    (if (treesit-compiled-query-p query) query
      (emacs-cc-treesit-make-query language query))))
(unless (fboundp 'treesit-query-eagerly-compiled-p)
  (defun treesit-query-eagerly-compiled-p (query)
    "Return non-nil if QUERY is eagerly compiled."
    (unless (treesit-compiled-query-p query)
      (signal 'wrong-type-argument (list 'treesit-compiled-query-p query)))
    (and (aref query 3) t)))

(provide 'emacs-cc-treesit-4)
