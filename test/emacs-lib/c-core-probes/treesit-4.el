(treesit-parser-notifiers
 (condition-case e (treesit-parser-notifiers nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-notifiers (current-buffer)) (error (list (car e) (cdr e)))))
(treesit-parser-p
 (treesit-parser-p nil)
 (treesit-parser-p (current-buffer)))
(treesit-parser-remove-notifier
 (condition-case e (treesit-parser-remove-notifier nil 'ignore) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-remove-notifier (current-buffer) 'ignore) (error (list (car e) (cdr e)))))
(treesit-parser-root-node
 (condition-case e (treesit-parser-root-node nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-root-node (current-buffer)) (error (list (car e) (cdr e)))))
(treesit-parser-set-embed-level
 (condition-case e (treesit-parser-set-embed-level nil nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-set-embed-level nil 2) (error (list (car e) (cdr e)))))
(treesit-parser-set-included-ranges
 (condition-case e (treesit-parser-set-included-ranges nil nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-set-included-ranges nil '((1 . 3))) (error (list (car e) (cdr e)))))
(treesit-parser-tag
 (condition-case e (treesit-parser-tag nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parser-tag (current-buffer)) (error (list (car e) (cdr e)))))
(treesit-parse-string
 (condition-case e (treesit-parse-string nil 'python) (error (list (car e) (cdr e))))
 (condition-case e (treesit-parse-string "x" 4) (error (list (car e) (cdr e)))))
(treesit-pattern-expand
 (treesit-pattern-expand :anchor)
 (treesit-pattern-expand '(identifier @name)))
(treesit-query-capture
 (condition-case e (treesit-query-capture nil nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-query-capture nil 4 1 4 t t) (error (list (car e) (cdr e)))))
(treesit-query-compile
 (condition-case e (treesit-query-compile nil nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-query-compile 'python nil) (error (list (car e) (cdr e)))))
(treesit-query-eagerly-compiled-p
 (condition-case e (treesit-query-eagerly-compiled-p nil) (error (list (car e) (cdr e))))
 (condition-case e (treesit-query-eagerly-compiled-p '(query)) (error (list (car e) (cdr e)))))
