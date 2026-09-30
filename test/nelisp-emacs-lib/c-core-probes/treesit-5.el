(treesit-query-expand
  (treesit-query-expand '(identifier (string) @x))
  (treesit-query-expand '["foo" (bar baz)])
  (condition-case e (treesit-query-expand 1) (error e)))

(treesit-query-language
  (condition-case e (treesit-query-language nil) (error e))
  (condition-case e (treesit-query-language 'not-a-query) (error e)))

(treesit-query-p
  (treesit-query-p nil)
  (treesit-query-p '(identifier)))

(treesit-query-source
  (condition-case e (treesit-query-source nil) (error e))
  (condition-case e (treesit-query-source "query") (error e)))

(treesit-search-forward
  (condition-case e (treesit-search-forward nil "identifier") (error e))
  (with-temp-buffer (insert "changed")
    (condition-case e (treesit-search-forward nil "changed" t t) (error e))))

(treesit-search-subtree
  (condition-case e (treesit-search-subtree nil "identifier") (error e))
  (condition-case e (treesit-search-subtree nil "x" t t 2) (error e)))

(treesit-subtree-stat
  (condition-case e (treesit-subtree-stat nil) (error e))
  (condition-case e (treesit-subtree-stat 'not-a-node) (error e)))
