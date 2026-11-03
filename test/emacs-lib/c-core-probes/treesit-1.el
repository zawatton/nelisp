(treesit-compiled-query-p
 (treesit-compiled-query-p nil)
 (treesit-compiled-query-p '(query . object)))
(treesit-grammar-location
 (treesit-grammar-location 'c)
 (condition-case e (treesit-grammar-location 7) (error e)))
(treesit-induce-sparse-tree
 (condition-case e (treesit-induce-sparse-tree nil "x") (error e))
 (condition-case e (treesit-induce-sparse-tree 7 (lambda (_) t) nil 2) (error e)))
(treesit-language-abi-version
 (treesit-language-abi-version)
 (treesit-language-abi-version 'c))
(treesit-language-available-p
 (treesit-language-available-p 'c)
 (car (treesit-language-available-p 'c t))
 (condition-case e (treesit-language-available-p "c" t) (error e)))
(treesit-library-abi-version
 (treesit-library-abi-version)
 (treesit-library-abi-version t))
(treesit--linecol-at
 (with-temp-buffer (insert "abc\ndef") (treesit--linecol-at 3))
 (with-temp-buffer (insert "abc\ndef") (treesit--linecol-at 6))
 (condition-case e (treesit--linecol-at "bad") (error e)))
(treesit--linecol-cache
 (treesit--linecol-cache)
 (with-temp-buffer (treesit--linecol-cache-set 4 7 21) (treesit--linecol-cache)))
(treesit--linecol-cache-set
 (progn (treesit--linecol-cache-set 2 5 9) (treesit--linecol-cache))
 (with-temp-buffer (treesit--linecol-cache-set 8 1 14) (treesit--linecol-cache)))
(treesit-node-check
 (treesit-node-check nil 'named)
 (condition-case e (treesit-node-check 5 'live) (error e)))
(treesit-node-child-by-field-name
 (treesit-node-child-by-field-name nil "field")
 (condition-case e (treesit-node-child-by-field-name 5 "field") (error e))
 (condition-case e (treesit-node-child-by-field-name nil nil) (error e)))
(treesit-node-child-count
 (treesit-node-child-count nil)
 (treesit-node-child-count nil t)
 (condition-case e (treesit-node-child-count 5) (error e)))
