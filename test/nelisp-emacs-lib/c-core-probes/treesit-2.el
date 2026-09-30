(treesit-node-child
 (treesit-node-child nil 0)
 (treesit-node-child nil -1 t)
 (treesit-node-child t 0)
 (with-temp-buffer (insert "changed text") (treesit-node-child nil (point-max))))

(treesit-node-descendant-for-range
 (treesit-node-descendant-for-range nil 1 1)
 (treesit-node-descendant-for-range nil 1 4 t)
 (treesit-node-descendant-for-range t 1 2)
 (with-temp-buffer (insert "range") (treesit-node-descendant-for-range nil (point-min) (point-max))))

(treesit-node-end
 (treesit-node-end nil)
 (treesit-node-end t)
 (with-temp-buffer (insert "end") (treesit-node-end nil)))

(treesit-node-eq
 (treesit-node-eq nil nil)
 (treesit-node-eq nil t)
 (treesit-node-eq t nil)
 (treesit-node-eq t t)
 (treesit-node-eq nil nil))

(treesit-node-field-name-for-child
 (treesit-node-field-name-for-child nil 0)
 (treesit-node-field-name-for-child nil -1)
 (treesit-node-field-name-for-child t 0)
 (with-temp-buffer (insert "field") (treesit-node-field-name-for-child nil (point))))

(treesit-node-first-child-for-pos
 (treesit-node-first-child-for-pos nil 0)
 (treesit-node-first-child-for-pos nil 1 t)
 (treesit-node-first-child-for-pos t 0)
 (with-temp-buffer (insert "position") (treesit-node-first-child-for-pos nil (point-max))))

(treesit-node-match-p
 (treesit-node-match-p nil "identifier")
 (treesit-node-match-p nil 'unknown-thing)
 (treesit-node-match-p t "identifier")
 (treesit-node-match-p nil '(not-a-predicate)))

(treesit-node-next-sibling
 (treesit-node-next-sibling nil)
 (treesit-node-next-sibling nil t)
 (treesit-node-next-sibling t)
 (with-temp-buffer (insert "siblings") (treesit-node-next-sibling nil t)))

(treesit-node-parent
 (treesit-node-parent nil)
 (treesit-node-parent t)
 (with-temp-buffer (insert "parent") (treesit-node-parent nil)))

(treesit-node-parser
 (treesit-node-parser nil)
 (treesit-node-parser t)
 (with-temp-buffer (insert "parser") (treesit-node-parser nil)))

(treesit-node-p
 (treesit-node-p nil)
 (treesit-node-p t)
 (with-temp-buffer (insert "object") (treesit-node-p (current-buffer))))

(treesit-node-prev-sibling
 (treesit-node-prev-sibling nil)
 (treesit-node-prev-sibling nil t)
 (treesit-node-prev-sibling t)
 (with-temp-buffer (insert "siblings") (treesit-node-prev-sibling nil t)))
