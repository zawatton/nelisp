;;; nelisp-char-table-vector-bridge-test.el --- tagged-vector bridge tests  -*- lexical-binding: t; -*-

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (file-name-directory this))
       (repo-root (file-name-directory (directory-file-name test-dir))))
  (dolist (dir '("lisp" "src" "scripts"))
    (add-to-list 'load-path (expand-file-name dir repo-root))))

(require 'nelisp-standalone-build)

(defun nelisp-char-table-vector-bridge-test--tree-member-p (needle tree)
  "Return non-nil when NEEDLE occurs in TREE."
  (cond
   ((equal needle tree) t)
   ((consp tree)
    (or (nelisp-char-table-vector-bridge-test--tree-member-p needle (car tree))
        (nelisp-char-table-vector-bridge-test--tree-member-p needle (cdr tree))))))

(ert-deftest nelisp-char-table-vector-bridge/dispatch-shape ()
  (dolist (symbol '(bf_nemacs_char_table_vector_p
                    bf_nemacs_char_table_call
                    bf_aref_nemacs_bridge
                    bf_aset_nemacs_bridge))
    (should (nelisp-char-table-vector-bridge-test--tree-member-p
             symbol nelisp-standalone--applyfn-bf-helpers)))
  (dolist (name '("nelisp--raw-aref" "nelisp--raw-aset"
                  "nelisp--char-table-vector-bridge-p"))
    (should (assoc (list :lit name) nelisp-standalone--applyfn-bf-arms))
    (should (member name nelisp-standalone--applyfn-bf-builtins))
    (should (member name nelisp-standalone--reader-builtins))))

(ert-deftest nelisp-char-table-vector-bridge/ordinary-arrays-stay-native ()
  (let ((aref-arm (cdr (assoc '(:lit "aref")
                              nelisp-standalone--applyfn-bf-arms)))
        (aset-arm (cdr (assoc '(:lit "aset")
                              nelisp-standalone--applyfn-bf-arms))))
    (should (equal aref-arm '(bf_aref_nemacs_bridge args env out)))
    (should (equal aset-arm '(bf_aset_nemacs_bridge args env out)))
    (should (nelisp-char-table-vector-bridge-test--tree-member-p
             'bf_aref_checked nelisp-standalone--applyfn-bf-helpers))
    (should (nelisp-char-table-vector-bridge-test--tree-member-p
             'bf_aset_checked nelisp-standalone--applyfn-bf-helpers))))

(provide 'nelisp-char-table-vector-bridge-test)

;;; nelisp-char-table-vector-bridge-test.el ends here
