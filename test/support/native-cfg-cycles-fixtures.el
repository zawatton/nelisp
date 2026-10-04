;;; native-cfg-cycles-fixtures.el --- Genuine cyclic bytecode -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(defvar cycles-count-state 0)
(defvar cycles-count-history nil)
(defun cycles-count-step ()
  "One effectful counted iteration, invoked through the ordinary F1 call path."
  (when (> cycles-count-state 0)
    (push cycles-count-state cycles-count-history)
    (setq cycles-count-state (1- cycles-count-state)) t))
(defun native-cfg-cycles-fixtures ()
  "Return bounded native/interpreter fixtures and one intentionally closed SCC."
  (list
   ;; Entry backedge, cdr replaces the carried list in a distinct result root.
   (list 'entry (make-byte-code 257 (unibyte-string 137 131 8 0 65 130 0 0 135) [] 2)
         '((nil) ((a)) ((a b c d)) ((a b c d e f))))
   ;; A counted loop starts after a constant prefix (not at entry).
   (list 'counted (make-byte-code 0 (unibyte-string 192 32 137 131 11 0 136 192 130 1 0 135)
                                 [cycles-count-step] 2) '(nil))
   ;; Extended offsets encode the same legal top-of-stack copy independently.
   (list 'ref6 (make-byte-code 257 (unibyte-string 6 0 135) [] 2)
         '((nil) ((a . b)) ([v]) ("text")))
   (list 'ref7 (make-byte-code 257 (unibyte-string 7 0 0 135) [] 2)
         '((nil) ((a . b)) ([v]) ("text")))
   ;; Both SCC nodes have an incoming edge from entry, so neither dominates it.
   (list 'irreducible (make-byte-code 257
                                    (unibyte-string 137 131 8 0 65 130 8 0
                                                    137 131 16 0 65 130 4 0 135) [] 2)
         '((nil) ((a)) ((a b c d)) ((a b c d e))))
   (list 'swap (make-byte-code 0 (unibyte-string 192 32 137 131 10 0 136 130 0 0 135)
                              [cycles-swap-step] 2) '(nil))
   (list 'closed (make-byte-code 0 (unibyte-string 130 0 0) [] 0) nil)))
(provide 'native-cfg-cycles-fixtures)
