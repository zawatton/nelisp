;;; nelisp-sub-char-table-reference.el --- Logical sub-char-table model -*- lexical-binding: t; -*-

;; This file is a Lisp reference model for the proposed distinct native
;; sub-char-table kind.  Its records are host-side model objects only: this
;; predicate does not define or emulate GNU's physical record classification.

(defconst nelisp-sub-char-table-reference--brand
  (make-symbol "nelisp-sub-char-table-reference-brand"))

(defconst nelisp-sub-char-table-reference--kind
  'nelisp-sub-char-table-reference--node)

(defconst nelisp-sub-char-table-reference--max-char #x3fffff)

(defconst nelisp-sub-char-table-reference--depth-spans [65536 4096 128])

(defconst nelisp-sub-char-table-reference--contents-lengths [16 32 128])

(defun nelisp-sub-char-table-reference--valid-geometry-p
    (depth min-char contents)
  "Return non-nil when DEPTH, MIN-CHAR, and CONTENTS form a valid node."
  (and (integerp depth)
       (<= 1 depth 3)
       (integerp min-char)
       (<= 0 min-char nelisp-sub-char-table-reference--max-char)
       (vectorp contents)
       (let* ((index (1- depth))
              (span (aref nelisp-sub-char-table-reference--depth-spans index))
              (expected
               (aref nelisp-sub-char-table-reference--contents-lengths index)))
         (and (= (% min-char span) 0)
              (<= (+ min-char span -1)
                  nelisp-sub-char-table-reference--max-char)
              (= (length contents) expected)))))

(defun nelisp-sub-char-table-reference--make (depth min-char contents)
  "Make a logical reference node at DEPTH and MIN-CHAR retaining CONTENTS."
  (unless (nelisp-sub-char-table-reference--valid-geometry-p
           depth min-char contents)
    (signal 'wrong-type-argument
            (list 'nelisp-sub-char-table-reference--valid-geometry-p
                  depth min-char contents)))
  ;; The unique brand distinguishes these model nodes from user records that
  ;; happen to use the public sub-char-table class name or this record tag.
  (record nelisp-sub-char-table-reference--kind
          nelisp-sub-char-table-reference--brand depth min-char contents))

(defun nelisp-sub-char-table-reference--p (object)
  "Return non-nil only for valid nodes created by this reference model."
  (and (recordp object)
       (condition-case nil
           (and (eq (type-of object) nelisp-sub-char-table-reference--kind)
                (= (length object) 5)
                (eq (aref object 1)
                    nelisp-sub-char-table-reference--brand)
                (nelisp-sub-char-table-reference--valid-geometry-p
                 (aref object 2) (aref object 3) (aref object 4)))
         (error nil))))

(defun nelisp-sub-char-table-reference--slot (node index)
  "Return NODE metadata slot INDEX: 0 depth, 1 minimum, or 2 contents.
Metadata has no mutation operation in this API.  The contents vector itself
is returned by identity and remains mutable, matching the logical contract.
Wrong-kind and bad-index errors belong to this reference API and make no claim
about GNU's internal accessor conditions."
  (unless (nelisp-sub-char-table-reference--p node)
    (signal 'wrong-type-argument
            (list 'nelisp-sub-char-table-reference--p node)))
  (unless (integerp index)
    (signal 'wrong-type-argument (list 'integerp index)))
  (unless (<= 0 index 2)
    (signal 'args-out-of-range node index))
  (aref node (+ 2 index)))

(provide 'nelisp-sub-char-table-reference)

;;; nelisp-sub-char-table-reference.el ends here
