;;; nelisp-cc-fxir-analysis.el --- Bounded FXIR diagnostic side tables -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; M0 source query only: no transformation or runtime admission.  A call
;; consumes CALLEES before its arguments, like the lowerer and harvester.
;; Structural paths are list indices in the original function body.

;;; Code:
(require 'cl-lib)
(require 'nelisp-cc-fxir-effects)

(defconst nelisp-cc-fxir-analysis-schema 1
  "Diagnostic schema, independent of unchanged artifact formats.")

(defun nelisp-cc-fxir-analysis-row (row &optional code)
  "Return bounded facts for harvested ROW, optionally checking producer CODE.
ROW is (NAME ID NLOCALS CALLEES (:fn BODY) (MIN MAX REST) [MARKER]).
CODE is a producer vector, never a live closure identity witness.  Without
CODE callee identity and constant values remain unverified.  Syntax digests
authenticate neither live entries nor source manifests.  Invalid input
returns :valid nil; even valid input never authorizes M1 transformation."
  (let ((sites nil) (constants nil) (effects nil) (refusals nil)
        (remaining nil) (nodes 0) (name nil) (id nil) (digest nil)
        (valid nil) (constant-proof t))
    (cl-labels
        ((fail (why) (push why refusals) (throw 'invalid nil))
         (refuse (why) (cl-pushnew why refusals))
         (effect (what) (cl-pushnew what effects))
         ;; Operand-independent diagnostics come from the canonical table.
         (op-effects (op)
           (dolist (what (plist-get (nelisp-cc-fxir-effects-of op) :diagnostic))
             (effect what)))
         (shape (n lo hi)
           (unless (and (proper-list-p n) (<= lo (length n))
                        (or (null hi) (<= (length n) hi)))
             (fail :invalid-node-shape)))
         (index (k) (unless (and (integerp k) (>= k 0)) (fail :invalid-index)))
         (operand (k op)
           (index k)
           (when code
             (unless (and (vectorp code) (> k 0) (< k (length code))
                          (eql (aref code (1- k)) op))
               (fail :code-operand-mismatch))))
         (slot (k)
           (index k)
           (unless (< k (nth 2 row)) (fail :invalid-slot)))
         (children (n start path depth)
           (let ((i start))
             (dolist (child (nthcdr start n))
               (walk child (append path (list i)) (1+ depth))
               (setq i (1+ i)))))
         (walk (n path depth)
           (setq nodes (1+ nodes))
           (when (or (> nodes 10000) (> depth 48)) (fail :analysis-budget))
           (shape n 1 nil)
           (pcase (car n)
             (:nil (shape n 1 1))
             (:slot (shape n 2 2) (slot (nth 1 n)))
             (:const
              (shape n 2 2) (operand (nth 1 n) 1)
              (let ((kind (if (not code) :unverified
                            (let ((value (aref code (nth 1 n))))
                              (cond ((null value) :nil) ((eq value t) :true)
                                    ;; Host integer width is not a target proof.
                                    ((integerp value) :integer-unverified-range)
                                    (t :non-immediate))))))
                (unless (memq kind '(:nil :true))
                  (setq constant-proof nil) (refuse :unproved-constant))
                (when (eq kind :non-immediate) (refuse :non-immediate-constant))
                (push (list :origin (list id path) :owner id
                            :index (nth 1 n) :kind kind) constants)))
             ((or :varref :alias)
              (shape n 2 2) (operand (nth 1 n) (if (eq (car n) :varref) 19 18))
              (op-effects (car n))
              (refuse :mutable-global-or-alias))
             (:setslot
              (shape n 3 3) (slot (nth 1 n))
              (walk (nth 2 n) (append path '(2)) (1+ depth)))
             (:call
              (shape n 3 nil) (operand (nth 1 n) 16)
              (unless (and (integerp (nth 2 n)) (>= (nth 2 n) 0)
                           (= (nth 2 n) (- (length n) 3))) (fail :call-arity-mismatch))
              (unless remaining (fail :missing-callee))
              (let ((callee (pop remaining)))
                (unless (and callee (symbolp callee) (not (keywordp callee)))
                  (fail :invalid-callee))
                (when code
                  (let* ((k (nth 1 n)) (cache (aref code k)))
                    (unless (and (< (1+ k) (length code))
                                 (eql (aref code (1+ k)) (nth 2 n))
                                 (vectorp cache) (= (length cache) 5)
                                 (eq (aref cache 0) callee)) (fail :callee-code-mismatch))))
                (push (list :id (list id path) :origin (list id path)
                            :callee callee :constant-owner id :cache-index (nth 1 n)
                            :argc (nth 2 n) :identity-proof (if code :producer-code :absent))
                      sites))
              (op-effects :call)
              (refuse :arbitrary-call) (children n 3 path depth))
             (:inline
              (shape n 3 nil) (index (nth 1 n)) (operand (nth 2 n) (nth 1 n))
              (let ((argc (cdr (assq (nth 1 n) '((4 . 2) (5 . 2) (6 . 2) (7 . 1)
                                                (13 . 2) (14 . 2) (15 . 1))))))
                (unless (and argc (= argc (- (length n) 3)))
                  (fail :inline-opcode-or-arity)))
              (op-effects :inline)
              (refuse :unproved-operation) (children n 3 path depth))
             ((or :seq :prog1 :and :or)
              (when (eq (car n) :prog1) (shape n 2 nil)) (children n 1 path depth))
             (:if (shape n 4 4) (children n 1 path depth))
             (:while
              (shape n 3 3) (op-effects :while) (refuse :loop) (children n 1 path depth))
             (:let
              (shape n 4 4)
              (unless (proper-list-p (nth 1 n)) (fail :invalid-bindings))
              (index (nth 3 n))
              (let ((i 0) (nbind 0))
                (dolist (binding (nth 1 n))
                  (shape binding 3 3)
                  (pcase (car binding)
                    (:store (slot (nth 1 binding)))
                    (:varbind
                     (operand (nth 1 binding) 20) (setq nbind (1+ nbind))
                     (op-effects :varbind) (refuse :dynamic-binding))
                    (_ (fail :invalid-binding)))
                  (walk (nth 2 binding) (append path (list 1 i 2)) (1+ depth))
                  (setq i (1+ i)))
                (unless (= nbind (nth 3 n)) (fail :binding-count-mismatch)))
              (walk (nth 2 n) (append path '(2)) (1+ depth)))
             ((or :catch :throw :unwind-protect)
              (shape n 3 3) (op-effects (car n))
              (refuse :handler-or-nonlocal-exit) (children n 1 path depth))
             (:condition-case
              (shape n 4 nil) (when (nth 1 n) (slot (nth 1 n)))
              (op-effects :condition-case) (refuse :handler-or-nonlocal-exit)
              (walk (nth 2 n) (append path '(2)) (1+ depth))
              (when (nth 3 n) (walk (nth 3 n) (append path '(3)) (1+ depth)))
              (let ((i 4))
                (dolist (clause (nthcdr 4 n))
                  (shape clause 2 2) (index (car clause))
                  (walk (nth 1 clause) (append path (list i 1)) (1+ depth))
                  (setq i (1+ i)))))
             (_ (fail :unknown-node)))))
      (catch 'invalid
        (shape row 6 7)
        (setq name (car row) id (nth 1 row))
        (unless (and name (symbolp name) (not (keywordp name))
                     (integerp id) (>= id 0) (integerp (nth 2 row)) (>= (nth 2 row) 0)
                     (proper-list-p (nth 3 row))) (fail :invalid-row))
        (setq remaining (copy-sequence (nth 3 row)))
        (shape (nth 5 row) 3 3)
        (let ((arity (nth 5 row)))
          (unless (and (cl-every #'integerp arity) (>= (nth 0 arity) 0)
                       (<= (nth 0 arity) (nth 2 row))
                       (if (= (nth 2 arity) -1)
                           (and (>= (nth 1 arity) (nth 0 arity))
                                (<= (nth 1 arity) (nth 2 row)))
                         (and (= (nth 1 arity) -1)
                              (>= (nth 2 arity) (nth 0 arity))
                              (< (nth 2 arity) (nth 2 row)))))
            (fail :invalid-arity))
          (unless (and (= (nth 0 arity) (nth 1 arity)) (= (nth 2 arity) -1))
            (refuse :optional-or-rest-arity)))
        (shape (nth 4 row) 2 2)
        (unless (eq (car (nth 4 row)) :fn) (fail :missing-fn))
        (walk (nth 1 (nth 4 row)) '(1) 0)
        (when remaining (fail :extra-callee))
        (let ((print-length nil) (print-level nil) (print-circle nil)
              (print-quoted t) (print-gensym t))
          (setq digest (secure-hash 'sha256 (prin1-to-string (cl-subseq row 0 6)))))
        (setq valid t)))
    (list :schema nelisp-cc-fxir-analysis-schema :valid valid
          :row (and valid (list name id)) :syntax-sha256 digest
          :digest-provenance :harvested-row-syntax
          :callee-proof (if code :producer-code :positional-only)
          :constant-proof constant-proof :nodes nodes
          :sites (nreverse sites) :constants (nreverse constants)
          :effects (nreverse effects) :refusals (cons :m0-no-transform (nreverse refusals)))))

(provide 'nelisp-cc-fxir-analysis)
;;; nelisp-cc-fxir-analysis.el ends here
