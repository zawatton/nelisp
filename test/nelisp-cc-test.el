;;; nelisp-cc-test.el --- ERT for nelisp-cc Phase 7.1.1 scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Smoke tests for the Phase 7.1.1 SSA IR scaffold in
;; `src/nelisp-cc.el'.  Coverage:
;;
;;   1. entry block creation by `nelisp-cc--ssa-make-function'
;;   2. instruction append order in a block
;;   3. predecessor / successor bidirectional edges
;;   4. pp / from-sexp round-trip preserves structure
;;   5. verifier rejects orphan (unreachable) blocks
;;   6. value IDs are unique within a function
;;   7. add-use is idempotent and updates use-list
;;   8. verifier rejects a one-sided pred/succ edge

;;; Code:

(require 'ert)
(require 'nelisp-cc)

;;; (1) entry block ---------------------------------------------------

(ert-deftest nelisp-cc-ssa-make-function-creates-entry-block ()
  "A fresh function has an ENTRY block in its BLOCKS table."
  (let* ((fn (nelisp-cc--ssa-make-function 'foo '(int int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (blocks (nelisp-cc--ssa-function-blocks fn)))
    (should entry)
    (should (memq entry blocks))
    (should (= 1 (length blocks)))
    (should (equal "entry" (nelisp-cc--ssa-block-label entry)))
    ;; Two params with :param def-point and the requested types.
    (let ((params (nelisp-cc--ssa-function-params fn)))
      (should (= 2 (length params)))
      (dolist (p params)
        (should (eq :param (nelisp-cc--ssa-value-def-point p))))
      (should (eq 'int (nelisp-cc--ssa-value-type (car params)))))))

;;; (2) instruction append --------------------------------------------

(ert-deftest nelisp-cc-ssa-add-instr-appends-to-block ()
  "`add-instr' appends to BLOCK-INSTRS in order; def-point is patched."
  (let* ((fn (nelisp-cc--ssa-make-function 'add2 '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (vsum (nelisp-cc--ssa-make-value fn 'int))
         (vret (nelisp-cc--ssa-make-value fn 'int))
         (i1 (nelisp-cc--ssa-add-instr fn entry 'add (list param param) vsum))
         (i2 (nelisp-cc--ssa-add-instr fn entry 'identity (list vsum) vret))
         (i3 (nelisp-cc--ssa-add-instr fn entry 'return (list vret) nil)))
    (should (equal (list i1 i2 i3) (nelisp-cc--ssa-block-instrs entry)))
    (should (eq i1 (nelisp-cc--ssa-value-def-point vsum)))
    (should (eq i2 (nelisp-cc--ssa-value-def-point vret)))
    (should (null (nelisp-cc--ssa-instr-def i3)))))

;;; (3) edge bidirectionality -----------------------------------------

(ert-deftest nelisp-cc-ssa-link-blocks-bidirectional ()
  "`link-blocks' adds the edge in both directions and is idempotent."
  (let* ((fn (nelisp-cc--ssa-make-function 'branchy nil))
         (a  (nelisp-cc--ssa-function-entry fn))
         (b  (nelisp-cc--ssa-make-block fn "then"))
         (c  (nelisp-cc--ssa-make-block fn "else")))
    (nelisp-cc--ssa-link-blocks a b)
    (nelisp-cc--ssa-link-blocks a c)
    (should (memq b (nelisp-cc--ssa-block-successors a)))
    (should (memq c (nelisp-cc--ssa-block-successors a)))
    (should (memq a (nelisp-cc--ssa-block-predecessors b)))
    (should (memq a (nelisp-cc--ssa-block-predecessors c)))
    ;; Idempotent — calling twice keeps the lists at length 2 / 1.
    (nelisp-cc--ssa-link-blocks a b)
    (should (= 2 (length (nelisp-cc--ssa-block-successors a))))
    (should (= 1 (length (nelisp-cc--ssa-block-predecessors b))))))

;;; (4) pp / from-sexp round-trip -------------------------------------

(ert-deftest nelisp-cc-ssa-pp-roundtrip ()
  "`pp' followed by `from-sexp' reproduces the printed shape."
  (let* ((fn (nelisp-cc--ssa-make-function 'rt '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (then (nelisp-cc--ssa-make-block fn "then"))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (v1 (nelisp-cc--ssa-make-value fn 'int)))
    (nelisp-cc--ssa-add-instr fn entry 'add (list param param) v1)
    (nelisp-cc--ssa-add-instr fn entry 'branch (list v1) nil)
    (nelisp-cc--ssa-add-instr fn then  'return (list v1) nil)
    (nelisp-cc--ssa-link-blocks entry then)
    (nelisp-cc--ssa-verify-function fn)
    (let* ((sexp (nelisp-cc--ssa-pp fn))
           (fn2 (nelisp-cc--ssa-from-sexp sexp))
           (sexp2 (nelisp-cc--ssa-pp fn2)))
      (should (equal sexp sexp2))
      (should (eq 'rt (nelisp-cc--ssa-function-name fn2)))
      (should (= 2 (length (nelisp-cc--ssa-function-blocks fn2))))
      ;; Round-tripped function is itself well-formed.
      (should (eq t (nelisp-cc--ssa-verify-function fn2))))))

;;; (5) verifier — orphan block ---------------------------------------

(ert-deftest nelisp-cc-ssa-verify-detects-orphan-block ()
  "An unreachable block is rejected by the verifier."
  (let* ((fn (nelisp-cc--ssa-make-function 'orphan nil))
         (_  (nelisp-cc--ssa-make-block fn "orphan")))
    ;; Note: no link from entry → orphan.  Verifier must complain.
    ;; `should-error' returns (ERR-SYMBOL . DATA-LIST) with DATA-LIST
    ;; spliced — so the first datum sits at (cadr err).
    (let ((err (should-error (nelisp-cc--ssa-verify-function fn)
                             :type 'nelisp-cc-verify-error)))
      (should (eq :orphan-block (cadr err))))))

;;; (6) value ID uniqueness -------------------------------------------

(ert-deftest nelisp-cc-ssa-value-id-uniqueness ()
  "Builders hand out monotonically increasing, unique value IDs."
  (let* ((fn (nelisp-cc--ssa-make-function 'unique '(int int int)))
         (params (nelisp-cc--ssa-function-params fn))
         (locals (list (nelisp-cc--ssa-make-value fn 'int)
                       (nelisp-cc--ssa-make-value fn 'int)
                       (nelisp-cc--ssa-make-value fn nil)))
         (ids (mapcar #'nelisp-cc--ssa-value-id (append params locals))))
    (should (equal ids (number-sequence 0 5)))
    (should (= (length ids) (length (delete-dups (copy-sequence ids)))))))

;;; (7) add-use idempotency -------------------------------------------

(ert-deftest nelisp-cc-ssa-add-use-updates-list ()
  "`add-use' inserts on first call and is a no-op on repeat."
  (let* ((fn (nelisp-cc--ssa-make-function 'uses '(int)))
         (entry (nelisp-cc--ssa-function-entry fn))
         (param (car (nelisp-cc--ssa-function-params fn)))
         (vd (nelisp-cc--ssa-make-value fn 'int))
         (instr (nelisp-cc--ssa-add-instr fn entry 'inc (list param) vd)))
    ;; The first add-use happened inside add-instr.
    (should (memq instr (nelisp-cc--ssa-value-use-list param)))
    (should (= 1 (length (nelisp-cc--ssa-value-use-list param))))
    ;; Re-adding must not duplicate.
    (nelisp-cc--ssa-add-use param instr)
    (should (= 1 (length (nelisp-cc--ssa-value-use-list param))))))

;;; (8) verifier — broken edge ----------------------------------------

(ert-deftest nelisp-cc-ssa-verify-detects-broken-edge ()
  "A one-sided pred/succ edge is rejected by the verifier."
  (let* ((fn (nelisp-cc--ssa-make-function 'broken nil))
         (a  (nelisp-cc--ssa-function-entry fn))
         (b  (nelisp-cc--ssa-make-block fn "leaf")))
    ;; Manually corrupt: A's successors mention B but B's predecessors do not.
    (setf (nelisp-cc--ssa-block-successors a) (list b))
    ;; Reachability now passes (B is reachable), but the back-edge is missing.
    (let ((err (should-error (nelisp-cc--ssa-verify-function fn)
                             :type 'nelisp-cc-verify-error)))
      (should (eq :missing-predecessor-back-edge (cadr err))))))

(provide 'nelisp-cc-test)
;;; nelisp-cc-test.el ends here
