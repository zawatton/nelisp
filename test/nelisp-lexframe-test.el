;;; nelisp-lexframe-test.el --- ERT for Doc 104 Phase 3 nelisp-lexframe  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 104 Phase 3 Stage 3.a — pure-elisp ert tests for
;; `lisp/nelisp-lexframe.el'.  Validates the 11 lexframe / stack
;; operations against the contract that Stage 3.b's Rust-direct
;; `mirror_frame_*' helpers will mirror.  Stage 3.a runs the elisp
;; surface standalone — no Rust integration yet.
;;
;; Test cells are plain integers / cons cells; the contract
;; verified is *identity preservation* (= the same cell object
;; survives push → capture → push-captured round-trips), not the
;; particular Sexp::Cell wrapping that the live runtime uses.  See
;; Doc 104 §2.3 + §5.3 for the closure-write-through invariant
;; this protects.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

;; Same record-primitive mocks as `nelisp-env-test' (runs
;; standalone in `make test' without the nelisp binary).
(unless (fboundp 'nelisp--make-record)
  (defun nelisp--make-record (tag &rest slots)
    (apply #'record tag slots)))

(unless (fboundp 'nelisp--record-ref)
  (defun nelisp--record-ref (rec idx)
    (aref rec (1+ idx))))

(unless (fboundp 'nelisp--record-set)
  (defun nelisp--record-set (rec idx val)
    (aset rec (1+ idx) val)
    val))

(unless (fboundp 'nelisp--record-type)
  (defun nelisp--record-type (rec)
    (aref rec 0)))

(require 'nelisp-lexframe)

;; ---- single-frame construction ----

(ert-deftest nelisp-lexframe/make-empty ()
  (let ((f (nelisp-lexframe-make)))
    (should (nelisp-lexframe-p f))
    (should (= (nelisp-lexframe-count f) 0))))

(ert-deftest nelisp-lexframe/p-rejects-non-record ()
  (should-not (nelisp-lexframe-p nil))
  (should-not (nelisp-lexframe-p 'symbol))
  (should-not (nelisp-lexframe-p 42))
  (should-not (nelisp-lexframe-p (nelisp-lexframe-stack-make))))

(ert-deftest nelisp-lexframe/make-custom-bucket-count ()
  ;; Smaller bucket count still works (collision tolerant by
  ;; design; the linear scan is correct at any size).
  (let ((f (nelisp-lexframe-make 4)))
    (should (nelisp-lexframe-p f))
    (nelisp-lexframe-bind f "a" 1)
    (nelisp-lexframe-bind f "b" 2)
    (nelisp-lexframe-bind f "c" 3)
    (nelisp-lexframe-bind f "d" 4)
    (nelisp-lexframe-bind f "e" 5)
    ;; All five reachable even though buckets < entries.
    (should (= (nelisp-lexframe-lookup f "a") 1))
    (should (= (nelisp-lexframe-lookup f "e") 5))
    (should (= (nelisp-lexframe-count f) 5))))

;; ---- bind / lookup ----

(ert-deftest nelisp-lexframe/bind-and-lookup ()
  (let ((f (nelisp-lexframe-make)))
    (should (eq (nelisp-lexframe-lookup f "x") nelisp--unbound-marker))
    (nelisp-lexframe-bind f "x" 42)
    (should (= (nelisp-lexframe-lookup f "x") 42))
    (should (= (nelisp-lexframe-count f) 1))))

(ert-deftest nelisp-lexframe/bind-overwrites ()
  (let ((f (nelisp-lexframe-make)))
    (nelisp-lexframe-bind f "x" 1)
    (nelisp-lexframe-bind f "x" 2)
    (should (= (nelisp-lexframe-lookup f "x") 2))
    (should (= (nelisp-lexframe-count f) 1))))

(ert-deftest nelisp-lexframe/bind-preserves-cell-identity ()
  ;; Doc 104 §2.3 + §5.3 — closure write-through invariant: the
  ;; *same* cell object must come back from lookup, not a copy.
  (let* ((f (nelisp-lexframe-make))
         (cell (cons 'init nil)))
    (nelisp-lexframe-bind f "x" cell)
    (should (eq (nelisp-lexframe-lookup f "x") cell))))

(ert-deftest nelisp-lexframe/lookup-returns-unbound-sentinel ()
  ;; `nelisp--unbound-marker' is the documented sentinel — callers
  ;; rely on `eq' for the absent check.
  (let ((f (nelisp-lexframe-make)))
    (should (eq (nelisp-lexframe-lookup f "absent")
                nelisp--unbound-marker))))

;; ---- stack construction ----

(ert-deftest nelisp-lexframe-stack/make-empty ()
  (let ((s (nelisp-lexframe-stack-make)))
    (should (nelisp-lexframe-stack-p s))
    (should (= (nelisp-lexframe-stack-depth s) 0))))

(ert-deftest nelisp-lexframe-stack/p-rejects-non-record ()
  (should-not (nelisp-lexframe-stack-p nil))
  (should-not (nelisp-lexframe-stack-p (nelisp-lexframe-make))))

;; ---- push / pop ----

(ert-deftest nelisp-lexframe-stack/push-pop-roundtrip ()
  (let* ((s (nelisp-lexframe-stack-make))
         (f1 (nelisp-lexframe-make))
         (f2 (nelisp-lexframe-make)))
    (nelisp-lexframe-stack-push! s f1)
    (should (= (nelisp-lexframe-stack-depth s) 1))
    (nelisp-lexframe-stack-push! s f2)
    (should (= (nelisp-lexframe-stack-depth s) 2))
    (should (eq (nelisp-lexframe-stack-pop! s) f2))
    (should (= (nelisp-lexframe-stack-depth s) 1))
    (should (eq (nelisp-lexframe-stack-pop! s) f1))
    (should (= (nelisp-lexframe-stack-depth s) 0))))

(ert-deftest nelisp-lexframe-stack/pop-empty-returns-nil ()
  (let ((s (nelisp-lexframe-stack-make)))
    (should (null (nelisp-lexframe-stack-pop! s)))
    (should (= (nelisp-lexframe-stack-depth s) 0))))

(ert-deftest nelisp-lexframe-stack/push-preserves-record-identity ()
  ;; The stack record itself is the same object after push/pop;
  ;; consumers hold the record and observe mutations.  Without
  ;; this, the Rust-side `Env::frames_record' would have to be
  ;; rebound on every push (= bad mutation story).
  (let* ((s (nelisp-lexframe-stack-make))
         (f (nelisp-lexframe-make)))
    (nelisp-lexframe-stack-push! s f)
    (should (nelisp-lexframe-stack-p s))
    (should (= (nelisp-lexframe-stack-depth s) 1))
    (nelisp-lexframe-stack-pop! s)
    (should (nelisp-lexframe-stack-p s))
    (should (= (nelisp-lexframe-stack-depth s) 0))))

(ert-deftest nelisp-lexframe-stack/push-grows-backing ()
  ;; Initial capacity = 8.  Push 20 frames; backing should auto-
  ;; double until it holds all of them, depth tracks accurately.
  (let ((s (nelisp-lexframe-stack-make 4))
        (i 0))
    (while (< i 20)
      (nelisp-lexframe-stack-push! s (nelisp-lexframe-make))
      (setq i (1+ i)))
    (should (= (nelisp-lexframe-stack-depth s) 20))
    ;; Pop them all back, ensure depth decrements cleanly.
    (while (> (nelisp-lexframe-stack-depth s) 0)
      (nelisp-lexframe-stack-pop! s))
    (should (= (nelisp-lexframe-stack-depth s) 0))))

;; ---- find — innermost first ----

(ert-deftest nelisp-lexframe-stack/find-innermost-wins ()
  ;; (let ((x 1)) (let ((x 2)) x)) — inner shadow wins.
  (let* ((s (nelisp-lexframe-stack-make))
         (outer (nelisp-lexframe-make))
         (inner (nelisp-lexframe-make)))
    (nelisp-lexframe-bind outer "x" 1)
    (nelisp-lexframe-bind inner "x" 2)
    (nelisp-lexframe-stack-push! s outer)
    (nelisp-lexframe-stack-push! s inner)
    (should (= (nelisp-lexframe-stack-find s "x") 2))
    ;; Pop inner: outer's binding now visible.
    (nelisp-lexframe-stack-pop! s)
    (should (= (nelisp-lexframe-stack-find s "x") 1))))

(ert-deftest nelisp-lexframe-stack/find-falls-through-frames ()
  ;; (let ((x 1)) (let ((y 2)) (lookup x))) — outer-only var
  ;; still reachable from inner frame.
  (let* ((s (nelisp-lexframe-stack-make))
         (outer (nelisp-lexframe-make))
         (inner (nelisp-lexframe-make)))
    (nelisp-lexframe-bind outer "x" 100)
    (nelisp-lexframe-bind inner "y" 200)
    (nelisp-lexframe-stack-push! s outer)
    (nelisp-lexframe-stack-push! s inner)
    (should (= (nelisp-lexframe-stack-find s "x") 100))
    (should (= (nelisp-lexframe-stack-find s "y") 200))
    (should (eq (nelisp-lexframe-stack-find s "z")
                nelisp--unbound-marker))))

(ert-deftest nelisp-lexframe-stack/find-empty-stack ()
  (let ((s (nelisp-lexframe-stack-make)))
    (should (eq (nelisp-lexframe-stack-find s "anything")
                nelisp--unbound-marker))))

;; ---- capture / push-captured roundtrip ----

(ert-deftest nelisp-lexframe-stack/capture-preserves-cell-identity ()
  ;; Doc 104 §5.3 regression — closure capture must NOT clone the
  ;; cell.  The captured alist's cdr is `eq' to the originating
  ;; binding's cell.
  (let* ((s (nelisp-lexframe-stack-make))
         (cell-x (cons 'x-init nil))
         (cell-y (cons 'y-init nil))
         (frame (nelisp-lexframe-make)))
    (nelisp-lexframe-bind frame "x" cell-x)
    (nelisp-lexframe-bind frame "y" cell-y)
    (nelisp-lexframe-stack-push! s frame)
    (let* ((captured (nelisp-lexframe-stack-capture s))
           (x-pair (assoc "x" captured))
           (y-pair (assoc "y" captured)))
      (should x-pair)
      (should y-pair)
      (should (eq (cdr x-pair) cell-x))
      (should (eq (cdr y-pair) cell-y)))))

(ert-deftest nelisp-lexframe-stack/capture-innermost-shadows-outer ()
  ;; capture walks innermost-first; inner binding for the same
  ;; NAME wins.  Outer's cell is invisible to the captured alist.
  (let* ((s (nelisp-lexframe-stack-make))
         (outer-cell (cons 'outer nil))
         (inner-cell (cons 'inner nil))
         (outer (nelisp-lexframe-make))
         (inner (nelisp-lexframe-make)))
    (nelisp-lexframe-bind outer "x" outer-cell)
    (nelisp-lexframe-bind inner "x" inner-cell)
    (nelisp-lexframe-stack-push! s outer)
    (nelisp-lexframe-stack-push! s inner)
    (let ((captured (nelisp-lexframe-stack-capture s)))
      (should (= (length captured) 1))
      (should (eq (cdr (car captured)) inner-cell)))))

(ert-deftest nelisp-lexframe-stack/capture-empty-stack ()
  (let ((s (nelisp-lexframe-stack-make)))
    (should (null (nelisp-lexframe-stack-capture s)))))

(ert-deftest nelisp-lexframe-stack/push-captured-roundtrip ()
  ;; capture → push-captured! reproduces the binding set on a
  ;; fresh stack.  Cell identities survive the round-trip.
  (let* ((src (nelisp-lexframe-stack-make))
         (dst (nelisp-lexframe-stack-make))
         (cell-x (cons 'x nil))
         (cell-y (cons 'y nil))
         (frame (nelisp-lexframe-make)))
    (nelisp-lexframe-bind frame "x" cell-x)
    (nelisp-lexframe-bind frame "y" cell-y)
    (nelisp-lexframe-stack-push! src frame)
    (let* ((captured (nelisp-lexframe-stack-capture src))
           (new-frame (nelisp-lexframe-stack-push-captured! dst captured)))
      (should (nelisp-lexframe-p new-frame))
      (should (= (nelisp-lexframe-stack-depth dst) 1))
      ;; Round-trip preserved cell identity (= the cell-x / cell-y
      ;; conses are the same objects, so a "setq" on either
      ;; observer would be seen by both).
      (should (eq (nelisp-lexframe-stack-find dst "x") cell-x))
      (should (eq (nelisp-lexframe-stack-find dst "y") cell-y)))))

(ert-deftest nelisp-lexframe-stack/push-captured-closure-write-through ()
  ;; The closure-write-through invariant in action: simulate a
  ;; let-bound x captured by two lambdas; mutating via one
  ;; observer must be visible via the other.  Cells here are
  ;; mutable cons cells; the runtime uses Sexp::Cell with the
  ;; same identity contract.
  (let* ((root (nelisp-lexframe-stack-make))
         (origin (nelisp-lexframe-make))
         (cell (cons 0 nil)))
    (nelisp-lexframe-bind origin "x" cell)
    (nelisp-lexframe-stack-push! root origin)
    (let* ((captured (nelisp-lexframe-stack-capture root))
           ;; Two "closure" stacks each get their own copy of the
           ;; captured alist; both alist cdrs point to the same
           ;; cell.
           (closure-a (nelisp-lexframe-stack-make))
           (closure-b (nelisp-lexframe-stack-make)))
      (nelisp-lexframe-stack-push-captured! closure-a captured)
      (nelisp-lexframe-stack-push-captured! closure-b captured)
      ;; Mutate through closure-a's cell view.
      (let ((found (nelisp-lexframe-stack-find closure-a "x")))
        (setcar found 42))
      ;; closure-b observes the mutation.
      (should (= (car (nelisp-lexframe-stack-find closure-b "x")) 42))
      ;; The originating frame also observes (= write-through).
      (should (= (car (nelisp-lexframe-stack-find root "x")) 42)))))

;;; nelisp-lexframe-test.el ends here
