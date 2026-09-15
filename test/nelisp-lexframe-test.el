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

(ert-deftest nelisp-lexframe-uninterned-capture-roundtrip ()
  (let* ((a (make-symbol "identity-local"))
         (b (make-symbol "identity-local"))
         (ca (list 1)) (cb (list 2))
         (frame (nelisp-lexframe-make-from-alist
                 (list (cons a ca) (cons b cb) (cons "identity-local" 3))))
         (stack (nelisp-lexframe-stack-make)))
    (should (= (nelisp-lexframe-count frame) 3))
    (should (eq (nelisp-lexframe-lookup frame a) ca))
    (should (eq (nelisp-lexframe-lookup frame b) cb))
    (should (= (nelisp-lexframe-lookup frame "identity-local") 3))
    (nelisp-lexframe-stack-push! stack frame)
    (cl-letf (((symbol-function 'nl-jit-call-out-1)
               (lambda (&rest _) (ert-fail "Unsupported native symbol-key capture"))))
      (let* ((captured (nelisp-lexframe-stack-capture-to-depth stack 1))
             (restored (nelisp-lexframe-stack-push-captured! stack captured)))
        (should (eq (nelisp-lexframe-lookup restored a) ca))
        (should (eq (nelisp-lexframe-lookup restored b) cb))
        (setcar ca 9)
        (should (equal (nelisp-lexframe-lookup restored a) '(9)))
        (should (equal (nelisp-lexframe-lookup restored b) '(2)))))))

(ert-deftest nelisp-lexframe-uninterned-declarations ()
  (let* ((a (make-symbol "identity-special"))
         (b (make-symbol "identity-special"))
         (stack (nelisp-lexframe-stack-make))
         (frame (nelisp-lexframe-make)))
    (nelisp-lexframe-stack-push! stack frame)
    (nelisp-lexframe-declare-special! frame a)
    (should (nelisp-lexframe-stack-local-special-p stack a))
    (should-not (nelisp-lexframe-stack-local-special-p stack b))
    (should-not (nelisp-lexframe-stack-local-special-p stack "identity-special"))
    (nelisp-lexframe-stack-push-captured!
     stack (nelisp-lexframe-stack-capture stack))
    (should (nelisp-lexframe-stack-local-special-p stack a))
    (should-not (nelisp-lexframe-stack-local-special-p stack b))))

(ert-deftest nelisp-lexframe-symbol-key-hash-opt-in ()
  (let ((ht (nelisp--fast-hash-make 1))
        (a (make-symbol "same")) (b (make-symbol "same")))
    (should-error (nelisp--fast-hash-put ht a 9))
    (nelisp--fast-hash-put ht "same" 0)
    (nelisp--fast-hash-put ht a 1 t)
    (nelisp--fast-hash-put ht b 2 t)
    (should (= (nelisp--fast-hash-count ht) 3))
    (should (= (nelisp--fast-hash-get ht "same") 0))
    (should (= (nelisp--fast-hash-get ht a nil t) 1))
    (should (= (nelisp--fast-hash-get ht b nil t) 2))
    (nelisp--fast-hash-put ht a 3 t)
    (should (nelisp--fast-hash-remove ht a t))
    (should (eq (nelisp--fast-hash-get ht a 'absent t) 'absent))
    (should (= (nelisp--fast-hash-get ht b nil t) 2))
    (should (= (nelisp--fast-hash-get ht "same") 0))
    (should (= (nelisp--fast-hash-count ht) 2))))

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

(ert-deftest nelisp-lexframe/binding-kind-is-stored-per-binding ()
  (let ((frame (nelisp-lexframe-make)))
    (nelisp-lexframe-bind frame "lexical" (list 1))
    (nelisp-lexframe-bind frame "dynamic" (list 2) t)
    (should-not (nelisp-lexframe-dynamic-p frame "lexical"))
    (should (nelisp-lexframe-dynamic-p frame (copy-sequence "dynamic")))
    (should-not (nelisp-lexframe-dynamic-p frame "absent"))
    ;; Installing a new binding explicitly replaces its classification.
    (nelisp-lexframe-bind frame "dynamic" (list 3))
    (should-not (nelisp-lexframe-dynamic-p frame "dynamic"))))

(ert-deftest nelisp-lexframe/kind-search-and-pop-restore ()
  (let ((stack (nelisp-lexframe-stack-make))
        (outer (nelisp-lexframe-make)) (inner (nelisp-lexframe-make))
        (lexical (list 1)) (dynamic (list 2)))
    (nelisp-lexframe-bind outer "x" lexical)
    (nelisp-lexframe-bind inner "x" dynamic t)
    (nelisp-lexframe-stack-push! stack outer)
    (nelisp-lexframe-stack-push! stack inner)
    (should (eq (nelisp-lexframe-stack-find-lexical stack "x") lexical))
    (should (eq (nelisp-lexframe-stack-find-dynamic stack "x") dynamic))
    (nelisp-lexframe-stack-pop! stack)
    (should (eq (nelisp-lexframe-stack-find-lexical stack "x") lexical))
    (should (eq (nelisp-lexframe-stack-find-dynamic stack "x")
                nelisp--unbound-marker))))

(ert-deftest nelisp-lexframe/capture-excludes-dynamic-without-hiding-lexical ()
  (let ((stack (nelisp-lexframe-stack-make))
        (outer (nelisp-lexframe-make)) (inner (nelisp-lexframe-make))
        (cell (list 1)))
    (nelisp-lexframe-bind outer "x" cell)
    (nelisp-lexframe-bind inner "x" (list 2) t)
    (nelisp-lexframe-bind inner "dynamic-only" (list 3) t)
    (nelisp-lexframe-stack-push! stack outer)
    (nelisp-lexframe-stack-push! stack inner)
    (let* ((captured (nelisp-lexframe-stack-capture stack))
           (copy (nelisp-lexframe-stack-make)))
      (should (equal (mapcar #'car captured) '("x")))
      (nelisp-lexframe-stack-push-captured! copy captured)
      (setcar cell 9)
      (should (eq (nelisp-lexframe-stack-find-lexical copy "x") cell))
      (should (= (car (nelisp-lexframe-stack-find-lexical copy "x")) 9)))))

(ert-deftest nelisp-lexframe/capture-depth-selects-compatible-walker ()
  (let ((stack (nelisp-lexframe-stack-make))
        (outer (nelisp-lexframe-make)) (inner (nelisp-lexframe-make))
        (cell (list 1)) called)
    (nelisp-lexframe-bind outer "x" cell)
    (nelisp-lexframe-bind inner "x" (list 2) t)
    (nelisp-lexframe-stack-push! stack outer)
    (nelisp-lexframe-stack-push! stack inner)
    (cl-letf (((symbol-function 'nl-jit-call-out-1)
               (lambda (name args)
                 (setq called (list name args)) 'native-result)))
      (should (equal (nelisp-lexframe-stack-capture-to-depth stack 2)
                     (list (cons "x" cell))))
      (should-not called)
      ;; Excluding the dynamic frame keeps the existing native fast path.
      (should (eq (nelisp-lexframe-stack-capture-to-depth stack 1) 'native-result))
      (should (equal (car called) "nl_capture_descend_native"))
      (should (eq (aref (cadr called) 0) stack))
      (should (= (aref (cadr called) 1) 1)))))

(ert-deftest nelisp-lexframe/legacy-frame-keeps-lexical-cells ()
  (let ((frame (nelisp--make-record 'nelisp-lexframe (nelisp--fast-hash-make 16))))
    (nelisp-lexframe-bind frame "old" 7)
    (should-not (nelisp-lexframe-dynamic-p frame "old"))
    (should-error (nelisp-lexframe-bind frame "new" 8 t))
    (should-error (nelisp-lexframe-mark-scope! frame))
    (should-error (nelisp-lexframe-declare-special! frame 'new))
    (should (= (nelisp-lexframe-lookup frame "old") 7))
    (should (eq (nelisp-lexframe-lookup frame "new") nelisp--unbound-marker))))

(ert-deftest nelisp-lexframe/call-boundary-preserves-dynamic-visibility ()
  (let ((stack (nelisp-lexframe-stack-make))
        (caller (nelisp-lexframe-make)) (callee (nelisp-lexframe-make))
        (lexical (list 1)) (dynamic (list 2)) (captured (list 3)))
    (nelisp-lexframe-bind caller "caller" lexical)
    (nelisp-lexframe-bind caller "dynamic" dynamic t)
    (nelisp-lexframe-bind callee "captured" captured)
    (nelisp-lexframe-stack-push! stack caller)
    (nelisp-lexframe-stack-push! stack callee)
    (nelisp-lexframe-mark-scope! callee)
    (should (eq (nelisp-lexframe-stack-find-lexical stack "caller")
                nelisp--unbound-marker))
    (should (eq (nelisp-lexframe-stack-find-dynamic stack "dynamic") dynamic))
    (should (equal (nelisp-lexframe-stack-capture stack)
                   (list (cons "captured" captured))))
    (should (equal (nelisp-lexframe-stack-capture-to-depth stack 2)
                   (list (cons "captured" captured))))
    (nelisp-lexframe-stack-pop! stack)
    (should (eq (nelisp-lexframe-stack-find-lexical stack "caller") lexical))))

(ert-deftest nelisp-lexframe/local-declarations-follow-lexical-environments ()
  (let ((stack (nelisp-lexframe-stack-make))
        (outer (nelisp-lexframe-make)) (empty (nelisp-lexframe-make))
        (dynamic (nelisp-lexframe-make)))
    (nelisp-lexframe-mark-scope! outer)
    (nelisp-lexframe-bind dynamic "dynamic" (list 1) t)
    (dolist (frame (list outer empty dynamic))
      (nelisp-lexframe-stack-push! stack frame))
    (nelisp-lexframe-stack-declare-special! stack 'declared)
    (should (equal (nelisp-lexframe-local-declarations outer) '("declared")))
    (should-not (nelisp-lexframe-local-declarations empty))
    (should-not (nelisp-lexframe-local-declarations dynamic))
    (should (nelisp-lexframe-stack-local-special-p stack 'declared))
    (should (eq (nelisp-lexframe-stack-find-dynamic stack "declared")
                nelisp--unbound-marker))
    (nelisp-lexframe-stack-pop! stack)
    (nelisp-lexframe-stack-pop! stack)
    (let ((callee (nelisp-lexframe-make)))
      (nelisp-lexframe-mark-scope! callee)
      (nelisp-lexframe-stack-push! stack callee)
      (should-not (nelisp-lexframe-stack-local-special-p stack 'declared)))))

(ert-deftest nelisp-lexframe/local-declarations-survive-capture ()
  (let ((stack (nelisp-lexframe-stack-make)) (frame (nelisp-lexframe-make)))
    (nelisp-lexframe-mark-scope! frame)
    (nelisp-lexframe-stack-push! stack frame)
    (let ((before (nelisp-lexframe-stack-capture stack)))
      (nelisp-lexframe-stack-declare-special! stack 'declared)
      (should-not before))
    (let* ((captured (nelisp-lexframe-stack-capture-to-depth stack 1))
           (restored (nelisp-lexframe-make-from-alist captured)))
      (should (equal captured '("declared")))
      (should (= (nelisp-lexframe-count restored) 0))
      (should (equal (nelisp-lexframe-local-declarations restored) '("declared"))))))

;;; nelisp-lexframe-test.el ends here
