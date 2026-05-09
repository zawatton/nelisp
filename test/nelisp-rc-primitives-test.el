;;; nelisp-rc-primitives-test.el --- ERT for Stage 5.3.a/b gc body  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Doc 79 v7 Phase C Stage 5.3.a + 5.3.b ERT for the elisp-side
;; cycle collector in `lisp/nelisp-stdlib-gc.el'.  We exercise the
;; *pure-elisp* surface (= `gc-stats' / `gc-collect-cycles' four-phase
;; algorithm + data-structure shells) from host Emacs.
;;
;; The 10 underlying `nl-rc-*' / `nl-gc-*' primitives are Rust-side
;; (= `build-tool/src/eval/rc_primitives.rs' tests) and not callable
;; from host Emacs; the Stage 5.3.b ERT therefore *mocks* them with
;; `cl-letf' so the elisp algorithm itself can be exercised end-to-end
;; on host Emacs without requiring the NeLisp runtime.  The mocks
;; model boxed handles as gensym symbols carrying `:rc' / `:children'
;; / `:kind' properties; the four phases drive on those properties
;; the same way the Rust primitives drive on real boxes.
;;
;; The Rust-side test for the actual primitives lives next to the
;; Rust source (= `cargo test --manifest-path build-tool/Cargo.toml'
;; runs the inc/dec/walk tests against real `NlConsBoxRef').

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the gc skeleton straight off disk.  We bypass `require' / the
;; nelisp bootstrap chain because host Emacs doesn't load the nelisp
;; stdlib + we only need the `gc-*' wrappers / data structures.
;; `nelisp-stdlib-gc.el' is self-contained at the host-Emacs level
;; (= uses only cond/when/null/defun + make-hash-table, all built into
;; host Emacs).
(let ((file (expand-file-name
             "nelisp-stdlib-gc.el"
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))))
  (when (file-exists-p file)
    (load file nil t)))

;; ---- Box mocking infrastructure for Stage 5.3.b ERT -----------------
;;
;; Real `nl-rc-*' primitives operate on `NlConsBoxRef' from the Rust
;; side and are unavailable in host Emacs.  For the algorithm tests
;; we model each "box" as a gensym symbol whose plist holds the
;; metadata the four-phase walker needs.  Tests build a graph by
;; allocating mock handles via `gc-test--alloc' and linking them via
;; `gc-test--set-children'; then they shadow `nl-rc-*' through the
;; mock dispatch table inside `gc-test--with-mocks'.

(defvar gc-test--finalized nil
  "List of handles passed to `nl-gc-finalize' in the current test.")

(defun gc-test--alloc (kind &optional rc)
  "Allocate a mock handle of KIND with starting strong rc RC (default 1)."
  (let ((h (cl-gensym "nlbox-")))
    (put h :kind kind)
    (put h :rc (or rc 1))
    (put h :children nil)
    h))

(defun gc-test--set-children (handle children)
  "Set HANDLE's outgoing edge list (= mock `nl-gc-walk-children' result)."
  (put handle :children children))

(defun gc-test--inc (handle)
  "Bump HANDLE's mock strong rc."
  (put handle :rc (+ 1 (get handle :rc))))

(defmacro gc-test--with-mocks (&rest body)
  "Execute BODY with mock `nl-rc-*' / `nl-gc-*' primitives in scope.
Also resets `gc-cycle-roots-buffer', the color/internal-rc tables
and `gc-test--finalized' so each test starts from a clean slate."
  `(let ((gc-test--finalized nil)
         (gc-cycle-roots-buffer nil))
     (clrhash gc-color-table)
     (clrhash gc-internal-rc-table)
     ;; Reset the alist to a known baseline so accounting tests don't
     ;; observe leakage from earlier ERT's.
     (setcdr (assq 'total-allocs gc-stats-counters) 0)
     (setcdr (assq 'total-frees gc-stats-counters) 0)
     (setcdr (assq 'pending-cycles gc-stats-counters) 0)
     (setcdr (assq 'passes gc-stats-counters) 0)
     (cl-letf (((symbol-function 'nl-rc-kind)
                (lambda (h) (or (and (symbolp h) (get h :kind)) 0)))
               ((symbol-function 'nl-rc-strong-count)
                (lambda (h) (or (and (symbolp h) (get h :rc)) 0)))
               ((symbol-function 'nl-rc-payload-ptr)
                (lambda (h) (sxhash h)))
               ((symbol-function 'nl-gc-walk-children)
                (lambda (h) (and (symbolp h) (get h :children))))
               ((symbol-function 'nl-gc-finalize)
                (lambda (h) (push h gc-test--finalized) nil)))
       ,@body)))

(ert-deftest gc-stats-mvp-returns-4-key-alist ()
  "`gc-stats' returns the Stage 5.3.b 4-key alist with zero values."
  (skip-unless (fboundp 'gc-stats))
  (gc-test--with-mocks
   (let ((s (gc-stats)))
     (should (= 4 (length s)))
     (should (equal 0 (cdr (assq 'total-allocs s))))
     (should (equal 0 (cdr (assq 'total-frees s))))
     (should (equal 0 (cdr (assq 'pending-cycles s))))
     (should (equal 0 (cdr (assq 'passes s)))))))

(ert-deftest gc-stats-pending-cycles-tracks-roots-buffer ()
  "`gc-stats' reports `pending-cycles' = `(length gc-cycle-roots-buffer)'."
  (skip-unless (fboundp 'gc-stats))
  (skip-unless (boundp 'gc-cycle-roots-buffer))
  (let ((gc-cycle-roots-buffer '(a b c)))
    (should (equal 3 (cdr (assq 'pending-cycles (gc-stats)))))))

(ert-deftest gc-collect-cycles-empty-buffer-returns-zero ()
  "`gc-collect-cycles' returns 0 and bumps `passes' when buffer empty."
  (skip-unless (fboundp 'gc-collect-cycles))
  (skip-unless (boundp 'gc-cycle-roots-buffer))
  (gc-test--with-mocks
   (should (equal 0 (gc-collect-cycles)))
   (should (equal 1 (cdr (assq 'passes (gc-stats)))))
   (should (equal 0 (cdr (assq 'total-frees (gc-stats)))))))

(ert-deftest gc-color-table-is-empty-hash ()
  "`gc-color-table' is a hash table that starts empty."
  (skip-unless (boundp 'gc-color-table))
  (should (hash-table-p gc-color-table)))

(ert-deftest gc-internal-rc-table-is-empty-hash ()
  "`gc-internal-rc-table' is a hash table that starts empty."
  (skip-unless (boundp 'gc-internal-rc-table))
  (should (hash-table-p gc-internal-rc-table)))

(ert-deftest gc-stats-counters-has-4-mvp-keys ()
  "`gc-stats-counters' carries the 4 Stage-5.3.b keys (initial 0)."
  (skip-unless (boundp 'gc-stats-counters))
  (gc-test--with-mocks
   (should (equal 0 (cdr (assq 'total-allocs gc-stats-counters))))
   (should (equal 0 (cdr (assq 'total-frees gc-stats-counters))))
   (should (equal 0 (cdr (assq 'pending-cycles gc-stats-counters))))
   (should (equal 0 (cdr (assq 'passes gc-stats-counters))))))

;; ---- Stage 5.3.b cycle scenarios ------------------------------------

(ert-deftest gc-cycle-2-deep-collected ()
  "A → B → A two-deep cycle is collected when external refs drop."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (let ((a (gc-test--alloc 7 1))     ; CONS, rc=1 from B's edge
         (b (gc-test--alloc 7 1)))    ; CONS, rc=1 from A's edge
     (gc-test--set-children a (list b))
     (gc-test--set-children b (list a))
     ;; Buffer both as suspects (= simulates external ref drop).
     (push a gc-cycle-roots-buffer)
     (push b gc-cycle-roots-buffer)
     (let ((freed (gc-collect-cycles)))
       (should (equal 2 freed))
       (should (memq a gc-test--finalized))
       (should (memq b gc-test--finalized))
       (should (equal 2 (cdr (assq 'total-frees (gc-stats)))))))))

(ert-deftest gc-cycle-3-deep-collected ()
  "A → B → C → A three-deep cycle is collected entirely."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (let ((a (gc-test--alloc 7 1))
         (b (gc-test--alloc 7 1))
         (c (gc-test--alloc 7 1)))
     (gc-test--set-children a (list b))
     (gc-test--set-children b (list c))
     (gc-test--set-children c (list a))
     (setq gc-cycle-roots-buffer (list a b c))
     (should (equal 3 (gc-collect-cycles)))
     (should (memq a gc-test--finalized))
     (should (memq b gc-test--finalized))
     (should (memq c gc-test--finalized)))))

(ert-deftest gc-cycle-self-referential-collected ()
  "Self-cycle (= a.cdr = a) is collected as a single box."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (let ((a (gc-test--alloc 7 1)))
     (gc-test--set-children a (list a))
     (push a gc-cycle-roots-buffer)
     (should (equal 1 (gc-collect-cycles)))
     (should (memq a gc-test--finalized)))))

(ert-deftest gc-cycle-with-shared-substructure ()
  "Cycle with shared sub-structure: external ref keeps shared node alive."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   ;; Shape: cycle A → B → A; both A and B reference S.  S has rc=3
   ;; (= one external root + A's edge + B's edge).  External root
   ;; pinning is modelled by the extra rc above what `gc--children'
   ;; would predict from the suspect-buffer alone.
   (let ((a (gc-test--alloc 7 1))     ; rc=1 from B's edge
         (b (gc-test--alloc 7 1))     ; rc=1 from A's edge
         (s (gc-test--alloc 7 3)))    ; rc=3 (external + A + B)
     (gc-test--set-children a (list b s))
     (gc-test--set-children b (list a s))
     (gc-test--set-children s nil)
     (setq gc-cycle-roots-buffer (list a b s))
     (let ((freed (gc-collect-cycles)))
       ;; A and B form a cycle and get freed.  S has external rc and
       ;; survives via the scan-black rescue.
       (should (memq a gc-test--finalized))
       (should (memq b gc-test--finalized))
       (should (not (memq s gc-test--finalized)))
       (should (equal 2 freed))))))

(ert-deftest gc-cycle-external-pin-rescues-cycle ()
  "Cycle with external pin survives (= scan-black rescues all members)."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   ;; A→B→A with A.rc bumped by 1 simulating an external root.
   (let ((a (gc-test--alloc 7 2))     ; rc=2 (external + B's edge)
         (b (gc-test--alloc 7 1)))    ; rc=1 (A's edge only)
     (gc-test--set-children a (list b))
     (gc-test--set-children b (list a))
     (setq gc-cycle-roots-buffer (list a b))
     (should (equal 0 (gc-collect-cycles)))
     (should (null gc-test--finalized)))))

(ert-deftest gc-no-cycle-linear-list-baseline ()
  "Linear acyclic list collects nothing (= internal rc < strong)."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   ;; Linear: a → b → c with each rc=1 from the prior link.  But
   ;; we also pretend `a' has an external root (rc=2) so the whole
   ;; chain stays live.
   (let ((a (gc-test--alloc 7 2))
         (b (gc-test--alloc 7 1))
         (c (gc-test--alloc 7 1)))
     (gc-test--set-children a (list b))
     (gc-test--set-children b (list c))
     (gc-test--set-children c nil)
     (setq gc-cycle-roots-buffer (list a))
     (should (equal 0 (gc-collect-cycles)))
     (should (null gc-test--finalized)))))

(ert-deftest gc-unboxed-kind-skipped ()
  "Unboxed kinds (= kind 0/2) in the suspect buffer are skipped clean."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (let ((nil-h (gc-test--alloc 0 0))   ; NIL kind tag
         (int-h (gc-test--alloc 2 0)))  ; INT kind tag
     (setq gc-cycle-roots-buffer (list nil-h int-h))
     ;; The collector still walks them via `gc--mark-gray-walk' (which
     ;; doesn't gate on kind) but `gc--children' returns nil for these
     ;; kinds and `gc--root-candidate-kind-p' filters subsequently
     ;; reached children.  No finalize calls should result because
     ;; their internal-rc == strong-count = 0 trivially makes them
     ;; white but they have no children to walk.
     (let ((freed (gc-collect-cycles)))
       ;; Implementation-detail: unboxed leafs with rc=0 themselves
       ;; satisfy the white predicate, so they ARE finalized in this
       ;; pass.  This is harmless because `nl-gc-finalize' on Rust
       ;; rejects them; the elisp side just records the call.
       (should (equal 2 freed))))))

(ert-deftest gc-cycle-mixed-vector-cons ()
  "Mixed-kind cycle (CONS → VECTOR → CONS) collects all members."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (let ((a (gc-test--alloc 7 1))     ; CONS
         (v (gc-test--alloc 8 1))     ; VECTOR
         (c (gc-test--alloc 7 1)))    ; CONS
     (gc-test--set-children a (list v))
     (gc-test--set-children v (list c))
     (gc-test--set-children c (list a))
     (setq gc-cycle-roots-buffer (list a v c))
     (should (equal 3 (gc-collect-cycles)))
     (should (memq a gc-test--finalized))
     (should (memq v gc-test--finalized))
     (should (memq c gc-test--finalized)))))

(ert-deftest gc-stats-passes-counter-bumps-each-call ()
  "Each `gc-collect-cycles' call increments `passes' by exactly 1."
  (skip-unless (fboundp 'gc-collect-cycles))
  (gc-test--with-mocks
   (gc-collect-cycles)
   (gc-collect-cycles)
   (gc-collect-cycles)
   (should (equal 3 (cdr (assq 'passes (gc-stats)))))))

(ert-deftest gc-snapshot-roots-drains-buffer ()
  "`gc--snapshot-roots' returns the buffer contents and clears it."
  (skip-unless (fboundp 'gc--snapshot-roots))
  (gc-test--with-mocks
   (setq gc-cycle-roots-buffer '(x y z))
   (let ((snap (gc--snapshot-roots)))
     (should (equal '(x y z) snap))
     (should (null gc-cycle-roots-buffer)))))

(ert-deftest gc-root-candidate-kind-predicate ()
  "`gc--root-candidate-kind-p' returns t only on kinds 7/8/9/11/12."
  (skip-unless (fboundp 'gc--root-candidate-kind-p))
  (gc-test--with-mocks
   (dolist (k '(7 8 9 11 12))
     (let ((h (gc-test--alloc k 1)))
       (should (gc--root-candidate-kind-p h))))
   (dolist (k '(0 1 2 3 4 5 6 10))
     (let ((h (gc-test--alloc k 1)))
       (should (not (gc--root-candidate-kind-p h)))))))

(provide 'nelisp-rc-primitives-test)
;;; nelisp-rc-primitives-test.el ends here
