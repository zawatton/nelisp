;;; nelisp-lexframe.el --- Elisp-side lexical frame stack (Doc 104 Phase 3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 104 Phase 3 — elisp-side replacement for the
;; `Env::frames: Vec<HashMap<String, FrameCell>>' lexical scope
;; stack in `build-tool/src/eval/env.rs'.
;;
;; Stage 3.a (this file): ship the elisp surface (= lexframe record
;; + lexframe-stack record + 11 helpers).  No Rust changes; tests
;; exercise this file standalone via the `nelisp-stdlib-fast-hash'
;; substrate.  Stage 3.b adds the Rust-direct `mirror_*' helpers
;; that walk these records from Rust without `apply_function'
;; round-trips.  Stage 3.c-e then retires `Env::frames'.
;;
;; The naming `lexframe' disambiguates this module (= lexical scope
;; frame, eval-time concept) from `nelisp-emacs/src/emacs-frame.el'
;; (= Emacs display frame, top-level GUI / TUI surface).  Same word,
;; orthogonal concerns; the `lex-' prefix names the eval-time
;; concept everywhere it appears.
;;
;; Storage layouts:
;;
;;   lexframe record (`Sexp::Record' type-tag `nelisp-lexframe'):
;;     slot 0 = HT : fast-hash-table mapping names/identity keys → cell
;;     slot 1 = DYNAMIC : keys classified as dynamic at binding time
;;       (older one-slot frames are lexical-only).
;;     slot 2 = SCOPE : non-nil at a callee's outermost lexical frame.
;;     slot 3 = DECLARATIONS : locally special names (not value bindings).
;;
;;     Each fast-hash entry is `(NAME . CELL)' where CELL is opaque
;;     to this module (in the live runtime it is a `Sexp::Cell',
;;     i.e. an `NlCellRef'; tests may use plain values to verify the
;;     identity preservation contract).
;;
;;   lexframe-stack record (`Sexp::Record' type-tag
;;   `nelisp-lexframe-stack'):
;;     slot 0 = BACKING : vector of frames (reserved capacity)
;;     slot 1 = DEPTH   : current logical depth (= number of live
;;                        frames; BACKING may be larger).
;;
;;     Push / pop mutate slot 1 directly; BACKING grows via
;;     `vconcat' when DEPTH would exceed its length (= amortised
;;     O(1) push at the cost of an occasional capacity-doubling
;;     copy, matching `Vec<T>::push' semantics).  The stack record
;;     identity is preserved across push/pop so consumers holding a
;;     reference see the mutation.
;;
;; The `nelisp--unbound-marker' sentinel is shared with the
;; `nelisp-stdlib-fast-hash' module; `nelisp-lexframe-lookup' and
;; `nelisp-lexframe-stack-find' return it when a NAME is absent so
;; callers can distinguish "absent" from "bound to nil" (= Doc 104
;; §2.1).

;;; Code:

(require 'nelisp-stdlib-fast-hash)

;; ---- Single lexframe ----

(defconst nelisp-lexframe--default-bucket-count 16
  "Default per-frame bucket count.
Frames typically hold 1-5 entries (let bindings + lambda formals),
so 16 buckets is plenty (= 1-2 entries per bucket avg, near-zero
collision cost) while keeping the per-frame backing vector small
(= 16 × Sexp::Nil ≈ 128 bytes, so 100+ live frames during deep
recursion fit in ~13KB).  Doc 104 §2.2 rationale.")

(defun nelisp-lexframe-make (&optional bucket-count)
  "Build a fresh empty lexframe record.
BUCKET-COUNT defaults to `nelisp-lexframe--default-bucket-count'
(= 16, see Doc 104 §2.2)."
  (let ((ht (nelisp--fast-hash-make
             (or bucket-count nelisp-lexframe--default-bucket-count))))
    (nelisp--make-record 'nelisp-lexframe ht nil nil nil)))

(defun nelisp-lexframe--key (name)
  "Normalize default-obarray symbols to strings, preserving other identities."
  (if (and (symbolp name) (eq (intern-soft name) name))
      (symbol-name name)
    name))

(defun nelisp-lexframe-local-declarations (frame)
  "Return FRAME's local special declarations; legacy frames have none."
  (and (> (length frame) 4)
       (mapcar #'nelisp-lexframe--key
               (nelisp--record-ref frame 3))))

(defun nelisp-lexframe-declare-special! (frame name)
  "Declare NAME locally special in FRAME without binding a value."
  (unless (> (length frame) 4) (error "Legacy frame has no declaration slot"))
  (let ((key (nelisp-lexframe--key name)))
    (unless (member key (nelisp-lexframe-local-declarations frame))
      (nelisp--record-set frame 3
                         (cons key (nelisp-lexframe-local-declarations frame)))))
  name)

(defun nelisp-lexframe-stack-declare-special! (stack name)
  "Declare NAME in the nearest lexical scope in STACK.
Empty and dynamic-only let frames do not introduce a declaration scope."
  (let ((i (1- (nelisp-lexframe-stack-depth stack))) done)
    (while (and (>= i 0) (not done))
      (let ((frame (aref (nelisp-lexframe-stack--backing stack) i)))
        (when (or (= i 0) (nelisp-lexframe-scope-p frame)
                  (> (nelisp-lexframe-count frame)
                     (if (> (length frame) 2)
                         (length (nelisp--record-ref frame 1)) 0)))
          (nelisp-lexframe-declare-special! frame name)
          (setq done t)))
      (setq i (1- i)))
    name))

(defun nelisp-lexframe-stack-local-special-p (stack name)
  "Return non-nil if NAME has a declaration in STACK's lexical scope."
  (let ((key (nelisp-lexframe--key name))
        (i (1- (nelisp-lexframe-stack-depth stack))) found)
    (while (and (>= i 0) (not found))
      (let ((frame (aref (nelisp-lexframe-stack--backing stack) i)))
        (setq found (and (member key (nelisp-lexframe-local-declarations frame)) t))
        (when (nelisp-lexframe-scope-p frame) (setq i 0)))
      (setq i (1- i)))
    found))

(defun nelisp-lexframe-scope-p (frame)
  "Return non-nil when FRAME ends the current function's lexical scope."
  (and (> (length frame) 3) (nelisp--record-ref frame 2)))

(defun nelisp-lexframe-mark-scope! (frame)
  "Mark FRAME as a function's outermost lexical frame."
  (unless (> (length frame) 3)
    (error "Legacy frame has no scope slot"))
  (nelisp--record-set frame 2 1)
  frame)

(defun nelisp-lexframe-p (obj)
  "Return non-nil iff OBJ is a lexframe record."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'nelisp-lexframe)))

(defun nelisp-lexframe--ht (frame)
  "Return FRAME's underlying fast-hash-table.  Internal."
  (nelisp--record-ref frame 0))

(defun nelisp-lexframe-count (frame)
  "Return the number of bindings live in FRAME."
  (nelisp--fast-hash-count (nelisp-lexframe--ht frame)))

(defun nelisp-lexframe-dynamic-p (frame name)
  "Return non-nil when FRAME's binding of NAME was created as dynamic.
This reads stored metadata, never a symbol's current declaration status."
  (and (> (length frame) 2)
       (member (nelisp-lexframe--key name) (nelisp--record-ref frame 1)) t))

(defun nelisp-lexframe-bind (frame name cell &optional dynamic)
  "Bind NAME → CELL in FRAME (= insert or update).
CELL is opaque — the runtime passes a `Sexp::Cell' so closures
write through; tests may pass any value to exercise identity
preservation.  DYNAMIC classifies this new binding, not subsequent cell
mutations.  Returns CELL.  Old one-slot frames accept lexical bindings only."
  (when (and dynamic (<= (length frame) 2))
    (error "Legacy frame cannot hold dynamic bindings"))
  (setq name (nelisp-lexframe--key name))
  (nelisp--fast-hash-put (nelisp-lexframe--ht frame) name cell (symbolp name))
  (when (> (length frame) 2)
    (let ((names (delete name (nelisp--record-ref frame 1))))
      (nelisp--record-set frame 1 (if dynamic (cons name names) names))))
  cell)

(defun nelisp-lexframe-lookup (frame name)
  "Return FRAME's cell for NAME, or `nelisp--unbound-marker' if
NAME is absent.
Doc 104 §2.1 contract — the sentinel is the same one used by
`nelisp-env-lookup-value', so callers can chain
`(eq result nelisp--unbound-marker)' across the frame stack +
globals."
  (setq name (nelisp-lexframe--key name))
  (nelisp--fast-hash-get
   (nelisp-lexframe--ht frame) name nelisp--unbound-marker (symbolp name)))

;; ---- Lexframe stack ----

(defconst nelisp-lexframe-stack--initial-capacity 8
  "Initial BACKING vector size for a fresh lexframe-stack.
Frames are usually shallow (= STDLIB load top-level ≤ 4 frames);
deeper recursion grows BACKING via capacity doubling.  Doc 104
§5.4 rationale.")

(defun nelisp-lexframe-stack-make (&optional initial-capacity)
  "Build a fresh empty lexframe-stack record.
INITIAL-CAPACITY hints the initial BACKING size (= default 8);
push grows the backing on overflow."
  (let* ((cap (or initial-capacity
                  nelisp-lexframe-stack--initial-capacity))
         (backing (make-vector cap nil)))
    (nelisp--make-record 'nelisp-lexframe-stack backing 0)))

(defun nelisp-lexframe-stack-p (obj)
  "Return non-nil iff OBJ is a lexframe-stack record."
  (and (recordp obj)
       (eq (nelisp--record-type obj) 'nelisp-lexframe-stack)))

(defun nelisp-lexframe-stack-depth (stack)
  "Return the number of live frames in STACK."
  (nelisp--record-ref stack 1))

(defun nelisp-lexframe-stack--backing (stack)
  "Return STACK's backing vector.  Internal."
  (nelisp--record-ref stack 0))

(defun nelisp-lexframe-stack--ensure-capacity (stack needed)
  "Grow STACK's BACKING to hold at least NEEDED entries.
No-op when current capacity suffices.  Doubles capacity on
overflow (= amortised O(1) push) and copies the live entries
across to the new vector.  Internal."
  (let* ((backing (nelisp-lexframe-stack--backing stack))
         (cap (length backing)))
    (when (< cap needed)
      (let* ((new-cap (let ((n (max 1 cap)))
                        (while (< n needed)
                          (setq n (* n 2)))
                        n))
             (new-backing (make-vector new-cap nil))
             (depth (nelisp-lexframe-stack-depth stack))
             (i 0))
        ;; Copy live entries; trailing slots stay nil (= the
        ;; reserved-capacity zone).
        (while (< i depth)
          (aset new-backing i (aref backing i))
          (setq i (1+ i)))
        (nelisp--record-set stack 0 new-backing)))))

(defun nelisp-lexframe-stack-push! (stack frame)
  "Push FRAME onto STACK (= mutating).  Returns FRAME.
Grows BACKING when full.  STACK record identity is preserved."
  (let ((depth (nelisp-lexframe-stack-depth stack)))
    (nelisp-lexframe-stack--ensure-capacity stack (1+ depth))
    (aset (nelisp-lexframe-stack--backing stack) depth frame)
    (nelisp--record-set stack 1 (1+ depth))
    frame))

(defun nelisp-lexframe-stack-pop! (stack)
  "Remove the top frame from STACK (= mutating).  Returns the
dropped frame, or nil when STACK is empty (= no-op).  STACK
record identity is preserved."
  (let ((depth (nelisp-lexframe-stack-depth stack)))
    (if (= depth 0)
        nil
      (let* ((new-depth (1- depth))
             (backing (nelisp-lexframe-stack--backing stack))
             (frame (aref backing new-depth)))
        ;; Clear the slot so the popped frame can be GC'd.
        (aset backing new-depth nil)
        (nelisp--record-set stack 1 new-depth)
        frame))))

(defun nelisp-lexframe-stack-find (stack name)
  "Walk STACK innermost-first; return the cell for NAME (= first
hit) or `nelisp--unbound-marker' if NAME is absent everywhere.
Doc 104 §2.1 contract — matches `find_frame_cell' inner-most-first
semantics."
  (let* ((depth (nelisp-lexframe-stack-depth stack))
         (backing (nelisp-lexframe-stack--backing stack))
         (i (1- depth))
         (result nelisp--unbound-marker))
    (while (and (>= i 0) (eq result nelisp--unbound-marker))
      (let ((found (nelisp-lexframe-lookup (aref backing i) name)))
        (unless (eq found nelisp--unbound-marker)
          (setq result found)))
      (setq i (1- i)))
    result))

(defun nelisp-lexframe-stack--find-kind (stack name dynamic)
  "Find NAME's innermost cell in STACK with the stored DYNAMIC kind."
  (let ((i (1- (nelisp-lexframe-stack-depth stack)))
        (backing (nelisp-lexframe-stack--backing stack))
        (result nelisp--unbound-marker))
    (while (and (>= i 0) (eq result nelisp--unbound-marker))
      (let ((frame (aref backing i)))
        (when (eq (nelisp-lexframe-dynamic-p frame name) dynamic)
          (setq result (nelisp-lexframe-lookup frame name)))
        (when (and (not dynamic) (nelisp-lexframe-scope-p frame))
          (setq i 0)))
      (setq i (1- i)))
    result))

(defun nelisp-lexframe-stack-find-lexical (stack name)
  "Return NAME's innermost lexical cell in STACK, or the unbound marker."
  (nelisp-lexframe-stack--find-kind stack name nil))

(defun nelisp-lexframe-stack-find-dynamic (stack name)
  "Return NAME's innermost dynamic cell in STACK, or the unbound marker."
  (nelisp-lexframe-stack--find-kind stack name t))

(defun nelisp-lexframe-stack-capture (stack &optional max-depth)
  "Walk STACK innermost-first; return an alist of (NAME . CELL).
Inner shadows outer (= only the first-seen binding per NAME
appears).  The cell identity is preserved exactly (= no clone),
so closure write-through holds.  Doc 104 §2.3 contract — matches
`capture_lexical'.  Dynamic cells are excluded without hiding outer lexical
cells.  Bare names preserve local special declarations in the result.
Optional MAX-DEPTH excludes frames at and above that depth."
  (let* ((depth (min (nelisp-lexframe-stack-depth stack)
                     (or max-depth (nelisp-lexframe-stack-depth stack))))
         (backing (nelisp-lexframe-stack--backing stack))
         ;; Use a small hash-table for the seen-set; equal-test for
         ;; string keys.  Allocates one cons per intra-frame entry +
         ;; one ht slot per unique name — bounded by total binding
         ;; count across the live stack.
         (seen (make-hash-table :test 'equal))
         (acc nil)
         (i (1- depth)))
    (while (>= i 0)
      (let* ((frame (aref backing i)) (ht (nelisp-lexframe--ht frame)))
        (nelisp--fast-hash-iter
         ht
         (lambda (name cell)
           (unless (or (nelisp-lexframe-dynamic-p frame name)
                       (gethash name seen))
             (puthash name t seen)
             (setq acc (cons (cons name cell) acc)))))
        (dolist (name (nelisp-lexframe-local-declarations frame))
          (setq acc (cons name acc)))
        (when (nelisp-lexframe-scope-p frame) (setq i 0)))
      (setq i (1- i)))
    acc))

(defun nelisp-lexframe-make-from-alist (alist)
  "Build a fresh frame populated from ALIST = ((NAME . CELL) ...)
without pushing onto any stack.  Returns the frame.

Doc 102 Phase 4.b helper — paired with a Rust-side push so the
apply_lambda_inner argument frame doesn't contaminate the
lexframe stack during dispatch (see Doc 102 §3 Phase 4 for the
layering rationale).

NAME may be a symbol or a string. Default-obarray symbols normalize to
strings; other symbols remain identity keys. CELL is stored verbatim — callers
pre-wrap bare values in `Sexp::Cell' for write-through.  Bare symbol or string
entries restore local declarations rather than value cells."
  (let ((frame (nelisp-lexframe-make)))
    (while alist
      (if (or (symbolp (car alist)) (stringp (car alist)))
          (nelisp-lexframe-declare-special! frame (car alist))
       (let* ((pair (car alist))
             (name (car pair))
             (cell (cdr pair)))
        (nelisp-lexframe-bind frame name cell)))
      (setq alist (cdr alist)))
    frame))

(defun nelisp-lexframe-stack-capture-to-depth (stack max-depth)
  "Walk STACK innermost-first up to MAX-DEPTH; return an alist of
\(NAME . CELL).  Frames at index >= MAX-DEPTH are skipped.  Inner
shadows outer (= the consumer-side `fast-hash-put' UPDATE-existing
semantics implement this: the alist may contain duplicate NAME
entries with OUTER ones at the head end + INNER ones nearer the
tail; `nelisp-lexframe-stack-push-captured!' iterates head-first
and each later bind UPDATES the earlier hash entry → INNER wins).
The cell identity is preserved exactly (= no clone), so closure
write-through holds.

Doc 102 Phase 4.b — MAX-DEPTH is the caller's pre-apply depth
snapshot.  apply_lambda_inner pushes its argument frame at depth
MAX-DEPTH during the dispatch, so capping the walk at MAX-DEPTH-1
skips that contamination.

Doc 49 Wave 10.1d-retry — AOT native fast path: dispatches
to `nl_capture_descend_native' (co-located in
`nelisp-cc-frame-stack-find.o') via `nl-jit-call-out-1', eliding
the elisp interpretation cost of the original R11b
`while'-loop + `gethash'/`puthash' dedup body.  The 3-slot scratch
vector is freshly allocated per call (= one `Sexp::Vector(3)' +
two `Sexp::Nil' init writes, far cheaper than the elided body's
`make-hash-table' + per-entry `puthash' allocs).  Walks
innermost-first + prepends — see the dedup-via-consumer note above
for the correctness argument."
  ;; scratch[0] = stack record (passed through)
  ;; scratch[1] = max-depth Sexp::Int (passed through)
  ;; scratch[2] = pair-slot scratch (Sexp::Nil — AOT helper
  ;;              reuses this as the destination for each inner
  ;;              (NAME . CELL) cons cell; refcount-safe per the
  ;;              `cons-make-with-clone' alias-safety contract).
  ;; scratch[3] = filter symbol list (Nil here: unfiltered public wrapper)
  ;; scratch[4] = filtered flag (0 here: preserve original capture-all API)
  ;; Retain source fallback for kind/scope metadata when this library is
  ;; reloaded into a reader built before the corresponding native support.
  (let ((i 0) (dynamic nil)
        (limit (min max-depth (nelisp-lexframe-stack-depth stack))))
    (while (and (< i limit) (not dynamic))
      (let ((frame (aref (nelisp-lexframe-stack--backing stack) i)))
        (setq dynamic (or (and (> (length frame) 2)
                              (nelisp--record-ref frame 1))
                         (nelisp-lexframe-scope-p frame)
                         (nelisp-lexframe-local-declarations frame))))
      ;; Existing native capture helpers compare string-shaped keys by name.
      ;; Keep identity-bearing keys on the source path until that ABI is ready.
      (unless dynamic
        (nelisp--fast-hash-iter
         (nelisp-lexframe--ht (aref (nelisp-lexframe-stack--backing stack) i))
         (lambda (key _cell) (when (symbolp key) (setq dynamic t)))))
      (setq i (1+ i)))
    (if dynamic
        (nelisp-lexframe-stack-capture stack max-depth)
      (nl-jit-call-out-1 "nl_capture_descend_native"
                         (vector stack max-depth nil nil 0)))))

(defun nelisp-lexframe-stack-push-captured! (stack alist)
  "Build a fresh frame populated from ALIST = ((NAME . CELL) ...),
push it onto STACK (= mutating).  Returns the new frame.  CELL
identity is preserved exactly so the pushed frame shares cells
with the originating let-binding (= closure write-through).  Doc
104 §2.3 contract — matches `push_captured'.

NAME may be a symbol or a string. Use the same identity-preserving conversion
as `nelisp-lexframe-make-from-alist', including bare local declarations."
  (let ((frame (nelisp-lexframe-make-from-alist alist)))
    (nelisp-lexframe-stack-push! stack frame)
    frame))

(provide 'nelisp-lexframe)

;;; nelisp-lexframe.el ends here
