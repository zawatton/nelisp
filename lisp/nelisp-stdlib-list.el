;;; nelisp-stdlib-list.el --- Sweep 9 G1 list operations  -*- lexical-binding: t; -*-

(defun car-safe (object)
  "Return the car of OBJECT if it is a cons cell, otherwise nil."
  (if (consp object) (car object) nil))

(defun cdr-safe (object)
  "Return the cdr of OBJECT if it is a cons cell, otherwise nil."
  (if (consp object) (cdr object) nil))

;; Kept in step with scripts/nelisp-stdlib-prelude.el, the copy the
;; standalone runs; `make ns-gate' reports any drift.
(defun nthcdr (n list)
  (unless (integerp n) (signal 'wrong-type-argument (list 'integerp n)))
  (if (<= n 0) list (if (null list) nil (nthcdr (1- n) (cdr list)))))

;;; nelisp-stdlib-list.el --- Sweep 9 G1 list operations  -*- lexical-binding: t; -*-

(defun nth (n list)
  (car (nthcdr n list)))

;; Kept byte-for-byte in step with the copy in
;; scripts/nelisp-stdlib-prelude.el, which is the one baked into the
;; standalone.  `make ns-gate' reports these as an ns-collision-divergent
;; the moment they differ, and it did: fixing only the prelude produced
;; exactly the drift that made this file's `split-string' and `sort' stale
;; enough to send a review chasing code nothing runs.
(defun reverse (seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((acc nil) (tail seq))
      (while tail
        (setq acc (cons (car tail) acc))
        (setq tail (cdr tail)))
      acc))
   ((stringp seq)
    (let ((n (length seq)) (out "") (i 0))
      (while (< i n)
        (setq out (concat (substring seq i (1+ i)) out))
        (setq i (1+ i)))
      out))
   ((vectorp seq)
    (let* ((n (length seq)) (out (make-vector n nil)) (i 0))
      (while (< i n)
        (aset out (- (- n 1) i) (aref seq i))
        (setq i (1+ i)))
      out))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

(defun nreverse (seq)
  (cond
   ((null seq) nil)
   ((consp seq)
    (let ((prev nil) (cur seq) next)
      (while cur
        (setq next (cdr cur))
        (setcdr cur prev)
        (setq prev cur)
        (setq cur next))
      prev))
   ((vectorp seq)
    (let* ((n (length seq)) (i 0) (j (- n 1)) tmp)
      (while (< i j)
        (setq tmp (aref seq i))
        (aset seq i (aref seq j))
        (aset seq j tmp)
        (setq i (1+ i))
        (setq j (- j 1)))
      seq))
   ((stringp seq) (reverse seq))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

;; Rust-min batch 6o (2026-05-06): `append' migrated from Rust to
;; elisp.  The previous `bi_append' (~61 LOC) implemented the
;; multi-arg sequence concatenation contract:
;;
;;   * 0 args                → nil
;;   * 1 arg                 → return the arg unchanged (no copy)
;;   * N args (N >= 2)       → fresh proper-list spine made of every
;;                             element from non-final args
;;                             (left-to-right, listwise) ending in
;;                             the FINAL arg (used as the tail
;;                             unchanged — can be any value, even
;;                             a non-list improper tail)
;;
;; Non-final args may be: nil (skipped), cons (walked spine), vector
;; (iter), or string (iter as int-codepoints).  An improper-list
;; non-final arg (= dotted tail) signals `wrong-type-argument' once
;; the cons chain exits onto a non-cons non-nil cell.  Non-sequence
;; non-final arg (e.g., an integer) signals immediately.
;;
;; All ingredients (`consp' / `vectorp' / `stringp' / `aref' /
;; `length' / `cons' / `signal') are primitives, so the elisp
;; version is straight transcription of the Rust loop.

(defun nelisp--append-collect (acc seq)
  "Walk SEQ and `cons' each element onto ACC (= reverse-order
accumulator).  SEQ may be nil / cons / vector / string.  Returns
the new ACC.  Signals `wrong-type-argument' for improper-list cons
or non-sequence atom."
  (cond
   ((null seq) acc)
   ((consp seq)
    (let ((cur seq) (nelisp--diag-steps 0))
      (while (consp cur)
        (setq acc (cons (car cur) acc))
        (setq cur (cdr cur))
        ;; DIAGNOSTIC (gc-retention-edge campaign, Phase B, 2026-07-06):
        ;; see the identical instrumentation + rationale on the sibling
        ;; copy of this function in scripts/nelisp-stdlib-prelude.el.
        ;; This copy (loaded later, as part of the full library
        ;; bootstrap bundle) shadows the prelude one via `defun'
        ;; redefinition, so it -- not the prelude copy -- is the one
        ;; actually active when the Magit-bridge repro runs; both must
        ;; carry the guard for the diagnostic to fire in every boot
        ;; path (bare `--load' vs. full-library `dump-runtime-image').
        (setq nelisp--diag-steps (1+ nelisp--diag-steps))
        (when (> nelisp--diag-steps 200000)
          (signal 'nelisp-diag-runaway-append-collect
                  (list seq cur acc nelisp--diag-steps))))
      (when cur
        (signal 'wrong-type-argument (list 'listp seq)))
      acc))
   ((vectorp seq)
    (let ((i 0)
          (n (length seq)))
      (while (< i n)
        (setq acc (cons (aref seq i) acc))
        (setq i (1+ i)))
      acc))
   ((stringp seq)
    (let ((i 0)
          (n (length seq)))
      (while (< i n)
        (setq acc (cons (aref seq i) acc))
        (setq i (1+ i)))
      acc))
   (t (signal 'wrong-type-argument (list 'sequencep seq)))))

(defun append (&rest args)
  "Concatenate sequences ARGS into a fresh proper-list spine.
Non-final args may be list / vector / string / nil.  The FINAL arg
is used as the tail (= unchanged, can be any value).  Single-arg
call returns the arg unchanged (= no copy)."
  (cond
   ((null args) nil)
   ((null (cdr args)) (car args))
   (t
    (let ((cur args)
          (acc nil)
          (tail nil))
      (while (cdr cur)
        (setq acc (nelisp--append-collect acc (car cur)))
        (setq cur (cdr cur)))
      (setq tail (car cur))
      (let ((result tail))
        (while acc
          (setq result (cons (car acc) result))
          (setq acc (cdr acc)))
        result)))))

;; Doc 61 stage 7 (2026-05-07) — `cXXr' accessors migrated from Rust
;; to elisp.  The previous dispatch (= 13 arms in build-tool/src/eval/
;; builtins.rs around line 420) was a pure composition of `car' / `cdr',
;; so promoting them to `defun' on the elisp side shrinks the Rust
;; dispatcher (= 13 arms + 13 names removed) without semantic change.
;; `car' / `cdr' themselves stay in Rust because they are leaf
;; primitives and the most heavily used.

;; nelisp-stdlib-list.el ends here
