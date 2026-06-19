;;; nelisp-stdlib-equal.el --- cycle-safe `equal'  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 50 stage 5b — pure-elisp `equal' that handles self-referential
;; cons / circular lists / mutually recursive struct graphs without
;; the depth-bound stack walk the previous Rust impl used.
;;
;; Strategy (Doc 50 §3.5):
;;
;;   (equal A B) creates a fresh visited hash-table (eq-keyed) and
;;   delegates to `nelisp--equal-rec', which:
;;
;;     1. short-circuits via `nelisp--ref-eq' when A and B refer to
;;        the same allocation (= identity match — also satisfies
;;        the cycle case where both arms re-enter the same node);
;;     2. records each cons / vector / record node in `visited' on
;;        the way *down* and treats a re-entry as `t' (= "we're
;;        already comparing this node");
;;     3. falls back to value comparison (`nelisp--ref-eq' for atoms
;;        does the right thing because `eq' applies to integers /
;;        symbols / nil / t already).
;;
;; Hash-table is allocated only on the first heap-touched node — a
;; tight integer/symbol pair never allocates.  `make-hash-table' is
;; itself an elisp re-impl on top of records (Doc 50 stage 4f-1), so
;; this file is only loadable AFTER nelisp-stdlib-hash.el.
;;
;; The Rust dispatch arm `bi_equal' is shadowed by the elisp defun
;; below — function-cell override at load time, same pattern as the
;; hash-table family.

;;; Code:

(declare-function nelisp--ref-eq "nelisp-jit-strategy" (a b))
(declare-function nelisp--record-length "nelisp-jit-strategy" (record))
(declare-function nelisp--record-ref "nelisp-jit-strategy" (record index))
(declare-function nelisp--record-type "nelisp-jit-strategy" (record))

(defun nelisp--equal-string (a b)
  "Value-compare two strings without recursing through `string='.

`string-equal' (= the user-facing `string=' alias) calls `equal'
on its normalised inputs, so dispatching back through `string='
from `nelisp--equal-rec' would loop.  Instead we use:

  1. `(eq A B)' — for the immutable `Sexp::Str' variant produced
     by the reader, the Rust `sexp_eq' arm returns value equality
     directly (no allocation identity check), so a literal-equal
     pair short-circuits here.
  2. Length + char-by-char `aref' fallback — handles
     `Sexp::MutStr' (= `copy-sequence' results) where Rust `eq' is
     identity-only.  Characters are fixnums; `eq' on them is
     pointwise value compare."
  (or (eq a b)
      (let ((n (length a)))
        (and (= n (length b))
             (let ((i 0) (ok t))
               (while (and ok (< i n))
                 (unless (eq (aref a i) (aref b i))
                   (setq ok nil))
                 (setq i (1+ i)))
               ok)))))

(defun nelisp--equal-vector-rec (a b visited)
  "Element-wise compare vectors A and B with visited memoisation.
Both A and B are already verified vectors with equal length."
  (let ((i 0)
        (n (length a))
        (ok t))
    (while (and ok (< i n))
      (unless (nelisp--equal-rec (aref a i) (aref b i) visited)
        (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun nelisp--equal-record-rec (a b visited)
  "Element-wise compare records A and B with visited memoisation.
The record `type-of' tags must match; slot count + slot values are
walked recursively."
  (and (eq (nelisp--record-type a) (nelisp--record-type b))
       (let ((n (nelisp--record-length a)))
         (and (= n (nelisp--record-length b))
              (let ((i 0) (ok t))
                (while (and ok (< i n))
                  (unless (nelisp--equal-rec
                           (nelisp--record-ref a i)
                           (nelisp--record-ref b i)
                           visited)
                    (setq ok nil))
                  (setq i (1+ i)))
                ok)))))

(defun nelisp--equal-rec (a b visited)
  "Cycle-safe structural compare worker — VISITED is an eq hash-table.
On entry, A and B are arbitrary Sexp values.  Heap-shared variants
(cons / vector / record) are recorded in VISITED so a circular
re-entry returns t instead of recursing forever.  Atoms (integer /
symbol / nil / t) bypass VISITED — `nelisp--ref-eq' handles them."
  (cond
   ;; Identity short-circuit (also covers atoms).
   ((nelisp--ref-eq a b) t)
   ;; Cons — recurse on car/cdr with visited registration.
   ((and (consp a) (consp b))
    (cond
     ((gethash a visited) t)
     (t
      (puthash a t visited)
      (and (nelisp--equal-rec (car a) (car b) visited)
           (nelisp--equal-rec (cdr a) (cdr b) visited)))))
   ;; String — must NOT route through `string=' / `string-equal'
   ;; because the latter ends in `(equal sa sb)' = back here.  See
   ;; `nelisp--equal-string' for the cycle-free short-circuit.
   ((and (stringp a) (stringp b))
    (nelisp--equal-string a b))
   ;; Vector — length + elementwise.
   ((and (vectorp a) (vectorp b))
    (cond
     ((gethash a visited) t)
     (t
      (puthash a t visited)
      (and (= (length a) (length b))
           (nelisp--equal-vector-rec a b visited)))))
   ;; Record — type-tag + slot-by-slot.  Note that hash-tables (Doc
   ;; 50 stage 4f-1) are records under the hood, so this branch
   ;; covers them by structural slot comparison.
   ((and (recordp a) (recordp b))
    (cond
     ((gethash a visited) t)
     (t
      (puthash a t visited)
      (nelisp--equal-record-rec a b visited))))
   ;; Numbers / symbols / nil / t etc — fall through to value compare.
   (t (eq a b))))

(defun nelisp--equal-heapish-p (x)
  "Return non-nil when X may need cycle-safe visited tracking."
  (or (consp x) (vectorp x) (recordp x)))

(defun equal (a b)
  "Return t if A and B are structurally equal, walking shared
sub-structure exactly once.  Cycle-safe: a self-referential cons
or graph with shared back-edges returns a stable answer instead
of looping.

Doc 50 stage 5b — re-implementation in elisp on top of
`nelisp--ref-eq' (= identity short-circuit / visited tag) and
`make-hash-table' (= visited registry).  Shadows the prior Rust
`bi_equal' dispatch arm via function-cell override at load."
  (cond
   ((nelisp--ref-eq a b) t)
   ((and (stringp a) (stringp b))
    (nelisp--equal-string a b))
   ((or (stringp a) (stringp b)) nil)
   ((and (numberp a) (numberp b)) (= a b))
   ((or (not (nelisp--equal-heapish-p a))
        (not (nelisp--equal-heapish-p b)))
    nil)
   (t
    (let ((visited (make-hash-table :test 'eq)))
      (nelisp--equal-rec a b visited)))))

(provide 'nelisp-stdlib-equal)

;; nelisp-stdlib-equal.el ends here
