;;; nelisp-t77-undo-tree-defstruct-setf-parity-test.el --- T77 regression guard  -*- lexical-binding: t; -*-

;; T77 (2026-09-06): investigation of a T63-reported standalone hang while
;; DEFINING `undo-tree-node-copy' (undo-tree.el top-level form 62 of 211),
;; a plain `defun' whose body is `cl-lib'-flavored: `while'/`pop'/`setf' on
;; `cl-defstruct' accessor places.
;;
;; Finding: the construct itself does not exponentially blow up under
;; `macroexpand-all', on either host Emacs or the NeLisp standalone, and
;; `undo-tree-node-copy' (and the much larger, multi-place-`setf'
;; `undo-tree-pull-redo-in-region-branch', form 124) both macroexpand and
;; evaluate in well under a millisecond in isolation.  The actual T63
;; hang is a general, PROCESS-WIDE per-form cost GROWTH (same class as the
;; separately tracked "per-form cost growth in large files" investigation)
;; traced to the standalone's `intern' primitive: a clean, isolated probe
;; that interns 2000 brand-new symbols with no cl-lib/setf/struct
;; involvement at all costs multiple SECONDS (vs. host Emacs's
;; microseconds), and this cost recurs for every fresh symbol any
;; construct in a large file happens to intern (cl-defstruct's own
;; accessor/constructor/predicate name synthesis included).  See
;; T77-REPORT.md for the full measurement trail.  `intern' is a
;; vendor/nelisp core primitive (no elisp shim anywhere shadows it), so no
;; fix belongs in this consumer's `src/'.
;;
;; This file is a REGRESSION GUARD, not a performance test: it pins down
;; that the setf/cl-defstruct/macroexpand-all SHIMS in `src/cl-lib.el',
;; `src/emacs-cl-macros.el', and `src/emacs-parity-macroexpand.el' keep
;; producing semantically correct expansions and behavior for this exact
;; construct shape (struct with `(:type vector)', 5 constructors, a
;; `while'/`pop'/`push' loop, a single-place `setf' chain, and a
;; multi-place `setf' in one call) -- so a future change to those shims
;; cannot silently reintroduce a real (not just slow) defect here.
;; Runs under host Emacs via `ert'; the standalone-specific code paths in
;; those shim files are inert on host by design (guarded on
;; `nelisp--write-stdout-bytes'/`emacs-version'), so this only exercises
;; the construct's ordinary Elisp semantics -- see T77-REPORT.md for the
;; standalone-side timing numbers this guard cannot itself capture.

(require 'ert)
(require 'cl-lib)

(cl-defstruct
  (nelisp-t77-node
   (:type vector)
   (:constructor nil)
   (:constructor nelisp-t77-make-node
                 (previous undo
                  &optional redo
                  &aux
                  (timestamp 0)
                  (branch 0)))
   (:constructor nelisp-t77-make-empty-node ())
   (:copier nil))
  previous next undo redo timestamp branch meta-data)

(cl-defstruct
  (nelisp-t77-tree
   :named
   (:constructor nil)
   (:constructor nelisp-t77-make-tree
                 (&aux
                  (root (nelisp-t77-make-node nil nil))
                  (current root)))
   (:copier nil))
  root current)

;; Mirrors undo-tree.el form 62 (`undo-tree-node-copy') verbatim in shape:
;; `let*' + `while'/`pop' + five sequential `setf' calls on `(:type
;; vector)' struct accessors + a nested nil-check `push'/`pop' loop.
(defun nelisp-t77-node-copy (node &optional tree current)
  (let* ((new (nelisp-t77-make-empty-node))
         (stack (list (cons node new)))
         n)
    (while (setq n (pop stack))
      (setf (nelisp-t77-node-undo (cdr n))
            (copy-tree (nelisp-t77-node-undo (car n)) 'copy-vectors))
      (setf (nelisp-t77-node-redo (cdr n))
            (copy-tree (nelisp-t77-node-redo (car n)) 'copy-vectors))
      (setf (nelisp-t77-node-timestamp (cdr n))
            (nelisp-t77-node-timestamp (car n)))
      (setf (nelisp-t77-node-branch (cdr n))
            (nelisp-t77-node-branch (car n)))
      (setf (nelisp-t77-node-next (cdr n))
            (mapcar (lambda (_) (nelisp-t77-make-empty-node))
                    (make-list (length (nelisp-t77-node-next (car n))) nil)))
      (when (and tree (eq (car n) current))
        (setf (nelisp-t77-tree-current tree) (cdr n)))
      (let ((next0 (nelisp-t77-node-next (car n)))
            (next1 (nelisp-t77-node-next (cdr n))))
        (while (and next0 next1)
          (push (cons (pop next0) (pop next1)) stack))))
    new))

;; Mirrors undo-tree.el form 124's distinguishing feature: ONE `setf' call
;; with TWO place/value pairs, one of which nests a `delq' read of the
;; place being written by the OTHER pair in the same call.
(defun nelisp-t77-detach-fragment (node)
  (let ((fragment (car (nelisp-t77-node-next node))))
    (when fragment
      (setf (nelisp-t77-node-previous fragment) nil
            (nelisp-t77-node-next node)
            (delq fragment (nelisp-t77-node-next node))))
    fragment))

(defvar nelisp-t77-form62-source
  '(defun nelisp-t77-node-copy--shadow (node &optional tree current)
     (let* ((new (nelisp-t77-make-empty-node))
            (stack (list (cons node new)))
            n)
       (while (setq n (pop stack))
         (setf (nelisp-t77-node-undo (cdr n))
               (copy-tree (nelisp-t77-node-undo (car n)) 'copy-vectors))
         (setf (nelisp-t77-node-redo (cdr n))
               (copy-tree (nelisp-t77-node-redo (car n)) 'copy-vectors))
         (setf (nelisp-t77-node-timestamp (cdr n))
               (nelisp-t77-node-timestamp (car n)))
         (setf (nelisp-t77-node-branch (cdr n))
               (nelisp-t77-node-branch (car n)))
         (setf (nelisp-t77-node-next (cdr n))
               (mapcar (lambda (_) (nelisp-t77-make-empty-node))
                       (make-list (length (nelisp-t77-node-next (car n))) nil)))
         (when (and tree (eq (car n) current))
           (setf (nelisp-t77-tree-current tree) (cdr n)))
         (let ((next0 (nelisp-t77-node-next (car n)))
               (next1 (nelisp-t77-node-next (cdr n))))
           (while (and next0 next1)
             (push (cons (pop next0) (pop next1)) stack))))
       new))
  "Verbatim copy of `nelisp-t77-node-copy''s source, kept as a quoted
sexp (rather than reflected back out of `symbol-function') so this test
does not depend on how the current Emacs happens to print/represent an
interpreted closure.")

(ert-deftest nelisp-t77-form62-construct-macroexpands-cleanly ()
  "`undo-tree-node-copy''s construct macroexpands to a bounded-size form.
Guards against a re-expansion/exponential-blowup regression in the
setf/cl-defstruct/macroexpand-all shims: the T63 hypothesis (a shim
re-expanding an already-expanded place per nesting level) would show up
here as a form whose size is wildly disproportionate to the ~15-line
source, or as this test simply failing to terminate."
  (let* ((expanded (macroexpand-all nelisp-t77-form62-source))
         (size (length (prin1-to-string expanded))))
    (should (> size 0))
    ;; The source form itself (unexpanded) prin1's to a few hundred
    ;; characters; a few thousand chars post-expansion is the honest
    ;; "some macros grew it" baseline, not this bug's blowup shape
    ;; (which the T63 report described as taking 900s to even finish
    ;; defining -- i.e. never reaching a `prin1-to-string' call at all).
    (should (< size 20000))))

(ert-deftest nelisp-t77-form62-construct-behaves-correctly ()
  "`undo-tree-node-copy''s construct produces a correct deep copy.
A 3-node chain (root -> child -> grandchild) copied via the construct
under test must be `equal' in shape/values to the original, and must be
a genuinely separate object (mutating the copy must not mutate the
original) -- the semantic contract `undo-tree-node-copy' relies on."
  (let* ((grandchild (nelisp-t77-make-node 'gp-marker '(gc-undo) nil))
         (child (nelisp-t77-make-node 'root-marker '(c-undo) '(c-redo)))
         (root (nelisp-t77-make-node nil '(r-undo) '(r-redo))))
    (setf (nelisp-t77-node-next child) (list grandchild))
    (setf (nelisp-t77-node-next root) (list child))
    (let ((copy (nelisp-t77-node-copy root)))
      (should (equal (nelisp-t77-node-undo copy) (nelisp-t77-node-undo root)))
      (should (equal (nelisp-t77-node-redo copy) (nelisp-t77-node-redo root)))
      (should (= (length (nelisp-t77-node-next copy)) 1))
      (let ((copy-child (car (nelisp-t77-node-next copy))))
        (should (equal (nelisp-t77-node-undo copy-child)
                       (nelisp-t77-node-undo child)))
        (should (= (length (nelisp-t77-node-next copy-child)) 1)))
      ;; Deep copy: mutating the copy's undo list must not alias the
      ;; original's (this is exactly what `copy-tree' in the construct
      ;; is for).
      (setcar (nelisp-t77-node-undo copy) 'mutated)
      (should-not (equal (nelisp-t77-node-undo copy) (nelisp-t77-node-undo root))))))

(ert-deftest nelisp-t77-form124-multi-place-setf-behaves-correctly ()
  "The double-place `setf' construct (form 124's distinguishing shape)
detaches a fragment and clears it from the parent's `next' list in one
call, matching plain sequential `setf' semantics."
  (let* ((fragment (nelisp-t77-make-node 'p 'u))
         (sibling (nelisp-t77-make-node 'p 'u))
         (node (nelisp-t77-make-node nil nil)))
    (setf (nelisp-t77-node-next node) (list fragment sibling))
    (let ((detached (nelisp-t77-detach-fragment node)))
      (should (eq detached fragment))
      (should (null (nelisp-t77-node-previous fragment)))
      (should (equal (nelisp-t77-node-next node) (list sibling))))))

(provide 'nelisp-t77-undo-tree-defstruct-setf-parity-test)
;;; nelisp-t77-undo-tree-defstruct-setf-parity-test.el ends here
