;;; nelisp-reclaim-veto-census.el --- build-time reclaim-veto producer census -*- lexical-binding: t; -*-

;; The standalone runtime's form-boundary bump allocator rewinds the arena
;; after every top-level form, via `nl_boundary_maybe_reclaim' (Doc 140
;; Stage 5, `scripts/nelisp-standalone-build.el').  It only rewinds when all
;; four of these hold:
;;
;;   1. the reclaim-enable flag at 268436216 is 1
;;   2. the form's result is an immediate (tag <= 3)
;;   3. the mutation-epoch counter at 268435544 is unchanged since the form
;;      started
;;   4. the signal flag at 268435472 is 0 and the quit flag at 268435464 is 0
;;
;; The soundness condition this gate exists to enforce dynamically is: no
;; storage allocated during the form has escaped (become reachable beyond
;; the form), so rewinding the arena over it is safe.  The epoch bump and
;; the two flag writes ARE the veto mechanism -- code that might leak arena
;; storage past the form boundary is expected to raise one of them, so the
;; reclaim declines.
;;
;; This file censuses the producer side of that contract.  It finds every
;; `defun' in the tree whose body both (a) allocates per-form arena storage
;; via `alloc-bytes' or `nl_alloc_vector', and (b) calls one of the
;; `nelisp-reclaim-veto-census--escape-primitives' -- the operations that
;; install a value into a long-lived structure (the env mirror or a lexical
;; frame) reachable after the form returns.  Calls are recognised whether
;; direct (`(FN ...)') or cross-unit (`(extern-call FN ...)'); this AOT
;; substrate uses both calling conventions for what is, at the source level,
;; the same kind of call.  Any such defun is an ESCAPE: it hands the reclaim
;; a storage location that must not be rewound.  If its own body also
;; contains the veto write (an epoch bump at 268435544 or a flag write to
;; 268435472/268435464), the reclaim is told to decline and the escape is
;; VETOED.  If it does not, the defun's storage is at risk of being rewound
;; out from under a value that is still reachable.
;;
;; Measured 2026-08: the two producer sets do not overlap.  Every function
;; that installs into a long-lived structure while allocating arena storage
;; is UNVETOED; every function that vetoes does so for an unrelated reason
;; (signal stashing, `bf_make_symbol') and does not itself escape storage
;; this way.  Whether an invariant elsewhere (a bound on possible escape
;; shapes, a caller-side guarantee) makes this safe anyway is UNESTABLISHED;
;; this is directly adjacent to the open 2026-08 symbol-name-overread defect
;; (task #13).  This file does not change runtime behaviour and does not
;; add veto writes -- it only makes the current producer set explicit, in
;; `nelisp-reclaim-veto-census--known-unvetoed', so a NEW escaping,
;; unvetoed producer cannot appear silently.  Whoever adds one must either
;; make it veto or consciously extend the allowlist.
;;
;; A stale allowlist entry (a name no longer found as an escaping-and-
;; unvetoed defun) is also a violation: a producer that disappears must not
;; leave a blind pass seated for whatever silently takes its place.
;;
;; The gate's own shape is asserted too: `nl_boundary_maybe_reclaim' must
;; still read exactly the epoch address (268435544) and the two flag
;; addresses (268435472, 268435464), alongside the reclaim-enable flag
;; (268436216).  If the gate changes -- a new flag, a moved address, a
;; dropped check -- this census's premise no longer holds and it must fail
;; loudly rather than silently keep validating the old contract.
;;
;; Deliberately conservative and syntactic, in the same spirit as
;; `tools/nelisp-buffer-len-gate.el' (see its header): a defun only counts
;; as ESCAPES when an allocation call and an escape-primitive call are both
;; literally present in its own body, at any nesting depth.  It does not
;; trace the call graph across defuns, so an indirect escape (allocate in A,
;; hand the pointer to B, B calls the primitive) is not detected here.  That
;; is a real gap, not a rounding error, and the report's denominators make
;; clear how many defuns were actually examined and how each classified --
;; "0 violations" is only meaningful next to that count.

;;; Code:

(require 'cl-lib)

(defconst nelisp-reclaim-veto-census--escape-primitives
  '(nelisp_mirror_install_entry
    nelisp_mirror_install_entry_or_insert
    nelisp_mirror_set_value
    nelisp_mirror_set_function
    nelisp_mirror_set_constant
    nelisp_mirror_set_value_or_insert
    nelisp_mirror_set_function_or_insert
    nelisp_mirror_set_constant_or_insert
    nelisp_mirror_bucket_prepend
    nelisp_env_bind_local
    nelisp_env_set_value
    nelisp_env_install_empty_globals_frames)
  "Functions that install a value directly into a long-lived structure.

Each one writes into the env mirror (the global symbol table) or a
lexical frame -- storage reachable after the calling form returns.  A
defun that both calls one of these AND allocates per-form arena storage
(`alloc-bytes' / `nl_alloc_vector') is handing the reclaim a pointer it
must not rewind past; see the file header for the full contract.

All twelve were confirmed present as `defun' forms in lisp/*.el or
scripts/*.el by grep before this list was written down.")

(defconst nelisp-reclaim-veto-census--veto-addrs '(268435544 268435472 268435464)
  "The epoch counter and the two abort flags a veto write must target.
268435544 is the mutation-epoch counter (bumped via `atomic-fetch-add');
268435472 is the signal flag and 268435464 is the quit flag (both set via
`ptr-write-u64').  These are exactly the three addresses (besides the
on/off switch below) `nl_boundary_maybe_reclaim' reads before rewinding.")

(defconst nelisp-reclaim-veto-census--enable-addr 268436216
  "The reclaim on/off flag `nl_boundary_maybe_reclaim' also reads.
Excluded from the gate-shape exact-match check: it toggles the mechanism
as a whole, it is not one of the per-form veto conditions.")

(defconst nelisp-reclaim-veto-census--gate-defun 'nl_boundary_maybe_reclaim
  "Name of the defun whose shape this census asserts against drift.")

(defconst nelisp-reclaim-veto-census--known-unvetoed
  '(;; env-mirror / frame installers -- mirror_*_or_insert dispatch bodies
    ;; (Doc 119 Sec.A): each stages a fresh symbol-entry in a per-form
    ;; alloc-bytes slot, then extern-calls `nelisp_mirror_bucket_prepend'
    ;; to splice it into the global mirror on a miss.
    nelisp_mirror_set_value_or_insert_dispatch
    nelisp_mirror_set_function_or_insert_dispatch
    nelisp_mirror_set_constant_or_insert_dispatch
    ;; env-leaves-logic.el (condition-case / let-binding CPS logic):
    ;; binds a lexical/global var and, on an unmatched signal, stashes it
    ;; into an Env variable -- both via `nelisp_env_bind_local' /
    ;; `nelisp_env_set_value' while allocating the scratch vector.
    nl_logic_bind_local
    nl_logic_stash_signal
    ;; evalport-bootstrap.el: installs a builtin/macro into the mirror
    ;; during bootstrap, and allocates the empty globals+frames pair.
    nl_install_one
    nl_bootstrap_make_mirror
    ;; evalport-combiner-apply.el (`apply'/`funcall'/`fset' combinators):
    ;; wrong-type-argument stash, lexical bind, and the verbatim-fset path
    ;; all allocate then install into the mirror or a frame.
    nl_apply_stash_wta
    nl_apply_bind_local
    nl_apply_do_fset
    ;; evalport-combiner-entry.el: max-recursion-depth / quit signal
    ;; stashes bound into the Env via `nelisp_env_bind_local'.
    nl_entry_stash_max_depth
    nl_entry_stash_quit
    ;; evalport-env-leaves-bind.el (`setq'/formal-parameter binding):
    ;; every bind/set leaf allocates its scratch vector then installs.
    nl_env_set_value
    nl_bf_bind_sym
    nl_bf_bind_optional
    nl_bf_bind_rest)
  "Defuns that escape arena storage into a long-lived structure without
vetoing the boundary reclaim (see the file header for the contract).

Whether a compensating invariant elsewhere makes each of these safe
despite not vetoing is UNESTABLISHED -- this sits next to the open
2026-08 symbol-name-overread defect (task #13).  This list exists so a
NEW escaping producer cannot join the set silently: anyone who adds one
must either make it veto the reclaim (bump the epoch at 268435544, or
raise the signal/quit flag at 268435472/268435464) or consciously extend
this list, understanding they are doing the same thing every entry here
already does.

A name here can appear in more than one file: `scripts/nelisp-standalone-
build.el' patches several env-leaves-bind / combiner-apply defuns at
build time (`nelisp-standalone--patch-env-leaves-bind-rest' and the
`nelisp-standalone--reader-*-fixed' constants around its Doc 137 M3
comment), so both the pre-patch `lisp/*.el' body and the patched
replacement are read by this census as separate `defun' occurrences of
the same name.  Some patched replacements (`nl_apply_stash_wta',
`nl_env_stash_signal') DO veto in their fixed form; others
(`nl_apply_do_fset', `nl_bf_bind_rest') still do not.  Listing the name
here covers every occurrence, vetoed or not -- an already-vetoed
occurrence simply never reaches the allowlist check.

Discovered by running this census over the tree on 2026-08-13; see
`nelisp-reclaim-veto-census-run'.")

(defvar nelisp-reclaim-veto-census--found nil
  "Accumulator: list of (NAME FILE BODY) for every `defun' seen this run.
BODY is the list of forms after the arglist -- not yet classified; kept
raw so both the escape/veto scan and the gate-shape scan can read it.")

(defun nelisp-reclaim-veto-census--walk-collect (form pred)
  "Collect PRED's non-nil results from every cons subform of FORM.
PRED is called on each cons reached and may return a list to splice into
the result; anything else is treated as \"no match\".  Iterative along
each cdr spine and recursive only into cars: these source files hold
thousand-element literal lists (IR tag tables, opcode tables), and a
naive recursive walk of the cdr spine exhausts `max-lisp-eval-depth'.
Mirrors `nelisp-buffer-len-gate--mentions-p'."
  (let ((stack (list form)) (acc nil))
    (while stack
      (let ((f (pop stack)))
        (when (consp f)
          (let ((r (funcall pred f)))
            (when r (setq acc (append r acc))))
          (let ((tail f))
            (while (consp tail)
              (push (car tail) stack)
              (setq tail (cdr tail)))
            (when tail (push tail stack))))))
    acc))

(defun nelisp-reclaim-veto-census--escape-calls (body)
  "Return the escape-primitive names BODY calls, direct or via `extern-call'."
  (delete-dups
   (nelisp-reclaim-veto-census--walk-collect
    body
    (lambda (f)
      (let ((head (car-safe f)))
        (cond
         ((memq head nelisp-reclaim-veto-census--escape-primitives)
          (list head))
         ((and (eq head 'extern-call)
               (consp (cdr f))
               (memq (cadr f) nelisp-reclaim-veto-census--escape-primitives))
          (list (cadr f)))
         (t nil)))))))

(defun nelisp-reclaim-veto-census--allocates (body)
  "Return the arena-allocation calls (`alloc-bytes' / `nl_alloc_vector')
found anywhere in BODY, or nil if none."
  (nelisp-reclaim-veto-census--walk-collect
   body
   (lambda (f)
     (when (memq (car-safe f) '(alloc-bytes nl_alloc_vector))
       (list (car f))))))

(defun nelisp-reclaim-veto-census--vetoes (body)
  "Return the veto writes found anywhere in BODY, or nil if none.
A veto write is `(atomic-fetch-add 268435544 ...)' (the epoch bump) or a
`(ptr-write-u64 268435472 ...)' / `(ptr-write-u64 268435464 ...)' (the
signal/quit flags) -- literal addresses only, matching how
`nl_boundary_maybe_reclaim' reads them."
  (nelisp-reclaim-veto-census--walk-collect
   body
   (lambda (f)
     (cond
      ((and (eq (car-safe f) 'atomic-fetch-add) (eql (nth 1 f) 268435544))
       (list 268435544))
      ((and (eq (car-safe f) 'ptr-write-u64)
            (memq (nth 1 f) '(268435472 268435464)))
       (list (nth 1 f)))
      (t nil)))))

(defun nelisp-reclaim-veto-census--read-u64-addrs (body)
  "Return the set of literal addresses BODY reads via `ptr-read-u64'."
  (delete-dups
   (nelisp-reclaim-veto-census--walk-collect
    body
    (lambda (f)
      (when (and (eq (car-safe f) 'ptr-read-u64) (integerp (nth 1 f)))
        (list (nth 1 f)))))))

(defun nelisp-reclaim-veto-census--set-diff (a b)
  "Return the elements of list A not `member' of list B."
  (cl-remove-if (lambda (x) (member x b)) a))

(defun nelisp-reclaim-veto-census--walk (form file)
  "Walk FORM (one top-level form read from FILE), recording every `defun'.
Most runtime defuns are nested inside quoted `(seq ...)' manifests, so
this descends through arbitrary wrapper structure rather than only
looking at the top level.  A `proper-list-p' guard comes before any
`nth'/`nthcdr': some literals in this tree are improper lists (alist
pairs like `(let . 31)' in an IR tag table), and destructuring those with
`nth' throws wrong-type-argument.  Mirrors `nelisp-buffer-len-gate--walk'."
  (when (consp form)
    (when (and (eq (car-safe form) 'defun) (proper-list-p form)
               (symbolp (nth 1 form)) (nth 1 form))
      (push (list (nth 1 form) file (nthcdr 3 form))
            nelisp-reclaim-veto-census--found))
    (let ((tail form))
      (while (consp tail)
        (nelisp-reclaim-veto-census--walk (car tail) file)
        (setq tail (cdr tail))))))

(defun nelisp-reclaim-veto-census--check-gate-shape ()
  "Assert `nl_boundary_maybe_reclaim' still reads exactly the veto addresses.
Returns non-nil (t) when the shape matches; pushes a violation string and
returns nil otherwise.  A missing or duplicated defun is also a failure --
either means this census is no longer looking at the gate it claims to."
  (let ((matches (cl-remove-if-not
                  (lambda (r) (eq (nth 0 r) nelisp-reclaim-veto-census--gate-defun))
                  nelisp-reclaim-veto-census--found)))
    (cond
     ((null matches)
      (list nil (format "GATE-SHAPE: `%s' not found in the scanned files"
                        nelisp-reclaim-veto-census--gate-defun)))
     ((> (length matches) 1)
      (list nil (format "GATE-SHAPE: %d `%s' defuns found, expected 1"
                        (length matches) nelisp-reclaim-veto-census--gate-defun)))
     (t
      (let* ((body (nth 2 (car matches)))
             (addrs (nelisp-reclaim-veto-census--read-u64-addrs body))
             (expected (cons nelisp-reclaim-veto-census--enable-addr
                             nelisp-reclaim-veto-census--veto-addrs))
             (missing (nelisp-reclaim-veto-census--set-diff expected addrs))
             (extra (nelisp-reclaim-veto-census--set-diff addrs expected)))
        (if (or missing extra)
            (list nil (format
                       "GATE-SHAPE: `%s' reads %S; expected exactly %S (missing=%S extra=%S)"
                       nelisp-reclaim-veto-census--gate-defun addrs expected
                       missing extra))
          (list t nil)))))))

(defun nelisp-reclaim-veto-census-run (files)
  "Census FILES for escaping-but-unvetoed reclaim producers.
Returns the number of violations found.  See the file header for the
contract and `nelisp-reclaim-veto-census--known-unvetoed' for the
allowlist this asserts against."
  (setq nelisp-reclaim-veto-census--found nil)
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((short (file-name-nondirectory file)))
        (condition-case nil
            (while t
              (nelisp-reclaim-veto-census--walk (read (current-buffer)) short))
          (end-of-file nil)
          (invalid-read-syntax nil)))))
  (setq nelisp-reclaim-veto-census--found
        (nreverse nelisp-reclaim-veto-census--found))
  (let* ((total-defuns (length nelisp-reclaim-veto-census--found))
         (escaping nil)
         (violations nil)
         (vetoed-count 0)
         (allowlisted-count 0))
    ;; Pass 2a: classify every defun that both allocates and calls an
    ;; escape primitive.
    (dolist (rec nelisp-reclaim-veto-census--found)
      (cl-destructuring-bind (name file body) rec
        (let ((calls (nelisp-reclaim-veto-census--escape-calls body))
              (allocs (nelisp-reclaim-veto-census--allocates body)))
          (when (and calls allocs)
            (let* ((vetoes (nelisp-reclaim-veto-census--vetoes body))
                   (vetoed (and vetoes t))
                   (allowlisted (and (not vetoed)
                                     (memq name nelisp-reclaim-veto-census--known-unvetoed)
                                     t)))
              (push (list name file calls vetoed allowlisted) escaping)
              (cond
               (vetoed (cl-incf vetoed-count))
               (allowlisted (cl-incf allowlisted-count))
               (t (push (format
                         "%s: `%s' escapes arena storage via %S without vetoing the reclaim, and is not in `nelisp-reclaim-veto-census--known-unvetoed'"
                         file name calls)
                        violations))))))))
    (setq escaping (nreverse escaping))
    ;; Pass 2b: stale allowlist entries -- a name listed but never found as
    ;; an escaping-and-unvetoed defun this run.
    (let ((live (mapcar #'car (cl-remove-if (lambda (e) (or (nth 3 e) (not (nth 4 e))))
                                            escaping))))
      (dolist (name nelisp-reclaim-veto-census--known-unvetoed)
        (unless (member name live)
          (push (format
                 "STALE allowlist entry `%s': no escaping-and-unvetoed defun found this run"
                 name)
                violations))))
    ;; Pass 2c: the gate's own shape.
    (cl-destructuring-bind (gate-ok gate-msg)
        (nelisp-reclaim-veto-census--check-gate-shape)
      (unless gate-ok
        (push gate-msg violations))
      (setq violations (nreverse violations))
      (dolist (e escaping)
        (cl-destructuring-bind (name file calls vetoed allowlisted) e
          (message "[reclaim-veto-census] %s (%s): calls=%S %s"
                   name file calls
                   (cond (vetoed "vetoed")
                         (allowlisted "allowlisted")
                         (t "VIOLATION")))))
      (dolist (v violations)
        (message "VIOLATION %s" v))
      (message (concat "[reclaim-veto-census] defuns-examined=%d escapes=%d"
                       " vetoed=%d allowlisted=%d gate-shape=%s violations=%d")
               total-defuns (length escaping) vetoed-count allowlisted-count
               (if gate-ok "ok" "FAIL") (length violations))
      (length violations))))

(provide 'nelisp-reclaim-veto-census)
;;; nelisp-reclaim-veto-census.el ends here
