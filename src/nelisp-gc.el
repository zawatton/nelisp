;;; nelisp-gc.el --- Phase 3c GC / NeLisp object tracking  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3c ships a NeLisp-level object tracker.  Host Emacs GC still
;; owns memory reclamation — this module answers "which NeLisp values
;; are reachable from NeLisp roots?" rather than freeing anything.
;;
;; Purpose (see docs/design/09-phase3c-gc.org):
;;   1. Phase 4 actor boundary reachability (who owns what?)
;;   2. Finalizer hooks for host-GC-invisible resources
;;   3. Heap introspection for diagnostics / MCP
;;
;; Phase 3c.1 scope: skeleton + root set enumeration only.  Mark pass,
;; finalizer registry, MCP tools and actor boundary API land in
;; 3c.2-5.  No public GC API yet except `nelisp-gc-root-set'.

;;; Code:

(require 'cl-lib)

(defvar nelisp-gc--active-vms nil
  "Stack of bytecode-VM state vectors currently executing.
`nelisp-bc-run' pushes the VM on entry and pops on exit via
`unwind-protect', so at GC scan time this holds only in-flight
stacks.  Leaf = outermost call; top-of-list = innermost.")

(defun nelisp-gc--globals-root ()
  "Return the four NeLisp-level global hash tables as a single plist root.
Each element of the returned list is `(:kind K :value HT)' so downstream
walkers treat each table as its own subtree without unpacking here."
  (let (out)
    (dolist (sym '(nelisp--globals nelisp--functions
                   nelisp--macros nelisp--specials))
      (when (boundp sym)
        (push (list :kind sym :value (symbol-value sym)) out)))
    (nreverse out)))

(defun nelisp-gc--vm-stacks-root ()
  "Return each live VM state vector as its own root entry.
Empty list when no bytecode VM is running.  Covers both classical
bcl-run and JIT-marker bcl, though JIT stacks never push onto
`nelisp-gc--active-vms' — the JIT fast-path skips VM setup entirely."
  (mapcar (lambda (vm) (list :kind 'vm-stack :value vm))
          nelisp-gc--active-vms))

(defun nelisp-gc-root-set ()
  "Return the current NeLisp root set as a list of `(:kind K :value V)' plists.
The set is recomputed on every call — call sites that scan repeatedly
during one mark pass should bind the result themselves.

Root categories (Phase 3c.1):
  `nelisp--globals'     — user-level defvar storage
  `nelisp--functions'   — defun / defalias cells
  `nelisp--macros'      — defmacro cells
  `nelisp--specials'    — dynamic-scope marker table
  `vm-stack'            — each active bytecode-VM state vector

specpdl is deliberately NOT a top-level root — it lives inside each
VM state vector (slot 8) and is already reached via `vm-stack'.  JIT
closure constants pools are host-Elisp-reachable; re-walking them
here would duplicate work the host GC already handles (design doc
§2.2 decision A)."
  (append (nelisp-gc--globals-root)
          (nelisp-gc--vm-stacks-root)))

(defmacro nelisp-gc--with-active-vm (vm &rest body)
  "Execute BODY with VM pushed onto `nelisp-gc--active-vms'.
Pops on both normal and non-local exit so the active-VM stack stays
consistent with actual dispatch depth.  Called by `nelisp-bc-run'
via bytecode.el to keep that file's external surface unchanged."
  (declare (indent 1))
  `(let ((nelisp-gc--active-vms (cons ,vm nelisp-gc--active-vms)))
     ,@body))

;;; Mark pass (Phase 3c.2) --------------------------------------------

(defun nelisp-gc--seed-work (root-override)
  "Return the initial work list for a mark pass.
Extract `:value' from each root plist (yielding the actual NeLisp
object) so the walker can treat roots and transitive edges uniformly
without carrying the plist shape through the loop."
  (let ((seeds (or root-override (nelisp-gc-root-set)))
        out)
    (dolist (r seeds)
      (push (plist-get r :value) out))
    out))

(defun nelisp-gc-reachable-set (&optional root-override)
  "Return a hash-table of `eq'-unique objects reachable from roots.

Walk is iterative (explicit work list) so deeply nested or cyclic
structures cannot trip `max-lisp-eval-depth'.  ROOT-OVERRIDE, if
given, replaces the default `nelisp-gc-root-set' as the seed; it
must share the plist shape `(:kind K :value V)' per root entry.

Edge definition (design doc §2.3 decision A, Elisp structural):
  cons        → car, cdr
  vector      → every element
  hash-table  → every key and every value
  closure /
    bcl-tag   → reached via their list/vector structure (no special
                case; nelisp-closure / nelisp-bcl are cons chains)
  other       → leaf (numbers, strings, symbols, buffers, etc.)

Keys of the returned hash-table map to t.  Callers wanting object
counts or type histograms iterate via `maphash'."
  (let ((visited (make-hash-table :test 'eq))
        (work    (nelisp-gc--seed-work root-override)))
    (while work
      (let ((obj (pop work)))
        (unless (or (null obj) (gethash obj visited))
          (puthash obj t visited)
          (cond
           ((consp obj)
            (push (car obj) work)
            (push (cdr obj) work))
           ((vectorp obj)
            (let ((i (length obj)))
              (while (> i 0)
                (setq i (1- i))
                (push (aref obj i) work))))
           ((hash-table-p obj)
            (maphash (lambda (k v) (push k work) (push v work)) obj))
           ;; Leaf (numbers, strings, symbols, other primitives).
           (t nil)))))
    visited))

(defun nelisp-gc-reachable-count (&optional root-override)
  "Return the count of objects in `nelisp-gc-reachable-set'.
Convenience for callers that only need the size (e.g. bench harness,
=nelisp-heap-count=)."
  (hash-table-count (nelisp-gc-reachable-set root-override)))

;;; Finalizer registry (Phase 3c.3) -----------------------------------

(defvar nelisp-gc--finalizers (make-hash-table :test 'eq :weakness 'key)
  "Registered finalizers: OBJ -> THUNK.
Weak-key so that when host Emacs GC reclaims OBJ before a NeLisp-level
sweep, the entry vanishes automatically — we cannot reach the already-
collected object anyway.  `nelisp-gc-collect' fires THUNK for any OBJ
still in this table that is unreachable from NeLisp roots.")

(defvar nelisp-gc-auto-sweep nil
  "When non-nil, `post-gc-hook' runs `nelisp-gc-collect' automatically.
Disabled by default — a full root-set mark on every host GC cycle
is expensive and most sessions don't register enough finalizers to
justify the cost.  Long-running NeLisp daemons that rely on
finalisers for resource cleanup (file handles, actor mailboxes) can
opt in.")

(defun nelisp-gc-register-finalizer (obj thunk)
  "Schedule THUNK to run when OBJ becomes unreachable from NeLisp roots.
THUNK is called with OBJ as its only argument so the same closure
can be reused for many registered objects.  Returns OBJ unchanged so
callers can wrap resource acquisition idiomatically:

  (nelisp-gc-register-finalizer (open-file path)
                                (lambda (h) (close-handle h)))."
  (unless (functionp thunk)
    (signal 'wrong-type-argument (list 'functionp thunk)))
  (puthash obj thunk nelisp-gc--finalizers)
  obj)

(defun nelisp-gc-unregister-finalizer (obj)
  "Remove any finalizer registered for OBJ.  Return t if one existed."
  (prog1 (gethash obj nelisp-gc--finalizers)
    (remhash obj nelisp-gc--finalizers)))

(defun nelisp-gc-collect ()
  "Run a full NeLisp-level mark pass and fire finalizers.
For each OBJ in `nelisp-gc--finalizers' that is unreachable from the
current root set, call its THUNK once and remove the entry.  A THUNK
error is caught and logged so one misbehaving finalizer cannot abort
the sweep of the rest.  Return the number of finalizers fired."
  (let ((live  (nelisp-gc-reachable-set))
        (fired '())
        (count 0))
    (maphash
     (lambda (obj thunk)
       (unless (gethash obj live)
         (push obj fired)
         (cl-incf count)
         (condition-case err
             (funcall thunk obj)
           (error
            (message "nelisp-gc: finalizer error for %S: %S" obj err)))))
     nelisp-gc--finalizers)
    (dolist (o fired) (remhash o nelisp-gc--finalizers))
    count))

(defun nelisp-gc--post-gc-handler ()
  "Internal: `post-gc-hook' callback.
Runs `nelisp-gc-collect' only when `nelisp-gc-auto-sweep' is non-nil,
and swallows any error so NeLisp bugs cannot escalate into post-GC
signal noise (which Emacs would surface via `message' at display
time and potentially confuse the user about the origin)."
  (when nelisp-gc-auto-sweep
    (condition-case err
        (nelisp-gc-collect)
      (error
       (message "nelisp-gc: post-gc-hook error: %S" err)))))

(add-hook 'post-gc-hook #'nelisp-gc--post-gc-handler)

;;; Heap introspection (Phase 3c.4) -----------------------------------

(defun nelisp-gc--classify (obj)
  "Return a symbol tag for OBJ used by `nelisp-heap-count'."
  (cond
   ((consp obj)
    (pcase (car-safe obj)
      ('nelisp-closure 'closure)
      ('nelisp-bcl     'bcl)
      (_               'cons)))
   ((vectorp obj)      'vector)
   ((stringp obj)      'string)
   ((hash-table-p obj) 'hash-table)
   ((symbolp obj)      'symbol)
   ((numberp obj)      'number)
   (t                  'other)))

;;;###autoload
(defun nelisp-heap-count ()
  "Return a plist counting reachable NeLisp objects by type.
Run a full mark pass from the default `nelisp-gc-root-set' and
tabulate how many reachable cells fall into each category.  Result
keys: `:cons :closure :bcl :vector :string :hash-table :symbol
:number :other'."
  (interactive)
  (let ((live (nelisp-gc-reachable-set))
        (counts (list :cons 0 :closure 0 :bcl 0
                      :vector 0 :string 0 :hash-table 0
                      :symbol 0 :number 0 :other 0)))
    (maphash
     (lambda (obj _)
       (let ((key (pcase (nelisp-gc--classify obj)
                    ('cons       :cons)
                    ('closure    :closure)
                    ('bcl        :bcl)
                    ('vector     :vector)
                    ('string     :string)
                    ('hash-table :hash-table)
                    ('symbol     :symbol)
                    ('number     :number)
                    (_           :other))))
         (plist-put counts key (1+ (plist-get counts key)))))
     live)
    (when (called-interactively-p 'any)
      (message "nelisp heap: %S" counts))
    counts))

;;;###autoload
(defun nelisp-heap-roots ()
  "Summarise the current root set as `((:kind K :count N) ...)'.
Count semantics per root kind:
  hash-table   → `hash-table-count'
  list         → `length' (not the walker's reachable size)
  anything else → 1"
  (interactive)
  (let ((summary
         (mapcar
          (lambda (r)
            (let ((v (plist-get r :value)))
              (list :kind  (plist-get r :kind)
                    :count (cond
                            ((hash-table-p v) (hash-table-count v))
                            ((listp v)        (length v))
                            (t                1)))))
          (nelisp-gc-root-set))))
    (when (called-interactively-p 'any)
      (message "nelisp roots: %S" summary))
    summary))

(provide 'nelisp-gc)
;;; nelisp-gc.el ends here
