;;; nl-clj-future.el --- Cooperative pmap/future/pcalls for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 199 Tier 1.  A cooperative `future'/`deref'/`pcalls'/`pmap'.
;; Single thread of Lisp execution: correct today, zero speedup today --
;; the honest "library ceiling" Doc 199's owner-constraint predicts, made
;; concrete rather than asserted.
;;
;; Why ship it with no speedup?  API stability (Doc 199 §7).  These
;; signatures are designed so the *implementation* under `nl-clj-future'
;; can later swap this deferred run-queue for a real `nl-thread-spawn' +
;; join (Doc 199 §4/§6) with no source change in any caller.  A caller who
;; adopts `nl-clj-pmap' today, and later runs on a NeLisp build where that
;; runtime work has landed, gets real speedup for free.
;;
;; Implementation note -- deliberate deviation from Doc 199 §3.1's sketch.
;; §3.1 built this on `nelisp-actor' (`nelisp-spawn' + `nelisp-actor-run-
;; until-idle').  That does not survive the standalone substrate: a NEW
;; `nelisp-actor-lambda' form needs `generator.el' to macroexpand, which
;; `nelisp-actor' itself documents as host-Emacs-only (see nl-clj-async-
;; standalone-smoke.el's own commentary).  A future worker here never
;; parks -- it runs a thunk straight to a value -- so the actor/CPS
;; machinery buys nothing it needs and costs standalone portability.  A
;; plain deferred queue delivers the identical observable semantics
;; (spawn defers; deref drives every pending worker to completion, none
;; starves) and runs on `target/nelisp'.  The API §7 fixes is unchanged.
;;
;; Naming note: `nl-clj--pmap' (double dash) is nl-clj-core's PERSISTENT
;; MAP tag.  `nl-clj-pmap' (single dash) here is Clojure's PARALLEL MAP
;; function.  Different things; the near-collision is Clojure's own.
;;
;; §7 worker-body discipline (checkable, not yet load-bearing): a thunk
;; handed to `nl-clj-future'/`nl-clj-pmap' must reach shared state only
;; through nl-clj's immutable persistent structures or an explicit nl-safe
;; borrow cell -- never a bare shared mutable cons/vector/hash-table.
;; `nl-clj-future-with-lint' points nl-safe's existing dynamic violation
;; logger at a pmap/pcalls run.  It cannot expose a race (there is no real
;; interleaving), but it makes nl-safe-visible worker mistakes reproducible
;; before the runtime tiers land.  See README.

;;; Code:

(require 'nl-clj-core)
(require 'nl-clj-seq)

;; The lint path loads nl-safe lazily.  These declarations make its dynamic
;; variables special for byte compilation without adding an nl-safe load or
;; any worker-path work when the lint is not invoked.
(defvar nl-safe-log-violations)
(defvar nl-safe--violation-log)

(define-error 'nl-clj-future-deadlock
  "nl-clj future cannot complete (no pending worker produced it)" 'nl-clj-error)

(defconst nl-clj--future-tag 'nl-clj--future
  "Tag in slot 0 of a future handle vector [TAG VALUE DONE-P THUNK].")

(defvar nl-clj-future--queue nil
  "FIFO list of pending future handles awaiting a drive.
Global and single-threaded, matching the one thread of Lisp execution
this cooperative tier runs on.")

(defvar nl-clj-future--lint-violations nil
  "Violations captured by the most recently completed worker lint scope.
Records retain nl-safe's native newest-first order.  Read this through
`nl-clj-future-lint-violations'.")

(defun nl-clj-future-lint-violations ()
  "Return nl-safe records from the most recent worker lint scope.
The list is newest first, matching `nl-safe--violation-log'.  A scope
that logged nothing leaves this nil."
  nl-clj-future--lint-violations)

(defun nl-clj-future--call-with-lint (thunk)
  "Call THUNK with nl-safe violation logging scoped to this invocation.
Save the scope's newest-first records for
`nl-clj-future-lint-violations', including when THUNK exits nonlocally.
The original nl-safe log and enable flag are dynamically preserved."
  ;; Load only on opt-in.  Normal future execution neither loads nl-safe
  ;; here nor tests a flag in the worker path.
  (require 'nl-safe)
  (setq nl-clj-future--lint-violations nil)
  (let ((nl-safe-log-violations t)
        (nl-safe--violation-log nil))
    (unwind-protect
        (funcall thunk)
      (setq nl-clj-future--lint-violations nl-safe--violation-log))))

(defmacro nl-clj-future-with-lint (&rest body)
  "Run BODY under nl-safe's violation logger and return BODY's value.
BODY should contain an `nl-clj-pmap' or `nl-clj-pcalls' run.  Afterwards,
call `nl-clj-future-lint-violations' to inspect any nl-safe records.  The
records are also retained when BODY signals; the original error still
propagates.  Code outside this explicit scope follows the unchanged,
un-instrumented future path."
  (declare (indent 0) (debug t))
  `(nl-clj-future--call-with-lint (lambda () ,@body)))

(defun nl-clj-future-p (object)
  "Return non-nil when OBJECT is an nl-clj future handle."
  (nl-clj--tagged-p object nl-clj--future-tag))

(defun nl-clj-future (thunk)
  "Enqueue THUNK (a 0-arg function) as a cooperative worker.
Return a future handle immediately; THUNK has not run yet.  Realise the
value with `nl-clj-deref' (or `nl-clj-future-await')."
  (let ((handle (vector nl-clj--future-tag nil nil thunk)))
    ;; Prepend (O(1)); run order is irrelevant to correctness -- each
    ;; worker's value lands in its own handle, and `nl-clj-pcalls'
    ;; collects by handle in argument order, not by queue order.  A
    ;; tail-append here would make `nl-clj-pmap' O(n^2).
    (push handle nl-clj-future--queue)
    handle))

(defun nl-clj-future-done-p (f)
  "Return non-nil when future F has produced its value."
  (unless (nl-clj-future-p f)
    (signal 'nl-clj-type-error (list 'nl-clj-future-done-p "not an nl-clj future" f)))
  (aref f 2))

(defun nl-clj-future--run-one (handle)
  "Run HANDLE's thunk unless already done; store its value; mark done.
An error in the thunk propagates to the caller and leaves HANDLE
unrealised, so nothing silently swallows it."
  (unless (aref handle 2)
    (let ((v (funcall (aref handle 3))))
      (aset handle 1 v)
      (aset handle 2 t)
      (aset handle 3 nil))))

(defun nl-clj-future--drain ()
  "Run every pending future to completion, FIFO.
Re-entrant: a worker that itself awaits another future drives the shared
queue recursively, which is correct because the queue is global and
single-threaded."
  (while nl-clj-future--queue
    (nl-clj-future--run-one (pop nl-clj-future--queue))))

(defun nl-clj-future-await (f)
  "Drive every pending worker, then return future F's value.
Runs all queued workers to completion -- nothing is lost and no worker
starves, matching Doc 199 §3.  Signals `nl-clj-future-deadlock' if F is
still unrealised after the drive (a handle that was never enqueued)."
  (unless (nl-clj-future-p f)
    (signal 'nl-clj-type-error (list 'nl-clj-future-await "not an nl-clj future" f)))
  (nl-clj-future--drain)
  (unless (aref f 2)
    (signal 'nl-clj-future-deadlock (list f)))
  (aref f 1))

(defun nl-clj-pcalls (&rest thunks)
  "Run each THUNK as a future; return their values as a list in argument
order, once every one has completed."
  (mapcar #'nl-clj-future-await (mapcar #'nl-clj-future thunks)))

(defun nl-clj-pmap (f coll)
  "Apply F to each element of COLL as a future; return results in COLL order.
COLL is any FINITE collection `nl-clj-seq' materialises to a list (an
nl-clj vector/map/set, an ordinary list, a plain vector).  Cooperative
today (no speedup); API-stable across Doc 199's later runtime tiers."
  (apply #'nl-clj-pcalls
         (mapcar (lambda (x) (lambda () (funcall f x)))
                 (nl-clj-seq coll))))

(provide 'nl-clj-future)

;;; nl-clj-future.el ends here
