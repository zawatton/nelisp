;;; nl-clj-atom.el --- Clojure-style atom for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.5: `atom'/`deref'/`swap!'/`reset!'/`compare-and-set!'.
;;
;; Real Clojure's `swap!' is a compare-and-set RETRY loop *because* the
;; JVM is genuinely concurrent -- another thread's `swap!' could
;; interleave between this call's read and its write.  Doc 195 §2.7
;; measured, from three independent facts (Doc 39 §6.5/§9.4's explicit
;; single-thread event-loop decision, Doc 146 §2.1's single-heap
;; tracing GC with no concurrent-mutation synchronization, and
;; `tools/selfhost-mt-test.sh' proving real `clone(2)' threads exist
;; only as a build-time AOT-codegen artifact, not an ordinary-Elisp-
;; callable primitive a running interpreter session can reach), that
;; NeLisp today has exactly one thread of Lisp-level control.  Nothing
;; can preempt between the `aref' read below and the `aset' write that
;; follows it in the same function call -- there is no second thread
;; to interleave with.  The retry loop's entire reason to exist is
;; therefore absent, and this single-assignment body is already, for
;; everything `swap!'/`reset!'/`compare-and-set!' are observably
;; supposed to do under NeLisp's actual execution model, as correct as
;; Clojure's own retry loop -- not an approximation of it.  The one
;; honest divergence: `swap!' here calls F exactly once, always, a
;; strictly STRONGER guarantee than Clojure's own contract ("F may run
;; more than once on retry" -- real Clojure's own docstring warns
;; callers not to rely on at-most-once for a side-effecting F,
;; precisely because real Clojure cannot promise it).  The day NeLisp
;; gains real concurrent Lisp-level execution, this file's strategy
;; (not its observable contract) needs to change back to a genuine
;; CAS retry loop -- gated on the same frontier work Doc 195 §5.2
;; names, not a defect in this file.
;;
;; Representation (Doc 195 §3.2): [nl-clj--atom VALUE WATCHES].
;; WATCHES is reserved (always nil in Tier 1) -- `add-watch'/
;; `set-validator!' are named by Doc 195 §4.5 as real but peripheral,
;; deliberately out of this phase's scope.

;;; Code:

(require 'nl-clj-core)

;; Soft dependency: `nl-clj-deref' below handles futures when
;; nl-clj-future.el is loaded, without a hard require on it.
(declare-function nl-clj-future-p "nl-clj-future" (object))
(declare-function nl-clj-future-await "nl-clj-future" (f))

(defun nl-clj-atom (value)
  "Return a new atom wrapping VALUE."
  (vector nl-clj--atom-tag value nil))

(defun nl-clj-atom-p (object)
  "Return non-nil when OBJECT is an nl-clj atom."
  (nl-clj--tagged-p object nl-clj--atom-tag))

(defun nl-clj--atom-check (a caller)
  "Signal `nl-clj-type-error' unless A is an nl-clj atom.
CALLER names the public function doing the check, for the error data."
  (unless (nl-clj-atom-p a)
    (signal 'nl-clj-type-error (list caller "not an nl-clj atom" a))))

(defun nl-clj-deref (a)
  "Return the value held by atom A, or block for future A (Clojure `deref'/`@').
Atoms return their current value.  Futures are supported when
`nl-clj-future' is loaded (Doc 199 Tier 1): A blocks cooperatively until
the future is realised.  Any other object signals `nl-clj-type-error',
exactly as before futures existed."
  (cond
   ((nl-clj-atom-p a) (aref a 1))
   ((and (fboundp 'nl-clj-future-p) (nl-clj-future-p a))
    (nl-clj-future-await a))
   (t (nl-clj--atom-check a 'nl-clj-deref))))

(defun nl-clj-swap! (a f &rest args)
  "Set A's value to (apply F current-value ARGS); return the new value.
See this file's Commentary for why NeLisp's current single-thread
execution model makes this single-assignment body exact, not a
shortcut."
  (nl-clj--atom-check a 'nl-clj-swap!)
  (let ((new (apply f (aref a 1) args)))
    (aset a 1 new)
    new))

(defun nl-clj-reset! (a v)
  "Set A's value to V, discarding the previous value; return V."
  (nl-clj--atom-check a 'nl-clj-reset!)
  (aset a 1 v)
  v)

(defun nl-clj-compare-and-set! (a old new)
  "Set A's value to NEW iff its current value is `eq' to OLD.
Return non-nil iff the swap happened."
  (nl-clj--atom-check a 'nl-clj-compare-and-set!)
  (if (eq old (aref a 1))
      (progn (aset a 1 new) t)
    nil))

(provide 'nl-clj-atom)

;;; nl-clj-atom.el ends here
