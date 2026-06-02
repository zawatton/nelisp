;;; emacs-standalone.el --- Standalone NeLisp dispatch scaffold  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track M (2026-05-03) — Layer 2 γ-deeper.
;;
;; Scaffolding for the "standalone" runtime mode (= NeLisp without a
;; host Emacs C-core).  Two-mode bridging is already in use across
;; the substrate (Track I being the canonical example): under host
;; Emacs, calls delegate to the host's C primitive; otherwise the
;; substrate signals `emacs-*-not-implemented' and stops.  The real
;; long-term goal is for those signals to be replaced by dispatches
;; into NeLisp's standalone primitives.
;;
;; This module provides the *registry* + *detection* layer those
;; dispatches will plug into — the actual primitive implementations
;; live in NeLisp itself and are registered from there.  The hook
;; the substrate exposes is:
;;
;;     (emacs-standalone-call-primitive NAME ARGS &optional FALLBACK)
;;
;; which the substrate calls in place of `signal' when no host
;; binding is present.  If a primitive is registered for NAME the
;; registered function is invoked with ARGS; otherwise FALLBACK is
;; called (= typically the signal form the caller would have raised
;; directly).
;;
;; Mode detection (`emacs-standalone-mode-p'):
;;
;;   1. If `emacs-standalone-force-mode' is bound to non-nil, it
;;      wins (= test fixtures + manual override).
;;   2. Else if NeLisp's runtime feature `nelisp-emacs-runtime' is
;;      `featurep'-true, we are standalone.
;;   3. Else, we infer from the absence of a known host primitive
;;      (= `make-process' is C in upstream Emacs and is one of the
;;      first things missing under a NeLisp-only runtime).
;;
;; The scaffold deliberately stops at the registry — wiring real
;; NeLisp primitives is a separate task that lives in the NeLisp
;; source tree.

;;; Code:

(require 'cl-lib)

(defconst emacs-standalone-version 1
  "Schema version of the standalone primitive-dispatch contract.")

;;;; --- mode detection ------------------------------------------------

(defvar emacs-standalone-force-mode 'auto
  "Override for `emacs-standalone-mode-p'.

When `auto' (default), the predicate auto-detects (= rule 2 then
rule 3 in the commentary).  When non-nil and not `auto', forces
standalone mode on.  When nil, forces it off (= test fixture aid
for asserting host-mode behaviour deterministically).")

(defvar emacs-standalone--detected nil
  "Cached result of the last `emacs-standalone--detect' call.")

(defvar emacs-standalone--initialized nil
  "Non-nil once `emacs-standalone-init' has been run.")

(defun emacs-standalone--detect ()
  "Run the auto-detection rules and cache the result.
Returns t when standalone, nil otherwise."
  (let ((result
         (cond
          ((featurep 'nelisp-emacs-runtime) t)
          ;; `make-process' under upstream Emacs is a C builtin; in
          ;; pure NeLisp the symbol may exist as a stub but its
          ;; implementation will not be a subr.  We treat absence of
          ;; `subrp' (= host's C-tag predicate) on `make-process'
          ;; as the standalone signal.
          ((and (fboundp 'make-process)
                (fboundp 'subrp)
                (not (subrp (symbol-function 'make-process))))
           t)
          ((not (fboundp 'make-process)) t)
          (t nil))))
    (setq emacs-standalone--detected result)
    result))

(defun emacs-standalone-mode-p ()
  "Return non-nil when the current runtime is standalone NeLisp.

Honours `emacs-standalone-force-mode': `auto' triggers detection,
non-nil non-`auto' forces t, nil forces nil."
  (cond
   ((eq emacs-standalone-force-mode 'auto)
    (emacs-standalone--detect))
   (emacs-standalone-force-mode t)
   (t nil)))

(defun emacs-standalone-active-p ()
  "Non-nil only when standalone *and* the dispatcher is initialised."
  (and emacs-standalone--initialized
       (emacs-standalone-mode-p)))

;;;; --- primitive registry --------------------------------------------

(defvar emacs-standalone--primitives (make-hash-table :test 'eq)
  "NAME → FN map of NeLisp primitives available to the substrate.")

(defun emacs-standalone-register-primitive (name fn)
  "Register FN as the standalone implementation of primitive NAME.
NAME is an unprefixed symbol (e.g. `make-process').  FN is a
callable that accepts the same argument list as the host binding.
Re-registering NAME replaces the previous FN."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (unless (functionp fn)
    (signal 'wrong-type-argument (list 'functionp fn)))
  (puthash name fn emacs-standalone--primitives)
  name)

(defun emacs-standalone-unregister-primitive (name)
  "Remove the standalone primitive registration for NAME.  Returns t
on success, nil if NAME was not registered."
  (when (gethash name emacs-standalone--primitives)
    (remhash name emacs-standalone--primitives)
    t))

(defun emacs-standalone-has-primitive-p (name)
  "Return non-nil when NAME has a registered standalone primitive."
  (and (gethash name emacs-standalone--primitives) t))

(defun emacs-standalone-registered-primitives ()
  "Return the list of currently-registered primitive names."
  (let (out)
    (maphash (lambda (k _v) (push k out)) emacs-standalone--primitives)
    (nreverse out)))

(defun emacs-standalone-clear-registry ()
  "Drop every registered standalone primitive.  Test-only helper."
  (clrhash emacs-standalone--primitives)
  nil)

;;;; --- dispatch core -------------------------------------------------

(define-error 'emacs-standalone-error "nelisp-emacs standalone error")
(define-error 'emacs-standalone-no-primitive
  "no NeLisp primitive registered for this name"
  'emacs-standalone-error)

(defun emacs-standalone-call-primitive (name args &optional fallback)
  "Dispatch a substrate call NAME with ARGS to the standalone runtime.

When a primitive is registered for NAME, calls (FN . ARGS).
When no primitive is registered:
  - if FALLBACK is non-nil, calls it with ARGS;
  - else signals `emacs-standalone-no-primitive'.

The substrate's `emacs-*-delegate' family calls this in place of
`signal' so that *any* Track I-style two-mode dispatcher can be
upgraded to true standalone behaviour just by registering names
on the runtime side."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (let ((fn (gethash name emacs-standalone--primitives)))
    (cond
     (fn (apply fn args))
     ((functionp fallback) (apply fallback args))
     (t (signal 'emacs-standalone-no-primitive (list name))))))

;;;; --- two-mode helper -----------------------------------------------

(defun emacs-standalone-dispatch (sym args host-fn signal-fn)
  "Generic two-mode dispatcher used by substrate modules.

SYM is the unprefixed symbol the substrate is implementing (= the
key the standalone runtime would register under).
ARGS is the argument list for SYM.
HOST-FN, when non-nil, is a callable to invoke under host mode
(= typically `(indirect-function sym)').
SIGNAL-FN, when non-nil, is a 0-arg callable invoked when neither
host nor standalone primitive is available; otherwise a default
`emacs-standalone-no-primitive' signal is raised.

Lookup order:
  1. host-mode + HOST-FN bound → apply HOST-FN.
  2. standalone-mode + primitive registered → dispatch.
  3. otherwise SIGNAL-FN or no-primitive signal."
  (cond
   ((and host-fn (not (emacs-standalone-mode-p)))
    (apply host-fn args))
   ((emacs-standalone-has-primitive-p sym)
    (emacs-standalone-call-primitive sym args))
   ((functionp signal-fn) (funcall signal-fn))
   (t (signal 'emacs-standalone-no-primitive (list sym)))))

;;;; --- bootstrap -----------------------------------------------------

(defun emacs-standalone-init ()
  "Mark the dispatch scaffold as initialised.

Idempotent: re-calls are no-ops but return t.  Invoked once from
`nemacs-init' (= `nemacs-loadup.el') so the rest of the substrate
can rely on `emacs-standalone-active-p' returning a stable value
after bootstrap."
  (unless emacs-standalone--initialized
    (emacs-standalone--detect)
    (setq emacs-standalone--initialized t))
  t)

(defun emacs-standalone-uninit ()
  "Reset the dispatcher state.  Test-only helper."
  (setq emacs-standalone--initialized nil
        emacs-standalone--detected nil)
  nil)

;;;; --- introspection -------------------------------------------------

(defun emacs-standalone-status ()
  "Return a plist describing the dispatcher state.  Useful for tooling."
  (list :version       emacs-standalone-version
        :initialized   emacs-standalone--initialized
        :force-mode    emacs-standalone-force-mode
        :detected      emacs-standalone--detected
        :mode-p        (emacs-standalone-mode-p)
        :primitives    (emacs-standalone-registered-primitives)
        :primitive-count (hash-table-count emacs-standalone--primitives)))

(provide 'emacs-standalone)

;;; emacs-standalone.el ends here
