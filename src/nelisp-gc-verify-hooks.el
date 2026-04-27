;;; nelisp-gc-verify-hooks.el --- Phase 7.3 oracle wire-up into GC cycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.3 invariant gate *wire-up* — the four oracles defined in
;; `nelisp-gc-verify.el' (Doc 30 v2 §6.10) are pure read-only / side-
;; table checks; this module hooks them into the actual GC cycle so
;; *debug builds* automatically witness invariant violations as they
;; happen, instead of only catching them in the standalone ERTs.
;;
;; *Strict opt-in.*  `nelisp-gc-verify-debug-mode' defaults to nil so
;; production GC paths take exactly zero cycles inside this module.
;; When the flag flips on (defcustom set, env var
;; `NELISP_GC_VERIFY_DEBUG' present at load time, or
;; `nelisp-gc-verify-hooks-enable' called explicitly), four pieces of
;; advice slot in around the matching GC entry points and call the
;; corresponding oracle after each cycle event:
;;
;;   verify-heap                  ← `nelisp-gc-inner-run-sweep-phase'
;;                                  (after sweep complete)
;;   verify-card-table            ← `nelisp-gc-inner-run-minor-gc-with-promotion'
;;                                  (after each minor GC promotion)
;;   verify-poison-from-space     ← `nelisp-gc-inner-flip-semispace'
;;                                  (after Cheney from↔to flip)
;;   verify-double-scan           ← `nelisp-gc-inner-run-mark-phase'
;;                                  (after first mark phase, opt-in via
;;                                  `nelisp-gc-verify-debug-double-scan'
;;                                  because the oracle re-runs mark and
;;                                  doubles its cost; only sensible
;;                                  during dedicated debug runs)
;;
;; Violations are *aggregated* into a session-wide ring buffer
;; `nelisp-gc-verify-debug-log' (newest entries at the head, capped by
;; `nelisp-gc-verify-debug-log-cap').  Severity policy is configurable
;; via `nelisp-gc-verify-debug-severity':
;;
;;   `log'    — silent log only (default; useful for soak runs that
;;              should not bail on a single divergence).
;;   `signal' — log plus `error' so the offending GC cycle aborts and
;;              the test/operator gets a backtrace.
;;
;; *Recursion guards.*  The double-scan hook re-enters
;; `run-mark-phase'; we set a one-shot dynamic flag so the inner call
;; does not re-trigger the oracle (otherwise infinite recursion).  The
;; sweep hook is naturally non-recursive (the sweep does not invoke
;; itself), but we still guard via the same flag for symmetry and for
;; safety against future refactors.
;;
;; *No public-API changes.*  Every existing entry point in
;; `nelisp-gc-verify' / `nelisp-gc-inner' keeps its current signature.
;; This module is purely additive.

;;; Code:

(require 'cl-lib)
(require 'nelisp-gc-inner)
(require 'nelisp-gc-verify)


;;; §1. defcustoms + env-var bootstrap ----------------------------

(defgroup nelisp-gc-verify-hooks nil
  "Phase 7.3 invariant-gate wire-up into the live GC cycle."
  :group 'nelisp)

(defcustom nelisp-gc-verify-debug-mode
  (and (getenv "NELISP_GC_VERIFY_DEBUG") t)
  "When non-nil, the four §6.10 oracles run after every GC cycle event.

Defaults to t when the `NELISP_GC_VERIFY_DEBUG' environment
variable is set at load time, otherwise nil.  *Production builds
must keep this nil* — the oracles are O(N) in heap size and would
double the GC pause budget if always on.

Toggling at runtime is supported: `nelisp-gc-verify-hooks-enable'
installs the advice when the flag becomes non-nil and
`nelisp-gc-verify-hooks-disable' removes it.  The defcustom
setter wires both for you so plain `setq' / Customize edits work
without a manual call."
  :type 'boolean
  :group 'nelisp-gc-verify-hooks
  :set (lambda (sym val)
         (set-default sym val)
         ;; Guard with `fboundp' because this :set runs during the
         ;; first `custom-declare-variable' pass, *before* the
         ;; `nelisp-gc-verify-hooks-{en,dis}able' defuns are read
         ;; further down in this same file.  Subsequent setq /
         ;; Customize edits land after those defuns and wire correctly.
         (cond
          ((and val (fboundp 'nelisp-gc-verify-hooks-enable))
           (nelisp-gc-verify-hooks-enable))
          ((and (not val) (fboundp 'nelisp-gc-verify-hooks-disable))
           (nelisp-gc-verify-hooks-disable)))))

(defcustom nelisp-gc-verify-debug-double-scan nil
  "When non-nil *and* `nelisp-gc-verify-debug-mode' is on, run double-scan.

The double-scan oracle re-runs `nelisp-gc-inner-run-mark-phase'
to compare the two mark states for byte-equality (Doc 30 v2 §6.10
oracle #4).  This roughly doubles mark-phase pause time, so even
in debug builds we keep it opt-in: enable only for a dedicated
non-determinism hunt, not as a soak default.

Honoured at hook-call time, not load time, so toggling between
soak iterations is cheap."
  :type 'boolean
  :group 'nelisp-gc-verify-hooks)

(defcustom nelisp-gc-verify-debug-severity 'log
  "Action taken when an oracle reports a violation in debug mode.

  `log'    — silent log only (newest entry at head of
             `nelisp-gc-verify-debug-log').  Use for soak / canary
             runs that should keep going on first divergence so the
             full session pattern can be analyzed.

  `signal' — log + `error' so the offending GC cycle aborts.  Use
             for ERTs and for live debugging where the first hit is
             the one you want a backtrace on."
  :type '(choice (const :tag "Log silently" log)
                 (const :tag "Log + error" signal))
  :group 'nelisp-gc-verify-hooks)

(defcustom nelisp-gc-verify-debug-log-cap 256
  "Maximum entries retained in `nelisp-gc-verify-debug-log'.

When the log exceeds this many entries, the oldest are dropped
(ring-buffer truncation).  256 covers a few thousand GC cycles
worth of *clean* runs (where no entry is added); a soak that
catches a real defect typically reports the first 5-10 hits and
stops — far below the cap."
  :type 'integer
  :group 'nelisp-gc-verify-hooks)


;;; §2. Session-wide log + bookkeeping ----------------------------

(defvar nelisp-gc-verify-debug-log nil
  "Session-wide list of `(:event SYM :violation PLIST :time FLOAT)' records.

Newest entries at the head (push/nreverse pattern).  Reset by
`nelisp-gc-verify-debug-log-clear'.  Capped to
`nelisp-gc-verify-debug-log-cap' entries — older records are
dropped silently when the cap is exceeded.")

(defvar nelisp-gc-verify-debug-event-counts
  (make-hash-table :test 'eq)
  "Hash of `EVENT-SYM → INT' counting how many times each oracle ran.

Reset by `nelisp-gc-verify-debug-log-clear'.  Used by the ERTs to
verify hooks fire exactly once per cycle event when debug-mode is
on, and zero times when it is off.")

(defvar nelisp-gc-verify-hooks--in-flight nil
  "Recursion guard: t while an oracle is already running.

Prevents the double-scan oracle from re-invoking itself when the
nested `run-mark-phase' triggers the same hook.  Also a safety
net against future refactors that might cause sweep / promotion /
flip to recursively call into themselves under exotic test
fixtures.")

(defvar nelisp-gc-verify-hooks--installed nil
  "Non-nil when the four pieces of advice are currently in place.")

(defun nelisp-gc-verify-debug-log-clear ()
  "Reset both the violation log and the per-event counters.
Call between independent ERT runs / debug sessions to ensure a
fresh window without cross-test contamination."
  (setq nelisp-gc-verify-debug-log nil)
  (clrhash nelisp-gc-verify-debug-event-counts))

(defun nelisp-gc-verify-debug-log-count ()
  "Return the number of violation records currently in the log."
  (length nelisp-gc-verify-debug-log))

(defun nelisp-gc-verify-debug-event-count (event)
  "Return how many times oracle EVENT has run since the last clear.
EVENT is one of `verify-heap', `verify-card-table',
`verify-poison-from-space', `verify-double-scan'."
  (gethash event nelisp-gc-verify-debug-event-counts 0))

(defun nelisp-gc-verify-hooks--bump-event (event)
  "Increment the per-event counter for EVENT.  Internal."
  (puthash event
           (1+ (gethash event nelisp-gc-verify-debug-event-counts 0))
           nelisp-gc-verify-debug-event-counts))

(defun nelisp-gc-verify-hooks--record (event violation)
  "Append a `(EVENT . VIOLATION)' record onto the debug log.

Honours `nelisp-gc-verify-debug-log-cap' (oldest entry is dropped
when the cap is reached) and dispatches on
`nelisp-gc-verify-debug-severity' (`log' just records,
`signal' raises an `error' so the GC cycle aborts).  Internal."
  (let ((rec (list :event event
                   :violation violation
                   :time (float-time))))
    (push rec nelisp-gc-verify-debug-log)
    ;; Ring-buffer truncation.  `nbutlast' on a list shorter than the
    ;; cap is a no-op so we don't pre-check length.
    (let ((excess (- (length nelisp-gc-verify-debug-log)
                     nelisp-gc-verify-debug-log-cap)))
      (when (> excess 0)
        (setq nelisp-gc-verify-debug-log
              (nbutlast nelisp-gc-verify-debug-log excess))))
    (when (eq nelisp-gc-verify-debug-severity 'signal)
      (error "nelisp-gc-verify [%s] violation: %s"
             event (or (plist-get violation :reason)
                       "unknown")))))


;;; §3. Per-event hook bodies -------------------------------------
;;
;; Each hook is a thin wrapper: bump the per-event counter, call the
;; oracle, push any violation onto the log.  All advice is `:after'
;; (or `:after-while' for one case) so the original return value is
;; preserved verbatim — no observable behaviour change for production
;; callers other than the (opt-in) per-cycle witness work.

(defun nelisp-gc-verify-hooks--after-sweep (heap-regions
                                            &optional _finalizer-table)
  "Advice on `nelisp-gc-inner-run-sweep-phase' — runs verify-heap.

HEAP-REGIONS is the same list the sweep just walked, so the
post-sweep state we verify exactly reflects what the cycle just
produced.  Skipped when the recursion guard is set or debug-mode
is off (the latter is also gated at advice-install time, but the
runtime check makes a hot toggle of the flag immediate-effect)."
  (when (and nelisp-gc-verify-debug-mode
             (not nelisp-gc-verify-hooks--in-flight))
    (let ((nelisp-gc-verify-hooks--in-flight t))
      (nelisp-gc-verify-hooks--bump-event 'verify-heap)
      (let* ((result (nelisp-gc-verify-heap (or heap-regions nil)))
             (violations (plist-get result :violations)))
        (dolist (v violations)
          (nelisp-gc-verify-hooks--record 'verify-heap v))))))

(defun nelisp-gc-verify-hooks--after-promotion
    (semi tenured card-table _root-set _from-fn _tenured-fn)
  "Advice on `nelisp-gc-inner-run-minor-gc-with-promotion' → verify-card-table.

After the minor GC + promotion pipeline completes, the card-table
should still cover every cross-gen pointer the just-promoted
objects retain — verify it.  Skipped when any of SEMI / TENURED /
CARD-TABLE is nil (the partial-init paths the wrapper itself
tolerates) or when debug-mode is off."
  (when (and nelisp-gc-verify-debug-mode
             (not nelisp-gc-verify-hooks--in-flight)
             card-table
             tenured
             semi)
    (let ((nelisp-gc-verify-hooks--in-flight t))
      (nelisp-gc-verify-hooks--bump-event 'verify-card-table)
      ;; The tenured allocator state has its own region descriptor; we
      ;; lift it from the card-table itself (which carries a `:region'
      ;; slot per `nelisp-gc-inner--card-table' struct, set to the
      ;; tenured region at init time).  Nursery descriptor comes from
      ;; the semispace's current from-space.
      (let ((tenured-region
             (nelisp-gc-inner--card-table-region card-table))
            (nursery-region
             (nelisp-gc-inner--semispace-from-space semi)))
        (let ((v (nelisp-gc-verify-card-table
                  card-table tenured-region nursery-region)))
          (when v
            (nelisp-gc-verify-hooks--record 'verify-card-table v)))))))

(defun nelisp-gc-verify-hooks--after-flip (_semi)
  "Advice on `nelisp-gc-inner-flip-semispace' — runs verify-poison-from-space.

The oracle reads `nelisp-gc-verify--poison-regions' (set up by
mutator code via `verify-poison-mark' / `-touch' on the now-dead
half) and reports any from-space read after the flip.  We do not
auto-poison-mark here — that policy stays with the caller so the
oracle's witnessing semantics aren't accidentally widened."
  (when (and nelisp-gc-verify-debug-mode
             (not nelisp-gc-verify-hooks--in-flight))
    (let ((nelisp-gc-verify-hooks--in-flight t))
      (nelisp-gc-verify-hooks--bump-event 'verify-poison-from-space)
      (let ((v (nelisp-gc-verify-poison-from-space)))
        (when v
          (nelisp-gc-verify-hooks--record
           'verify-poison-from-space v))))))

(defun nelisp-gc-verify-hooks--after-mark (root-set heap-regions)
  "Advice on `nelisp-gc-inner-run-mark-phase' — opt-in verify-double-scan.

Re-runs the mark phase a second time and compares — only fires
when both `nelisp-gc-verify-debug-mode' and
`nelisp-gc-verify-debug-double-scan' are on.  The recursion guard
keeps the inner re-run from re-triggering the oracle."
  (when (and nelisp-gc-verify-debug-mode
             nelisp-gc-verify-debug-double-scan
             (not nelisp-gc-verify-hooks--in-flight))
    (let ((nelisp-gc-verify-hooks--in-flight t))
      (nelisp-gc-verify-hooks--bump-event 'verify-double-scan)
      (let ((v (nelisp-gc-verify-double-scan
                root-set heap-regions)))
        (when v
          (nelisp-gc-verify-hooks--record 'verify-double-scan v))))))


;;; §4. Install / uninstall the four pieces of advice -------------

(defun nelisp-gc-verify-hooks-enable ()
  "Install the four pieces of advice that wire oracles into the GC cycle.

Idempotent — calling twice is harmless.  See the `Commentary'
section for the event → oracle mapping.

Note: this honours `nelisp-gc-verify-debug-mode' at *call time*
of each hook, not at install time, so toggling the defcustom off
after install just makes the hooks no-op — no need to call
`nelisp-gc-verify-hooks-disable' for that purpose.  Disable is for
when you want to fully remove the advice (e.g. before a
`unload-feature' cycle in a debug REPL)."
  (unless nelisp-gc-verify-hooks--installed
    (advice-add 'nelisp-gc-inner-run-sweep-phase
                :after #'nelisp-gc-verify-hooks--after-sweep)
    (advice-add 'nelisp-gc-inner-run-minor-gc-with-promotion
                :after #'nelisp-gc-verify-hooks--after-promotion)
    (advice-add 'nelisp-gc-inner-flip-semispace
                :after #'nelisp-gc-verify-hooks--after-flip)
    (advice-add 'nelisp-gc-inner-run-mark-phase
                :after #'nelisp-gc-verify-hooks--after-mark)
    (setq nelisp-gc-verify-hooks--installed t)))

(defun nelisp-gc-verify-hooks-disable ()
  "Remove every piece of advice installed by `-enable'.
Idempotent — safe to call when never installed."
  (when nelisp-gc-verify-hooks--installed
    (advice-remove 'nelisp-gc-inner-run-sweep-phase
                   #'nelisp-gc-verify-hooks--after-sweep)
    (advice-remove 'nelisp-gc-inner-run-minor-gc-with-promotion
                   #'nelisp-gc-verify-hooks--after-promotion)
    (advice-remove 'nelisp-gc-inner-flip-semispace
                   #'nelisp-gc-verify-hooks--after-flip)
    (advice-remove 'nelisp-gc-inner-run-mark-phase
                   #'nelisp-gc-verify-hooks--after-mark)
    (setq nelisp-gc-verify-hooks--installed nil)))


;;; §5. Boot-time auto-install on env-var ------------------------

;; If the env var is set at load time, install advice immediately.
;; (The defcustom setter runs `-enable' too, but the setter only
;; fires for setq/Customize after this file is loaded; the env-var
;; case needs the explicit boot-time call so the hooks are live by
;; the time the first GC cycle runs.)
(when nelisp-gc-verify-debug-mode
  (nelisp-gc-verify-hooks-enable))


(provide 'nelisp-gc-verify-hooks)
;;; nelisp-gc-verify-hooks.el ends here
