;;; nelisp-textprop-hook.el --- Text-property + overlay change hook fan-out (Phase 9c.5)  -*- lexical-binding: t; -*-

;; Phase 9c.5 per Doc 41 LOCKED-2026-04-25-v2 §3.5 / §2.8 / §4.6.
;; Layer: Phase 9c text-property advanced — final sub-phase (= sister
;; module of T138 `nelisp-emacs-compat-face' / T144
;; `nelisp-textprop-display' / T146 `nelisp-textprop-keymap' / T150
;; `nelisp-overlay').
;;
;; Goal: provide the *modification + point-motion + overlay change
;; hook fan-out machinery* — the per-buffer registry, the depth=1
;; re-entrancy guard counters, the suppress macro, and the public
;; trigger entry point — so that downstream subscribers (mutation
;; events, cursor moves, overlay changes) can register handlers via
;; one stable contract without re-implementing the guard logic.
;;
;; Contract: =TEXT_PROPERTY_HOOK_CONTRACT_VERSION = 1=
;;           (Doc 41 §3.5.1 + §4.6 LOCKED v1).
;;
;; Naming convention: this module uses the prefix
;; `nelisp-textprop-hook-' for every public function/variable.  Doc
;; 41 §3.5 mentions a candidate file name `nelisp-emacs-compat-tp-hooks.el',
;; but the actual T154 ship target is `nelisp-textprop-hook.el' for
;; symmetry with the four sibling Phase 9c modules.
;;
;; KIND domain (Doc 41 §4.6 LOCKED v1):
;;   - `modification'    — `modification-hooks' text-property fan-out
;;                         (range overlap mutation)
;;   - `insert-in-front' — `insert-in-front-hooks' text-property fan-out
;;                         (mutation start = range start)
;;   - `insert-behind'   — `insert-behind-hooks' text-property fan-out
;;                         (mutation end   = range end)
;;   - `before-change'   — pre-mutation broadcast (= Emacs
;;                         `before-change-functions')
;;   - `after-change'    — post-mutation broadcast (= Emacs
;;                         `after-change-functions')
;;   - `point-motion'    — `point-entered' / `point-left' fan-out
;;                         (cursor position change driven, Doc 41
;;                         §3.5.2 / Doc 34 §2.4 event-loop subscriber)
;;
;; Re-entrancy model (Doc 41 §2.8 + §3.5.4 LOCKED v1):
;;   - depth=1 secondary trigger suppression — when a registered
;;     handler itself causes another mutation in the *same buffer*,
;;     that nested mutation does NOT re-fire the same hook *kind*.
;;   - per-buffer counter, independent across the 3 hook *categories*:
;;       modification     (modification, insert-in-front, insert-behind)
;;       point-motion     (point-motion)
;;       overlay-change   (= overlay-change-functions, registered via
;;                          KIND = modification but trigger context
;;                          discriminated by the public trigger API
;;                          contract — guarded together with text-prop
;;                          modification per Doc 41 §4.6)
;;   - cross-buffer mutation is NOT suppressed (= each buffer carries
;;     its own counter), Doc 41 §2.8 LOCKED v1.
;;   - signal-safe: depth counter is restored via `unwind-protect' so
;;     a thrown error inside a handler doesn't leave the guard pinned.
;;   - global guard variables (`inhibit-modification-hooks' /
;;     `inhibit-point-motion-hooks') override the per-buffer counter:
;;     when bound non-nil, the trigger entry returns immediately
;;     without fan-out.  These mirror Emacs behaviour (let-bind for
;;     scoped suppression, dynamic binding semantics).
;;
;; Public API (8 functions + 1 macro + 2 vars + 1 const, all
;;             `nelisp-textprop-hook-' prefix):
;;
;;   Registration:
;;     `nelisp-textprop-hook-add'        KIND CALLBACK    -> CALLBACK
;;     `nelisp-textprop-hook-remove'     KIND CALLBACK    -> CALLBACK or nil
;;     `nelisp-textprop-hook-handlers'   KIND             -> LIST
;;     `nelisp-textprop-hook-clear-all'  ()               -> t
;;
;;   Trigger / fan-out:
;;     `nelisp-textprop-hook-run'        KIND BUF BEG END &rest ARGS -> t or nil
;;
;;   Re-entrancy / suppression:
;;     `nelisp-textprop-hook-with-suppressed' (KIND) BODY... (macro)
;;     `nelisp-textprop-hook-depth'      KIND &optional BUF -> NON-NEG-INT
;;     `nelisp-textprop-hook-suppressed-p' KIND &optional BUF -> bool
;;
;;   Variables (dynamic, global suppression):
;;     `inhibit-modification-hooks'   — t suppresses modification +
;;                                      insert-in-front + insert-behind +
;;                                      before-change + after-change
;;                                      fan-out (Emacs precedent)
;;     `inhibit-point-motion-hooks'   — t suppresses point-motion fan-out
;;
;;   Contract version (Doc 41 §3.5.1 + §4.6 LOCKED v1):
;;     `nelisp-textprop-hook-contract-version' = 1
;;
;; Contract invariants (Doc 41 §2.8 + §3.5.4 LOCKED v1):
;;   - depth=1 guard fires once per (BUF . CATEGORY) — recursion
;;     within the same buffer + category is suppressed.
;;   - cross-buffer mutation triggers the other buffer's hooks
;;     normally (= per-buffer counter, no global suppression).
;;   - thrown signal inside a handler must reset the counter back to
;;     its pre-trigger value (= signal-safe unwind).
;;   - the 3 categories (modification / point-motion / overlay) carry
;;     independent counters; trigger of one does not suppress the other.
;;   - `inhibit-*' let-bind to nil restores fan-out within scope.
;;
;; Non-goals (deferred to Phase 11 backend / event-loop integration):
;;   - actual cursor motion event source subscription (= Doc 34 §2.4
;;     event-loop / Doc 39 §2.4 — Phase 11 wires the trigger sites).
;;   - text mutation primitive integration (= Phase 8 mutable text
;;     primitive emits the trigger; this module owns fan-out only).
;;   - actual rendering of `display' / `face' / `before-string' /
;;     `after-string' overlay payloads (= Phase 11).

;;; Code:

(require 'cl-lib)

;;; ─────────────────────────────────────────────────────────────────────
;;; Errors
;;; ─────────────────────────────────────────────────────────────────────

(define-error 'nelisp-textprop-hook-error
  "NeLisp text-property / overlay hook error")
(define-error 'nelisp-textprop-hook-bad-kind
  "Unknown text-property hook KIND" 'nelisp-textprop-hook-error)
(define-error 'nelisp-textprop-hook-bad-callback
  "Hook CALLBACK must be a function" 'nelisp-textprop-hook-error)
(define-error 'nelisp-textprop-hook-bad-buffer
  "Hook BUF must be a live buffer" 'nelisp-textprop-hook-error)

;;; ─────────────────────────────────────────────────────────────────────
;;; Contract version (Doc 41 §3.5.1 + §4.6 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defconst nelisp-textprop-hook-contract-version 1
  "Doc 41 §3.5.1 + §4.6 LOCKED contract version for the hook layer.
Increment only via a new Doc 41 LOCKED revision.")

;;; ─────────────────────────────────────────────────────────────────────
;;; KIND domain + category mapping (Doc 41 §3.5.1-3.5.3 + §4.6 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defconst nelisp-textprop-hook--kinds
  '(modification insert-in-front insert-behind
                 before-change after-change
                 point-motion)
  "Closed enumeration of accepted KIND symbols.

Doc 41 §3.5.1 (modification / insert-in-front / insert-behind),
§3.5.2 (point-motion), and the two Emacs-precedent broadcast
hooks (before-change / after-change) that share the
modification category guard.

`nelisp-textprop-hook-add' / `-remove' / `-run' all signal
`nelisp-textprop-hook-bad-kind' for any symbol not in this list.")

(defconst nelisp-textprop-hook--category-of
  '((modification     . modification)
    (insert-in-front  . modification)
    (insert-behind    . modification)
    (before-change    . modification)
    (after-change     . modification)
    (point-motion     . point-motion))
  "KIND → CATEGORY map (Doc 41 §2.8 LOCKED v1).

CATEGORY drives the per-buffer depth-counter scope: KINDs in
the same CATEGORY share a counter, KINDs across CATEGORYs do
NOT (= independent counters per Doc 41 §3.5.4 / §4.6 LOCKED v1).

The current implementation defines two categories — `modification'
(text-prop modification + insert-in-front + insert-behind +
broadcast before/after-change) and `point-motion' — matching
the two `inhibit-*' guard variables exposed by the Emacs API.

A future overlay-change-functions wiring (= Doc 41 §3.5.3) would
re-use the `modification' category counter so that overlay
fan-out invoked from inside a text-prop handler is suppressed
(per Doc 41 §2.8 + §4.6 modification-vs-overlay grouping).")

(defun nelisp-textprop-hook--category (kind)
  "Resolve KIND to its CATEGORY symbol or signal `nelisp-textprop-hook-bad-kind'."
  (or (cdr (assq kind nelisp-textprop-hook--category-of))
      (signal 'nelisp-textprop-hook-bad-kind (list kind))))

(defun nelisp-textprop-hook--check-kind (kind)
  "Signal `nelisp-textprop-hook-bad-kind' unless KIND is a recognised symbol."
  (unless (memq kind nelisp-textprop-hook--kinds)
    (signal 'nelisp-textprop-hook-bad-kind (list kind))))

(defun nelisp-textprop-hook--check-callback (cb)
  "Signal `nelisp-textprop-hook-bad-callback' unless CB is callable."
  (unless (functionp cb)
    (signal 'nelisp-textprop-hook-bad-callback (list cb))))

(defun nelisp-textprop-hook--check-buffer (buf)
  "Signal `nelisp-textprop-hook-bad-buffer' unless BUF is a live buffer."
  (unless (and (bufferp buf) (buffer-live-p buf))
    (signal 'nelisp-textprop-hook-bad-buffer (list buf))))

;;; ─────────────────────────────────────────────────────────────────────
;;; Global guard variables (Doc 41 §2.8 + §4.6 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defvar inhibit-modification-hooks nil
  "Non-nil suppresses `modification' / `insert-in-front' / `insert-behind' /
`before-change' / `after-change' fan-out globally (= across all buffers).

Doc 41 §2.8 LOCKED v1: dynamic binding (`let') restores the
ambient value on scope exit; `setq' direct-write is undefined
behaviour with respect to the per-buffer depth counter.

Mirrors Emacs `inhibit-modification-hooks' precedent.")

;; Vanilla Emacs 25.1+ marks `inhibit-point-motion-hooks' obsolete in
;; favour of `cursor-intangible-mode' / `cursor-sensor-mode'; NeLisp
;; Phase 9c.5 still ships the variable per Doc 41 §3.5.2 + §4.6 LOCKED
;; v1 contract (= it is the public guard for `point-motion' fan-out).
;; The byte-compile-error-on-warn build would otherwise reject the
;; defvar / let-bind sites — silence the deprecation locally.
(with-suppressed-warnings ((obsolete inhibit-point-motion-hooks))
  (defvar inhibit-point-motion-hooks nil
    "Non-nil suppresses `point-motion' fan-out globally.

Doc 41 §3.5.2 + §4.6 LOCKED v1.  Same dynamic-binding semantics
as `inhibit-modification-hooks'."))

(defun nelisp-textprop-hook--global-inhibited-p (category)
  "Return non-nil if global guard variable for CATEGORY is set."
  (cond
   ((eq category 'modification) inhibit-modification-hooks)
   ((eq category 'point-motion)
    (with-suppressed-warnings ((obsolete inhibit-point-motion-hooks))
      inhibit-point-motion-hooks))
   (t nil)))

;;; ─────────────────────────────────────────────────────────────────────
;;; Handler registry — per-KIND callback lists (process-global)
;;; ─────────────────────────────────────────────────────────────────────

(defvar nelisp-textprop-hook--handlers (make-hash-table :test 'eq)
  "KIND symbol → list of registered callbacks.

Process-global, not per-buffer: the *trigger* carries the BUFFER
argument, so registration scope is hook-kind level.  The list is
ordered by registration time (= add-order), and `run' walks it
left-to-right preserving that order (Emacs `run-hook-with-args'
precedent).")

(defun nelisp-textprop-hook--handlers-of (kind)
  "Return KIND's callback list (live cell, never a copy)."
  (gethash kind nelisp-textprop-hook--handlers))

(defun nelisp-textprop-hook--set-handlers-of (kind list)
  "Store LIST as KIND's callback list (or remove the entry if LIST is nil)."
  (if (null list)
      (remhash kind nelisp-textprop-hook--handlers)
    (puthash kind list nelisp-textprop-hook--handlers))
  list)

;;; ─────────────────────────────────────────────────────────────────────
;;; Per-buffer depth counter (Doc 41 §2.8 + §3.5.4 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defvar nelisp-textprop-hook--depth
  (make-hash-table :test 'equal)
  "(BUFFER . CATEGORY) → non-negative integer depth counter.

`equal'-keyed because the key is a freshly consed cons cell.
Buffer entries are reaped on `kill-buffer' via the
`kill-buffer-hook' wired by `nelisp-textprop-hook--install-cleanup'.

Counter scope (Doc 41 §3.5.4 LOCKED v1):
  - depth = 0 → not currently inside a fan-out for that
                (buf . category)
  - depth >= 1 → inside a fan-out, secondary triggers for the
                same (buf . category) are suppressed.

Independent counters per CATEGORY ensure that a `modification'
handler can still trigger `point-motion' fan-out in the same
buffer (and vice versa) without spurious suppression.")

(defun nelisp-textprop-hook--depth-key (buf category)
  "Build the registry key for BUF + CATEGORY."
  (cons buf category))

(defun nelisp-textprop-hook-depth (kind &optional buf)
  "Return the current depth counter for KIND in BUF (default current buffer).

KIND is mapped to its CATEGORY for counter lookup, so `point-motion'
and `modification' carry independent counts.

MCP Parameters:
  KIND — registered hook kind symbol
  BUF  — host buffer or nil = current"
  (nelisp-textprop-hook--check-kind kind)
  (let ((cat (nelisp-textprop-hook--category kind))
        (b (or buf (current-buffer))))
    (gethash (nelisp-textprop-hook--depth-key b cat)
             nelisp-textprop-hook--depth 0)))

(defun nelisp-textprop-hook--depth-bump (buf category)
  "Increment BUF + CATEGORY depth counter and return the new value."
  (let* ((k (nelisp-textprop-hook--depth-key buf category))
         (n (1+ (gethash k nelisp-textprop-hook--depth 0))))
    (puthash k n nelisp-textprop-hook--depth)
    n))

(defun nelisp-textprop-hook--depth-drop (buf category)
  "Decrement BUF + CATEGORY depth counter; remove entry when it hits 0."
  (let* ((k (nelisp-textprop-hook--depth-key buf category))
         (cur (gethash k nelisp-textprop-hook--depth 0))
         (n (max 0 (1- cur))))
    (if (zerop n)
        (remhash k nelisp-textprop-hook--depth)
      (puthash k n nelisp-textprop-hook--depth))
    n))

(defun nelisp-textprop-hook-suppressed-p (kind &optional buf)
  "Return non-nil if KIND fan-out in BUF is currently suppressed.

A KIND is suppressed when either:
  - the global guard variable for its CATEGORY is non-nil, or
  - the per-buffer depth counter for that CATEGORY is >= 1
    (= we are already inside a fan-out for the same CATEGORY).

MCP Parameters:
  KIND — registered hook kind symbol
  BUF  — host buffer or nil = current"
  (nelisp-textprop-hook--check-kind kind)
  (let ((cat (nelisp-textprop-hook--category kind))
        (b (or buf (current-buffer))))
    (or (nelisp-textprop-hook--global-inhibited-p cat)
        (>= (gethash (nelisp-textprop-hook--depth-key b cat)
                     nelisp-textprop-hook--depth 0)
            1))))

;;; ─────────────────────────────────────────────────────────────────────
;;; kill-buffer cleanup — drop per-buffer depth counter rows
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-hook--cleanup-killed-buffer ()
  "Drop depth-counter rows for the buffer being killed.

Wired into `kill-buffer-hook' so the counter table never leaks
rows for dead buffers.  Iterates the table (small N in practice)
and removes any key whose `car' is the dying buffer."
  (let* ((dying (current-buffer))
         (victims nil))
    (maphash
     (lambda (k _v)
       (when (and (consp k) (eq (car k) dying))
         (push k victims)))
     nelisp-textprop-hook--depth)
    (dolist (k victims)
      (remhash k nelisp-textprop-hook--depth))))

(defun nelisp-textprop-hook--install-cleanup ()
  "Install the kill-buffer cleanup hook globally.

Idempotent — `add-hook' deduplicates."
  (add-hook 'kill-buffer-hook
            #'nelisp-textprop-hook--cleanup-killed-buffer))

;; Auto-install at load time; tests can `remove-hook' if needed.
(nelisp-textprop-hook--install-cleanup)

;;; ─────────────────────────────────────────────────────────────────────
;;; Public registration API (Doc 41 §3.5 + §4.6 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-hook-add (kind callback)
  "Register CALLBACK as a handler for KIND fan-out events.

KIND must be one of the symbols enumerated in
`nelisp-textprop-hook--kinds'.  Re-adding a CALLBACK that is
already registered for KIND is a no-op (= deduplicated by
`equal' identity to mirror `add-hook' precedent).

Returns CALLBACK on success.

MCP Parameters:
  KIND     — symbol from `nelisp-textprop-hook--kinds'
  CALLBACK — function called as
             (CALLBACK BUF BEG END . ARGS)
             with the trigger's (BEG END . ARGS) payload"
  (nelisp-textprop-hook--check-kind kind)
  (nelisp-textprop-hook--check-callback callback)
  (let ((lst (nelisp-textprop-hook--handlers-of kind)))
    (unless (member callback lst)
      (nelisp-textprop-hook--set-handlers-of
       kind (append lst (list callback)))))
  callback)

(defun nelisp-textprop-hook-remove (kind callback)
  "Remove CALLBACK from KIND's handler list.

Returns CALLBACK when a removal happened, nil otherwise (= no-op
when CALLBACK was not registered).  Idempotent.

MCP Parameters:
  KIND     — symbol from `nelisp-textprop-hook--kinds'
  CALLBACK — function previously registered via
             `nelisp-textprop-hook-add'"
  (nelisp-textprop-hook--check-kind kind)
  (let* ((old (nelisp-textprop-hook--handlers-of kind))
         (new (and old (delete callback (copy-sequence old)))))
    (cond
     ((null old) nil)
     ((equal old new) nil)
     (t
      (nelisp-textprop-hook--set-handlers-of kind new)
      callback))))

(defun nelisp-textprop-hook-handlers (kind)
  "Return a fresh copy of KIND's registered handler list (callbacks).

The result is a copy, so mutating it does NOT affect the
internal registry.  Returns nil for KIND that has no handler.

MCP Parameters:
  KIND — symbol from `nelisp-textprop-hook--kinds'"
  (nelisp-textprop-hook--check-kind kind)
  (copy-sequence (nelisp-textprop-hook--handlers-of kind)))

(defun nelisp-textprop-hook-clear-all ()
  "Remove every registered handler for every KIND, and reset all depth counters.

Test hygiene helper — production code should prefer per-KIND
`nelisp-textprop-hook-remove'.  Returns t.

MCP Parameters: (none)"
  (clrhash nelisp-textprop-hook--handlers)
  (clrhash nelisp-textprop-hook--depth)
  t)

;;; ─────────────────────────────────────────────────────────────────────
;;; Public trigger entry (Doc 41 §2.8 + §3.5.4 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-hook-run (kind buf beg end &rest args)
  "Fan-out KIND's handlers with payload (BUF BEG END . ARGS).

Walks `nelisp-textprop-hook--handlers-of' KIND in registration
order and calls each with the same payload.  Honours the depth=1
secondary-trigger guard (Doc 41 §2.8 + §3.5.4 LOCKED v1):
  - returns nil immediately when the global guard variable for
    KIND's CATEGORY is non-nil
  - returns nil immediately when the per-buffer depth counter
    for (BUF . CATEGORY) is already >= 1
  - otherwise bumps the counter, runs the handlers, and drops
    the counter on exit (signal-safe via `unwind-protect')

Cross-buffer mutation: if a handler triggers fan-out on a
*different* BUF, that other buffer's counter is independent (=
not suppressed), per Doc 41 §2.8 LOCKED v1.

Returns t when fan-out actually ran, nil when it was suppressed.

MCP Parameters:
  KIND — symbol from `nelisp-textprop-hook--kinds'
  BUF  — host buffer the mutation belongs to
  BEG  — integer position (mutation start)
  END  — integer position (mutation end)
  ARGS — extra payload pieces (length / inserted text / etc.)"
  (nelisp-textprop-hook--check-kind kind)
  (nelisp-textprop-hook--check-buffer buf)
  (let* ((cat (nelisp-textprop-hook--category kind))
         (handlers (nelisp-textprop-hook--handlers-of kind)))
    (cond
     ((null handlers) nil)
     ((nelisp-textprop-hook--global-inhibited-p cat) nil)
     ((>= (gethash (nelisp-textprop-hook--depth-key buf cat)
                   nelisp-textprop-hook--depth 0)
          1)
      nil)
     (t
      (nelisp-textprop-hook--depth-bump buf cat)
      (unwind-protect
          (progn
            (dolist (cb handlers)
              (apply cb buf beg end args))
            t)
        (nelisp-textprop-hook--depth-drop buf cat))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; Suppression macro (Doc 41 §3.5.4 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defmacro nelisp-textprop-hook-with-suppressed (kind &rest body)
  "Evaluate BODY with KIND's fan-out globally suppressed.

KIND is evaluated at runtime; its CATEGORY decides which guard
variable is let-bound:
  - `modification' CATEGORY → `inhibit-modification-hooks' = t
  - `point-motion' CATEGORY → `inhibit-point-motion-hooks' = t

The let-bind is dynamic (= scoped), so any `setq' the body
performs on the guard variable is ignored on scope exit (Emacs
precedent / Doc 41 §2.8 LOCKED v1).

Returns the value of BODY's last form.

Usage parallels `with-suppressed-modification-hooks' in vanilla
Emacs but accepts the broader Doc 41 KIND domain.

MCP Parameters:
  KIND — runtime-evaluated symbol from `nelisp-textprop-hook--kinds'
  BODY — forms to evaluate with suppression in effect"
  (declare (indent 1) (debug (form body)))
  (let ((kind-sym (make-symbol "kind"))
        (cat-sym  (make-symbol "cat")))
    `(let* ((,kind-sym ,kind)
            (,cat-sym  (nelisp-textprop-hook--category ,kind-sym)))
       (cond
        ((eq ,cat-sym 'modification)
         (let ((inhibit-modification-hooks t))
           ,@body))
        ((eq ,cat-sym 'point-motion)
         (with-suppressed-warnings ((obsolete inhibit-point-motion-hooks))
           (let ((inhibit-point-motion-hooks t))
             ,@body)))
        (t
         (signal 'nelisp-textprop-hook-bad-kind (list ,kind-sym)))))))

(provide 'nelisp-textprop-hook)

;;; nelisp-textprop-hook.el ends here
