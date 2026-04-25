;;; nelisp-textprop-keymap.el --- Text-property keymap chain inject (Phase 9c.3)  -*- lexical-binding: t; -*-

;; Phase 9c.3 per Doc 41 LOCKED-2026-04-25-v2 §3.3 / §2.5.
;; Layer: Phase 9c text-property advanced (sister module of T138
;; `nelisp-emacs-compat-face' / T144 `nelisp-textprop-display').
;;
;; Goal: provide the *keymap chain provider* that injects two new
;; precedence slots — overlay keymap (slot 6) and text-property
;; keymap (slot 7) — on top of the Doc 34 §2.3 LOCKED 7-stage
;; keymap precedence chain, while preserving Doc 34 SHIPPED bin
;; byte-identical behaviour by default (= opt-in via
;; `nelisp-textprop-keymap-with-injection').
;;
;; Contract: =KEYMAP_CHAIN_INJECT_CONTRACT_VERSION = 1=
;;           (Doc 41 §2.5 LOCKED v1).
;;
;; Provider model (decoupled from T148 overlay impl + T147 hook impl):
;;   - This module owns *the precedence assembly* — it does NOT own
;;     the overlay storage layer (= Doc 41 §3.4 / T148) or the
;;     text-property hook layer (= Doc 41 §3.5 / T147).
;;   - The two *injectable slots* (overlay slot 6, text-property
;;     slot 7) are filled by *provider functions* of the form
;;       (PROVIDER POS BUFFER) -> KEYMAP-or-nil
;;     installed via `nelisp-textprop-keymap-set-overlay-provider' /
;;     `nelisp-textprop-keymap-set-textprop-provider'.
;;   - Default providers return nil (= no inject) so 9c.3 can ship
;;     before 9c.4 (overlay) without the chain ever returning a
;;     stale or fictional keymap.
;;
;; Public API (8 functions + 2 vars + 1 const, all
;;             `nelisp-textprop-keymap-' prefix):
;;
;;   Install / opt-in:
;;     `nelisp-textprop-keymap-install'             ()       -> t
;;     `nelisp-textprop-keymap-uninstall'           ()       -> t
;;     `nelisp-textprop-keymap-installed-p'         ()       -> bool
;;
;;   Provider hooks (Doc 41 §3.3.1):
;;     `nelisp-textprop-keymap-set-overlay-provider'  FN     -> FN
;;     `nelisp-textprop-keymap-set-textprop-provider' FN     -> FN
;;     `nelisp-textprop-keymap-chain-providers'       ()     -> (OFN TFN)
;;
;;   Lookup (Doc 41 §2.5 + §3.3.2):
;;     `nelisp-textprop-keymap-resolve-at'   POS &optional BUF
;;                                                 -> KEYMAP-or-nil
;;     `nelisp-textprop-keymap-precedence-at' POS BUF
;;                                                 -> ((SLOT . KEYMAP) ...)
;;
;;   Contract version (Doc 41 §2.5 + §4.5 LOCKED v1):
;;     `nelisp-textprop-keymap-chain-inject-contract-version' = 1
;;
;; Precedence (Doc 41 §2.5 LOCKED v1, 9 stages, only when
;; `nelisp-textprop-keymap-with-injection' = t):
;;   1. `overriding-terminal-local-map'
;;   2. `overriding-local-map'
;;   3. `minor-mode-overriding-map-alist'
;;   4. `minor-mode-map-alist'
;;   5. `emulation-mode-map-alists'
;;   6. *overlay keymap at POS*  (Doc 41 9c.4, `keymap' prop > `local-map' prop)
;;   7. *text-property keymap at POS*  (Doc 41 9c.3, this module)
;;   8. `current-local-map'
;;   9. `global-map'
;;
;; v1 → v2 invariant (Doc 41 §2.5):
;;   - injection = nil (default): chain is Doc 34 §2.3 7-stage,
;;     byte-identical with Doc 34 SHIPPED bin
;;   - injection = t: chain is Doc 41 §2.5 9-stage,
;;     `KEYMAP_CHAIN_INJECT_CONTRACT_VERSION = 1' applied
;;
;; Non-goals (deferred):
;;   - overlay storage / overlay tree / overlays-at (= Doc 41 §3.4 / T148)
;;   - text-property change hook (= Doc 41 §3.5 / T147)
;;   - lookup-key / command-execute API (= Doc 34 §4.3 SHIPPED, unchanged)
;;   - actual key-event dispatch (= Doc 34 event-loop SHIPPED, unchanged)

;;; Code:

(require 'cl-lib)

;;; ─────────────────────────────────────────────────────────────────────
;;; Errors
;;; ─────────────────────────────────────────────────────────────────────

(define-error 'nelisp-textprop-keymap-error
  "NeLisp text-property keymap chain error")
(define-error 'nelisp-textprop-keymap-bad-provider
  "Invalid keymap chain provider"
  'nelisp-textprop-keymap-error)

;;; ─────────────────────────────────────────────────────────────────────
;;; Contract version (Doc 41 §2.5 + §4.5 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defconst nelisp-textprop-keymap-chain-inject-contract-version 1
  "Doc 41 §2.5 + §4.5 LOCKED contract version for the keymap chain inject layer.
Increment only via a new Doc 41 LOCKED revision.")

;;; ─────────────────────────────────────────────────────────────────────
;;; Opt-in flag (Doc 41 §2.5 v1 → v2 parallel invariant)
;;; ─────────────────────────────────────────────────────────────────────

(defcustom nelisp-textprop-keymap-with-injection nil
  "Non-nil means inject overlay/text-property keymap slots into the chain.

When nil (default), `nelisp-textprop-keymap-resolve-at' and
`nelisp-textprop-keymap-precedence-at' both return as if the
overlay (slot 6) and text-property (slot 7) injection slots were
empty — i.e. byte-identical with the Doc 34 §2.3 LOCKED 7-stage
keymap chain.

When t, the two injection slots are populated by the registered
overlay / text-property provider functions and the resolver
returns a 9-stage chain per Doc 41 §2.5 LOCKED v1.

Flip via `nelisp-textprop-keymap-install' /
`nelisp-textprop-keymap-uninstall' rather than `setq'."
  :type 'boolean
  :group 'nelisp)

;;; ─────────────────────────────────────────────────────────────────────
;;; Slot indices (Doc 41 §2.5 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defconst nelisp-textprop-keymap-slot-overriding-terminal-local 1)
(defconst nelisp-textprop-keymap-slot-overriding-local 2)
(defconst nelisp-textprop-keymap-slot-minor-mode-overriding 3)
(defconst nelisp-textprop-keymap-slot-minor-mode-map-alist 4)
(defconst nelisp-textprop-keymap-slot-emulation-mode-map-alists 5)
(defconst nelisp-textprop-keymap-slot-overlay 6)
(defconst nelisp-textprop-keymap-slot-textprop 7)
(defconst nelisp-textprop-keymap-slot-current-local 8)
(defconst nelisp-textprop-keymap-slot-global 9)

;;; ─────────────────────────────────────────────────────────────────────
;;; Provider registry (Doc 41 §3.3.1)
;;; ─────────────────────────────────────────────────────────────────────

(defvar nelisp-textprop-keymap--overlay-provider nil
  "Provider for slot 6 (overlay keymap).
A function (POS BUFFER) -> KEYMAP-or-nil, or nil for none.")

(defvar nelisp-textprop-keymap--textprop-provider nil
  "Provider for slot 7 (text-property keymap).
A function (POS BUFFER) -> KEYMAP-or-nil, or nil for none.")

(defun nelisp-textprop-keymap--validate-provider (fn label)
  "Signal `nelisp-textprop-keymap-bad-provider' unless FN is nil or callable.
LABEL is the slot name used in the error string."
  (unless (or (null fn) (functionp fn))
    (signal 'nelisp-textprop-keymap-bad-provider
            (list label fn))))

(defun nelisp-textprop-keymap-set-overlay-provider (fn)
  "Install FN as the slot-6 (overlay) keymap provider, return FN.
FN is called as (FN POS BUFFER) and must return a keymap or nil.
A nil FN clears the provider.

MCP Parameters: FN — callable (POS BUFFER) -> KEYMAP-or-nil, or nil."
  (nelisp-textprop-keymap--validate-provider fn 'overlay)
  (setq nelisp-textprop-keymap--overlay-provider fn)
  fn)

(defun nelisp-textprop-keymap-set-textprop-provider (fn)
  "Install FN as the slot-7 (text-property) keymap provider, return FN.
FN is called as (FN POS BUFFER) and must return a keymap or nil.
A nil FN clears the provider.

MCP Parameters: FN — callable (POS BUFFER) -> KEYMAP-or-nil, or nil."
  (nelisp-textprop-keymap--validate-provider fn 'textprop)
  (setq nelisp-textprop-keymap--textprop-provider fn)
  fn)

(defun nelisp-textprop-keymap-chain-providers ()
  "Return the registered (OVERLAY-PROVIDER . TEXTPROP-PROVIDER) pair.

MCP Parameters: (none)"
  (cons nelisp-textprop-keymap--overlay-provider
        nelisp-textprop-keymap--textprop-provider))

;;; ─────────────────────────────────────────────────────────────────────
;;; Install / opt-in (Doc 41 §2.5 v1 → v2 parallel invariant)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-keymap-installed-p ()
  "Return t when the 9-stage chain is active (= injection enabled).

MCP Parameters: (none)"
  (and nelisp-textprop-keymap-with-injection t))

(defun nelisp-textprop-keymap-install ()
  "Enable the Doc 41 §2.5 9-stage chain (overlay + text-property slots).
Returns t.  Idempotent.

MCP Parameters: (none)"
  (setq nelisp-textprop-keymap-with-injection t)
  t)

(defun nelisp-textprop-keymap-uninstall ()
  "Disable the Doc 41 §2.5 9-stage chain (= revert to Doc 34 7-stage).
Returns t.  Idempotent.

The provider registry is *not* cleared — re-`install' re-enables
without re-registration.

MCP Parameters: (none)"
  (setq nelisp-textprop-keymap-with-injection nil)
  t)

;;; ─────────────────────────────────────────────────────────────────────
;;; Internal: keymap predicate + safe slot probe
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-keymap--keymap-p (km)
  "Return non-nil if KM is a keymap.
Wraps `keymapp' so the chain assembler stays single-source."
  (and km (keymapp km)))

(defun nelisp-textprop-keymap--probe-slot (slot pos buf)
  "Return the keymap (or nil) for SLOT at POS in BUF.
SLOT is one of the integers 1..9 from
`nelisp-textprop-keymap-slot-*'.  POS / BUF apply only to slots
6 and 7 (the injection slots); other slots read their respective
special variables / functions."
  (cond
   ;; 1. overriding-terminal-local-map (per-terminal special var,
   ;;    bound by Emacs core; we read it via boundp + symbol-value
   ;;    so the module loads cleanly even when the host is a bare
   ;;    --batch session that has never bound it.)
   ((= slot nelisp-textprop-keymap-slot-overriding-terminal-local)
    (nelisp-textprop-keymap--special-var
     'overriding-terminal-local-map))

   ;; 2. overriding-local-map
   ((= slot nelisp-textprop-keymap-slot-overriding-local)
    (nelisp-textprop-keymap--special-var
     'overriding-local-map))

   ;; 3. minor-mode-overriding-map-alist — alist of (SYMBOL . KEYMAP).
   ;;    Doc 34 §2.3 treats the merged composite as one slot; we
   ;;    fold the alist into a composite keymap when non-empty.
   ((= slot nelisp-textprop-keymap-slot-minor-mode-overriding)
    (nelisp-textprop-keymap--composite-from-alist-var
     'minor-mode-overriding-map-alist))

   ;; 4. minor-mode-map-alist
   ((= slot nelisp-textprop-keymap-slot-minor-mode-map-alist)
    (nelisp-textprop-keymap--composite-from-alist-var
     'minor-mode-map-alist))

   ;; 5. emulation-mode-map-alists — list of alist-vars, each
   ;;    referenced through symbol-value indirection.
   ((= slot nelisp-textprop-keymap-slot-emulation-mode-map-alists)
    (nelisp-textprop-keymap--composite-from-emulation
     'emulation-mode-map-alists))

   ;; 6. overlay keymap (Doc 41 §3.4 / T148, opt-in).
   ((= slot nelisp-textprop-keymap-slot-overlay)
    (when (and nelisp-textprop-keymap-with-injection
               nelisp-textprop-keymap--overlay-provider)
      (nelisp-textprop-keymap--call-provider
       nelisp-textprop-keymap--overlay-provider pos buf 'overlay)))

   ;; 7. text-property keymap (Doc 41 §3.3, this module).
   ((= slot nelisp-textprop-keymap-slot-textprop)
    (when (and nelisp-textprop-keymap-with-injection
               nelisp-textprop-keymap--textprop-provider)
      (nelisp-textprop-keymap--call-provider
       nelisp-textprop-keymap--textprop-provider pos buf 'textprop)))

   ;; 8. current-local-map
   ((= slot nelisp-textprop-keymap-slot-current-local)
    (when (fboundp 'current-local-map)
      (current-local-map)))

   ;; 9. global-map
   ((= slot nelisp-textprop-keymap-slot-global)
    (cond
     ((boundp 'global-map) (symbol-value 'global-map))
     ((fboundp 'current-global-map) (current-global-map))
     (t nil)))))

(defun nelisp-textprop-keymap--special-var (sym)
  "Return SYM's value if bound *and* a keymap, else nil."
  (and (boundp sym)
       (let ((v (symbol-value sym)))
         (and (nelisp-textprop-keymap--keymap-p v) v))))

(defun nelisp-textprop-keymap--composite-from-alist-var (sym)
  "Compose a keymap from alist variable SYM (= ((KEY . KEYMAP) ...)).
Each KEY in the alist is a symbol; if its `symbol-value' is non-nil
the associated KEYMAP is included.  This mirrors how Emacs core
walks `minor-mode-map-alist' / `minor-mode-overriding-map-alist'
during `current-active-maps'.

Returns a single composite keymap, or nil when no entry is active."
  (when (boundp sym)
    (let ((alist (symbol-value sym))
          (composite '()))
      (when (consp alist)
        (dolist (cell alist)
          (let ((flag (car-safe cell))
                (km   (cdr-safe cell)))
            (when (and (symbolp flag)
                       (boundp flag)
                       (symbol-value flag)
                       (nelisp-textprop-keymap--keymap-p km))
              (push km composite)))))
      (nelisp-textprop-keymap--combine (nreverse composite)))))

(defun nelisp-textprop-keymap--composite-from-emulation (sym)
  "Compose keymaps from `emulation-mode-map-alists'-shape variable SYM.
SYM's value is a list of alist-bearing *symbols* (or alists in-line);
each is folded through `nelisp-textprop-keymap--composite-from-alist-var'."
  (when (boundp sym)
    (let ((entries (symbol-value sym))
          (composite '()))
      (when (consp entries)
        (dolist (e entries)
          (let ((km (cond
                     ((symbolp e)
                      (nelisp-textprop-keymap--composite-from-alist-var e))
                     ((consp e)
                      (let ((tmpvar (make-symbol "nelisp-tp-keymap-tmp")))
                        (set tmpvar e)
                        (nelisp-textprop-keymap--composite-from-alist-var
                         tmpvar))))))
            (when (nelisp-textprop-keymap--keymap-p km)
              (push km composite)))))
      (nelisp-textprop-keymap--combine (nreverse composite)))))

(defun nelisp-textprop-keymap--combine (kms)
  "Combine a list of keymaps KMS into a single composite (= make-composed-keymap).
Returns nil when the list is empty, the lone keymap when exactly
one, otherwise `make-composed-keymap'."
  (cond
   ((null kms) nil)
   ((null (cdr kms)) (car kms))
   (t
    (if (fboundp 'make-composed-keymap)
        (make-composed-keymap kms)
      ;; Fallback for ancient hosts: just take the highest-priority
      ;; (= first) keymap.  Composed keymaps were standardised in
      ;; Emacs 24 so this branch is essentially dead code.
      (car kms)))))

(defun nelisp-textprop-keymap--call-provider (fn pos buf label)
  "Call provider FN with (POS BUF) and validate the result.
Returns the keymap, or nil when FN returned nil.  Signals
`nelisp-textprop-keymap-bad-provider' when FN returns a non-keymap
non-nil value (= contract violation)."
  (let ((km (funcall fn pos buf)))
    (cond
     ((null km) nil)
     ((nelisp-textprop-keymap--keymap-p km) km)
     (t
      (signal 'nelisp-textprop-keymap-bad-provider
              (list label km))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; Public lookup (Doc 41 §3.3.2)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-textprop-keymap-precedence-at (pos &optional buf)
  "Return the keymap chain at POS in BUF as a list of (SLOT . KEYMAP) cells.
SLOT is the integer 1..9 from the LOCKED §2.5 precedence; the
returned list is ordered highest-priority → lowest, and only
slots whose probe yielded a keymap are included.

When `nelisp-textprop-keymap-with-injection' is nil the result
is byte-identical with the Doc 34 §2.3 7-stage chain (slots 6
and 7 are skipped — i.e. never appear in the returned list).

BUF defaults to the current buffer.  POS may be nil only when
both injection providers are nil (a slot-6/slot-7 probe with
nil POS / BUF is treated as no-op = nil keymap).

MCP Parameters:
  POS — integer position in BUF, or nil (when no injection providers)
  BUF — buffer (or nil = current)"
  (let ((buf (or buf (current-buffer)))
        (out '()))
    (dotimes (i 9)
      (let* ((slot (1+ i))
             (km   (nelisp-textprop-keymap--probe-slot slot pos buf)))
        (when (nelisp-textprop-keymap--keymap-p km)
          (push (cons slot km) out))))
    (nreverse out)))

(defun nelisp-textprop-keymap-resolve-at (pos &optional buf)
  "Return a single composite keymap representing the chain at POS in BUF.
The composite preserves precedence (highest-priority first).  If
no slot yielded a keymap the result is nil.

When `nelisp-textprop-keymap-with-injection' is nil the
composite is over the Doc 34 §2.3 7-stage chain only.

MCP Parameters:
  POS — integer position in BUF, or nil
  BUF — buffer (or nil = current)"
  (let* ((cells (nelisp-textprop-keymap-precedence-at pos buf))
         (kms   (mapcar #'cdr cells)))
    (nelisp-textprop-keymap--combine kms)))

(provide 'nelisp-textprop-keymap)

;;; nelisp-textprop-keymap.el ends here
