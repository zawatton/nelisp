;;; nelisp-overlay.el --- Overlay storage + Emacs-compat API (Phase 9c.4)  -*- lexical-binding: t; -*-

;; Phase 9c.4 per Doc 41 LOCKED-2026-04-25-v2 §3.4 / §2.5 / §2.6.
;; Layer: Phase 9c text-property advanced (sister module of T138
;; `nelisp-emacs-compat-face' / T144 `nelisp-textprop-display' / T146
;; `nelisp-textprop-keymap').
;;
;; Goal: provide the *overlay storage and accessor layer* — a
;; per-buffer overlay registry plus the Emacs-compat surface
;; (=make-overlay= / =overlay-put= / =overlays-at= / etc.) — so that
;; downstream modules (T146 keymap inject, future T149 hooks, Phase 11
;; display backend) can pull overlay state through a stable contract.
;;
;; Contract: =OVERLAY_CONTRACT_VERSION = 1= (Doc 41 §3.4.1 + §4.5 LOCKED v1).
;;
;; Naming convention: this module uses the prefix `nelisp-ovly-' for
;; every public function/variable.  The shorter prefix avoids a
;; namespace collision with the Phase 5-B.2 placeholder cl-defstruct
;; `nelisp-overlay' in `nelisp-buffer.el' (whose auto-generated
;; accessors `nelisp-overlay-start' / `-end' / `-buffer' / etc. would
;; otherwise clash with the Doc 41 §3.4.2 Emacs-compat accessor names).
;; Module file name remains `nelisp-overlay.el' per Doc 41.
;;
;; Storage model (Doc 41 §2.6 LOCKED, recommendation A "per-buffer
;; independent interval tree"):
;;   - Each *host* buffer (Emacs `buffer' object — i.e. the
;;     `current-buffer' / argument of `make-overlay') carries a
;;     per-buffer registry, keyed in a module-local `eq' hash.
;;   - The registry is a sorted list (ascending overlay START), so
;;     `overlays-at' / `overlays-in' / `next-overlay-change' /
;;     `previous-overlay-change' all walk in-order without re-sorting.
;;   - Insertion stamp (= `nelisp-ovly--counter' monotonic) is
;;     the *secondary key* for priority-tie ordering (Doc 41 §2.5
;;     LOCKED v1: priority nil = -∞, ties broken by insertion order
;;     so the *later* overlay wins lookups).
;;
;; Public API (16 functions, all `nelisp-ovly-' prefix):
;;
;;   Construct / classify:
;;     `nelisp-ovly-make'         BEG END &optional BUF FRONT-ADV REAR-ADV
;;     `nelisp-ovly-overlayp'     OBJECT
;;     `nelisp-ovly-copy'         OVERLAY
;;
;;   Endpoints / placement:
;;     `nelisp-ovly-start'        OVERLAY -> INT-or-nil
;;     `nelisp-ovly-end'          OVERLAY -> INT-or-nil
;;     `nelisp-ovly-buffer'       OVERLAY -> BUFFER-or-nil
;;     `nelisp-ovly-move'         OVERLAY BEG END &optional BUF
;;     `nelisp-ovly-delete'       OVERLAY
;;     `nelisp-ovly-delete-all'   &optional BUF
;;
;;   Properties:
;;     `nelisp-ovly-properties'   OVERLAY -> PLIST
;;     `nelisp-ovly-put'          OVERLAY PROP VALUE
;;     `nelisp-ovly-get'          OVERLAY PROP
;;
;;   Range queries:
;;     `nelisp-ovly-lists'           &optional BUF -> (BEFORE-LIST . AFTER-LIST)
;;     `nelisp-ovly-overlays-at'     POS &optional SORTED
;;     `nelisp-ovly-overlays-in'     BEG END
;;     `nelisp-ovly-remove-many'     &optional BEG END NAME VALUE
;;     `nelisp-ovly-next-change'     POS
;;     `nelisp-ovly-previous-change' POS
;;
;;   Provider hooks (Doc 41 §3.4.4):
;;     `nelisp-ovly-get-char-property' POS PROP &optional BUF
;;     `nelisp-ovly-keymap-provider'   POS BUF -> KEYMAP-or-nil
;;     `nelisp-ovly-display-provider'  POS BUF -> SPEC-or-nil
;;
;;   Maintenance:
;;     `nelisp-ovly-recenter'           POS
;;     `nelisp-ovly-shift-on-insert'    BUF AT LEN
;;     `nelisp-ovly-shift-on-delete'    BUF START END
;;
;;   Contract version (Doc 41 §3.4.1 + §4.5 LOCKED v1):
;;     `nelisp-ovly-contract-version' = 1
;;
;; Precedence (Doc 41 §2.5 LOCKED v1, in `nelisp-ovly-overlays-at'
;; SORTED mode and in `nelisp-ovly-get-char-property'):
;;
;;   1. higher numeric `priority' beats lower (= descending priority sort)
;;   2. nil priority is treated as -∞ (= below every numeric priority)
;;   3. on ties (same priority OR both nil), *later insertion order
;;      wins* — i.e. the most recently created overlay shadows the
;;      earlier one.
;;
;; Provider model (Doc 41 §3.4 + §3.3.1 integration):
;;   - `nelisp-ovly-keymap-provider' is registered with T146 via
;;     `nelisp-textprop-keymap-set-overlay-provider' so that the
;;     keymap chain can pull slot 6 (overlay keymap) without knowing
;;     the overlay storage representation.  Registration is opt-in
;;     via `nelisp-ovly-install-providers'.
;;   - The T144 display + T138 face layers do *not* themselves take a
;;     provider hook (per their design); instead their overlay-aware
;;     query route is `nelisp-ovly-get-char-property' which the
;;     primitive layer can call.
;;
;; Non-goals (deferred):
;;   - text-property change hook + overlay-change-functions
;;     (= Doc 41 §3.5 / future T149)
;;   - actual *display backend rendering* of `display' / `face' /
;;     `before-string' / `after-string' / `invisible' overlay payloads
;;     (= Phase 11)
;;   - interval-tree storage data structure (= sorted-list MVP suffices
;;     for Phase 9c.4 close — the contract is the API, not the storage
;;     layout)

;;; Code:

(require 'cl-lib)

;; Forward declaration: T146 keymap inject is loaded *on demand* by
;; `nelisp-ovly-install-providers' / `nelisp-ovly-uninstall-providers'.
;; A hard `require' would create a 9c.3 ↔ 9c.4 cycle and force tests
;; that never touch keymap injection to load the chain module.
(declare-function nelisp-textprop-keymap-set-overlay-provider
                  "nelisp-textprop-keymap" (fn))

;;; ─────────────────────────────────────────────────────────────────────
;;; Errors
;;; ─────────────────────────────────────────────────────────────────────

(define-error 'nelisp-ovly-error
  "NeLisp overlay error")
(define-error 'nelisp-ovly-bad-range
  "Invalid overlay range" 'nelisp-ovly-error)
(define-error 'nelisp-ovly-bad-buffer
  "Invalid overlay buffer" 'nelisp-ovly-error)
(define-error 'nelisp-ovly-dead
  "Operation on a deleted overlay" 'nelisp-ovly-error)

;;; ─────────────────────────────────────────────────────────────────────
;;; Contract version (Doc 41 §3.4.1 + §4.5 LOCKED v1)
;;; ─────────────────────────────────────────────────────────────────────

(defconst nelisp-ovly-contract-version 1
  "Doc 41 §3.4.1 + §4.5 LOCKED contract version for the overlay layer.
Increment only via a new Doc 41 LOCKED revision.")

;;; ─────────────────────────────────────────────────────────────────────
;;; Overlay record
;;; ─────────────────────────────────────────────────────────────────────

(cl-defstruct (nelisp-ovly-record
               (:constructor nelisp-ovly--record-make)
               (:copier nil))
  "Internal overlay record.

Slots:
  ID            — monotonic insertion stamp (= secondary precedence key)
  START / END   — 1-based positions inside BUFFER (Emacs convention)
  BUFFER        — the host buffer the overlay lives in, or nil if dead
  PROPERTIES    — plist of per-overlay properties
  FRONT-ADVANCE — endpoint type for START (Doc 41 §2.6 endpoint LOCKED v0)
  REAR-ADVANCE  — endpoint type for END"
  (id 0)
  (start 1)
  (end 1)
  (buffer nil)
  (properties nil)
  (front-advance nil)
  (rear-advance nil))

;;; ─────────────────────────────────────────────────────────────────────
;;; Per-buffer registry (module-local hash)
;;; ─────────────────────────────────────────────────────────────────────

(defvar nelisp-ovly--registry (make-hash-table :test 'eq :weakness 'key)
  "BUFFER → list of `nelisp-ovly-record', kept sorted ascending START.

Weak keys: when the host buffer is GC'd, the entry vanishes
without leaking the per-buffer overlay list.")

(defvar nelisp-ovly--counter 0
  "Monotonic insertion-stamp counter, source for `id' slot.

The `id' slot is the secondary precedence key when overlays
share a `priority' value (Doc 41 §2.5 LOCKED v1: later
insertion wins).")

(defun nelisp-ovly--registry-clear ()
  "Drop the entire per-buffer overlay registry.  Test hygiene only.

MCP Parameters: (none)"
  (clrhash nelisp-ovly--registry)
  (setq nelisp-ovly--counter 0)
  t)

(defun nelisp-ovly--list-of (buf)
  "Return BUF's overlay record list (the live cell, not a copy)."
  (gethash buf nelisp-ovly--registry))

(defun nelisp-ovly--set-list-of (buf list)
  "Store LIST as BUF's overlay record list.

When LIST is nil the BUF entry is removed entirely so the
registry never grows unbounded with empty cells."
  (if (null list)
      (remhash buf nelisp-ovly--registry)
    (puthash buf list nelisp-ovly--registry))
  list)

(defun nelisp-ovly--ambient-buffer (buf)
  "Resolve BUF (or the current buffer) to an Emacs `bufferp', else signal."
  (let ((b (or buf (current-buffer))))
    (unless (bufferp b)
      (signal 'nelisp-ovly-bad-buffer (list b)))
    b))

;;; ─────────────────────────────────────────────────────────────────────
;;; Sorting / insertion
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly--insert-sorted (buf rec)
  "Insert REC into BUF's overlay list, keeping ascending START order.
Ties on START preserve insertion order (= REC ends up after any
existing record with equal START)."
  (let* ((lst   (nelisp-ovly--list-of buf))
         (start (nelisp-ovly-record-start rec)))
    (cond
     ((null lst)
      (nelisp-ovly--set-list-of buf (list rec)))
     ((< start (nelisp-ovly-record-start (car lst)))
      (nelisp-ovly--set-list-of buf (cons rec lst)))
     (t
      (let ((tail lst))
        (while (and (cdr tail)
                    (<= (nelisp-ovly-record-start (cadr tail)) start))
          (setq tail (cdr tail)))
        (setcdr tail (cons rec (cdr tail)))
        lst)))))

(defun nelisp-ovly--remove-rec (buf rec)
  "Remove REC from BUF's overlay list (one-shot delq)."
  (let ((lst (delq rec (nelisp-ovly--list-of buf))))
    (nelisp-ovly--set-list-of buf lst)))

(defun nelisp-ovly--re-sort-after-mutation (buf)
  "Re-sort BUF's overlay list by ascending START (in-place stable sort).

Called after `move-overlay' / shift-on-{insert,delete} since
those can break the list's ordering invariant."
  (when-let* ((lst (nelisp-ovly--list-of buf)))
    (nelisp-ovly--set-list-of
     buf
     (sort (copy-sequence lst)
           (lambda (a b)
             (let ((sa (nelisp-ovly-record-start a))
                   (sb (nelisp-ovly-record-start b)))
               (if (= sa sb)
                   (< (nelisp-ovly-record-id a)
                      (nelisp-ovly-record-id b))
                 (< sa sb))))))))

;;; ─────────────────────────────────────────────────────────────────────
;;; Liveness
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly--alive-p (rec)
  "Return non-nil if REC is still attached to a buffer."
  (and (nelisp-ovly-record-p rec)
       (nelisp-ovly-record-buffer rec)
       t))

(defun nelisp-ovly--check-alive (rec)
  "Signal `nelisp-ovly-dead' unless REC is still attached."
  (unless (nelisp-ovly--alive-p rec)
    (signal 'nelisp-ovly-dead (list rec))))

;;; ─────────────────────────────────────────────────────────────────────
;;; Construct / classify
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly-overlayp (object)
  "Return non-nil if OBJECT is a NeLisp overlay record.

MCP Parameters: OBJECT — anything."
  (and (nelisp-ovly-record-p object) t))

(defun nelisp-ovly-make (beg end &optional buffer
                             front-advance rear-advance)
  "Create and return a new overlay covering [BEG, END) in BUFFER.
BUFFER defaults to the current buffer.  FRONT-ADVANCE and
REAR-ADVANCE control endpoint behaviour under insertion at the
respective endpoint, mirroring Emacs `make-overlay' semantics
(Doc 41 §2.6 endpoint type LOCKED v0).

Signals `nelisp-ovly-bad-range' when BEG / END are non-integer,
or `nelisp-ovly-bad-buffer' when BUFFER is not a `bufferp'.

The new overlay carries an empty property list and is appended
to the per-buffer registry with a fresh insertion stamp.

MCP Parameters:
  BEG, END     — integer positions in BUFFER (BEG <= END after swap)
  BUFFER       — host buffer or nil = current
  FRONT-ADVANCE — boolean, t = START moves on insertion at START
  REAR-ADVANCE  — boolean, t = END moves on insertion at END"
  (unless (and (integerp beg) (integerp end))
    (signal 'nelisp-ovly-bad-range (list beg end)))
  (let* ((buf (nelisp-ovly--ambient-buffer buffer))
         (s (min beg end))
         (e (max beg end))
         (id (cl-incf nelisp-ovly--counter))
         (rec (nelisp-ovly--record-make
               :id id :start s :end e :buffer buf
               :front-advance (and front-advance t)
               :rear-advance  (and rear-advance  t)
               :properties nil)))
    (nelisp-ovly--insert-sorted buf rec)
    rec))

(defun nelisp-ovly-copy (overlay)
  "Return a copy of OVERLAY in the same buffer with the same range / props.
The copy gets a fresh insertion stamp (= treated as the *later*
overlay for tie-breaking).  Signals `nelisp-ovly-dead' when
OVERLAY has been deleted.

MCP Parameters: OVERLAY — overlay record."
  (nelisp-ovly--check-alive overlay)
  (let* ((buf (nelisp-ovly-record-buffer overlay))
         (id (cl-incf nelisp-ovly--counter))
         (rec (nelisp-ovly--record-make
               :id id
               :start (nelisp-ovly-record-start overlay)
               :end   (nelisp-ovly-record-end   overlay)
               :buffer buf
               :front-advance (nelisp-ovly-record-front-advance overlay)
               :rear-advance  (nelisp-ovly-record-rear-advance overlay)
               :properties (copy-sequence
                            (nelisp-ovly-record-properties overlay)))))
    (nelisp-ovly--insert-sorted buf rec)
    rec))

;;; ─────────────────────────────────────────────────────────────────────
;;; Endpoints / placement
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly-start (overlay)
  "Return the start position of OVERLAY, or nil if it is dead.

MCP Parameters: OVERLAY — overlay record."
  (and (nelisp-ovly--alive-p overlay)
       (nelisp-ovly-record-start overlay)))

(defun nelisp-ovly-end (overlay)
  "Return the end position of OVERLAY, or nil if it is dead.

MCP Parameters: OVERLAY — overlay record."
  (and (nelisp-ovly--alive-p overlay)
       (nelisp-ovly-record-end overlay)))

(defun nelisp-ovly-buffer (overlay)
  "Return the buffer of OVERLAY, or nil if it is dead.

MCP Parameters: OVERLAY — overlay record."
  (and (nelisp-ovly-record-p overlay)
       (nelisp-ovly-record-buffer overlay)))

(defun nelisp-ovly-move (overlay beg end &optional buffer)
  "Re-anchor OVERLAY to [BEG, END) (and optionally to BUFFER).
When BUFFER is supplied and differs from OVERLAY's current
buffer, OVERLAY migrates: it is removed from its old buffer's
registry and inserted into BUFFER's registry (preserving its
insertion stamp).

Returns OVERLAY.  Signals `nelisp-ovly-dead' when OVERLAY has
been deleted.

MCP Parameters:
  OVERLAY — overlay record (must be alive)
  BEG, END — integer positions in BUFFER (or current OVERLAY buffer)
  BUFFER  — optional target buffer"
  (nelisp-ovly--check-alive overlay)
  (unless (and (integerp beg) (integerp end))
    (signal 'nelisp-ovly-bad-range (list beg end)))
  (let* ((old-buf (nelisp-ovly-record-buffer overlay))
         (new-buf (if buffer (nelisp-ovly--ambient-buffer buffer) old-buf))
         (s (min beg end))
         (e (max beg end)))
    (when (not (eq old-buf new-buf))
      (nelisp-ovly--remove-rec old-buf overlay)
      (setf (nelisp-ovly-record-buffer overlay) new-buf)
      (nelisp-ovly--insert-sorted new-buf overlay))
    (setf (nelisp-ovly-record-start overlay) s)
    (setf (nelisp-ovly-record-end overlay) e)
    (when (eq old-buf new-buf)
      ;; In-buffer move can break the start-ordering invariant; re-sort.
      (nelisp-ovly--re-sort-after-mutation new-buf))
    overlay))

(defun nelisp-ovly-delete (overlay)
  "Detach OVERLAY from its buffer's registry.  Returns nil.
After deletion `nelisp-ovly-buffer' returns nil, and any
attempt to `nelisp-ovly-move' / `nelisp-ovly-copy' will signal
`nelisp-ovly-dead'.  `nelisp-ovly-get' / `nelisp-ovly-put' on a
dead overlay are tolerant (they read / mutate the property plist
without signalling, mirroring Emacs precedent).

MCP Parameters: OVERLAY — overlay record."
  (when (nelisp-ovly--alive-p overlay)
    (let ((buf (nelisp-ovly-record-buffer overlay)))
      (nelisp-ovly--remove-rec buf overlay)
      (setf (nelisp-ovly-record-buffer overlay) nil)))
  nil)

(defun nelisp-ovly-delete-all (&optional buffer)
  "Remove every overlay in BUFFER (or the current buffer).
Each overlay is properly detached (= `nelisp-ovly-buffer' returns
nil afterwards).  Returns nil.

MCP Parameters: BUFFER — host buffer or nil = current."
  (let* ((buf (nelisp-ovly--ambient-buffer buffer))
         (lst (nelisp-ovly--list-of buf)))
    (dolist (rec lst)
      (setf (nelisp-ovly-record-buffer rec) nil))
    (nelisp-ovly--set-list-of buf nil))
  nil)

;;; ─────────────────────────────────────────────────────────────────────
;;; Properties
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly-properties (overlay)
  "Return a *fresh copy* of OVERLAY's property plist.

MCP Parameters: OVERLAY — overlay record."
  (when (nelisp-ovly-record-p overlay)
    (copy-sequence (nelisp-ovly-record-properties overlay))))

(defun nelisp-ovly-put (overlay prop value)
  "Set PROP to VALUE on OVERLAY, returning VALUE.
PROP can be any symbol; VALUE is stored as-is.  Property store is
a plist; an existing PROP is updated in place, otherwise the new
key/value pair is prepended (so `nelisp-ovly-get' sees fresh
writes first).

Tolerant of dead overlays (matches Emacs precedent).

MCP Parameters:
  OVERLAY — overlay record
  PROP    — symbol property key
  VALUE   — anything"
  (when (nelisp-ovly-record-p overlay)
    (let* ((pl (nelisp-ovly-record-properties overlay))
           (cell (and pl (memq prop pl))))
      (cond
       (cell
        (setcar (cdr cell) value))
       (t
        (setf (nelisp-ovly-record-properties overlay)
              (cons prop (cons value pl)))))))
  value)

(defun nelisp-ovly-get (overlay prop)
  "Return the value of PROP on OVERLAY, or nil if absent.

MCP Parameters:
  OVERLAY — overlay record
  PROP    — symbol property key"
  (when (nelisp-ovly-record-p overlay)
    (plist-get (nelisp-ovly-record-properties overlay) prop)))

;;; ─────────────────────────────────────────────────────────────────────
;;; Range queries
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly--effective-priority (rec)
  "Return REC's `priority' as a number for ordering purposes.
nil priority is mapped to a *finite* sentinel that compares
below every numeric priority for descending sort
(Doc 41 §2.5 LOCKED v1)."
  (let ((p (plist-get (nelisp-ovly-record-properties rec) 'priority)))
    (if (numberp p) p most-negative-fixnum)))

(defun nelisp-ovly--precedence-> (a b)
  "Return non-nil if A outranks B (descending priority, then later id wins).

Doc 41 §2.5 LOCKED v1:
  - higher numeric priority > lower numeric priority
  - nil priority = -∞
  - same priority: later insertion (= higher id) wins"
  (let ((pa (nelisp-ovly--effective-priority a))
        (pb (nelisp-ovly--effective-priority b)))
    (if (= pa pb)
        (> (nelisp-ovly-record-id a) (nelisp-ovly-record-id b))
      (> pa pb))))

(defun nelisp-ovly-lists (&optional buffer)
  "Return (BEFORE-LIST . AFTER-LIST) of overlays in BUFFER.
Mirrors Emacs `overlay-lists': BEFORE-LIST is overlays whose end
is < point, AFTER-LIST is the rest.  Both lists are returned as
*fresh* lists ordered by ascending START.

If BUFFER has no current point reference (= no `nelisp-point'
analogue), BEFORE-LIST is empty and AFTER-LIST contains every
overlay.  In a host Emacs buffer the host `point' is used.

MCP Parameters: BUFFER — host buffer or nil = current."
  (let* ((buf (nelisp-ovly--ambient-buffer buffer))
         (pt (with-current-buffer buf (point)))
         (lst (nelisp-ovly--list-of buf))
         before after)
    (dolist (rec lst)
      (if (< (nelisp-ovly-record-end rec) pt)
          (push rec before)
        (push rec after)))
    (cons (nreverse before) (nreverse after))))

(defun nelisp-ovly-overlays-at (pos &optional sorted)
  "Return overlays in the current buffer covering POS.
An overlay covers POS when START <= POS < END (half-open),
matching Emacs `overlays-at' semantics.

When SORTED is non-nil the returned list is ordered by
descending precedence (= highest-priority first per
Doc 41 §2.5 LOCKED v1: priority desc → insertion desc on ties).

When SORTED is nil the result is in registry-order (ascending
START), matching Emacs precedent.

MCP Parameters:
  POS    — integer position
  SORTED — non-nil = sort by precedence (descending)"
  (let* ((buf (current-buffer))
         (lst (nelisp-ovly--list-of buf))
         hits)
    (dolist (rec lst)
      (when (and (<= (nelisp-ovly-record-start rec) pos)
                 (<  pos (nelisp-ovly-record-end rec)))
        (push rec hits)))
    (let ((out (nreverse hits)))
      (if sorted
          (sort out #'nelisp-ovly--precedence->)
        out))))

(defun nelisp-ovly-overlays-in (beg end)
  "Return overlays in the current buffer overlapping [BEG, END).
An overlay overlaps when its [START, END) intersects [BEG, END);
i.e. START < END-arg AND END > BEG-arg.  Zero-length overlays
(START == END) at position BEG are also returned, matching
Emacs `overlays-in' precedent for boundary inclusion.

MCP Parameters: BEG, END — integer positions."
  (let* ((buf (current-buffer))
         (lst (nelisp-ovly--list-of buf))
         (lo (min beg end))
         (hi (max beg end))
         hits)
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        (when (or (and (< s hi) (> e lo))
                  (and (= s e) (= s lo)))
          (push rec hits))))
    (nreverse hits)))

(defun nelisp-ovly-remove-many (&optional beg end name value)
  "Delete overlays in [BEG, END) of the current buffer matching NAME=VALUE.
When BEG / END are nil they default to point-min / point-max
(= the entire buffer).  When NAME is nil every overlay in range
is deleted; when NAME is non-nil only overlays whose
`nelisp-ovly-get' of NAME equals VALUE (under `equal') are
deleted.

Mirrors Emacs `remove-overlays' semantics.  Returns nil.

MCP Parameters:
  BEG, END — integer positions or nil
  NAME    — property name or nil
  VALUE   — required value of NAME (when NAME non-nil)"
  (let* ((buf (current-buffer))
         (lo  (or beg (point-min)))
         (hi  (or end (point-max)))
         (kill nil))
    (dolist (rec (nelisp-ovly--list-of buf))
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        (when (and (< s hi) (> e lo))
          (when (or (null name)
                    (equal (plist-get (nelisp-ovly-record-properties rec)
                                      name)
                           value))
            (push rec kill)))))
    (dolist (rec kill)
      (nelisp-ovly-delete rec)))
  nil)

(defun nelisp-ovly-next-change (pos)
  "Return the next position after POS where an overlay endpoint occurs.
Returns `point-max' when no later endpoint exists.

`Endpoint' = either START or END of any overlay in the current
buffer.  Mirrors Emacs `next-overlay-change' semantics: the
returned position is the *strictly greater* nearest endpoint.

MCP Parameters: POS — integer position."
  (let* ((buf (current-buffer))
         (lst (nelisp-ovly--list-of buf))
         (best (with-current-buffer buf (point-max))))
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        (when (and (> s pos) (< s best)) (setq best s))
        (when (and (> e pos) (< e best)) (setq best e))))
    best))

(defun nelisp-ovly-previous-change (pos)
  "Return the previous position before POS where an overlay endpoint occurs.
Returns `point-min' when no earlier endpoint exists.

`Endpoint' = either START or END of any overlay in the current
buffer.  Mirrors Emacs `previous-overlay-change' semantics: the
returned position is the *strictly less* nearest endpoint.

MCP Parameters: POS — integer position."
  (let* ((buf (current-buffer))
         (lst (nelisp-ovly--list-of buf))
         (best (with-current-buffer buf (point-min))))
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        (when (and (< s pos) (> s best)) (setq best s))
        (when (and (< e pos) (> e best)) (setq best e))))
    best))

;;; ─────────────────────────────────────────────────────────────────────
;;; Maintenance — gap-buffer adjust hooks
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly-recenter (_pos)
  "Hint the overlay registry to optimise around POS.
Currently a no-op (= the sorted-list MVP doesn't benefit from
recenter); kept for Emacs API parity so callers don't need to
guard.  Returns nil.

MCP Parameters: POS — integer position (ignored in MVP)."
  nil)

(defun nelisp-ovly-shift-on-insert (buffer at len)
  "Update overlays in BUFFER when LEN chars were inserted at AT.

Endpoint movement rules per Emacs precedent:
  - START strictly less than AT: untouched.
  - START exactly at AT: advances iff `front-advance' = t.
  - START strictly greater than AT: shifted by +LEN.
  - END strictly less than AT: untouched.
  - END exactly at AT: advances iff `rear-advance' = t.
  - END strictly greater than AT: shifted by +LEN.

Returns nil.  Re-sorts the per-buffer list (the START shifts
above can break the ascending-start invariant).

MCP Parameters:
  BUFFER — host buffer
  AT     — integer insertion position
  LEN    — non-negative integer insertion length"
  (let ((lst (nelisp-ovly--list-of buffer)))
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        ;; START
        (cond
         ((< s at) nil)
         ((and (= s at) (null (nelisp-ovly-record-front-advance rec))) nil)
         (t (setf (nelisp-ovly-record-start rec) (+ s len))))
        ;; END
        (cond
         ((< e at) nil)
         ((and (= e at) (null (nelisp-ovly-record-rear-advance rec))) nil)
         (t (setf (nelisp-ovly-record-end rec) (+ e len))))))
    (when lst
      (nelisp-ovly--re-sort-after-mutation buffer)))
  nil)

(defun nelisp-ovly-shift-on-delete (buffer start end)
  "Update overlays in BUFFER when text in [START, END) was deleted.

Endpoint collapse rules per Emacs precedent:
  - point <= START                    : untouched.
  - START < point <= END              : collapses to START.
  - point > END                       : shifted by -(END-START).

Returns nil.  Re-sorts the per-buffer list afterwards.

MCP Parameters:
  BUFFER     — host buffer
  START, END — integer deletion range (END exclusive)"
  (let ((delta (- end start))
        (lst (nelisp-ovly--list-of buffer)))
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec)))
        (setf (nelisp-ovly-record-start rec)
              (cond ((<= s start) s)
                    ((>= s end) (- s delta))
                    (t start)))
        (setf (nelisp-ovly-record-end rec)
              (cond ((<= e start) e)
                    ((>= e end) (- e delta))
                    (t start)))))
    (when lst
      (nelisp-ovly--re-sort-after-mutation buffer)))
  nil)

;;; ─────────────────────────────────────────────────────────────────────
;;; Provider hooks (Doc 41 §3.4.4 / §3.3.1 integration)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly--top-by-property (pos prop buf)
  "Return the value of PROP from the highest-precedence overlay covering POS.
Walks the per-buffer overlay list, restricts to overlays whose
PROP is *present* (= `plist-member'), and picks the winner via
`nelisp-ovly--precedence->'.  Returns nil when no overlay
covers POS or none has PROP."
  (let* ((lst (nelisp-ovly--list-of buf))
         winner)
    (dolist (rec lst)
      (let ((s (nelisp-ovly-record-start rec))
            (e (nelisp-ovly-record-end rec))
            (pl (nelisp-ovly-record-properties rec)))
        (when (and (<= s pos) (< pos e)
                   (plist-member pl prop))
          (when (or (null winner)
                    (nelisp-ovly--precedence-> rec winner))
            (setq winner rec)))))
    (and winner
         (plist-get (nelisp-ovly-record-properties winner) prop))))

(defun nelisp-ovly-get-char-property (pos prop &optional buffer)
  "Return PROP at POS, with overlays taking precedence over text properties.

Doc 41 §3.4.4 + Doc 34 §4.6 precedence (= overlay > text-property
> default).  When no overlay at POS has PROP, the lookup falls
through to host `get-char-property' via `get-text-property'-style
fallback (= the host text-property layer is consulted).

If there are no overlays in BUFFER, the result is byte-identical
with `get-text-property'.

MCP Parameters:
  POS    — integer position
  PROP   — symbol property name
  BUFFER — host buffer or nil = current"
  (let ((buf (nelisp-ovly--ambient-buffer buffer)))
    (or (nelisp-ovly--top-by-property pos prop buf)
        (with-current-buffer buf
          (get-text-property pos prop)))))

(defun nelisp-ovly-keymap-provider (pos buf)
  "Slot-6 (overlay) keymap provider for `nelisp-textprop-keymap'.

Looks up overlays at POS in BUF and returns the *highest-precedence*
overlay's `keymap' property; falls back to `local-map' (Doc 41 §3.4.3
property semantics).  Returns nil when no overlay at POS carries
either property.

MCP Parameters:
  POS — integer position
  BUF — host buffer"
  (or (nelisp-ovly--top-by-property pos 'keymap   buf)
      (nelisp-ovly--top-by-property pos 'local-map buf)))

(defun nelisp-ovly-display-provider (pos buf)
  "Display-spec provider returning the highest-precedence `display' value at POS.

Mirrors `nelisp-ovly-keymap-provider' but for the `display'
property — used by Phase 11 backend pull / and by
`nelisp-ovly-get-char-property' callers.

MCP Parameters:
  POS — integer position
  BUF — host buffer"
  (nelisp-ovly--top-by-property pos 'display buf))

;;; ─────────────────────────────────────────────────────────────────────
;;; Provider registration (Doc 41 §3.3.1 integration)
;;; ─────────────────────────────────────────────────────────────────────

(defun nelisp-ovly-install-providers ()
  "Wire `nelisp-ovly-keymap-provider' into the T146 keymap chain.

After this call, `nelisp-textprop-keymap-resolve-at' (when its
own injection flag is on) sees the slot-6 overlay keymap derived
from this module's overlay storage.

Returns t.  Idempotent.  Loads `nelisp-textprop-keymap' on demand.

MCP Parameters: (none)"
  (require 'nelisp-textprop-keymap)
  (nelisp-textprop-keymap-set-overlay-provider
   #'nelisp-ovly-keymap-provider)
  t)

(defun nelisp-ovly-uninstall-providers ()
  "Clear the slot-6 overlay keymap provider in T146.

Returns t.  Safe to call when the providers were never installed.

MCP Parameters: (none)"
  (require 'nelisp-textprop-keymap)
  (nelisp-textprop-keymap-set-overlay-provider nil)
  t)

(provide 'nelisp-overlay)

;;; nelisp-overlay.el ends here
