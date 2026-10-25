;;; nelisp-buffer.el --- Phase 5-B.1 gap buffer primitive -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 13 Phase 5-B.1 — NeLisp-side buffer primitive.  §2.6 is
;; LOCKED "Parallel" (Doc 13 §5), so this module builds an
;; independent buffer registry under the `nelisp-' prefix; host
;; `buffer-*' functions are not involved in the data path.  Text
;; storage uses a gap buffer represented by two strings
;; (before-gap / after-gap), chosen per §2.1 LOCK "A".  The
;; representation is correct but not particularly fast — the
;; interface is crafted so that a later swap to a rope or piece
;; table can keep the public API stable.
;;
;; Position semantics follow Emacs: 1-based, `point-min' defaults
;; to 1, `point-max' is `(1+ buffer-size)', `point' is always in
;; `[point-min, point-max]'.  Narrowing is recorded per buffer
;; without actually shrinking the underlying text.
;;
;; The user-facing surface intentionally mirrors the host names
;; with an `nelisp-' prefix (e.g. `nelisp-insert' ≈ `insert'), so
;; parity tests can compare behaviour call-for-call.

;;; Code:

(require 'cl-lib)

(cl-defstruct (nelisp-buffer
               (:constructor nelisp-buffer--make)
               (:copier nil))
  name
  (before-gap "")
  (after-gap "")
  (modified nil)
  (markers nil)
  (overlays nil)
  (narrow-start nil)
  (narrow-end nil)
  (text-properties nil)   ; list of (START END PROP-PLIST) intervals, §3.2
  (tick 0)                ; text-change counter (formerly the `nelisp-buffer--tick' table)
  (pend nil)              ; pending `goto-char' target, or nil (formerly `nelisp-buffer--pending-point')
  (blen 0)                ; (length before-gap), exact at all times
  (len 0)                 ; total text length, exact at all times
  (tcache nil)            ; (TICK . STRING) whole-text memo (formerly `nelisp-buffer--text-cache')
  (ins nil))              ; plain strings inserted at the gap, newest first, not yet in before-gap

(cl-defstruct (nelisp-marker
               (:constructor nelisp-marker--make-plain)
               (:copier nil))
  "Position reference that follows text mutation of a `nelisp-buffer'.
A marker of record type `nelisp-marker-ins' advances when text is inserted
exactly at its position; one of type `nelisp-marker' stays put."
  (buffer nil)
  (position 1))

;; Doc 216 S5: the insertion type is the record type, not a third field, so a
;; marker is a two-field record.  `nelisp-marker-ins' marks one that advances
;; past text inserted at its position; `nelisp-marker-p' accepts both.
(cl-defstruct (nelisp-marker-ins
               (:include nelisp-marker)
               (:constructor nelisp-marker-ins--make-plain)
               (:copier nil)))

(defun nelisp-marker--make (&rest args)
  "Make a marker from the keywords :buffer, :position and :insertion-type."
  (let ((buffer (plist-get args :buffer))
        (position (if (plist-member args :position) (plist-get args :position) 1)))
    (if (plist-get args :insertion-type)
        (nelisp-marker-ins--make-plain :buffer buffer :position position)
      (nelisp-marker--make-plain :buffer buffer :position position))))

(defun nelisp-marker-insertion-type (marker)
  "Return non-nil when MARKER advances past text inserted at its position."
  (nelisp-marker-ins-p marker))

(defun nelisp-marker--set-insertion-type (marker type)
  "Set MARKER's insertion type to TYPE by retyping its record."
  (aset marker 0 (if type 'nelisp-marker-ins 'nelisp-marker))
  type)

(cl-defstruct (nelisp-overlay
               (:constructor nelisp-overlay--make)
               (:copier nil))
  "Range [START, END) in a `nelisp-buffer' that carries a plist of props.
FRONT-ADVANCE / REAR-ADVANCE mirror Emacs' overlay insertion-
type semantics for the START and END endpoints respectively."
  (buffer nil)
  (start 1)
  (end 1)
  (front-advance nil)
  (rear-advance nil)
  (props nil))

(defvar nelisp-buffer--registry
  (make-hash-table :test 'equal)
  "Name → `nelisp-buffer' map.  Name collisions are disambiguated
by `nelisp-generate-new-buffer'.")

(defvar nelisp-buffer--current nil
  "The currently selected NeLisp buffer, or nil.
`nelisp-with-buffer' binds this dynamically; most operations
default to this value.")

;; PERF (search-forward quadratic-cost fix, 2026-09-28): `nelisp-buffer-
;; string' used to `concat' `before-gap'/`after-gap' -- an O(buffer size)
;; copy -- on EVERY call, and `nelisp-buffer-substring' called it just to
;; slice out a range as small as one character (`nelisp-char-after' et
;; al.), and `nelisp-goto-char' called it just to re-split the same total
;; text at a new boundary.  Measured on the standalone binary: a
;; `search-forward' loop over an 80KB buffer (`re-search-forward' calling
;; `buffer-substring'/`goto-char' once per match) took 36.7s, vs. 0.11s
;; for a 2KB buffer of the same shape -- quadratic, not linear, in
;; buffer size.  `nelisp-buffer-substring' now slices directly from
;; whichever of `before-gap'/`after-gap' the range falls in (O(range
;; size)), and `nelisp-buffer-string' itself is memoized per BUF against
;; `nelisp-buffer--tick' (the few callers -- search's own whole-buffer
;; scan window, `write-region' with START nil -- that really do want the
;; whole text now pay one O(size) rebuild between edits, not N of them).
;;
;; That alone was not enough: with `before-gap'/`after-gap' as PLAIN
;; strings, moving the gap boundary at all -- even by one character --
;; still costs O(current position) via `concat'/`substring' (strings are
;; immutable; producing a new one of length K always copies K
;; characters, regardless of how much of that K is actually new).  A
;; `search-forward' LOOP calls `goto-char' once per match, so this alone
;; reproduced the original quadratic total even after the fix above --
;; measured directly, see delta.patch's `probe-cache2.el' timings.
;;
;; `nelisp-goto-char' therefore no longer moves the gap at all: it
;; records the new point in `nelisp-buffer--pending-point', O(1), and
;; `nelisp-point' returns that pending value when one is outstanding.
;; `before-gap'/`after-gap' are left exactly as they were, which stays
;; correct for every READER (`nelisp-buffer-substring', `nelisp-char-
;; after'/`-before', `nelisp-buffer-string') because their sum is the
;; same text no matter where the boundary between them currently sits.
;; Only `nelisp-insert' (and this file's ported `nelisp-insert-before-
;; markers', where a copy exists) genuinely needs the gap AT point --
;; it appends new text straight onto `before-gap' -- so those call
;; `nelisp-buffer--settle' first, paying the O(distance) move exactly
;; once, lazily, instead of on every intervening `goto-char'.
(defvar nelisp-buffer--text-cache (make-hash-table :test 'eq)
  "BUF -> cached whole-buffer text, as `nelisp-buffer-string' would
build it fresh every time.  Valid only while `(car (gethash BUF
nelisp-buffer--text-cache))' still matches `(gethash BUF nelisp-
buffer--tick)'; see that variable's docstring.")

(defvar nelisp-buffer--tick (make-hash-table :test 'eq)
  "BUF -> monotonically increasing integer, bumped by `nelisp-buffer--
bump-tick' once per call that changes BUF's TEXT (`nelisp-insert',
`nelisp-delete-region', `nelisp-erase-buffer' -- and this file's own
ported `nelisp-insert-before-markers' where a copy of it exists).
Deliberately NOT bumped by `nelisp-goto-char': a pending point (see
`nelisp-buffer--pending-point') changes nothing about the concatenated
text, only which position a later `nelisp-insert' will settle to.")

(defun nelisp-buffer--bump-tick (buf)
  "Invalidate BUF's cached whole-buffer text (see `nelisp-buffer-tick')."
  (setf (nelisp-buffer-tick buf) (1+ (nelisp-buffer-tick buf))))

(defvar nelisp-buffer--pending-point (make-hash-table :test 'eq)
  "BUF -> a point value `nelisp-goto-char' recorded but has not yet
physically moved BUF's gap to match.  Absent means the gap already
sits exactly at BUF's current point (`(1+ (length before-gap))' is
already correct).  See `nelisp-buffer--settle' and the PERF block
comment above `nelisp-buffer--text-cache'.")

(defun nelisp-buffer--settle (buf &optional flush-only)
  "Make BUF's gap strings current.  First append the pending inserts in
the INS slot to `before-gap' (one concat of a grouped list, so a run of
small inserts costs O(total) instead of O(n^2)); then, unless FLUSH-ONLY,
physically move the gap to the pending point.  Moving is O(distance from
the gap's CURRENT physical position to the pending target) -- paid once,
lazily, right before an operation (`nelisp-insert' et al.) that needs the
gap actually at point."
  (let ((parts (nreverse (nelisp-buffer-ins buf))))
    (when parts
      (setf (nelisp-buffer-ins buf) nil)
      (while (cdr parts)
        (let ((out nil))
          (while parts
            (let ((chunk nil) (k 0))
              (while (and parts (< k 256))
                (setq chunk (cons (car parts) chunk) parts (cdr parts) k (1+ k)))
              (setq out (cons (apply #'concat (nreverse chunk)) out))))
          (setq parts (nreverse out))))
      (setf (nelisp-buffer-before-gap buf)
            (concat (nelisp-buffer-before-gap buf) (car parts)))))
  (let ((pending (and (not flush-only) (nelisp-buffer-pend buf))))
    (when pending
      (let ((idx (1- pending)) (blen (nelisp-buffer-blen buf)))
        (if (< idx blen)
            (let ((before (nelisp-buffer-before-gap buf)))
              (setf (nelisp-buffer-after-gap buf)
                    (concat (substring before idx) (nelisp-buffer-after-gap buf)))
              (setf (nelisp-buffer-before-gap buf) (substring before 0 idx)))
          (let ((k (- idx blen)) (after (nelisp-buffer-after-gap buf)))
            (setf (nelisp-buffer-before-gap buf)
                  (concat (nelisp-buffer-before-gap buf) (substring after 0 k)))
            (setf (nelisp-buffer-after-gap buf) (substring after k))))
        (setf (nelisp-buffer-blen buf) idx)
        (setf (nelisp-buffer-pend buf) nil)))))

;; Defined with their initial values below; declared here for the reset.
(defvar nelisp-buffer--blen-cache)
(defvar nelisp-buffer--size-cache)

(defun nelisp-buffer--reset-registry ()
  "Clear the NeLisp buffer registry.  Test hygiene only."
  (clrhash nelisp-buffer--registry)
  (clrhash nelisp-buffer--text-cache)
  (clrhash nelisp-buffer--tick)
  (clrhash nelisp-buffer--pending-point)
  (clrhash nelisp-buffer--blen-cache)
  (clrhash nelisp-buffer--size-cache)
  (setq nelisp-buffer--current nil))

;;; Constructors / lookup ---------------------------------------------

(defun nelisp-generate-new-buffer (name)
  "Return a fresh `nelisp-buffer', uniquifying NAME via `<N>' suffix."
  (let* ((base name)
         (final name)
         (count 1))
    (while (gethash final nelisp-buffer--registry)
      (setq count (1+ count))
      (setq final (format "%s<%d>" base count)))
    (let ((buf (nelisp-buffer--make :name final)))
      (puthash final buf nelisp-buffer--registry)
      buf)))

(defun nelisp-get-buffer-create (name)
  "Return the buffer named NAME, creating it if absent."
  (or (gethash name nelisp-buffer--registry)
      (let ((buf (nelisp-buffer--make :name name)))
        (puthash name buf nelisp-buffer--registry)
        buf)))

(defun nelisp-get-buffer (name)
  "Return the buffer named NAME, or nil if absent."
  (gethash name nelisp-buffer--registry))

(defun nelisp-kill-buffer (buf)
  "Remove BUF from the registry.  Returns t on success."
  (let ((name (nelisp-buffer-name buf)))
    (when (gethash name nelisp-buffer--registry)
      (remhash name nelisp-buffer--registry)
      ;; Drop BUF's search-cache entries too, or a killed buffer's cached
      ;; text (see `nelisp-buffer-string') lingers in both hash tables
      ;; forever, keyed `eq' on an object nothing else can reach.
      (remhash buf nelisp-buffer--text-cache)
      (remhash buf nelisp-buffer--tick)
      (remhash buf nelisp-buffer--pending-point)
      (remhash buf nelisp-buffer--blen-cache)
      (remhash buf nelisp-buffer--size-cache)
      (when (eq nelisp-buffer--current buf)
        (setq nelisp-buffer--current nil))
      t)))

(defun nelisp-buffer-list ()
  "Return a list of live NeLisp buffers, oldest first, as GNU's buffer
list holds them.  An Emacs hash table walks in insertion order."
  (let (result)
    (maphash (lambda (_ buf) (push buf result))
             nelisp-buffer--registry)
    (nreverse result)))

;;; Current buffer dispatch -------------------------------------------

(defun nelisp-current-buffer ()
  "Return the currently selected NeLisp buffer, or nil."
  nelisp-buffer--current)

(defun nelisp-set-buffer (buf)
  "Set BUF as the current NeLisp buffer.  Returns BUF."
  (setq nelisp-buffer--current buf)
  (nelisp-goto-char (nelisp-point buf) buf)
  buf)

(defmacro nelisp-with-buffer (buf &rest body)
  "Evaluate BODY with BUF as the NeLisp current buffer.
Dynamically rebinds `nelisp-buffer--current' so nested
`with-buffer' forms stack correctly."
  (declare (indent 1))
  `(let ((nelisp-buffer--current ,buf))
     (nelisp-goto-char (nelisp-point nelisp-buffer--current)
                       nelisp-buffer--current)
     ,@body))

(defun nelisp-buffer--ambient (buf-or-nil)
  "Resolve BUF-OR-NIL to an actual buffer (defaulting to current).
Signals `error' when neither argument nor current is set."
  (or buf-or-nil nelisp-buffer--current
      (error "No NeLisp current buffer")))

;;; Size / position ---------------------------------------------------

;; PERF (syntax-scanning quadratic-cost fix, 2026-09-28): mirrors
;; `scripts/nelisp-stdlib-prelude.el's fix of the same name for the
;; standalone runtime, where `length' on a large string measured
;; ~1.1ms/call vs ~0.13ms/call on a 100-char one (not O(1) there the
;; way it is under host Emacs) -- see that file's own PERF comment
;; above its `nelisp-buffer--ambient' for the measurement.  Kept in
;; sync here per this file's existing convention of mirroring the
;; standalone's buffer-layer fixes (see the search-forward PERF
;; comment already on `nelisp-buffer--text-cache' below), even though
;; under host Emacs `length' is already O(1) and this cache is a
;; smaller win: it still removes redundant `length' calls from
;; `nelisp-point'/`nelisp-buffer-size'/`nelisp-buffer-substring'/
;; `nelisp-char-after'/`nelisp-char-before'/`nelisp-goto-char', all
;; called once per character by the syntax-scanning primitives.
(defvar nelisp-buffer--blen-cache (make-hash-table :test 'eq)
  "BUF -> (TICK . BEFORE-GAP-LENGTH); see the PERF comment above.")

(defvar nelisp-buffer--size-cache (make-hash-table :test 'eq)
  "BUF -> (TICK . TOTAL-SIZE); see the PERF comment above.")

(defun nelisp-buffer--before-length (buf)
  "Return the length of BUF's before-gap string (the BLEN slot)."
  (nelisp-buffer-blen buf))

(defun nelisp-buffer-size (&optional buf)
  "Return the length of BUF's visible (unrestricted) text."
  (nelisp-buffer-len (or buf nelisp-buffer--current
                         (error "No NeLisp current buffer"))))

(defun nelisp-point (&optional buf)
  "Return the current point in BUF (1-based).
A pending, not-yet-settled `goto-char' target wins over the gap's
physical position -- see `nelisp-buffer-pend'."
  (let ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer"))))
    (or (nelisp-buffer-pend b) (1+ (nelisp-buffer-blen b)))))

(defun nelisp-point-min (&optional buf)
  "Return the narrowed point-min of BUF (defaults to 1)."
  (or (nelisp-buffer-narrow-start
       (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
      1))

(defun nelisp-point-max (&optional buf)
  "Return the narrowed point-max of BUF."
  (let ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer"))))
    (or (nelisp-buffer-narrow-end b) (1+ (nelisp-buffer-len b)))))

(defun nelisp-buffer-string (&optional buf)
  "Return the entire text of BUF as a new string.
Memoized against `nelisp-buffer-tick': O(1) when BUF's text has not
changed since the last call, O(buffer size) to rebuild otherwise.  The
returned string may be shared across calls -- treat it as read-only."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (cached (nelisp-buffer-tcache b)))
    (if (and cached (= (car cached) (nelisp-buffer-tick b)))
        (cdr cached)
      (let ((s (progn (when (nelisp-buffer-ins b) (nelisp-buffer--settle b t))
                      (concat (nelisp-buffer-before-gap b)
                       (nelisp-buffer-after-gap b)))))
        (setf (nelisp-buffer-tcache b) (cons (nelisp-buffer-tick b) s))
        s))))

(defun nelisp-buffer-substring (start end &optional buf)
  "Return the substring between 1-based START and END in BUF.
Slices directly from `before-gap'/`after-gap' -- O(END - START)
regardless of BUF's total size -- rather than building the whole
buffer text first via `nelisp-buffer-string'."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (blen (progn (when (nelisp-buffer-ins b) (nelisp-buffer--settle b t))
                      (nelisp-buffer-blen b)))
         (si (1- start))
         (ei (1- end)))
    (cond
     ((<= ei blen) (substring (nelisp-buffer-before-gap b) si ei))
     ((>= si blen) (substring (nelisp-buffer-after-gap b) (- si blen) (- ei blen)))
     (t (concat (substring (nelisp-buffer-before-gap b) si)
                (substring (nelisp-buffer-after-gap b) 0 (- ei blen)))))))

(defun nelisp-char-after (&optional pos buf)
  "Return the character at POS (default point) in BUF, or nil."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (blen (progn (when (nelisp-buffer-ins b) (nelisp-buffer--settle b t))
                      (nelisp-buffer-blen b)))
         (idx (1- (or pos (nelisp-buffer-pend b) (1+ blen)))))
    (and (>= idx 0) (< idx (nelisp-buffer-len b))
         (if (< idx blen)
             (aref (nelisp-buffer-before-gap b) idx)
           (aref (nelisp-buffer-after-gap b) (- idx blen))))))

(defun nelisp-char-before (&optional pos buf)
  "Return the character before POS (default point) in BUF, or nil.
The character before POS sits at POS - 2 in the 0-based string."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (blen (progn (when (nelisp-buffer-ins b) (nelisp-buffer--settle b t))
                      (nelisp-buffer-blen b)))
         (idx (- (or pos (nelisp-buffer-pend b) (1+ blen)) 2)))
    (and (>= idx 0) (< idx (nelisp-buffer-len b))
         (if (< idx blen)
             (aref (nelisp-buffer-before-gap b) idx)
           (aref (nelisp-buffer-after-gap b) (- idx blen))))))

;;; Marker / overlay / text-property shift helpers -------------------
;;
;; Phase 5-B.2 replaces the Phase 5-B.1 cons-cell placeholder with
;; full marker/overlay structs + a sparse text-property interval
;; list.  Every mutation path (insert / delete / erase) walks all
;; three registries so position-following invariants hold.

(defun nelisp-buffer--shift-markers-on-insert (buf at inserted-len)
  "Advance markers at or past AT by INSERTED-LEN.
Markers strictly before AT are untouched.  A marker exactly at AT
advances only when its `insertion-type' is non-nil (Emacs
semantics)."
  (dolist (m (nelisp-buffer-markers buf))
    (when (nelisp-marker-p m)
      (let ((pos (nelisp-marker-position m)))
        (cond
         ((< pos at) nil)
         ((and (= pos at) (null (nelisp-marker-insertion-type m))) nil)
         (t (setf (nelisp-marker-position m) (+ pos inserted-len))))))))

(defun nelisp-buffer--shift-markers-on-delete (buf start end)
  "Collapse markers inside [START, END] to START, shift markers past END."
  (let ((delta (- end start)))
    (dolist (m (nelisp-buffer-markers buf))
      (when (nelisp-marker-p m)
        (let ((pos (nelisp-marker-position m)))
          (cond
           ((<= pos start) nil)
           ((>= pos end)
            (setf (nelisp-marker-position m) (- pos delta)))
           (t
            (setf (nelisp-marker-position m) start))))))))

(defun nelisp-buffer--shift-overlays-on-insert (buf at inserted-len)
  "Update overlay endpoints when INSERTED-LEN chars land at AT."
  (dolist (o (nelisp-buffer-overlays buf))
    (when (nelisp-overlay-p o)
      (let ((s (nelisp-overlay-start o))
            (e (nelisp-overlay-end o)))
        ;; START endpoint
        (cond
         ((< s at) nil)
         ((and (= s at) (null (nelisp-overlay-front-advance o))) nil)
         (t (setf (nelisp-overlay-start o) (+ s inserted-len))))
        ;; END endpoint
        (cond
         ((< e at) nil)
         ((and (= e at) (null (nelisp-overlay-rear-advance o))) nil)
         (t (setf (nelisp-overlay-end o) (+ e inserted-len))))))))

(defun nelisp-buffer--shift-overlays-on-delete (buf start end)
  "Collapse overlay endpoints falling in [START, END] to START;
shift endpoints past END backwards by (END - START)."
  (let ((delta (- end start)))
    (dolist (o (nelisp-buffer-overlays buf))
      (when (nelisp-overlay-p o)
        (let ((s (nelisp-overlay-start o))
              (e (nelisp-overlay-end o)))
          (setf (nelisp-overlay-start o)
                (cond
                 ((<= s start) s)
                 ((>= s end) (- s delta))
                 (t start)))
          (setf (nelisp-overlay-end o)
                (cond
                 ((<= e start) e)
                 ((>= e end) (- e delta))
                 (t start))))))))

(defun nelisp-buffer--shift-text-properties-on-insert (buf at len)
  "Expand text-property intervals straddling AT; shift those past AT."
  (dolist (ival (nelisp-buffer-text-properties buf))
    (let ((s (nth 0 ival))
          (e (nth 1 ival)))
      ;; START endpoint
      (cond ((< s at) nil)
            (t (setcar ival (+ s len))))
      ;; END endpoint (in-place via (nth 1) update)
      (cond ((< e at) nil)
            ((= e at) nil)
            (t (setcar (cdr ival) (+ e len)))))))

(defun nelisp-buffer--shift-text-properties-on-delete (buf start end)
  "Collapse text-property intervals within [START, END] and shift later ones."
  (let ((delta (- end start)))
    (dolist (ival (nelisp-buffer-text-properties buf))
      (let ((s (nth 0 ival))
            (e (nth 1 ival)))
        (setcar ival
                (cond
                 ((<= s start) s)
                 ((>= s end) (- s delta))
                 (t start)))
        (setcar (cdr ival)
                (cond
                 ((<= e start) e)
                 ((>= e end) (- e delta))
                 (t start)))))))

;;; Mutation ----------------------------------------------------------

(defun nelisp-goto-char (pos &optional buf)
  "Move point to POS in BUF.  POS is clamped into [point-min,
point-max] per Emacs semantics.  Records the move in the PEND slot --
O(1) -- instead of physically re-splitting `before-gap'/`after-gap'.
Does not bump the tick: the concatenated text is unchanged."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (clamped (max (or (nelisp-buffer-narrow-start b) 1)
                       (min (or (nelisp-buffer-narrow-end b)
                                (1+ (nelisp-buffer-len b)))
                            pos))))
    (setf (nelisp-buffer-pend b)
          (if (= clamped (1+ (nelisp-buffer-blen b))) nil clamped))
    clamped))

(defun nelisp-insert (text &optional buf)
  "Insert TEXT at point in BUF.  TEXT must be a string.
Markers / overlays / text-property intervals at or past point
advance by the length of TEXT; anything strictly before point is
untouched.  Settles any pending `goto-char' first (see
`nelisp-buffer--settle'): the gap must sit at point.  TEXT is then
queued in the INS slot and joined to `before-gap' lazily, so a run of
inserts costs one concat."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer"))))
    (when (nelisp-buffer-pend b)
      (nelisp-buffer--settle b))
    (let* ((blen (nelisp-buffer-blen b))
           (at (1+ blen))
           (n (length text)))
      (setf (nelisp-buffer-ins b) (cons text (nelisp-buffer-ins b)))
      (setf (nelisp-buffer-blen b) (+ blen n))
      (setf (nelisp-buffer-len b) (+ (nelisp-buffer-len b) n))
      (when (nelisp-buffer-narrow-end b)
        (setf (nelisp-buffer-narrow-end b)
              (+ (nelisp-buffer-narrow-end b) n)))
      (setf (nelisp-buffer-modified b) t)
      (setf (nelisp-buffer-tick b) (1+ (nelisp-buffer-tick b)))
      ;; Empty metadata is the common case; skip the interpreted helpers then.
      (when (nelisp-buffer-markers b)
        (nelisp-buffer--shift-markers-on-insert b at n))
      (when (nelisp-buffer-overlays b)
        (nelisp-buffer--shift-overlays-on-insert b at n))
      (when (nelisp-buffer-text-properties b)
        (nelisp-buffer--shift-text-properties-on-insert b at n))))
  nil)

(defun nelisp-delete-region (start end &optional buf)
  "Delete the text between 1-based START and END (exclusive) in BUF."
  (let* ((b (or buf nelisp-buffer--current (error "No NeLisp current buffer")))
         (len (nelisp-buffer-len b))
         (blen (progn (when (nelisp-buffer-ins b) (nelisp-buffer--settle b t))
                      (nelisp-buffer-blen b)))
         (point-before (or (nelisp-buffer-pend b) (1+ blen)))
         (s (min start end))
         (e (max start end)))
    (when (or (< s 1) (> e (1+ len)))
      (signal 'args-out-of-range (list start end)))
    (let* ((si (1- s))
           (ei (1- e))
           (delta (- e s))
           (old-min (nelisp-buffer-narrow-start b))
           (old-max (nelisp-buffer-narrow-end b))
           (new-point (cond ((<= point-before s) point-before)
                            ((>= point-before e) (- point-before delta))
                            (t s))))
      ;; Cut [si, ei) out of whichever gap string holds it; only the
      ;; string(s) touching the cut are copied.
      (cond
       ((<= ei blen)
        (let ((before (nelisp-buffer-before-gap b)))
          (setf (nelisp-buffer-before-gap b)
                (concat (substring before 0 si) (substring before ei)))
          (setf (nelisp-buffer-blen b) (- blen delta))))
       ((>= si blen)
        (let ((after (nelisp-buffer-after-gap b)))
          (setf (nelisp-buffer-after-gap b)
                (concat (substring after 0 (- si blen))
                        (substring after (- ei blen))))))
       (t
        (setf (nelisp-buffer-before-gap b)
              (substring (nelisp-buffer-before-gap b) 0 si))
        (setf (nelisp-buffer-after-gap b)
              (substring (nelisp-buffer-after-gap b) (- ei blen)))
        (setf (nelisp-buffer-blen b) si)))
      (setf (nelisp-buffer-len b) (- len delta))
      (when old-min
        (setf (nelisp-buffer-narrow-start b)
              (cond ((<= old-min s) old-min)
                    ((>= old-min e) (- old-min delta))
                    (t s))))
      (when old-max
        (setf (nelisp-buffer-narrow-end b)
              (cond ((<= old-max s) old-max)
                    ((>= old-max e) (- old-max delta))
                    (t s))))
      (setf (nelisp-buffer-modified b) t)
      (setf (nelisp-buffer-tick b) (1+ (nelisp-buffer-tick b)))
      (when (nelisp-buffer-markers b)
        (nelisp-buffer--shift-markers-on-delete b s e))
      (when (nelisp-buffer-overlays b)
        (nelisp-buffer--shift-overlays-on-delete b s e))
      (when (nelisp-buffer-text-properties b)
        (nelisp-buffer--shift-text-properties-on-delete b s e))
      (nelisp-goto-char new-point b)))
  nil)

(defun nelisp-erase-buffer (&optional buf)
  "Clear BUF entirely.  Markers / overlays collapse to `point-min'."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-before-gap b) "")
    (setf (nelisp-buffer-after-gap b) "")
    (setf (nelisp-buffer-ins b) nil)
    (setf (nelisp-buffer-blen b) 0)
    (setf (nelisp-buffer-len b) 0)
    (setf (nelisp-buffer-narrow-start b) nil)
    (setf (nelisp-buffer-narrow-end b) nil)
    (setf (nelisp-buffer-modified b) t)
    (nelisp-buffer--bump-tick b)
    ;; A pending point from before the erase would otherwise be read back
    ;; by `nelisp-point' as if still valid -- out of range for the now-
    ;; empty buffer.
    (setf (nelisp-buffer-pend b) nil)
    (dolist (m (nelisp-buffer-markers b))
      (when (nelisp-marker-p m)
        (setf (nelisp-marker-position m) 1)))
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlay-p o)
        (setf (nelisp-overlay-start o) 1)
        (setf (nelisp-overlay-end o) 1)))
    (setf (nelisp-buffer-text-properties b) nil))
  nil)

(defun nelisp-buffer-modified-p (&optional buf)
  "Return non-nil if BUF has been modified since creation/last reset."
  (nelisp-buffer-modified (nelisp-buffer--ambient buf)))

(defun nelisp-buffer-set-modified (flag &optional buf)
  "Set BUF's modified flag to FLAG (t/nil)."
  (setf (nelisp-buffer-modified (nelisp-buffer--ambient buf))
        (and flag t))
  flag)

;;; Narrowing ---------------------------------------------------------

(defun nelisp-narrow-to-region (start end &optional buf)
  "Restrict visible range of BUF to [START, END].
Inverted ranges are swapped; START is clamped to >= 1 and END to
<= `(1+ buffer-size)'."
  (let* ((b (nelisp-buffer--ambient buf))
         (size (nelisp-buffer-size b))
         (lo 1)
         (hi (1+ size))
         (s (max lo (min hi (min start end))))
         (e (max lo (min hi (max start end)))))
    (setf (nelisp-buffer-narrow-start b) s)
    (setf (nelisp-buffer-narrow-end b) e)
    (nelisp-goto-char (nelisp-point b) b)
    nil))

(defun nelisp-widen (&optional buf)
  "Remove the narrowing of BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-narrow-start b) nil)
    (setf (nelisp-buffer-narrow-end b) nil))
  nil)

(defmacro nelisp-save-restriction (&rest body)
  "Evaluate BODY saving/restoring the current buffer's narrowing."
  (declare (indent 0))
  (let ((buf (make-symbol "buf"))
        (start (make-symbol "start"))
        (end (make-symbol "end")))
    `(let* ((,buf (nelisp-buffer--ambient nil))
            (,start (nelisp-buffer-narrow-start ,buf))
            (,end (nelisp-buffer-narrow-end ,buf)))
       (unwind-protect (progn ,@body)
         (setf (nelisp-buffer-narrow-start ,buf) ,start)
         (setf (nelisp-buffer-narrow-end ,buf) ,end)))))

(defmacro nelisp-save-excursion (&rest body)
  "Evaluate BODY saving/restoring the current buffer's point."
  (declare (indent 0))
  (let ((buf (make-symbol "buf"))
        (saved (make-symbol "saved")))
    `(let* ((,buf (nelisp-buffer--ambient nil))
            (,saved (nelisp-point ,buf)))
       (unwind-protect (progn ,@body)
         (nelisp-goto-char ,saved ,buf)))))

;;; Marker API (Phase 5-B.2) ------------------------------------------

(defun nelisp-markerp (obj)
  "Return non-nil when OBJ is a `nelisp-marker'."
  (nelisp-marker-p obj))

(defun nelisp-make-marker ()
  "Return a marker not yet attached to any buffer."
  (nelisp-marker--make))

(defun nelisp-copy-marker (buf pos &optional insertion-type)
  "Return a fresh marker inside BUF at POS.
INSERTION-TYPE t makes the marker advance on insertion at its
position; nil keeps it put."
  (let ((m (nelisp-marker--make :buffer buf
                                :position pos
                                :insertion-type insertion-type)))
    (push m (nelisp-buffer-markers buf))
    m))

(defun nelisp-set-marker (marker pos &optional buf)
  "Re-point MARKER at POS, optionally moving it to BUF.
Migration unlinks from the old buffer's markers list and links
into the new one.  POS nil detaches the marker from its buffer."
  (cond
   ((null pos)
    (when-let* ((old (nelisp-marker-buffer marker)))
      (setf (nelisp-buffer-markers old)
            (delq marker (nelisp-buffer-markers old))))
    (setf (nelisp-marker-buffer marker) nil)
    (setf (nelisp-marker-position marker) 1))
   (t
    (let ((target (or buf (nelisp-marker-buffer marker))))
      (unless target (error "nelisp-set-marker: no target buffer"))
      (when (and (nelisp-marker-buffer marker)
                 (not (eq target (nelisp-marker-buffer marker))))
        (setf (nelisp-buffer-markers (nelisp-marker-buffer marker))
              (delq marker (nelisp-buffer-markers
                            (nelisp-marker-buffer marker))))
        (push marker (nelisp-buffer-markers target)))
      (unless (nelisp-marker-buffer marker)
        (push marker (nelisp-buffer-markers target)))
      (setf (nelisp-marker-buffer marker) target)
      (setf (nelisp-marker-position marker) pos))))
  marker)

(defun nelisp-marker-delete (marker)
  "Unlink MARKER from its buffer's marker list.  Returns nil."
  (when-let* ((b (nelisp-marker-buffer marker)))
    (setf (nelisp-buffer-markers b)
          (delq marker (nelisp-buffer-markers b))))
  (setf (nelisp-marker-buffer marker) nil)
  nil)

;;; Markers in arithmetic --------------------------------------------
;;
;; In Emacs a marker IS a valid argument to the arithmetic and comparison
;; primitives -- that is what the predicate `number-or-marker-p' in their
;; error message is naming.  Here a marker is an ordinary struct, so the
;; native `<' cannot know about it and signals `wrong-type-argument
;; number-or-marker-p'.  Real code hits this immediately: DDSKK's
;; `skk-start-henkan' guards with `(< pos skk-henkan-start-point)' where
;; the second operand is a marker set through `skk-set-marker', and the
;; conversion path died there on every input (found 2026-08-31 behind a
;; TSF-host E2E assertion that had been failing earlier for an unrelated
;; reason, so nothing had ever reached it).
;;
;; The wrappers below are installed HERE, in the file that introduces
;; markers, rather than in the prelude: a program that never loads buffer
;; support keeps the bare primitives and pays nothing.
;;
;; Cost: these are the hottest primitives in the system, and this runtime
;; charges roughly one basic operation per call, so a wrapper that merely
;; forwards would make every comparison in every loop measurably slower.
;; Hence the shape: the ordinary two-number case is decided by one
;; `integerp' pair and goes straight to the native subr, and only an
;; argument that is NOT a number reaches the coercion path.  Measure any
;; change to this shape -- see docs/design/201's engine-load numbers for
;; the harness.
(defvar nelisp-marker--native-ops nil
  "Alist of (SYMBOL . NATIVE-SUBR) captured before wrapping.")

(defun nelisp-marker--num (x)
  "Return X as a number, taking a marker's position."
  (if (nelisp-marker-p x)
      (or (nelisp-marker-position x)
          (signal 'error (list "Marker does not point anywhere" x)))
    x))

(defmacro nelisp-marker--defarithn (name)
  "Define NAME as a marker-tolerant wrapper around the native variadic NAME."
  (let ((native (intern (concat "nelisp-marker--native-" (symbol-name name)))))
    `(progn
       (declare-function ,native nil)
       (unless (assq ',name nelisp-marker--native-ops)
         (defalias ',native (symbol-function ',name))
         (push (cons ',name (symbol-function ',name)) nelisp-marker--native-ops)
         (defun ,name (&rest args)
           (apply #',native (mapcar #'nelisp-marker--num args)))))))

(defmacro nelisp-marker--defarith1 (name)
  "Define NAME as a marker-tolerant wrapper around the native 1-arg NAME."
  (let ((native (intern (concat "nelisp-marker--native-" (symbol-name name)))))
    `(progn
       (declare-function ,native nil)
       (unless (assq ',name nelisp-marker--native-ops)
         (defalias ',native (symbol-function ',name))
         (push (cons ',name (symbol-function ',name)) nelisp-marker--native-ops)
         (defun ,name (a)
           (if (numberp a) (,native a) (,native (nelisp-marker--num a))))))))

(defmacro nelisp-marker--defarith2 (name)
  "Define NAME as a marker-tolerant wrapper around the native NAME.
Two-argument fast path first; anything else falls back to `apply'."
  (let ((native (intern (concat "nelisp-marker--native-" (symbol-name name)))))
    `(progn
       (declare-function ,native nil)
       (unless (assq ',name nelisp-marker--native-ops)
         (defalias ',native (symbol-function ',name))
         (push (cons ',name (symbol-function ',name)) nelisp-marker--native-ops)
         (defun ,name (a b &rest more)
           (if (and (null more) (numberp a) (numberp b))
               (,native a b)
             (apply #',native (nelisp-marker--num a) (nelisp-marker--num b)
                    (mapcar #'nelisp-marker--num more))))))))

(defun nelisp-marker-install-arithmetic ()
  "Make the arithmetic and comparison primitives accept markers.
Idempotent; returns the list of names wrapped."
  ;; `<' is deliberately NOT wrapped.  Interleaved A/B on the DDSKK engine
  ;; load, three pairs: wrapping it costs 1.30x, 1.41x, 1.42x.  It is the
  ;; hottest primitive in the system and a wrapper cannot be cheaper than
  ;; one extra call, which this runtime charges a full basic operation for
  ;; (~262us, docs/design/201).  A caller that needs to compare a marker
  ;; should not put one in front of `<' -- see `skk-set-marker' in
  ;; nelisp-skk-ime's compat layer for how that is done there.  Wrapping
  ;; `+'/`-'/`=' as well measured 2.8-3.6x and is out of the question.
  (nelisp-marker--defarith1 1-)
  (nelisp-marker--defarithn max)
  (nelisp-marker--defarithn min)
  (mapcar #'car nelisp-marker--native-ops))

(nelisp-marker-install-arithmetic)

;;; Overlay API (Phase 5-B.2) -----------------------------------------

(defun nelisp-overlayp (obj)
  "Return non-nil when OBJ is a `nelisp-overlay'."
  (nelisp-overlay-p obj))

(defun nelisp-make-overlay (start end &optional buf
                                  front-advance rear-advance)
  "Create an overlay covering [START, END) in BUF.
FRONT-ADVANCE / REAR-ADVANCE control the behaviour of the
respective endpoints under insertion at that exact position,
mirroring Emacs' `make-overlay' last two args."
  (let* ((b (nelisp-buffer--ambient buf))
         (o (nelisp-overlay--make :buffer b :start start :end end
                                  :front-advance front-advance
                                  :rear-advance rear-advance)))
    (push o (nelisp-buffer-overlays b))
    o))

(defun nelisp-delete-overlay (o)
  "Unlink O from its buffer's overlay list.  Returns nil."
  (when-let* ((b (nelisp-overlay-buffer o)))
    (setf (nelisp-buffer-overlays b)
          (delq o (nelisp-buffer-overlays b))))
  (setf (nelisp-overlay-buffer o) nil)
  nil)

(defun nelisp-overlay-put (o prop val)
  "Store (PROP . VAL) on overlay O.  Returns VAL."
  (let ((cell (assq prop (nelisp-overlay-props o))))
    (if cell
        (setcdr cell val)
      (push (cons prop val) (nelisp-overlay-props o))))
  val)

(defun nelisp-overlay-get (o prop)
  "Return the value of PROP stored on O, or nil."
  (cdr (assq prop (nelisp-overlay-props o))))

(defun nelisp-overlays-at (pos &optional buf)
  "Return the list of overlays in BUF whose range covers POS."
  (let ((b (nelisp-buffer--ambient buf))
        result)
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlayp o)
        (when (and (>= pos (nelisp-overlay-start o))
                   (< pos (nelisp-overlay-end o)))
          (push o result))))
    (nreverse result)))

(defun nelisp-overlays-in (start end &optional buf)
  "Return the list of overlays in BUF overlapping [START, END)."
  (let ((b (nelisp-buffer--ambient buf))
        result)
    (dolist (o (nelisp-buffer-overlays b))
      (when (nelisp-overlayp o)
        (let ((s (nelisp-overlay-start o))
              (e (nelisp-overlay-end o)))
          (when (and (< s end) (> e start))
            (push o result)))))
    (nreverse result)))

;;; Text-property API (Phase 5-B.2 sparse list) ----------------------

(defun nelisp-put-text-property (start end prop val &optional buf)
  "Store PROP=VAL for text in [START, END) of BUF.
The representation is a list of (S E PLIST) intervals; subsequent
puts are prepended, so `nelisp-get-text-property' sees the most
recent write first (shadowing model)."
  (let ((b (nelisp-buffer--ambient buf)))
    (push (list start end (list prop val))
          (nelisp-buffer-text-properties b))
    val))

(defun nelisp-get-text-property (pos prop &optional buf)
  "Return the value of PROP at POS in BUF, or nil.
Walks intervals newest-first; the first covering POS with PROP
set wins.  `plist-member' is used to distinguish \"prop missing\"
from \"prop set to nil\" so the newest-interval-wins semantics
don't get shadowed by an old nil write."
  (let ((b (nelisp-buffer--ambient buf))
        (hit nil))
    (dolist (ival (nelisp-buffer-text-properties b))
      (unless hit
        (let ((s (nth 0 ival))
              (e (nth 1 ival))
              (pl (nth 2 ival)))
          (when (and (>= pos s) (< pos e)
                     (plist-member pl prop))
            (setq hit (cons :v (plist-get pl prop)))))))
    (and hit (cdr hit))))

(defun nelisp-text-property-intervals (&optional buf)
  "Return a shallow copy of BUF's text-property interval list."
  (copy-sequence
   (nelisp-buffer-text-properties (nelisp-buffer--ambient buf))))

(defun nelisp-remove-text-properties (start end props &optional buf)
  "Drop each key in PROPS (a plain list of symbols) from any
interval overlapping [START, END) in BUF.  Intervals are not
split; properties simply disappear from intervals that touch the
removal window.  Returns nil."
  (let ((b (nelisp-buffer--ambient buf)))
    (dolist (ival (nelisp-buffer-text-properties b))
      (let ((s (nth 0 ival))
            (e (nth 1 ival)))
        (when (and (< s end) (> e start))
          (let* ((pl (nth 2 ival))
                 (new (let (out)
                        (while pl
                          (unless (memq (car pl) props)
                            (push (car pl) out)
                            (push (cadr pl) out))
                          (setq pl (cddr pl)))
                        (nreverse out))))
            (setcar (cddr ival) new))))))
  nil)

(provide 'nelisp-buffer)

;;; nelisp-buffer.el ends here
