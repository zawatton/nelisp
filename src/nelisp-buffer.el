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
  (text-properties nil))  ; list of (START END PROP-PLIST) intervals, §3.2

(cl-defstruct (nelisp-marker
               (:constructor nelisp-marker--make)
               (:copier nil))
  "Position reference that follows text mutation of a `nelisp-buffer'.
INSERTION-TYPE t means the marker advances when text is inserted
exactly at its position; nil means it stays put."
  (buffer nil)
  (position 1)
  (insertion-type nil))

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
  "Invalidate BUF's cached whole-buffer text (see `nelisp-buffer--tick')."
  (puthash buf (1+ (gethash buf nelisp-buffer--tick 0)) nelisp-buffer--tick))

(defvar nelisp-buffer--pending-point (make-hash-table :test 'eq)
  "BUF -> a point value `nelisp-goto-char' recorded but has not yet
physically moved BUF's gap to match.  Absent means the gap already
sits exactly at BUF's current point (`(1+ (length before-gap))' is
already correct).  See `nelisp-buffer--settle' and the PERF block
comment above `nelisp-buffer--text-cache'.")

(defun nelisp-buffer--settle (buf)
  "Physically move BUF's gap to its pending point, if one is
outstanding, then clear it.  O(distance from the gap's CURRENT
physical position to the pending target) -- paid once, lazily, right
before an operation (`nelisp-insert' et al.) that needs the gap
actually at point, rather than on every `goto-char' that got it there."
  (let ((pending (gethash buf nelisp-buffer--pending-point)))
    (when pending
      (let* ((total (nelisp-buffer-string buf))
             (idx (1- pending)))
        (setf (nelisp-buffer-before-gap buf) (substring total 0 idx))
        (setf (nelisp-buffer-after-gap buf) (substring total idx)))
      (remhash buf nelisp-buffer--pending-point))))

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
         (count 0))
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
  "Return a list of live NeLisp buffers."
  (let (result)
    (maphash (lambda (_ buf) (push buf result))
             nelisp-buffer--registry)
    result))

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
  "Cached `(length (nelisp-buffer-before-gap BUF))'; see the PERF
comment above."
  (let* ((tick (gethash buf nelisp-buffer--tick 0))
         (cached (gethash buf nelisp-buffer--blen-cache)))
    (if (and cached (= (car cached) tick))
        (cdr cached)
      (let ((blen (length (nelisp-buffer-before-gap buf))))
        (puthash buf (cons tick blen) nelisp-buffer--blen-cache)
        blen))))

(defun nelisp-buffer-size (&optional buf)
  "Return the length of BUF's visible (unrestricted) text."
  (let* ((b (nelisp-buffer--ambient buf))
         (tick (gethash b nelisp-buffer--tick 0))
         (cached (gethash b nelisp-buffer--size-cache)))
    (if (and cached (= (car cached) tick))
        (cdr cached)
      (let ((sz (+ (nelisp-buffer--before-length b)
                   (length (nelisp-buffer-after-gap b)))))
        (puthash b (cons tick sz) nelisp-buffer--size-cache)
        sz))))

(defun nelisp-point (&optional buf)
  "Return the current point in BUF (1-based).
Prefers a pending, not-yet-settled `goto-char' target (see
`nelisp-buffer--pending-point') over the gap's physical position,
which is what makes that target correct without moving anything."
  (let ((b (nelisp-buffer--ambient buf)))
    (or (gethash b nelisp-buffer--pending-point)
        (1+ (nelisp-buffer--before-length b)))))

(defun nelisp-point-min (&optional buf)
  "Return the narrowed point-min of BUF (defaults to 1)."
  (or (nelisp-buffer-narrow-start
       (nelisp-buffer--ambient buf))
      1))

(defun nelisp-point-max (&optional buf)
  "Return the narrowed point-max of BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (or (nelisp-buffer-narrow-end b)
        (1+ (nelisp-buffer-size b)))))

(defun nelisp-buffer-string (&optional buf)
  "Return the entire text of BUF as a new string.
Memoized against `nelisp-buffer--tick' (see that variable and
`nelisp-buffer--text-cache'): O(1) whenever BUF's text has not
changed since the last call, O(buffer size) to rebuild otherwise.
The returned string may be the SAME object handed back on a later
cache hit, so treat it as read-only -- mutating it via `aset' would
corrupt what that later call sees."
  (let* ((b (nelisp-buffer--ambient buf))
         (tick (gethash b nelisp-buffer--tick 0))
         (cached (gethash b nelisp-buffer--text-cache)))
    (if (and cached (= (car cached) tick))
        (cdr cached)
      (let ((s (concat (nelisp-buffer-before-gap b)
                        (nelisp-buffer-after-gap b))))
        (puthash b (cons tick s) nelisp-buffer--text-cache)
        s))))

(defun nelisp-buffer-substring (start end &optional buf)
  "Return the substring between 1-based START and END in BUF.
Slices directly from `before-gap'/`after-gap' (concatenating the two
only when [START, END) itself straddles the gap boundary), costing
O(END - START) regardless of BUF's total size."
  (let* ((b (nelisp-buffer--ambient buf))
         (before (nelisp-buffer-before-gap b))
         (blen (nelisp-buffer--before-length b))
         (si (1- start))
         (ei (1- end)))
    (cond
     ((<= ei blen) (substring before si ei))
     ((>= si blen) (substring (nelisp-buffer-after-gap b) (- si blen) (- ei blen)))
     (t (concat (substring before si)
                (substring (nelisp-buffer-after-gap b) 0 (- ei blen)))))))

(defun nelisp-char-after (&optional pos buf)
  "Return the character at POS (default point) in BUF, or nil."
  (let* ((b (nelisp-buffer--ambient buf))
         (p (or pos (nelisp-point b)))
         (before (nelisp-buffer-before-gap b))
         (blen (nelisp-buffer--before-length b))
         (idx (1- p)))
    (and (>= idx 0) (< idx (nelisp-buffer-size b))
         (if (< idx blen) (aref before idx) (aref (nelisp-buffer-after-gap b) (- idx blen))))))

(defun nelisp-char-before (&optional pos buf)
  "Return the character before POS (default point) in BUF, or nil.
Doc 204 P1 -- no body for this existed in either copy before; written
in the same shape as `nelisp-char-after' just above, one index earlier
(the character before POS sits at POS - 2 in the 0-based string)."
  (let* ((b (nelisp-buffer--ambient buf))
         (p (or pos (nelisp-point b)))
         (before (nelisp-buffer-before-gap b))
         (blen (nelisp-buffer--before-length b))
         (idx (- p 2)))
    (and (>= idx 0) (< idx (nelisp-buffer-size b))
         (if (< idx blen) (aref before idx) (aref (nelisp-buffer-after-gap b) (- idx blen))))))

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
point-max] per Emacs semantics.  Records the move in
`nelisp-buffer--pending-point' -- O(1) -- instead of physically
re-splitting `before-gap'/`after-gap': with plain (immutable) strings,
producing a new `before-gap' of length K always costs O(K) via
`concat'/`substring', REGARDLESS of how much of that K is actually
new, so even moving the gap by one character costs O(current
position); a `search-forward' loop calling this once per match paid
that cost every match.  See the PERF block comment above
`nelisp-buffer--text-cache' for the measurement and full rationale.
Does not touch `nelisp-buffer--tick': the concatenated text is
unchanged, only which position a later `nelisp-insert' settles to."
  (let* ((b (nelisp-buffer--ambient buf))
         (lo (nelisp-point-min b))
         (hi (nelisp-point-max b))
         (clamped (max lo (min hi pos)))
         (physical (1+ (nelisp-buffer--before-length b))))
    (if (= clamped physical)
        (remhash b nelisp-buffer--pending-point)
      (puthash b clamped nelisp-buffer--pending-point))
    clamped))

(defun nelisp-insert (text &optional buf)
  "Insert TEXT at point in BUF.  TEXT must be a string.
Markers / overlays / text-property intervals at or past point
advance by the length of TEXT; anything strictly before point is
untouched.  Settles any pending `goto-char' first (see
`nelisp-buffer--settle'): this is the one place that genuinely needs
`before-gap' to already end exactly at point, since it appends TEXT
straight onto it."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let ((b (nelisp-buffer--ambient buf)))
    (nelisp-buffer--settle b)
    (let* ((before (nelisp-buffer-before-gap b))
           (at (1+ (length before)))
           (n (length text)))
      (setf (nelisp-buffer-before-gap b) (concat before text))
      (when (nelisp-buffer-narrow-end b)
        (setf (nelisp-buffer-narrow-end b)
              (+ (nelisp-buffer-narrow-end b) n)))
      (setf (nelisp-buffer-modified b) t)
      (nelisp-buffer--bump-tick b)
      ;; Empty metadata is the common case; skip the interpreted helpers then.
      (when (nelisp-buffer-markers b)
        (nelisp-buffer--shift-markers-on-insert b at n))
      (when (nelisp-buffer-overlays b)
        (nelisp-buffer--shift-overlays-on-insert b at n))
      (when (nelisp-buffer-text-properties b)
        (nelisp-buffer--shift-text-properties-on-insert b at n))))
  nil)

(defun nelisp-delete-region (start end &optional buf)
  "Delete the text between 1-based START and END in BUF.
END is exclusive per Emacs convention.  Signals `args-out-of-range'
if the range is inverted or outside the buffer."
  (let* ((b (nelisp-buffer--ambient buf))
         (size (nelisp-buffer-size b))
         (point-before (nelisp-point b))
         (lo 1)
         (hi (1+ size))
         (s (min start end))
         (e (max start end)))
    (when (or (< s lo) (> e hi))
      (signal 'args-out-of-range (list start end)))
    (let* ((total (nelisp-buffer-string b))
           (si (1- s))
           (ei (1- e))
           (delta (- e s))
           (old-min (nelisp-buffer-narrow-start b))
           (old-max (nelisp-buffer-narrow-end b))
           (new-point (cond ((<= point-before s) point-before)
                            ((>= point-before e) (- point-before delta))
                            (t s)))
           (map-position (lambda (position)
                           (cond ((<= position s) position)
                                 ((>= position e) (- position delta))
                                 (t s)))))
      (setf (nelisp-buffer-before-gap b) (substring total 0 si))
      (setf (nelisp-buffer-after-gap b) (substring total ei))
      (when old-min
        (setf (nelisp-buffer-narrow-start b) (funcall map-position old-min)))
      (when old-max
        (setf (nelisp-buffer-narrow-end b) (funcall map-position old-max)))
      (setf (nelisp-buffer-modified b) t)
      (nelisp-buffer--bump-tick b)
      (nelisp-buffer--shift-markers-on-delete b s e)
      (nelisp-buffer--shift-overlays-on-delete b s e)
      (nelisp-buffer--shift-text-properties-on-delete b s e)
      (nelisp-goto-char new-point b)))
  nil)

(defun nelisp-erase-buffer (&optional buf)
  "Clear BUF entirely.  Markers / overlays collapse to `point-min'."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-before-gap b) "")
    (setf (nelisp-buffer-after-gap b) "")
    (setf (nelisp-buffer-narrow-start b) nil)
    (setf (nelisp-buffer-narrow-end b) nil)
    (setf (nelisp-buffer-modified b) t)
    (nelisp-buffer--bump-tick b)
    ;; A pending point from before the erase would otherwise be read back
    ;; by `nelisp-point' as if still valid -- out of range for the now-
    ;; empty buffer, since `before-gap'/`after-gap' just went to "".
    (remhash b nelisp-buffer--pending-point)
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
