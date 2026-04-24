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

(defun nelisp-buffer--reset-registry ()
  "Clear the NeLisp buffer registry.  Test hygiene only."
  (clrhash nelisp-buffer--registry)
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
  buf)

(defmacro nelisp-with-buffer (buf &rest body)
  "Evaluate BODY with BUF as the NeLisp current buffer.
Dynamically rebinds `nelisp-buffer--current' so nested
`with-buffer' forms stack correctly."
  (declare (indent 1))
  `(let ((nelisp-buffer--current ,buf))
     ,@body))

(defun nelisp-buffer--ambient (buf-or-nil)
  "Resolve BUF-OR-NIL to an actual buffer (defaulting to current).
Signals `error' when neither argument nor current is set."
  (or buf-or-nil nelisp-buffer--current
      (error "No NeLisp current buffer")))

;;; Size / position ---------------------------------------------------

(defun nelisp-buffer-size (&optional buf)
  "Return the length of BUF's visible (unrestricted) text."
  (let ((b (nelisp-buffer--ambient buf)))
    (+ (length (nelisp-buffer-before-gap b))
       (length (nelisp-buffer-after-gap b)))))

(defun nelisp-point (&optional buf)
  "Return the current point in BUF (1-based)."
  (1+ (length (nelisp-buffer-before-gap
               (nelisp-buffer--ambient buf)))))

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
  "Return the entire text of BUF as a new string."
  (let ((b (nelisp-buffer--ambient buf)))
    (concat (nelisp-buffer-before-gap b)
            (nelisp-buffer-after-gap b))))

(defun nelisp-buffer-substring (start end &optional buf)
  "Return the substring between 1-based START and END in BUF."
  (let ((b (nelisp-buffer--ambient buf)))
    (substring (nelisp-buffer-string b) (1- start) (1- end))))

(defun nelisp-char-after (&optional pos buf)
  "Return the character at POS (default point) in BUF, or nil."
  (let* ((b (nelisp-buffer--ambient buf))
         (p (or pos (nelisp-point b)))
         (total (nelisp-buffer-string b))
         (idx (1- p)))
    (and (>= idx 0) (< idx (length total))
         (elt total idx))))

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
  "Move point to POS in BUF, rebalancing the gap.
POS is clamped into [point-min, point-max] per Emacs semantics."
  (let* ((b (nelisp-buffer--ambient buf))
         (total (nelisp-buffer-string b))
         (lo (nelisp-point-min b))
         (hi (nelisp-point-max b))
         (clamped (max lo (min hi pos)))
         (idx (1- clamped)))
    (setf (nelisp-buffer-before-gap b) (substring total 0 idx))
    (setf (nelisp-buffer-after-gap b) (substring total idx))
    clamped))

(defun nelisp-insert (text &optional buf)
  "Insert TEXT at point in BUF.  TEXT must be a string.
Markers / overlays / text-property intervals at or past point
advance by the length of TEXT; anything strictly before point is
untouched."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let* ((b (nelisp-buffer--ambient buf))
         (before (nelisp-buffer-before-gap b))
         (at (1+ (length before)))
         (n (length text)))
    (setf (nelisp-buffer-before-gap b) (concat before text))
    (setf (nelisp-buffer-modified b) t)
    (nelisp-buffer--shift-markers-on-insert b at n)
    (nelisp-buffer--shift-overlays-on-insert b at n)
    (nelisp-buffer--shift-text-properties-on-insert b at n))
  nil)

(defun nelisp-delete-region (start end &optional buf)
  "Delete the text between 1-based START and END in BUF.
END is exclusive per Emacs convention.  Signals `args-out-of-range'
if the range is inverted or outside the buffer."
  (let* ((b (nelisp-buffer--ambient buf))
         (size (nelisp-buffer-size b))
         (lo 1)
         (hi (1+ size))
         (s (min start end))
         (e (max start end)))
    (when (or (< s lo) (> e hi))
      (signal 'args-out-of-range (list start end)))
    (let* ((total (nelisp-buffer-string b))
           (si (1- s))
           (ei (1- e)))
      (setf (nelisp-buffer-before-gap b) (substring total 0 si))
      (setf (nelisp-buffer-after-gap b) (substring total ei))
      (setf (nelisp-buffer-modified b) t)
      (nelisp-buffer--shift-markers-on-delete b s e)
      (nelisp-buffer--shift-overlays-on-delete b s e)
      (nelisp-buffer--shift-text-properties-on-delete b s e)))
  nil)

(defun nelisp-erase-buffer (&optional buf)
  "Clear BUF entirely.  Markers / overlays collapse to `point-min'."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-before-gap b) "")
    (setf (nelisp-buffer-after-gap b) "")
    (setf (nelisp-buffer-modified b) t)
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
