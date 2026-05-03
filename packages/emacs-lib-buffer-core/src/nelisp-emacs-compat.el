;;; nelisp-emacs-compat.el --- Emacs editor API on NeLisp (MVP)  -*- lexical-binding: t; -*-

;; Phase 9a MVP per Doc 33 LOCKED-2026-04-25-v2 §4.2a.
;; Layer: extension package (Layer 2 per Doc 33 v2 §3.1.0).
;; Foundation: `nelisp-text-buffer' (T36 SHIPPED, gap-buffer 9 primitives).
;;
;; Goal: provide the *minimum* Emacs editor API surface (top 30 APIs)
;; required for `claude' self-extension via `anvil.el'.  Extension
;; packages outside NeLisp core can `(require 'nelisp-emacs-compat)' and
;; immediately call `nelisp-ec-*' to manipulate buffers, points, markers,
;; narrowing, and perform literal / regex search — without dragging in the
;; full Emacs C runtime.
;;
;; Prefix: `nelisp-ec-' (= NeLisp Emacs Compat) so that loading this
;; module inside a host Emacs does NOT shadow `current-buffer',
;; `point', `insert', etc.  Application code calls `nelisp-ec-insert',
;; `nelisp-ec-current-buffer', and so on.  When NeLisp finally hosts
;; itself the symbol mapping (= `nelisp-ec-insert' → `insert') happens
;; in the loader.
;;
;; API surface (37 public APIs after Phase 9a regex wire):
;;
;;   A. buffer registry + current buffer  (5)
;;      generate-new-buffer, current-buffer, set-buffer,
;;      with-current-buffer (macro), kill-buffer
;;
;;   B. point + cursor control  (7)
;;      point, point-min, point-max, goto-char,
;;      forward-char, backward-char, buffer-size
;;
;;   C. text editing  (6)
;;      insert, delete-region, delete-char, erase-buffer,
;;      buffer-substring, buffer-string
;;
;;   D. save-* family  (3 macros)
;;      save-excursion, save-restriction, save-current-buffer
;;
;;   E. narrowing  (2)
;;      narrow-to-region, widen
;;
;;   F. marker  (5)
;;      make-marker, set-marker, marker-position, marker-buffer,
;;      point-marker
;;
;;   G. search + match-data  (9)
;;      search-forward, search-backward, looking-at-p,
;;      re-search-forward, re-search-backward, looking-at,
;;      match-data, match-beginning, match-end
;;
;; Non-goals (deferred to later phases per spec):
;;   - full Emacs regexp compatibility beyond `nelisp-regex' MVP syntax
;;   - marker insertion-type (advance-on-insert) — markers in MVP are
;;     "static": they hold a position that does NOT auto-update when
;;     surrounding text is inserted/deleted.  This keeps the data flow
;;     simple and matches Doc 33 v2 §4.2a "simple marker" note.
;;   - window / frame / keymap / buffer-local variables / text-properties
;;   - undo / coding system
;;
;; Foundation contract (T36 frozen):
;;   T36 `nelisp-text-buffer' is treated as an opaque mutable text
;;   container.  We never `setf' its slots from this module — all text
;;   mutation goes through the public T36 API (`text-buffer-insert',
;;   `text-buffer-delete', `text-buffer-set-cursor', ...).  The cursor
;;   stored *inside* the underlying `nelisp-text-buffer' is treated as
;;   private state we drive from the buffer-level `point' (= a 1-based
;;   integer); we never read it back as the source of truth.

;;; Code:

(require 'cl-lib)
(require 'nelisp-regex)
(require 'nelisp-text-buffer)

;;; defstruct: buffer (Layer 2, wraps Layer 0 text-buffer)

(cl-defstruct (nelisp-ec-buffer
               (:constructor nelisp-ec-buffer--make-raw)
               (:copier nil)
               (:predicate nelisp-ec-buffer-p))
  "Editor-level buffer object (Phase 9a MVP, Doc 33 v2 §4.2a).

Wraps a `nelisp-text-buffer' (Layer 0 mutable text container) and
layers Emacs editor concepts on top: a 1-based POINT, a NAME, a
MODIFIED-P flag, and an optional NARROW-START / NARROW-END range.

Slots:
- NAME         : human-readable buffer name (string).  Disambiguated by
  `nelisp-ec-generate-new-buffer' so registry keys are unique.
- TEXT         : underlying `nelisp-text-buffer' (T36 SHIPPED).  Never
  mutate its slots from outside; always use the T36 public API.
- POINT        : 1-based char position of the cursor.  Range is
  [`point-min', `point-max'].  We treat this as the source of truth and
  push/pop it into the underlying text-buffer cursor (0-based) before
  every text mutation.
- NARROW-START : 1-based inclusive lower bound, or nil = widen.
- NARROW-END   : 1-based exclusive upper bound, or nil = widen.
- MODIFIED-P   : t once the buffer has been mutated (insert / delete /
  erase).  Reset by `nelisp-ec-erase-buffer' callers as needed.
- KILLED-P     : t after `nelisp-ec-kill-buffer'.  Operations on a
  killed buffer signal `nelisp-ec-buffer-killed'."
  (name         ""  :type string)
  (text         nil)
  (point        1   :type integer)
  (narrow-start nil)
  (narrow-end   nil)
  (modified-p   nil)
  (killed-p     nil))

;;; defstruct: marker

(cl-defstruct (nelisp-ec-marker
               (:constructor nelisp-ec-marker--make-raw)
               (:copier nil)
               (:predicate nelisp-ec-marker-p)
               (:conc-name nelisp-ec-marker--))
  "MVP marker (Phase 9a, Doc 33 v2 §4.2a).
Holds a (POSITION, BUFFER) pair.  In MVP markers are *static* — they
do NOT auto-update when surrounding text changes; advance-on-insert
behavior and insertion-type are deferred to Phase 9b.

Note the `:conc-name' is `nelisp-ec-marker--' to leave the public
function names `nelisp-ec-marker-position' and `nelisp-ec-marker-buffer'
free for the API surface; access slots only via the `--' accessors."
  (position nil)
  (buffer   nil))

;;; Errors

(define-error 'nelisp-ec-error "NeLisp emacs-compat error")
(define-error 'nelisp-ec-no-current-buffer
  "No current buffer" 'nelisp-ec-error)
(define-error 'nelisp-ec-buffer-killed
  "Buffer is killed" 'nelisp-ec-error)
(define-error 'nelisp-ec-args-out-of-range
  "Argument out of range" 'nelisp-ec-error)

;;; Internal state: registry + current buffer

(defvar nelisp-ec--buffers nil
  "Alist of (NAME . BUFFER) for all live buffers in this NeLisp world.
Killed buffers are removed.  This is the single source of truth for
`nelisp-ec-generate-new-buffer' name disambiguation.")

(defvar nelisp-ec--current-buffer nil
  "The currently-selected `nelisp-ec-buffer', or nil if none.")

(defvar nelisp-ec--match-data nil
  "Last successful match data in Emacs-compatible list form.

The representation is a flat list of 1-based buffer positions:
  (MATCH-START MATCH-END G1-START G1-END ...)

Unmatched optional groups are stored as nil pairs.  The list is updated
by successful literal/regex search APIs and by `nelisp-ec-looking-at'.")

(defun nelisp-ec--ensure-current ()
  "Return the current buffer or signal `nelisp-ec-no-current-buffer'."
  (or nelisp-ec--current-buffer
      (signal 'nelisp-ec-no-current-buffer nil)))

(defun nelisp-ec--check-live (buf)
  "Signal `nelisp-ec-buffer-killed' if BUF is killed."
  (when (nelisp-ec-buffer-killed-p buf)
    (signal 'nelisp-ec-buffer-killed (list (nelisp-ec-buffer-name buf)))))

(defun nelisp-ec--unique-name (base)
  "Return a buffer name based on BASE that is not in `nelisp-ec--buffers'.
If BASE is free, return it as-is; otherwise append =<2>=, =<3>=, ..."
  (if (null (assoc base nelisp-ec--buffers))
      base
    (let ((n 2))
      (while (assoc (format "%s<%d>" base n) nelisp-ec--buffers)
        (setq n (1+ n)))
      (format "%s<%d>" base n))))

(defun nelisp-ec--text (buf)
  "Return the underlying `nelisp-text-buffer' of BUF, asserting liveness."
  (nelisp-ec--check-live buf)
  (nelisp-ec-buffer-text buf))

(defun nelisp-ec--sync-cursor (buf)
  "Push BUF's 1-based POINT down into the T36 cursor (0-based).
Call this immediately before any text mutation on BUF's underlying
`nelisp-text-buffer', so that the gap is positioned correctly."
  (text-buffer-set-cursor (nelisp-ec--text buf)
                          (1- (nelisp-ec-buffer-point buf))))

(defun nelisp-ec--search-region (buf)
  "Return searchable region metadata for BUF as (BASE TEXT POINT-INDEX HI).

BASE is the absolute 1-based position corresponding to TEXT index 0.
TEXT spans the current visible region (`point-min'..`point-max').
POINT-INDEX is the current point as a 0-based index into TEXT.
HI is the absolute exclusive upper bound of the visible region."
  (let* ((base (or (nelisp-ec-buffer-narrow-start buf) 1))
         (hi (or (nelisp-ec-buffer-narrow-end buf)
                 (1+ (text-buffer-length (nelisp-ec--text buf)))))
         (text (text-buffer-substring (nelisp-ec--text buf)
                                      (1- base)
                                      (1- hi)))
         (point-index (- (nelisp-ec-buffer-point buf) base)))
    (list base text point-index hi)))

(defun nelisp-ec--set-match-data (data)
  "Store DATA as the current match-data and return DATA."
  (setq nelisp-ec--match-data data))

(defun nelisp-ec--set-simple-match-data (start end)
  "Store a whole-match START/END pair in `nelisp-ec--match-data'."
  (nelisp-ec--set-match-data (list start end)))

(defun nelisp-ec--rx-match-data-to-ec (base match)
  "Convert regex MATCH relative to BASE into Emacs-compatible match-data."
  (let ((data (list (+ base (plist-get match :start))
                    (+ base (plist-get match :end)))))
    (dolist (group (plist-get match :groups))
      (let ((gstart (plist-get group :start))
            (gend (plist-get group :end)))
        (setq data
              (append data
                      (list (and gstart (+ base gstart))
                            (and gend (+ base gend)))))))
    (nelisp-ec--set-match-data data)))

;;; A. buffer registry + current buffer  (5 APIs)

;;;###autoload
(defun nelisp-ec-generate-new-buffer (name)
  "Create and return a fresh buffer with a name derived from NAME.
Like Emacs `generate-new-buffer': if NAME is already in use the new
buffer is given a uniquified name (=NAME<2>=, =NAME<3>=, ...).
The new buffer is empty, has POINT = 1, and is registered in
`nelisp-ec--buffers'.  It does NOT become the current buffer."
  (unless (stringp name)
    (signal 'wrong-type-argument (list 'stringp name)))
  (let* ((unique (nelisp-ec--unique-name name))
         (tb (make-text-buffer))
         (buf (nelisp-ec-buffer--make-raw
               :name unique
               :text tb
               :point 1
               :narrow-start nil
               :narrow-end nil
               :modified-p nil
               :killed-p nil)))
    (push (cons unique buf) nelisp-ec--buffers)
    buf))

;;;###autoload
(defun nelisp-ec-current-buffer ()
  "Return the current buffer, or nil if none has been set."
  nelisp-ec--current-buffer)

;;;###autoload
(defun nelisp-ec-set-buffer (buf)
  "Make BUF the current buffer and return BUF."
  (unless (nelisp-ec-buffer-p buf)
    (signal 'wrong-type-argument (list 'nelisp-ec-buffer-p buf)))
  (nelisp-ec--check-live buf)
  (setq nelisp-ec--current-buffer buf)
  buf)

;;;###autoload
(defmacro nelisp-ec-with-current-buffer (buf &rest body)
  "Execute BODY with BUF as the current buffer, restoring afterwards.
Equivalent shape to Emacs `with-current-buffer'.  BUF is evaluated
once.  The previous current buffer is restored on normal exit, error,
or non-local exit (e.g. `throw')."
  (declare (indent 1) (debug (form body)))
  (let ((saved (make-symbol "saved"))
        (newbuf (make-symbol "newbuf")))
    `(let ((,saved nelisp-ec--current-buffer)
           (,newbuf ,buf))
       (unwind-protect
           (progn
             (nelisp-ec-set-buffer ,newbuf)
             ,@body)
         (setq nelisp-ec--current-buffer ,saved)))))

;;;###autoload
(defun nelisp-ec-kill-buffer (buf)
  "Kill BUF.  Returns t.
The buffer is removed from the registry, marked killed, and if it was
the current buffer the selection is cleared (set to nil)."
  (unless (nelisp-ec-buffer-p buf)
    (signal 'wrong-type-argument (list 'nelisp-ec-buffer-p buf)))
  (unless (nelisp-ec-buffer-killed-p buf)
    (let ((name (nelisp-ec-buffer-name buf)))
      (setq nelisp-ec--buffers
            (assoc-delete-all name nelisp-ec--buffers)))
    (setf (nelisp-ec-buffer-killed-p buf) t)
    (when (eq buf nelisp-ec--current-buffer)
      (setq nelisp-ec--current-buffer nil)))
  t)

;;; B. point + cursor control  (7 APIs)

;;;###autoload
(defun nelisp-ec-point ()
  "Return value of POINT in the current buffer (1-based)."
  (nelisp-ec-buffer-point (nelisp-ec--ensure-current)))

;;;###autoload
(defun nelisp-ec-point-min ()
  "Return the minimum permissible value of POINT in the current buffer.
This is `nelisp-ec-buffer-narrow-start' if narrowing is active, else 1."
  (let ((buf (nelisp-ec--ensure-current)))
    (or (nelisp-ec-buffer-narrow-start buf) 1)))

;;;###autoload
(defun nelisp-ec-point-max ()
  "Return the maximum permissible value of POINT in the current buffer.
This is `nelisp-ec-buffer-narrow-end' if narrowing is active, else
the buffer length + 1 (= one past the last char)."
  (let ((buf (nelisp-ec--ensure-current)))
    (or (nelisp-ec-buffer-narrow-end buf)
        (1+ (text-buffer-length (nelisp-ec--text buf))))))

;;;###autoload
(defun nelisp-ec-goto-char (pos)
  "Set POINT to POS in the current buffer.  Return POS.
POS must be a 1-based integer in [`point-min', `point-max'].  Out-of-
range values signal `nelisp-ec-args-out-of-range'."
  (unless (integerp pos)
    (signal 'wrong-type-argument (list 'integerp pos)))
  (let ((buf (nelisp-ec--ensure-current))
        (lo (nelisp-ec-point-min))
        (hi (nelisp-ec-point-max)))
    (when (or (< pos lo) (> pos hi))
      (signal 'nelisp-ec-args-out-of-range (list pos lo hi)))
    (setf (nelisp-ec-buffer-point buf) pos)
    pos))

;;;###autoload
(defun nelisp-ec-forward-char (&optional n)
  "Move POINT N (default 1) characters forward.  Return t.
Signals `nelisp-ec-args-out-of-range' if the move would leave the
narrowed region (or buffer)."
  (let* ((n (or n 1))
         (buf (nelisp-ec--ensure-current))
         (new (+ (nelisp-ec-buffer-point buf) n))
         (lo (nelisp-ec-point-min))
         (hi (nelisp-ec-point-max)))
    (when (or (< new lo) (> new hi))
      (signal 'nelisp-ec-args-out-of-range (list new lo hi)))
    (setf (nelisp-ec-buffer-point buf) new)
    t))

;;;###autoload
(defun nelisp-ec-backward-char (&optional n)
  "Move POINT N (default 1) characters backward.  Return t.
Signals `nelisp-ec-args-out-of-range' on underflow."
  (nelisp-ec-forward-char (- (or n 1))))

;;;###autoload
(defun nelisp-ec-buffer-size (&optional buf)
  "Return the total number of characters in BUF (default = current).
Ignores narrowing — this is always the underlying text length."
  (let ((b (or buf (nelisp-ec--ensure-current))))
    (text-buffer-length (nelisp-ec--text b))))

;;; C. text editing  (6 APIs)

;;;###autoload
(defun nelisp-ec-insert (&rest strings)
  "Insert STRINGS at POINT in the current buffer.  Return nil.
POINT advances past the inserted text.  Each element of STRINGS must
be a string; nil elements are ignored (Emacs forbids them, but our
MVP is forgiving for callers that build arg lists dynamically)."
  (let ((buf (nelisp-ec--ensure-current)))
    (dolist (s strings)
      (when s
        (unless (stringp s)
          (signal 'wrong-type-argument (list 'stringp s)))
        (unless (string-empty-p s)
          (let* ((insert-point (nelisp-ec-buffer-point buf))
                 (n-chars (length s))
                 (ne (nelisp-ec-buffer-narrow-end buf)))
            (nelisp-ec--sync-cursor buf)
            (text-buffer-insert (nelisp-ec--text buf) s)
            (let* ((tb (nelisp-ec--text buf))
                   (new-point (1+ (nelisp-text-buffer-cursor-char tb))))
              (setf (nelisp-ec-buffer-point buf) new-point)
              (setf (nelisp-ec-buffer-modified-p buf) t)
              ;; Push narrow-end out when insertion occurred at or before it.
              (when (and ne (<= insert-point ne))
                (setf (nelisp-ec-buffer-narrow-end buf)
                      (+ ne n-chars))))))))
    nil))

;;;###autoload
(defun nelisp-ec-delete-region (start end)
  "Delete the text between positions START and END.  Return nil.
Both positions are 1-based; START <= END.  POINT is moved to MIN
(START, END) if it lay inside the deleted range, or shifted left by
the deleted char count if it lay after END.  Narrowing bounds are
adjusted analogously."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (let* ((buf (nelisp-ec--ensure-current))
         (lo (nelisp-ec-point-min))
         (hi (nelisp-ec-point-max))
         (s (min start end))
         (e (max start end)))
    (when (or (< s lo) (> e hi))
      (signal 'nelisp-ec-args-out-of-range (list s e lo hi)))
    (when (> e s)
      (let ((n (- e s))
            (point (nelisp-ec-buffer-point buf)))
        (text-buffer-delete (nelisp-ec--text buf) (1- s) (1- e))
        (cond
         ((<= point s)
          ;; point before deletion: unchanged
          )
         ((<= point e)
          (setf (nelisp-ec-buffer-point buf) s))
         (t
          (setf (nelisp-ec-buffer-point buf) (- point n))))
        (let ((ne (nelisp-ec-buffer-narrow-end buf)))
          (when ne
            (cond
             ((<= ne s) nil)
             ((<= ne e) (setf (nelisp-ec-buffer-narrow-end buf) s))
             (t (setf (nelisp-ec-buffer-narrow-end buf) (- ne n))))))
        (setf (nelisp-ec-buffer-modified-p buf) t)))
    nil))

;;;###autoload
(defun nelisp-ec-delete-char (n)
  "Delete the next N characters (negative = previous |N|).  Return nil."
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (let* ((buf (nelisp-ec--ensure-current))
         (point (nelisp-ec-buffer-point buf)))
    (cond
     ((zerop n) nil)
     ((> n 0) (nelisp-ec-delete-region point (+ point n)))
     (t       (nelisp-ec-delete-region (+ point n) point)))))

;;;###autoload
(defun nelisp-ec-erase-buffer ()
  "Delete all text in the current buffer.  Return nil.
If narrowing is active, only the narrowed region is erased — matching
Emacs `erase-buffer' which signals an error under narrowing in some
modes; our MVP simply erases the visible region."
  (let ((buf (nelisp-ec--ensure-current)))
    (nelisp-ec-delete-region (nelisp-ec-point-min) (nelisp-ec-point-max))
    (setf (nelisp-ec-buffer-modified-p buf) t)
    nil))

;;;###autoload
(defun nelisp-ec-buffer-substring (start end)
  "Return the text between positions START and END (1-based)."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (let* ((buf (nelisp-ec--ensure-current))
         (lo (nelisp-ec-point-min))
         (hi (nelisp-ec-point-max))
         (s (min start end))
         (e (max start end)))
    (when (or (< s lo) (> e hi))
      (signal 'nelisp-ec-args-out-of-range (list s e lo hi)))
    (text-buffer-substring (nelisp-ec--text buf) (1- s) (1- e))))

;;;###autoload
(defun nelisp-ec-buffer-string ()
  "Return the entire text of the current buffer (respects narrowing)."
  (nelisp-ec-buffer-substring (nelisp-ec-point-min) (nelisp-ec-point-max)))

;;; D. save-* family  (3 macros)

;;;###autoload
(defmacro nelisp-ec-save-excursion (&rest body)
  "Save POINT (and current buffer), evaluate BODY, restore both.
The saved POINT is restored even on non-local exit.  POINT-restoration
is byvalue, not by marker — so insertions before the saved position
will leave the restored POINT pointing at a *different* character than
when it was saved.  This matches the simple-marker-deferred policy of
Phase 9a MVP; switch to a marker-backed restore in Phase 9b."
  (declare (indent 0) (debug (body)))
  (let ((saved-buf (make-symbol "saved-buf"))
        (saved-pt (make-symbol "saved-pt")))
    `(let* ((,saved-buf nelisp-ec--current-buffer)
            (,saved-pt (and ,saved-buf
                            (nelisp-ec-buffer-point ,saved-buf))))
       (unwind-protect
           (progn ,@body)
         (when (and ,saved-buf
                    (not (nelisp-ec-buffer-killed-p ,saved-buf)))
           (setf (nelisp-ec-buffer-point ,saved-buf) ,saved-pt))
         (setq nelisp-ec--current-buffer ,saved-buf)))))

;;;###autoload
(defmacro nelisp-ec-save-restriction (&rest body)
  "Save the narrowing state of the current buffer, run BODY, restore.
Restoration occurs even on non-local exit.  Like Emacs the restored
narrowing follows the *buffer* that was current at save time, even if
BODY changes the current buffer."
  (declare (indent 0) (debug (body)))
  (let ((saved-buf (make-symbol "saved-buf"))
        (saved-lo (make-symbol "saved-lo"))
        (saved-hi (make-symbol "saved-hi")))
    `(let* ((,saved-buf nelisp-ec--current-buffer)
            (,saved-lo (and ,saved-buf
                            (nelisp-ec-buffer-narrow-start ,saved-buf)))
            (,saved-hi (and ,saved-buf
                            (nelisp-ec-buffer-narrow-end ,saved-buf))))
       (unwind-protect
           (progn ,@body)
         (when (and ,saved-buf
                    (not (nelisp-ec-buffer-killed-p ,saved-buf)))
           (setf (nelisp-ec-buffer-narrow-start ,saved-buf) ,saved-lo)
           (setf (nelisp-ec-buffer-narrow-end ,saved-buf) ,saved-hi))))))

;;;###autoload
(defmacro nelisp-ec-save-current-buffer (&rest body)
  "Save the current buffer selection, run BODY, restore on exit."
  (declare (indent 0) (debug (body)))
  (let ((saved (make-symbol "saved")))
    `(let ((,saved nelisp-ec--current-buffer))
       (unwind-protect
           (progn ,@body)
         (setq nelisp-ec--current-buffer ,saved)))))

;;; E. narrowing  (2 APIs)

;;;###autoload
(defun nelisp-ec-narrow-to-region (start end)
  "Restrict POINT-MIN / POINT-MAX of the current buffer to [START, END).
START / END are 1-based; START <= END.  POINT is clamped to the new
range.  Returns nil."
  (unless (and (integerp start) (integerp end))
    (signal 'wrong-type-argument (list 'integerp start end)))
  (let* ((buf (nelisp-ec--ensure-current))
         (raw-len (text-buffer-length (nelisp-ec--text buf)))
         (max-end (1+ raw-len))
         (s (min start end))
         (e (max start end)))
    (when (or (< s 1) (> e max-end))
      (signal 'nelisp-ec-args-out-of-range (list s e 1 max-end)))
    (setf (nelisp-ec-buffer-narrow-start buf) s)
    (setf (nelisp-ec-buffer-narrow-end buf) e)
    (let ((p (nelisp-ec-buffer-point buf)))
      (cond
       ((< p s) (setf (nelisp-ec-buffer-point buf) s))
       ((> p e) (setf (nelisp-ec-buffer-point buf) e))))
    nil))

;;;###autoload
(defun nelisp-ec-widen ()
  "Remove any narrowing restriction on the current buffer.  Return nil."
  (let ((buf (nelisp-ec--ensure-current)))
    (setf (nelisp-ec-buffer-narrow-start buf) nil)
    (setf (nelisp-ec-buffer-narrow-end buf) nil)
    nil))

;;; F. marker  (5 APIs)

;;;###autoload
(defun nelisp-ec-make-marker ()
  "Return a fresh marker that does not point anywhere."
  (nelisp-ec-marker--make-raw :position nil :buffer nil))

;;;###autoload
(defun nelisp-ec-set-marker (marker pos &optional buf)
  "Set MARKER to point to POS in BUF (default = current buffer).
If POS is nil the marker is detached (= points nowhere).  Returns
MARKER.  POS is *not* clamped against BUF's narrowing — narrowing
affects where you can move POINT, not where a marker may sit."
  (unless (nelisp-ec-marker-p marker)
    (signal 'wrong-type-argument (list 'nelisp-ec-marker-p marker)))
  (cond
   ((null pos)
    (setf (nelisp-ec-marker--position marker) nil)
    (setf (nelisp-ec-marker--buffer marker) nil))
   (t
    (unless (integerp pos)
      (signal 'wrong-type-argument (list 'integerp pos)))
    (let ((b (or buf (nelisp-ec--ensure-current))))
      (unless (nelisp-ec-buffer-p b)
        (signal 'wrong-type-argument (list 'nelisp-ec-buffer-p b)))
      (nelisp-ec--check-live b)
      (let ((max-end (1+ (text-buffer-length (nelisp-ec--text b)))))
        (when (or (< pos 1) (> pos max-end))
          (signal 'nelisp-ec-args-out-of-range (list pos 1 max-end))))
      (setf (nelisp-ec-marker--position marker) pos)
      (setf (nelisp-ec-marker--buffer marker) b))))
  marker)

;;;###autoload
(defun nelisp-ec-marker-position (marker)
  "Return the position of MARKER (1-based integer), or nil if detached."
  (unless (nelisp-ec-marker-p marker)
    (signal 'wrong-type-argument (list 'nelisp-ec-marker-p marker)))
  (nelisp-ec-marker--position marker))

;;;###autoload
(defun nelisp-ec-marker-buffer (marker)
  "Return the buffer that MARKER refers to, or nil if detached."
  (unless (nelisp-ec-marker-p marker)
    (signal 'wrong-type-argument (list 'nelisp-ec-marker-p marker)))
  (nelisp-ec-marker--buffer marker))

;;;###autoload
(defun nelisp-ec-point-marker ()
  "Return a fresh marker pointing to POINT in the current buffer."
  (let ((buf (nelisp-ec--ensure-current)))
    (nelisp-ec-set-marker (nelisp-ec-make-marker)
                          (nelisp-ec-buffer-point buf)
                          buf)))

;;; G. search + match-data  (9 APIs)

;;;###autoload
(defun nelisp-ec-search-forward (string &optional bound noerror)
  "Search forward from POINT for literal STRING.
On match: move POINT past the match and return the new POINT (= end
position of the match).  On failure: if NOERROR is non-nil return nil,
else signal `nelisp-ec-error'.  BOUND, if non-nil, is the upper
position bound (1-based) — searches stop without matching past it."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let* ((buf (nelisp-ec--ensure-current))
         (start (1- (nelisp-ec-buffer-point buf)))
         (hit (text-buffer-search (nelisp-ec--text buf) string start)))
    (cond
     ((and hit
           (let* ((match-end-char (+ hit (length string)))
                  (match-end-pos (1+ match-end-char)))
             (and (or (null bound) (<= match-end-pos bound))
                  (<= match-end-pos (nelisp-ec-point-max)))))
      (let ((new-point (1+ (+ hit (length string)))))
        (setf (nelisp-ec-buffer-point buf) new-point)
        (nelisp-ec--set-simple-match-data (1+ hit) new-point)
        new-point))
     (noerror nil)
     (t (signal 'nelisp-ec-error
                (list "Search failed" string))))))

;;;###autoload
(defun nelisp-ec-search-backward (string &optional bound noerror)
  "Search backward from POINT for literal STRING.
On match: move POINT to the start of the match and return new POINT.
On failure honor NOERROR like `nelisp-ec-search-forward'.  BOUND, if
non-nil, is the lower position bound (1-based)."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let* ((buf (nelisp-ec--ensure-current))
         (point (nelisp-ec-buffer-point buf))
         (slen (length string))
         (lo (or bound (nelisp-ec-point-min)))
         ;; Search the substring from point-min..point for the *last*
         ;; occurrence whose end is <= point.  We do this by linear
         ;; scan forward from lo, since T36 only offers forward search.
         (scan-from (1- lo))
         (best nil))
    (when (>= point lo)
      (let ((tb (nelisp-ec--text buf))
            (cur scan-from)
            (ceiling (- point 1))) ;; end-char must be <= ceiling (0-based)
        (catch 'done
          (while t
            (let ((hit (text-buffer-search tb string cur)))
              (cond
               ((null hit) (throw 'done nil))
               ((> (+ hit slen) ceiling) (throw 'done nil))
               (t (setq best hit
                        cur (1+ hit)))))))))
    (cond
     (best
      (let ((new-point (1+ best)))
        (setf (nelisp-ec-buffer-point buf) new-point)
        (nelisp-ec--set-simple-match-data new-point (+ new-point slen))
        new-point))
     (noerror nil)
     (t (signal 'nelisp-ec-error
                (list "Search failed" string))))))

;;;###autoload
(defun nelisp-ec-looking-at-p (string)
  "Return non-nil if text at POINT begins with literal STRING.
STRING is interpreted *literally* (= as a fixed substring), not as a
regular expression."
  (unless (stringp string)
    (signal 'wrong-type-argument (list 'stringp string)))
  (let* ((buf (nelisp-ec--ensure-current))
         (point (nelisp-ec-buffer-point buf))
         (slen (length string))
         (hi (nelisp-ec-point-max)))
    (cond
     ((zerop slen) t)
     ((> (+ point slen) hi) nil)
     (t
      (let ((substr (text-buffer-substring (nelisp-ec--text buf)
                                           (1- point)
                                           (+ (1- point) slen))))
        (when (string-equal substr string)
          (nelisp-ec--set-simple-match-data point (+ point slen))))))))

;;;###autoload
(defun nelisp-ec-re-search-forward (regexp &optional bound noerror)
  "Search forward from POINT for REGEXP via `nelisp-regex'.

On match, move POINT to the end of the match and return that position.
BOUND, if non-nil, is an absolute 1-based upper bound on the match end.
On failure, return nil when NOERROR is non-nil, else signal
`nelisp-ec-error'."
  (unless (stringp regexp)
    (signal 'wrong-type-argument (list 'stringp regexp)))
  (let* ((buf (nelisp-ec--ensure-current))
         (region (nelisp-ec--search-region buf))
         (base (nth 0 region))
         (text (nth 1 region))
         (point-index (nth 2 region))
         (match (nelisp-rx-string-match regexp text point-index))
         (abs-end (and match (+ base (plist-get match :end)))))
    (cond
     ((and match
           (or (null bound) (<= abs-end bound)))
      (setf (nelisp-ec-buffer-point buf) abs-end)
      (nelisp-ec--rx-match-data-to-ec base match)
      abs-end)
     (noerror nil)
     (t (signal 'nelisp-ec-error
                (list "Search failed" regexp))))))

;;;###autoload
(defun nelisp-ec-re-search-backward (regexp &optional bound noerror)
  "Search backward from POINT for REGEXP via `nelisp-regex'.

Returns the 1-based start position of the rightmost match whose end is
at or before POINT and whose start is at or after BOUND when BOUND is
non-nil.  On failure honor NOERROR like `nelisp-ec-re-search-forward'."
  (unless (stringp regexp)
    (signal 'wrong-type-argument (list 'stringp regexp)))
  (let* ((buf (nelisp-ec--ensure-current))
         (region (nelisp-ec--search-region buf))
         (base (nth 0 region))
         (text (nth 1 region))
         (point-index (nth 2 region))
         (lo-index (if bound (max 0 (- bound base)) 0))
         (matches (nelisp-rx-string-match-all regexp text lo-index))
         (best nil))
    (dolist (match matches)
      (when (and (>= (plist-get match :start) lo-index)
                 (<= (plist-get match :end) point-index))
        (setq best match)))
    (cond
     (best
      (let ((new-point (+ base (plist-get best :start))))
        (setf (nelisp-ec-buffer-point buf) new-point)
        (nelisp-ec--rx-match-data-to-ec base best)
        new-point))
     (noerror nil)
     (t (signal 'nelisp-ec-error
                (list "Search failed" regexp))))))

;;;###autoload
(defun nelisp-ec-looking-at (regexp)
  "Return non-nil if REGEXP matches starting exactly at POINT."
  (unless (stringp regexp)
    (signal 'wrong-type-argument (list 'stringp regexp)))
  (let* ((buf (nelisp-ec--ensure-current))
         (region (nelisp-ec--search-region buf))
         (base (nth 0 region))
         (text (nth 1 region))
         (point-index (nth 2 region))
         (match (nelisp-rx-string-match regexp text point-index)))
    (when (and match (= (plist-get match :start) point-index))
      (nelisp-ec--rx-match-data-to-ec base match))))

;;;###autoload
(defun nelisp-ec-match-data ()
  "Return the last successful match data in Emacs-compatible list form."
  (copy-sequence nelisp-ec--match-data))

;;;###autoload
(defun nelisp-ec-match-beginning (subexp)
  "Return the start position of SUBEXP from `nelisp-ec-match-data'."
  (unless (integerp subexp)
    (signal 'wrong-type-argument (list 'integerp subexp)))
  (nth (* 2 subexp) nelisp-ec--match-data))

;;;###autoload
(defun nelisp-ec-match-end (subexp)
  "Return the end position of SUBEXP from `nelisp-ec-match-data'."
  (unless (integerp subexp)
    (signal 'wrong-type-argument (list 'integerp subexp)))
  (nth (1+ (* 2 subexp)) nelisp-ec--match-data))

(provide 'nelisp-emacs-compat)
;;; nelisp-emacs-compat.el ends here
