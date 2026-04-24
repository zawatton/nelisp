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
  (narrow-end nil))

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

;;; Marker update helpers ---------------------------------------------

(defun nelisp-buffer--shift-markers-on-insert (buf at inserted-len)
  "Advance every marker at or past AT by INSERTED-LEN.
Placeholder: Phase 5-B.2 adds the marker struct proper; here we
walk the raw list of cons cells (POS . TAIL-IS-NIL)."
  (dolist (cell (nelisp-buffer-markers buf))
    (when (and (consp cell) (>= (car cell) at))
      (setcar cell (+ (car cell) inserted-len)))))

(defun nelisp-buffer--shift-markers-on-delete (buf start end)
  "Collapse markers in (START, END] to START, shift markers past END."
  (let ((delta (- end start)))
    (dolist (cell (nelisp-buffer-markers buf))
      (when (consp cell)
        (cond
         ((<= (car cell) start) nil)
         ((>= (car cell) end)
          (setcar cell (- (car cell) delta)))
         (t
          (setcar cell start)))))))

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
Markers at or past point advance by the length of TEXT; markers
strictly before point are not touched."
  (unless (stringp text)
    (signal 'wrong-type-argument (list 'stringp text)))
  (let* ((b (nelisp-buffer--ambient buf))
         (before (nelisp-buffer-before-gap b))
         (at (1+ (length before)))
         (n (length text)))
    (setf (nelisp-buffer-before-gap b) (concat before text))
    (setf (nelisp-buffer-modified b) t)
    (nelisp-buffer--shift-markers-on-insert b at n))
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
      (nelisp-buffer--shift-markers-on-delete b s e)))
  nil)

(defun nelisp-erase-buffer (&optional buf)
  "Clear BUF entirely.  Markers collapse to `point-min'."
  (let ((b (nelisp-buffer--ambient buf)))
    (setf (nelisp-buffer-before-gap b) "")
    (setf (nelisp-buffer-after-gap b) "")
    (setf (nelisp-buffer-modified b) t)
    (dolist (cell (nelisp-buffer-markers b))
      (when (consp cell) (setcar cell 1))))
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

(provide 'nelisp-buffer)

;;; nelisp-buffer.el ends here
