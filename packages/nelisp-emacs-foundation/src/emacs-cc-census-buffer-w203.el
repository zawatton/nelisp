;;; emacs-cc-census-buffer-w203.el --- Match data, intervals and overlays  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-buffer-w203--arity (name args minimum maximum)
  "Validate ARGS against NAME's inclusive argument-count bounds."
  (let ((count (length args)))
    (unless (and (>= count minimum) (<= count maximum))
      (signal 'wrong-number-of-arguments (list name count)))))

(defun emacs-cc-census-buffer-w203--check-overlay (overlay)
  "Validate OVERLAY and detach it if its owning buffer has been killed."
  (unless (overlayp overlay)
    (signal 'wrong-type-argument (list 'overlayp overlay)))
  (let ((buffer (emacs-buffer--overlay-rec-buffer overlay)))
    (when (and buffer (not (buffer-live-p buffer)))
      (emacs-buffer--overlay-remove-rec buffer overlay)
      (setf (emacs-buffer--overlay-rec-buffer overlay) nil))))

(defun emacs-cc-census-buffer-w203--position (position buffer)
  "Return POSITION's integer value, requiring markers to belong to BUFFER."
  (cond
   ((integerp position) position)
   ((markerp position)
    (unless (eq (marker-buffer position) buffer)
      (signal 'error (list "Marker points into wrong buffer" position)))
    (marker-position position))
   (t (signal 'wrong-type-argument
              (list 'integer-or-marker-p position)))))

(defun emacs-cc-census-buffer-w203--translate-position (position n)
  "Translate POSITION by N with the engine's fixnum representation."
  ;; GNU clamps the untagged sum before converting it back to a fixnum.
  ;; The standalone's integer arithmetic has a wider range than fixnums.
  (let ((sum (logand (max 0 (+ position n))
                     (1- (* 2 (1+ most-positive-fixnum))))))
    (if (> sum most-positive-fixnum)
        (- sum (* 2 (1+ most-positive-fixnum)))
      sum)))

(defvar emacs-cc-census-buffer-w203--translated-captures nil
  "Capture vector and original offsets for translated, wrapped positions.")

(unless (fboundp 'match-data--translate)
  (defun match-data--translate (&rest args)
    "Add fixnum N to every matched position, clamping at zero."
    (emacs-cc-census-buffer-w203--arity 'match-data--translate args 1 1)
    (let ((n (car args)))
      (unless (fixnump n)
	(signal 'wrong-type-argument (list 'fixnump n)))
      ;; The standalone regex engine stores each matched pair in this vector.
      ;; Keep unmatched groups nil and do not change the match's origin.
      ;; GNU retains untagged offsets even when match-data prints a wrapped
      ;; fixnum.  Preserve those offsets across successive translations.
      (let ((i 0)
            (previous (and (eq nlre--last-caps
                               (car emacs-cc-census-buffer-w203--translated-captures))
                           (cdr emacs-cc-census-buffer-w203--translated-captures)))
            (next (make-vector (length nlre--last-caps) nil)))
	(while (< i (length nlre--last-caps))
          (let ((pair (aref nlre--last-caps i)))
            (when pair
              (let* ((saved (and previous (< i (length previous))
				 (aref previous i)))
                     (original (if (eq pair (car saved)) (cdr saved) pair))
                     (raw (cons (max 0 (+ (car original) n))
				(max 0 (+ (cdr original) n))))
                     (translated
                      (cons (emacs-cc-census-buffer-w203--translate-position
                             (car raw) 0)
                            (emacs-cc-census-buffer-w203--translate-position
                             (cdr raw) 0))))
		(aset nlre--last-caps i translated)
		(aset next i (cons translated raw)))))
          (setq i (1+ i)))
	(setq emacs-cc-census-buffer-w203--translated-captures
              (cons nlre--last-caps next)))
      nil)))

(unless (fboundp 'move-overlay)
  (defun move-overlay (&rest args)
    "Move OVERLAY to BEG and END in BUFFER, reattaching it if necessary."
    (emacs-cc-census-buffer-w203--arity 'move-overlay args 3 4)
    (let ((overlay (nth 0 args)) (beg (nth 1 args))
          (end (nth 2 args)) (buffer (nth 3 args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (dolist (position (list beg end))
	(unless (or (integerp position) (markerp position))
          (signal 'wrong-type-argument
                  (list 'integer-or-marker-p position))))
      (let* ((old-buffer (emacs-buffer--overlay-rec-buffer overlay))
             (target (or buffer old-buffer (current-buffer))))
	(unless (bufferp target)
          (signal 'wrong-type-argument (list 'bufferp target)))
	(unless (buffer-live-p target)
          (signal 'error (list "Attempt to move overlay to a dead buffer")))
	(setq beg (emacs-cc-census-buffer-w203--position beg target)
              end (emacs-cc-census-buffer-w203--position end target))
	(let* ((limit (1+ (buffer-size target)))
               (start (max 1 (min limit (min beg end))))
               (finish (max 1 (min limit (max beg end)))))
          (when old-buffer
            (emacs-buffer--overlay-remove-rec old-buffer overlay))
          (setf (emacs-buffer--overlay-rec-start overlay) start
		(emacs-buffer--overlay-rec-end overlay) finish
		(emacs-buffer--overlay-rec-buffer overlay) target)
          (emacs-buffer--overlay-insert-sorted target overlay)
          (when (and (= start finish) (overlay-get overlay 'evaporate))
            (delete-overlay overlay))))
      overlay)))

(defun emacs-cc-census-buffer-w203--intervals (object)
  "Return OBJECT's stored intervals as zero-based triples sharing plists."
  (let* ((string (stringp object))
         (ext (gethash object (if string emacs-buffer--string-state
				emacs-buffer--state)))
         (offset (if string 0 1))
         (intervals (if ext (emacs-buffer--ext-text-props ext)
                      (if string (gethash object nelisp--tp-string-properties)
                        (nelisp-buffer-text-properties object))))
         (result nil))
    (dolist (interval intervals)
      (push (list (- (car interval) offset)
                  (- (cadr interval) offset)
                  (if ext (cddr interval) (nth 2 interval)))
            result))
    (nreverse result)))

(unless (fboundp 'object-intervals)
  (defun object-intervals (&rest args)
    "Return copied, zero-based text-property intervals of OBJECT."
    (emacs-cc-census-buffer-w203--arity 'object-intervals args 1 1)
    (let ((object (car args)))
      (unless (or (stringp object) (bufferp object))
	(signal 'wrong-type-argument (list 'buffer-or-string-p object)))
      (let* ((string (stringp object))
             (size (if string (length object)
                     (if (buffer-live-p object) (buffer-size object) 0)))
             (intervals (and (> size 0)
                             (emacs-cc-census-buffer-w203--intervals object)))
             (position 0) (result nil))
	;; GNU copies the interval records, but retains their property plists.
	;; Preserve stored boundaries even when adjacent plists are identical.
	(dolist (interval intervals)
          (let ((start (max 0 (min size (car interval))))
		(end (max 0 (min size (cadr interval)))))
            (when (< start end)
              (when (< position start)
		(push (list position start nil) result))
              (push (list start end (nth 2 interval)) result)
              (setq position end))))
	(when result
          (when (< position size)
            (push (list position size nil) result))
          (nreverse result))))))

(unless (fboundp 'overlay-buffer)
  (defun overlay-buffer (&rest args)
    "Return OVERLAY's buffer, or nil when it is detached."
    (emacs-cc-census-buffer-w203--arity 'overlay-buffer args 1 1)
    (let ((overlay (car args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (emacs-buffer--overlay-rec-buffer overlay))))

(unless (fboundp 'overlay-end)
  (defun overlay-end (&rest args)
    "Return OVERLAY's end position, or nil when it is detached."
    (emacs-cc-census-buffer-w203--arity 'overlay-end args 1 1)
    (let ((overlay (car args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (and (emacs-buffer--overlay-rec-buffer overlay)
           (emacs-buffer--overlay-rec-end overlay)))))

(unless (fboundp 'overlay-get)
  (defun overlay-get (&rest args)
    "Return OVERLAY's PROP value, falling back to its category."
    (emacs-cc-census-buffer-w203--arity 'overlay-get args 2 2)
    (let ((overlay (car args)) (prop (cadr args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (let* ((properties (emacs-buffer--overlay-rec-properties overlay))
             (entry (plist-member properties prop))
             (category (plist-member properties 'category)))
	(if entry (cadr entry)
          (and category (symbolp (cadr category))
               (get (cadr category) prop)))))))

(unless (fboundp 'overlay-lists)
  (defun overlay-lists (&rest args)
    "Return a one-element list of all overlays in the current buffer."
    (emacs-cc-census-buffer-w203--arity 'overlay-lists args 0 0)
    (let ((ext (gethash (current-buffer) emacs-buffer--state)))
      (list (and ext (copy-sequence (emacs-buffer--ext-overlays ext)))))))

(unless (fboundp 'overlay-properties)
  (defun overlay-properties (&rest args)
    "Return a copy of OVERLAY's property list, even if detached."
    (emacs-cc-census-buffer-w203--arity 'overlay-properties args 1 1)
    (let ((overlay (car args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (copy-sequence (emacs-buffer--overlay-rec-properties overlay)))))

(unless (fboundp 'overlay-put)
  (defun overlay-put (&rest args)
    "Set OVERLAY's PROP to VALUE and return VALUE."
    (emacs-cc-census-buffer-w203--arity 'overlay-put args 3 3)
    (let ((overlay (nth 0 args)) (prop (nth 1 args)) (value (nth 2 args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (let* ((properties (emacs-buffer--overlay-rec-properties overlay))
             (entry (plist-member properties prop)))
	(if entry
            (setcar (cdr entry) value)
          (setf (emacs-buffer--overlay-rec-properties overlay)
		(cons prop (cons value properties)))))
      (when (and (eq prop 'evaporate) value
		 (emacs-buffer--overlay-rec-buffer overlay)
		 (= (emacs-buffer--overlay-rec-start overlay)
                    (emacs-buffer--overlay-rec-end overlay)))
	(delete-overlay overlay))
      value)))

(unless (fboundp 'overlay-start)
  (defun overlay-start (&rest args)
    "Return OVERLAY's start position, or nil when it is detached."
    (emacs-cc-census-buffer-w203--arity 'overlay-start args 1 1)
    (let ((overlay (car args)))
      (emacs-cc-census-buffer-w203--check-overlay overlay)
      (and (emacs-buffer--overlay-rec-buffer overlay)
           (emacs-buffer--overlay-rec-start overlay)))))

(provide 'emacs-cc-census-buffer-w203)
;;; emacs-cc-census-buffer-w203.el ends here
