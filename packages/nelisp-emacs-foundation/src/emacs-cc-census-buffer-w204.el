;;; emacs-cc-census-buffer-w204.el --- Overlay queries, syntax motion and casing  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-buffer-w204--position (position)
  "Return the integer value of POSITION, validating integers and markers."
  (cond
   ((integerp position) position)
   ((markerp position)
    (or (marker-position position)
        (error "Marker does not point anywhere")))
   (t (signal 'wrong-type-argument (list 'integer-or-marker-p position)))))

(defun emacs-cc-census-buffer-w204--overlays ()
  "Return all overlays belonging to the current buffer."
  (let ((lists (overlay-lists)))
    (append (car lists) (cdr lists))))

(defun emacs-cc-census-buffer-w204--priority (overlay)
  "Return OVERLAY's primary and secondary numeric priorities."
  (let ((priority (overlay-get overlay 'priority)))
    (cons (cond ((integerp priority) priority)
                ((and (consp priority) (integerp (car priority)))
                 (car priority))
                (t 0))
          (if (and (consp priority) (integerp (cdr priority)))
              (cdr priority) 0))))

(defun emacs-cc-census-buffer-w204--overlay-before-p (a b)
  "Return non-nil if overlay A takes precedence over B."
  (let ((pa (emacs-cc-census-buffer-w204--priority a))
        (pb (emacs-cc-census-buffer-w204--priority b)))
    (cond
     ((/= (car pa) (car pb)) (> (car pa) (car pb)))
     ((and (>= (overlay-start a) (overlay-start b))
           (<= (overlay-end a) (overlay-end b))
           (or (> (overlay-start a) (overlay-start b))
               (< (overlay-end a) (overlay-end b)))) t)
     ((and (>= (overlay-start b) (overlay-start a))
           (<= (overlay-end b) (overlay-end a))
           (or (> (overlay-start b) (overlay-start a))
               (< (overlay-end b) (overlay-end a)))) nil)
     (t (> (cdr pa) (cdr pb))))))

(unless (fboundp 'overlays-at)
  (defun overlays-at (position &optional sorted)
    "Return overlays containing POSITION, sorted by priority if SORTED."
    (let ((pos (emacs-cc-census-buffer-w204--position position)) result)
      (dolist (overlay (emacs-cc-census-buffer-w204--overlays))
        (when (and (<= (overlay-start overlay) pos)
                   (< pos (overlay-end overlay)))
          (push overlay result)))
      (if sorted
          (sort result #'emacs-cc-census-buffer-w204--overlay-before-p)
        (nreverse result)))))

(unless (fboundp 'overlays-in)
  (defun overlays-in (beg end)
    "Return overlays overlapping BEG through END, including empty overlays."
    (let ((start (emacs-cc-census-buffer-w204--position beg))
          (finish (emacs-cc-census-buffer-w204--position end))
          (buffer-end (save-restriction (widen) (point-max)))
          result)
      (dolist (overlay (emacs-cc-census-buffer-w204--overlays))
        (let ((a (overlay-start overlay)) (b (overlay-end overlay)))
          (when (if (= a b)
                    (and (<= start finish) (>= a start)
                         (or (< a finish) (= a start)
                             (and (= a finish) (= finish buffer-end))))
                  (and (< a finish) (> b start)))
            (push overlay result))))
      (nreverse result))))

(unless (fboundp 'pos-bol)
  (defun pos-bol (&optional n)
    "Return the beginning of the line N minus one lines from point."
    (when (and n (not (integerp n)))
      (signal 'wrong-type-argument (list 'integerp n)))
    (line-beginning-position n)))

(unless (fboundp 'pos-eol)
  (defun pos-eol (&optional n)
    "Return the end of the line N minus one lines from point."
    (when (and n (not (integerp n)))
      (signal 'wrong-type-argument (list 'integerp n)))
    (line-end-position n)))

(unless (fboundp 'previous-overlay-change)
  (defun previous-overlay-change (position)
    "Return the nearest overlay boundary before POSITION, or `point-min'."
    (let ((pos (emacs-cc-census-buffer-w204--position position))
          (previous (point-min)))
      (dolist (overlay (emacs-cc-census-buffer-w204--overlays))
        (let ((start (overlay-start overlay)) (end (overlay-end overlay)))
          (when (and (< start pos) (> start previous))
            (setq previous start))
          (when (and (< end pos) (> end previous))
            (setq previous end))))
      previous)))

(defun emacs-cc-census-buffer-w204--syntax (position)
  "Return the syntax class designator at POSITION in the current buffer."
  (let ((character (char-after position))
        (property (and (boundp 'parse-sexp-lookup-properties)
                       parse-sexp-lookup-properties
                       (get-text-property position 'syntax-table))))
    (when (and property (char-table-p property))
      (setq property (aref property character)))
    (if (and (consp property) (integerp (car property))
             (/= (logand (car property) 255) 13))
        (aref " .w_()'\"$\\/<>@!|" (logand (car property) 15))
      (char-syntax character))))

(defun emacs-cc-census-buffer-w204--skip-syntax (syntax limit backward)
  "Skip classes in SYNTAX up to LIMIT, moving backward if BACKWARD."
  (unless (stringp syntax)
    (signal 'wrong-type-argument (list 'stringp syntax)))
  (let* ((origin (point))
         (bound (if limit (emacs-cc-census-buffer-w204--position limit)
                  (if backward (point-min) (point-max))))
         (negate (and (> (length syntax) 0) (= (aref syntax 0) 94)))
         (index (if negate 1 0))
         classes
         (pos origin))
    (while (< index (length syntax))
      (let ((class (aref syntax index)))
        (push (if (= class 45) 32 class) classes))
      (setq index (1+ index)))
    (setq bound (if backward (max bound (point-min))
                  (min bound (point-max))))
    (while (and (if backward (> pos bound) (< pos bound))
                (let ((member (memq (emacs-cc-census-buffer-w204--syntax
                                     (if backward (1- pos) pos)) classes)))
                  (if negate (not member) member)))
      (setq pos (+ pos (if backward -1 1))))
    (goto-char pos)
    (- pos origin)))

(unless (fboundp 'skip-syntax-backward)
  (defun skip-syntax-backward (syntax &optional limit)
    "Move backward across SYNTAX classes, stopping at LIMIT; return distance."
    (emacs-cc-census-buffer-w204--skip-syntax syntax limit t)))

(unless (fboundp 'skip-syntax-forward)
  (defun skip-syntax-forward (syntax &optional limit)
    "Move forward across SYNTAX classes, stopping at LIMIT; return distance."
    (emacs-cc-census-buffer-w204--skip-syntax syntax limit nil)))

;; `suspend-emacs' requires terminal and process suspension support.
;; Leave its existing definition intact rather than emulate suspension.

(defun emacs-cc-census-buffer-w204--replace-character (pos upper)
  "Replace the character at POS with UPPER without moving existing markers."
  (let* ((buffer (current-buffer))
         (full (nelisp-buffer-string buffer))
         (old-point (point))
         (size (length upper))
         (delta (1- size))
         (new-point (if (> old-point pos) (+ old-point delta) old-point))
         (replacement (concat (substring full 0 (1- pos))
                              (substring-no-properties upper)
                              (substring full pos)))
         (ext (gethash buffer emacs-buffer--state)))
    ;; The bundle's public substitution function still uses the editor-core
    ;; scratch buffer.  Change the real buffer's gap, as its in-place
    ;; substitution backend does, retaining marker and overlay positions.
    (when (and ext (not (eq (emacs-buffer--ext-undo-list ext) t)))
      (setf (emacs-buffer--ext-undo-list ext)
            (cons (cons pos (+ pos size))
                  (cons (cons (substring full (1- pos) pos) pos)
                        (emacs-buffer--ext-undo-list ext)))))
    (when (/= delta 0)
      (nelisp-buffer--shift-text-properties-on-insert buffer pos delta)
      (when (nelisp-buffer-narrow-end buffer)
        (setf (nelisp-buffer-narrow-end buffer)
              (+ (nelisp-buffer-narrow-end buffer) delta))))
    (setf (nelisp-buffer-before-gap buffer)
          (substring replacement 0 (1- new-point)))
    (setf (nelisp-buffer-after-gap buffer)
          (substring replacement (1- new-point)))
    (remhash buffer nelisp-buffer--pending-point)
    (nelisp-buffer--bump-tick buffer)
    (setf (nelisp-buffer-modified buffer) t)
    (when (fboundp 'emacs-buffer-builtins--share-text)
      (emacs-buffer-builtins--share-text buffer))
    (set-buffer-modified-p t)))

(defun emacs-cc-census-buffer-w204--upcase (beg end)
  "Uppercase the validated accessible region BEG through END."
  (when (< beg end)
    (when (and buffer-read-only (not inhibit-read-only))
      (signal 'buffer-read-only (list (current-buffer))))
    (let ((pos beg))
      (while (< pos end)
        (let ((read-only (get-text-property pos 'read-only)))
          (when (and read-only
                     (not (if (listp inhibit-read-only)
                              (memq read-only inhibit-read-only)
                            inhibit-read-only)))
            (signal 'text-read-only nil)))
        (setq pos (1+ pos))))
    (let ((pos beg))
      (while (< pos end)
        (let* ((text (buffer-substring-no-properties pos (1+ pos)))
               (upper (upcase text))
               (size (length upper)))
          (unless (equal text upper)
            (emacs-cc-census-buffer-w204--replace-character pos upper))
          (setq end (+ end (1- size)) pos (+ pos size))))))
  nil)

(unless (fboundp 'upcase-region)
  (defun upcase-region (beg end &optional region-noncontiguous-p)
    "Convert BEG through END to uppercase, preserving point and properties."
    (if region-noncontiguous-p
        (dolist (bounds (funcall region-extract-function 'bounds))
          (upcase-region (car bounds) (cdr bounds)))
      (let ((start (emacs-cc-census-buffer-w204--position beg))
            (finish (emacs-cc-census-buffer-w204--position end)))
        (when (or (< (min start finish) (point-min))
                  (> (max start finish) (point-max)))
          (signal 'args-out-of-range (list (current-buffer) start finish)))
        (emacs-cc-census-buffer-w204--upcase (min start finish)
                                          (max start finish))))
    nil))

(unless (fboundp 'upcase-word)
  (defun upcase-word (arg)
    "Uppercase ARG words; move forward, or preserve point for negative ARG."
    (unless (fixnump arg)
      (signal 'wrong-type-argument (list 'fixnump arg)))
    (let ((origin (point)) (pos (point)) (count (abs arg)))
      (while (and (> count 0)
                  (if (< arg 0) (> pos (point-min)) (< pos (point-max))))
        (if (< arg 0)
            (progn
              (while (and (> pos (point-min))
                          (/= (emacs-cc-census-buffer-w204--syntax (1- pos)) 119))
                (setq pos (1- pos)))
              (while (and (> pos (point-min))
                          (= (emacs-cc-census-buffer-w204--syntax (1- pos)) 119))
                (setq pos (1- pos))))
          (while (and (< pos (point-max))
                      (/= (emacs-cc-census-buffer-w204--syntax pos) 119))
            (setq pos (1+ pos)))
          (while (and (< pos (point-max))
                      (= (emacs-cc-census-buffer-w204--syntax pos) 119))
            (setq pos (1+ pos))))
        (setq count (1- count)))
      (let ((old-size (buffer-size)))
        (upcase-region (min origin pos) (max origin pos))
        (when (>= arg 0)
          (goto-char (+ pos (- (buffer-size) old-size))))))
    nil))

(provide 'emacs-cc-census-buffer-w204)
;;; emacs-cc-census-buffer-w204.el ends here
