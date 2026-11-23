;;; emacs-cc-editfns-1.el --- editfns.c Lisp primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'byte-to-position)
  (defun byte-to-position (bytepos)
    "Return the character position for byte position BYTEPOS, or nil."
    (unless (integerp bytepos) (signal 'wrong-type-argument (list 'fixnump bytepos)))
    (let ((p (point-min)) (limit (point-max)) (bytes 1) found)
      (while (and (<= p limit) (not found))
        (when (= bytes bytepos) (setq found p))
        (when (< p limit)
          (setq bytes (+ bytes (string-bytes (encode-coding-string
                                               (char-to-string (char-after p)) 'utf-8 t)))))
        (setq p (1+ p)))
      found)))

(unless (fboundp 'byte-to-string)
  (defun byte-to-string (byte)
    "Convert BYTE to a unibyte string containing that byte."
    (unless (integerp byte) (signal 'wrong-type-argument (list 'integerp byte)))
    (unless (<= 0 byte 255) (signal 'error (list "Invalid byte")))
    (unibyte-string byte)))

(unless (fboundp 'compare-buffer-substrings)
  (defun compare-buffer-substrings (buffer1 start1 end1 buffer2 start2 end2)
    "Compare the designated buffer substrings and return a signed difference position."
    ;; gnu562: no buffer switch for a nil BUFFER (the current buffer) or
    ;; for reading the current buffer's `case-fold-search', and no
    ;; `downcase' for equal characters (about 21 -> 12 us).
    (let* ((a (if buffer1
                  (with-current-buffer buffer1
                    (buffer-substring-no-properties (or start1 (point-min)) (or end1 (point-max))))
                (buffer-substring-no-properties (or start1 (point-min)) (or end1 (point-max)))))
           (b (if buffer2
                  (with-current-buffer buffer2
                    (buffer-substring-no-properties (or start2 (point-min)) (or end2 (point-max))))
                (buffer-substring-no-properties (or start2 (point-min)) (or end2 (point-max)))))
           (i 0) (n (min (length a) (length b))))
      (while (and (< i n)
                  (let ((ca (aref a i)) (cb (aref b i)))
                    (or (eq ca cb)
                        (and case-fold-search (eq (downcase ca) (downcase cb))))))
        (setq i (1+ i)))
      (cond ((= i n) (cond ((= (length a) (length b)) 0)
                           ((< (length a) (length b)) (- (1+ i)))
                           (t (1+ i))))
            ((< (aref a i) (aref b i)) (- (1+ i)))
            (t (1+ i))))))

(defun emacs-cc-editfns-1--field-bounds (pos)
  "Return the field boundaries around POS, honoring boundary insertion behavior."
  (unless (or (integerp pos) (and (fboundp 'markerp) (markerp pos)))
    (signal 'wrong-type-argument (list 'integer-or-marker-p pos)))
  (let* ((lo (point-min)) (hi (point-max))
         (at (get-text-property pos 'field))
         (before (and (> pos lo) (get-text-property (1- pos) 'field)))
         (value (if (and (/= pos lo) (not (eq at before))) before at))
         (beg pos) (end pos))
    ;; gnu562: up to eight characters one at a time (a scan costs about
    ;; three reads, so short runs stay cheaper this way), then one property scan for
    ;; the rest of the run (it stops at the first value that is not `eq').
    (let ((steps 9))
      (while (and (> beg lo) (eq (get-text-property (1- beg) 'field) value))
        (setq beg (if (> (setq steps (1- steps)) 0) (1- beg)
                    (previous-single-property-change beg 'field nil lo)))))
    (let ((steps 9))
      (while (and (< end hi) (eq (get-text-property end 'field) value))
        (setq end (if (> (setq steps (1- steps)) 0) (1+ end)
                    (next-single-property-change end 'field nil hi)))))
    (cons beg end)))

(unless (fboundp 'constrain-to-field)
  (defun constrain-to-field (new-pos old-pos &optional escape-from-edge only-in-line inhibit-capture-property)
    "Constrain NEW-POS to the text field containing OLD-POS."
    (unless (or (null new-pos) (integerp new-pos)
                (and (fboundp 'markerp) (markerp new-pos)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p new-pos)))
    (unless (or (integerp old-pos) (and (fboundp 'markerp) (markerp old-pos)))
      (signal 'wrong-type-argument (list 'integer-or-marker-p old-pos)))
    (let* ((pos (or new-pos (point)))
           (bounds (emacs-cc-editfns-1--field-bounds old-pos))
           (beg (car bounds)) (end (cdr bounds))
           (result (if (or (and (boundp 'inhibit-field-text-motion) inhibit-field-text-motion)
                           (and inhibit-capture-property (get-text-property old-pos inhibit-capture-property))
                           (and new-pos (or (< pos beg) (> pos end))))
                       (if (and (<= beg pos) (<= pos end)) pos
                         (if (< pos beg) beg end)) pos)))
      (when (and only-in-line (/= (line-number-at-pos result) (line-number-at-pos pos)))
        (setq result pos))
      (when (null new-pos) (goto-char result))
      result)))

(unless (fboundp 'delete-field)
  (defun delete-field (&optional pos)
    "Delete the field surrounding POS."
    (let* ((p (or pos (point))) (bounds (emacs-cc-editfns-1--field-bounds p)))
      (delete-region (car bounds) (cdr bounds)))))

(unless (fboundp 'field-string)
  (defun field-string (&optional pos)
    "Return the contents of the field surrounding POS."
    (let* ((p (or pos (point))) (bounds (emacs-cc-editfns-1--field-bounds p)))
      (buffer-substring (car bounds) (cdr bounds)))))

(unless (fboundp 'field-string-no-properties)
  (defun field-string-no-properties (&optional pos)
    "Return the contents of the field surrounding POS without text properties."
    (let* ((p (or pos (point))) (bounds (emacs-cc-editfns-1--field-bounds p)))
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(unless (fboundp 'gap-position)
  (defun gap-position ()
    "Return the position of the gap in the current buffer."
    (point)))

(unless (fboundp 'gap-size)
  (defun gap-size ()
    "Return the size of the current buffer's gap."
    (max 0 (- (max (buffer-size) 20) (buffer-size)))))

(unless (fboundp 'get-pos-property)
  (defun get-pos-property (position prop &optional object)
    "Return POSITION's property PROP, observing text-property stickiness."
    (unless (integerp position) (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
    (if (bufferp object)
        (with-current-buffer object
          (if (> position (point-min)) (get-text-property (1- position) prop) nil))
      (if (> position (with-current-buffer (or object (current-buffer)) (point-min)))
          (get-text-property (1- position) prop object)
        nil))))

(defun emacs-cc-editfns-1--group-line (text start end gid search)
  "Return the group name on TEXT's START..END line when its GID matches.
Compare the literal field spelling, retaining the seed's integral-float
and leading-zero behavior instead of numerically coercing database IDs."
  (let* ((first (funcall search ":" text start))
         (second (and first (< first end)
                      (funcall search ":" text (1+ first))))
         (third (and second (< second end)
                     (funcall search ":" text (1+ second)))))
    (when (and first (> first start) second third (< third end)
               (string= gid (substring text (1+ second) third)))
      (substring text start first))))

(defun emacs-cc-editfns-1--group-tail (text start gid search)
  "Find GID from START with bounded stack usage for a large database."
  (let (name)
    (while (and (not name) (< start (length text)))
      (let ((end (or (funcall search "\n" text start) (length text))))
        (setq name (emacs-cc-editfns-1--group-line text start end gid search)
              start (1+ end))))
    name))

(defun emacs-cc-editfns-1--group-small (text start gid search remaining)
  "Find GID in TEXT, using at most REMAINING recursive line frames.
Use native delimiter searches rather than the interpreted regexp engine's
per-character loops. Large databases fall back to a bounded-stack loop."
  (if (>= start (length text)) nil
    (if (= remaining 0)
        (emacs-cc-editfns-1--group-tail text start gid search)
      (let ((end (or (funcall search "\n" text start) (length text))))
        (or (emacs-cc-editfns-1--group-line text start end gid search)
            (emacs-cc-editfns-1--group-small
             text (1+ end) gid search (1- remaining)))))))

(unless (fboundp 'group-name)
  (defun group-name (gid)
    "Return the name of the group with numeric ID GID, or nil."
    (unless (numberp gid) (signal 'error (list "Invalid GID specification")))
    (let ((n (if (consp gid) (cdr gid) gid)))
      (when (and (numberp n) (= n (truncate n)))
        (when (< n 0) (signal 'error (list "Not an in-range integer, integral float, or cons of integers")))
        (let ((text (if (fboundp 'nl-syscall-read-file)
                        (nl-syscall-read-file "/etc/group")
                      (with-temp-buffer
                        (insert-file-contents "/etc/group")
                        (buffer-string)))))
          (emacs-cc-editfns-1--group-small
           text 0 (number-to-string n)
           (if (fboundp 'nelisp--string-search)
               #'nelisp--string-search #'string-search)
           100))))))

(unless (fboundp 'group-real-gid)
  (defun group-real-gid ()
    "Return the real group ID of Emacs."
    (if (fboundp 'user-real-gid) (user-real-gid) 0)))

(provide 'emacs-cc-editfns-1)
