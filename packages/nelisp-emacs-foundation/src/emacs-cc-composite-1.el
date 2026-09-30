;;; emacs-cc-composite-1.el --- composite.c compatibility -*- lexical-binding: t; -*-
;;; Code:

(defun emacs-cc-composite-1--text (object)
  (cond ((null object) (buffer-string))
        ((stringp object) object)
        ((and (fboundp 'bufferp) (bufferp object))
         (with-current-buffer object (buffer-string)))
        (t (signal 'wrong-type-argument (list 'buffer-or-string-p object)))))

(defun emacs-cc-composite-1--validate-rules (rules)
  (unless (listp rules)
    (signal 'wrong-type-argument (list 'listp rules)))
  (dolist (rule rules)
    (unless (and (vectorp rule) (= (length rule) 3)
                 (or (null (aref rule 0)) (stringp (aref rule 0)))
                 (integerp (aref rule 1)) (symbolp (aref rule 2)))
      (error "Invalid composition rule in RULES argument"))))

(defvar emacs-cc-composite-1--cache (make-hash-table :test 'equal)
  "Cache used by composition support.")

(unless (fboundp 'clear-composition-cache)
  (defun clear-composition-cache ()
    "Internal use only.\nClear composition cache."
    (clrhash emacs-cc-composite-1--cache)
    nil))

(unless (fboundp 'compose-region-internal)
  (defun compose-region-internal (start end &optional components modification-func)
    "Internal use only.\n\nCompose text in the region between START and END.\nOptional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC\nfor the composition.  See `compose-region' for more details."
    (ignore components modification-func)
    (unless (and (or (integerp start) (markerp start))
                 (or (integerp end) (markerp end)))
      (signal 'wrong-type-argument
              (list 'integer-or-marker-p
                    (if (not (or (integerp start) (markerp start))) start end))))
    (let ((lo (min start end)) (hi (max start end)))
      (unless (and (<= (point-min) lo) (<= hi (point-max)))
        (signal 'args-out-of-range (list (current-buffer) lo hi)))
      (when (< lo hi)
        (put-text-property lo hi 'composition (list (list (- hi lo)))))
      nil)))

(unless (fboundp 'compose-string-internal)
  (defun compose-string-internal (string start end &optional components modification-func)
    "Internal use only.\n\nCompose text between indices START and END of STRING, where\nSTART and END are treated as in `substring'.  Optional 4th\nand 5th arguments are COMPONENTS and MODIFICATION-FUNC\nfor the composition.  See `compose-string' for more details."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (ignore components modification-func)
    (let ((lo (if (integerp start) start (signal 'wrong-type-argument (list 'integerp start))))
          (hi (if (integerp end) end (signal 'wrong-type-argument (list 'integerp end)))))
      (setq lo (if (< lo 0) (+ (length string) lo) lo)
            hi (if (< hi 0) (+ (length string) hi) hi))
      (when (> lo hi) (signal 'args-out-of-range (list string lo hi)))
      (when (or (< lo 0) (> hi (length string)))
        (signal 'args-out-of-range (list string start end)))
      (when (< lo hi)
        (put-text-property lo hi 'composition (list (list (- hi lo))) string))
      string)))

(unless (fboundp 'composition-get-gstring)
  (defun composition-get-gstring (from to font-object string)
    "Return a glyph-string for characters between FROM and TO."
    (ignore font-object)
    (let ((text (emacs-cc-composite-1--text string)))
      (unless (and (or (integerp from) (markerp from))
                   (or (integerp to) (markerp to)))
        (signal 'wrong-type-argument
                (list 'integer-or-marker-p
                      (if (not (or (integerp from) (markerp from))) from to))))
      (let* ((from (if (markerp from) (marker-position from) from))
             (to (if (markerp to) (marker-position to) to))
             (a (if (< from to) from to)) (b (if (< from to) to from)))
        (when (or (null a) (null b) (< a 0) (> b (length text)))
          (signal 'args-out-of-range (list (if string text (current-buffer)) a b)))
        (when (= a b) (error "Attempt to shape zero-length text"))
        (let* ((coding (or (and (fboundp 'terminal-coding-system) (terminal-coding-system))
                           'utf-8-unix))
               (header (vconcat (list coding) (string-to-list (substring text a b))))
               (glyphs (mapcar (lambda (i)
                                 (let ((c (aref text i)))
                                   (vector (- i a) (1+ (- i a)) c c 1 0 1 1 0 nil)))
                               (number-sequence a (1- b)))))
          (vconcat (vector header nil) glyphs (make-list (- 8 (length glyphs)) nil)))))))

(unless (fboundp 'composition-sort-rules)
  (defun composition-sort-rules (rules)
    "Sort composition RULES by their LOOKBACK parameter."
    (emacs-cc-composite-1--validate-rules rules)
    (if (null (cdr rules)) rules
      (sort (copy-sequence rules)
            (lambda (a b) (> (aref a 1) (aref b 1)))))))

(unless (fboundp 'find-composition-internal)
  (defun find-composition-internal (pos limit string detail-p)
    "Internal use only.\n\nReturn information about composition at or nearest to position POS.\nSee `find-composition' for more details."
    (ignore limit detail-p)
    (let ((text (emacs-cc-composite-1--text string)))
      (unless (or (integerp pos) (markerp pos))
        (signal 'wrong-type-argument (list 'integer-or-marker-p pos)))
      (when (markerp pos) (setq pos (marker-position pos)))
      (if string
          (let ((p (if (< pos 0) (+ (length text) pos) pos)))
            (when (> p (length text)) (signal 'args-out-of-range (list text p)))
            nil)
        (unless (and (<= (point-min) pos) (<= pos (point-max)))
          (signal 'args-out-of-range (list (current-buffer) pos)))
        nil))))

(provide 'emacs-cc-composite-1)
;;; emacs-cc-composite-1.el ends here
