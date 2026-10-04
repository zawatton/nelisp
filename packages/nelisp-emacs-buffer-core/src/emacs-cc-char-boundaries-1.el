;;; emacs-cc-char-boundaries-1.el --- Character property boundaries -*- lexical-binding: t; -*-

(defun emacs-cc-char-boundaries-1--position (position)
  "Resolve an integer or marker POSITION."
  (cond ((integerp position) position)
        ((markerp position) (marker-position position))
        (t (signal 'wrong-type-argument
                   (list 'integer-or-marker-p position)))))

(defun emacs-cc-char-boundaries-1--arguments (name arguments)
  "Validate NAME's boundary search ARGUMENTS and return numeric values."
  (unless (<= 1 (length arguments) 2)
    (signal 'wrong-number-of-arguments (list name (length arguments))))
  (let ((position (emacs-cc-char-boundaries-1--position (car arguments)))
        (limit (and (cadr arguments)
                    (emacs-cc-char-boundaries-1--position (cadr arguments)))))
    (unless (<= (point-min) position (point-max))
      (signal 'args-out-of-range (list position position)))
    (cons position limit)))

(unless (fboundp 'next-char-property-change)
  (defun next-char-property-change (&rest arguments)
    "Return the next text-property or overlay boundary after POSITION."
    (let* ((values (emacs-cc-char-boundaries-1--arguments
                    'next-char-property-change arguments))
           (position (car values))
           (limit (min (or (cdr values) (point-max)) (point-max))))
      (if (<= limit position)
          limit
        (min (or (next-property-change position nil limit) limit)
             (next-overlay-change position)
             limit)))))

(unless (fboundp 'previous-char-property-change)
  (defun previous-char-property-change (&rest arguments)
    "Return the previous text-property or overlay boundary before POSITION."
    (let* ((values (emacs-cc-char-boundaries-1--arguments
                    'previous-char-property-change arguments))
           (position (car values))
           (limit (max (or (cdr values) (point-min)) (point-min)))
           (scan limit)
           (previous limit)
           (next nil))
      (if (>= limit position)
          limit
        (while (and (< scan position)
                    (setq next (next-property-change scan nil position))
                    (< next position)
                    (> next scan))
          (setq previous next scan next))
        (max previous (previous-overlay-change position) limit)))))

(defun emacs-cc-char-boundaries-1--overlay-priority (overlay)
  "Return OVERLAY's numeric primary and secondary priorities."
  (let ((priority (overlay-get overlay 'priority)))
    (cons (cond ((integerp priority) priority)
                ((and (consp priority) (integerp (car priority)))
                 (car priority))
                (t 0))
          (if (and (consp priority) (integerp (cdr priority)))
              (cdr priority) 0))))

(defun emacs-cc-char-boundaries-1--overlay-before-p (a b)
  "Return non-nil when overlay A takes precedence over B."
  (let ((pa (emacs-cc-char-boundaries-1--overlay-priority a))
        (pb (emacs-cc-char-boundaries-1--overlay-priority b)))
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
     ((/= (cdr pa) (cdr pb)) (> (cdr pa) (cdr pb)))
     (t (> (emacs-buffer--overlay-rec-id a)
           (emacs-buffer--overlay-rec-id b))))))

(unless (fboundp 'get-char-property-and-overlay)
  (defun get-char-property-and-overlay (&rest arguments)
    "Return (VALUE . OVERLAY) for PROP at POSITION in OBJECT."
    (unless (<= 2 (length arguments) 3)
      (signal 'wrong-number-of-arguments
              (list 'get-char-property-and-overlay (length arguments))))
    (let* ((position (emacs-cc-char-boundaries-1--position (car arguments)))
           (property (cadr arguments))
           (object (or (caddr arguments) (current-buffer)))
           (window (and (windowp object) object)))
      (when window (setq object (window-buffer window)))
      (unless (or (stringp object) (bufferp object))
        (signal 'wrong-type-argument (list 'buffer-or-string-p object)))
      (let ((minimum (if (stringp object) 0
                       (with-current-buffer object (point-min))))
            (maximum (if (stringp object) (length object)
                       (with-current-buffer object (point-max)))))
        (unless (<= minimum position maximum)
          (signal 'args-out-of-range
                  (if (stringp object) (list position position)
                    (list position))))
        (if (= position maximum)
            (cons nil nil)
          (or (and (not (stringp object))
                   (catch 'found
                     (dolist (overlay
                              (sort (copy-sequence
                                     (emacs-buffer-overlays-at position object))
                                    #'emacs-cc-char-boundaries-1--overlay-before-p))
                       (when (or (null window)
                                 (null (overlay-get overlay 'window))
                                 (eq (overlay-get overlay 'window) window))
                         (let* ((properties (overlay-properties overlay))
                                (value (if (plist-member properties property)
                                           (plist-get properties property)
                                         (get (plist-get properties 'category)
                                              property))))
                           (when value (throw 'found (cons value overlay)))))
                     nil)))
              (cons (get-text-property position property object) nil)))))))

(provide 'emacs-cc-char-boundaries-1)
;;; emacs-cc-char-boundaries-1.el ends here
