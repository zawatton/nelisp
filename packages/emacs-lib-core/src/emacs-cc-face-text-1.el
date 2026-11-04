;;; emacs-cc-face-text-1.el --- Face text-property compatibility -*- lexical-binding: t; -*-

(defun emacs-cc-face-text-1--combine (face old append present)
  "Combine FACE with OLD, respecting APPEND and property PRESENT."
  (cond
   ((not present) face)
   ((and (atom old) (eq face old)) old)
   ((consp old)
    (if append (append old (list face)) (cons face old)))
   (append (list old face))
   (t (list face old))))

(unless (fboundp 'add-face-text-property)
  (defun add-face-text-property (&rest arguments)
    "Add FACE to the face property from START to END in OBJECT."
    (let ((count (length arguments)))
      (unless (<= 3 count 5)
        (signal 'wrong-number-of-arguments
                (list 'add-face-text-property count))))
    (let* ((start (nth 0 arguments))
           (end (nth 1 arguments))
           (face (nth 2 arguments))
           (append (nth 3 arguments))
           (object (nth 4 arguments)))
      (unless (or (integerp start) (markerp start))
        (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
      (unless (or (integerp end) (markerp end))
        (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
      (let* ((bounds
              (cond
               ((stringp object) (cons 0 (length object)))
               ((or (null object) (bufferp object))
                (with-current-buffer (or object (current-buffer))
                  (cons (point-min) (point-max))))
               (t (signal 'wrong-type-argument
                          (list 'buffer-or-string-p object)))))
             (lower (car bounds))
             (upper (cdr bounds)))
        (unless (and (<= lower start upper) (<= lower end upper))
          (signal 'args-out-of-range (list start end))))
      (when (> start end)
        (let ((old-start start))
          (setq start end end old-start)))
      (let ((position start))
        (while (< position end)
          (let* ((next (or (next-property-change position object end) end))
                 (properties (text-properties-at position object))
                 (present (plist-member properties 'face))
                 (old (get-text-property position 'face object))
                 (value (emacs-cc-face-text-1--combine
                         face old append present)))
            (put-text-property position next 'face value object)
            (setq position next))))
      nil)))

(provide 'emacs-cc-face-text-1)
;;; emacs-cc-face-text-1.el ends here
