;;; emacs-cc-x-geometry-1.el --- X geometry C primitives -*- lexical-binding: t; -*-

(defun emacs-cc-x-geometry-1--number-at (string position)
  "Return (NUMBER . END) for digits at POSITION in STRING, or nil."
  (let ((index position)
        (value 0))
    (while (and (< index (length string))
                (<= ?0 (aref string index) ?9))
      (setq value (+ (* value 10) (- (aref string index) ?0)))
      (setq index (1+ index)))
    (and (> index position) (cons value index))))

(unless (fboundp 'x-parse-geometry)
  (defun x-parse-geometry (string)
    "Parse display geometry STRING into GNU-compatible frame parameters.
Return an alist with any of `height', `width', `top', and `left', or nil
when STRING is not a complete geometry specification."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (let ((length (length string))
          (index 0)
          width height x-offset y-offset invalid)
      ;; XParseGeometry accepts leading whitespace and an optional `='.
      (while (and (< index length)
                  (memq (aref string index) '(32 9 10 13 12)))
        (setq index (1+ index)))
      (when (and (< index length) (= (aref string index) ?=))
        (setq index (1+ index))
        (while (and (< index length)
                    (memq (aref string index) '(32 9 10 13 12)))
          (setq index (1+ index))))
      ;; A leading sign starts an x offset.  Otherwise parse an optional
      ;; width followed by an optional x/X and required height.
      (cond
       ((and (< index length)
             (memq (aref string index) '(?+ ?-)))
        nil)
       ((and (< index length) (= (aref string index) ?x))
        (setq index (1+ index))
        (when (and (< index length)
                   (memq (aref string index) '(?+ ?-)))
          (setq index (1+ index)))
        (let ((parsed (emacs-cc-x-geometry-1--number-at string index)))
          (if parsed
              (setq height (car parsed) index (cdr parsed))
            (setq invalid t))))
       ((and (< index length) (= (aref string index) ?X))
        (setq index (1+ index))
        (when (and (< index length)
                   (memq (aref string index) '(?+ ?-)))
          (setq index (1+ index)))
        (let ((parsed (emacs-cc-x-geometry-1--number-at string index)))
          (if parsed
              (setq height (car parsed) index (cdr parsed))
            (setq invalid t))))
       (t
        (let ((parsed (emacs-cc-x-geometry-1--number-at string index)))
          (when parsed
            (setq width (car parsed) index (cdr parsed)))
          (when (and (< index length)
                     (memq (aref string index) '(?x ?X)))
            (setq index (1+ index))
            (let ((height-parsed
                   (emacs-cc-x-geometry-1--number-at string index)))
              (if height-parsed
                  (setq height (car height-parsed)
                        index (cdr height-parsed))
                (setq invalid t))))
          (unless parsed
            (setq invalid t)))))
      ;; Each signed offset is parsed separately so a negative zero retains
      ;; its sign, which GNU returns as the list (- 0).
      (let ((offset-count 0))
        (while (and (not invalid) (< index length))
          (let ((sign (aref string index)))
            (if (not (memq sign '(?+ ?-)))
                (setq invalid t)
              (setq index (1+ index))
              (let ((parsed (emacs-cc-x-geometry-1--number-at string index)))
                (if (not parsed)
                    (setq invalid t)
                  (setq index (cdr parsed))
                  (setq offset-count (1+ offset-count))
                  (if (> offset-count 2)
                      (setq invalid t)
                    (let ((value (car parsed))
                          (negative (= sign ?-)))
                      (if (= offset-count 1)
                          (setq x-offset (if (and negative (= value 0))
                                             (list '- 0)
                                           (if negative (- value) value)))
                        (setq y-offset (if (and negative (= value 0))
                                          (list '- 0)
                                        (if negative (- value) value)))))))))))
      (unless (or invalid (/= index length))
        (let (result)
          (when x-offset (push (cons 'left x-offset) result))
          (when y-offset (push (cons 'top y-offset) result))
          (when width (push (cons 'width width) result))
          (when height (push (cons 'height height) result))
          result))))))

(provide 'emacs-cc-x-geometry-1)
;;; emacs-cc-x-geometry-1.el ends here
