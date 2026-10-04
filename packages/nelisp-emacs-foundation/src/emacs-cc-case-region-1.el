;;; emacs-cc-case-region-1.el --- Region case conversion -*- lexical-binding: t; -*-
(require 'emacs-cc-case-data)
(require 'emacs-mark-state)
(defvar region-extract-function nil)

(defvar emacs-cc-case--mapping
  (let ((table (make-hash-table :test 'eql)))
    (dolist (entry emacs-cc-case--unicode-data)
      (puthash (car entry) (cdr entry) table))
    table))

(defun emacs-cc-case--replace-character (position replacement)
  "Replace one character without collapsing markers; return the length delta."
  (let* ((buffer (current-buffer)) (delta (1- (length replacement))))
    (if (and (fboundp 'nelisp-buffer-p) (nelisp-buffer-p buffer))
        (progn
          ;; Unicode titles can change byte width and character count.
          (let* ((before (nelisp-buffer-before-gap buffer)) (index (1- position)))
            (if (< index (length before))
                (setf (nelisp-buffer-before-gap buffer)
                      (concat (substring before 0 index) replacement
                              (substring before (1+ index))))
              (let* ((after (nelisp-buffer-after-gap buffer))
                     (offset (- index (length before))))
                (setf (nelisp-buffer-after-gap buffer)
                      (concat (substring after 0 offset) replacement
                              (substring after (1+ offset)))))))
          (unless (= delta 0)
            ;; GNU case conversion preserves marker positions even when a
            ;; title expands. Point still follows its converted character.
            (let ((pending (gethash buffer nelisp-buffer--pending-point)))
              (when (and pending (> pending position))
                (puthash buffer (+ pending delta) nelisp-buffer--pending-point)))
            (when (and (nelisp-buffer-narrow-start buffer)
                       (> (nelisp-buffer-narrow-start buffer) position))
              (setf (nelisp-buffer-narrow-start buffer)
                    (+ (nelisp-buffer-narrow-start buffer) delta)))
            (when (and (nelisp-buffer-narrow-end buffer)
                       (> (nelisp-buffer-narrow-end buffer) position))
              (setf (nelisp-buffer-narrow-end buffer)
                    (+ (nelisp-buffer-narrow-end buffer) delta)))
            (let ((ext (gethash buffer emacs-buffer--state)))
              (when ext
                (setf (emacs-buffer--ext-text-props ext)
                      (emacs-buffer--tp-after-insert
                       (emacs-buffer--ext-text-props ext) position delta)))))
          (setf (nelisp-buffer-modified buffer) t)
          (nelisp-buffer--bump-tick buffer))
      (let ((saved (point)))
        (goto-char position) (delete-region position (1+ position)) (insert replacement)
        (goto-char (+ saved (if (> saved position) delta 0)))))
    delta))

(defun emacs-cc-case--final-sigma-p (position limit previous-cased)
  "Return whether the sigma at POSITION needs its final form."
  (and previous-cased
       (let ((next (1+ position)))
         (while (and (< next limit)
                     (not (gethash (char-after next) emacs-cc-case--mapping))
                     (or (<= #x300 (char-after next) #x36f)
                         (= (char-after next) 39)
                         (and (fboundp 'get-char-code-property)
                              (memq (get-char-code-property (char-after next) 'general-category)
                                    '(Mn Me Cf Lm Sk)))))
           (setq next (1+ next)))
         (or (>= next limit)
             (not (gethash (char-after next) emacs-cc-case--mapping))))))

(defun emacs-cc-case--region (begin end initials noncontiguous)
  "Change case between BEGIN and END, preserving properties and positions."
  (let ((regions (if noncontiguous (funcall region-extract-function 'bounds)
                   (list (cons begin end)))))
    (dolist (region regions)
      (let* ((first (if (markerp (car region)) (marker-position (car region)) (car region)))
             (last (if (markerp (cdr region)) (marker-position (cdr region)) (cdr region))))
        (unless (integerp first)
          (signal 'wrong-type-argument (list 'integer-or-marker-p (car region))))
        (unless (integerp last)
          (signal 'wrong-type-argument (list 'integer-or-marker-p (cdr region))))
        (let ((low (min first last)) (high (max first last)))
          (unless (and (>= low (point-min)) (<= high (point-max)))
            (signal 'args-out-of-range (list (current-buffer) first last)))
          ;; Check the whole writable range before changing any character.
          (when (< low high)
            (barf-if-buffer-read-only)
            (when (and (not inhibit-read-only)
                       (text-property-not-all low high 'read-only nil))
              (signal 'text-read-only nil)))
          (let ((position low) (in-word nil) (previous-cased nil))
            (while (< position high)
              (let* ((character (char-after position))
                     (word (eq (char-syntax character) ?w))
                     (mapping (gethash character emacs-cc-case--mapping))
                     (old (char-to-string character))
                     (replacement
                      (cond
                       ((and word (not in-word) mapping) (car mapping))
                       ((and word (not initials) mapping)
                        (if (and (= character #x3a3)
                                 (emacs-cc-case--final-sigma-p position high previous-cased))
                            "ς" (cadr mapping)))
                       (t old)))
                     (delta (if (equal old replacement) 0
                              (emacs-cc-case--replace-character position replacement))))
                (setq high (+ high delta) position (+ position 1 delta)
                      in-word word previous-cased (and word mapping)))))))))
  nil)

(unless (fboundp 'capitalize-region)
  (defun capitalize-region (begin end &optional noncontiguous)
    "Capitalize words in the buffer region from BEGIN to END."
    (emacs-cc-case--region begin end nil noncontiguous)))
(unless (fboundp 'upcase-initials-region)
  (defun upcase-initials-region (begin end &optional noncontiguous)
    "Uppercase word initials in the region from BEGIN to END."
    (emacs-cc-case--region begin end t noncontiguous)))
(provide 'emacs-cc-case-region-1)
;;; emacs-cc-case-region-1.el ends here
