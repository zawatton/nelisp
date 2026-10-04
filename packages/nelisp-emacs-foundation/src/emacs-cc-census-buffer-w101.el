;;; emacs-cc-census-buffer-w101.el --- Local values and word capitalization  -*- lexical-binding: t; -*-

(defun emacs-cc-census-buffer-w101--buffer (buffer)
  "Return BUFFER or the current buffer, checking its type."
  (setq buffer (or buffer (current-buffer)))
  (unless (bufferp buffer)
    (signal 'wrong-type-argument (list 'bufferp buffer)))
  buffer)

(defun emacs-cc-census-buffer-w101--locals (buffer)
  "Return the buffer-local cells stored for BUFFER."
  (let ((state (gethash buffer emacs-buffer--state)))
    (and state (emacs-buffer--ext-locals state))))

(unless (fboundp 'buffer-local-toplevel-value)
  (defun buffer-local-toplevel-value (symbol &optional buffer)
    "Return SYMBOL's local value in BUFFER outside any let binding.
BUFFER defaults to the current buffer.  Signal `void-variable' if
SYMBOL has no local binding or its local binding has no value."
    (unless (symbolp symbol)
      (signal 'wrong-type-argument (list 'symbolp symbol)))
    (let* ((buffer (emacs-cc-census-buffer-w101--buffer buffer))
           (cell (assq symbol (emacs-cc-census-buffer-w101--locals buffer))))
      (unless cell
        (signal 'void-variable (list symbol)))
      (if (eq buffer (current-buffer))
          ;; The evaluator's global-cell API bypasses dynamic bindings.
          ;; The sidecar may be stale after an ordinary `set' or `setq'.
          (if (nelisp--env-globals-is-bound symbol)
              (nelisp--env-globals-get-value symbol)
            (signal 'void-variable (list symbol)))
        (cdr cell)))))

(unless (fboundp 'buffer-local-variables)
  (defun buffer-local-variables (&optional buffer)
    "Return a fresh alist of BUFFER's buffer-local variables.
A locally unbound variable appears as a symbol rather than a cons.
BUFFER defaults to the current buffer.  Changing the returned conses
does not change any variable value."
    (let* ((buffer (emacs-cc-census-buffer-w101--buffer buffer))
           (current (eq buffer (current-buffer)))
           (cells (emacs-cc-census-buffer-w101--locals buffer))
           result)
      (dolist (cell cells)
        (let ((symbol (car cell)))
          (push (if current
                    (if (boundp symbol)
                        (cons symbol (symbol-value symbol))
                      symbol)
                  (cons symbol (cdr cell)))
                result)))
      (nreverse result))))

(defun emacs-cc-census-buffer-w101--word-character-p (position)
  "Return whether the character at POSITION participates in word motion."
  (let ((syntax (char-syntax (char-after position))))
    (or (eq syntax ?w)
        (and (boundp 'words-include-escapes) words-include-escapes
             (memq syntax '(?\\ ?/))))))

(defun emacs-cc-census-buffer-w101--word-end (position count)
  "Scan COUNT words from POSITION without changing point.
Stop at the accessible buffer boundary if there are too few words."
  (let ((low (point-min)) (high (point-max)))
    (if (> count 0)
        (while (and (> count 0) (< position high))
          (while (and (< position high)
                      (not (emacs-cc-census-buffer-w101--word-character-p position)))
            (setq position (1+ position)))
          (while (and (< position high)
                      (emacs-cc-census-buffer-w101--word-character-p position))
            (setq position (1+ position)))
          (setq count (1- count)))
      (while (and (< count 0) (> position low))
        (while (and (> position low)
                    (not (emacs-cc-census-buffer-w101--word-character-p (1- position))))
          (setq position (1- position)))
        (while (and (> position low)
                    (emacs-cc-census-buffer-w101--word-character-p (1- position)))
          (setq position (1- position)))
        (setq count (1+ count))))
    position))

(defun emacs-cc-census-buffer-w101--capitalize (low high)
  "Capitalize LOW through HIGH and return the adjusted end position."
  (let ((size (buffer-size)))
    (capitalize-region low high)
    (+ high (- (buffer-size) size))))

(unless (fboundp 'capitalize-word)
  (defun capitalize-word (arg)
    "Capitalize ARG words from point, advancing over them.
If point is within a word, change only the portion after point.
With negative ARG, capitalize preceding words and leave point at
its original location relative to the text.  Return nil."
    (interactive "p")
    (unless (integerp arg)
      (signal 'wrong-type-argument (list 'fixnump arg)))
    (let* ((start (point))
           (end (emacs-cc-census-buffer-w101--word-end start arg))
           (low (min start end))
           (high (max start end))
           (adjusted (emacs-cc-census-buffer-w101--capitalize low high)))
      (goto-char (if (< arg 0) (+ start (- adjusted high)) adjusted)))
    nil))

(provide 'emacs-cc-census-buffer-w101)
;;; emacs-cc-census-buffer-w101.el ends here
