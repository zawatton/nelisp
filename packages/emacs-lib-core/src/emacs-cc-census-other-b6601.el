;;; emacs-cc-census-other-b6601.el --- Obarray buckets and focus event validation  -*- lexical-binding: t; -*-

(unless (fboundp 'internal--obarray-buckets)
  (defun internal--obarray-buckets (&rest arguments)
    "Return a list containing the symbols in each bucket of OBARRAY."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'internal--obarray-buckets (length arguments))))
    (let* ((array (emacs-symbol--obarray-object (car arguments)))
           (state (emacs-symbol--obarray-state array)))
      (unless state
        (signal 'unsupported-feature '(obarray-bucket-history)))
      (let ((buckets (aref state 1)) (result nil) (i 0))
        (while (< i (length buckets))
          (push (copy-sequence (aref buckets i)) result)
          (setq i (1+ i)))
        (nreverse result)))))

(unless (fboundp 'internal-handle-focus-in)
  (defun internal-handle-focus-in (&rest arguments)
    "Validate EVENT as a focus-in event.
Valid events require pending switch-frame state owned by the command loop,
which the standalone runtime does not expose to this provider."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'internal-handle-focus-in (length arguments))))
    (let ((event (car arguments)))
      (unless (and (consp event) (eq (car event) 'focus-in)
                   (consp (cdr event)) (cadr event) (framep (cadr event)))
        (error "Invalid focus-in event")))
    (signal 'unsupported-feature '(focus-in-command-loop-state))))

(provide 'emacs-cc-census-other-b6601)
;;; emacs-cc-census-other-b6601.el ends here
