;;; emacs-cc-logcount-1.el --- C-core integer bit count -*- lexical-binding: t; -*-

(unless (fboundp 'logcount)
  (defun logcount (&rest arguments)
    "Count the one bits in INTEGER's two's-complement representation.
For negative integers, count the bits in its complement."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'logcount (length arguments))))
    (let ((integer (car arguments)))
      (unless (integerp integer)
        (signal 'wrong-type-argument (list 'integerp integer)))
      (let ((bits (if (< integer 0) (lognot integer) integer))
            (count 0))
        (while (> bits 0)
          (setq bits (logand bits (1- bits))
                count (1+ count)))
        count))))

(provide 'emacs-cc-logcount-1)
;;; emacs-cc-logcount-1.el ends here
