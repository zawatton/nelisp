;;; emacs-cc-bare-symbol-1.el --- bare-symbol-p compatibility -*- lexical-binding: t; -*-

(unless (fboundp 'bare-symbol-p)
  (defun bare-symbol-p (&rest arguments)
    "Return non-nil when OBJECT is a bare symbol."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'bare-symbol-p (length arguments))))
    (let ((object (car arguments)))
      (and (symbolp object)
         (not (and (fboundp 'symbol-with-pos-p)
                   (symbol-with-pos-p object)))))))

(provide 'emacs-cc-bare-symbol-1)
;;; emacs-cc-bare-symbol-1.el ends here
