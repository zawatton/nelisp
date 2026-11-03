;;; emacs-cc-obarray-clear-1.el --- Clear owned obarrays -*- lexical-binding: t; -*-

(unless (fboundp 'obarray-clear)
  (defun obarray-clear (&rest arguments)
    "Remove every interned symbol from OBARRAY."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'obarray-clear (length arguments))))
    (let ((obarray (car arguments)))
      (unless (obarrayp obarray)
        (signal 'wrong-type-argument (list 'obarrayp obarray)))
      (let ((table (nelisp--obarray-table obarray)))
        (clrhash table)
        nil))))

(provide 'emacs-cc-obarray-clear-1)
;;; emacs-cc-obarray-clear-1.el ends here
