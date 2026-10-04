;;; emacs-cc-byte8-conversion-1.el --- Byte8 conversion diagnostics -*- lexical-binding: t; -*-

(defun emacs-byte8--string-to-unibyte (arguments)
  "Convert ARGUMENTS using native storage with Lisp character validation."
  ;; A captured native function called through apply bypasses its arity guard.
  (unless (= (length arguments) 1)
    (signal 'wrong-number-of-arguments
            (list 'string-to-unibyte (length arguments))))
  (condition-case condition
      (apply #'emacs-byte8--native-string-to-unibyte arguments)
    (error
     (let ((source (car arguments)))
       (if (and (eq (car condition) 'error)
                (stringp source) (multibyte-string-p source))
           (let ((index 0) (length (length source)) (bad nil))
             (while (and (< index length) (null bad))
               (let ((character (aref source index)))
                 (unless (or (< character 128) (>= character #x3fff80))
                   (setq bad index)))
               (setq index (1+ index)))
             (if bad
                 (signal 'error
                         (list (format "Cannot convert character at index %d to unibyte" bad)))
               (signal (car condition) (cdr condition))))
         (signal (car condition) (cdr condition)))))))

(when (fboundp 'nelisp--repr)
  (unless (fboundp 'emacs-byte8--native-string-to-unibyte)
    (fset 'emacs-byte8--native-string-to-unibyte
          (symbol-function 'string-to-unibyte)))
  (defun string-to-unibyte (&rest arguments)
    "Return a unibyte string with SOURCE's ASCII or byte8 characters."
    (emacs-byte8--string-to-unibyte arguments))
  ;; The obsolete operation truncates Unicode characters instead of rejecting.
  (defun string-make-unibyte (&rest arguments)
    "Return SOURCE's characters truncated to their low eight bits."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'string-make-unibyte (length arguments))))
    (let ((source (car arguments)))
      (unless (stringp source)
        (signal 'wrong-type-argument (list 'stringp source)))
      (if (not (multibyte-string-p source))
          source
        (let ((index 0) (parts nil))
          (while (< index (length source))
            (push (unibyte-string (logand (aref source index) 255)) parts)
            (setq index (1+ index)))
          (apply #'concat (nreverse parts)))))))

(provide 'emacs-cc-byte8-conversion-1)
;;; emacs-cc-byte8-conversion-1.el ends here
