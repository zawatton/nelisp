;;; emacs-cc-character-display-1.el --- Text character descriptions -*- lexical-binding: t; -*-

(unless (fboundp 'text-char-description)
  (defun text-char-description (&rest arguments)
    "Describe CHARACTER using text notation, including ASCII caret escapes."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'text-char-description (length arguments))))
    (let ((character (car arguments)))
      (unless (characterp character)
        (signal 'wrong-type-argument (list 'characterp character)))
      (cond
       ((< character 32) (unibyte-string ?^ (+ character 64)))
       ((= character 127) (unibyte-string ?^ ??))
       ((< character 128) (unibyte-string character))
       (t (char-to-string character))))))

(provide 'emacs-cc-character-display-1)
;;; emacs-cc-character-display-1.el ends here
