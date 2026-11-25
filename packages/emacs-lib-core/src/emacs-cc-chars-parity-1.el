;;; emacs-cc-chars-parity-1.el --- chars C-core parity fixes -*- lexical-binding: t; -*-

;;; Code:

;; Older readers need separate runs for strings mixing ASCII and high bytes.
(defun emacs-cc-chars-parity-1--mixed-bytes-p (string)
  "Non-nil when unibyte STRING has both ASCII and non-ASCII bytes."
  (if (> (length string) 128)
      (and (string-match-p "[\200-\377]" string)
           (string-match-p "[\0-\177]" string))
    (let ((i 0) (n (length string)) (high nil) (low nil))
      (while (and (< i n) (not (and high low)))
        (if (< (aref string i) 128) (setq low t) (setq high t))
        (setq i (1+ i)))
      (and high low))))

(when (and (fboundp 'nelisp--repr) (fboundp 'string-as-multibyte))
  (unless (fboundp 'emacs-cc-chars-parity-1--native-string-as-multibyte)
    (fset 'emacs-cc-chars-parity-1--native-string-as-multibyte
          (symbol-function 'string-as-multibyte)))
  (defun emacs-cc-chars-parity-1--convert-runs (string)
    (if (or (not (stringp string)) (multibyte-string-p string)
            (not (emacs-cc-chars-parity-1--mixed-bytes-p string)))
        (emacs-cc-chars-parity-1--native-string-as-multibyte string)
      (let ((index 0) (length (length string)) (parts nil))
        (while (< index length)
          (let ((start index)
                (ascii (< (aref string index) 128)))
            (while (and (< index length)
                        (eq ascii (< (aref string index) 128)))
              (setq index (1+ index)))
            (push (emacs-cc-chars-parity-1--native-string-as-multibyte
                   (substring string start index))
                  parts)))
        (apply #'concat (nreverse parts)))))
  (let* ((native (symbol-function
                  'emacs-cc-chars-parity-1--native-string-as-multibyte))
         ;; Older readers corrupted the boundary after a raw high byte.
         ;; Check both the raw and the valid UTF-8 paths at installation.
         (sample (ignore-errors
                   (funcall native (unibyte-string #xff ?a #xc3 #xa9 ?b))))
         (convert (if (and (multibyte-string-p sample)
                           (equal (append sample nil) '(#x3fffff ?a #xe9 ?b)))
                      native
                    #'emacs-cc-chars-parity-1--convert-runs)))
    (fset 'string-as-multibyte
          (lambda (&rest arguments)
            "Reinterpret the bytes in STRING as multibyte text.

(fn STRING)"
            (unless (= (length arguments) 1)
              (signal 'wrong-number-of-arguments
                      (list 'string-as-multibyte (length arguments))))
            (funcall convert (car arguments))))))

;; The native boundary searches report arity errors with a subr object, GNU
;; reports the function symbol.
(when (and (fboundp 'nelisp--repr) (fboundp 'next-char-property-change)
           (subrp (symbol-function 'next-char-property-change)))
  (dolist (name '(next-char-property-change previous-char-property-change))
    (let ((native (intern (format "emacs-cc-chars-parity-1--native-%s" name)))
          (name name))
      (unless (fboundp native) (fset native (symbol-function name)))
      (fset name
            (lambda (&rest arguments)
              (unless (<= 1 (length arguments) 2)
                (signal 'wrong-number-of-arguments
                        (list name (length arguments))))
              (apply native arguments))))))

(provide 'emacs-cc-chars-parity-1)
;;; emacs-cc-chars-parity-1.el ends here
