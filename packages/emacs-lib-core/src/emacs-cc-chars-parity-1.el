;;; emacs-cc-chars-parity-1.el --- chars C-core parity fixes -*- lexical-binding: t; -*-

;;; Code:

;; gnu562: whether unibyte STRING has both ASCII and non-ASCII bytes, the
;; case `string-as-multibyte' below must split.  A short string is scanned
;; directly; two regexp searches cost about 45 us however short it is.
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
  (defun string-as-multibyte (&rest arguments)
    "Reinterpret a unibyte string's bytes as multibyte text.
The native conversion mis-stores a high byte directly followed by an ASCII
byte.  ASCII bytes never join a multibyte sequence, so convert each run of
non-ASCII bytes natively and pass ASCII runs through."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'string-as-multibyte (length arguments))))
    (let ((string (car arguments)))
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
          (apply #'concat (nreverse parts)))))))

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
