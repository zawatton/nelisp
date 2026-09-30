;;; emacs-cc-charset-1.el --- Charset C primitive compatibility -*- lexical-binding: t; -*-

(defvar emacs-cc-charset-1--aliases nil)
(defvar emacs-cc-charset-1--equivalences nil)
(defvar emacs-cc-charset-1--properties nil)

(defun emacs-cc-charset-1--check-charset (charset)
  (unless (memq charset '(ascii unicode eight-bit))
    (signal 'wrong-type-argument (list 'charsetp charset)))
  charset)

(defun emacs-cc-charset-1--charsets (string)
  (let ((i 0) (len (length string)) result)
    (while (< i len)
      (let ((cs (if (< (aref string i) 128) 'ascii 'unicode)))
        (unless (memq cs result) (setq result (append result (list cs)))))
      (setq i (1+ i)))
    result))

(unless (fboundp 'char-charset)
  (defun char-charset (ch &optional restriction)
    "Return the charset of highest priority that contains CH."
    (unless (and (integerp ch) (<= 0 ch) (<= ch #x3fffff))
      (signal 'wrong-type-argument (list 'characterp ch)))
    (let ((cs (if (< ch 128) 'ascii 'unicode)))
      (if (or (null restriction) (memq cs restriction)) cs nil))))

(unless (fboundp 'charset-after)
  (defun charset-after (&optional pos)
    "Return charset of a character in the current buffer at position POS."
    (let ((p (or pos (point))))
      (when (and (integerp p) (<= (point-min) p) (< p (point-max)))
        (char-charset (char-after p))))))

(unless (fboundp 'charset-id-internal)
  (defun charset-id-internal (&optional charset)
    "Internal use only. Return charset identification number of CHARSET."
    (emacs-cc-charset-1--check-charset charset)
    (cond ((eq charset 'ascii) 0) ((eq charset 'unicode) 2) (t 1))))

(unless (fboundp 'charset-plist)
  (defun charset-plist (charset)
    "Return the property list of CHARSET."
    (emacs-cc-charset-1--check-charset charset)
    (or (plist-get emacs-cc-charset-1--properties charset)
        (list :name charset))))

(unless (fboundp 'charset-priority-list)
  (defun charset-priority-list (&optional highestp)
    "Return the list of charsets ordered by priority."
    (if highestp 'ascii '(ascii unicode eight-bit))))

(unless (fboundp 'clear-charset-maps)
  (defun clear-charset-maps ()
    "Internal use only. Clear temporary charset mapping tables."
    (setq emacs-cc-charset-1--equivalences nil)))

(unless (fboundp 'declare-equiv-charset)
  (defun declare-equiv-charset (dimension chars final-char charset)
    "Declare an equivalent charset for ISO-2022 decoding."
    (unless (memq dimension '(1 2)) (signal 'wrong-type-argument (list 'integerp dimension)))
    (unless (memq chars '(94 96)) (error "Invalid CHARS %s, it should be 94 or 96" chars))
    (unless (and (integerp final-char) (<= 0 final-char) (<= final-char 127))
      (signal 'wrong-type-argument (list 'characterp final-char)))
    (emacs-cc-charset-1--check-charset charset)
    (push (list dimension chars final-char charset) emacs-cc-charset-1--equivalences)
    nil))

(unless (fboundp 'define-charset-alias)
  (defun define-charset-alias (alias charset)
    "Define ALIAS as an alias for charset CHARSET."
    (unless (symbolp alias) (signal 'wrong-type-argument (list 'symbolp alias)))
    (emacs-cc-charset-1--check-charset charset)
    (setq emacs-cc-charset-1--aliases (assq-delete-all alias emacs-cc-charset-1--aliases))
    (push (cons alias charset) emacs-cc-charset-1--aliases)
    nil))

(unless (fboundp 'define-charset-internal)
  (defun define-charset-internal (arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 &rest rest)
    "For internal use only."
    (ignore arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 rest)
    (let ((n (+ 17 (length rest))))
      (unless (= n 17)
        (signal 'wrong-number-of-arguments (list 'define-charset-internal n)))
      (unless (symbolp arg1) (signal 'wrong-type-argument (list 'symbolp arg1)))
      (unless (arrayp arg2) (signal 'wrong-type-argument (list 'arrayp arg2)))
      (error "Charset definitions are not supported by this runtime"))))

(unless (fboundp 'encode-char)
  (defun encode-char (ch charset)
    "Encode the character CH into a code-point of CHARSET."
    (unless (and (integerp ch) (<= 0 ch) (<= ch #x3fffff))
      (signal 'wrong-type-argument (list 'characterp ch)))
    (emacs-cc-charset-1--check-charset charset)
    (if (or (and (eq charset 'ascii) (< ch 128))
            (and (eq charset 'unicode) (>= ch 128))) ch nil)))

(unless (fboundp 'find-charset-region)
  (defun find-charset-region (beg end &optional table)
    "Return a list of charsets in the region between BEG and END."
    (ignore table)
    (unless (integerp beg) (signal 'wrong-type-argument (list 'integer-or-marker-p beg)))
    (unless (integerp end) (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
    (let ((p beg) result)
      (while (< p end)
        (let* ((ch (char-after p)) (cs (and ch (if (< ch 128) 'ascii 'unicode))))
          (when (and cs (not (memq cs result)))
            (setq result (append result (list cs)))))
        (setq p (1+ p)))
      result)))

(unless (fboundp 'find-charset-string)
  (defun find-charset-string (str &optional table)
    "Return a list of charsets in STR."
    (ignore table)
    (unless (stringp str) (signal 'wrong-type-argument (list 'stringp str)))
    (emacs-cc-charset-1--charsets str)))

(provide 'emacs-cc-charset-1)
