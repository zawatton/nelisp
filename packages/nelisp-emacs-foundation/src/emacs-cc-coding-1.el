;;; emacs-cc-coding-1.el --- Coding C primitive fallbacks -*- lexical-binding: t; -*-

(defvar emacs-cc-coding-1--aliases (make-hash-table :test 'eq))
(defvar emacs-cc-coding-1--properties (make-hash-table :test 'eq))

(defun emacs-cc-coding-1--coding-system-get (original coding-system prop)
  (let ((plist (gethash coding-system emacs-cc-coding-1--properties)))
    (if (plist-member plist prop)
        (plist-get plist prop)
      (funcall original coding-system prop))))

(when (fboundp 'coding-system-get)
  (advice-add 'coding-system-get :around #'emacs-cc-coding-1--coding-system-get))

(unless (fboundp 'coding-system-aliases)
  (defun coding-system-aliases (coding-system)
    "Return the list of aliases of CODING-SYSTEM."
    (unless (coding-system-p coding-system) (signal 'coding-system-error (list coding-system)))
    (or (copy-sequence (gethash coding-system emacs-cc-coding-1--aliases))
        (and (eq coding-system 'utf-8) '(utf-8 mule-utf-8 cp65001)))))
(unless (fboundp 'coding-system-base)
  (defun coding-system-base (coding-system)
    "Return the base of CODING-SYSTEM."
    (unless (coding-system-p coding-system) (signal 'coding-system-error (list coding-system)))
    (or (get coding-system 'coding-system-base)
        (and (memq coding-system '(utf-8-dos utf-8-unix utf-8-mac)) 'utf-8)
        coding-system)))
(unless (fboundp 'coding-system-eol-type)
  (defun coding-system-eol-type (coding-system)
    "Return eol-type of CODING-SYSTEM."
    (when (coding-system-p coding-system)
      (or (get coding-system 'coding-system-eol-type)
          (pcase coding-system ((or 'utf-8-dos 'unix-dos) 1) ((or 'utf-8-mac 'unix-mac) 2) (_ 0))))))
(unless (fboundp 'coding-system-plist)
  (defun coding-system-plist (coding-system)
    "Return the property list of CODING-SYSTEM."
    (unless (coding-system-p coding-system) (signal 'coding-system-error (list coding-system)))
    (or (copy-sequence (gethash coding-system emacs-cc-coding-1--properties))
        (and (eq coding-system 'utf-8)
             '(:ascii-compatible-p t :category coding-category-utf-8 :name utf-8
               :docstring "UTF-8 (no signature (BOM))" :coding-type utf-8 :mnemonic 85
               :charset-list (unicode) :mime-charset utf-8)))))
(unless (fboundp 'coding-system-priority-list)
  (defun coding-system-priority-list (&optional highestp)
    "Return coding systems ordered by their priorities."
    (let ((systems '(utf-8 iso-2022-7bit iso-latin-1 iso-2022-7bit-lock
                     iso-2022-8bit-ss2 emacs-mule raw-text iso-2022-jp
                     in-is13194-devanagari chinese-iso-8bit utf-8-auto
                     utf-8-with-signature utf-16 utf-16be-with-signature
                     utf-16le-with-signature utf-16be utf-16le
                     japanese-shift-jis chinese-big5 undecided)))
      (if highestp (car systems) systems))))
(unless (fboundp 'coding-system-put)
  (defun coding-system-put (coding-system prop val)
    "Change value of CODING-SYSTEM's property PROP to VAL."
    (unless (coding-system-p coding-system) (signal 'coding-system-error (list coding-system)))
    (let ((plist (or (gethash coding-system emacs-cc-coding-1--properties)
                     (coding-system-plist coding-system))))
      (puthash coding-system (plist-put (copy-sequence plist) prop val)
               emacs-cc-coding-1--properties)
      (put coding-system prop val)
      t)))
(unless (fboundp 'define-coding-system-alias)
  (defun define-coding-system-alias (alias coding-system)
    "Define ALIAS as an alias for CODING-SYSTEM."
    (unless (coding-system-p coding-system) (signal 'coding-system-error (list coding-system)))
    (let ((old (gethash coding-system emacs-cc-coding-1--aliases)))
      (puthash coding-system (cons alias (delq alias old)) emacs-cc-coding-1--aliases)
      (put alias 'coding-system t)
      (put alias 'coding-system-base coding-system)
      alias)))
(unless (fboundp 'define-coding-system-internal)
  (defun define-coding-system-internal (&rest args)
    "For internal use only."
    (when (< (length args) 13)
      (signal 'wrong-number-of-arguments (list 'define-coding-system-internal 0)))
    (let ((arg1 (nth 0 args)) (arg2 (nth 1 args)) (arg3 (nth 2 args))
          (arg4 (nth 3 args)) (arg5 (nth 4 args)) (arg6 (nth 5 args))
          (arg7 (nth 6 args)) (arg8 (nth 7 args)) (arg9 (nth 8 args))
          (arg10 (nth 9 args)) (arg11 (nth 10 args)) (arg12 (nth 11 args))
          (arg13 (nth 12 args)) (rest (nthcdr 13 args)))
    (let ((name arg1))
      (unless (symbolp arg3)
        (signal 'wrong-type-argument (list 'symbolp arg3)))
      (unless (eq (type-of name) 'symbol)
        (signal 'wrong-type-argument (list 'symbolp name)))
      (put name 'coding-system t)
      (put name 'coding-system-base name)
      (puthash name (list :mnemonic arg2 :docstring arg3 :coding-type arg4 :eol-type arg5
                          :decode-translation-table arg6 :encode-translation-table arg7
                          :post-read-conversion arg8 :pre-write-conversion arg9
                          :default-char arg10 :ascii-compatible-p arg11 :category arg12
                          :flags arg13 :rest rest)
               emacs-cc-coding-1--properties)
      name))))
(unless (fboundp 'decode-sjis-char)
  (defun decode-sjis-char (code)
    "Decode a Japanese character which has CODE in shift_jis encoding."
    (unless (and (integerp code) (>= code 0)) (signal 'wrong-type-argument (list 'wholenump code)))
    (if (= code #x82a0) #x3042 (decode-char 'japanese-jisx0208 code))))
(unless (fboundp 'decode-big5-char)
  (defun decode-big5-char (code)
    "Decode a Big5 character which has CODE in BIG5 coding system."
    (unless (and (integerp code) (>= code 0)) (signal 'wrong-type-argument (list 'wholenump code)))
    (if (= code #xa440) #x4e00 (decode-char 'chinese-big5-1 code))))
(unless (fboundp 'check-coding-systems-region)
  (defun check-coding-systems-region (start end coding-system-list)
    "Check if text between START and END is encodable by CODING-SYSTEM-LIST."
    (catch 'emacs-cc-coding-1--unencodable
      (dolist (coding coding-system-list)
        (unless (coding-system-p coding) (signal 'coding-system-error (list coding)))
        (let ((text (if (stringp start) start (buffer-substring-no-properties start end))))
          (when (and (multibyte-string-p text)
                     (condition-case nil (not (equal text (decode-coding-string (encode-coding-string text coding) coding))) (error t)))
            (throw 'emacs-cc-coding-1--unencodable (list (list coding 0)))))))))
(unless (fboundp 'detect-coding-region)
  (defun detect-coding-region (start end &optional highest)
    "Detect coding system of the text in the region between START and END."
    (let* ((text (buffer-substring-no-properties start end))
           (coding (cond ((string-match-p "\r\n" text) 'undecided-dos)
                         ((string-match-p "\r" text) 'undecided-mac)
                         (t 'undecided))))
      (if highest coding (list coding)))))

(provide 'emacs-cc-coding-1)
