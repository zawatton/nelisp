;;; census-chars-01.el --- canonical probes  -*- lexical-binding: t; -*-
(base64-decode-string
 (base64-decode-string "SGVsbG8=")
 (base64-decode-string "")
 (condition-case e (base64-decode-string "!!!!") (error e)))
(base64-encode-string
 (base64-encode-string "Hello" t)
 (base64-encode-string "" t)
 (condition-case e (base64-encode-string 42) (error e)))
(char-equal
 (with-temp-buffer (let ((case-fold-search nil)) (char-equal ?a ?a)))
 (with-temp-buffer (let ((case-fold-search t)) (char-equal ?a ?A)))
 (with-temp-buffer (let ((case-fold-search nil)) (char-equal ?a ?A)))
 (condition-case e (char-equal 'a ?a) (error e)))
(char-or-string-p
 (char-or-string-p ?A)
 (list (char-or-string-p "") (char-or-string-p -1) (char-or-string-p nil)))
(char-table-extra-slot
 (let ((table (make-char-table 'case-table nil)))
   (set-char-table-extra-slot table 0 'extra)
   (char-table-extra-slot table 0))
 (char-table-extra-slot (make-char-table 'case-table nil) 2)
 (condition-case e (char-table-extra-slot 42 0) (error e)))
(char-table-parent
 (char-table-parent (make-char-table 'syntax-table nil))
 (let ((table (make-char-table 'syntax-table nil))
       (parent (make-char-table 'syntax-table nil)))
   (set-char-table-parent table parent)
   (eq (char-table-parent table) parent))
 (condition-case e (char-table-parent 42) (error e)))
(char-table-range
 (let ((table (make-char-table 'syntax-table 'default)))
   (set-char-table-range table '(65 . 67) 'letters)
   (char-table-range table 66))
 (let ((table (make-char-table 'syntax-table 'default)))
   (set-char-table-range table '(65 . 67) 'letters)
   (list (char-table-range table nil) (char-table-range table '(65 . 70))))
 (condition-case e (char-table-range 42 65) (error e)))
(char-table-subtype
 (char-table-subtype (make-char-table 'syntax-table nil))
 (char-table-subtype (make-char-table 'case-table nil))
 (condition-case e (char-table-subtype []) (error e)))
(char-to-string
 (char-to-string ?A)
 (append (char-to-string 0) nil)
 (condition-case e (char-to-string 'a) (error e)))
(char-width
 (char-width ?A)
 (list (char-width #x4e2d) (char-width #x301))
 (condition-case e (char-width 'a) (error e)))
(characterp
 (characterp ?A)
 (list (characterp 0) (characterp -1) (characterp #x400000))
 (characterp "A"))
(charsetp
 (charsetp 'ascii)
 (charsetp 'unicode)
 (charsetp 'canonical-probe-no-such-charset))
(check-coding-system
 (check-coding-system 'utf-8-unix)
 (check-coding-system nil)
 (condition-case e (check-coding-system 'canonical-probe-no-such-coding) (error e)))
(clear-string
 (let ((s (copy-sequence "abc"))) (list (clear-string s) (append s nil)))
 (let ((s (copy-sequence "é"))) (clear-string s) (list (length s) (multibyte-string-p s) (append s nil)))
 (condition-case e (clear-string 42) (error e)))
(coding-system-p
 (coding-system-p 'utf-8-unix)
 (coding-system-p nil)
 (coding-system-p 'canonical-probe-no-such-coding))
(compare-strings
 (compare-strings "abc" nil nil "abc" nil nil)
 (list (compare-strings "abc" nil nil "abd" nil nil)
       (compare-strings "abd" nil nil "abc" nil nil))
 (compare-strings "xAbCz" 1 4 "abc" nil nil t)
 (condition-case e (compare-strings 42 nil nil "a" nil nil) (error e)))
(decode-char
 (decode-char 'ascii 65)
 (decode-char 'ascii 128)
 (condition-case e (decode-char 'canonical-probe-no-such-charset 65) (error e)))
(decode-coding-region
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (set-buffer-multibyte nil) (insert (unibyte-string 195 169))
     (decode-coding-region (point-min) (point-max) 'utf-8-unix t)))
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (insert "abc")
     (list (decode-coding-region (point-min) (point-max) 'us-ascii)
           (buffer-string))))
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (condition-case e (decode-coding-region 1 1 'canonical-probe-no-such-coding) (error e)))))
(decode-coding-string
 (let ((last-coding-system-used nil))
   (decode-coding-string (unibyte-string 195 169) 'utf-8-unix))
 (let ((last-coding-system-used nil)) (decode-coding-string "" 'utf-8-unix))
 (let ((last-coding-system-used nil))
   (condition-case e (decode-coding-string "abc" 'canonical-probe-no-such-coding) (error e))))
(encode-coding-region
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (insert "é")
     (append (encode-coding-region (point-min) (point-max) 'utf-8-unix t) nil)))
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (insert "abc")
     (list (encode-coding-region (point-min) (point-max) 'us-ascii)
           (buffer-string))))
 (let ((last-coding-system-used nil))
   (with-temp-buffer
     (condition-case e (encode-coding-region 1 1 'canonical-probe-no-such-coding) (error e)))))
(encode-coding-string
 (let ((last-coding-system-used nil))
   (append (encode-coding-string "é" 'utf-8-unix) nil))
 (let ((last-coding-system-used nil)) (encode-coding-string "" 'utf-8-unix))
 (let ((last-coding-system-used nil))
   (condition-case e (encode-coding-string "abc" 'canonical-probe-no-such-coding) (error e))))
(format-message
 (let ((text-quoting-style 'straight)) (format-message "Value: %s/%d" "abc" 7))
 (let ((text-quoting-style 'straight)) (format-message "`%s' %%" "word"))
 (condition-case e (format-message "%d" "abc") (error e)))
(keyboard-coding-system
 (coding-system-p (keyboard-coding-system))
 (eq (keyboard-coding-system) (keyboard-coding-system nil))
 (condition-case e (keyboard-coding-system 'bogus) (error e)))
(make-char
 (make-char 'ascii 65)
 (make-char 'ascii)
 (condition-case e (make-char 'canonical-probe-no-such-charset 65) (error e)))
