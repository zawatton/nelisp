(check-coding-systems-region
 (check-coding-systems-region "hello" nil '(utf-8))
 (check-coding-systems-region "é" nil '(ascii)))
(coding-system-aliases
 (coding-system-aliases 'utf-8)
 (condition-case e (coding-system-aliases 'coding-1-no-such-system) (error e)))
(coding-system-base
 (coding-system-base 'utf-8-dos)
 (condition-case e (coding-system-base 'coding-1-no-such-system) (error e)))
(coding-system-eol-type
 (coding-system-eol-type 'utf-8-dos)
 (coding-system-eol-type 'utf-8-mac))
(coding-system-plist
 (coding-system-plist 'utf-8)
 (condition-case e (coding-system-plist 'coding-1-no-such-system) (error e)))
(coding-system-priority-list
 (length (coding-system-priority-list))
 (coding-system-priority-list t))
(coding-system-put
 (let ((cs (coding-system-plist 'utf-8))) (coding-system-put 'utf-8 :coding-1-probe t) (eq (coding-system-get 'utf-8 :coding-1-probe) t))
 (condition-case e (coding-system-put 'coding-1-no-such-system :coding-1-probe t) (error e)))
(decode-big5-char
 (decode-big5-char #xa440)
 (condition-case e (decode-big5-char 'bad) (error e)))
(decode-sjis-char
 (decode-sjis-char #x82a0)
 (condition-case e (decode-sjis-char 'bad) (error e)))
(define-coding-system-alias
 (progn (define-coding-system-alias 'coding-1-utf8-alias 'utf-8) (coding-system-base 'coding-1-utf8-alias))
 (condition-case e (define-coding-system-alias 'coding-1-bad-alias 'coding-1-no-such-system) (error e)))
(define-coding-system-internal
 (condition-case e (define-coding-system-internal) (error e))
 (condition-case e (define-coding-system-internal 'coding-1-probe 65 "probe" 'utf-8 0 nil nil nil nil nil t nil nil) (error e)))
(detect-coding-region
 (with-temp-buffer (insert "plain") (detect-coding-region (point-min) (point-max)))
 (with-temp-buffer (insert "line1\r\nline2") (detect-coding-region (point-min) (point-max) t)))
