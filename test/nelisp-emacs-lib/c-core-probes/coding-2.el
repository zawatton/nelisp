(detect-coding-string
 (equal (detect-coding-string "plain ASCII") '(undecided))
 (detect-coding-string (concat "line one" (string 13 10) "line two") t)
 (condition-case e (detect-coding-string nil) (error (list (car e) (cdr e)))))
(encode-big5-char (encode-big5-char ?A)
                  (encode-big5-char ?中)
                  (condition-case e (encode-big5-char nil) (error (list (car e) (cdr e)))))
(encode-sjis-char (encode-sjis-char ?A)
                  (encode-sjis-char ?あ)
                  (condition-case e (encode-sjis-char nil) (error (list (car e) (cdr e)))))
(find-coding-systems-region-internal
 (with-temp-buffer (insert "ASCII") (eq (find-coding-systems-region-internal (point-min) (point-max)) t))
 (with-temp-buffer (insert "あ") (consp (find-coding-systems-region-internal (point-min) (point-max) '(utf-8))))
 (condition-case e (find-coding-systems-region-internal 1 2) (error (list (car e) (cdr e)))))
(find-operation-coding-system
 (equal (find-operation-coding-system 'insert-file-contents "file.txt") '(undecided))
 (let ((file-coding-system-alist '(("\\.probe\\'" . (utf-8-unix . iso-8859-1)))))
   (equal (find-operation-coding-system 'insert-file-contents "x.probe") '(utf-8-unix . iso-8859-1)))
 (condition-case e (find-operation-coding-system 'unknown-operation "x") (error (list (car e) (cdr e)))))
(read-coding-system
 (and (fboundp 'read-coding-system) (symbolp 'utf-8-unix))
 (and (fboundp 'read-coding-system) (symbolp 'iso-8859-1)))
(read-non-nil-coding-system
 (and (fboundp 'read-non-nil-coding-system) (stringp "Coding: "))
 (and (fboundp 'read-non-nil-coding-system) (stringp "Required coding: ")))
(set-coding-system-priority
 (progn (set-coding-system-priority 'utf-8) nil)
 (condition-case e (set-coding-system-priority 'not-a-coding-system) (error (list (car e) (cdr e)))))
(set-keyboard-coding-system-internal
 (condition-case e (progn (set-keyboard-coding-system-internal 'utf-8-unix) t) (error (list (car e) (cdr e))))
 (condition-case e (set-keyboard-coding-system-internal 'not-a-coding-system) (error (list (car e) (cdr e)))))
(set-safe-terminal-coding-system-internal
 (condition-case e (progn (set-safe-terminal-coding-system-internal 'utf-8-unix) t) (error (list (car e) (cdr e))))
 (condition-case e (set-safe-terminal-coding-system-internal 'not-a-coding-system) (error (list (car e) (cdr e)))))
(set-terminal-coding-system-internal
 (condition-case e (progn (set-terminal-coding-system-internal 'utf-8-unix) t) (error (list (car e) (cdr e))))
 (condition-case e (set-terminal-coding-system-internal 'not-a-coding-system) (error (list (car e) (cdr e)))))
(terminal-coding-system
 (terminal-coding-system)
 (and (windowp (selected-window))
      (terminal-coding-system (window-frame (selected-window)))))
