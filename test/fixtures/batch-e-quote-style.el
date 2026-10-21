;;; batch-e-quote-style.el --- text-quoting-style, GNU 31.1 golden  -*- lexical-binding: t; -*-

;; doc.c text_quoting_style: with `text-quoting-style' nil the style is grave
;; unless startup--setup-quote-display found curved quotes displayable:
;; grave under LC_ALL=C, curve under C.UTF-8.  `format-message' applies it;
;; `error-message-string' substitutes the error symbol's message (print.c
;; print_error_message) but not the data of an `error' signal.

(princ (format "style=%S\n" (text-quoting-style)))
(princ (format "%s\n" (format-message "Symbol `%s' is void" 'foo)))
(princ (format "%s\n" (error-message-string '(error "Not `here' or 'there'"))))
(princ (format "%s\n" (error-message-string '(void-variable foo))))
(princ (format "%s\n" (let ((text-quoting-style 'straight)) (format-message "`a' and `b'"))))
(princ (format "%s\n" (let ((text-quoting-style 'grave)) (format-message "`a'"))))
(princ (format "%s\n" (let ((text-quoting-style 'curve)) (format-message "`a'"))))
