;;; -*- lexical-binding: t; -*-
;; init.el (require 'eat) -> tramp -> tramp-compat: (eval (car (get 'temporary-file-directory 'standard-value)) t)
;; then (file-name-as-directory nil) -> (wrong-type-argument stringp nil).  GNU files.el defcustom gives the
;; option a standard-value.  Output avoids the host-specific path: only shapes are compared.
(defmacro row (n f) `(princ (format "%-34s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(row "D1 standard-value present" (and (get 'temporary-file-directory 'standard-value) t))
(row "D2 evaluates to a string" (stringp (eval (car (get 'temporary-file-directory 'standard-value)) t)))
(row "D3 ends with slash" (string-suffix-p "/" (eval (car (get 'temporary-file-directory 'standard-value)) t)))
(row "D4 file-name-as-directory ok" (stringp (file-name-as-directory (eval (car (get 'temporary-file-directory 'standard-value)) t))))
