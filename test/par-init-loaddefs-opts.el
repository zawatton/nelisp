;;; -*- lexical-binding: t; -*-
;; init.el (display-time-mode 1) -> (void-variable rmail-spool-directory).  GNU loaddefs.el binds every autoloaded
;; option (a `defcustom' with :initialize, here rmail-spool-directory) at startup, rmail loaded or not; time.el
;; reads it without requiring rmail.  Boot-image autoload copy kept only `autoload' forms.
(defmacro row (n f) `(princ (format "%-34s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(row "L1 bound" (boundp 'rmail-spool-directory))
(row "L2 string" (stringp rmail-spool-directory))
(row "L3 ends with slash" (string-suffix-p "/" rmail-spool-directory))
(row "L4 savehist-mode bound" (boundp 'savehist-mode))
