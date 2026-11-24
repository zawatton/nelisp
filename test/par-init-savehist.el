;;; -*- lexical-binding: t; -*-
;; init.el (require 'savehist) -> (void-variable savehist-mode).  GNU loaddefs.el defvars every minor-mode
;; variable (`(defvar savehist-mode nil)'), and savehist's defcustom :set (savehist--manage-timer) reads
;; savehist-mode while the library loads.  nelisp-boot-image-gnu-autoloads-source copied only `autoload' forms.
(defmacro row (n f) `(princ (format "%-34s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(row "S1 var bound before load" (boundp 'savehist-mode))
(row "S2 var value before load" (and (boundp 'savehist-mode) (symbol-value 'savehist-mode)))
(row "S3 require savehist" (require 'savehist))
(row "S4 var after load" (symbol-value 'savehist-mode))
(row "S5 flymake-mode var bound" (boundp 'flymake-mode))
