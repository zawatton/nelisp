;;; -*- lexical-binding: t; -*-
;; init.el (require 'treemacs) -> (error "Invalid image type `png'").  GNU image.c defines the variable
;; `image-types'; image--check-type signals unless the type is a member.  Spec = par-init-image-types.gnu.
(defmacro row (n f) `(princ (format "%-34s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(row "I1 image-types bound" (boundp 'image-types))
(row "I2 special variable" (special-variable-p 'image-types))
(row "I3 png member" (and (memq 'png image-types) t))
(row "I4 jpeg member" (and (memq 'jpeg image-types) t))
(row "I5 create-image png" (car (create-image "/nonexistent/x.png" 'png nil :ascent 'center)))
(row "I6 create-image png :type" (plist-get (cdr (create-image "/nonexistent/x.png" 'png nil :ascent 'center)) :type))
(row "I7 bogus type signals" (car (create-image "/nonexistent/x.foo" 'foo-type nil)))
