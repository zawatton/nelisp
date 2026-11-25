;;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defmacro par-tab-line-row (label form)
  `(princ (format "%s %S\n" ,label
                  (condition-case err ,form (error (car err))))))

(par-tab-line-row "cl-every string" (cl-every #'integerp " •"))
(par-tab-line-row "cl-some first value"
  (cl-some (lambda (ch) (and (> ch 127) ch)) " •×"))
(par-tab-line-row "cl-every mixed sequences"
  (cl-every #'= "ab" [97 98 99] '(97 98)))
(par-tab-line-row "cl-notany mixed sequences" (cl-notany #'= "a" [98]))
(par-tab-line-row "cl-notevery bool vector"
  (cl-notevery #'identity (bool-vector t nil)))
(par-tab-line-row "tab-line mode"
  (progn
    (require 'tab-line)
    (with-temp-buffer
      (tab-line-mode 1)
      (let ((enabled tab-line-mode)
            (format tab-line-format))
        (tab-line-mode -1)
        (list enabled format tab-line-mode tab-line-format)))))
