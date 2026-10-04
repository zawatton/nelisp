;;; rooted-conditional-public.el --- GNU conditional public-route fixture -*- lexical-binding: t; -*-
(defun nelisp-native-rooted-conditional-public-fixture (condition when-true when-false)
  (if condition when-true when-false))
(provide 'rooted-conditional-public)
