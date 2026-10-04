;;; standalone-native-funcall-v2-cold-before-driver.el --- Old cold proof control -*- lexical-binding: t; -*-
(require 'nelisp-native-cache)
(let ((condition (condition-case err (progn (nelisp-native-compiler-f1-runtime-proof-create) nil) (error err))))
  (unless (equal condition '(error "Unauthenticated environment allocation domain"))
    (error "Cold allocation negative control differed: %S" condition))
  (princ "F1B-COLD-BEFORE-REFUSAL=Unauthenticated environment allocation domain\n"))
