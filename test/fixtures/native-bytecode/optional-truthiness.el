;;; optional-truthiness.el --- GNU 31.1 optional branch bytecode fixture

(defun nelisp-native-optional-or (value &optional supplied)
  (or supplied value))

(defun nelisp-native-optional-and (value &optional supplied)
  (and supplied value))

(provide 'optional-truthiness)
;;; optional-truthiness.el ends here
