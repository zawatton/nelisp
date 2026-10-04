;;; frame-ir-catch-throw.el --- GNU byte-code frame fixture -*- lexical-binding: t; -*-

(defvar nelisp-bytecode-frame-ir-handler-special nil)

(defun nelisp-bytecode-frame-ir-catch-throw (tag value)
  (catch tag
    (let ((nelisp-bytecode-frame-ir-handler-special value))
      (throw tag nelisp-bytecode-frame-ir-handler-special))))

(provide 'nelisp-bytecode-frame-ir-catch-throw)
;;; frame-ir-catch-throw.el ends here
