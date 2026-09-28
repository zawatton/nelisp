;;; standalone-byte-get-parity.el --- Host/standalone byte-get parity -*- lexical-binding: t; -*-

(defun nelisp-native-byte-get-parity-run ()
  (let* ((symbol 'nelisp-native-byte-get-parity-target)
       (code (unibyte-string 192 193 78 135))
       (constants (vector symbol 'nelisp-native-byte-get-parity-property))
       (read-property
        (lambda ()
          (byte-code code constants 2)))
       (invalid
        (condition-case err
            (byte-code code (vector 1 'nelisp-native-byte-get-parity-property) 2)
          (error (car err))))
       (value-31 nil)
       (value-nil nil)
       (value-missing nil)
       (value-after-gc nil)
       (value-shadow nil))
  (when (fboundp 'byte-compile)
    (let* ((compiled (byte-compile `(lambda () (get ',symbol
                                                   'nelisp-native-byte-get-parity-property))))
           (compiled-code (aref compiled 1)))
      (unless (= (aref compiled-code 2) 78)
        (error "Host compiler did not emit byte-get (78): %S" compiled-code))
      (setq read-property compiled)))
  (put symbol 'nelisp-native-byte-get-parity-property 31)
  (setq value-31 (funcall read-property))
  (put symbol 'nelisp-native-byte-get-parity-property nil)
  (setq value-nil (funcall read-property))
  (setplist symbol nil)
  (setq value-missing (funcall read-property))
  (put symbol 'nelisp-native-byte-get-parity-property 31)
  (garbage-collect)
  (setq value-after-gc (funcall read-property))
  (put symbol 'nelisp-native-byte-get-parity-property nil)
  (let ((old-get (symbol-function 'get)))
    (setq value-shadow
          (unwind-protect
              (progn
                (fset 'get (lambda (&rest _) 99))
                (list (funcall read-property)
                      (get symbol 'nelisp-native-byte-get-parity-property)))
            (fset 'get old-get))))
  (list value-31 value-nil value-missing invalid value-after-gc value-shadow)))

(provide 'standalone-byte-get-parity)

;;; standalone-byte-get-parity.el ends here
