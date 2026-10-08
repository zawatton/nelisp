;;; nelisp-bytecode-native-optional-arg-test.el --- optional argument compiler test -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-compiler)

(ert-deftest nelisp-bytecode-native-optional-arg/packed-descriptor-only ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((packed (make-byte-code 513 (unibyte-string 135) [] 3))
         (list-descriptor (make-byte-code '(x &optional y)
                                          (unibyte-string 135) [] 3))
         (packed-path
          (make-temp-name (expand-file-name "nelisp-packed-optional-"
                                            temporary-file-directory)))
         (list-path
          (make-temp-name (expand-file-name "nelisp-list-optional-"
                                            temporary-file-directory)))
         (packed-result nil) (list-result nil))
    (unwind-protect
        (progn
          (setq packed-result
                (nelisp-bytecode-native-compiler-build
                 packed packed-path "nl_optional_return"))
          (setq list-result
                (nelisp-bytecode-native-compiler-build
                 list-descriptor list-path "nl_list_optional"))
          (should (eq (plist-get packed-result :status) 'complete))
          (should (file-exists-p packed-path))
          (should (null (funcall packed 'required)))
          (should-not (eq (plist-get list-result :status) 'complete))
          (should-not (file-exists-p list-path)))
      (when (file-exists-p packed-path) (delete-file packed-path))
      (when (file-exists-p list-path) (delete-file list-path)))))

(provide 'nelisp-bytecode-native-optional-arg-test)
;;; nelisp-bytecode-native-optional-arg-test.el ends here
