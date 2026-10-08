;;; nelisp-bytecode-native-optional-default-test.el --- S3.4b compiler checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-compiler)

(ert-deftest nelisp-bytecode-native-optional-default/exact-shape-only ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((path (make-temp-name
                (expand-file-name "nelisp-optional-default-"
                                  temporary-file-directory)))
         (bad-path (concat path ".bad"))
         (code (unibyte-string 137 134 5 0 1 135))
         (good (nelisp-bytecode-native-compiler-build
                (make-byte-code 513 code [] 3) path "nl_optional_default_ert"))
         (wrong (nelisp-bytecode-native-compiler-build
                 (make-byte-code 514 code [] 3) bad-path "nl_wrong_optional_ert")))
    (unwind-protect
        (progn
          (should (eq (plist-get good :status) 'complete))
          (should (file-readable-p path))
          (should (eq (plist-get wrong :status) 'unsupported))
          (should-not (file-exists-p bad-path)))
      (when (file-exists-p path) (delete-file path))
      (when (file-exists-p bad-path) (delete-file bad-path)))))

(provide 'nelisp-bytecode-native-optional-default-test)
;;; nelisp-bytecode-native-optional-default-test.el ends here
