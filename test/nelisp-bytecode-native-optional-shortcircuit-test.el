;;; nelisp-bytecode-native-optional-shortcircuit-test.el --- S3.4c compiler checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-bytecode-native-compiler)

(ert-deftest nelisp-bytecode-native-optional-shortcircuit/exact-or-and-only ()
  (skip-unless (equal emacs-version "31.1"))
  (let* ((base (make-temp-name
                (expand-file-name "nelisp-optional-shortcircuit-"
                                  temporary-file-directory)))
         (or-path (concat base ".or"))
         (and-path (concat base ".and"))
         (wrong-path (concat base ".wrong"))
         (altered-path (concat base ".altered"))
         (effect-path (concat base ".effect"))
         (or-code (unibyte-string 137 134 5 0 1 135))
         (and-code (unibyte-string 137 133 5 0 1 135))
         (or-result (nelisp-bytecode-native-compiler-build
                     (make-byte-code 513 or-code [] 3) or-path "nl_short_or_ert"))
         (and-result (nelisp-bytecode-native-compiler-build
                      (make-byte-code 513 and-code [] 3) and-path "nl_short_and_ert"))
         (wrong (nelisp-bytecode-native-compiler-build
                 (make-byte-code 514 and-code [] 3) wrong-path "nl_short_wrong_ert"))
         (altered (nelisp-bytecode-native-compiler-build
                   (make-byte-code 513 (unibyte-string 137 133 4 0 1 135) [] 3)
                   altered-path "nl_short_altered_ert"))
         (effectful (nelisp-bytecode-native-compiler-build
                     (make-byte-code 513 (unibyte-string 137 33 135) [] 3)
                     effect-path "nl_short_effect_ert")))
    (unwind-protect
        (progn
          (should (eq (plist-get or-result :status) 'complete))
          (should (eq (plist-get and-result :status) 'complete))
          (should (file-readable-p or-path))
          (should (file-readable-p and-path))
          (should (eq (plist-get wrong :status) 'unsupported))
          (should (memq (plist-get altered :status) '(unsupported malformed)))
          (should (eq (plist-get effectful :status) 'unsupported))
          (should-not (file-exists-p wrong-path))
          (should-not (file-exists-p altered-path))
          (should-not (file-exists-p effect-path)))
      (dolist (path (list or-path and-path wrong-path altered-path effect-path))
        (when (file-exists-p path) (delete-file path))))))

(provide 'nelisp-bytecode-native-optional-shortcircuit-test)
;;; nelisp-bytecode-native-optional-shortcircuit-test.el ends here
