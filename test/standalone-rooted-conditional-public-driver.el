;;; standalone-rooted-conditional-public-driver.el --- public route smoke -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-rooted-conditional-call)
(require 'nelisp-bytecode-native-package)

(let* ((elc (getenv "NELISP_CONDITIONAL_ELC"))
       (artifact (getenv "NELISP_CONDITIONAL_ARTIFACT")))
  (unless (and elc artifact (file-readable-p elc))
    (error "conditional public smoke fixture/artifact path missing"))
  (let* ((definitions (nelisp-bytecode-native-package-read-elc-functions elc))
         (data (cdr (assq 'nelisp-native-rooted-conditional-public-fixture definitions)))
         (function (and data (make-byte-code (aref data 0) (aref data 1)
                                             (aref data 2) (aref data 3))))
         (result (nelisp-bytecode-native-compiler-build
                  function artifact "nl_native_rooted_conditional_probe_v1")))
    (unless (and (eq (plist-get result :status) 'complete)
                 (nelisp-bytecode-native-rooted-conditional-authenticated-result-p result))
      (error "public conditional compiler route refused: %S" result))
    (let ((yes (list 'yes)) (no (vector 'no)))
      (unless (and (eq (funcall function nil yes no) no)
                   (eq (nelisp-bytecode-native-rooted-conditional-call result nil yes no) no)
                   (eq (funcall function t yes no) yes)
                   (eq (nelisp-bytecode-native-rooted-conditional-call result t yes no) yes))
        (error "public conditional VM/native arm or identity mismatch")))
    (princ "rooted-conditional-public: PASS (public compiler, both arms, identity)\n")))

;;; standalone-rooted-conditional-public-driver.el ends here
