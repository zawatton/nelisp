;;; nelisp-native-relocation-dictionary-test.el --- Relocation codec tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-native-rooted-build-evidence)
(require 'nelisp-native-rooted-startup-evidence)

(ert-deftest nelisp-native-relocation-dictionary-roundtrip ()
  (let* ((input (list (list :offset 1 :symbol "same" :type 'pc32)
                      (list :offset 2 :symbol "same" :type 'plt32)
                      (list :offset 3 :symbol "other" :type 'pc32)))
         (snapshot (copy-tree input))
         (encoded (nelisp-native-rooted-build-evidence--relocations input))
         (unit `((relocation-symbol-format . "dictionary-v1")
                 (relocation-symbols . ,(append (car encoded) nil)))))
    (should (equal input snapshot))
    (should (equal (append (car encoded) nil) '("same" "other")))
    (should (equal (mapcar (lambda (record)
                             (nelisp-native-rooted-startup-evidence--relocation-symbol
                              unit `((symbol . ,(plist-get record :symbol)))))
                           (append (cdr encoded) nil))
                   '("same" "same" "other")))
    (should (equal (nelisp-native-rooted-startup-evidence--relocation-symbol
                    nil '((symbol . "legacy"))) "legacy"))
    (dolist (value '(-1 2 "same"))
      (should-error (nelisp-native-rooted-startup-evidence--relocation-symbol
                     unit `((symbol . ,value))))))
  (let ((empty (nelisp-native-rooted-build-evidence--relocations nil)))
    (should (equal (append (car empty) nil) nil))
    (should (equal (append (cdr empty) nil) nil)))
  (should-error (nelisp-native-rooted-startup-evidence--relocation-symbol
                 '((relocation-symbol-format . "dictionary-v1")) '((symbol . 0))))
  (should-error (nelisp-native-rooted-startup-evidence--relocation-symbol
                 '((relocation-symbol-format . "dictionary-v1") (relocation-symbols)) '((symbol . 0)))))

(provide 'nelisp-native-relocation-dictionary-test)
