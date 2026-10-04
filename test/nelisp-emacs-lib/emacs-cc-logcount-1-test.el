;;; emacs-cc-logcount-1-test.el --- logcount C-core coverage -*- lexical-binding: t; -*-

(require 'ert)

(defconst emacs-cc-logcount-1-test--root
  (expand-file-name "../.."
                    (file-name-directory (or load-file-name buffer-file-name))))

(add-to-list 'load-path
             (expand-file-name "packages/nelisp-emacs-foundation/src"
                               emacs-cc-logcount-1-test--root))
(load "emacs-cc-logcount-1" nil t)

(ert-deftest emacs-cc-logcount-1/finite-and-complement-semantics ()
  (should (equal '(0 1 4 64)
                 (mapcar #'logcount '(0 1 15 18446744073709551615))))
  (should (equal '(0 1 1 2)
                 (mapcar #'logcount '(-1 -2 -3 -4)))))

(ert-deftest emacs-cc-logcount-1/arbitrary-precision-integers ()
  (should (equal '(1 1024 129 128)
                 (list (logcount (ash 1 1024))
                       (logcount (- (ash 1 1024)))
                       (logcount (1- (ash 1 129)))
                       (logcount (- (1- (ash 1 129))))))))

(ert-deftest emacs-cc-logcount-1/type-and-arity-errors ()
  (should (equal '(wrong-type-argument (integerp nil))
                 (condition-case error-data (logcount nil)
                   (error (list (car error-data) (cdr error-data))))))
  (should (equal '(wrong-type-argument (integerp 1.5))
                 (condition-case error-data (logcount 1.5)
                   (error (list (car error-data) (cdr error-data))))))
  (should (equal '(wrong-number-of-arguments (logcount 0))
                 (condition-case error-data (logcount)
                   (error (list (car error-data) (cdr error-data))))))
  (should (equal '(wrong-number-of-arguments (logcount 2))
                 (condition-case error-data (logcount 1 2)
                   (error (list (car error-data) (cdr error-data)))))))

(provide 'emacs-cc-logcount-1-test)
;;; emacs-cc-logcount-1-test.el ends here
