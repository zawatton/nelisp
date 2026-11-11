;;; nelisp-cc-fxir-rows-test.el --- shared FXIR row registry tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-test-fresh-emacs)
(require 'nelisp-cc-fxir-rows)

(ert-deftest nelisp-cc-fxir-rows-baseline-and-mutant-are-stable ()
  (nelisp-test-fresh-emacs-unless (not (featurep 'nelisp-cc-fxir-native))
    (let* ((rows (nelisp-cc-fxir-native-all-rows))
           (print-length nil) (print-level nil) (print-circle nil)
           (print-quoted t) (print-gensym t) (print-escape-nonascii t))
      (should (= (length rows) 467))
      (should (eq (nth 6 (car (last rows))) :mutant))
      (should (= (cadr (car (last rows)))
                 (1+ (apply #'max (mapcar #'cadr (butlast rows))))))
      (should (equal (secure-hash 'sha256 (prin1-to-string rows))
                     "acc00ace89e2346c4381f825762bc452a9564318e6beb698a3e67daaa1853902")))
    (should-not (fboundp 'nelisp-cc-fxir-native-source))))

(ert-deftest nelisp-cc-fxir-rows-win64-retains-word-vm-path ()
  (let ((nelisp-cc-fxir-native-windows t))
    (should-not (nelisp-cc-fxir-native--rows))
    (should-not (nelisp-cc-fxir-native-all-rows))))

(ert-deftest nelisp-cc-fxir-rows-require-validator-for-additional-snapshot ()
  (let ((nelisp-cc-fxir-native--additional-rows 'snapshot)
        (validator-was-called nil)
        (old (and (fboundp 'nelisp-cc-fxir-admission-rows)
                  (symbol-function 'nelisp-cc-fxir-admission-rows))))
    (unwind-protect
        (progn
          (fmakunbound 'nelisp-cc-fxir-admission-rows)
          (should-error (nelisp-cc-fxir-native--rows) :type 'error)
          (fset 'nelisp-cc-fxir-admission-rows
                (lambda (snapshot)
                  (setq validator-was-called snapshot)
                  '((extra 900 0 nil (:const 1) 0))))
          (should (= (length (nelisp-cc-fxir-native--rows)) 467))
          (should (eq validator-was-called 'snapshot)))
      (if old
          (fset 'nelisp-cc-fxir-admission-rows old)
        (fmakunbound 'nelisp-cc-fxir-admission-rows)))))

(provide 'nelisp-cc-fxir-rows-test)

;;; nelisp-cc-fxir-rows-test.el ends here
