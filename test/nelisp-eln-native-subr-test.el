;;; nelisp-eln-native-subr-test.el --- Managed unary factory checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'nelisp-eln-native-subr)

(defun nelisp-eln-native-subr-test--create
    (bytes &optional reported-size handle)
  "Call the factory with BYTES, optional REPORTED-SIZE, and HANDLE."
  (let ((captured nil)
        (size (or reported-size (length bytes))))
    (cl-letf (((symbol-function 'nelisp-eln-system-loader-function-capability)
               (lambda (_handle _name)
                 (list nil nil nil 123 nil nil size)))
              ((symbol-function 'nelisp-eln-system-loader-read-root-function-bytes)
               (lambda (_handle _name _offset _size) bytes))
              ((symbol-function 'nelisp-eln-system-loader-module-id)
               (lambda (_handle) 'mock-module))
              ((symbol-function 'nelisp-eln-system-loader-validate-function-capability)
               (lambda (_capability) t))
              ((symbol-function 'nelisp--native-subr-create)
               (lambda (&rest args) (setq captured args) 'mock-subr)))
      (let ((result (nelisp-eln-native-subr-create
                     handle "leaf" "leaf-fn")))
        (list result captured)))))

(ert-deftest nelisp-eln-native-subr-admits-verified-unary-through-bridge ()
  (let* ((bytes (unibyte-string
                 72 137 248 72 133 192 15 132 7 0 0 0
                 49 192 233 10 0 0 0
                 72 184 2 0 0 0 0 0 0 0 195))
         (result (nelisp-eln-native-subr-test--create bytes))
         (args (cadr result)))
    (should (eq (car result) 'mock-subr))
    (should (= (length args) 5))
    (should (functionp (nth 3 args)))
    (should (= (nth 4 args) 1))))

(ert-deftest nelisp-eln-native-subr-preserves-scalar0-fast-path ()
  (let* ((result (nelisp-eln-native-subr-test--create
                  (unibyte-string 184 6 0 0 0 195)))
         (args (cadr result)))
    (should (eq (car result) 'mock-subr))
    (should (= (length args) 3))))

(ert-deftest nelisp-eln-native-subr-rejects-short-read-and-unsafe-code ()
  (should-error (nelisp-eln-native-subr-test--create
                 (unibyte-string 184 6 0 0 0 195) 7 nil)
                :type 'nelisp-eln-native-subr-error)
  (should-error (nelisp-eln-native-subr-test--create
                 (concat (unibyte-string 72 184)
                         (apply #'unibyte-string
                                '(0 16 0 0 0 0 0 0))
                         (unibyte-string 195)) nil nil)
                :type 'nelisp-eln-native-subr-error)
  (should-error (nelisp-eln-native-subr-test--create
                 (unibyte-string 15 5 195) nil nil)
                :type 'nelisp-eln-native-subr-error))

(provide 'nelisp-eln-native-subr-test)

;;; nelisp-eln-native-subr-test.el ends here
