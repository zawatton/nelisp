;;; nelisp-runtime-reload-abi-test.el --- ABI digest regression -*- lexical-binding: t; -*-
(require 'ert)
(load (expand-file-name "nelisp-runtime-reload-abi-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)
(ert-deftest nelisp-runtime-reload-abi/cache-security-and-printer-semantics ()
  (let ((old-cache nelisp-runtime-reload--digest-cache)
        (old-hash (symbol-function 'secure-hash))
        (old-printer print-circle))
    (should (nelisp-runtime-reload-abi-regression-run))
    (should (eq old-cache nelisp-runtime-reload--digest-cache))
    (should (eq old-hash (symbol-function 'secure-hash)))
    (should (eq old-printer print-circle))))
