;;; nelisp-eval-function-store-api-test.el --- public function-cell API -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'nelisp-eval)

(ert-deftest nelisp-eval-function-store-api/read-and-presence-distinguish-nil ()
  (let* ((symbol (make-symbol "function-store-api-nil"))
         (absent (make-symbol "function-store-api-absent"))
         (saved (nelisp-eval-function-cell-snapshot (list symbol))))
    (unwind-protect
        (progn
          (should (nelisp-eval-function-store-ready-p))
          (should-not (nelisp-eval-function-cell-present-p symbol))
          (should (eq absent (nelisp-eval-function-cell-ref symbol absent)))
          (should (eq nil (nelisp-eval-function-cell-put symbol nil)))
          (should (nelisp-eval-function-cell-present-p symbol))
          (should (eq nil (nelisp-eval-function-cell-ref symbol absent)))
          (should (equal (nelisp-eval-function-cell-snapshot (list symbol))
                         (list (list symbol t nil))))
          (should (assq symbol (nelisp-eval-function-cell-snapshot-all))))
      (nelisp-eval-function-cell-restore saved))))

(ert-deftest nelisp-eval-function-store-api/restore-preserves-presence ()
  (let* ((present (make-symbol "function-store-api-present"))
         (missing (make-symbol "function-store-api-missing"))
         (value (list 'original))
         (original (nelisp-eval-function-cell-snapshot (list present missing)))
         saved
         (absent (make-symbol "function-store-api-absent")))
    (unwind-protect
        (progn
          (nelisp-eval-function-cell-put present value)
          (setq saved
                (nelisp-eval-function-cell-snapshot (list present missing)))
          (should-not (nelisp-eval-function-cell-delete present))
          (should-not (nelisp-eval-function-cell-present-p present))
          (should-not (nelisp-eval-function-cell-delete missing))
          (nelisp-eval-function-cell-put missing 'temporary)
          (nelisp-eval-function-cell-restore saved)
          (should (nelisp-eval-function-cell-present-p present))
          (should (eq value
                      (nelisp-eval-function-cell-ref present absent)))
          (should-not (nelisp-eval-function-cell-present-p missing))
          (should (eq absent (nelisp-eval-function-cell-ref missing absent))))
      (nelisp-eval-function-cell-restore original))))

(ert-deftest nelisp-eval-function-store-api/defalias-validates-and-installs ()
  (let* ((symbol (make-symbol "function-store-api-defalias"))
         (function (lambda () 'ok))
         (saved (nelisp-eval-function-cell-snapshot (list symbol)))
         (absent (make-symbol "function-store-api-absent")))
    (unwind-protect
        (progn
          (should (eq symbol (nelisp-eval-function-defalias symbol function)))
          (should (eq function
                      (nelisp-eval-function-cell-ref symbol absent)))
          (should-error (nelisp-eval-function-defalias 7 function)
                        :type 'wrong-type-argument))
      (nelisp-eval-function-cell-restore saved))))

(ert-deftest nelisp-eval-function-store-api/invalid-restore-is-checked-before-write ()
  (let* ((symbol (make-symbol "function-store-api-rollback"))
         (value 'stable)
         (saved (nelisp-eval-function-cell-snapshot (list symbol))))
    (unwind-protect
        (progn
          (nelisp-eval-function-cell-put symbol value)
          (should-error
           (nelisp-eval-function-cell-restore
            (list (list symbol t value) (list 9 t 'bad)))
           :type 'wrong-type-argument)
          (should (eq value
                      (nelisp-eval-function-cell-ref symbol :absent))))
      (nelisp-eval-function-cell-restore saved))))

(provide 'nelisp-eval-function-store-api-test)
;;; nelisp-eval-function-store-api-test.el ends here
