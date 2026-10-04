;;; nelisp-bytecode-native-rooted-cfg-contract-plan-data-test.el --- Serialization -*- lexical-binding: t; -*-
(require 'ert)
(require 'bytecomp)
(require 'nelisp-bytecode-native-consumer)
(require 'nelisp-bytecode-native-rooted-cfg-contract)

(defun nelisp-bytecode-native-rooted-cfg-contract-plan-data-test--reference (plan)
  "Retain the original GNU cl-loop serialization as an independent oracle."
  (let ((copy (copy-sequence plan)))
    (setq copy (plist-put copy :input nil))
    (when (plist-get copy :exit-root-base)
      (setq copy (plist-put copy :arithmetic-context nil)))
    (cl-labels ((data-plist (value)
                  (cl-loop for (key item) on value by #'cddr
                           unless (or (eq key :arithmetic-guard-context)
                                      (and (eq key :arithmetic-guard-mode) (eq item 'off)))
                           append (list key item))))
      (setq copy (data-plist copy))
      (setq copy (plist-put copy :entry-ast (data-plist (plist-get copy :entry-ast)))))
    (copy-tree copy)))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract-plan-data/genuine-gnu-off-on ()
  (let* ((directory (make-temp-file "nelisp-plan-data-" t))
         (source (expand-file-name "fixture.el" directory))
         (elc (concat source "c")))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(defun nelisp-plan-data-test-fixture (left right) (cons left right))\n"))
          (should (byte-compile-file source))
          (delete-file source)
          (let* ((function (cdr (assq 'nelisp-plan-data-test-fixture
                                     (nelisp-bytecode-native-consumer-read-elc-functions elc))))
                 (input (nelisp-bytecode-compiler-input-build function)))
            (should-not (file-exists-p source))
            (should (byte-code-function-p function))
            (dolist (mode '(off on))
              (let* ((plan (nelisp-bytecode-native-rooted-cfg-plan input nil mode))
                     (expected (nelisp-bytecode-native-rooted-cfg-contract-plan-data-test--reference plan))
                     (actual (nelisp-bytecode-native-rooted-cfg-contract--plan-data plan)))
                (should (eq (plist-get plan :status) 'complete))
                (should (equal actual expected))
                (should (equal (nelisp-bytecode-native-rooted-cfg-contract--digest actual)
                               (nelisp-bytecode-native-rooted-cfg-contract--digest expected)))
                (should-not (plist-get actual :input))
                (should-not (memq :arithmetic-guard-context actual))
                (if (eq mode 'off)
                    (should-not (memq :arithmetic-guard-mode actual))
                  (should (eq (plist-get actual :arithmetic-guard-mode) 'on)))))))
      (delete-directory directory t))))

(ert-deftest nelisp-bytecode-native-rooted-cfg-contract-plan-data/order-and-mutations ()
  (dolist (mode '(off on))
    (let ((plan (list :status 'complete :input 'opaque :exit-root-base 3
                      :arithmetic-context 'opaque :arithmetic-guard-context 'opaque
                      :arithmetic-guard-mode mode :unrelated '(alpha . beta)
                      :duplicate 1 :duplicate 2
                      :entry-ast (list :tag 'entry :arithmetic-guard-context 'opaque
                                       :arithmetic-guard-mode mode :unrelated 7 :odd-tail))))
      (dolist (mutation '((:unrelated changed) (:duplicate 9) (:exit-root-base nil)))
        (let* ((changed (plist-put (copy-tree plan) (car mutation) (cadr mutation)))
               (expected (nelisp-bytecode-native-rooted-cfg-contract-plan-data-test--reference changed)))
          (should (equal (nelisp-bytecode-native-rooted-cfg-contract--plan-data changed)
                         expected)))))))

(provide 'nelisp-bytecode-native-rooted-cfg-contract-plan-data-test)
