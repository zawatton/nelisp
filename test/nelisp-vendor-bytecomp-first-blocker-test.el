;;; nelisp-vendor-bytecomp-first-blocker-test.el --- First blocker tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'nelisp-vendor-bytecomp-first-blocker)

(ert-deftest nelisp-vendor-bytecomp-first-blocker-pinned-form-location ()
  (skip-unless (equal emacs-version "31.1"))
  (nelisp-vendor-bytecode-jit-coverage--verify-sources)
  (let* ((source (expand-file-name
                  "bytecomp.el"
                  (nelisp-vendor-bytecode-jit-coverage--source-root)))
         (form (nth 3 (nelisp-vendor-bytecomp-first-blocker--source-forms
                       source))))
    (should (= (plist-get form :index) 4))
    (should (= (plist-get form :line) 127))
    (should (equal (plist-get form :form)
                   "(eval-when-compile (require 'compile))"))))

(ert-deftest nelisp-vendor-bytecomp-first-blocker-broken-fixture-is-detected ()
  (let* ((root (nelisp-vendor-bytecode-triparity--root))
         (binary (or (getenv "NELISP_BIN")
                     (expand-file-name "target/nelisp" root)))
         (parent (expand-file-name "target/bytecomp-first-blocker" root))
         (directory (progn
                      (make-directory parent t)
                      (make-temp-file (expand-file-name "negative-ert-" parent) t))))
    (unwind-protect
        (let* ((object (make-byte-code 513 (unibyte-string 135) [] 1))
               (result (nelisp-vendor-bytecomp-first-blocker--negative-control
                        binary directory object nil)))
          (should (plist-get result :detected))
          (should (equal (plist-get (plist-get result :result) :missing_symbol)
                         "nelisp-bytecomp-deliberately-missing")))
      (delete-directory directory t))))

(ert-deftest nelisp-vendor-bytecomp-first-blocker-detects-binary-mutation ()
  (let* ((binary (make-temp-file "nelisp-bytecomp-binary-"))
         (expected nil))
    (unwind-protect
        (progn
          (with-temp-file binary (insert "pinned executable"))
          (set-file-modes binary #o755)
          (setq expected
                (nelisp-vendor-bytecode-jit-coverage--source-fingerprint binary))
          (cl-letf (((symbol-function
                      'nelisp-vendor-bytecomp-first-blocker--run-lane)
                     (lambda (&rest _arguments)
                       (with-temp-file binary (insert "mutated executable"))
                       (list :status "error" :exit_code 1))))
            (let ((result
                   (nelisp-vendor-bytecomp-first-blocker--run-pinned-lane
                    binary expected nil nil 'vm nil nil 1)))
              (should (equal (plist-get result :status) "binary-mismatch"))
              (should (equal (plist-get result :lane_status) "error"))
              (should-not (plist-get result :binary_identity_ok))
              (should-not (equal (plist-get result :binary_sha256_after) expected)))))
      (when (file-exists-p binary) (delete-file binary)))))

(provide 'nelisp-vendor-bytecomp-first-blocker-test)
;;; nelisp-vendor-bytecomp-first-blocker-test.el ends here
