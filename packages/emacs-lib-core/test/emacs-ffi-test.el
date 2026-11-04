;;; emacs-ffi-test.el --- ERT tests for emacs-ffi -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-ffi)

(ert-deftest emacs-ffi-test/no-backend-is-unavailable ()
  "Host Emacs without NeLisp FFI primitives reports unavailable."
  (let ((orig (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (and (not (memq sym '(nl-ffi-call nelisp-ffi-call)))
                      (funcall orig sym)))))
      (should-not (emacs-ffi-available-p)))))

(ert-deftest emacs-ffi-test/runtime-library-env-wins ()
  "NELISP_RUNTIME_LIBRARY overrides default platform candidates."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY=/tmp/runtime/custom.dll"
                     process-environment))))
    (should (equal "/tmp/runtime/custom.dll"
                   (emacs-ffi-default-nelisp-runtime-library)))))

(ert-deftest emacs-ffi-test/runtime-library-keeps-legacy-env-first ()
  "NELISP_RUNTIME_SO remains the highest-priority compatibility env var."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO=/tmp/runtime/legacy.so"
               (cons "NELISP_RUNTIME_LIBRARY=/tmp/runtime/new.dll"
                     process-environment))))
    (should (equal "/tmp/runtime/legacy.so"
                   (emacs-ffi-default-nelisp-runtime-library)))))

(ert-deftest emacs-ffi-test/windows-runtime-name-candidates ()
  "Windows targets prefer dll runtime names."
  (let ((system-type 'windows-nt)
        (system-configuration "x86_64-w64-mingw32")
        (process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY="
                     (cons "NELISP_HOME=C:/nelisp"
                           (cons "ANVIL_HOME=" process-environment))))))
    (should (string-match-p "target/release/nelisp_runtime\\.dll\\'"
                            (emacs-ffi-default-nelisp-runtime-library)))))

(provide 'emacs-ffi-test)

;;; emacs-ffi-test.el ends here
