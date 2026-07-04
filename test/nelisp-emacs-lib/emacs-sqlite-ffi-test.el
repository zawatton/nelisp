;;; emacs-sqlite-ffi-test.el --- ERT tests for emacs-sqlite-ffi -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 51 Track I (2026-05-04) — regression tests for the FFI library
;; path resolution helper.  The actual FFI calls require a running
;; libnelisp_runtime.so so they are out of scope here; we verify the
;; envvar / default-path logic only.

;;; Code:

(require 'ert)
(require 'emacs-sqlite-ffi)

(ert-deftest emacs-sqlite-ffi-test/libpath-honours-runtime-so-env ()
  "When NELISP_RUNTIME_SO is set, default-libpath returns it verbatim."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO=/some/explicit/path.so"
               (cons "NELISP_HOME=/ignored"
                     process-environment))))
    (should (equal "/some/explicit/path.so"
                   (emacs-sqlite-ffi--default-libpath)))))

(ert-deftest emacs-sqlite-ffi-test/libpath-falls-back-to-nelisp-home ()
  "When NELISP_RUNTIME_SO is empty and NELISP_HOME is set, use runtime lib."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY="
                     (cons "NELISP_HOME=/opt/nelisp"
                     process-environment))))
    (should (equal "/opt/nelisp/target/release/libnelisp_runtime.so"
                   (emacs-sqlite-ffi--default-libpath))))))

(ert-deftest emacs-sqlite-ffi-test/libpath-falls-back-to-home ()
  "With neither env var set, the result includes a sensible HOME-based path."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY="
                     (cons "NELISP_HOME="
                     (cons "HOME=/tmp/u"
                           process-environment))))))
    (let ((p (emacs-sqlite-ffi--default-libpath)))
      (should (string-match-p "Notes/dev/nelisp/target/release/\\(lib\\)?nelisp_runtime\\."
                              p))
      (should (string-match-p "\\`\\([A-Za-z]:\\)?/tmp/u/" p)))))

(ert-deftest emacs-sqlite-ffi-test/libpath-honours-trailing-slash ()
  "NELISP_HOME with or without trailing slash should produce the same path."
  (let ((process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY="
                     (cons "NELISP_HOME=/opt/nelisp/"
                     process-environment))))
    (should (equal "/opt/nelisp/target/release/libnelisp_runtime.so"
                   (emacs-sqlite-ffi--default-libpath))))))

(ert-deftest emacs-sqlite-ffi-test/libpath-supports-windows-runtime ()
  "Windows targets resolve to nelisp_runtime.dll by default."
  (let ((system-type 'windows-nt)
        (system-configuration "x86_64-w64-mingw32")
        (process-environment
         (cons "NELISP_RUNTIME_SO="
               (cons "NELISP_RUNTIME_LIBRARY="
                     (cons "NELISP_HOME=C:/nelisp"
                           process-environment)))))
    (should (equal "c:/nelisp/target/release/nelisp_runtime.dll"
                   (downcase (emacs-sqlite-ffi--default-libpath))))))

(provide 'emacs-sqlite-ffi-test)

;;; emacs-sqlite-ffi-test.el ends here
