;;; nelisp-bytecode-native-rooted-stack-cold-compare-test.el --- artifact comparison -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-bytecode-native-rooted-stack-cold-compare)

(defun nelisp-bytecode-native-rooted-stack-cold-compare-test--manifest (source text)
  (list :format 'nelisp-private-nelr-v2 :kind 'raw-runtime
        :source source :source-sha256 "source-sha" :compiled-source-sha256 "compiled-sha"
        :binary-sha256 "runtime-sha" :artifact-sha256 "inner-artifact-sha"
        :native-rooted-stack-contract-version "nelisp-native-rooted-stack-v1"
        :native-rooted-stack-entry "nl_native_stack_probe_v1"
        :native-rooted-stack-gateway-imports '("nl_native_car_v2")
        :native-rooted-stack-status-base 256 :native-rooted-stack-contract-hash "contract-sha"
        :native (list :text-base64 text
                      :exports '((:name "nl_native_stack_probe_v1" :type func :abi "nelisp-runtime-raw-v2"
                                  :arity 4 :return u64 :params (u64 u64 u64 u64)))
                      :imports '((:name "nl_native_car_v2" :kind func :abi "nelisp-runtime-raw-v2"
                                  :index 34 :address-mode native-bridgeable-v1))
                      :relocs '((:offset 1 :symbol "nl_native_car_v2")))))

(defun nelisp-bytecode-native-rooted-stack-cold-compare-test--write (path manifest)
  (with-temp-file path
    (insert ";;; nelisp-private-nelr-v2\n")
    (prin1 manifest (current-buffer))))

(ert-deftest nelisp-bytecode-native-rooted-stack-cold-compare-normalizes-only-source-and-outer-hash ()
  (let* ((dir (make-temp-file "rooted-compare-" t))
         (fresh-path (expand-file-name "fresh.nelr" dir))
         (stored-path (expand-file-name "stored.nelr" dir))
         (fresh (nelisp-bytecode-native-rooted-stack-cold-compare-test--manifest "/tmp/fresh.el" "code"))
         (stored (nelisp-bytecode-native-rooted-stack-cold-compare-test--manifest "/tmp/stored.el" "code"))
         (result nil))
    (unwind-protect
        (progn
          (nelisp-bytecode-native-rooted-stack-cold-compare-test--write fresh-path fresh)
          (nelisp-bytecode-native-rooted-stack-cold-compare-test--write stored-path stored)
          (setq result (list :artifact-path fresh-path
                             :artifact-sha256 (nelisp-bytecode-native-package-raw-file-sha256 fresh-path)
                             :runtime-binary-sha256 "runtime-sha"))
          (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-stack-authenticated-result-p)
                     (lambda (_) t))
                    ((symbol-function 'nelisp-native-load-running-binary-sha256)
                     (lambda () "runtime-sha"))
                    ((symbol-function 'nelisp-native-load-raw-v2-check)
                     (lambda (_manifest _name) nil)))
            (should (nelisp-bytecode-native-rooted-stack-cold-equivalent-p result stored-path))
            (dolist (mutate
                     (list (lambda (m) (plist-put m :source-sha256 "changed"))
                           (lambda (m) (plist-put m :unknown-field 'extra))
                           (lambda (m)
                             (let ((native (plist-get m :native)))
                               (plist-put native :text-base64 "changed")
                               (plist-put m :native native)))
                           (lambda (m)
                             (let ((native (plist-get m :native)))
                               (plist-put native :imports nil)
                               (plist-put m :native native)))))
              (let ((changed (copy-tree stored)))
                (setq changed (funcall mutate changed))
                (nelisp-bytecode-native-rooted-stack-cold-compare-test--write stored-path changed)
                (should-not (nelisp-bytecode-native-rooted-stack-cold-equivalent-p result stored-path))))))
      (delete-directory dir t))))

(ert-deftest nelisp-bytecode-native-rooted-stack-cold-compare-matches-retained-artifact ()
  (let ((stored (getenv "NELISP_ROOTED_STORED_ARTIFACT"))
        (elc (getenv "NELISP_ROOTED_ELC"))
        (source (getenv "NELISP_ROOTED_SOURCE"))
        (dir (make-temp-file "rooted-compare-real-" t)))
    (unwind-protect
        (if (not (and stored (file-readable-p stored) elc (file-readable-p elc)
                      (not (file-exists-p source))))
            (ert-skip "real retained artifact inputs not configured")
          (let* ((raw (cdr (assq 'nelisp-rooted-stack-documented-car
                                 (nelisp-bytecode-native-package-read-elc-functions elc))))
                 (function (make-byte-code (aref raw 0) (aref raw 1) (aref raw 2)
                                           (aref raw 3) (aref raw 4)))
                 (fresh-path (expand-file-name "fresh.nelr" dir))
                 (result (nelisp-bytecode-native-compiler-build
                          function fresh-path "nl_native_stack_probe_v1")))
            (should (eq (plist-get result :status) 'complete))
            (should (nelisp-bytecode-native-rooted-stack-authenticated-result-p result))
            (should (nelisp-bytecode-native-rooted-stack-cold-equivalent-p result stored))))
      (delete-directory dir t))))

(provide 'nelisp-bytecode-native-rooted-stack-cold-compare-test)
