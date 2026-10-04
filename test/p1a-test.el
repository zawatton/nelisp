;;; p1a-test.el --- Cache seal and source snapshot controls -*- lexical-binding: t; -*-
(require 'ert)
(require 'bytecomp)
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-rooted-cfg-native)
(require 'nelisp-bytecode-native-rooted-cfg-call)
(require 'nelisp-aot-compiler)

(defun p1a-test--fixture (check)
  "Run CHECK with real bytecode and compilation, simulating only runtime identity."
  (let* ((directory (make-temp-file "p1a-host-" t))
         (artifact (expand-file-name "unit.nelr" directory))
         (function (byte-compile '(lambda (a b) (cons a b))))
         (input (nelisp-bytecode-compiler-input-build function))
         (nelisp-bytecode-native-rooted-cfg-native--registry nil))
    (unwind-protect
        (cl-letf (((symbol-function 'nelisp-native-load-running-binary-sha256)
                   (lambda () (make-string 64 ?a)))
                  ((symbol-function 'nelisp-runtime-reload-contract-matches-p)
                   (lambda () t)))
          (funcall check function input artifact directory))
      (delete-directory directory t))))

(ert-deftest p1a/cache-does-not-seal-or-register ()
  (p1a-test--fixture
   (lambda (function _input _artifact directory)
     (let ((file (expand-file-name "cache.neln" directory))
           (nelisp-native-cache-backend 'in-house)
           (nelisp-native-cache-mode 'shared-v2)
           (nelisp-native-cache-guard-mode 'off))
       (cl-letf (((symbol-function 'nelisp-native-cache-file) (lambda (_) file))
                 ((symbol-function 'nelisp-native-cache-abi-hash) (lambda () "host-abi"))
                 ((symbol-function 'nelisp-native-cache--input-hash) (lambda (_) "host-input"))
                 ((symbol-function 'nelisp-native-cache--publish)
                  (lambda (temporary final) (rename-file temporary final)))
                 ((symbol-function 'nelisp-bytecode-native-rooted-cfg-native--fingerprint)
                  (lambda (&rest _) (ert-fail "cache computed admission fingerprint")))
                 ((symbol-function 'nelisp-bytecode-native-rooted-cfg-native--file-sha256)
                  (lambda (&rest _) (ert-fail "cache computed admission artifact seal"))))
         (should (equal (nelisp-native-cache-compile function) file))
         (should (file-exists-p file))
         (should-not nelisp-bytecode-native-rooted-cfg-native--registry))))))

(ert-deftest p1a/public-build-still-seals ()
  (p1a-test--fixture
   (lambda (_function input artifact _directory)
     (let* ((fingerprint (symbol-function 'nelisp-bytecode-native-rooted-cfg-native--fingerprint))
            (calls 0)
            (result
             (cl-letf (((symbol-function 'nelisp-bytecode-native-rooted-cfg-native--fingerprint)
                        (lambda (value) (setq calls (1+ calls)) (funcall fingerprint value))))
               (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 input artifact 'off))))
       (should (= calls 1))
       (should (assq result nelisp-bytecode-native-rooted-cfg-native--registry))
       (should (nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p result))))))

(ert-deftest p1a/unsealed-result-refused-by-authentication-and-admission ()
  (p1a-test--fixture
   (lambda (_function input artifact _directory)
     (let ((result (nelisp-bytecode-native-rooted-cfg-native--build input artifact t 'off t)))
       (should (eq (plist-get result :status) 'complete))
       (should-not (nelisp-bytecode-native-rooted-cfg-native-authenticated-result-p result))
       (should-error (nelisp-bytecode-native-rooted-cfg-call result))))))

(ert-deftest p1a/forms-and-file-manifests-equal ()
  (p1a-test--fixture
   (lambda (_function input artifact _directory)
     (let ((compiler (symbol-function 'nelisp-native-load-raw-v2-compile-file))
           (parser (symbol-function 'nelisp-native-load--raw-source-forms))
           (writer (symbol-function 'write-region))
           (calls 0))
       (cl-letf (((symbol-function 'write-region)
                  (lambda (start end filename &rest options)
                    ;; Exercise write-region's buffer coding selection with no
                    ;; explicit writer override, only on the producer source.
                    (when (string-prefix-p "nelisp-rooted-cfg-" (file-name-nondirectory filename))
                      (should-not coding-system-for-write)
                      (setq buffer-file-coding-system 'utf-8-dos))
                    (apply writer start end filename options)))
                 ((symbol-function 'nelisp-native-load-raw-v2-compile-file)
                  (lambda (&rest args)
                    (should (consp (nth 12 args)))
                    (setq calls (1+ calls))
                    (let ((forms-manifest
                           (cl-letf (((symbol-function 'nelisp-native-load--raw-source-forms)
                                      (lambda (&rest _) (ert-fail "forms path re-parsed source"))))
                             (apply compiler args))))
                      (cl-letf (((symbol-function 'nelisp-native-load--raw-source-forms) parser))
                        (should (equal forms-manifest
                                       (apply compiler (append (butlast args) '(nil))))))
                      forms-manifest))))
         (nelisp-bytecode-native-rooted-cfg-native-build-shared-v2 input artifact 'off)
         (should (= calls 1)))))))

(ert-deftest p1a/snapshot-provenance-respects-file-encoding ()
  (let* ((directory (make-temp-file "p1a-encoding-" t))
         (source (expand-file-name "source.el" directory))
         (artifact (expand-file-name "unit.nelr" directory))
         (forms (mapcar
                 (lambda (entry)
                   (list 'defun (intern (car entry))
                         (cl-loop for i below (cdr entry) collect (intern (format "arg%d" i))) 0))
                 (nelisp-native-load-raw-v2-contract))))
    (unwind-protect
        (dolist (coding-system-for-write '(utf-8-unix utf-8-dos))
          (let (bytes)
            (with-temp-file source
              (dolist (form forms) (prin1 form (current-buffer)) (insert "\n"))
              (setq bytes (encode-coding-string (buffer-string) coding-system-for-write)))
            (let ((file-manifest (nelisp-native-load-raw-v2-compile-file source artifact "encoding" (make-string 64 ?a))))
              (should (equal file-manifest
                             (nelisp-native-load-raw-v2-compile-file
                              source artifact "encoding" (make-string 64 ?a)
                              nil nil nil nil nil nil nil nil (cons forms bytes)))))))
      (delete-directory directory t))))

(provide 'p1a-test)
