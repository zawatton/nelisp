;;; standalone-bytecode-native-unary-driver.el --- raw unary proof -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-package)
(defun nelisp-test-native-unary-bytecode-smoke ()
(let* ((source (getenv "NELISP_UNARY_SOURCE"))
       (elc (getenv "NELISP_UNARY_ELC"))
       (car-artifact (getenv "NELISP_UNARY_CAR_ARTIFACT"))
       (cdr-artifact (getenv "NELISP_UNARY_CDR_ARTIFACT"))
       (expected (getenv "NELISP_UNARY_GNU_ORACLE"))
       (definitions (nelisp-bytecode-native-package-read-elc-functions elc))
       (car-function (cdr (assq 'nelisp-unary-car definitions)))
       (cdr-function (cdr (assq 'nelisp-unary-cdr definitions)))
       (car-result nil) (cdr-result nil) (car-handle nil) (cdr-handle nil)
       (malformed nil) (nearby nil) (import-refused nil) (bad-source nil)
       (bad-artifact (concat cdr-artifact ".bad")))
  (unless (and (stringp source) (not (file-exists-p source))
               (stringp expected) (byte-code-function-p car-function)
               (byte-code-function-p cdr-function))
    (error "unary smoke did not load source-free GNU byte-code functions"))
  (setq car-result
        (nelisp-bytecode-native-compiler-build
         car-function car-artifact "nl_native_car_probe")
        cdr-result
        (nelisp-bytecode-native-compiler-build
         cdr-function cdr-artifact "nl_native_cdr_probe"))
  (unless (and (eq (plist-get car-result :status) 'complete)
               (eq (plist-get cdr-result :status) 'complete)
               (equal (plist-get car-result :gateway-import) "nl_native_car_v2")
               (equal (plist-get cdr-result :gateway-import) "nl_native_cdr_v2"))
    (error "unary compiler refused exact template: %S %S" car-result cdr-result))
  (setq car-handle
        (nelisp-native-load-raw-v2-artifact car-artifact
                                            "nl_native_car_probe"
                                            (nelisp-native-load-running-binary-sha256))
        cdr-handle
        (nelisp-native-load-raw-v2-artifact cdr-artifact
                                            "nl_native_cdr_probe"
                                            (nelisp-native-load-running-binary-sha256)))
  (unless (and (equal (plist-get car-handle :imports) '("nl_native_car_v2"))
               (equal (plist-get cdr-handle :imports) '("nl_native_cdr_v2")))
    (error "unary artifact imports are not operation-specific"))
  (let* ((value (cons 'oracle-car (list 'oracle-tail)))
         (oracle (with-temp-buffer (insert expected) (goto-char (point-min))
                   (read (current-buffer))))
         (native (list :car (nelisp-native-load-raw-v2-car-call car-handle value)
                       :cdr (nelisp-native-load-raw-v2-cdr-call cdr-handle value))))
    (unless (and (equal oracle '(:car oracle-car :cdr (oracle-tail)))
                 (equal native oracle))
      (error "native unary result diverges from stock GNU oracle: %S / %S"
             native oracle)))
  (let* ((leaf (list 'leaf)) (car-input (cons leaf nil))
         (tail (list 'tail)) (cdr-input (cons 'head tail)))
    (unless (and (eq (nelisp-native-load-raw-v2-car-call car-handle car-input) leaf)
                 (eq (nelisp-native-load-raw-v2-cdr-call cdr-handle cdr-input) tail))
      (error "native unary identity mismatch before GC"))
    (garbage-collect)
    (setcar leaf 'car-mutated)
    (setcar tail 'cdr-mutated)
    (unless (and (eq (nelisp-native-load-raw-v2-car-call car-handle car-input) leaf)
                 (eq (nelisp-native-load-raw-v2-cdr-call cdr-handle cdr-input) tail)
                 (eq (car (nelisp-native-load-raw-v2-car-call car-handle car-input))
                     'car-mutated)
                 (eq (car (nelisp-native-load-raw-v2-cdr-call cdr-handle cdr-input))
                     'cdr-mutated))
      (error "native unary identity/mutation mismatch after GC")))
  (unless (and (null (nelisp-native-load-raw-v2-car-call car-handle nil))
               (null (nelisp-native-load-raw-v2-cdr-call cdr-handle nil)))
    (error "native unary nil mismatch"))
  (unless (and (condition-case nil
                   (progn (nelisp-native-load-raw-v2-car-call car-handle 9) nil)
                 (wrong-type-argument t))
               (condition-case nil
                   (progn (nelisp-native-load-raw-v2-cdr-call cdr-handle 9) nil)
                 (wrong-type-argument t)))
    (error "native unary wrong-type argument was accepted"))
  (setq malformed
        (nelisp-bytecode-native-compiler-build
         (make-byte-code 257 (unibyte-string 64 135) [] 0)
         (concat car-artifact ".malformed") "nl_native_car_probe")
        nearby
        (nelisp-bytecode-native-compiler-build
         (make-byte-code 257 (unibyte-string 64 137 135) [] 3)
         (concat cdr-artifact ".nearby") "nl_native_cdr_probe"))
  (unless (and (eq (plist-get malformed :status) 'malformed)
               (eq (plist-get nearby :status) 'unsupported)
               (not (file-exists-p (concat car-artifact ".malformed")))
               (not (file-exists-p (concat cdr-artifact ".nearby"))))
    (error "unary malformed/nearby refusal failed: %S %S" malformed nearby))
  (setq bad-source (make-temp-file "nelisp-unary-bad-import-" nil ".el"))
  (unwind-protect
      (progn
        (with-temp-file bad-source
          (insert "(defun nl_native_car_probe (env ticket input-index output-index)\n"
                  "  (extern-call nl_not_authenticated env ticket input-index output-index 0 0))\n"))
        (setq import-refused
              (condition-case nil
                  (progn
                    (nelisp-native-load-raw-v2-compile-file
                     bad-source bad-artifact "unary-import-negative"
                     (nelisp-native-load-running-binary-sha256))
                    nil)
                (error t)))
        (unless (and import-refused (not (file-exists-p bad-artifact)))
          (error "unauthenticated unary import published an artifact")))
    (when (file-exists-p bad-source) (delete-file bad-source)))
  (unless (equal expected "(:car oracle-car :cdr (oracle-tail))")
    (error "unexpected stock GNU oracle: %s" expected))
  (nelisp-native-load-unload car-handle)
  (nelisp-native-load-unload cdr-handle)
  t))

;;; standalone-bytecode-native-unary-driver.el ends here
