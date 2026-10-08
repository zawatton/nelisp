;;; standalone-bytecode-native-gnu-tq-driver.el --- pinned GNU tq proof -*- lexical-binding: t; -*-

(require 'nelisp-bytecode-native-compiler)
(require 'nelisp-native-load)
(require 'nelisp-bytecode-native-package)

(defun nelisp-test-gnu-tq-raw-smoke ()
  "Compile and call GNU tq-queue from byte-code without loading tq source."
  (let* ((source (getenv "NELISP_TQ_SOURCE"))
         (elc (getenv "NELISP_TQ_ELC"))
         (artifact (getenv "NELISP_TQ_ARTIFACT"))
         (expected-text (getenv "NELISP_TQ_ORACLE"))
         (definitions (nelisp-bytecode-native-package-read-elc-functions elc))
         (function (cdr (assq 'tq-queue definitions)))
         (input (cons (list 'queue) (cons 'process 'buffer)))
         (expected (with-temp-buffer
                     (insert expected-text)
                     (goto-char (point-min))
                     (read (current-buffer))))
         (build nil)
         (handle nil))
    (unless (and (not (file-exists-p source))
                 (byte-code-function-p function)
                 (not (featurep 'tq))
                 (not (fboundp 'tq-queue)))
      (error "GNU tq byte-code was evaluated or its source is unavailable"))
    (setq build
          (nelisp-bytecode-native-compiler-build
           function artifact "nl_native_car_probe"))
    (unless (and (eq (plist-get build :status) 'complete)
                 (equal (plist-get build :gateway-import) "nl_native_car_v2"))
      (error "GNU tq-queue did not compile through raw CAR: %S" build))
    (setq handle
          (nelisp-native-load-raw-v2-artifact
           artifact "nl_native_car_probe"
           (nelisp-native-load-running-binary-sha256)))
    (unwind-protect
        (let ((native (nelisp-native-load-raw-v2-car-call handle input))
              (vm (funcall function input)))
          (unless (and (equal native expected)
                       (equal vm expected)
                       (eq native (car input))
                       (eq vm (car input)))
            (error "GNU tq native/VM/stock parity or input identity failed: %S"
                   (list native vm expected)))
          (garbage-collect)
          (unless (and (eq (nelisp-native-load-raw-v2-car-call handle input) native)
                       (eq (funcall function input) native))
            (error "GNU tq queue identity changed across GC"))
          (setcar native 'queue-mutated)
          (unless (and (eq (nelisp-native-load-raw-v2-car-call handle input) native)
                       (eq (funcall function input) native)
                       (equal native '(queue-mutated)))
            (error "GNU tq queue mutation did not preserve aliasing"))
          (unless (and (not (file-exists-p source))
                       (not (featurep 'tq))
                       (not (fboundp 'tq-queue)))
            (error "GNU tq source or feature was loaded during native/VM calls"))
          (princ "GNU-TQ-RAW: PASS (GNU 31.1 pinned ELC, removed source, raw CAR, mutable identity across GC, VM and stock parity)\n"))
      (when handle
        (nelisp-native-load-unload handle)))))

;;; standalone-bytecode-native-gnu-tq-driver.el ends here
