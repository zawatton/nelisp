;;; nelisp-native-f1-probe-test.el --- F1 measurement boundaries -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'bytecomp)
(require 'nelisp-native-cache)
(require 'nelisp-bytecode-native-consumer)

(defconst nelisp-native-f1-probe-test--driver
  (expand-file-name "standalone-bytecode-native-funcall-driver.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun nelisp-native-f1-probe-test--run (cache-validations checked-validations)
  "Run the actual driver, simulating only the runtime/cache boundary."
  (let* ((file (make-temp-file "f1-probe-"))
         (process-environment (copy-sequence process-environment))
         (nelisp-bytecode-native-rooted-cfg-contract--validation-count 0)
         (byte-compile-warnings nil)
         (function (byte-compile '(lambda (x) (f1-user (cons (car x) (cdr x))))))
         (output "")
         (handle '(:exports (probe) :gc-table 100))
         (manifest '(:native (:imports ((:name "nl_native_funcall_v2")
                                        (:name "nl_root_pin_slot_v2")))
                     :gc-entries ((:name "gc-entry" :index 0)))))
    (setenv "F1_PHASE" "load")
    (setenv "F1_BACKEND" "in-house")
    (setenv "F1_FORCE_GC" "1")
    (unwind-protect
        (progn
          (write-region (concat "(:entry \"probe\")\n" (prin1-to-string manifest))
                        nil file nil 'silent)
          (cl-letf (((symbol-function 'nelisp-bytecode-native-consumer-read-elc-functions)
                     (lambda (_) (list (cons 'f1-fixture function))))
                    ((symbol-function 'nelisp-native-cache-file) (lambda (_) file))
                    ((symbol-function 'nelisp-native-cache-load)
                     (lambda (_)
                       (cl-incf nelisp-bytecode-native-rooted-cfg-contract--validation-count cache-validations)
                       function))
                    ((symbol-function 'nelisp-native-load-raw-v2-artifact)
                     (lambda (&rest _)
                       (cl-incf nelisp-bytecode-native-rooted-cfg-contract--validation-count checked-validations)
                       handle))
                    ((symbol-function 'nelisp-native-load-raw-v2-artifact-trusted) (lambda (&rest _) handle))
                    ((symbol-function 'nelisp-native-load-running-binary-sha256) (lambda () (make-string 64 ?a)))
                    ((symbol-function 'nelisp-native-load--symbol-addr) (lambda (_) 200))
                    ((symbol-function 'ptr-read-u64) (lambda (&rest _) 200))
                    ((symbol-function 'nelisp-native-load-unload) #'ignore)
                    ((symbol-function 'garbage-collect) #'ignore)
                    ((symbol-function 'princ)
                     (lambda (value &optional _stream)
                       (setq output (concat output value)) value))
                    ((symbol-function 'f1-user) #'identity))
            (load nelisp-native-f1-probe-test--driver nil t t)
            output))
      (delete-file file))))

(ert-deftest nelisp-native-f1-probe-cache-and-checked-counts ()
  "The checked GC positive control must not contaminate cache measurements."
  (let ((output (nelisp-native-f1-probe-test--run 0 1)))
    (should (string-match-p "F1-CACHE-PASS backend=in-house corpus=5 seconds=[0-9.]+ validations=0" output))
    (should (string-match-p "F1-GC-MAPPER-PASS mapper=checked seconds=[0-9.]+ validations=1" output))
    (should (string-match-p "F1-GC-MAPPER-PASS mapper=trusted seconds=[0-9.]+ validations=0" output))
    (should (string-match-p "F1-FORCED-GC-PASS backend=in-house" output))))

(ert-deftest nelisp-native-f1-probe-actual-revalidation-refuses ()
  "Neither cache revalidation nor a vacuous checked control is accepted."
  (should-error (nelisp-native-f1-probe-test--run 1 1))
  (should-error (nelisp-native-f1-probe-test--run 0 0)))

(provide 'nelisp-native-f1-probe-test)
