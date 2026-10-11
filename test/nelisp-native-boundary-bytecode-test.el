;;; nelisp-native-boundary-bytecode-test.el --- Complete GNU link units -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(setq backtrace-line-length 200 print-length 20 print-level 8)
(require 'nelisp-native-cache)
(require 'nelisp-aot-compiler)
(require 'nelisp-bytecode-native-rooted-cfg-shared-emit)
(require 'nelisp-bytecode-native-consumer)
(let ((names '(p34-arith3 file-name-directory expand-file-name directory-files locate-file emacs-redisplay--ml-spans))
      (before nil) (forms nil) (plans nil) (emissions nil))
 (dolist (name names)
  (princ (format "P35-UNIT-BEFORE %s\n" name))
  (let* ((function (nelisp-bytecode-native-consumer-read-elc-function
                   (expand-file-name (concat "input-" (symbol-name name) ".elc") (or (getenv "FP_OUT") "target/p34/qualified")) name))
         (input (nelisp-bytecode-compiler-input-build function))
         (plan (nelisp-bytecode-native-rooted-cfg-plan input))
         (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "p35_test_entry"))
         (form (list 'seq (plist-get emitted :form))))
   (unless (eq (plist-get emitted :status) 'complete) (error "Incomplete %s" name))
   (push plan plans) (push emitted emissions)
   (let ((paired (nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input input "p35_test_entry")))
    (unless (and (equal plan (plist-get paired :plan)) (equal emitted (plist-get paired :emitted)))
     (error "Input-only emitter changed complete source plan/emission: %s" name)))
   (push form forms) (push (nelisp-aot-compile-to-link-unit form :arch 'x86_64 :format 'elf) before)))
 (let ((form (with-temp-buffer
   (insert-file-contents "target/nelisp-compiler-bytecode-load.el") (goto-char (point-min)) (read (current-buffer)))))
  (setq form (cons 'progn (cl-remove-if (lambda (x) (or (eq (car-safe x) 'nelisp-native-cache-prepare-cold-compiler) (and (eq (car-safe x) 'unless) (equal (cadr x) '(featurep 'nelisp-native-structural-bytecode))) (and (eq (car-safe x) 'load) (stringp (cadr x)) (string-suffix-p "nelisp-structural-bytecode.el" (cadr x))))) (cdr form))))
  ;; This GNU-only comparison uses an independent fresh projected owner set.
  ;; Production cold startup preserves its already authenticated owners.
  (dolist (module nelisp-native-cache--compiler-modules)
    (setq features (delq module features)))
  (eval (cons 'progn (append (butlast (cdr form)) '((load (locate-library "nelisp-bytecode-native-rooted-cfg-native.el" t) nil t t) (require 'nelisp-aot-compiler)) (last (cdr form)))) t))

 (dolist (name '(nelisp-aot-compiler--parse
                 nelisp-bytecode-native-rooted-cfg-plan
                 nelisp-bytecode-native-rooted-cfg-shared-emit-build))
   (unless (byte-code-function-p (symbol-function name))
     (error "Parity control did not load projected owner: %s" name)))
 ;; Also reconstruct each input and emitted form through the newly captured
 ;; compiler owners; comparing only the assembler would miss changed plans.
 (cl-mapc
  (lambda (name expected expected-plan expected-emission)
   (princ (format "P35-UNIT-AFTER %s\n" name))
   (let* ((fn (nelisp-bytecode-native-consumer-read-elc-function
               (expand-file-name (concat "input-" (symbol-name name) ".elc")
                                 (or (getenv "FP_OUT") "target/p34/qualified")) name))
          (input (nelisp-bytecode-compiler-input-build fn))
          (plan (nelisp-bytecode-native-rooted-cfg-plan input))
          (emitted (nelisp-bytecode-native-rooted-cfg-shared-emit-build plan "p35_test_entry"))
          (paired (nelisp-bytecode-native-rooted-cfg-shared-emit-build-from-input input "p35_test_entry")))
    (unless (and (equal plan expected-plan) (equal emitted expected-emission)
                 (equal plan (plist-get paired :plan)) (equal emitted (plist-get paired :emitted)))
     (error "Input-only bytecode emitter changed complete plan/emission: %s" name))
    (unless (and (eq (plist-get emitted :status) 'complete)
                 (equal expected (list 'seq (plist-get emitted :form))))
     (progn
      (with-temp-file (expand-file-name "target/p34/unit-form-before.el") (let ((print-length nil) (print-level nil)) (prin1 expected (current-buffer))))
      (with-temp-file (expand-file-name "target/p34/unit-form-after.el") (let ((print-length nil) (print-level nil)) (prin1 (list 'seq (plist-get emitted :form)) (current-buffer))))
      (error "Bytecode compiler changed a verified plan/emission: %s" name)))))
  (reverse names) forms plans emissions)
 (when (getenv "P35_BROKEN_EMITTER")
  (fset 'nelisp-asm-x86_64--imm32-bytes (lambda (_n) '(0 0 0 0))))
 (let ((count 0))
  (cl-mapc (lambda (form unit)
   (unless (equal unit (nelisp-aot-compile-to-link-unit form :arch 'x86_64 :format 'elf))
    (princ "P35-AOT-BROKEN-UNIT-DETECTED\n")
    (error "Bytecode compiler changed a complete link unit"))
   (setq count (1+ count))) forms before)
  (princ (format "P35-AOT-UNIT-PARITY-PASS cases=%d\n" count))))
