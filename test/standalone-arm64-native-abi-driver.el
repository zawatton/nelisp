;;; standalone-arm64-native-abi-driver.el --- AAPCS64 acceptance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(load "test/standalone-native-funcall-v2-driver.el" nil t)
(let ((executions 0))
  (f1-root-case (lambda () (setq executions (+ executions 1))) nil t)
  (f1-root-case (lambda (&rest _) (setq executions (+ executions 1))) '(1 2 3 4 5 6) t)
  (f1-root-assert (= executions 0) "wrong tickets refused before Lisp execution")
  (princ "ARM64-TICKET-REFUSED N=0 N=6 executions=0\n"))
(load "test/support/arm64-native-probes.el" nil t)
(unless (nelisp-native-load--arm64-p) (error "AAPCS64 sentinel requires Linux ARM64"))
(let* ((original (symbol-function 'nelisp-native-load--symbol-addr))
       (target (funcall original "nl_native_funcall_v2"))
       (size (nelisp-native-load--page-round (length arm64-native-sentinel-code)))
       (memory (nelisp-native-load--mmap size nil)))
  (unwind-protect
      (progn
        (nelisp-native-load--poke-string memory 0 arm64-native-sentinel-code)
        (ptr-write-u64 memory arm64-native-sentinel-target-offset target)
        (nelisp-native-load--mprotect-rx memory size)
        (cl-letf (((symbol-function 'nelisp-native-load--symbol-addr)
                   (lambda (name) (if (equal name "nl_native_funcall_v2") memory
                                    (funcall original name)))))
          (f1-root-assert (eq (f1-root-case (lambda () (garbage-collect) 'zero) nil) 'zero) "sentinel N=0")
          (f1-root-assert (= (f1-root-case (lambda (a b c d e f) (garbage-collect) (+ a b c d e f))
                                         '(1 2 3 4 5 6)) 21) "sentinel N=6"))
        (princ "ARM64-REGISTER-SENTINEL-PASS N=0 N=6 GP=11 D=8 SP=16\n"))
    (nelisp-native-load--unmap memory size)))
