;;; nelisp-aot-extern-call-ptr-callee-test.el --- extern-call-ptr with an inline callee  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `(extern-call-ptr FN-EXPR ARG...)' used to emit FN-EXPR *after* the
;; arguments were placed in rdi/rsi/rdx, so a non-trivial FN-EXPR (for
;; example an inline `(extern-call dlsym ...)') clobbered them and the
;; program crashed.  The parser now hoists a non-trivial FN-EXPR into a
;; fresh local evaluated before the arguments.  Each test compiles a
;; program to an object, links it with a tiny C driver that prints the
;; result of `probe', and compares with the value computed in Lisp.

;;; Code:

(require 'ert)
(require 'nelisp-aot-compiler)

(defun nelisp-aot-ecp-callee-test--int32 (text)
  "Parse TEXT as a long and sign-interpret its low 32 bits (C `int' result)."
  (let ((u (logand (string-to-number text) #xffffffff)))
    (if (>= u #x80000000) (- u #x100000000) u)))

(defun nelisp-aot-ecp-callee-test--run (program)
  "Compile PROGRAM, link it with a C driver and return probe's output string."
  (let ((cc (or (executable-find "cc") (executable-find "gcc"))))
    (let* ((dir (make-temp-file "nelisp-ecp-callee" t))
           (obj (expand-file-name "p.o" dir))
           (drv (expand-file-name "d.c" dir))
           (bin (expand-file-name "p" dir)))
      (unwind-protect
          (progn
            (nelisp-aot-compile-to-object program obj)
            (with-temp-file drv
              (insert "#include <stdio.h>\nextern long probe(void);\n"
                      "int main(void){ printf(\"%ld\\n\", probe()); return 0; }\n"))
            (should (zerop (call-process cc nil nil nil drv obj "-o" bin
                                         "-ldl" "-lm")))
            (with-temp-buffer
              (call-process bin nil t nil)
              (string-trim (buffer-string))))
        (ignore-errors (delete-directory dir t))))))

(ert-deftest nelisp-aot-ecp-callee/strncmp-callee-in-local ()
  "Control: the callee held in a local keeps working."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)
                    (or (executable-find "cc") (executable-find "gcc"))))
  (should (= -1
             (nelisp-aot-ecp-callee-test--int32
                 (nelisp-aot-ecp-callee-test--run
                  '(seq (data-blob libc_name "libc.so.6\0" rodata)
                        (data-blob sym "strncmp\0" rodata)
                        (data-blob sa "abc\0" rodata)
                        (data-blob sb "abd\0" rodata)
                        (defun probe ()
                          (let* ((h (extern-call dlopen (data-addr libc_name) 2))
                                 (f (extern-call dlsym h (data-addr sym))))
                            (extern-call-ptr f (data-addr sa) (data-addr sb) 3)))))))))

(ert-deftest nelisp-aot-ecp-callee/inline-dlsym-callee ()
  "An inline `extern-call dlsym' callee must not clobber the arguments."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)
                    (or (executable-find "cc") (executable-find "gcc"))))
  ;; strncmp ("abc", "abd", 3) < 0 ; glibc returns -1 for this pair, and
  ;; strncmp (.., 2) is 0.
  (dolist (case '((3 . -1) (2 . 0)))
    (should (= (cdr case)
               (nelisp-aot-ecp-callee-test--int32
                   (nelisp-aot-ecp-callee-test--run
                    `(seq (data-blob libc_name "libc.so.6\0" rodata)
                          (data-blob sym "strncmp\0" rodata)
                          (data-blob sa "abc\0" rodata)
                          (data-blob sb "abd\0" rodata)
                          (defun probe ()
                            (let ((h (extern-call dlopen (data-addr libc_name) 2)))
                              (extern-call-ptr (extern-call dlsym h (data-addr sym))
                                               (data-addr sa) (data-addr sb)
                                               ,(car case)))))))))))

(ert-deftest nelisp-aot-ecp-callee/inline-callee-with-computed-args ()
  "Callee and arguments that are themselves calls/arithmetic."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)
                    (or (executable-find "cc") (executable-find "gcc"))))
  ;; labs(-(40 + 2)) = 42 via an inline dlsym callee and a computed argument.
  (should (equal "42"
                 (nelisp-aot-ecp-callee-test--run
                  '(seq (data-blob libc_name "libc.so.6\0" rodata)
                        (data-blob sym "labs\0" rodata)
                        (defun probe ()
                          (let ((h (extern-call dlopen (data-addr libc_name) 2)))
                            (extern-call-ptr (extern-call dlsym h (data-addr sym))
                                             (- 0 (+ 40 2))))))))))

(ert-deftest nelisp-aot-ecp-callee/f64-callee-in-local-and-inline-rejected ()
  "`extern-call-ptr-f64' works with a local callee; an inline one is a clear error."
  (skip-unless (and (eq system-type 'gnu/linux)
                    (string-match-p "x86_64" system-configuration)
                    (or (executable-find "cc") (executable-find "gcc"))))
  ;; sqrt (4.0) = 2.0 = #x4000000000000000
  (should (equal (number-to-string #x4000000000000000)
                 (nelisp-aot-ecp-callee-test--run
                  '(seq (data-blob libm_name "libm.so.6\0" rodata)
                        (data-blob sym "sqrt\0" rodata)
                        (defun probe ()
                          (let* ((h (extern-call dlopen (data-addr libm_name) 2))
                                 (f (extern-call dlsym h (data-addr sym))))
                            (f64-bits (extern-call-ptr-f64 f (:f64 4.0)))))))))
  (let ((err (should-error
              (nelisp-aot-compile-to-object
               '(seq (defun probe (h) (f64-bits (extern-call-ptr-f64
                                                 (extern-call dlsym h 0)
                                                 (:f64 4.0)))))
               (make-temp-file "nelisp-ecp-callee-rej-" nil ".o"))
              :type 'nelisp-aot-compiler-error)))
    (should (eq :extern-call-ptr-f64-fn-must-be-trivial (cadr err)))))

(ert-deftest nelisp-aot-ecp-callee/aarch64-trivial-callee-compiles ()
  "The trivial (local) callee form still compiles to an aarch64 object."
  (let ((path (make-temp-file "nelisp-ecp-callee-a64-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-aot-compile-to-object
           '(seq (defun probe (f a) (extern-call-ptr f a 3)))
           path :arch 'aarch64)
          (should (> (file-attribute-size (file-attributes path)) 0)))
      (ignore-errors (delete-file path)))))

(provide 'nelisp-aot-extern-call-ptr-callee-test)
;;; nelisp-aot-extern-call-ptr-callee-test.el ends here
