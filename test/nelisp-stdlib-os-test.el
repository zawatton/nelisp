;;; nelisp-stdlib-os-test.el --- ERT tests for OS stdlib surface  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Focused host-side tests for `lisp/nelisp-stdlib-os.el'.  The real runtime
;; provides raw memory / FFI primitives; these tests mock that boundary so the
;; Windows HANDLE branch can be verified on non-Windows hosts.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(unless (fboundp 'nelisp--syscall-supported-p)
  (defun nelisp--syscall-supported-p () nil))

(require 'nelisp-stdlib-os)

(ert-deftest nelisp-stdlib-os-windows-std-handle-selectors ()
  "POSIX-like std fds map to Windows GetStdHandle selector constants."
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDIN)
             nelisp-os-WIN-STD-INPUT-HANDLE))
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDOUT)
             nelisp-os-WIN-STD-OUTPUT-HANDLE))
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDERR)
             nelisp-os-WIN-STD-ERROR-HANDLE))
  (should-not (nelisp-os--windows-std-handle-selector 99)))

(ert-deftest nelisp-stdlib-os-write-windows-stdout-uses-kernel32 ()
  "On Windows, stdout write routes through GetStdHandle + WriteFile."
  (let ((calls nil)
        (alloc-next 1000)
        (freed nil)
        (written-string nil)
        (zeroed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str)
                 (setq written-string (cons ptr str))))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val)
                 (setq zeroed (list ptr off val))
                 val))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (_ptr _off) 3))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #x12345678)
                  ((equal fn "WriteFile") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-write nelisp-os-STDOUT "abc") 3))))
    (setq calls (nreverse calls))
    (should (equal (car calls)
                   (list "kernel32" "GetStdHandle"
                         [:pointer :sint32]
                         (list nelisp-os-WIN-STD-OUTPUT-HANDLE))))
    (should (equal (cadr calls)
                   (list "kernel32" "WriteFile"
                         [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                         (list #x12345678 1000 3 2000 0))))
    (should (equal written-string (cons 1000 "abc")))
    (should (equal zeroed (list 2000 0 0)))
    (should (equal (sort freed #'<) '(1000 2000)))))

(ert-deftest nelisp-stdlib-os-write-windows-stderr-selector ()
  "Windows stderr write asks GetStdHandle for STD_ERROR_HANDLE."
  (let ((selector nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 1000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os--write-bytes) (lambda (_ptr _str) nil))
              ((symbol-function 'nelisp-os-write-u32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-u32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (_dll fn _sig &rest args)
                 (cond
                  ((equal fn "GetStdHandle")
                   (setq selector (car args))
                   #x1234)
                  ((equal fn "WriteFile") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-write nelisp-os-STDERR "x") 1))))
    (should (= selector nelisp-os-WIN-STD-ERROR-HANDLE))))

(ert-deftest nelisp-stdlib-os-write-non-windows-keeps-libc-write ()
  "Non-Windows hosts still use the existing libc.write path."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os--write-bytes) (lambda (_ptr _str) nil))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 3)))
      (let ((system-type 'gnu/linux))
        (should (= (nelisp-os-write nelisp-os-STDOUT "abc") 3))))
    (should (equal call
                   (list "libc" "write"
                         [:sint64 :sint32 :pointer :uint64]
                         (list nelisp-os-STDOUT 3000 3))))))

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
