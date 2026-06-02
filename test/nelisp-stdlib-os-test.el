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

(ert-deftest nelisp-stdlib-os-error-condition-is-defined ()
  "`nelisp-os-error' is a catchable project-local condition."
  (should-error (signal 'nelisp-os-error (list 9))
                :type 'nelisp-os-error))

(ert-deftest nelisp-stdlib-os-windows-ffi-error-uses-getlasterror ()
  "Windows FFI failures report the raw kernel32 GetLastError value."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 12345)))
      (condition-case err
          (progn
            (nelisp-os--windows-ffi-error-signal)
            (ert-fail "expected nelisp-os-error"))
        (nelisp-os-error
         (should (equal (cdr err) '(12345))))))
    (should (equal call
                   (list "kernel32" "GetLastError" [:uint32] nil)))))

(ert-deftest nelisp-stdlib-os-exit-windows-uses-exitprocess ()
  "Windows process exit routes through kernel32 ExitProcess."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 :unreachable)))
      (let ((system-type 'windows-nt))
        (should (eq (nelisp-os-exit 42) :unreachable))))
    (should (equal call
                   (list "kernel32" "ExitProcess"
                         [:void :uint32]
                         (list 42))))))

(ert-deftest nelisp-stdlib-os-getpid-windows-uses-getcurrentprocessid ()
  "Windows getpid routes through kernel32 GetCurrentProcessId."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 98765)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getpid) 98765))))
    (should (equal call
                   (list "kernel32" "GetCurrentProcessId" [:uint32] nil)))))

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

(ert-deftest nelisp-stdlib-os-write-windows-non-std-fd-errors ()
  "Windows write on an unknown fd does not fall through to POSIX libc."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-write 99 "abc")
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-open-windows-uses-createfilew ()
  "On Windows, regular file open returns a POSIX-like fd backed by HANDLE."
  (let ((call nil)
        (freed nil)
        (wide-writes nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 4000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u16)
               (lambda (ptr off val)
                 (push (list ptr off val) wide-writes)
                 val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 #xabcdef)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-open "out.txt"
                                   (logior nelisp-os-O-CREAT
                                           nelisp-os-O-TRUNC
                                           nelisp-os-O-WRONLY)
                                   0)
                   3))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal call
                   (list "kernel32" "CreateFileW"
                         [:pointer :pointer :uint32 :uint32 :pointer
                          :uint32 :uint32 :pointer]
                         (list 4000
                               nelisp-os-WIN-GENERIC-WRITE
                               (logior nelisp-os-WIN-FILE-SHARE-READ
                                       nelisp-os-WIN-FILE-SHARE-WRITE
                                       nelisp-os-WIN-FILE-SHARE-DELETE)
                               0
                               nelisp-os-WIN-CREATE-ALWAYS
                               nelisp-os-WIN-FILE-ATTRIBUTE-NORMAL
                               0))))
    (should (equal (nreverse wide-writes)
                   '((4000 0 111)
                     (4000 2 117)
                     (4000 4 116)
                     (4000 6 46)
                     (4000 8 116)
                     (4000 10 120)
                     (4000 12 116)
                     (4000 14 0))))
    (should (equal freed '(4000)))))

(ert-deftest nelisp-stdlib-os-windows-utf16-code-units ()
  "Windows path conversion emits UTF-16 code units, including surrogate pairs."
  (should (equal (nelisp-os--windows-utf16-code-units
                  (string #x41 #x3042 #x10437))
                 '(#x41 #x3042 #xd801 #xdc37))))

(ert-deftest nelisp-stdlib-os-windows-open-flag-translation ()
  "Windows open flag translation preserves key POSIX-like modes."
  (should (= (nelisp-os--windows-open-access nelisp-os-O-RDONLY)
             nelisp-os-WIN-GENERIC-READ))
  (should (= (nelisp-os--windows-open-access nelisp-os-O-WRONLY)
             nelisp-os-WIN-GENERIC-WRITE))
  (should (= (nelisp-os--windows-open-access
              (logior nelisp-os-O-WRONLY nelisp-os-O-APPEND))
             nelisp-os-WIN-FILE-APPEND-DATA))
  (should (= (nelisp-os--windows-open-access
              (logior nelisp-os-O-RDONLY nelisp-os-O-APPEND))
             nelisp-os-WIN-GENERIC-READ))
  (should (= (nelisp-os--windows-open-access
              (logior nelisp-os-O-RDWR nelisp-os-O-APPEND))
             (logior nelisp-os-WIN-GENERIC-READ
                     nelisp-os-WIN-FILE-APPEND-DATA)))
  (should (= (nelisp-os--windows-open-disposition
              (logior nelisp-os-O-CREAT nelisp-os-O-EXCL))
             nelisp-os-WIN-CREATE-NEW))
  (should (= (nelisp-os--windows-open-disposition nelisp-os-O-TRUNC)
             nelisp-os-WIN-TRUNCATE-EXISTING))
  (should (= (nelisp-os--windows-open-disposition nelisp-os-O-RDONLY)
             nelisp-os-WIN-OPEN-EXISTING)))

(ert-deftest nelisp-stdlib-os-windows-page-protect-translation ()
  "Windows virtual-memory protection maps POSIX-like PROT bits."
  (should (= (nelisp-os--windows-page-protect nelisp-os-PROT-NONE)
             nelisp-os-WIN-PAGE-NOACCESS))
  (should (= (nelisp-os--windows-page-protect nelisp-os-PROT-READ)
             nelisp-os-WIN-PAGE-READONLY))
  (should (= (nelisp-os--windows-page-protect nelisp-os-PROT-WRITE)
             nelisp-os-WIN-PAGE-READWRITE))
  (should (= (nelisp-os--windows-page-protect
              (logior nelisp-os-PROT-READ nelisp-os-PROT-WRITE))
             nelisp-os-WIN-PAGE-READWRITE))
  (should (= (nelisp-os--windows-page-protect nelisp-os-PROT-EXEC)
             nelisp-os-WIN-PAGE-EXECUTE))
  (should (= (nelisp-os--windows-page-protect
              (logior nelisp-os-PROT-READ nelisp-os-PROT-EXEC))
             nelisp-os-WIN-PAGE-EXECUTE-READ))
  (should (= (nelisp-os--windows-page-protect
              (logior nelisp-os-PROT-READ
                      nelisp-os-PROT-WRITE
                      nelisp-os-PROT-EXEC))
             nelisp-os-WIN-PAGE-EXECUTE-READWRITE)))

(ert-deftest nelisp-stdlib-os-mmap-windows-uses-virtualalloc ()
  "Windows anonymous mmap routes through kernel32 VirtualAlloc."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 #x70000000)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-mmap
                    4096
                    (logior nelisp-os-PROT-READ nelisp-os-PROT-WRITE)
                    (logior nelisp-os-MAP-PRIVATE nelisp-os-MAP-ANONYMOUS)
                    -1
                    0)
                   #x70000000))))
    (should (equal call
                   (list "kernel32" "VirtualAlloc"
                         [:pointer :pointer :uint64 :uint32 :uint32]
                         (list 0
                               4096
                               (logior nelisp-os-WIN-MEM-COMMIT
                                       nelisp-os-WIN-MEM-RESERVE)
                               nelisp-os-WIN-PAGE-READWRITE))))))

(ert-deftest nelisp-stdlib-os-mmap-windows-rejects-fd-backed-mapping ()
  "Windows mmap branch only accepts anonymous mappings for now."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-mmap 4096 nelisp-os-PROT-READ
                                      nelisp-os-MAP-PRIVATE 3 0)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-mprotect-windows-uses-virtualprotect ()
  "Windows mprotect routes through kernel32 VirtualProtect."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 9000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-mprotect
                    #x70000000 4096
                    (logior nelisp-os-PROT-READ nelisp-os-PROT-EXEC))
                   0))))
    (should (equal call
                   (list "kernel32" "VirtualProtect"
                         [:sint32 :pointer :uint64 :uint32 :pointer]
                         (list #x70000000 4096
                               nelisp-os-WIN-PAGE-EXECUTE-READ
                               9000))))
    (should (equal freed '(9000)))))

(ert-deftest nelisp-stdlib-os-munmap-windows-uses-virtualfree ()
  "Windows munmap routes through kernel32 VirtualFree MEM_RELEASE."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-munmap #x70000000 4096) 0))))
    (should (equal call
                   (list "kernel32" "VirtualFree"
                         [:sint32 :pointer :uint64 :uint32]
                         (list #x70000000 0
                               nelisp-os-WIN-MEM-RELEASE))))))

(ert-deftest nelisp-stdlib-os-pipe-windows-uses-createpipe ()
  "Windows pipe routes through CreatePipe and records both HANDLEs."
  (let ((call nil)
        (alloc-next 1000)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr _off)
                 (cond
                  ((= ptr 1000) #x1111222233334444)
                  ((= ptr 2000) #x5555666677778888)
                  (t (error "unexpected pointer %S" ptr)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-pipe) '(3 . 4)))))
    (should (equal call
                   (list "kernel32" "CreatePipe"
                         [:sint32 :pointer :pointer :pointer :uint32]
                         (list 1000 2000 0 0))))
    (should (equal nelisp-os--windows-fd-table
                   '((4 . #x5555666677778888)
                     (3 . #x1111222233334444))))
    (should (equal (sort freed #'<) '(1000 2000)))))

(ert-deftest nelisp-stdlib-os-lseek-windows-uses-setfilepointerex ()
  "Windows lseek routes through SetFilePointerEx and returns new offset."
  (let ((call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #x445566778899aabb))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 123456))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-lseek 3 120 nelisp-os-SEEK-CUR) 123456))))
    (should (equal call
                   (list "kernel32" "SetFilePointerEx"
                         [:sint32 :pointer :sint64 :pointer :uint32]
                         (list #x445566778899aabb 120 3000
                               nelisp-os-SEEK-CUR))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-lseek-windows-invalid-whence-errors ()
  "Windows lseek rejects unsupported whence before calling FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #x1234))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-lseek 3 0 99)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fstat-windows-disk-uses-getfilesizeex ()
  "Windows fstat on disk HANDLE returns size and regular-file mode."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #x44556677))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 987654321))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-DISK)
                  ((equal fn "GetFileSizeEx") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-size st) 987654321))
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFREG))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x44556677))
                    (list "kernel32" "GetFileSizeEx"
                          [:sint32 :pointer :pointer]
                          (list #x44556677 3000)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fstat-windows-pipe-skips-getfilesizeex ()
  "Windows fstat on pipe HANDLE returns a zero-size non-regular stat."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((4 . #x55667788))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-PIPE)
                  (t (error "unexpected ffi call %S" fn))))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 4)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) 0))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x55667788)))))))

(ert-deftest nelisp-stdlib-os-read-windows-regular-fd-uses-readfile ()
  "Windows regular fd read uses the HANDLE table and ReadFile."
  (let ((calls nil)
        (alloc-next 1000)
        (freed nil)
        (zeroed nil)
        (read-request nil)
        (nelisp-os--windows-fd-table '((3 . #x44556677))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (setq zeroed (list ptr off val)) val))
              ((symbol-function 'nelisp-os-read-u32) (lambda (_ptr _off) 4))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (setq read-request (list ptr n))
                 "data"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 1)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-read 3 16) "data"))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "ReadFile"
                          [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                          (list #x44556677 1000 16 2000 0)))))
    (should (equal zeroed (list 2000 0 0)))
    (should (equal read-request (list 1000 4)))
    (should (equal (sort freed #'<) '(1000 2000)))))

(ert-deftest nelisp-stdlib-os-write-windows-regular-fd-uses-writefile ()
  "Windows regular fd write uses the HANDLE table and WriteFile."
  (let ((calls nil)
        (alloc-next 1000)
        (freed nil)
        (written-string nil)
        (nelisp-os--windows-fd-table '((3 . #x55667788))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written-string (cons ptr str))))
              ((symbol-function 'nelisp-os-write-u32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-u32) (lambda (_ptr _off) 4))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-write 3 "data") 4))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WriteFile"
                          [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                          (list #x55667788 1000 4 2000 0)))))
    (should (equal written-string (cons 1000 "data")))
    (should (equal (sort freed #'<) '(1000 2000)))))

(ert-deftest nelisp-stdlib-os-close-windows-regular-fd-uses-closehandle ()
  "Windows regular fd close removes the table entry and calls CloseHandle."
  (let ((call nil)
        (nelisp-os--windows-fd-table '((3 . #x778899aa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should-not (nelisp-os-close 3))))
    (should-not nelisp-os--windows-fd-table)
    (should (equal call
                   (list "kernel32" "CloseHandle"
                         [:sint32 :pointer]
                         (list #x778899aa))))))

(ert-deftest nelisp-stdlib-os-read-windows-stdin-uses-kernel32 ()
  "On Windows, stdin read routes through GetStdHandle + ReadFile."
  (let ((calls nil)
        (alloc-next 1000)
        (freed nil)
        (zeroed nil)
        (read-request nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val)
                 (setq zeroed (list ptr off val))
                 val))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (_ptr _off) 3))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (setq read-request (list ptr n))
                 "abc"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #x11223344)
                  ((equal fn "ReadFile") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-read nelisp-os-STDIN 8) "abc"))))
    (setq calls (nreverse calls))
    (should (equal (car calls)
                   (list "kernel32" "GetStdHandle"
                         [:pointer :sint32]
                         (list nelisp-os-WIN-STD-INPUT-HANDLE))))
    (should (equal (cadr calls)
                   (list "kernel32" "ReadFile"
                         [:sint32 :pointer :pointer :uint32 :pointer :pointer]
                         (list #x11223344 1000 8 2000 0))))
    (should (equal zeroed (list 2000 0 0)))
    (should (equal read-request (list 1000 3)))
    (should (equal (sort freed #'<) '(1000 2000)))))

(ert-deftest nelisp-stdlib-os-read-windows-stdin-empty-count-skips-ffi ()
  "A zero-byte Windows read returns immediately without FFI."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-read nelisp-os-STDIN 0) ""))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-read-non-windows-keeps-libc-read ()
  "Non-Windows hosts still use the existing libc.read path."
  (let ((call nil)
        (read-request nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (setq read-request (list ptr n))
                 "abc"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 3)))
      (let ((system-type 'gnu/linux))
        (should (equal (nelisp-os-read nelisp-os-STDIN 8) "abc"))))
    (should (equal call
                   (list "libc" "read"
                         [:sint64 :sint32 :pointer :uint64]
                         (list nelisp-os-STDIN 3000 8))))
    (should (equal read-request (list 3000 3)))))

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
