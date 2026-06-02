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

(ert-deftest nelisp-stdlib-os-windows-command-line-quotes-argv ()
  "Windows CreateProcess command-line helper quotes argv safely."
  (should (equal (nelisp-os--windows-command-line
                  "prog.exe"
                  '("prog.exe" "plain" "two words" "quote\"x" "trail\\"))
                 "prog.exe plain \"two words\" \"quote\\\"x\" trail\\")))

(ert-deftest nelisp-stdlib-os-windows-env-block-is-double-nul-terminated ()
  "Windows environment block helper emits NUL-separated entries."
  (should (equal (string-to-list
                  (nelisp-os--windows-env-block '("A=1" "B=2")))
                 '(65 61 49 0 66 61 50 0)))
  (should (equal (string-to-list (nelisp-os--windows-env-block nil))
                 '(0))))

(ert-deftest nelisp-stdlib-os-execve-windows-uses-createprocessw ()
  "Windows execve launches via CreateProcessW, closes handles, then exits."
  (let ((alloc-next 1000)
        (freed nil)
        (wide-writes nil)
        (startup-cb nil)
        (calls nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u16)
               (lambda (ptr off val)
                 (push (list ptr off val) wide-writes)
                 val))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val)
                 (setq startup-cb (list ptr off val))
                 val))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 5000))
                 (cond
                  ((= off nelisp-os-WIN-PROCESS-INFORMATION-HTHREAD-OFFSET)
                   #x1111)
                  ((= off nelisp-os-WIN-PROCESS-INFORMATION-HPROCESS-OFFSET)
                   #x2222)
                  (t (error "unexpected PROCESS_INFORMATION offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateProcessW") 1)
                  ((equal fn "CloseHandle") 1)
                  ((equal fn "ExitProcess") :exited)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (eq (nelisp-os-execve "prog.exe"
                                      '("prog.exe" "two words")
                                      '("A=1"))
                    :exited))))
    (should (equal startup-cb
                   (list 4000
                         nelisp-os-WIN-STARTUPINFOW-CB-OFFSET
                         nelisp-os-WIN-STARTUPINFOW-SIZE)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateProcessW"
                          [:sint32 :pointer :pointer :pointer :pointer :sint32
                           :uint32 :pointer :pointer :pointer :pointer]
                          (list 1000 2000 0 0 1 0 3000 0 4000 5000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #x1111))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #x2222))
                    (list "kernel32" "ExitProcess"
                          [:void :uint32]
                          (list 0)))))
    (should (equal (sort freed #'<) '(1000 2000 3000 4000 5000)))
    (should wide-writes)))

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

(ert-deftest nelisp-stdlib-os-network-byte-order-windows-uses-ws2-32 ()
  "Windows sockaddr byte-order helpers route through ws2_32."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig arg)
                 (push (list dll fn sig arg) calls)
                 (cond
                  ((equal fn "htons") #x3412)
                  ((equal fn "htonl") #x78563412)
                  ((equal fn "ntohs") #x1234)
                  ((equal fn "ntohl") #x12345678)
                  (t (error "unexpected byte-order call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os--htons #x1234) #x3412))
        (should (= (nelisp-os--htonl #x12345678) #x78563412))
        (should (= (nelisp-os--ntohs #x3412) #x1234))
        (should (= (nelisp-os--ntohl #x78563412) #x12345678))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "htons" [:uint16 :uint16] #x1234)
                    (list "ws2_32" "htonl" [:uint32 :uint32] #x12345678)
                    (list "ws2_32" "ntohs" [:uint16 :uint16] #x3412)
                    (list "ws2_32" "ntohl" [:uint32 :uint32] #x78563412))))))

(ert-deftest nelisp-stdlib-os-network-byte-order-non-windows-keeps-libc ()
  "Non-Windows sockaddr byte-order helpers keep using libc."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig arg)
                 (setq call (list dll fn sig arg))
                 #x3412)))
      (let ((system-type 'gnu/linux))
        (should (= (nelisp-os--htons #x1234) #x3412))))
    (should (equal call
                   (list "libc" "htons" [:uint16 :uint16] #x1234)))))

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

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-regular-fd ()
  "Windows dup2 duplicates a HANDLE and installs it in the fd table."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 6)
        (nelisp-os--windows-fd-table '((5 . #xcccc)
                                       (3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 #xbbbb))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcess") #x9999)
                  ((equal fn "DuplicateHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess"
                          [:pointer]
                          nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xcccc)))))
    (should (equal nelisp-os--windows-fd-table '((5 . #xbbbb) (3 . #xaaaa))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-can-target-stdout ()
  "Windows dup2 to stdout uses SetStdHandle for the duplicated HANDLE."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (_ptr _off) #xbbbb))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcess") #x9999)
                  ((equal fn "DuplicateHandle") 1)
                  ((equal fn "SetStdHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess"
                          [:pointer]
                          nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #xbbbb)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-handle ()
  "Windows F_DUPFD duplicates a HANDLE into the fd table at or above ARG."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 #xbbbb))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcess") #x9999)
                  ((equal fn "DuplicateHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess"
                          [:pointer]
                          nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS)))))
    (should (equal nelisp-os--windows-fd-table '((10 . #xbbbb) (3 . #xaaaa))))
    (should (= nelisp-os--windows-next-fd 11))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-getfl-and-setfl-noop ()
  "Windows F_GETFL returns 0 and F_SETFL 0 validates the fd as a no-op."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0) 0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFL 0) 0))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fcntl-windows-setfl-nonzero-errors ()
  "Windows F_SETFL rejects unsupported flag updates before FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-fcntl 3 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-linux-only-apis-windows-error-before-syscall ()
  "Linux-only event/process fd APIs reject Windows before raw syscall/libc."
  (let ((called nil)
        (forms
         (list
          (lambda () (nelisp-os-fork))
          (lambda () (nelisp-os-pidfd-open 1234 0))
          (lambda () (nelisp-os-pidfd-send-signal 3 nelisp-os-SIGTERM 0))
          (lambda () (nelisp-os-inotify-init 0))
          (lambda () (nelisp-os-inotify-add-watch 3 "x" nelisp-os-IN-ALL-EVENTS))
          (lambda () (nelisp-os-inotify-rm-watch 3 1))
          (lambda () (nelisp-os-inotify-read 3 1))
          (lambda () (nelisp-os-eventfd 0 0))
          (lambda () (nelisp-os-signalfd -1 (list nelisp-os-SIGTERM) 0))
          (lambda () (nelisp-os-signalfd-read 3 1))
          (lambda () (nelisp-os-sigprocmask nelisp-os-SIG-BLOCK
                                            (list nelisp-os-SIGTERM)))
          (lambda () (nelisp-os-timerfd-create 0 0))
          (lambda () (nelisp-os-timerfd-settime 3 0 0 0 1 0))
          (lambda () (nelisp-os-timerfd-gettime 3))
          (lambda () (nelisp-os-timerfd-set-relative-ms 3 10))
          (lambda () (nelisp-os-bind-unix-abstract 3 "x"))
          (lambda () (nelisp-os-connect-unix-abstract 3 "x"))
          (lambda () (nelisp-os-socketpair
                      nelisp-os-AF-UNIX nelisp-os-SOCK-STREAM 0))
          (lambda () (nelisp-os-sendmsg-fds 3 (list 4) "x"))
          (lambda () (nelisp-os-recvmsg-fds 3 1 1))
          (lambda () (nelisp-os-getsockopt-peercred 3)))))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (dolist (fn forms)
          (should-error (funcall fn) :type 'nelisp-os-error))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-socket-windows-uses-winsock ()
  "Windows AF_INET socket initializes Winsock and returns a socket fd."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-winsock-started-p nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WSAStartup") 0)
                  ((equal fn "socket") #xabcdef)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-INET
                                     nelisp-os-SOCK-STREAM
                                     nelisp-os-IPPROTO-TCP)
                   3))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "WSAStartup"
                          [:sint32 :uint16 :pointer]
                          (list nelisp-os-WIN-WINSOCK-VERSION-2-2 3000))
                    (list "ws2_32" "socket"
                          [:pointer :sint32 :sint32 :sint32]
                          (list nelisp-os-AF-INET
                                nelisp-os-SOCK-STREAM
                                nelisp-os-IPPROTO-TCP)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))
    (should nelisp-os--windows-winsock-started-p)
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-socket-windows-allows-af-inet6 ()
  "Windows AF_INET6 socket uses Winsock and returns a socket fd."
  (let ((call nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 #xabcdef)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-INET6
                                     nelisp-os-SOCK-STREAM
                                     nelisp-os-IPPROTO-TCP)
                   3))))
    (should (equal call
                   (list "ws2_32" "socket"
                         [:pointer :sint32 :sint32 :sint32]
                         (list nelisp-os-AF-INET6
                               nelisp-os-SOCK-STREAM
                               nelisp-os-IPPROTO-TCP))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))))

(ert-deftest nelisp-stdlib-os-socket-windows-allows-af-unix ()
  "Windows AF_UNIX socket uses Winsock and returns a socket fd."
  (let ((call nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 #xabcdef)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-UNIX
                                     nelisp-os-SOCK-STREAM
                                     0)
                   3))))
    (should (equal call
                   (list "ws2_32" "socket"
                         [:pointer :sint32 :sint32 :sint32]
                         (list nelisp-os-AF-UNIX
                               nelisp-os-SOCK-STREAM
                               0))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))))

(ert-deftest nelisp-stdlib-os-socket-windows-rejects-linux-socket-flags ()
  "Windows socket rejects SOCK_NONBLOCK/SOCK_CLOEXEC before Winsock FFI."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socket nelisp-os-AF-INET
                           (logior nelisp-os-SOCK-STREAM
                                   nelisp-os-SOCK-NONBLOCK)
                           nelisp-os-IPPROTO-TCP)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-uses-winsock ()
  "Windows int-valued socket options translate to Winsock setsockopt."
  (let ((call nil)
        (write nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-REUSEADDR 1)
                   0))))
    (should (equal write (list 3000 0 1)))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-SOL-SOCKET
                               nelisp-os-WIN-SO-REUSEADDR
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-rejects-non-socket-fd-before-ffi ()
  "Windows setsockopt rejects regular HANDLE fds before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-SOL-SOCKET nelisp-os-SO-KEEPALIVE 1)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-uses-winsock ()
  "Windows int-valued socket options translate to Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val)
                 (push (list ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-KEEPALIVE)
                   1))))
    (should (equal (nreverse writes) '((4000 0 4))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-SOL-SOCKET
                               nelisp-os-WIN-SO-KEEPALIVE
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-rejects-non-socket-fd-before-ffi ()
  "Windows getsockopt rejects regular HANDLE fds before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-getsockopt-int
          3 nelisp-os-SOL-SOCKET nelisp-os-SO-REUSEADDR)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-bind-inet-windows-uses-winsock ()
  "Windows AF_INET bind passes encoded sockaddr_in to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-bind-inet 3 nelisp-os-INADDR-LOOPBACK 4444) 0))))
    (should (equal encoded (list 3000 nelisp-os-INADDR-LOOPBACK 4444)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "bind"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 nelisp-os--sockaddr-in-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-connect-inet-windows-uses-winsock ()
  "Windows AF_INET connect passes encoded sockaddr_in to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-connect-inet 3 nelisp-os-INADDR-LOOPBACK 4444) 0))))
    (should (equal encoded (list 3000 nelisp-os-INADDR-LOOPBACK 4444)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "connect"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 nelisp-os--sockaddr-in-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-listen-windows-uses-winsock ()
  "Windows listen dispatches to Winsock for socket-kind fds."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-listen 3 8) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "listen"
                          [:sint32 :pointer :sint32]
                          (list #xabcdef 8)))))))

(ert-deftest nelisp-stdlib-os-listen-windows-rejects-non-socket-fd-before-ffi ()
  "Windows listen rejects regular HANDLE fds before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-listen 3 8) :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-accept-inet-windows-uses-winsock ()
  "Windows accept returns a socket-kind fd plus decoded peer address."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-in-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (ptr)
                 (should (= ptr 3000))
                 (cons nelisp-os-INADDR-LOOPBACK 4444)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 #x123456)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-accept-inet 3)
                       (list 4 nelisp-os-INADDR-LOOPBACK 4444)))))
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "accept"
                          [:pointer :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal nelisp-os--windows-fd-table
                   '((4 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((4 . socket) (3 . socket))))
    (should (= nelisp-os--windows-next-fd 5))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-bind-inet6-windows-uses-winsock ()
  "Windows AF_INET6 bind passes encoded sockaddr_in6 to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-bind-inet6 3 nelisp-os-IN6ADDR-LOOPBACK 4444) 0))))
    (should (equal encoded (list 3000 nelisp-os-IN6ADDR-LOOPBACK 4444)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "bind"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 nelisp-os--sockaddr-in6-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-connect-inet6-windows-uses-winsock ()
  "Windows AF_INET6 connect passes encoded sockaddr_in6 to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-connect-inet6 3 nelisp-os-IN6ADDR-LOOPBACK 4444) 0))))
    (should (equal encoded (list 3000 nelisp-os-IN6ADDR-LOOPBACK 4444)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "connect"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 nelisp-os--sockaddr-in6-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-accept-inet6-windows-uses-winsock ()
  "Windows AF_INET6 accept returns a socket-kind fd plus decoded peer."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-in6-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6)
               (lambda (ptr)
                 (should (= ptr 3000))
                 (cons nelisp-os-IN6ADDR-LOOPBACK 4444)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 #x123456)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-accept-inet6 3)
                       (list 4 nelisp-os-IN6ADDR-LOOPBACK 4444)))))
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in6-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "accept"
                          [:pointer :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal nelisp-os--windows-fd-table
                   '((4 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((4 . socket) (3 . socket))))
    (should (= nelisp-os--windows-next-fd 5))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-bind-unix-windows-uses-winsock ()
  "Windows AF_UNIX filesystem bind passes encoded sockaddr_un to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-un)
               (lambda (buf path) (setq encoded (list buf path)) 9))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-bind-unix 3 "sockpath") 0))))
    (should (equal encoded (list 3000 "sockpath")))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "bind"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 9)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-connect-unix-windows-uses-winsock ()
  "Windows AF_UNIX filesystem connect passes encoded sockaddr_un to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-un)
               (lambda (buf path) (setq encoded (list buf path)) 9))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-connect-unix 3 "sockpath") 0))))
    (should (equal encoded (list 3000 "sockpath")))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "connect"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef 3000 9)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-accept-unix-windows-uses-winsock ()
  "Windows AF_UNIX accept returns a socket-kind fd plus decoded peer path."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-un-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 4000))
                 (should (= off 0))
                 7))
              ((symbol-function 'nelisp-os-read-u8)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (if (and (>= off 2) (<= off 6)) ?x 0)))
              ((symbol-function 'nelisp-os--read-bytes-at)
               (lambda (ptr off n)
                 (should (= ptr 3000))
                 (should (= off 2))
                 (should (= n 5))
                 "sockp"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 #x123456)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-accept-unix 3)
                       (cons 4 "sockp")))))
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-un-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "accept"
                          [:pointer :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal nelisp-os--windows-fd-table
                   '((4 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((4 . socket) (3 . socket))))
    (should (= nelisp-os--windows-next-fd 5))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockname-inet-windows-uses-winsock ()
  "Windows IPv4 getsockname uses Winsock and decodes sockaddr_in."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (decoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-in-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (ptr)
                 (should (= ptr 3000))
                 (setq decoded t)
                 (cons nelisp-os-INADDR-LOOPBACK 4444)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getsockname-inet 3)
                       (list nelisp-os-INADDR-LOOPBACK 4444)))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockname"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getpeername-inet6-windows-uses-winsock ()
  "Windows IPv6 getpeername uses Winsock and decodes sockaddr_in6."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (decoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-in6-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6)
               (lambda (ptr)
                 (should (= ptr 3000))
                 (setq decoded t)
                 (cons nelisp-os-IN6ADDR-LOOPBACK 4444)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getpeername-inet6 3)
                       (list nelisp-os-IN6ADDR-LOOPBACK 4444)))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in6-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getpeername"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockname-unix-windows-uses-winsock ()
  "Windows AF_UNIX getsockname uses Winsock and decodes sockaddr_un."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (len-read nil)
        (decoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-un-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (setq len-read (list ptr off))
                 9))
              ((symbol-function 'nelisp-os--decode-sockaddr-un)
               (lambda (ptr socklen)
                 (should (= ptr 3000))
                 (should (= socklen 9))
                 (setq decoded t)
                 "sockpath"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getsockname-unix 3) "sockpath"))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-un-len)))
    (should (equal len-read (list 4000 0)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockname"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-bind-inet6-scoped-windows-uses-winsock ()
  "Windows scoped AF_INET6 bind passes encoded sockaddr_in6 to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6-scoped)
               (lambda (buf host port flowinfo scope-id)
                 (setq encoded (list buf host port flowinfo scope-id))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-bind-inet6-scoped
                    3 nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)
                   0))))
    (should (equal encoded
                   (list 3000 nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "bind"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef
                                3000
                                nelisp-os--sockaddr-in6-scoped-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-connect-inet6-scoped-windows-uses-winsock ()
  "Windows scoped AF_INET6 connect passes encoded sockaddr_in6 to Winsock."
  (let ((calls nil)
        (freed nil)
        (encoded nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6-scoped)
               (lambda (buf host port flowinfo scope-id)
                 (setq encoded (list buf host port flowinfo scope-id))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-connect-inet6-scoped
                    3 nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)
                   0))))
    (should (equal encoded
                   (list 3000 nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "connect"
                          [:sint32 :pointer :pointer :sint32]
                          (list #xabcdef
                                3000
                                nelisp-os--sockaddr-in6-scoped-len)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-accept-inet6-scoped-windows-uses-winsock ()
  "Windows scoped AF_INET6 accept returns socket-kind fd plus decoded peer."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (decoded (list nelisp-os-IN6ADDR-LOOPBACK 4444 9 2))
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n)
                 (if (= n nelisp-os--sockaddr-in6-scoped-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (setq len-write (list ptr off val)) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6-scoped)
               (lambda (ptr)
                 (should (= ptr 3000))
                 decoded))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 #x123456)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-accept-inet6-scoped 3)
                       (cons 4 decoded)))))
    (should (equal len-write
                   (list 4000 0 nelisp-os--sockaddr-in6-scoped-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "accept"
                          [:pointer :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal nelisp-os--windows-fd-table
                   '((4 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((4 . socket) (3 . socket))))
    (should (= nelisp-os--windows-next-fd 5))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-kill-windows-uses-terminateprocess ()
  "Windows kill terminates a single PID through kernel32 process APIs."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "OpenProcess") #xabcdef)
                  ((equal fn "TerminateProcess") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-kill 1234 nelisp-os-SIGTERM) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "OpenProcess"
                          [:pointer :uint32 :sint32 :uint32]
                          (list nelisp-os-WIN-PROCESS-TERMINATE 0 1234))
                    (list "kernel32" "TerminateProcess"
                          [:sint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-SIGTERM))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))))

(ert-deftest nelisp-stdlib-os-kill-windows-invalid-signal-errors ()
  "Windows kill rejects unsupported signals before FFI."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-kill 1234 nelisp-os-SIGHUP)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-getppid-windows-uses-toolhelp-snapshot ()
  "Windows getppid walks PROCESSENTRY32W records and returns parent PID."
  (let ((calls nil)
        (freed nil)
        (dwsize-write nil)
        (entry-index nil)
        (entries '((111 . 7) (222 . 44))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val)
                 (setq dwsize-write (list ptr off val))
                 val))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (let ((entry (nth entry-index entries)))
                   (cond
                    ((= off nelisp-os-WIN-PROCESSENTRY32W-PID-OFFSET)
                     (car entry))
                    ((= off nelisp-os-WIN-PROCESSENTRY32W-PPID-OFFSET)
                     (cdr entry))
                    (t (error "unexpected PROCESSENTRY32W offset %S" off))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcessId") 222)
                  ((equal fn "CreateToolhelp32Snapshot") #xaaaa)
                  ((equal fn "Process32FirstW") (setq entry-index 0) 1)
                  ((equal fn "Process32NextW") (setq entry-index 1) 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getppid) 44))))
    (should (equal dwsize-write
                   (list 3000
                         nelisp-os-WIN-PROCESSENTRY32W-DWSIZE-OFFSET
                         nelisp-os-WIN-PROCESSENTRY32W-SIZE)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcessId" [:uint32] nil)
                    (list "kernel32" "CreateToolhelp32Snapshot"
                          [:pointer :uint32 :uint32]
                          (list nelisp-os-WIN-TH32CS-SNAPPROCESS 0))
                    (list "kernel32" "Process32FirstW"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    (list "kernel32" "Process32NextW"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xaaaa)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-getppid-windows-missing-current-pid-errors ()
  "Windows getppid signals ESRCH when the snapshot lacks the current PID."
  (let ((calls nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32) (lambda (&rest _args) nil))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (_ptr off)
                 (if (= off nelisp-os-WIN-PROCESSENTRY32W-PID-OFFSET)
                     111
                   7)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcessId") 222)
                  ((equal fn "CreateToolhelp32Snapshot") #xaaaa)
                  ((equal fn "Process32FirstW") 1)
                  ((equal fn "Process32NextW") 0)
                  ((equal fn "GetLastError") nelisp-os-WIN-ERROR-NO-MORE-FILES)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-getppid) :type 'nelisp-os-error)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcessId" [:uint32] nil)
                    (list "kernel32" "CreateToolhelp32Snapshot"
                          [:pointer :uint32 :uint32]
                          (list nelisp-os-WIN-TH32CS-SNAPPROCESS 0))
                    (list "kernel32" "Process32FirstW"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    (list "kernel32" "Process32NextW"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    (list "kernel32" "GetLastError" [:uint32] nil)
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xaaaa)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-wait-windows-uses-waitforsingleobject ()
  "Windows wait opens a process, waits, reads exit code, and closes it."
  (let ((calls nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 7))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "OpenProcess") #xabcdef)
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-OBJECT-0)
                  ((equal fn "GetExitCodeProcess") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait 1234 0)
                       (cons 1234 (ash 7 8))))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "OpenProcess"
                          [:pointer :uint32 :sint32 :uint32]
                          (list (logior nelisp-os-WIN-SYNCHRONIZE
                                        nelisp-os-WIN-PROCESS-QUERY-LIMITED-INFORMATION)
                                0 1234))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-WIN-INFINITE))
                    (list "kernel32" "GetExitCodeProcess"
                          [:sint32 :pointer :pointer]
                          (list #xabcdef 3000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-wait-windows-wnohang-timeout ()
  "Windows WNOHANG wait returns (0 . 0) on timeout."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "OpenProcess") #xabcdef)
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-TIMEOUT)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait 1234 nelisp-os-WNOHANG)
                       '(0 . 0)))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "OpenProcess"
                          [:pointer :uint32 :sint32 :uint32]
                          (list (logior nelisp-os-WIN-SYNCHRONIZE
                                        nelisp-os-WIN-PROCESS-QUERY-LIMITED-INFORMATION)
                                0 1234))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))))

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

(ert-deftest nelisp-stdlib-os-read-windows-socket-fd-uses-recv ()
  "Windows socket fd read uses Winsock recv."
  (let ((calls nil)
        (freed nil)
        (read-request nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (setq read-request (list ptr n))
                 "data"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-read 3 16) "data"))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "recv"
                          [:sint32 :pointer :pointer :sint32 :sint32]
                          (list #xabcdef 3000 16 0)))))
    (should (equal read-request (list 3000 4)))
    (should (equal freed '(3000)))))

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

(ert-deftest nelisp-stdlib-os-write-windows-socket-fd-uses-send ()
  "Windows socket fd write uses Winsock send."
  (let ((calls nil)
        (freed nil)
        (written-string nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written-string (cons ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-write 3 "data") 4))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "send"
                          [:sint32 :pointer :pointer :sint32 :sint32]
                          (list #xabcdef 3000 4 0)))))
    (should (equal written-string (cons 3000 "data")))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-poll-windows-socket-fds-use-wsapoll ()
  "Windows poll writes WSAPOLLFD records and decodes revents."
  (let ((call nil)
        (freed nil)
        (writes nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa) (4 . #xbbbb)))
        (nelisp-os--windows-fd-kind-table '((3 . socket) (4 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list 'i64 ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os-write-i16)
               (lambda (ptr off val)
                 (push (list 'i16 ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os-read-i16)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (cond
                  ((= off 10) nelisp-os-POLLIN)
                  ((= off 26) nelisp-os-POLLOUT)
                  (t (error "unexpected WSAPOLLFD revents offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 2)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-poll
                        (list (cons 3 nelisp-os-POLLIN)
                              (cons 4 nelisp-os-POLLOUT))
                        50)
                       (list (cons 3 nelisp-os-POLLIN)
                             (cons 4 nelisp-os-POLLOUT))))))
    (should (equal call
                   (list "ws2_32" "WSAPoll"
                         [:sint32 :pointer :uint32 :sint32]
                         (list 3000 2 50))))
    (should (equal (nreverse writes)
                   (list
                    (list 'i64 3000 0 #xaaaa)
                    (list 'i16 3000 8 nelisp-os-POLLIN)
                    (list 'i64 3000 16 #xbbbb)
                    (list 'i16 3000 24 nelisp-os-POLLOUT))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-poll-windows-rejects-non-socket-fd-before-ffi ()
  "Windows poll rejects regular HANDLE fds before calling WSAPoll."
  (let ((called nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                      :type 'nelisp-os-error)))
    (should-not called)
    (should (equal freed '(3000)))))

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

(ert-deftest nelisp-stdlib-os-close-windows-socket-fd-uses-closesocket ()
  "Windows socket fd close removes fd tables and calls closesocket."
  (let ((call nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should-not (nelisp-os-close 3))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should (equal call
                   (list "ws2_32" "closesocket"
                         [:sint32 :pointer]
                         (list #xabcdef))))))

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
