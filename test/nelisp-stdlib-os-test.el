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

(defun nelisp-stdlib-os-test--windows-cells (count)
  "Return COUNT synthetic Windows ID . HANDLE cells."
  (let (cells)
    (dotimes (idx count)
      (push (cons (1+ idx) (+ #x1000 idx)) cells))
    (nreverse cells)))

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

(ert-deftest nelisp-stdlib-os-windows-winsock-error-maps-posix-errno ()
  "Winsock WSAE* errors are reported as POSIX errno payloads."
  (let ((call nil))
    (should (= (nelisp-os--windows-winsock-error-code->errno
                nelisp-os-WIN-WSAEWOULDBLOCK)
               11))
    (should (= (nelisp-os--windows-winsock-error-code->errno
                nelisp-os-WIN-WSAECONNRESET)
               104))
    (should (= (nelisp-os--windows-winsock-error-code->errno 12345)
               12345))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 nelisp-os-WIN-WSAECONNREFUSED)))
      (condition-case err
          (progn
            (nelisp-os--windows-winsock-error-signal)
            (ert-fail "expected nelisp-os-error"))
        (nelisp-os-error
         (should (equal (cdr err) '(111))))))
    (should (equal call
                   (list "ws2_32" "WSAGetLastError" [:sint32] nil)))))

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
        (startup-u32-writes nil)
        (startup-handle-writes nil)
        (calls nil)
        (nelisp-os--windows-process-table nil))
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
                 (push (list ptr off val) startup-u32-writes)
                 val))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) startup-handle-writes)
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
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 5000))
                 (if (= off nelisp-os-WIN-PROCESS-INFORMATION-DWPROCESSID-OFFSET)
                     4321
                   (error "unexpected PROCESS_INFORMATION offset %S" off))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateProcessW") 1)
                  ((equal fn "GetStdHandle")
                   (pcase (car args)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-INPUT-HANDLE)))
                      #x1010)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-OUTPUT-HANDLE)))
                      #x2020)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-ERROR-HANDLE)))
                      #x3030)
                     (_ (error "unexpected GetStdHandle selector %S" args))))
                  ((equal fn "CloseHandle") 1)
                  ((equal fn "ExitProcess") :exited)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (eq (nelisp-os-execve "prog.exe"
                                      '("prog.exe" "two words")
                                      '("A=1"))
                    :exited))))
    (should (equal (nreverse startup-u32-writes)
                   (list
                    (list 4000
                          nelisp-os-WIN-STARTUPINFOW-CB-OFFSET
                          nelisp-os-WIN-STARTUPINFOW-SIZE)
                    (list 4000
                          nelisp-os-WIN-STARTUPINFOW-DWFLAGS-OFFSET
                          nelisp-os-WIN-STARTF-USESTDHANDLES))))
    (should (equal (nreverse startup-handle-writes)
                   (list
                    (list 4000
                          nelisp-os-WIN-STARTUPINFOW-HSTDINPUT-OFFSET
                          #x1010)
                    (list 4000
                          nelisp-os-WIN-STARTUPINFOW-HSTDOUTPUT-OFFSET
                          #x2020)
                    (list 4000
                          nelisp-os-WIN-STARTUPINFOW-HSTDERROR-OFFSET
                          #x3030))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-INPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-ERROR-HANDLE))
                    (list "kernel32" "CreateProcessW"
                          [:sint32 :pointer :pointer :pointer :pointer :sint32
                           :uint32 :pointer :pointer :pointer :pointer]
                          (list 1000 2000 0 0 1 0 3000 0 4000 5000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #x1111))
                    (list "kernel32" "ExitProcess"
                          [:void :uint32]
                          (list 0)))))
    (should (equal (sort freed #'<) '(1000 2000 3000 4000 5000)))
    (should (equal nelisp-os--windows-process-table
                   '((4321 . #x2222))))
    (should wide-writes)))

(ert-deftest nelisp-stdlib-os-windows-create-process-registers-child-handle ()
  "Windows CreateProcess helper returns PID and registers the process HANDLE."
  (let ((alloc-next 1000)
        (freed nil)
        (wide-writes nil)
        (u32-writes nil)
        (i64-writes nil)
        (calls nil)
        (nelisp-os--windows-process-table nil))
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
                 (push (list ptr off val) u32-writes)
                 val))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) i64-writes)
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
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 5000))
                 (if (= off nelisp-os-WIN-PROCESS-INFORMATION-DWPROCESSID-OFFSET)
                     4321
                   (error "unexpected PROCESS_INFORMATION offset %S" off))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle")
                   (pcase (car args)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-INPUT-HANDLE)))
                      #x1010)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-OUTPUT-HANDLE)))
                      #x2020)
                     ((pred (lambda (selector)
                              (= selector nelisp-os-WIN-STD-ERROR-HANDLE)))
                      #x3030)
                     (_ (error "unexpected GetStdHandle selector %S" args))))
                  ((equal fn "CreateProcessW") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (= (nelisp-os--windows-create-process
                  "prog.exe"
                  '("prog.exe" "two words")
                  '("A=1"))
                 4321)))
    (should (equal nelisp-os--windows-process-table
                   '((4321 . #x2222))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-INPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-ERROR-HANDLE))
                    (list "kernel32" "CreateProcessW"
                          [:sint32 :pointer :pointer :pointer :pointer :sint32
                           :uint32 :pointer :pointer :pointer :pointer]
                          (list 1000 2000 0 0 1 0 3000 0 4000 5000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #x1111)))))
    (should (equal (sort freed #'<) '(1000 2000 3000 4000 5000)))
    (should wide-writes)
    (should u32-writes)
    (should i64-writes)))

(ert-deftest nelisp-stdlib-os-windows-create-thread-registers-handle ()
  "Windows CreateThread helper returns TID and registers the thread HANDLE."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-thread-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 77))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateThread") #xabcdef)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (= (nelisp-os--windows-create-thread #x11112222 #x33334444)
                 77)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateThread"
                          [:pointer :pointer :uint64 :pointer :pointer :uint32
                           :pointer]
                          (list 0 0 #x11112222 #x33334444 0 3000)))))
    (should (equal nelisp-os--windows-thread-table
                   '((77 . #xabcdef))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-windows-join-thread-completes-and-forgets ()
  "Windows thread join waits, reads exit code, closes, and forgets HANDLE."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-thread-table '((77 . #xabcdef))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 12))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-OBJECT-0)
                  ((equal fn "GetExitCodeThread") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (equal (nelisp-os--windows-join-thread 77 0)
                     '(77 . 12))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-WIN-INFINITE))
                    (list "kernel32" "GetExitCodeThread"
                          [:sint32 :pointer :pointer]
                          (list #xabcdef 3000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))
    (should-not nelisp-os--windows-thread-table)
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-windows-join-thread-wnohang-keeps-handle ()
  "Windows thread join WNOHANG keeps registered HANDLE on timeout."
  (let ((calls nil)
        (called-alloc nil)
        (nelisp-os--windows-thread-table '((77 . #xabcdef))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called-alloc t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-TIMEOUT)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (equal (nelisp-os--windows-join-thread
                      77 nelisp-os-WNOHANG)
                     '(0 . 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0)))))
    (should (equal nelisp-os--windows-thread-table
                   '((77 . #xabcdef))))
    (should-not called-alloc)))

(ert-deftest nelisp-stdlib-os-windows-join-any-thread-completes-one ()
  "Windows thread join-any reaps one registered thread."
  (let ((calls nil)
        (freed nil)
        (handle-writes nil)
        (alloc-next 3000)
        (nelisp-os--windows-thread-table '((77 . #xaaaa) (88 . #xbbbb))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) handle-writes)
                 val))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 4000))
                 (should (= off 0))
                 13))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForMultipleObjects")
                   (+ nelisp-os-WIN-WAIT-OBJECT-0 1))
                  ((equal fn "GetExitCodeThread") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (equal (nelisp-os--windows-join-any-thread 0)
                     '(88 . 13))))
    (should (equal (nreverse handle-writes)
                   (list
                    (list 3000 0 #xaaaa)
                    (list 3000 8 #xbbbb))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForMultipleObjects"
                          [:uint32 :uint32 :pointer :sint32 :uint32]
                          (list 2 3000 0 nelisp-os-WIN-INFINITE))
                    (list "kernel32" "GetExitCodeThread"
                          [:sint32 :pointer :pointer]
                          (list #xbbbb 4000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal nelisp-os--windows-thread-table
                   '((77 . #xaaaa))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-windows-join-any-thread-wnohang-keeps-all ()
  "Windows thread join-any WNOHANG keeps all registered threads on timeout."
  (let ((calls nil)
        (freed nil)
        (handle-writes nil)
        (nelisp-os--windows-thread-table '((77 . #xaaaa) (88 . #xbbbb))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) handle-writes)
                 val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForMultipleObjects")
                   nelisp-os-WIN-WAIT-TIMEOUT)
                  (t (error "unexpected ffi call %S" fn))))))
      (should (equal (nelisp-os--windows-join-any-thread nelisp-os-WNOHANG)
                     '(0 . 0))))
    (should (equal (nreverse handle-writes)
                   (list
                    (list 3000 0 #xaaaa)
                    (list 3000 8 #xbbbb))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForMultipleObjects"
                          [:uint32 :uint32 :pointer :sint32 :uint32]
                          (list 2 3000 0 0)))))
    (should (equal nelisp-os--windows-thread-table
                   '((77 . #xaaaa) (88 . #xbbbb))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-windows-join-any-thread-empty-errors ()
  "Windows thread join-any reports ECHILD when no thread HANDLE is registered."
  (let ((called nil)
        (nelisp-os--windows-thread-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (should-error (nelisp-os--windows-join-any-thread 0)
                    :type 'nelisp-os-error))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-windows-join-any-thread-too-many-errors ()
  "Windows thread join-any rejects HANDLE counts above the native wait limit."
  (let ((called nil)
        (nelisp-os--windows-thread-table
         (nelisp-stdlib-os-test--windows-cells
          (1+ nelisp-os-WIN-MAXIMUM-WAIT-OBJECTS))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (should-error (nelisp-os--windows-join-any-thread 0)
                    :type 'nelisp-os-error))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-windows-std-handle-selectors ()
  "POSIX-like std fds map to Windows GetStdHandle selector constants."
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDIN)
             nelisp-os-WIN-STD-INPUT-HANDLE))
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDOUT)
             nelisp-os-WIN-STD-OUTPUT-HANDLE))
  (should (= (nelisp-os--windows-std-handle-selector nelisp-os-STDERR)
             nelisp-os-WIN-STD-ERROR-HANDLE))
  (should-not (nelisp-os--windows-std-handle-selector 99)))

(ert-deftest nelisp-stdlib-os-windows-startup-std-handles-skip-when-unavailable ()
  "STARTUPINFOW std handles are optional when a host has no std HANDLE."
  (let ((calls nil)
        (u32-writes nil)
        (i64-writes nil))
    (cl-letf (((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val)
                 (push (list ptr off val) u32-writes)
                 val))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) i64-writes)
                 val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle")
                   (if (= (car args) nelisp-os-WIN-STD-OUTPUT-HANDLE)
                       0
                     #x1111))
                  ((equal fn "GetLastError") 6)
                  (t (error "unexpected ffi call %S" fn))))))
      (nelisp-os--windows-fill-startup-std-handles 4000))
    (should-not u32-writes)
    (should-not i64-writes)
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-INPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetLastError"
                          [:uint32]
                          nil)
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-ERROR-HANDLE)))))))

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
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-flags-table nil))
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
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-WRONLY))))
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

(ert-deftest nelisp-stdlib-os-open-windows-supports-o-cloexec ()
  "Windows open O_CLOEXEC clears HANDLE_FLAG_INHERIT after CreateFileW."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 4000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u16) (lambda (&rest _args) nil))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateFileW") #xabcdef)
                  ((equal fn "SetHandleInformation") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-open "out.txt"
                                   (logior nelisp-os-O-CREAT
                                           nelisp-os-O-TRUNC
                                           nelisp-os-O-WRONLY
                                           nelisp-os-O-CLOEXEC)
                                   0)
                   3))))
    (should (equal (nreverse calls)
                   (list
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
                                0))
                    (list "kernel32" "SetHandleInformation"
                          [:sint32 :pointer :uint32 :uint32]
                          (list #xabcdef
                                nelisp-os-WIN-HANDLE-FLAG-INHERIT
                                0)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-WRONLY))))
    (should (equal freed '(4000)))))

(ert-deftest nelisp-stdlib-os-open-windows-tracks-status-flags-for-getfl ()
  "Windows regular open records access and O_APPEND status for F_GETFL."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 4000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-u16) (lambda (&rest _args) nil))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 #xabcdef)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-open "append.txt"
                                   (logior nelisp-os-O-RDWR
                                           nelisp-os-O-APPEND)
                                   0)
                   3))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   (logior nelisp-os-O-RDWR nelisp-os-O-APPEND)))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateFileW"
                          [:pointer :pointer :uint32 :uint32 :pointer
                           :uint32 :uint32 :pointer]
                          (list 4000
                                (logior nelisp-os-WIN-GENERIC-READ
                                        nelisp-os-WIN-FILE-APPEND-DATA)
                                (logior nelisp-os-WIN-FILE-SHARE-READ
                                        nelisp-os-WIN-FILE-SHARE-WRITE
                                        nelisp-os-WIN-FILE-SHARE-DELETE)
                                0
                                nelisp-os-WIN-OPEN-EXISTING
                                nelisp-os-WIN-FILE-ATTRIBUTE-NORMAL
                                0)))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,(logior nelisp-os-O-RDWR
                                   nelisp-os-O-APPEND)))))))

(ert-deftest nelisp-stdlib-os-open-windows-o-cloexec-cleans-up-on-setfd-error ()
  "Windows open O_CLOEXEC closes the new fd when inheritability update fails."
  (let ((closed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 4000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-u16) (lambda (&rest _args) nil))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (_dll fn _sig &rest _args)
                 (if (equal fn "CreateFileW")
                     #xabcdef
                   (error "unexpected ffi call %S" fn))))
              ((symbol-function 'nelisp-os--windows-setfd-flags)
               (lambda (_fd _flags)
                 (signal 'nelisp-os-error (list 10022))))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push fd closed)
                 0)))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-open "out.txt"
                         (logior nelisp-os-O-CREAT
                                 nelisp-os-O-WRONLY
                                 nelisp-os-O-CLOEXEC)
                         0)
         :type 'nelisp-os-error)))
    (should (equal closed '(3)))))

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
  (let ((call nil)
        (nelisp-os--windows-mmap-table nil))
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
                               nelisp-os-WIN-PAGE-READWRITE))))
    (should (equal nelisp-os--windows-mmap-table
                   '((#x70000000 . virtualalloc))))))

(ert-deftest nelisp-stdlib-os-mmap-windows-file-backed-uses-mapviewoffile ()
  "Windows file-backed mmap routes through CreateFileMappingW + MapViewOfFile."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-mmap-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateFileMappingW") #xbbbb)
                  ((equal fn "MapViewOfFile") #x71000000)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-mmap 4096 nelisp-os-PROT-READ
                                   nelisp-os-MAP-PRIVATE 3 #x1000)
                   #x71000000))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateFileMappingW"
                          [:pointer :pointer :pointer :uint32 :uint32 :uint32 :pointer]
                          (list #xaaaa 0 nelisp-os-WIN-PAGE-READONLY
                                0 #x2000 0))
                    (list "kernel32" "MapViewOfFile"
                          [:pointer :pointer :uint32 :uint32 :uint32 :uint64]
                          (list #xbbbb nelisp-os-WIN-FILE-MAP-READ
                                0 #x1000 4096))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal nelisp-os--windows-mmap-table
                   '((#x71000000 . mapped-file))))))

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
  (let ((call nil)
        (nelisp-os--windows-mmap-table nil))
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

(ert-deftest nelisp-stdlib-os-munmap-windows-file-view-uses-unmapviewoffile ()
  "Windows munmap releases file-backed views through UnmapViewOfFile."
  (let ((call nil)
        (nelisp-os--windows-mmap-table '((#x71000000 . mapped-file))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-munmap #x71000000 4096) 0))))
    (should (equal call
                   (list "kernel32" "UnmapViewOfFile"
                         [:sint32 :pointer]
                         (list #x71000000))))
    (should-not nelisp-os--windows-mmap-table)))

(ert-deftest nelisp-stdlib-os-munmap-windows-virtualfree-failure-keeps-mapping-table ()
  "Windows VirtualFree failures keep mmap tracking intact for retry."
  (let ((call nil)
        (nelisp-os--windows-mmap-table '((#x70000000 . virtualalloc))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0))
              ((symbol-function 'nelisp-os--windows-ffi-error-signal)
               (lambda () (signal 'nelisp-os-error (list 1234)))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-munmap #x70000000 4096)
                      :type 'nelisp-os-error)))
    (should (equal call
                   (list "kernel32" "VirtualFree"
                         [:sint32 :pointer :uint64 :uint32]
                         (list #x70000000 0
                               nelisp-os-WIN-MEM-RELEASE))))
    (should (equal nelisp-os--windows-mmap-table
                   '((#x70000000 . virtualalloc))))))

(ert-deftest nelisp-stdlib-os-munmap-windows-file-view-failure-keeps-mapping-table ()
  "Windows UnmapViewOfFile failures keep mmap tracking intact for retry."
  (let ((call nil)
        (nelisp-os--windows-mmap-table '((#x71000000 . mapped-file))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0))
              ((symbol-function 'nelisp-os--windows-ffi-error-signal)
               (lambda () (signal 'nelisp-os-error (list 1234)))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-munmap #x71000000 4096)
                      :type 'nelisp-os-error)))
    (should (equal call
                   (list "kernel32" "UnmapViewOfFile"
                         [:sint32 :pointer]
                         (list #x71000000))))
    (should (equal nelisp-os--windows-mmap-table
                   '((#x71000000 . mapped-file))))))

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

(ert-deftest nelisp-stdlib-os-lseek-windows-socket-signals-espipe-before-ffi ()
  "Windows lseek on socket-kind fd signals ESPIPE before HANDLE FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (let ((err (should-error (nelisp-os-lseek 3 0 nelisp-os-SEEK-SET)
                                 :type 'nelisp-os-error)))
          (should (equal (cdr err) '(29))))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-lseek-windows-eventfd-returns-zero-before-ffi ()
  "Windows lseek on eventfd-kind fd matches Linux eventfd's zero result."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . 0)))
        (nelisp-os--windows-fd-kind-table '((3 . eventfd)))
        (nelisp-os--windows-eventfd-table '((3 . (1 . 0)))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-lseek 3 120 nelisp-os-SEEK-SET) 0))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-lseek-windows-process-signals-espipe-before-ffi ()
  "Windows lseek on process-kind fd signals ESPIPE like Linux pidfd."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (let ((err (should-error (nelisp-os-lseek 3 0 nelisp-os-SEEK-SET)
                                 :type 'nelisp-os-error)))
          (should (equal (cdr err) '(29))))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fstat-windows-disk-uses-file-information ()
  "Windows fstat on disk HANDLE decodes BY_HANDLE_FILE_INFORMATION."
  (let* ((mtime-sec 1700000000)
         (mtime-nsec 123456700)
         (atime-sec 1700000100)
         (atime-nsec 765432100)
         (ctime-sec 1700000200)
         (ctime-nsec 500)
         (mtime-ticks (+ (* (+ mtime-sec
                                nelisp-os-WIN-FILETIME-UNIX-EPOCH-SECONDS)
                             10000000)
                          (/ mtime-nsec 100)))
         (atime-ticks (+ (* (+ atime-sec
                                nelisp-os-WIN-FILETIME-UNIX-EPOCH-SECONDS)
                             10000000)
                          (/ atime-nsec 100)))
         (ctime-ticks (+ (* (+ ctime-sec
                                nelisp-os-WIN-FILETIME-UNIX-EPOCH-SECONDS)
                             10000000)
                          (/ ctime-nsec 100)))
         (size #x123456789)
         (ino #x1122334455667788)
         (dev #xaabbccdd)
         (calls nil)
         (freed nil)
         (nelisp-os--windows-fd-table '((3 . #x44556677))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n)
                 (should (= n nelisp-os-WIN-BY-HANDLE-FILE-INFORMATION-SIZE))
                 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (cond
                  ((= off nelisp-os-WIN-BHFI-ATTRIBUTES-OFFSET)
                   nelisp-os-WIN-FILE-ATTRIBUTE-NORMAL)
                  ((= off nelisp-os-WIN-BHFI-CREATION-TIME-OFFSET)
                   (nelisp-os--windows-u64-low ctime-ticks))
                  ((= off (+ nelisp-os-WIN-BHFI-CREATION-TIME-OFFSET 4))
                   (nelisp-os--windows-u64-high ctime-ticks))
                  ((= off nelisp-os-WIN-BHFI-LAST-ACCESS-TIME-OFFSET)
                   (nelisp-os--windows-u64-low atime-ticks))
                  ((= off (+ nelisp-os-WIN-BHFI-LAST-ACCESS-TIME-OFFSET 4))
                   (nelisp-os--windows-u64-high atime-ticks))
                  ((= off nelisp-os-WIN-BHFI-LAST-WRITE-TIME-OFFSET)
                   (nelisp-os--windows-u64-low mtime-ticks))
                  ((= off (+ nelisp-os-WIN-BHFI-LAST-WRITE-TIME-OFFSET 4))
                   (nelisp-os--windows-u64-high mtime-ticks))
                  ((= off nelisp-os-WIN-BHFI-VOLUME-SERIAL-OFFSET) dev)
                  ((= off nelisp-os-WIN-BHFI-FILE-SIZE-HIGH-OFFSET)
                   (nelisp-os--windows-u64-high size))
                  ((= off nelisp-os-WIN-BHFI-FILE-SIZE-LOW-OFFSET)
                   (nelisp-os--windows-u64-low size))
                  ((= off nelisp-os-WIN-BHFI-NUMBER-OF-LINKS-OFFSET) 3)
                  ((= off nelisp-os-WIN-BHFI-FILE-INDEX-HIGH-OFFSET)
                   (nelisp-os--windows-u64-high ino))
                  ((= off nelisp-os-WIN-BHFI-FILE-INDEX-LOW-OFFSET)
                   (nelisp-os--windows-u64-low ino))
                  (t (error "unexpected read-u32 offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-DISK)
                  ((equal fn "GetFileInformationByHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-size st) size))
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFREG))
        (should (= (nelisp-os-stat-mtime st) mtime-sec))
        (should (= (nelisp-os-stat-mtime-nsec st) mtime-nsec))
        (should (= (nelisp-os-stat-atime st) atime-sec))
        (should (= (nelisp-os-stat-atime-nsec st) atime-nsec))
        (should (= (nelisp-os-stat-ctime st) ctime-sec))
        (should (= (nelisp-os-stat-ctime-nsec st) 500))
        (should (= (nelisp-os-stat-nlink st) 3))
        (should (= (nelisp-os-stat-ino st) ino))
        (should (= (nelisp-os-stat-dev st) dev))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x44556677))
                    (list "kernel32" "GetFileInformationByHandle"
                          [:sint32 :pointer :pointer]
                          (list #x44556677 3000)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fstat-windows-directory-mode ()
  "Windows fstat maps disk HANDLE directory attributes to S_IFDIR."
  (let ((nelisp-os--windows-fd-table '((3 . #x44556677))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (_ptr off)
                 (cond
                  ((= off nelisp-os-WIN-BHFI-ATTRIBUTES-OFFSET)
                   nelisp-os-WIN-FILE-ATTRIBUTE-DIRECTORY)
                  (t 0))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (_dll fn _sig &rest _args)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-DISK)
                  ((equal fn "GetFileInformationByHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFDIR))))))

(ert-deftest nelisp-stdlib-os-fstat-windows-pipe-skips-file-information ()
  "Windows fstat on pipe HANDLE returns a zero-size FIFO stat."
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
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFIFO))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x55667788)))))))

(ert-deftest nelisp-stdlib-os-fstat-windows-char-returns-char-mode ()
  "Windows fstat on char HANDLE returns a zero-size character stat."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((4 . #x55667788))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-CHAR)
                  (t (error "unexpected ffi call %S" fn))))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 4)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFCHR))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x55667788)))))))

(ert-deftest nelisp-stdlib-os-fstat-windows-unknown-type-checks-lasterror ()
  "Windows fstat treats FILE_TYPE_UNKNOWN with NO_ERROR as unknown mode."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((4 . #x55667788))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-UNKNOWN)
                  ((equal fn "GetLastError") nelisp-os-WIN-NO-ERROR)
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
                          (list #x55667788))
                    (list "kernel32" "GetLastError" [:uint32] nil))))))

(ert-deftest nelisp-stdlib-os-fstat-windows-unknown-type-errors-on-lasterror ()
  "Windows fstat signals GetLastError for failed FILE_TYPE_UNKNOWN."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((4 . #x55667788))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetFileType") nelisp-os-WIN-FILE-TYPE-UNKNOWN)
                  ((equal fn "GetLastError") 1234)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-fstat 4) :type 'nelisp-os-error)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetFileType"
                          [:uint32 :pointer]
                          (list #x55667788))
                    (list "kernel32" "GetLastError" [:uint32] nil))))))

(ert-deftest nelisp-stdlib-os-fstat-windows-socket-returns-socket-mode ()
  "Windows fstat on socket-kind fd returns S_IFSOCK before kernel32 FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) nelisp-os-S-IFSOCK))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fstat-windows-eventfd-returns-compat-stat ()
  "Windows fstat on eventfd-kind fd returns Linux-compatible stat metadata."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . 0)))
        (nelisp-os--windows-fd-kind-table '((3 . eventfd)))
        (nelisp-os--windows-eventfd-table '((3 . (7 . 0)))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) nelisp-os--eventfd-stat-mode))
        (should (= (logand (nelisp-os-stat-mode st) nelisp-os-S-IFMT) 0))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fstat-windows-process-returns-pidfd-stat ()
  "Windows fstat on process-kind fd returns Linux pidfd-compatible metadata."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat 3)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) nelisp-os--pidfd-stat-mode))
        (should (= (logand (nelisp-os-stat-mode st) nelisp-os-S-IFMT) 0))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fstat-windows-stdout-eventfd-skips-handle-ffi ()
  "Windows fstat on a standard eventfd-kind fd does not call HANDLE APIs."
  (let ((called nil)
        (nelisp-os--windows-fd-kind-table
         `((,nelisp-os-STDOUT . eventfd)))
        (nelisp-os--windows-eventfd-table
         `((,nelisp-os-STDOUT . (0 . 0)))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (st (nelisp-os-fstat nelisp-os-STDOUT)))
        (should (= (nelisp-os-stat-size st) 0))
        (should (= (nelisp-os-stat-mode st) nelisp-os--eventfd-stat-mode))
        (should (= (nelisp-os-stat-nlink st) 1))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-regular-fd ()
  "Windows dup2 duplicates a HANDLE and installs it in the fd table."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 6)
        (nelisp-os--windows-fd-table '((5 . #xcccc)
                                       (3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,(logior nelisp-os-O-WRONLY nelisp-os-O-APPEND)))))
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
    (should (equal nelisp-os--windows-fd-flags-table
                   `((5 . ,(logior nelisp-os-O-WRONLY
                                   nelisp-os-O-APPEND))
                     (3 . ,(logior nelisp-os-O-WRONLY
                                   nelisp-os-O-APPEND)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-can-target-stdout ()
  "Windows dup2 to stdout replaces and closes the old standard HANDLE."
  (let ((calls nil)
        (std-handles '(#xdddd #xbbbb))
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,(logior nelisp-os-O-WRONLY nelisp-os-O-APPEND)))))
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
                  ((equal fn "GetStdHandle") (pop std-handles))
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   (logior nelisp-os-O-WRONLY nelisp-os-O-APPEND)))))
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
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #xbbbb))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,(logior nelisp-os-O-WRONLY
                                                   nelisp-os-O-APPEND))
                     (3 . ,(logior nelisp-os-O-WRONLY
                                   nelisp-os-O-APPEND)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-cleans-duplicate-on-target-close-error ()
  "Windows dup2 closes the duplicate if replacing the target fd fails."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((5 . #xcccc)
                                       (3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((5 . ,nelisp-os-O-RDWR)
           (3 . ,nelisp-os-O-WRONLY))))
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
                  ((and (equal fn "CloseHandle")
                        (= (car args) #xcccc))
                   0)
                  ((and (equal fn "CloseHandle")
                        (= (car args) #xbbbb))
                   1)
                  ((equal fn "GetLastError") 1234)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-dup2 3 5)
                      :type 'nelisp-os-error)))
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
                          (list #xcccc))
                    (list "kernel32" "GetLastError"
                          [:uint32]
                          nil)
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal nelisp-os--windows-fd-table '((5 . #xcccc) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((5 . ,nelisp-os-O-RDWR)
                     (3 . ,nelisp-os-O-WRONLY))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-same-fd-validates-oldfd ()
  "Windows dup2(oldfd, oldfd) validates OLDFD before returning."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 3) 3))
        (should-error (nelisp-os-dup2 4 4)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-eventfd ()
  "Windows eventfd dup2 installs another synthetic fd sharing the counter."
  (let ((called nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-eventfd
                    8
                    (logior nelisp-os-EFD-NONBLOCK
                            nelisp-os-EFD-CLOEXEC))
                   3))
        (should (= (nelisp-os-dup2 3 5) 5))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFD 0)
                   nelisp-os-FD-CLOEXEC))
        (should (= (nelisp-os-fcntl 5 nelisp-os-F-GETFD 0) 0))
        (should (= (nelisp-os-fcntl 5 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-write 5 (nelisp-os--u64le-string 2)) 8))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 10))
        (should-not (nelisp-os-close 5))
        (should-not (nelisp-os-close 3))))
    (should-not called)
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-eventfd-table)
    (should-not nelisp-os--windows-eventfd-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-dup2-windows-eventfd-can-target-stdout ()
  "Windows eventfd dup2 to stdout installs a synthetic standard fd."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xdddd)
                  ((equal fn "SetStdHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-eventfd 1 nelisp-os-EFD-NONBLOCK) 3))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-write nelisp-os-STDOUT
                                    (nelisp-os--u64le-string 4))
                   8))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 5))
        (should-not (nelisp-os-close nelisp-os-STDOUT))
        (should-not (nelisp-os-close 3))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE 0))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd)))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-eventfd-table)
    (should-not nelisp-os--windows-eventfd-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-process-fd-kind ()
  "Windows process fd dup2 preserves process-kind non-file behavior."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . process)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK))))
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
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))
        (should (= (nelisp-os-fcntl 5 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (let ((err (should-error (nelisp-os-lseek 5 0 nelisp-os-SEEK-SET)
                                 :type 'nelisp-os-error)))
          (should (equal (cdr err) '(29))))))
    (should (equal nelisp-os--windows-fd-table '((5 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((5 . process) (3 . process))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((5 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-process-can-target-stdout ()
  "Windows process fd dup2 to stdout preserves process-kind std fd behavior."
  (let ((calls nil)
        (freed nil)
        (std-handles '(#xdddd #xbbbb #xbbbb))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . process)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK))))
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
                  ((equal fn "GetStdHandle") (pop std-handles))
                  ((equal fn "SetStdHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (let ((err (should-error (nelisp-os-write nelisp-os-STDOUT "x")
                                 :type 'nelisp-os-error)))
          (should (equal (cdr err) '(22))))))
    (should (equal nelisp-os--windows-fd-kind-table
                   `((,nelisp-os-STDOUT . process) (3 . process))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #xbbbb))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-socket-fd ()
  "Windows socket fd dup2 uses WSADuplicateSocketW + WSASocketW."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-winsock-started-p t)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcessId") 222)
                  ((equal fn "WSADuplicateSocketW") 0)
                  ((equal fn "WSASocketW") #x123456)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcessId"
                          [:uint32]
                          nil)
                    (list "ws2_32" "WSADuplicateSocketW"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef 222 3000))
                    (list "ws2_32" "WSASocketW"
                          [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                          (list nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                3000
                                0
                                nelisp-os-WIN-WSA-FLAG-OVERLAPPED)))))
    (should (equal nelisp-os--windows-fd-table
                   '((5 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((5 . socket) (3 . socket))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-socket-can-target-stdout ()
  "Windows socket dup2 to stdout installs a socket-kind standard fd."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-winsock-started-p t)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,nelisp-os-O-NONBLOCK))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcessId") 222)
                  ((equal fn "WSADuplicateSocketW") 0)
                  ((equal fn "WSASocketW") #x123456)
                  ((equal fn "GetStdHandle") #xdddd)
                  ((equal fn "SetStdHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcessId"
                          [:uint32]
                          nil)
                    (list "ws2_32" "WSADuplicateSocketW"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef 222 3000))
                    (list "ws2_32" "WSASocketW"
                          [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                          (list nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                3000
                                0
                                nelisp-os-WIN-WSA-FLAG-OVERLAPPED))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #x123456))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   `((,nelisp-os-STDOUT . socket) (3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-handle ()
  "Windows F_DUPFD duplicates a HANDLE into the fd table at or above ARG."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,(logior nelisp-os-O-RDWR nelisp-os-O-APPEND)))))
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
    (should (equal nelisp-os--windows-fd-flags-table
                   `((10 . ,(logior nelisp-os-O-RDWR
                                    nelisp-os-O-APPEND))
                     (3 . ,(logior nelisp-os-O-RDWR
                                   nelisp-os-O-APPEND)))))
    (should (= nelisp-os--windows-next-fd 11))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-negative-min-errors-before-ffi ()
  "Windows F_DUPFD rejects negative MIN-FD before duplicating a HANDLE."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-fcntl 3 nelisp-os-F-DUPFD -1)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-socket-fd ()
  "Windows F_DUPFD for socket fd creates a new socket-kind fd."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-winsock-started-p t)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,nelisp-os-O-NONBLOCK))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcessId") 222)
                  ((equal fn "WSADuplicateSocketW") 0)
                  ((equal fn "WSASocketW") #x123456)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcessId"
                          [:uint32]
                          nil)
                    (list "ws2_32" "WSADuplicateSocketW"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef 222 3000))
                    (list "ws2_32" "WSASocketW"
                          [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                          (list nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                nelisp-os-WIN-FROM-PROTOCOL-INFO
                                3000
                                0
                                nelisp-os-WIN-WSA-FLAG-OVERLAPPED)))))
    (should (equal nelisp-os--windows-fd-table
                   '((10 . #x123456) (3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((10 . socket) (3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((10 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (= nelisp-os--windows-next-fd 11))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-socket-negative-min-errors-before-ffi ()
  "Windows F_DUPFD rejects negative MIN-FD before duplicating a socket."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-fcntl 3 nelisp-os-F-DUPFD -1)
                      :type 'nelisp-os-error)))
    (should-not called)))

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

(ert-deftest nelisp-stdlib-os-fcntl-windows-setfl-regular-append-change-errors ()
  "Windows regular F_SETFL refuses to desync tracked O_APPEND from the HANDLE."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,(logior nelisp-os-O-WRONLY nelisp-os-O-APPEND)))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-fcntl 3 nelisp-os-F-SETFL 0)
                      :type 'nelisp-os-error)
        (should (= (nelisp-os-fcntl
                    3 nelisp-os-F-SETFL
                    (logior nelisp-os-O-WRONLY nelisp-os-O-APPEND))
                   0))))
    (should-not called)
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,(logior nelisp-os-O-WRONLY
                                   nelisp-os-O-APPEND)))))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-getfd-uses-gethandleinformation ()
  "Windows F_GETFD maps HANDLE_FLAG_INHERIT to POSIX FD_CLOEXEC."
  (let ((calls nil)
        (reads (list 0 nelisp-os-WIN-HANDLE-FLAG-INHERIT))
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (push (list 'read-u32 ptr off) calls)
                 (pop reads)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFD 0)
                   nelisp-os-FD-CLOEXEC))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFD 0) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetHandleInformation"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    '(read-u32 3000 0)
                    (list "kernel32" "GetHandleInformation"
                          [:sint32 :pointer :pointer]
                          (list #xaaaa 3000))
                    '(read-u32 3000 0))))
    (should (equal freed '(3000 3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-setfd-uses-sethandleinformation ()
  "Windows F_SETFD toggles HANDLE_FLAG_INHERIT for FD_CLOEXEC."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFD
                                    nelisp-os-FD-CLOEXEC)
                   0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFD 0) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "SetHandleInformation"
                          [:sint32 :pointer :uint32 :uint32]
                          (list #xaaaa
                                nelisp-os-WIN-HANDLE-FLAG-INHERIT
                                0))
                    (list "kernel32" "SetHandleInformation"
                          [:sint32 :pointer :uint32 :uint32]
                          (list #xaaaa
                                nelisp-os-WIN-HANDLE-FLAG-INHERIT
                                nelisp-os-WIN-HANDLE-FLAG-INHERIT)))))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-setfd-socket-uses-sethandleinformation ()
  "Windows F_SETFD works on socket-kind fds through the socket HANDLE."
  (let ((call nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFD
                                    nelisp-os-FD-CLOEXEC)
                   0))))
    (should (equal call
                   (list "kernel32" "SetHandleInformation"
                         [:sint32 :pointer :uint32 :uint32]
                         (list #xabcdef
                               nelisp-os-WIN-HANDLE-FLAG-INHERIT
                               0))))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-setfd-extra-flags-error ()
  "Windows F_SETFD rejects unsupported descriptor flags before FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-fcntl 3 nelisp-os-F-SETFD
                          (logior nelisp-os-FD-CLOEXEC #x100))
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fcntl-windows-socket-setfl-nonblock ()
  "Windows socket F_SETFL O_NONBLOCK uses ioctlsocket(FIONBIO)."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFL
                                    nelisp-os-O-NONBLOCK)
                   0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFL 0) 0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0) 0))))
    (should (equal (nreverse writes)
                   '((3000 0 1) (3000 0 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000))
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000)))))
    (should (equal (sort freed #'<) '(3000 3000)))
    (should-not nelisp-os--windows-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-fcntl-windows-stdout-socket-setfl-nonblock ()
  "Windows socket-kind stdout F_SETFL O_NONBLOCK uses ioctlsocket(FIONBIO)."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-kind-table `((,nelisp-os-STDOUT . socket)))
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xabcdef)
                  ((equal fn "ioctlsocket") 0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-SETFL
                                    nelisp-os-O-NONBLOCK)
                   0))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))))
    (should (equal (nreverse writes) '((3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle" [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle" [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000))
                    (list "kernel32" "GetStdHandle" [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE)))))
    (should (equal freed '(3000)))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK))))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-socket-setfl-extra-flags-error ()
  "Windows socket F_SETFL rejects unsupported flags before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-fcntl 3 nelisp-os-F-SETFL
                          (logior nelisp-os-O-NONBLOCK nelisp-os-O-APPEND))
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-process-fd-kind ()
  "Windows F_DUPFD preserves process-kind fd tracking."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . process)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK))))
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
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))
        (should (= (nelisp-os-fcntl 10 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (let ((err (should-error (nelisp-os-read 10 1)
                                 :type 'nelisp-os-error)))
          (should (equal (cdr err) '(22))))))
    (should (equal nelisp-os--windows-next-fd 11))
    (should (equal nelisp-os--windows-fd-table
                   '((10 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((10 . process) (3 . process))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((10 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-linux-only-apis-windows-error-before-syscall ()
  "Linux-only fork and untracked fd-passing reject Windows before FFI."
  (let ((called nil)
        (forms
         (list
          (lambda () (nelisp-os-fork))
          (lambda () (nelisp-os-sendmsg-fds 3 (list 4) "x"))
          (lambda () (nelisp-os-recvmsg-fds 3 1 1)))))
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

(ert-deftest nelisp-stdlib-os-getsockopt-peercred-windows-tracks-socketpair ()
  "Windows AF_UNIX socketpair tracks synthetic peer credentials."
  (let ((calls nil)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-socket-peercred-table nil)
        (nelisp-os--windows-socket-fdpass-table nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-socketpair-stream)
               (lambda (type domain)
                 (push (list 'socketpair-stream type domain) calls)
                 (setq nelisp-os--windows-fd-table '((4 . #xbbbb) (3 . #xaaaa)))
                 (setq nelisp-os--windows-fd-kind-table
                       '((4 . socket) (3 . socket)))
                 (cons 3 4))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX nelisp-os-SOCK-STREAM 0)
                       '(3 . 4)))
        (should (equal (nelisp-os-getsockopt-peercred 3)
                       (list (emacs-pid) 0 0)))
        (should (equal (nelisp-os-getsockopt-peercred 4)
                       (list (emacs-pid) 0 0)))))
    (should (equal calls
                   (list (list 'socketpair-stream
                               nelisp-os-SOCK-STREAM
                               nelisp-os-AF-UNIX))))
    (should (equal nelisp-os--windows-socket-peercred-table
                   `((4 . ,(list (emacs-pid) 0 0))
                     (3 . ,(list (emacs-pid) 0 0)))))
    (should (assq 3 nelisp-os--windows-socket-fdpass-table))
    (should (assq 4 nelisp-os--windows-socket-fdpass-table))))

(ert-deftest nelisp-stdlib-os-getsockopt-peercred-windows-duplicates-socket-state ()
  "Windows socket dup preserves tracked synthetic peer credentials."
  (let ((nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-socket-peercred-table '((3 . (222 0 0))))
        (nelisp-os--windows-socket-fdpass-table nil))
    (nelisp-os--windows-socket-fdpass-track-pair 3 4)
    (cl-letf (((symbol-function 'nelisp-os--windows-duplicate-socket)
               (lambda (fd)
                 (should (= fd 3))
                 #xbbbb)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))
        (should (equal (nelisp-os-getsockopt-peercred 5) '(222 0 0)))
        (should (eq (cdr (assq 3 nelisp-os--windows-socket-fdpass-table))
                    (cdr (assq 5 nelisp-os--windows-socket-fdpass-table)))))))
  (let ((nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-socket-peercred-table '((3 . (222 0 0))))
        (nelisp-os--windows-socket-fdpass-table nil))
    (nelisp-os--windows-socket-fdpass-track-pair 3 4)
    (cl-letf (((symbol-function 'nelisp-os--windows-duplicate-socket)
               (lambda (fd)
                 (should (= fd 3))
                 #xbbbb)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))
        (should (equal (nelisp-os-getsockopt-peercred 10) '(222 0 0)))
        (should (eq (cdr (assq 3 nelisp-os--windows-socket-fdpass-table))
                    (cdr (assq 10 nelisp-os--windows-socket-fdpass-table))))))))

(ert-deftest nelisp-stdlib-os-getsockopt-peercred-windows-rejects-untracked-socket ()
  "Windows peercred rejects sockets without synthetic peer credentials."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-socket-peercred-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-getsockopt-peercred 3)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-getsockopt-peercred-windows-cleans-up-on-close ()
  "Windows socket close removes synthetic peer credential state."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-socket-peercred-table '((3 . (222 0 0))))
        (nelisp-os--windows-socket-fdpass-table nil))
    (nelisp-os--windows-socket-fdpass-track-pair 3 4)
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should-not (nelisp-os-close 3))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-socket-peercred-table)
    (should-not nelisp-os--windows-socket-fdpass-table)
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "closesocket"
                          [:sint32 :pointer]
                          (list #xaaaa)))))))

(ert-deftest nelisp-stdlib-os-inotify-windows-creates-watches-and-cleans-up ()
  "Windows inotify creates fd-kind state and starts a native directory watch."
  (let ((calls nil)
        (alloc-next 1000)
        (freed nil)
        (wide-writes nil)
        (i64-writes nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-inotify-table nil))
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
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) i64-writes)
                 val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateEventW") #xaaaa)
                  ((equal fn "CreateFileW") #xbbbb)
                  ((equal fn "ReadDirectoryChangesW") 1)
                  ((equal fn "GetOverlappedResult") 0)
                  ((equal fn "GetLastError")
                   nelisp-os-WIN-ERROR-IO-INCOMPLETE)
                  ((equal fn "ResetEvent") 1)
                  ((equal fn "CancelIo") 1)
                  ((equal fn "SetHandleInformation") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-inotify-init
                    (logior nelisp-os-IN-NONBLOCK nelisp-os-IN-CLOEXEC))
                   3))
        (should (= (nelisp-os-inotify-add-watch
                    3 "C:/tmp" nelisp-os-IN-ALL-EVENTS)
                   1))
        (should (equal (nelisp-os-inotify-read 3 4) nil))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-stat-mode (nelisp-os-fstat 3))
                   nelisp-os--inotify-stat-mode))
        (should-error (nelisp-os-read 3 8) :type 'nelisp-os-error)
        (should-error (nelisp-os-write 3 "x") :type 'nelisp-os-error)
        (should-error (nelisp-os-lseek 3 0 nelisp-os-SEEK-SET)
                      :type 'nelisp-os-error)
        (should (= (nelisp-os-inotify-rm-watch 3 1) 0))
        (should-not (nelisp-os-close 3))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-inotify-table)
    (should i64-writes)
    (should wide-writes)
    (should (member (list "kernel32" "CreateFileW"
                          [:pointer :pointer :uint32 :uint32 :pointer
                           :uint32 :uint32 :pointer]
                          (list 1000
                                nelisp-os-WIN-FILE-LIST-DIRECTORY
                                (logior nelisp-os-WIN-FILE-SHARE-READ
                                        nelisp-os-WIN-FILE-SHARE-WRITE
                                        nelisp-os-WIN-FILE-SHARE-DELETE)
                                0
                                nelisp-os-WIN-OPEN-EXISTING
                                (logior
                                 nelisp-os-WIN-FILE-FLAG-BACKUP-SEMANTICS
                                 nelisp-os-WIN-FILE-FLAG-OVERLAPPED)
                                0))
                    calls))
    (should (member (list "kernel32" "ReadDirectoryChangesW"
                          [:sint32 :pointer :pointer :uint32 :sint32 :uint32
                           :pointer :pointer :pointer]
                          (list #xbbbb 2000
                                nelisp-os-WIN-INOTIFY-BUFFER-SIZE
                                0
                                (nelisp-os--windows-inotify-notify-mask
                                 nelisp-os-IN-ALL-EVENTS)
                                0 3000 0))
                    calls))
    (should (member (list "kernel32" "CancelIo"
                          [:sint32 :pointer]
                          (list #xbbbb))
                    calls))
    (should (member (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb))
                    calls))
    (should (member (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xaaaa))
                    calls))
    (should (equal (sort (copy-sequence freed) #'<)
                   '(1000 2000 3000 4000)))))

(ert-deftest nelisp-stdlib-os-inotify-windows-pumps-native-events ()
  "Windows inotify converts completed ReadDirectoryChangesW records to events."
  (let ((calls nil)
        (freed nil)
        (complete-count 0)
        (state (vector 2
                       (list (list 1 "C:/tmp" nelisp-os-IN-ALL-EVENTS
                                   #xbbbb #x2000 #x3000 t))
                       nil))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . inotify)))
        (nelisp-os--windows-inotify-table nil))
    (setq nelisp-os--windows-inotify-table (list (cons 3 state)))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n) #x4000))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (cond
                  ((= ptr #x4000) 22)
                  ((and (= ptr #x2000) (= off 0)) 0)
                  ((and (= ptr #x2000) (= off 4))
                   nelisp-os-WIN-FILE-ACTION-ADDED)
                  ((and (= ptr #x2000) (= off 8)) 10)
                  (t (error "unexpected read-u32 %S %S" ptr off)))))
              ((symbol-function 'nelisp-os-read-u16)
               (lambda (ptr off)
                 (unless (= ptr #x2000)
                   (error "unexpected read-u16 ptr %S" ptr))
                 (pcase off
                   (12 97)
                   (14 46)
                   (16 116)
                   (18 120)
                   (20 116)
                   (_ 0))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetOverlappedResult")
                   (if (= complete-count 0)
                       (progn
                         (setq complete-count (1+ complete-count))
                         1)
                     0))
                  ((equal fn "GetLastError")
                   nelisp-os-WIN-ERROR-IO-INCOMPLETE)
                  ((equal fn "SetEvent") 1)
                  ((equal fn "ResetEvent") 1)
                  ((equal fn "ReadDirectoryChangesW") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 nelisp-os-POLLIN))))
        (should (equal (nelisp-os-inotify-read 3 4)
                       (list (list 1 nelisp-os-IN-CREATE 0 "a.txt"))))))
    (should (equal freed '(#x4000 #x4000 #x4000)))
    (should (equal (aref (cdr (assq 3 nelisp-os--windows-inotify-table)) 2)
                   nil))
    (should (equal (nth 6 (car (aref state 1))) t))
    (should (member (list "kernel32" "SetEvent"
                          [:sint32 :pointer]
                          (list #xaaaa))
                    calls))))

(ert-deftest nelisp-stdlib-os-inotify-windows-reads-queued-events ()
  "Windows inotify read/poll consume queued compatibility events."
  (let ((calls nil)
        (state (vector 2
                       (list (list 1 "C:/tmp" nelisp-os-IN-ALL-EVENTS))
                       (list (list 1 nelisp-os-IN-CREATE 0 "a.txt")
                             (list 1 nelisp-os-IN-DELETE 0 "b.txt"))))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . inotify)))
        (nelisp-os--windows-inotify-table nil))
    (setq nelisp-os--windows-inotify-table (list (cons 3 state)))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "ResetEvent") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 nelisp-os-POLLIN))))
        (should (equal (nelisp-os-inotify-read 3 1)
                       (list (list 1 nelisp-os-IN-CREATE 0 "a.txt"))))
        (should (equal (nelisp-os-inotify-read 3 4)
                       (list (list 1 nelisp-os-IN-DELETE 0 "b.txt"))))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 0))))))
    (should (equal (aref (cdr (assq 3 nelisp-os--windows-inotify-table)) 2)
                   nil))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "ResetEvent"
                          [:sint32 :pointer]
                          (list #xaaaa)))))))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-inotify-kind ()
  "Windows inotify dup2 preserves inotify kind and shared state."
  (let ((calls nil)
        (freed nil)
        (state (vector 2
                       (list (list 1 "C:/tmp" nelisp-os-IN-ALL-EVENTS))
                       (list (list 1 nelisp-os-IN-CREATE 0 "a.txt"))))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . inotify)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-inotify-table nil))
    (setq nelisp-os--windows-inotify-table (list (cons 3 state)))
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
                  ((equal fn "ResetEvent") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))
        (should (= (nelisp-os-fcntl 5 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (equal (nelisp-os-inotify-read 5 4)
                       (list (list 1 nelisp-os-IN-CREATE 0 "a.txt"))))))
    (should (equal nelisp-os--windows-fd-table
                   '((5 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((5 . inotify) (3 . inotify))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((5 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (eq (cdr (assq 3 nelisp-os--windows-inotify-table))
                (cdr (assq 5 nelisp-os--windows-inotify-table))))
    (should (equal (aref (cdr (assq 3 nelisp-os--windows-inotify-table)) 2)
                   nil))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "ResetEvent"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-inotify-kind ()
  "Windows F_DUPFD preserves inotify kind and shared watch state."
  (let ((calls nil)
        (freed nil)
        (alloc-next 3000)
        (wide-writes nil)
        (i64-writes nil)
        (state (vector 2
                       (list (list 1 "C:/tmp" nelisp-os-IN-ALL-EVENTS))
                       nil))
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . inotify)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-inotify-table nil))
    (setq nelisp-os--windows-inotify-table (list (cons 3 state)))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u16)
               (lambda (ptr off val)
                 (push (list ptr off val) wide-writes)
                 val))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) i64-writes)
                 val))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr _off)
                 (should (= ptr 3000))
                 #xbbbb))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetCurrentProcess") #x9999)
                  ((equal fn "DuplicateHandle") 1)
                  ((equal fn "CreateFileW") #xcccc)
                  ((equal fn "ResetEvent") 1)
                  ((equal fn "ReadDirectoryChangesW") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))
        (should (= (nelisp-os-fcntl 10 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-inotify-add-watch 10 "C:/next"
                                                nelisp-os-IN-CREATE)
                   2))))
    (should (equal nelisp-os--windows-next-fd 11))
    (should (equal nelisp-os--windows-fd-table
                   '((10 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((10 . inotify) (3 . inotify))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((10 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (eq (cdr (assq 3 nelisp-os--windows-inotify-table))
                (cdr (assq 10 nelisp-os--windows-inotify-table))))
    (should (equal (aref (cdr (assq 3 nelisp-os--windows-inotify-table)) 0)
                   3))
    (should wide-writes)
    (should (equal i64-writes
                   (list (list 6000
                               nelisp-os-WIN-OVERLAPPED-HEVENT-OFFSET
                               #xbbbb))))
    (should (member (list "kernel32" "CreateFileW"
                          [:pointer :pointer :uint32 :uint32 :pointer
                           :uint32 :uint32 :pointer]
                          (list 4000
                                nelisp-os-WIN-FILE-LIST-DIRECTORY
                                (logior nelisp-os-WIN-FILE-SHARE-READ
                                        nelisp-os-WIN-FILE-SHARE-WRITE
                                        nelisp-os-WIN-FILE-SHARE-DELETE)
                                0
                                nelisp-os-WIN-OPEN-EXISTING
                                (logior
                                 nelisp-os-WIN-FILE-FLAG-BACKUP-SEMANTICS
                                 nelisp-os-WIN-FILE-FLAG-OVERLAPPED)
                                0))
                    calls))
    (should (member (list "kernel32" "ReadDirectoryChangesW"
                          [:sint32 :pointer :pointer :uint32 :sint32 :uint32
                           :pointer :pointer :pointer]
                          (list #xcccc 5000
                                nelisp-os-WIN-INOTIFY-BUFFER-SIZE
                                0
                                (nelisp-os--windows-inotify-notify-mask
                                 nelisp-os-IN-CREATE)
                                0 6000 0))
                    calls))
    (should (equal freed '(4000 3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-inotify-can-target-stdout ()
  "Windows inotify dup2 to stdout preserves inotify standard fd behavior."
  (let ((calls nil)
        (freed nil)
        (state (vector 2
                       (list (list 1 "C:/tmp" nelisp-os-IN-ALL-EVENTS))
                       (list (list 1 nelisp-os-IN-DELETE 0 "old.txt"))))
        (std-handles '(#xdddd #xbbbb #xbbbb))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . inotify)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-inotify-table nil))
    (setq nelisp-os--windows-inotify-table (list (cons 3 state)))
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
                  ((equal fn "GetStdHandle") (pop std-handles))
                  ((equal fn "SetStdHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  ((equal fn "ResetEvent") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (equal (nelisp-os-inotify-read nelisp-os-STDOUT 4)
                       (list (list 1 nelisp-os-IN-DELETE 0 "old.txt"))))))
    (should (equal nelisp-os--windows-fd-kind-table
                   `((,nelisp-os-STDOUT . inotify) (3 . inotify))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (eq (cdr (assq 3 nelisp-os--windows-inotify-table))
                (cdr (assq nelisp-os-STDOUT
                           nelisp-os--windows-inotify-table))))
    (should (equal (aref (cdr (assq 3 nelisp-os--windows-inotify-table)) 2)
                   nil))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #xbbbb))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "ResetEvent"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-sendmsg-fds-windows-empty-fds-uses-send ()
  "Windows sendmsg-fds with no fd passing uses Winsock send for payload."
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
        (should (= (nelisp-os-sendmsg-fds 3 nil "data") 4))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "send"
                          [:sint32 :pointer :pointer :sint32 :sint32]
                          (list #xabcdef 3000 4 0)))))
    (should (equal written-string (cons 3000 "data")))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-recvmsg-fds-windows-zero-fds-uses-recv ()
  "Windows recvmsg-fds with no fd passing uses Winsock recv for payload."
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
        (should (equal (nelisp-os-recvmsg-fds 3 0 16)
                       (cons "data" nil)))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "recv"
                          [:sint32 :pointer :pointer :sint32 :sint32]
                          (list #xabcdef 3000 16 0)))))
    (should (equal read-request (list 3000 4)))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-send-recvmsg-fds-windows-socketpair-passes-fds ()
  "Windows AF_UNIX socketpairs support same-process synthetic fd passing."
  (let ((writes nil)
        (reads nil)
        (dups nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)
                                       (4 . #xbbbb)
                                       (10 . #xcccc)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)
                                            (4 . socket)
                                            (10 . handle)))
        (nelisp-os--windows-socket-fdpass-table nil))
    (nelisp-os--windows-socket-fdpass-track-pair 3 4)
    (cl-letf (((symbol-function 'nelisp-os--windows-write-socket)
               (lambda (fd payload)
                 (push (list fd payload) writes)
                 (length payload)))
              ((symbol-function 'nelisp-os--windows-read-socket)
               (lambda (fd max-bytes)
                 (push (list fd max-bytes) reads)
                 "data"))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (fd cmd arg)
                 (push (list fd cmd arg) dups)
                 11)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-sendmsg-fds 3 (list 10) "data") 4))
        (should (equal (nelisp-os-recvmsg-fds 4 1 16)
                       (cons "data" (list 11))))))
    (should (equal writes '((3 "data"))))
    (should (equal reads '((4 16))))
    (should (equal dups
                   `((10 ,nelisp-os-F-DUPFD 0))))
    (should-not (aref (aref (cdr (assq 4
                                       nelisp-os--windows-socket-fdpass-table))
                            0)
                      0))))

(ert-deftest nelisp-stdlib-os-recvmsg-fds-windows-zero-max-discards-fds ()
  "Windows recvmsg-fds with MAX-FDS 0 discards queued synthetic fds."
  (let ((dups nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)
                                       (4 . #xbbbb)
                                       (10 . #xcccc)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)
                                            (4 . socket)
                                            (10 . handle)))
        (nelisp-os--windows-socket-fdpass-table nil))
    (nelisp-os--windows-socket-fdpass-track-pair 3 4)
    (cl-letf (((symbol-function 'nelisp-os--windows-write-socket)
               (lambda (_fd payload) (length payload)))
              ((symbol-function 'nelisp-os--windows-read-socket)
               (lambda (_fd _max-bytes) "data"))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (&rest args)
                 (push args dups)
                 11)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-sendmsg-fds 3 (list 10) "data") 4))
        (should (equal (nelisp-os-recvmsg-fds 4 0 16)
                       (cons "data" nil)))))
    (should-not dups)
    (should-not (aref (aref (cdr (assq 4
                                       nelisp-os--windows-socket-fdpass-table))
                            0)
                      0))))

(ert-deftest nelisp-stdlib-os-send-recvmsg-fds-windows-untracked-rejects-fds ()
  "Windows arbitrary sockets still reject real fd passing before Winsock I/O."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa) (10 . #xbbbb)))
        (nelisp-os--windows-fd-kind-table '((3 . socket) (10 . handle)))
        (nelisp-os--windows-socket-fdpass-table nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-write-socket)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--windows-read-socket)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-sendmsg-fds 3 (list 10) "data")
                      :type 'nelisp-os-error)
        (should-error (nelisp-os-recvmsg-fds 3 1 16)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-sigprocmask-windows-tracks-emulated-mask ()
  "Windows sigprocmask tracks an emulated mask without POSIX FFI."
  (let ((called nil)
        (nelisp-os--windows-signal-mask nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-not
         (nelisp-os-sigprocmask
          nelisp-os-SIG-BLOCK
          (list nelisp-os-SIGTERM nelisp-os-SIGUSR1 nelisp-os-SIGTERM)))
        (should (equal nelisp-os--windows-signal-mask
                       (list nelisp-os-SIGUSR1 nelisp-os-SIGTERM)))
        (should (equal
                 (nelisp-os-sigprocmask
                  nelisp-os-SIG-BLOCK
                  (list nelisp-os-SIGUSR2))
                 (list nelisp-os-SIGUSR1 nelisp-os-SIGTERM)))
        (should (equal nelisp-os--windows-signal-mask
                       (list nelisp-os-SIGUSR1
                             nelisp-os-SIGUSR2
                             nelisp-os-SIGTERM)))
        (should (equal
                 (nelisp-os-sigprocmask
                  nelisp-os-SIG-UNBLOCK
                  (list nelisp-os-SIGUSR1))
                 (list nelisp-os-SIGUSR1
                       nelisp-os-SIGUSR2
                       nelisp-os-SIGTERM)))
        (should (equal nelisp-os--windows-signal-mask
                       (list nelisp-os-SIGUSR2 nelisp-os-SIGTERM)))
        (should (equal
                 (nelisp-os-sigprocmask
                  nelisp-os-SIG-SETMASK
                  (list nelisp-os-SIGCHLD))
                 (list nelisp-os-SIGUSR2 nelisp-os-SIGTERM)))
        (should (equal nelisp-os--windows-signal-mask
                       (list nelisp-os-SIGCHLD)))
        (should-error (nelisp-os-sigprocmask 99 nil)
                      :type 'nelisp-os-error)
        (should-error (nelisp-os-sigprocmask nelisp-os-SIG-BLOCK '(0))
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-pidfd-open-windows-opens-process-handle ()
  "Windows pidfd_open returns a process HANDLE-backed fd."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "OpenProcess") #xabcdef)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-pidfd-open 1234 nelisp-os-PIDFD-NONBLOCK) 3))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should-not (nelisp-os-close 3))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "OpenProcess"
                          [:pointer :uint32 :sint32 :uint32]
                          (list (logior
                                 nelisp-os-WIN-PROCESS-TERMINATE
                                 nelisp-os-WIN-SYNCHRONIZE
                                 nelisp-os-WIN-PROCESS-QUERY-LIMITED-INFORMATION)
                                0 1234))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-pidfd-send-signal-windows-uses-terminateprocess ()
  "Windows pidfd_send_signal terminates process HANDLE-backed fds."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "TerminateProcess") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-pidfd-send-signal 3 0 0) 0))
        (should (= (nelisp-os-pidfd-send-signal 3 nelisp-os-SIGTERM 0) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "TerminateProcess"
                          [:sint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-SIGTERM)))))))

(ert-deftest nelisp-stdlib-os-pidfd-windows-validates-before-ffi ()
  "Windows pidfd compatibility rejects invalid args before FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef) (4 . #xbbbb)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-pidfd-open 0 0) :type 'nelisp-os-error)
        (should-error (nelisp-os-pidfd-open 1234 #x40000000)
                      :type 'nelisp-os-error)
        (should-error
         (nelisp-os-pidfd-send-signal 3 nelisp-os-SIGHUP 0)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-pidfd-send-signal 3 nelisp-os-SIGTERM 1)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-pidfd-send-signal 4 nelisp-os-SIGTERM 0)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-eventfd-windows-supports-counter-read-write ()
  "Windows eventfd compatibility tracks counter fd read/write state."
  (let ((called nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-eventfd 5 nelisp-os-EFD-NONBLOCK) 3))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 5))
        (should-error (nelisp-os-read 3 8) :type 'nelisp-os-error)
        (should (= (nelisp-os-write 3 (nelisp-os--u64le-string 7)) 8))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 7))
        (should-not (nelisp-os-close 3))))
    (should-not called)
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-eventfd-table)
    (should-not nelisp-os--windows-eventfd-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-eventfd-windows-supports-semaphore-mode ()
  "Windows eventfd compatibility honors EFD_SEMAPHORE read semantics."
  (let ((nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (let ((system-type 'windows-nt))
      (should (= (nelisp-os-eventfd 2 nelisp-os-EFD-SEMAPHORE) 3))
      (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 1))
      (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 1))
      (should-error (nelisp-os-read 3 8) :type 'nelisp-os-error))
    (should (equal nelisp-os--windows-eventfd-table
                   `((3 . (0 . ,nelisp-os-EFD-SEMAPHORE)))))))

(ert-deftest nelisp-stdlib-os-eventfd-windows-validates-operations ()
  "Windows eventfd compatibility validates flags, sizes, and overflow."
  (let ((nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (let ((system-type 'windows-nt))
      (should-error (nelisp-os-eventfd -1 0) :type 'nelisp-os-error)
      (should-error (nelisp-os-eventfd 0 #x40000000) :type 'nelisp-os-error)
      (should (= (nelisp-os-eventfd nelisp-os--eventfd-max-counter 0) 3))
      (should-error (nelisp-os-read 3 4) :type 'nelisp-os-error)
      (should-error (nelisp-os-write 3 "short") :type 'nelisp-os-error)
      (should-error
       (nelisp-os-write 3 (nelisp-os--u64le-string
                           nelisp-os--eventfd-invalid-value))
       :type 'nelisp-os-error)
      (should-error
       (nelisp-os-write 3 (nelisp-os--u64le-string 1))
       :type 'nelisp-os-error))))

(ert-deftest nelisp-stdlib-os-eventfd-windows-supports-fcntl-dupfd-and-cloexec ()
  "Windows eventfd compatibility supports synthetic F_DUPFD/F_GETFD/F_SETFD."
  (let ((called nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-eventfd-table nil)
        (nelisp-os--windows-eventfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-eventfd
                    4
                    (logior nelisp-os-EFD-NONBLOCK
                            nelisp-os-EFD-CLOEXEC))
                   3))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFD 0)
                   nelisp-os-FD-CLOEXEC))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFD 0) 0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFD 0) 0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))
        (should (= (nelisp-os-fcntl 10 nelisp-os-F-GETFD 0) 0))
        (should (= (nelisp-os-fcntl 10 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 4))
        (should (= (nelisp-os-write 10 (nelisp-os--u64le-string 6)) 8))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 6))
        (should-error
         (nelisp-os-fcntl 10 nelisp-os-F-SETFD
                          (logior nelisp-os-FD-CLOEXEC #x100))
         :type 'nelisp-os-error)
        (should-error (nelisp-os-fcntl 10 nelisp-os-F-DUPFD -1)
                      :type 'nelisp-os-error)
        (should-not (nelisp-os-close 10))
        (should-not (nelisp-os-close 3))))
    (should-not called)
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-eventfd-table)
    (should-not nelisp-os--windows-eventfd-fd-flags-table)))

(ert-deftest nelisp-stdlib-os-timerfd-windows-create-uses-waitable-timer ()
  "Windows timerfd_create creates a waitable timer HANDLE-backed fd."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-timerfd-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateWaitableTimerW") #xaaaa)
                  ((equal fn "SetHandleInformation") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-timerfd-create
                    nelisp-os-CLOCK-MONOTONIC
                    (logior nelisp-os-TFD-NONBLOCK
                            nelisp-os-TFD-CLOEXEC))
                   3))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . timerfd))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal nelisp-os--windows-timerfd-table
                   `((3 . [0 0 nil ,nelisp-os-CLOCK-MONOTONIC]))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateWaitableTimerW"
                          [:pointer :pointer :sint32 :pointer]
                          (list 0 0 0))
                    (list "kernel32" "SetHandleInformation"
                          [:sint32 :pointer :uint32 :uint32]
                          (list #xaaaa
                                nelisp-os-WIN-HANDLE-FLAG-INHERIT
                                0)))))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-settime-gettime-and-read ()
  "Windows timerfd settime/gettime/read use a waitable timer HANDLE."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (ticks '(1000 1200))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-timerfd-table '((3 . [0 0 nil]))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetTickCount64") (pop ticks))
                  ((equal fn "SetWaitableTimer") 1)
                  ((equal fn "WaitForSingleObject")
                   nelisp-os-WIN-WAIT-OBJECT-0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-timerfd-set-relative-ms 3 2500)
                       '(0 0 0 0)))
        (should (equal (nelisp-os-timerfd-gettime 3)
                       '(0 0 2 300000000)))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 3 8)) 1))))
    (should (equal nelisp-os--windows-timerfd-table
                   '((3 . [0 0 nil]))))
    (should (equal writes '((3000 0 -25000000))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "SetWaitableTimer"
                          [:sint32 :pointer :pointer :sint32 :pointer :pointer
                           :sint32]
                          (list #xaaaa 3000 0 0 0 0))
                    (list "kernel32" "GetTickCount64" [:uint64] nil)
                    (list "kernel32" "GetTickCount64" [:uint64] nil)
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xaaaa 0)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-settime-supports-abstime ()
  "Windows timerfd converts TFD_TIMER_ABSTIME into a relative wait."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (clock-reads nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-timerfd-table
         `((3 . [0 0 nil ,nelisp-os-CLOCK-REALTIME]))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--windows-timerfd-clock-now-ms)
               (lambda (clockid)
                 (push clockid clock-reads)
                 9000))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "SetWaitableTimer") 1)
                  ((equal fn "GetTickCount64") 5000)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-timerfd-settime
                        3 nelisp-os-TFD-TIMER-ABSTIME
                        0 0
                        10 0)
                       '(0 0 0 0)))))
    (should (equal nelisp-os--windows-timerfd-table
                   `((3 . [0 6000 t ,nelisp-os-CLOCK-REALTIME]))))
    (should (equal writes '((3000 0 -10000000))))
    (should (equal clock-reads (list nelisp-os-CLOCK-REALTIME)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "SetWaitableTimer"
                          [:sint32 :pointer :pointer :sint32 :pointer :pointer
                           :sint32]
                          (list #xaaaa 3000 0 0 0 0))
                    (list "kernel32" "GetTickCount64" [:uint64] nil))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-supports-alarm-clocks ()
  "Windows timerfd ALARM clocks arm waitable timers with resume enabled."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (clock-reads nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-timerfd-table
         `((3 . [0 0 nil ,nelisp-os-CLOCK-REALTIME-ALARM]))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--windows-timerfd-clock-now-ms)
               (lambda (clockid)
                 (push clockid clock-reads)
                 9000))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "SetWaitableTimer") 1)
                  ((equal fn "GetTickCount64") 5000)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-timerfd-settime
                        3 nelisp-os-TFD-TIMER-ABSTIME
                        0 0
                        10 0)
                       '(0 0 0 0)))))
    (should (equal nelisp-os--windows-timerfd-table
                   `((3 . [0 6000 t ,nelisp-os-CLOCK-REALTIME-ALARM]))))
    (should (equal writes '((3000 0 -10000000))))
    (should (equal clock-reads (list nelisp-os-CLOCK-REALTIME-ALARM)))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "SetWaitableTimer"
                          [:sint32 :pointer :pointer :sint32 :pointer :pointer
                           :sint32]
                          (list #xaaaa 3000 0 0 0 1))
                    (list "kernel32" "GetTickCount64" [:uint64] nil))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-create-supports-alarm-clocks ()
  "Windows timerfd_create accepts supported ALARM clock ids."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-timerfd-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateWaitableTimerW") #xaaaa)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-timerfd-create
                    nelisp-os-CLOCK-BOOTTIME-ALARM 0)
                   3))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . timerfd))))
    (should (equal nelisp-os--windows-timerfd-table
                   `((3 . [0 0 nil ,nelisp-os-CLOCK-BOOTTIME-ALARM]))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateWaitableTimerW"
                          [:pointer :pointer :sint32 :pointer]
                          (list 0 0 0)))))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-poll-uses-waitforsingleobject ()
  "Windows poll reports timerfd readiness from WaitForSingleObject."
  (let ((calls nil)
        (wait-results (list nelisp-os-WIN-WAIT-TIMEOUT
                            nelisp-os-WIN-WAIT-TIMEOUT
                            nelisp-os-WIN-WAIT-OBJECT-0
                            nelisp-os-WIN-WAIT-OBJECT-0))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-timerfd-table '((3 . [0 1000 t]))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject") (pop wait-results))
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 0))))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 nelisp-os-POLLIN))))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xaaaa 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xaaaa 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xaaaa 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xaaaa 0)))))))

(ert-deftest nelisp-stdlib-os-dup2-windows-duplicates-timerfd-kind ()
  "Windows timerfd dup2 preserves timerfd kind and shared state."
  (let ((calls nil)
        (freed nil)
        (state [0 1000 t])
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-timerfd-table nil))
    (setq nelisp-os--windows-timerfd-table (list (cons 3 state)))
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
                  ((equal fn "WaitForSingleObject")
                   nelisp-os-WIN-WAIT-OBJECT-0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 5) 5))
        (should (= (nelisp-os-fcntl 5 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 5 8)) 1))))
    (should (equal nelisp-os--windows-fd-table
                   '((5 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((5 . timerfd) (3 . timerfd))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((5 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal nelisp-os--windows-timerfd-table
                   '((5 . [0 0 nil]) (3 . [0 0 nil]))))
    (should (eq (cdr (assq 3 nelisp-os--windows-timerfd-table))
                (cdr (assq 5 nelisp-os--windows-timerfd-table))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xbbbb 0)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-fcntl-windows-dupfd-duplicates-timerfd-kind ()
  "Windows F_DUPFD preserves timerfd kind and shared timer state."
  (let ((calls nil)
        (freed nil)
        (state [0 1000 t])
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-timerfd-table nil))
    (setq nelisp-os--windows-timerfd-table (list (cons 3 state)))
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
                  ((equal fn "WaitForSingleObject")
                   nelisp-os-WIN-WAIT-OBJECT-0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-DUPFD 10) 10))
        (should (= (nelisp-os-fcntl 10 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os--u64le-from-string (nelisp-os-read 10 8)) 1))))
    (should (equal nelisp-os--windows-next-fd 11))
    (should (equal nelisp-os--windows-fd-table
                   '((10 . #xbbbb) (3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table
                   '((10 . timerfd) (3 . timerfd))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((10 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal nelisp-os--windows-timerfd-table
                   '((10 . [0 0 nil]) (3 . [0 0 nil]))))
    (should (eq (cdr (assq 3 nelisp-os--windows-timerfd-table))
                (cdr (assq 10 nelisp-os--windows-timerfd-table))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xbbbb 0)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-windows-timerfd-can-target-stdout ()
  "Windows timerfd dup2 to stdout preserves timerfd standard fd behavior."
  (let ((calls nil)
        (freed nil)
        (state [0 1000 t])
        (std-handles '(#xdddd #xbbbb #xbbbb))
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table '((3 . timerfd)))
        (nelisp-os--windows-fd-flags-table `((3 . ,nelisp-os-O-NONBLOCK)))
        (nelisp-os--windows-timerfd-table nil))
    (setq nelisp-os--windows-timerfd-table (list (cons 3 state)))
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
                  ((equal fn "GetStdHandle") (pop std-handles))
                  ((equal fn "SetStdHandle") 1)
                  ((equal fn "CloseHandle") 1)
                  ((equal fn "WaitForSingleObject")
                   nelisp-os-WIN-WAIT-OBJECT-0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-dup2 3 nelisp-os-STDOUT)
                   nelisp-os-STDOUT))
        (should (= (nelisp-os-fcntl nelisp-os-STDOUT nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os--u64le-from-string
                    (nelisp-os-read nelisp-os-STDOUT 8))
                   1))))
    (should (equal nelisp-os--windows-fd-kind-table
                   `((,nelisp-os-STDOUT . timerfd) (3 . timerfd))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal nelisp-os--windows-timerfd-table
                   `((,nelisp-os-STDOUT . [0 0 nil])
                     (3 . [0 0 nil]))))
    (should (eq (cdr (assq 3 nelisp-os--windows-timerfd-table))
                (cdr (assq nelisp-os-STDOUT
                           nelisp-os--windows-timerfd-table))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetCurrentProcess" [:pointer] nil)
                    (list "kernel32" "DuplicateHandle"
                          [:sint32 :pointer :pointer :pointer :pointer
                           :uint32 :sint32 :uint32]
                          (list #x9999 #xaaaa #x9999 3000 0 1
                                nelisp-os-WIN-DUPLICATE-SAME-ACCESS))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE #xbbbb))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xdddd))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xbbbb 0)))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-timerfd-windows-validates-and-cleans-up ()
  "Windows timerfd validates arguments and removes timer state on close."
  (let ((calls nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-timerfd-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "CreateWaitableTimerW") #xaaaa)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-timerfd-create
                       nelisp-os-CLOCK-MONOTONIC #x40000000)
                      :type 'nelisp-os-error)
        (should (= (nelisp-os-timerfd-create nelisp-os-CLOCK-MONOTONIC 0) 3))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-SETFL
                                    nelisp-os-O-NONBLOCK)
                   0))
        (should (= (nelisp-os-fcntl 3 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should-error (nelisp-os-read 3 4) :type 'nelisp-os-error)
        (should-error (nelisp-os-write 3 "x") :type 'nelisp-os-error)
        (should-error (nelisp-os-lseek 3 0 nelisp-os-SEEK-SET)
                      :type 'nelisp-os-error)
        (should (= (nelisp-os-stat-mode (nelisp-os-fstat 3))
                   nelisp-os--timerfd-stat-mode))
        (should-not (nelisp-os-close 3))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-timerfd-table)
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "CreateWaitableTimerW"
                          [:pointer :pointer :sint32 :pointer]
                          (list 0 0 0))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xaaaa)))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-uses-loopback-stream ()
  "Windows socketpair uses a temporary loopback listener and returns peers."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-listen)
               (lambda (fd backlog)
                 (push (list 'listen fd backlog) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-accept-inet)
               (lambda (fd)
                 (push (list 'accept fd) calls)
                 (list 5 nelisp-os-INADDR-LOOPBACK 60000)))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX nelisp-os-SOCK-STREAM 0)
                       '(4 . 5)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'listen 3 1)
                    (list 'getsockname 3)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 54321)
                    (list 'accept 3)
                    (list 'close 3))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-seqpacket-uses-stream-pair ()
  "Windows AF_UNIX SOCK_SEQPACKET socketpair uses stream-pair compatibility."
  (let ((calls nil)
        (nelisp-os--windows-socket-peercred-table nil)
        (nelisp-os--windows-socket-fdpass-table nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-socketpair-stream)
               (lambda (type domain)
                 (push (list 'socketpair-stream type domain) calls)
                 (cons 3 4))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX nelisp-os-SOCK-SEQPACKET 0)
                       '(3 . 4)))))
    (should (equal calls
                   (list
                    (list 'socketpair-stream
                          nelisp-os-SOCK-STREAM
                          nelisp-os-AF-UNIX))))
    (should (assq 3 nelisp-os--windows-socket-peercred-table))
    (should (assq 4 nelisp-os--windows-socket-fdpass-table))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-seqpacket-rejects-non-unix ()
  "Windows SOCK_SEQPACKET socketpair is limited to AF_UNIX compatibility."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-socketpair-stream)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--windows-socketpair-dgram)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socketpair nelisp-os-AF-INET
                               nelisp-os-SOCK-SEQPACKET 0)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-socketpair-windows-cleans-up-on-connect-error ()
  "Windows socketpair closes already-created fds when setup fails."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-listen)
               (lambda (fd backlog)
                 (push (list 'listen fd backlog) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 (signal 'nelisp-os-error (list 10061))))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socketpair nelisp-os-AF-INET nelisp-os-SOCK-STREAM 0)
         :type 'nelisp-os-error)))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'listen 3 1)
                    (list 'getsockname 3)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 54321)
                    (list 'close 3)
                    (list 'close 4))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-uses-ipv6-loopback-stream ()
  "Windows AF_INET6 stream socketpair uses an IPv6 loopback listener."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet6)
               (lambda (fd host port)
                 (push (list 'bind6 fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-listen)
               (lambda (fd backlog)
                 (push (list 'listen fd backlog) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet6)
               (lambda (fd)
                 (push (list 'getsockname6 fd) calls)
                 (list nelisp-os-IN6ADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os-connect-inet6)
               (lambda (fd host port)
                 (push (list 'connect6 fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-accept-inet6)
               (lambda (fd)
                 (push (list 'accept6 fd) calls)
                 (list 5 nelisp-os-IN6ADDR-LOOPBACK 60000)))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-INET6 nelisp-os-SOCK-STREAM 0)
                       '(4 . 5)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET6
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'bind6 3 nelisp-os-IN6ADDR-LOOPBACK 0)
                    (list 'listen 3 1)
                    (list 'getsockname6 3)
                    (list 'socket nelisp-os-AF-INET6
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'connect6 4 nelisp-os-IN6ADDR-LOOPBACK 54321)
                    (list 'accept6 3)
                    (list 'close 3))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-uses-connected-udp-pair ()
  "Windows datagram socketpair uses connected UDP loopback sockets."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK
                       (if (= fd 3) 50001 50002))))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX nelisp-os-SOCK-DGRAM 0)
                       '(3 . 4)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 4 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'getsockname 3)
                    (list 'getsockname 4)
                    (list 'connect 3 nelisp-os-INADDR-LOOPBACK 50002)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 50001))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-dgram-cleans-up-on-connect-error ()
  "Windows datagram socketpair closes fds when setup fails."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK
                       (if (= fd 3) 50001 50002))))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 (signal 'nelisp-os-error (list 10061))))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socketpair nelisp-os-AF-INET nelisp-os-SOCK-DGRAM 0)
         :type 'nelisp-os-error)))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 4 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'getsockname 3)
                    (list 'getsockname 4)
                    (list 'connect 3 nelisp-os-INADDR-LOOPBACK 50002)
                    (list 'close 3)
                    (list 'close 4))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-uses-ipv6-connected-udp-pair ()
  "Windows AF_INET6 datagram socketpair uses connected IPv6 UDP loopback sockets."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet6)
               (lambda (fd host port)
                 (push (list 'bind6 fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet6)
               (lambda (fd)
                 (push (list 'getsockname6 fd) calls)
                 (list nelisp-os-IN6ADDR-LOOPBACK
                       (if (= fd 3) 50001 50002))))
              ((symbol-function 'nelisp-os-connect-inet6)
               (lambda (fd host port)
                 (push (list 'connect6 fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-INET6 nelisp-os-SOCK-DGRAM 0)
                       '(3 . 4)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET6
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind6 3 nelisp-os-IN6ADDR-LOOPBACK 0)
                    (list 'socket nelisp-os-AF-INET6
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind6 4 nelisp-os-IN6ADDR-LOOPBACK 0)
                    (list 'getsockname6 3)
                    (list 'getsockname6 4)
                    (list 'connect6 3 nelisp-os-IN6ADDR-LOOPBACK 50002)
                    (list 'connect6 4 nelisp-os-IN6ADDR-LOOPBACK 50001))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-rejects-bad-protocol-before-ffi ()
  "Windows socketpair rejects mismatched protocol before opening sockets."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socketpair nelisp-os-AF-UNIX
                               nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-TCP)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-socketpair-windows-stream-supports-nonblock ()
  "Windows stream socketpair applies SOCK_NONBLOCK after setup succeeds."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-listen)
               (lambda (fd backlog)
                 (push (list 'listen fd backlog) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-accept-inet)
               (lambda (fd)
                 (push (list 'accept fd) calls)
                 (list 5 nelisp-os-INADDR-LOOPBACK 60000)))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (fd cmd arg)
                 (push (list 'fcntl fd cmd arg) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX
                        (logior nelisp-os-SOCK-STREAM
                                nelisp-os-SOCK-NONBLOCK)
                        0)
                       '(4 . 5)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'listen 3 1)
                    (list 'getsockname 3)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-STREAM nelisp-os-IPPROTO-TCP)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 54321)
                    (list 'accept 3)
                    (list 'close 3)
                    (list 'fcntl 4 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                    (list 'fcntl 5 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-seqpacket-supports-nonblock ()
  "Windows SOCK_SEQPACKET compatibility applies SOCK_NONBLOCK after setup."
  (let ((calls nil)
        (nelisp-os--windows-socket-peercred-table nil)
        (nelisp-os--windows-socket-fdpass-table nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-socketpair-stream)
               (lambda (type domain)
                 (push (list 'socketpair-stream type domain) calls)
                 (cons 3 4)))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (fd cmd arg)
                 (push (list 'fcntl fd cmd arg) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX
                        (logior nelisp-os-SOCK-SEQPACKET
                                nelisp-os-SOCK-NONBLOCK)
                        0)
                       '(3 . 4)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socketpair-stream
                          nelisp-os-SOCK-STREAM
                          nelisp-os-AF-UNIX)
                    (list 'fcntl 3 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                    (list 'fcntl 4 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-dgram-supports-nonblock ()
  "Windows datagram socketpair applies SOCK_NONBLOCK after setup succeeds."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK
                       (if (= fd 3) 50001 50002))))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (fd cmd arg)
                 (push (list 'fcntl fd cmd arg) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX
                        (logior nelisp-os-SOCK-DGRAM
                                nelisp-os-SOCK-NONBLOCK)
                        0)
                       '(3 . 4)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'socket nelisp-os-AF-INET
                          nelisp-os-SOCK-DGRAM nelisp-os-IPPROTO-UDP)
                    (list 'bind 4 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'getsockname 3)
                    (list 'getsockname 4)
                    (list 'connect 3 nelisp-os-INADDR-LOOPBACK 50002)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 50001)
                    (list 'fcntl 3 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                    (list 'fcntl 4 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-propagates-cloexec ()
  "Windows socketpair propagates SOCK_CLOEXEC to internal socket creation."
  (let ((calls nil)
        (socket-fds '(3 4)))
    (cl-letf (((symbol-function 'nelisp-os-socket)
               (lambda (domain type proto)
                 (push (list 'socket domain type proto) calls)
                 (pop socket-fds)))
              ((symbol-function 'nelisp-os-bind-inet)
               (lambda (fd host port)
                 (push (list 'bind fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-listen)
               (lambda (fd backlog)
                 (push (list 'listen fd backlog) calls)
                 0))
              ((symbol-function 'nelisp-os-getsockname-inet)
               (lambda (fd)
                 (push (list 'getsockname fd) calls)
                 (list nelisp-os-INADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os-connect-inet)
               (lambda (fd host port)
                 (push (list 'connect fd host port) calls)
                 0))
              ((symbol-function 'nelisp-os-accept-inet)
               (lambda (fd)
                 (push (list 'accept fd) calls)
                 (list 5 nelisp-os-INADDR-LOOPBACK 60000)))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX
                        (logior nelisp-os-SOCK-STREAM
                                nelisp-os-SOCK-CLOEXEC)
                        0)
                       '(4 . 5)))))
    (should (equal (nreverse calls)
                   (list
                    (list 'socket nelisp-os-AF-INET
                          (logior nelisp-os-SOCK-STREAM
                                  nelisp-os-SOCK-CLOEXEC)
                          nelisp-os-IPPROTO-TCP)
                    (list 'bind 3 nelisp-os-INADDR-LOOPBACK 0)
                    (list 'listen 3 1)
                    (list 'getsockname 3)
                    (list 'socket nelisp-os-AF-INET
                          (logior nelisp-os-SOCK-STREAM
                                  nelisp-os-SOCK-CLOEXEC)
                          nelisp-os-IPPROTO-TCP)
                    (list 'connect 4 nelisp-os-INADDR-LOOPBACK 54321)
                    (list 'accept 3)
                    (list 'close 3))))))

(ert-deftest nelisp-stdlib-os-socketpair-windows-nonblock-cleans-up-on-fcntl-error ()
  "Windows socketpair closes both fds when post-setup nonblocking fails."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp-os--windows-socketpair-stream)
               (lambda (&optional _type _domain)
                 (push '(socketpair-stream) calls)
                 '(4 . 5)))
              ((symbol-function 'nelisp-os-fcntl)
               (lambda (fd cmd arg)
                 (push (list 'fcntl fd cmd arg) calls)
                 (if (= fd 5)
                     (signal 'nelisp-os-error (list 10022))
                   0)))
              ((symbol-function 'nelisp-os-close)
               (lambda (fd)
                 (push (list 'close fd) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socketpair nelisp-os-AF-UNIX
                               (logior nelisp-os-SOCK-STREAM
                                       nelisp-os-SOCK-NONBLOCK)
                               0)
         :type 'nelisp-os-error)))
    (should (equal (nreverse calls)
                   (list
                    '(socketpair-stream)
                    (list 'fcntl 4 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                    (list 'fcntl 5 nelisp-os-F-SETFL nelisp-os-O-NONBLOCK)
                    '(close 4)
                    '(close 5))))))

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

(ert-deftest nelisp-stdlib-os-socket-windows-seqpacket-uses-af-unix-stream ()
  "Windows AF_UNIX SOCK_SEQPACKET socket maps to a stream Winsock socket."
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
                                     nelisp-os-SOCK-SEQPACKET
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

(ert-deftest nelisp-stdlib-os-socket-windows-seqpacket-rejects-non-unix ()
  "Windows non-AF_UNIX SOCK_SEQPACKET sockets reject before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-socket nelisp-os-AF-INET
                           nelisp-os-SOCK-SEQPACKET
                           nelisp-os-IPPROTO-TCP)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-socket-windows-supports-sock-nonblock ()
  "Windows SOCK_NONBLOCK creates a socket then sets FIONBIO."
  (let ((alloc-next 3000)
        (calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "socket") #xabcdef)
                  ((equal fn "ioctlsocket") 0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-INET
                                     (logior nelisp-os-SOCK-STREAM
                                             nelisp-os-SOCK-NONBLOCK)
                                     nelisp-os-IPPROTO-TCP)
                   3))))
    (should (equal (nreverse writes) '((3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "socket"
                          [:pointer :sint32 :sint32 :sint32]
                          (list nelisp-os-AF-INET nelisp-os-SOCK-STREAM
                                nelisp-os-IPPROTO-TCP))
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-socket-windows-seqpacket-supports-nonblock ()
  "Windows AF_UNIX SOCK_SEQPACKET socket supports SOCK_NONBLOCK."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "socket") #xabcdef)
                  ((equal fn "ioctlsocket") 0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-UNIX
                                     (logior nelisp-os-SOCK-SEQPACKET
                                             nelisp-os-SOCK-NONBLOCK)
                                     0)
                   3))))
    (should (equal (nreverse writes) '((3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "socket"
                          [:pointer :sint32 :sint32 :sint32]
                          (list nelisp-os-AF-UNIX
                                nelisp-os-SOCK-STREAM
                                0))
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-socket-windows-supports-sock-cloexec ()
  "Windows SOCK_CLOEXEC uses WSASocketW with NO_HANDLE_INHERIT."
  (let ((call nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 (cond
                  ((equal fn "WSASocketW") #xabcdef)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-INET
                                     (logior nelisp-os-SOCK-STREAM
                                             nelisp-os-SOCK-CLOEXEC)
                                     nelisp-os-IPPROTO-TCP)
                   3))))
    (should (equal call
                   (list "ws2_32" "WSASocketW"
                         [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                         (list nelisp-os-AF-INET nelisp-os-SOCK-STREAM
                               nelisp-os-IPPROTO-TCP
                               0
                               0
                               (logior
                                nelisp-os-WIN-WSA-FLAG-OVERLAPPED
                                nelisp-os-WIN-WSA-FLAG-NO-HANDLE-INHERIT)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))))

(ert-deftest nelisp-stdlib-os-socket-windows-seqpacket-supports-cloexec ()
  "Windows AF_UNIX SOCK_SEQPACKET socket supports SOCK_CLOEXEC."
  (let ((call nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 (cond
                  ((equal fn "WSASocketW") #xabcdef)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket nelisp-os-AF-UNIX
                                     (logior nelisp-os-SOCK-SEQPACKET
                                             nelisp-os-SOCK-CLOEXEC)
                                     0)
                   3))))
    (should (equal call
                   (list "ws2_32" "WSASocketW"
                         [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                         (list nelisp-os-AF-UNIX
                               nelisp-os-SOCK-STREAM
                               0
                               0
                               0
                               (logior
                                nelisp-os-WIN-WSA-FLAG-OVERLAPPED
                                nelisp-os-WIN-WSA-FLAG-NO-HANDLE-INHERIT)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))))

(ert-deftest nelisp-stdlib-os-socket-windows-supports-nonblock-cloexec ()
  "Windows SOCK_NONBLOCK plus SOCK_CLOEXEC uses WSASocketW then FIONBIO."
  (let ((alloc-next 3000)
        (calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-winsock-started-p t))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-u32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WSASocketW") #xabcdef)
                  ((equal fn "ioctlsocket") 0)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-socket
                    nelisp-os-AF-INET
                    (logior nelisp-os-SOCK-STREAM
                            nelisp-os-SOCK-NONBLOCK
                            nelisp-os-SOCK-CLOEXEC)
                    nelisp-os-IPPROTO-TCP)
                   3))))
    (should (equal (nreverse writes) '((3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "WSASocketW"
                          [:pointer :sint32 :sint32 :sint32 :pointer :uint32 :uint32]
                          (list nelisp-os-AF-INET nelisp-os-SOCK-STREAM
                                nelisp-os-IPPROTO-TCP
                                0
                                0
                                (logior
                                 nelisp-os-WIN-WSA-FLAG-OVERLAPPED
                                 nelisp-os-WIN-WSA-FLAG-NO-HANDLE-INHERIT)))
                    (list "ws2_32" "ioctlsocket"
                          [:sint32 :pointer :uint32 :pointer]
                          (list #xabcdef nelisp-os-WIN-FIONBIO 3000)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xabcdef))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-NONBLOCK))))
    (should (equal freed '(3000)))))

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

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-tcp-nodelay ()
  "Windows TCP_NODELAY translates through Winsock setsockopt."
  (let ((call nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-NODELAY 1)
                   0))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-IPPROTO-TCP
                               nelisp-os-WIN-TCP-NODELAY
                               3000
                               4))))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-tcp-keepalive-options ()
  "Windows TCP keepalive int options translate through Winsock setsockopt."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPIDLE 7200)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPINTVL 75)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPCNT 9)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 7200)
                     (3000 0 75)
                     (3000 0 9))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPIDLE 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPINTVL 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPCNT 3000 4)))))
    (should (equal freed '(3000 3000 3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-tcp-generic-options ()
  "Windows TCP generic int options translate through Winsock setsockopt."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-MAXSEG 1460)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-TIMESTAMPS 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-FASTOPEN 1)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 1460)
                     (3000 0 1)
                     (3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-MAXSEG 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-TIMESTAMPS 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-FASTOPEN 3000 4)))))
    (should (equal freed '(3000 3000 3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-udp-options ()
  "Windows IPPROTO_UDP int options translate through Winsock setsockopt."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-UDP nelisp-os-UDP-NOCHECKSUM 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-UDP nelisp-os-UDP-SEND-MSG-SIZE 1200)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-UDP
                    nelisp-os-UDP-RECV-MAX-COALESCED-SIZE 1400)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-UDP
                    nelisp-os-UDP-CHECKSUM-COVERAGE 20)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 1)
                     (3000 0 1200)
                     (3000 0 1400)
                     (3000 0 20))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-NOCHECKSUM 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-SEND-MSG-SIZE 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-RECV-MAX-COALESCED-SIZE
                                3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-CHECKSUM-COVERAGE
                                3000 4)))))
    (should (equal freed '(3000 3000 3000 3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-ttl ()
  "Windows IP_TTL translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-TTL 64)
                   0))))
    (should (equal writes '((3000 0 64))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-TTL
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-tos ()
  "Windows IP_TOS translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-TOS #x10)
                   0))))
    (should (equal writes '((3000 0 16))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-TOS
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-recvttl ()
  "Windows IP_RECVTTL translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-RECVTTL 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-RECVTTL
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-recvtos ()
  "Windows IP_RECVTOS translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-RECVTOS 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-RECVTOS
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-hdrincl ()
  "Windows IP_HDRINCL translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-HDRINCL 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-HDRINCL
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-multicast-ttl ()
  "Windows IP_MULTICAST_TTL translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-TTL 32)
                   0))))
    (should (equal writes '((3000 0 32))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-TTL
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-unicast-if ()
  "Windows IP_UNICAST_IF translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-UNICAST-IF 7)
                   0))))
    (should (equal writes '((3000 0 7))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-UNICAST-IF
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-pktinfo ()
  "Windows IP_PKTINFO translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-PKTINFO 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-PKTINFO
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-mtu-discover ()
  "Windows IP_MTU_DISCOVER translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MTU-DISCOVER 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MTU-DISCOVER
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-multicast-if ()
  "Windows IP_MULTICAST_IF translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-IF 7)
                   0))))
    (should (equal writes '((3000 0 7))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-IF
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ip-multicast-loop ()
  "Windows IP_MULTICAST_LOOP translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-LOOP 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-LOOP
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-v6only ()
  "Windows IPV6_V6ONLY translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-V6ONLY 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-V6ONLY
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-generic-options ()
  "Windows IPv6 generic int options translate through Winsock setsockopt."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-DONTFRAG 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MTU-DISCOVER 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-TCLASS 32)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVTCLASS 1)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 1)
                     (3000 0 1)
                     (3000 0 32)
                     (3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-DONTFRAG 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-MTU-DISCOVER 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-TCLASS 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-RECVTCLASS 3000 4)))))
    (should (equal freed '(3000 3000 3000 3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-metadata-options ()
  "Windows IPv6 metadata int options translate through Winsock setsockopt."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-HDRINCL 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVPKTINFO 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVHOPLIMIT 1)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 1)
                     (3000 0 1)
                     (3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-HDRINCL 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-PKTINFO 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-HOPLIMIT 3000 4)))))
    (should (equal freed '(3000 3000 3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-unicast-hops ()
  "Windows IPV6_UNICAST_HOPS translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-UNICAST-HOPS 64)
                   0))))
    (should (equal writes '((3000 0 64))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-UNICAST-HOPS
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-unicast-if ()
  "Windows IPV6_UNICAST_IF translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-UNICAST-IF 7)
                   0))))
    (should (equal writes '((3000 0 7))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-UNICAST-IF
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-multicast-hops ()
  "Windows IPV6_MULTICAST_HOPS translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-HOPS 32)
                   0))))
    (should (equal writes '((3000 0 32))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-HOPS
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-multicast-if ()
  "Windows IPV6_MULTICAST_IF translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-IF 7)
                   0))))
    (should (equal writes '((3000 0 7))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-IF
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-ipv6-multicast-loop ()
  "Windows IPV6_MULTICAST_LOOP translates through Winsock setsockopt."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-LOOP 1)
                   0))))
    (should (equal writes '((3000 0 1))))
    (should (equal call
                   (list "ws2_32" "setsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-LOOP
                               3000
                               4))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-supports-buffer-options ()
  "Windows SOL_SOCKET int options translate through Winsock."
  (let ((calls nil)
        (writes nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-DEBUG 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-SNDBUF 4096)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-RCVBUF 8192)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-BROADCAST 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-DONTROUTE 1)
                   0))
        (should (= (nelisp-os-setsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-OOBINLINE 1)
                   0))))
    (should (equal (nreverse writes)
                   '((3000 0 1)
                     (3000 0 4096)
                     (3000 0 8192)
                     (3000 0 1)
                     (3000 0 1)
                     (3000 0 1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-DEBUG 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-SNDBUF 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-RCVBUF 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-BROADCAST 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-DONTROUTE 3000 4))
                    (list "ws2_32" "setsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-OOBINLINE 3000 4)))))))

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

(ert-deftest nelisp-stdlib-os-shutdown-windows-uses-winsock ()
  "Windows shutdown dispatches to Winsock for socket-kind fds."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-shutdown 3 nelisp-os-SHUT-WR) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "shutdown"
                          [:sint32 :pointer :sint32]
                          (list #xabcdef nelisp-os-SHUT-WR)))))))

(ert-deftest nelisp-stdlib-os-shutdown-windows-rejects-bad-how-before-ffi ()
  "Windows shutdown rejects unsupported HOW values before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-shutdown 3 99) :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-shutdown-windows-rejects-non-socket-fd-before-ffi ()
  "Windows shutdown rejects regular HANDLE fds before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-shutdown 3 nelisp-os-SHUT-RDWR)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-sendto-inet-windows-uses-winsock ()
  "Windows AF_INET sendto encodes sockaddr_in and dispatches to Winsock."
  (let ((alloc-next 3000)
        (calls nil)
        (encoded nil)
        (written nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written (list ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-sendto-inet
                    3 "ping" nelisp-os-INADDR-LOOPBACK 9999 0)
                   4))))
    (should (equal encoded
                   (list 3000 nelisp-os-INADDR-LOOPBACK 9999)))
    (should (equal written '(4000 "ping")))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "sendto"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :sint32]
                          (list #xabcdef 4000 4 0 3000
                                nelisp-os--sockaddr-in-len)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-sendto-inet-windows-rejects-non-socket-before-alloc ()
  "Windows AF_INET sendto rejects regular HANDLE fds before allocation or FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-sendto-inet 3 "ping" nelisp-os-INADDR-LOOPBACK 9999)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet-windows-uses-winsock ()
  "Windows AF_INET recvfrom decodes payload and peer sockaddr_in."
  (let ((alloc-next 3000)
        (calls nil)
        (writes nil)
        (decoded nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val)
                 (push (list ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (buf)
                 (setq decoded buf)
                 (cons nelisp-os-INADDR-LOOPBACK 9999)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr len)
                 (should (= ptr 3000))
                 (should (= len 4))
                 "pong"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-recvfrom-inet 3 16 0)
                       (list "pong" nelisp-os-INADDR-LOOPBACK 9999)))))
    (should (equal (nreverse writes)
                   `((5000 0 ,nelisp-os--sockaddr-in-len))))
    (should (= decoded 4000))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "recvfrom"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :pointer]
                          (list #xabcdef 3000 16 0 4000 5000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000)))))

(ert-deftest nelisp-stdlib-os-recvfrom-inet-windows-rejects-non-socket-before-alloc ()
  "Windows AF_INET recvfrom rejects regular HANDLE fds before allocation or FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet 3 16)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet-windows-rejects-negative-size-before-alloc ()
  "Windows AF_INET recvfrom rejects negative MAX-BYTES before allocation."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet 3 -1)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-sendto-inet6-windows-uses-winsock ()
  "Windows AF_INET6 sendto encodes sockaddr_in6 and dispatches to Winsock."
  (let ((alloc-next 3000)
        (calls nil)
        (encoded nil)
        (written nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written (list ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-sendto-inet6
                    3 "ping" nelisp-os-IN6ADDR-LOOPBACK 9999 0)
                   4))))
    (should (equal encoded
                   (list 3000 nelisp-os-IN6ADDR-LOOPBACK 9999)))
    (should (equal written '(4000 "ping")))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "sendto"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :sint32]
                          (list #xabcdef 4000 4 0 3000
                                nelisp-os--sockaddr-in6-len)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-sendto-inet6-windows-rejects-non-socket-before-alloc ()
  "Windows AF_INET6 sendto rejects regular HANDLE fds before allocation or FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-sendto-inet6 3 "ping" nelisp-os-IN6ADDR-LOOPBACK 9999)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-windows-uses-winsock ()
  "Windows AF_INET6 recvfrom decodes payload and peer sockaddr_in6."
  (let ((alloc-next 3000)
        (calls nil)
        (writes nil)
        (decoded nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val)
                 (push (list ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6)
               (lambda (buf)
                 (setq decoded buf)
                 (cons nelisp-os-IN6ADDR-LOOPBACK 9999)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr len)
                 (should (= ptr 3000))
                 (should (= len 4))
                 "pong"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-recvfrom-inet6 3 16 0)
                       (list "pong" nelisp-os-IN6ADDR-LOOPBACK 9999)))))
    (should (equal (nreverse writes)
                   `((5000 0 ,nelisp-os--sockaddr-in6-len))))
    (should (= decoded 4000))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "recvfrom"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :pointer]
                          (list #xabcdef 3000 16 0 4000 5000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000)))))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-windows-rejects-non-socket-before-alloc ()
  "Windows AF_INET6 recvfrom rejects regular HANDLE fds before allocation or FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet6 3 16)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-windows-rejects-negative-size-before-alloc ()
  "Windows AF_INET6 recvfrom rejects negative MAX-BYTES before allocation."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet6 3 -1)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-sendto-inet6-scoped-windows-uses-winsock ()
  "Windows scoped AF_INET6 sendto encodes sockaddr_in6 and dispatches to Winsock."
  (let ((alloc-next 3000)
        (calls nil)
        (encoded nil)
        (written nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6-scoped)
               (lambda (buf host port flowinfo scope-id)
                 (setq encoded (list buf host port flowinfo scope-id))))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written (list ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-sendto-inet6-scoped
                    3 "ping" nelisp-os-IN6ADDR-LOOPBACK 9999 1234 7 0)
                   4))))
    (should (equal encoded
                   (list 3000 nelisp-os-IN6ADDR-LOOPBACK 9999 1234 7)))
    (should (equal written '(4000 "ping")))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "sendto"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :sint32]
                          (list #xabcdef 4000 4 0 3000
                                nelisp-os--sockaddr-in6-scoped-len)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-sendto-inet6-scoped-windows-rejects-non-socket-before-alloc ()
  "Windows scoped AF_INET6 sendto rejects regular HANDLE fds before allocation."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-sendto-inet6-scoped
          3 "ping" nelisp-os-IN6ADDR-LOOPBACK 9999 1234 7)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-scoped-windows-uses-winsock ()
  "Windows scoped AF_INET6 recvfrom decodes payload and scoped peer sockaddr."
  (let ((alloc-next 3000)
        (calls nil)
        (writes nil)
        (decoded nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free)
               (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val)
                 (push (list ptr off val) writes)
                 val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6-scoped)
               (lambda (buf)
                 (setq decoded buf)
                 (list nelisp-os-IN6ADDR-LOOPBACK 9999 1234 7)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr len)
                 (should (= ptr 3000))
                 (should (= len 4))
                 "pong"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 4)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-recvfrom-inet6-scoped 3 16 0)
                       (list "pong" nelisp-os-IN6ADDR-LOOPBACK
                             9999 1234 7)))))
    (should (equal (nreverse writes)
                   `((5000 0 ,nelisp-os--sockaddr-in6-scoped-len))))
    (should (= decoded 4000))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "recvfrom"
                          [:sint32 :pointer :pointer :sint32 :sint32
                           :pointer :pointer]
                          (list #xabcdef 3000 16 0 4000 5000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000)))))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-scoped-windows-rejects-non-socket-before-alloc ()
  "Windows scoped AF_INET6 recvfrom rejects regular HANDLE fds before allocation."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet6-scoped 3 16)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-recvfrom-inet6-scoped-windows-rejects-negative-size-before-alloc ()
  "Windows scoped AF_INET6 recvfrom rejects negative MAX-BYTES before allocation."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-recvfrom-inet6-scoped 3 -1)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-tcp-nodelay ()
  "Windows TCP_NODELAY translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-NODELAY)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-IPPROTO-TCP
                               nelisp-os-WIN-TCP-NODELAY
                               3000
                               4000))))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-tcp-keepalive-options ()
  "Windows TCP keepalive int options translate through Winsock getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr _off)
                 (cdr (assq ptr '((3000 . 7200)
                                  (5000 . 75)
                                  (7000 . 9))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPIDLE)
                   7200))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPINTVL)
                   75))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-KEEPCNT)
                   9))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPIDLE 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPINTVL 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-KEEPCNT 7000 8000)))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-tcp-generic-options ()
  "Windows TCP generic int options translate through Winsock getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr _off)
                 (cdr (assq ptr '((3000 . 1460)
                                  (5000 . 1)
                                  (7000 . 1))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-MAXSEG)
                   1460))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-TIMESTAMPS)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-TCP nelisp-os-TCP-FASTOPEN)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-MAXSEG 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-TIMESTAMPS 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-TCP
                                nelisp-os-WIN-TCP-FASTOPEN 7000 8000)))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-udp-options ()
  "Windows IPPROTO_UDP int options translate through Winsock getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr _off)
                 (cdr (assq ptr '((3000 . 1)
                                  (5000 . 1200)
                                  (7000 . 1400)
                                  (9000 . 20))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-UDP nelisp-os-UDP-NOCHECKSUM)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-UDP nelisp-os-UDP-SEND-MSG-SIZE)
                   1200))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-UDP
                    nelisp-os-UDP-RECV-MAX-COALESCED-SIZE)
                   1400))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-UDP
                    nelisp-os-UDP-CHECKSUM-COVERAGE)
                   20))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-NOCHECKSUM 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-SEND-MSG-SIZE 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-RECV-MAX-COALESCED-SIZE
                                7000 8000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-IPPROTO-UDP
                                nelisp-os-WIN-UDP-CHECKSUM-COVERAGE
                                9000 10000)))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000 9000 10000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-ttl ()
  "Windows IP_TTL translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 64))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-TTL)
                   64))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-TTL
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-tos ()
  "Windows IP_TOS translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) #x10))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-TOS)
                   #x10))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-TOS
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-recvttl ()
  "Windows IP_RECVTTL translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-RECVTTL)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-RECVTTL
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-recvtos ()
  "Windows IP_RECVTOS translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-RECVTOS)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-RECVTOS
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-hdrincl ()
  "Windows IP_HDRINCL translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-HDRINCL)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-HDRINCL
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-multicast-ttl ()
  "Windows IP_MULTICAST_TTL translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 32))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-TTL)
                   32))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-TTL
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-unicast-if ()
  "Windows IP_UNICAST_IF translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 7))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-UNICAST-IF)
                   7))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-UNICAST-IF
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-pktinfo ()
  "Windows IP_PKTINFO translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-PKTINFO)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-PKTINFO
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-mtu-discover ()
  "Windows IP_MTU_DISCOVER translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MTU-DISCOVER)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MTU-DISCOVER
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-mtu ()
  "Windows IP_MTU translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1500))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MTU)
                   1500))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MTU
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-multicast-if ()
  "Windows IP_MULTICAST_IF translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 7))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-IF)
                   7))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-IF
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ip-multicast-loop ()
  "Windows IP_MULTICAST_LOOP translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IP nelisp-os-IP-MULTICAST-LOOP)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IP
                               nelisp-os-WIN-IP-MULTICAST-LOOP
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-v6only ()
  "Windows IPV6_V6ONLY translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-V6ONLY)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-V6ONLY
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-generic-options ()
  "Windows IPv6 generic int options translate through Winsock getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr _off)
                 (cdr (assq ptr '((3000 . 1)
                                  (5000 . 1)
                                  (7000 . 1500)
                                  (9000 . 32)
                                  (11000 . 1))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-DONTFRAG)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MTU-DISCOVER)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MTU)
                   1500))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-TCLASS)
                   32))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVTCLASS)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-DONTFRAG 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-MTU-DISCOVER 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-MTU 7000 8000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-TCLASS 9000 10000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-RECVTCLASS 11000 12000)))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000 9000 10000
                     11000 12000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-metadata-options ()
  "Windows IPv6 metadata int options translate through Winsock getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr _off)
                 (cdr (assq ptr '((3000 . 1)
                                  (5000 . 1)
                                  (7000 . 1))))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-HDRINCL)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVPKTINFO)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-RECVHOPLIMIT)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-HDRINCL 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-PKTINFO 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-IPPROTO-IPV6
                                nelisp-os-WIN-IPV6-HOPLIMIT 7000 8000)))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-unicast-hops ()
  "Windows IPV6_UNICAST_HOPS translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 64))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-UNICAST-HOPS)
                   64))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-UNICAST-HOPS
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-unicast-if ()
  "Windows IPV6_UNICAST_IF translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 7))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-UNICAST-IF)
                   7))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-UNICAST-IF
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-multicast-hops ()
  "Windows IPV6_MULTICAST_HOPS translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 32))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-HOPS)
                   32))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-HOPS
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-multicast-if ()
  "Windows IPV6_MULTICAST_IF translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 7))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-IF)
                   7))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-IF
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-ipv6-multicast-loop ()
  "Windows IPV6_MULTICAST_LOOP translates through Winsock getsockopt."
  (let ((alloc-next 3000)
        (call nil)
        (freed nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32) (lambda (_ptr _off) 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MULTICAST-LOOP)
                   1))))
    (should (equal call
                   (list "ws2_32" "getsockopt"
                         [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                         (list #xabcdef
                               nelisp-os-WIN-IPPROTO-IPV6
                               nelisp-os-WIN-IPV6-MULTICAST-LOOP
                               3000
                               4000))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-buffer-options ()
  "Windows SOL_SOCKET int options translate through getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (reads '(1 4096 8192 1 1 1))
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (_ptr _off) (pop reads)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-DEBUG)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-SNDBUF)
                   4096))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-RCVBUF)
                   8192))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-BROADCAST)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-DONTROUTE)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-OOBINLINE)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-DEBUG 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-SNDBUF 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-RCVBUF 7000 8000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-BROADCAST 9000 10000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-DONTROUTE 11000 12000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-OOBINLINE 13000 14000)))))))

(ert-deftest nelisp-stdlib-os-getsockopt-int-windows-supports-error-and-type ()
  "Windows get-only SOL_SOCKET options translate through getsockopt."
  (let ((alloc-next 3000)
        (calls nil)
        (reads '(0 1 1))
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (_ptr) nil))
              ((symbol-function 'nelisp-os-write-i32) (lambda (_ptr _off val) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (_ptr _off) (pop reads)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-ERROR)
                   0))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-TYPE)
                   1))
        (should (= (nelisp-os-getsockopt-int
                    3 nelisp-os-SOL-SOCKET nelisp-os-SO-ACCEPTCONN)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-ERROR 3000 4000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-TYPE 5000 6000))
                    (list "ws2_32" "getsockopt"
                          [:sint32 :pointer :sint32 :sint32 :pointer :pointer]
                          (list #xabcdef nelisp-os-WIN-SOL-SOCKET
                                nelisp-os-WIN-SO-ACCEPTCONN 7000 8000)))))))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-rejects-get-only-options ()
  "Windows setsockopt rejects get-only SOL_SOCKET options before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-SOL-SOCKET nelisp-os-SO-ERROR 0)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-SOL-SOCKET nelisp-os-SO-TYPE nelisp-os-SOCK-STREAM)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-SOL-SOCKET nelisp-os-SO-ACCEPTCONN 1)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-rejects-ip-mtu ()
  "Windows setsockopt rejects get-only IP_MTU before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-IPPROTO-IP nelisp-os-IP-MTU 1500)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-setsockopt-int-windows-rejects-ipv6-mtu ()
  "Windows setsockopt rejects get-only IPV6_MTU before Winsock FFI."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-setsockopt-int
          3 nelisp-os-IPPROTO-IPV6 nelisp-os-IPV6-MTU 1500)
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

(ert-deftest nelisp-stdlib-os-accept-inet-windows-preserves-nonblock-flag ()
  "Windows accepted socket fds keep listener O_NONBLOCK tracking."
  (let ((freed nil)
        (nelisp-os--windows-next-fd 4)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . socket)))
        (nelisp-os--windows-fd-flags-table
         `((3 . ,nelisp-os-O-NONBLOCK))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (n) (if (= n nelisp-os--sockaddr-in-len) 3000 4000)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32) (lambda (&rest _args) nil))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (_ptr) (cons nelisp-os-INADDR-LOOPBACK 4444)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (_dll fn _sig &rest _args)
                 (if (equal fn "accept")
                     #x123456
                   (error "unexpected ffi call %S" fn)))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-accept-inet 3)
                       (list 4 nelisp-os-INADDR-LOOPBACK 4444)))
        (should (= (nelisp-os-fcntl 4 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((4 . ,nelisp-os-O-NONBLOCK)
                     (3 . ,nelisp-os-O-NONBLOCK))))
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

(ert-deftest nelisp-stdlib-os-bind-unix-abstract-windows-maps-to-temp-path ()
  "Windows abstract AF_UNIX bind maps the name to a temporary filesystem path."
  (let* ((tmpdir (make-temp-file "nelisp-afunix-abstract-" t))
         (called nil)
         (bound nil)
         (nelisp-os--windows-abstract-unix-dir tmpdir)
         (expected (nelisp-os--windows-abstract-unix-path "svc")))
    (unwind-protect
        (progn
          (write-region "" nil expected nil 'silent)
          (cl-letf (((symbol-function 'nelisp--syscall)
                     (lambda (&rest _args) (setq called t)))
                    ((symbol-function 'nelisp-os--libc-call)
                     (lambda (&rest _args) (setq called t)))
                    ((symbol-function 'nelisp-os--alloc)
                     (lambda (&rest _args) (setq called t)))
                    ((symbol-function 'nelisp-os-bind-unix)
                     (lambda (fd path)
                       (setq bound (list fd path))
                       0)))
            (let ((system-type 'windows-nt))
              (should (= (nelisp-os-bind-unix-abstract 3 "svc") 0))))
          (should (equal bound (list 3 expected)))
          (should-not (file-exists-p expected))
          (should-not called))
      (delete-directory tmpdir t))))

(ert-deftest nelisp-stdlib-os-unix-abstract-windows-path-fits-temp-dir ()
  "Windows abstract AF_UNIX paths stay within the sockaddr_un sun_path limit."
  (let* ((tmpdir (make-temp-file "nelisp-afunix-abstract-" t))
         (nelisp-os--windows-abstract-unix-dir tmpdir)
         (path (nelisp-os--windows-abstract-unix-path "svc")))
    (unwind-protect
        (progn
          (should (string-prefix-p
                   (file-name-as-directory tmpdir)
                   path))
          (should (< (string-bytes path) nelisp-os-SUN-PATH-MAX)))
      (delete-directory tmpdir t))))

(ert-deftest nelisp-stdlib-os-connect-unix-abstract-windows-maps-to-temp-path ()
  "Windows abstract AF_UNIX connect uses the same temporary filesystem path."
  (let ((called nil)
        (connected nil)
        (nelisp-os--windows-abstract-unix-dir "C:/tmp/nelisp-afunix"))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os-connect-unix)
               (lambda (fd path)
                 (setq connected (list fd path))
                 0)))
      (let* ((system-type 'windows-nt)
             (expected (nelisp-os--windows-abstract-unix-path "svc")))
        (should (= (nelisp-os-connect-unix-abstract 4 "svc") 0))
        (should (equal connected (list 4 expected)))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-unix-abstract-windows-validates-name-before-ffi ()
  "Windows abstract AF_UNIX rejects NUL-containing names before delegation."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os-bind-unix)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os-connect-unix)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-bind-unix-abstract 3 "bad\0name")
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-connect-unix-abstract 3 "bad\0name")
         :type 'nelisp-os-error)))
    (should-not called)))

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

(ert-deftest nelisp-stdlib-os-getpeername-inet-windows-uses-winsock ()
  "Windows IPv4 getpeername uses Winsock and decodes sockaddr_in."
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
        (should (equal (nelisp-os-getpeername-inet 3)
                       (list nelisp-os-INADDR-LOOPBACK 4444)))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getpeername"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getsockname-inet6-windows-uses-winsock ()
  "Windows IPv6 getsockname uses Winsock and decodes sockaddr_in6."
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
        (should (equal (nelisp-os-getsockname-inet6 3)
                       (list nelisp-os-IN6ADDR-LOOPBACK 4444)))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-in6-len)))
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

(ert-deftest nelisp-stdlib-os-getsockname-inet6-scoped-windows-uses-winsock ()
  "Windows scoped IPv6 getsockname uses Winsock and decodes sockaddr_in6."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (decoded nil)
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
                 (setq decoded t)
                 (list nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getsockname-inet6-scoped 3)
                       (list nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))))
    (should decoded)
    (should (equal len-write
                   (list 4000 0 nelisp-os--sockaddr-in6-scoped-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getsockname"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getpeername-inet6-scoped-windows-uses-winsock ()
  "Windows scoped IPv6 getpeername uses Winsock and decodes sockaddr_in6."
  (let ((calls nil)
        (freed nil)
        (len-write nil)
        (decoded nil)
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
                 (setq decoded t)
                 (list nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getpeername-inet6-scoped 3)
                       (list nelisp-os-IN6ADDR-LOOPBACK 4444 9 2)))))
    (should decoded)
    (should (equal len-write
                   (list 4000 0 nelisp-os--sockaddr-in6-scoped-len)))
    (should (equal (nreverse calls)
                   (list
                    (list "ws2_32" "getpeername"
                          [:sint32 :pointer :pointer :pointer]
                          (list #xabcdef 3000 4000)))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-getpeername-unix-windows-uses-winsock ()
  "Windows AF_UNIX getpeername uses Winsock and decodes sockaddr_un."
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
               (lambda (ptr off) (setq len-read (list ptr off)) 9))
              ((symbol-function 'nelisp-os--decode-sockaddr-un)
               (lambda (ptr len)
                 (should (= ptr 3000))
                 (should (= len 9))
                 (setq decoded t)
                 "sockpath"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-getpeername-unix 3) "sockpath"))))
    (should decoded)
    (should (equal len-write (list 4000 0 nelisp-os--sockaddr-un-len)))
    (should (equal len-read (list 4000 0)))
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

(ert-deftest nelisp-stdlib-os-kill-windows-uses-registered-child-handle ()
  "Windows kill reuses a registered child process HANDLE and keeps it waitable."
  (let ((calls nil)
        (nelisp-os--windows-process-table nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "TerminateProcess") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (nelisp-os--windows-register-process 1234 #xabcdef)
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-kill 1234 nelisp-os-SIGKILL) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "TerminateProcess"
                          [:sint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-SIGKILL)))))
    (should (equal nelisp-os--windows-process-table
                   '((1234 . #xabcdef))))))

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

(ert-deftest nelisp-stdlib-os-wait-windows-uses-registered-child-handle ()
  "Windows wait reuses a registered child process HANDLE."
  (let ((calls nil)
        (freed nil)
        (nelisp-os--windows-process-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 9))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-OBJECT-0)
                  ((equal fn "GetExitCodeProcess") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (nelisp-os--windows-register-process 1234 #xabcdef)
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait 1234 0)
                       (cons 1234 (ash 9 8))))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef nelisp-os-WIN-INFINITE))
                    (list "kernel32" "GetExitCodeProcess"
                          [:sint32 :pointer :pointer]
                          (list #xabcdef 3000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xabcdef)))))
    (should-not nelisp-os--windows-process-table)
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-wait-windows-keeps-registered-handle-on-wnohang ()
  "Windows WNOHANG wait keeps registered HANDLEs while the child is running."
  (let ((calls nil)
        (nelisp-os--windows-process-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (error "unexpected alloc")))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject") nelisp-os-WIN-WAIT-TIMEOUT)
                  (t (error "unexpected ffi call %S" fn))))))
      (nelisp-os--windows-register-process 1234 #xabcdef)
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait 1234 nelisp-os-WNOHANG)
                       '(0 . 0)))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0)))))
    (should (equal nelisp-os--windows-process-table
                   '((1234 . #xabcdef))))))

(ert-deftest nelisp-stdlib-os-wait-windows-any-registered-child ()
  "Windows wait(-1) reaps one registered child through WaitForMultipleObjects."
  (let ((calls nil)
        (freed nil)
        (handle-writes nil)
        (alloc-next 3000)
        (nelisp-os--windows-process-table '((111 . #xaaaa) (222 . #xbbbb))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (prog1 alloc-next
                   (setq alloc-next (+ alloc-next 1000)))))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) handle-writes)
                 val))
              ((symbol-function 'nelisp-os-read-u32)
               (lambda (ptr off)
                 (should (= ptr 4000))
                 (should (= off 0))
                 12))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForMultipleObjects")
                   (+ nelisp-os-WIN-WAIT-OBJECT-0 1))
                  ((equal fn "GetExitCodeProcess") 1)
                  ((equal fn "CloseHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait -1 0)
                       (cons 222 (ash 12 8))))))
    (should (equal (nreverse handle-writes)
                   (list
                    (list 3000 0 #xaaaa)
                    (list 3000 8 #xbbbb))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForMultipleObjects"
                          [:uint32 :uint32 :pointer :sint32 :uint32]
                          (list 2 3000 0 nelisp-os-WIN-INFINITE))
                    (list "kernel32" "GetExitCodeProcess"
                          [:sint32 :pointer :pointer]
                          (list #xbbbb 4000))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb)))))
    (should (equal nelisp-os--windows-process-table
                   '((111 . #xaaaa))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-wait-windows-any-registered-wnohang-timeout ()
  "Windows wait(-1 WNOHANG) keeps all registered children on timeout."
  (let ((calls nil)
        (freed nil)
        (handle-writes nil)
        (nelisp-os--windows-process-table '((111 . #xaaaa) (222 . #xbbbb))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val)
                 (push (list ptr off val) handle-writes)
                 val))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForMultipleObjects")
                   nelisp-os-WIN-WAIT-TIMEOUT)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-wait -1 nelisp-os-WNOHANG)
                       '(0 . 0)))))
    (should (equal (nreverse handle-writes)
                   (list
                    (list 3000 0 #xaaaa)
                    (list 3000 8 #xbbbb))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForMultipleObjects"
                          [:uint32 :uint32 :pointer :sint32 :uint32]
                          (list 2 3000 0 0)))))
    (should (equal nelisp-os--windows-process-table
                   '((111 . #xaaaa) (222 . #xbbbb))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-wait-windows-any-registered-empty-errors ()
  "Windows wait(-1) reports ECHILD when no child HANDLE is registered."
  (let ((called nil)
        (nelisp-os--windows-process-table nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-wait -1 0)
                      :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-wait-windows-any-registered-too-many-errors ()
  "Windows wait(-1) rejects child HANDLE counts above the native wait limit."
  (let ((called nil)
        (nelisp-os--windows-process-table
         (nelisp-stdlib-os-test--windows-cells
          (1+ nelisp-os-WIN-MAXIMUM-WAIT-OBJECTS))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error (nelisp-os-wait -1 0)
                      :type 'nelisp-os-error)))
    (should-not called)))

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

(ert-deftest nelisp-stdlib-os-read-write-windows-process-errors-before-ffi ()
  "Windows process fd read/write signal EINVAL like Linux pidfd."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (let ((read-err (should-error (nelisp-os-read 3 1)
                                      :type 'nelisp-os-error))
              (write-err (should-error (nelisp-os-write 3 "x")
                                       :type 'nelisp-os-error)))
          (should (equal (cdr read-err) '(22)))
          (should (equal (cdr write-err) '(22))))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-read-windows-stdin-socket-uses-recv ()
  "Windows stdin with socket kind reads through Winsock recv."
  (let ((calls nil)
        (freed nil)
        (read-request nil)
        (nelisp-os--windows-fd-kind-table
         `((,nelisp-os-STDIN . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (setq read-request (list ptr n))
                 "data"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xabcdef)
                  ((equal fn "recv") 4)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-read nelisp-os-STDIN 16) "data"))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-INPUT-HANDLE))
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

(ert-deftest nelisp-stdlib-os-write-windows-stdout-socket-uses-send ()
  "Windows stdout with socket kind writes through Winsock send."
  (let ((calls nil)
        (freed nil)
        (written-string nil)
        (nelisp-os--windows-fd-kind-table
         `((,nelisp-os-STDOUT . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written-string (cons ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xabcdef)
                  ((equal fn "send") 4)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (= (nelisp-os-write nelisp-os-STDOUT "data") 4))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
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
  "Windows poll rejects regular HANDLE fds before allocation or WSAPoll."
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
    (should-not freed)))

(ert-deftest nelisp-stdlib-os-poll-windows-eventfd-uses-counter-readiness ()
  "Windows poll reports synthetic eventfd POLLIN/POLLOUT readiness."
  (let ((called nil)
        (nelisp-os--windows-fd-table '((3 . 0) (4 . 0)))
        (nelisp-os--windows-fd-kind-table '((3 . eventfd) (4 . eventfd)))
        (nelisp-os--windows-eventfd-table
         `((3 . (5 . 0))
           (4 . (,nelisp-os--eventfd-max-counter . 0)))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should (equal
                 (nelisp-os-poll
                  (list (cons 3 (logior nelisp-os-POLLIN nelisp-os-POLLOUT))
                        (cons 4 (logior nelisp-os-POLLIN nelisp-os-POLLOUT)))
                 0)
                 (list (cons 3 (logior nelisp-os-POLLIN nelisp-os-POLLOUT))
                       (cons 4 nelisp-os-POLLIN))))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-poll-windows-eventfd-socket-mix-preserves-order ()
  "Windows poll combines eventfd readiness with WSAPoll socket results."
  (let ((call nil)
        (freed nil)
        (writes nil)
        (nelisp-os--windows-fd-table '((3 . 0) (4 . #xbbbb)))
        (nelisp-os--windows-fd-kind-table '((3 . eventfd) (4 . socket)))
        (nelisp-os--windows-eventfd-table '((3 . (1 . 0)))))
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
                 (should (= off 10))
                 nelisp-os-POLLOUT))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'windows-nt))
        (should (equal
                 (nelisp-os-poll
                  (list (cons 3 nelisp-os-POLLIN)
                        (cons 4 nelisp-os-POLLOUT))
                 5000)
                 (list (cons 3 nelisp-os-POLLIN)
                       (cons 4 nelisp-os-POLLOUT))))))
    (should (equal call
                   (list "ws2_32" "WSAPoll"
                         [:sint32 :pointer :uint32 :sint32]
                         (list 3000 1 0))))
    (should (equal (nreverse writes)
                   (list
                    (list 'i64 3000 0 #xbbbb)
                    (list 'i16 3000 8 nelisp-os-POLLOUT))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-poll-windows-process-uses-waitforsingleobject ()
  "Windows poll reports process fd readiness from WaitForSingleObject."
  (let ((calls nil)
        (wait-results (list nelisp-os-WIN-WAIT-TIMEOUT
                            nelisp-os-WIN-WAIT-TIMEOUT
                            nelisp-os-WIN-WAIT-OBJECT-0
                            nelisp-os-WIN-WAIT-OBJECT-0))
        (nelisp-os--windows-fd-table '((3 . #xabcdef)))
        (nelisp-os--windows-fd-kind-table '((3 . process))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject")
                   (pop wait-results))
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 0))))
        (should (equal (nelisp-os-poll (list (cons 3 nelisp-os-POLLIN)) 0)
                       (list (cons 3 nelisp-os-POLLIN))))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #xabcdef 0)))))))

(ert-deftest nelisp-stdlib-os-poll-windows-process-socket-mix-preserves-order ()
  "Windows poll combines process readiness with WSAPoll socket results."
  (let ((calls nil)
        (freed nil)
        (writes nil)
        (nelisp-os--windows-fd-table '((3 . #x1111) (4 . #x2222)))
        (nelisp-os--windows-fd-kind-table '((3 . process) (4 . socket))))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i64)
               (lambda (ptr off val) (push (list 'i64 ptr off val) writes) val))
              ((symbol-function 'nelisp-os-write-i16)
               (lambda (ptr off val) (push (list 'i16 ptr off val) writes) val))
              ((symbol-function 'nelisp-os-read-i16)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 10))
                 nelisp-os-POLLOUT))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "WaitForSingleObject")
                   nelisp-os-WIN-WAIT-OBJECT-0)
                  ((equal fn "WSAPoll") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should
         (equal (nelisp-os-poll
                 (list (cons 3 nelisp-os-POLLIN)
                       (cons 4 nelisp-os-POLLOUT))
                500)
               (list (cons 3 nelisp-os-POLLIN)
                      (cons 4 nelisp-os-POLLOUT))))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #x1111 0))
                    (list "ws2_32" "WSAPoll"
                          [:sint32 :pointer :uint32 :sint32]
                          (list 3000 1 0))
                    (list "kernel32" "WaitForSingleObject"
                          [:uint32 :pointer :uint32]
                          (list #x1111 0)))))
    (should (equal (nreverse writes)
                   (list
                    (list 'i64 3000 0 #x2222)
                    (list 'i16 3000 8 nelisp-os-POLLOUT))))
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

(ert-deftest nelisp-stdlib-os-close-windows-stdout-closes-standard-handle ()
  "Windows stdout close clears the standard HANDLE slot and tracked flags."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-flags-table
         `((,nelisp-os-STDOUT . ,(logior nelisp-os-O-WRONLY
                                         nelisp-os-O-APPEND))
           (3 . ,nelisp-os-O-RDWR))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xbbbb)
                  ((equal fn "CloseHandle") 1)
                  ((equal fn "SetStdHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-not (nelisp-os-close nelisp-os-STDOUT))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "kernel32" "CloseHandle"
                          [:sint32 :pointer]
                          (list #xbbbb))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE 0)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-RDWR))))))

(ert-deftest nelisp-stdlib-os-close-windows-stdout-socket-uses-closesocket ()
  "Windows stdout close uses closesocket when stdout tracks socket kind."
  (let ((calls nil)
        (nelisp-os--windows-fd-table '((3 . #xaaaa)))
        (nelisp-os--windows-fd-kind-table
         `((,nelisp-os-STDOUT . socket) (3 . socket)))
        (nelisp-os--windows-fd-flags-table
         `((,nelisp-os-STDOUT . ,nelisp-os-O-NONBLOCK)
           (3 . ,nelisp-os-O-NONBLOCK))))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (cond
                  ((equal fn "GetStdHandle") #xbbbb)
                  ((equal fn "closesocket") 0)
                  ((equal fn "SetStdHandle") 1)
                  (t (error "unexpected ffi call %S" fn))))))
      (let ((system-type 'windows-nt))
        (should-not (nelisp-os-close nelisp-os-STDOUT))))
    (should (equal (nreverse calls)
                   (list
                    (list "kernel32" "GetStdHandle"
                          [:pointer :sint32]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE))
                    (list "ws2_32" "closesocket"
                          [:sint32 :pointer]
                          (list #xbbbb))
                    (list "kernel32" "SetStdHandle"
                          [:sint32 :sint32 :pointer]
                          (list nelisp-os-WIN-STD-OUTPUT-HANDLE 0)))))
    (should (equal nelisp-os--windows-fd-table '((3 . #xaaaa))))
    (should (equal nelisp-os--windows-fd-kind-table '((3 . socket))))
    (should (equal nelisp-os--windows-fd-flags-table
                   `((3 . ,nelisp-os-O-NONBLOCK))))))

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

(ert-deftest nelisp-stdlib-os-signalfd-windows-creates-updates-and-cleans-up ()
  "Windows signalfd uses a synthetic fd with tracked mask and descriptor state."
  (let ((called nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-signalfd-table nil)
        (nelisp-os--windows-signalfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (flags (logior nelisp-os-SFD-NONBLOCK nelisp-os-SFD-CLOEXEC))
             (fd (nelisp-os-signalfd
                  -1
                  (list nelisp-os-SIGTERM nelisp-os-SIGUSR1 nelisp-os-SIGTERM)
                  flags))
             (state (cdr (assq fd nelisp-os--windows-signalfd-table))))
        (should (= fd 3))
        (should (= nelisp-os--windows-next-fd 4))
        (should (equal nelisp-os--windows-fd-table '((3 . 0))))
        (should (equal nelisp-os--windows-fd-kind-table '((3 . signalfd))))
        (should (equal nelisp-os--windows-fd-flags-table
                       `((3 . ,nelisp-os-O-NONBLOCK))))
        (should (equal nelisp-os--windows-signalfd-fd-flags-table
                       `((3 . ,nelisp-os-FD-CLOEXEC))))
        (should (equal (aref state 0)
                       (list nelisp-os-SIGUSR1 nelisp-os-SIGTERM)))
        (should (equal (nelisp-os-signalfd-read fd 1) nil))
        (should (= (nelisp-os-signalfd fd (list nelisp-os-SIGUSR2) 0)
                   fd))
        (should (equal (aref state 0) (list nelisp-os-SIGUSR2)))
        (should-not (nelisp-os-close fd))))
    (should-not nelisp-os--windows-fd-table)
    (should-not nelisp-os--windows-fd-kind-table)
    (should-not nelisp-os--windows-fd-flags-table)
    (should-not nelisp-os--windows-signalfd-table)
    (should-not nelisp-os--windows-signalfd-fd-flags-table)
    (should-not called)))

(ert-deftest nelisp-stdlib-os-signalfd-windows-validates-before-ffi ()
  "Windows signalfd rejects invalid inputs without falling through to FFI."
  (let ((called nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'windows-nt))
        (should-error
         (nelisp-os-signalfd -1 (list nelisp-os-SIGTERM) #x40000000)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-signalfd -1 '(0) 0)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-signalfd 9 (list nelisp-os-SIGTERM) 0)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-signalfd-read 9 1)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-signalfd-read 9 -1)
         :type 'nelisp-os-error)))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-signalfd-windows-supports-fd-operations ()
  "Windows signalfd supports synthetic fd metadata, fcntl, dup, poll and I/O errors."
  (let ((called nil)
        (nelisp-os--windows-next-fd 3)
        (nelisp-os--windows-fd-table nil)
        (nelisp-os--windows-fd-kind-table nil)
        (nelisp-os--windows-fd-flags-table nil)
        (nelisp-os--windows-signalfd-table nil)
        (nelisp-os--windows-signalfd-fd-flags-table nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let* ((system-type 'windows-nt)
             (fd (nelisp-os-signalfd
                  -1
                  (list nelisp-os-SIGTERM)
                  (logior nelisp-os-SFD-NONBLOCK nelisp-os-SFD-CLOEXEC)))
             (dupfd (nelisp-os-fcntl fd nelisp-os-F-DUPFD 5))
             (state (cdr (assq fd nelisp-os--windows-signalfd-table)))
             (dup-state (cdr (assq dupfd nelisp-os--windows-signalfd-table))))
        (should (= dupfd 5))
        (should (eq state dup-state))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-GETFD 0)
                   nelisp-os-FD-CLOEXEC))
        (should (= (nelisp-os-fcntl dupfd nelisp-os-F-GETFD 0) 0))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-SETFL 0) 0))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-GETFL 0) 0))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-SETFD 0) 0))
        (should (= (nelisp-os-fcntl fd nelisp-os-F-GETFD 0) 0))
        (should-error
         (nelisp-os-fcntl fd nelisp-os-F-SETFL nelisp-os-O-APPEND)
         :type 'nelisp-os-error)
        (should (equal (nelisp-os-poll (list (cons fd nelisp-os-POLLIN)) 0)
                       (list (cons fd 0))))
        (let ((st (nelisp-os-fstat fd)))
          (should (= (nelisp-os-stat-size st) 0))
          (should (= (nelisp-os-stat-mode st)
                     nelisp-os--signalfd-stat-mode))
          (should (= (nelisp-os-stat-nlink st) 1)))
        (should-error
         (nelisp-os-lseek fd 0 nelisp-os-SEEK-SET)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-read fd 8)
         :type 'nelisp-os-error)
        (should-error
         (nelisp-os-write fd "")
         :type 'nelisp-os-error)
        (should (= (nelisp-os-signalfd
                    fd
                    (list nelisp-os-SIGUSR1 nelisp-os-SIGUSR2)
                    0)
                   fd))
        (should (equal (aref dup-state 0)
                       (list nelisp-os-SIGUSR1 nelisp-os-SIGUSR2)))
        (should-not (nelisp-os-close dupfd))
        (should (assq fd nelisp-os--windows-signalfd-table))
        (should-not (assq dupfd nelisp-os--windows-signalfd-table))))
    (should-not called)))

;; ---------------------------------------------------------------------------
;; Darwin / macOS OS compatibility smoke selector.
;; ---------------------------------------------------------------------------

(ert-deftest nelisp-stdlib-os-exit-darwin-uses-libc-exit ()
  "Darwin process exit routes through libc _exit, not Windows ExitProcess."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 :unreachable)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (eq (nelisp-os-exit 42) :unreachable))))
    (should (equal call
                   (list "libc" "_exit" [:void :int] (list 42))))))

(ert-deftest nelisp-stdlib-os-open-darwin-uses-libc-open ()
  "Darwin regular file open uses libc open."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 7)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-open "/tmp/nelisp" nelisp-os-O-RDONLY 0) 7))))
    (should (equal call
                   (list "libc" "open"
                         [:sint32 :string :sint32 :uint32]
                         (list "/tmp/nelisp" nelisp-os-O-RDONLY 0))))))

(ert-deftest nelisp-stdlib-os-close-darwin-uses-libc-close ()
  "Darwin close uses libc close when direct syscalls are unavailable."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should-not (nelisp-os-close 7))))
    (should (equal call
                   (list "libc" "close" [:int :int] (list 7))))))

(ert-deftest nelisp-stdlib-os-read-darwin-uses-libc-read ()
  "Darwin read allocates a mutable buffer and routes through libc read."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr n)
                 (should (= ptr 3000))
                 (should (= n 3))
                 "abc"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 3)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-read 9 16) "abc"))))
    (should (equal call
                   (list "libc" "read"
                         [:sint64 :sint32 :pointer :uint64]
                         (list 9 3000 16))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-write-darwin-uses-libc-write ()
  "Darwin write preserves binary-safe buffer routing through libc write."
  (let ((call nil)
        (freed nil)
        (written nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written (list ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 3)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-write nelisp-os-STDOUT "abc") 3))))
    (should (equal written '(3000 "abc")))
    (should (equal call
                   (list "libc" "write"
                         [:sint64 :sint32 :pointer :uint64]
                         (list nelisp-os-STDOUT 3000 3))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-pipe-darwin-uses-libc-pipe ()
  "Darwin pipe uses libc pipe and decodes the two int fds."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (pcase off
                   (0 11)
                   (4 12)
                   (_ (error "unexpected pipe offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-pipe) '(11 . 12)))))
    (should (equal call
                   (list "libc" "pipe" [:sint32 :pointer] (list 3000))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-network-byte-order-darwin-uses-libc ()
  "Darwin sockaddr byte-order helpers route through libc."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig arg)
                 (setq call (list dll fn sig arg))
                 #x3412)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os--htons #x1234) #x3412))))
    (should (equal call
                   (list "libc" "htons" [:uint16 :uint16] #x1234)))))

(ert-deftest nelisp-stdlib-os-vm-darwin-falls-back-to-libc ()
  "Darwin VM wrappers use libc when direct syscall primitives are unavailable."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args)
                 (ert-fail "Darwin VM wrapper unexpectedly used nelisp--syscall")))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (pcase fn
                   ("mmap" #x100000000)
                   (_ 0)))))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-mmap 4096 nelisp-os-PROT-READ
                                   nelisp-os-MAP-PRIVATE -1 0)
                   #x100000000))
        (should (= (nelisp-os-mprotect #x100000000 4096 nelisp-os-PROT-READ)
                   0))
        (should (= (nelisp-os-munmap #x100000000 4096) 0))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "mmap"
                          [:pointer :pointer :uint64 :sint32 :sint32 :sint32 :sint64]
                          (list 0 4096 nelisp-os-PROT-READ
                                nelisp-os-MAP-PRIVATE -1 0))
                    (list "libc" "mprotect"
                          [:sint32 :pointer :uint64 :sint32]
                          (list #x100000000 4096 nelisp-os-PROT-READ))
                    (list "libc" "munmap"
                          [:sint32 :pointer :uint64]
                          (list #x100000000 4096)))))))

(ert-deftest nelisp-stdlib-os-basic-syscalls-darwin-fall-back-to-libc ()
  "Darwin getpid/socket/listen/lseek use libc without direct syscall primitives."
  (let ((calls nil))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args)
                 (ert-fail "Darwin basic wrapper unexpectedly used nelisp--syscall")))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (pcase fn
                   ("getpid" 1234)
                   ("socket" 55)
                   ("listen" 0)
                   ("lseek" 99)
                   (_ (error "unexpected libc call %S" fn))))))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-getpid) 1234))
        (should (= (nelisp-os-socket nelisp-os-AF-INET nelisp-os-SOCK-STREAM 0)
                   55))
        (should (= (nelisp-os-listen 55 16) 0))
        (should (= (nelisp-os-lseek 7 99 nelisp-os-SEEK-SET) 99))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "getpid" [:sint32] nil)
                    (list "libc" "socket"
                          [:sint32 :sint32 :sint32 :sint32]
                          (list nelisp-os-AF-INET nelisp-os-SOCK-STREAM 0))
                    (list "libc" "listen"
                          [:sint32 :sint32 :sint32]
                          (list 55 16))
                    (list "libc" "lseek"
                          [:sint64 :sint32 :sint64 :sint32]
                          (list 7 99 nelisp-os-SEEK-SET)))))))

(ert-deftest nelisp-stdlib-os-linux-only-apis-darwin-error-before-syscall ()
  "Darwin rejects Linux-only fd APIs before syscall/libc allocation paths."
  (let ((called nil)
        (forms
         (list
          (lambda () (nelisp-os-pidfd-open 1234 0))
          (lambda () (nelisp-os-pidfd-send-signal 7 nelisp-os-SIGTERM 0))
          (lambda () (nelisp-os-inotify-init 0))
          (lambda () (nelisp-os-inotify-add-watch 7 "/tmp" nelisp-os-IN-ALL-EVENTS))
          (lambda () (nelisp-os-inotify-rm-watch 7 1))
          (lambda () (nelisp-os-inotify-read 7 1))
          (lambda () (nelisp-os-eventfd 0 0))
          (lambda () (nelisp-os-timerfd-create nelisp-os-CLOCK-MONOTONIC 0))
          (lambda () (nelisp-os-timerfd-settime 7 0 0 0 1 0))
          (lambda () (nelisp-os-timerfd-gettime 7)))))
    (cl-letf (((symbol-function 'nelisp--syscall)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (&rest _args) (setq called t)))
              ((symbol-function 'nelisp-os--alloc)
               (lambda (&rest _args) (setq called t))))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (dolist (fn forms)
          (condition-case err
              (progn
                (funcall fn)
                (ert-fail "expected nelisp-os-error"))
            (nelisp-os-error
             (should (equal (cdr err) '(38))))))))
    (should-not called)))

(ert-deftest nelisp-stdlib-os-sockopts-darwin-use-libc ()
  "Darwin int-valued socket options use libc setsockopt/getsockopt."
  (let ((calls nil)
        (freed nil)
        (writes nil)
        (allocs (list 3000 4000 5000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc)
               (lambda (_n)
                 (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 4000))
                 (should (= off 0))
                 1))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-setsockopt-int
                    8 nelisp-os-SOL-SOCKET nelisp-os-SO-REUSEADDR 1)
                   0))
        (should (= (nelisp-os-getsockopt-int
                    8 nelisp-os-SOL-SOCKET nelisp-os-SO-REUSEADDR)
                   1))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "setsockopt"
                          [:sint32 :sint32 :sint32 :sint32 :pointer :uint32]
                          (list 8 nelisp-os-SOL-SOCKET
                                nelisp-os-SO-REUSEADDR 3000 4))
                    (list "libc" "getsockopt"
                          [:sint32 :sint32 :sint32 :sint32 :pointer :pointer]
                          (list 8 nelisp-os-SOL-SOCKET
                                nelisp-os-SO-REUSEADDR 4000 5000)))))
    (should writes)
    (should (equal (sort freed #'<) '(3000 4000 5000)))))

(ert-deftest nelisp-stdlib-os-shutdown-darwin-uses-libc ()
  "Darwin shutdown uses libc shutdown and validates HOW first."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-shutdown 8 nelisp-os-SHUT-RDWR) 0))))
    (should (equal call
                   (list "libc" "shutdown"
                         [:sint32 :sint32 :sint32]
                         (list 8 nelisp-os-SHUT-RDWR))))))

(ert-deftest nelisp-stdlib-os-poll-darwin-uses-libc ()
  "Darwin poll marshals pollfd entries and calls libc poll."
  (let ((call nil)
        (freed nil)
        (writes nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list 'i32 ptr off val) writes) val))
              ((symbol-function 'nelisp-os-write-i16)
               (lambda (ptr off val) (push (list 'i16 ptr off val) writes) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (_ptr off)
                 (pcase off
                   (0 7)
                   (8 8)
                   (_ (error "unexpected poll fd offset %S" off)))))
              ((symbol-function 'nelisp-os-read-i16)
               (lambda (_ptr off)
                 (pcase off
                   (6 nelisp-os-POLLIN)
                   (14 0)
                   (_ (error "unexpected poll revents offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-poll
                        (list (cons 7 nelisp-os-POLLIN)
                              (cons 8 nelisp-os-POLLOUT))
                        0)
                       (list (cons 7 nelisp-os-POLLIN)
                             (cons 8 0))))))
    (should (equal call
                   (list "libc" "poll"
                         [:sint32 :pointer :uint64 :sint32]
                         (list 3000 2 0))))
    (should (= (length writes) 4))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-dup2-darwin-uses-libc-dup2 ()
  "Darwin dup2 uses libc dup2 when direct syscall primitives are unavailable."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 9)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-dup2 7 9) 9))))
    (should (equal call
                   (list "libc" "dup2"
                         [:sint32 :sint32 :sint32]
                         (list 7 9))))))

(ert-deftest nelisp-stdlib-os-fcntl-darwin-uses-libc-fcntl ()
  "Darwin fcntl uses libc fcntl for integer-valued commands."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 nelisp-os-O-NONBLOCK)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-fcntl 7 nelisp-os-F-GETFL 0)
                   nelisp-os-O-NONBLOCK))))
    (should (equal call
                   (list "libc" "fcntl"
                         [:sint32 :sint32 :sint32 :sint64]
                         (list 7 nelisp-os-F-GETFL 0))))))

(ert-deftest nelisp-stdlib-os-fstat-darwin-uses-libc-fstat ()
  "Darwin fstat allocates a stat buffer, calls libc fstat, and decodes fields."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i64)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (pcase off
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-size))) 123)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-mtime))) 456)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-mtime-nsec))) 789)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-atime))) 111)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-atime-nsec))) 222)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-ctime))) 333)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-ctime-nsec))) 444)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-nlink))) 2)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-ino))) 555)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-dev))) 666)
                   (_ (error "unexpected stat i64 offset %S" off)))))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (pcase off
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-mode))) #o100644)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-uid))) 501)
                   ((pred (lambda (x) (= x nelisp-os--stat-offset-gid))) 20)
                   (_ (error "unexpected stat i32 offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin))
        (let ((st (nelisp-os-fstat 7)))
          (should (= (nelisp-os-stat-size st) 123))
          (should (= (nelisp-os-stat-mode st) #o100644))
          (should (= (nelisp-os-stat-mtime st) 456))
          (should (= (nelisp-os-stat-mtime-nsec st) 789))
          (should (= (nelisp-os-stat-atime st) 111))
          (should (= (nelisp-os-stat-atime-nsec st) 222))
          (should (= (nelisp-os-stat-ctime st) 333))
          (should (= (nelisp-os-stat-ctime-nsec st) 444))
          (should (= (nelisp-os-stat-nlink st) 2))
          (should (= (nelisp-os-stat-uid st) 501))
          (should (= (nelisp-os-stat-gid st) 20))
          (should (= (nelisp-os-stat-ino st) 555))
          (should (= (nelisp-os-stat-dev st) 666)))))
    (should (equal call
                   (list "libc" "fstat"
                         [:sint32 :sint32 :pointer]
                         (list 7 3000))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-execve-darwin-marshals-argv-envp ()
  "Darwin execve uses libc execve with allocated argv/envp arrays and cleanup."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--build-cstr-array)
               (lambda (strs)
                 (pcase strs
                   (`("prog" "arg") (cons 3000 '(3008 3016)))
                   (`("A=B") (cons 4000 '(4008)))
                   (_ (error "unexpected cstr array %S" strs)))))
              ((symbol-function 'nelisp-os--free-cstr-array)
               (lambda (pair) (push pair freed)))
              ((symbol-function 'nelisp-os--ffi-errno-signal)
               (lambda () :errno))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 -1)))
      (let ((system-type 'darwin))
        (should (eq (nelisp-os-execve "/tmp/prog" '("prog" "arg") '("A=B"))
                    :errno))))
    (should (equal call
                   (list "libc" "execve"
                         [:sint32 :string :pointer :pointer]
                         (list "/tmp/prog" 3000 4000))))
    (should (equal freed '((4000 4008) (3000 3008 3016))))))

(ert-deftest nelisp-stdlib-os-wait-darwin-uses-libc-wait4 ()
  "Darwin wait uses libc wait4 and decodes the child status buffer."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (should (= off 0))
                 #x2a00))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 1234)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-wait -1 0) (cons 1234 #x2a00)))))
    (should (equal call
                   (list "libc" "wait4"
                         [:sint32 :sint32 :pointer :sint32 :pointer]
                         (list -1 3000 0 0))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-kill-darwin-uses-libc-kill ()
  "Darwin kill uses libc kill for POSIX signal delivery."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-kill 1234 nelisp-os-SIGTERM) 0))))
    (should (equal call
                   (list "libc" "kill"
                         [:sint32 :sint32 :sint32]
                         (list 1234 nelisp-os-SIGTERM))))))

(ert-deftest nelisp-stdlib-os-getppid-darwin-uses-libc-getppid ()
  "Darwin getppid uses libc getppid when direct syscall primitives are unavailable."
  (let ((call nil))
    (cl-letf (((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 4321)))
      (let ((system-type 'darwin)
            (nelisp-os--use-direct-syscall nil))
        (should (= (nelisp-os-getppid) 4321))))
    (should (equal call
                   (list "libc" "getppid" [:sint32] nil)))))

(ert-deftest nelisp-stdlib-os-socketpair-darwin-uses-libc-socketpair ()
  "Darwin socketpair uses libc socketpair and decodes the two int fds."
  (let ((call nil)
        (freed nil))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) 3000))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 3000))
                 (pcase off
                   (0 17)
                   (4 18)
                   (_ (error "unexpected socketpair offset %S" off)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 0)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-socketpair
                        nelisp-os-AF-UNIX nelisp-os-SOCK-STREAM 0)
                       '(17 . 18)))))
    (should (equal call
                   (list "libc" "socketpair"
                         [:sint32 :sint32 :sint32 :sint32 :pointer]
                         (list nelisp-os-AF-UNIX
                               nelisp-os-SOCK-STREAM
                               0
                               3000))))
    (should (equal freed '(3000)))))

(ert-deftest nelisp-stdlib-os-sendto-inet-darwin-uses-libc-sendto ()
  "Darwin AF_INET sendto encodes sockaddr_in and dispatches to libc."
  (let ((call nil)
        (encoded nil)
        (written nil)
        (freed nil)
        (allocs '(3000 4000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in)
               (lambda (buf host port) (setq encoded (list buf host port))))
              ((symbol-function 'nelisp-os--write-bytes)
               (lambda (ptr str) (setq written (list ptr str))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 4)))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-sendto-inet
                    7 "ping" nelisp-os-INADDR-LOOPBACK 9999)
                   4))))
    (should (equal encoded
                   (list 3000 nelisp-os-INADDR-LOOPBACK 9999)))
    (should (equal written '(4000 "ping")))
    (should (equal call
                   (list "libc" "sendto"
                         [:sint32 :sint32 :pointer :uint32 :sint32
                          :pointer :uint32]
                         (list 7 4000 4 0
                               3000 nelisp-os--sockaddr-in-len))))
    (should (equal (sort freed #'<) '(3000 4000)))))

(ert-deftest nelisp-stdlib-os-recvfrom-inet-darwin-uses-libc-recvfrom ()
  "Darwin AF_INET recvfrom decodes payload and peer sockaddr through libc."
  (let ((call nil)
        (writes nil)
        (freed nil)
        (allocs '(3000 4000 5000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (buf)
                 (should (= buf 4000))
                 (cons nelisp-os-INADDR-LOOPBACK 9999)))
              ((symbol-function 'nelisp-os--read-bytes)
               (lambda (ptr len)
                 (should (= ptr 3000))
                 (should (= len 4))
                 "pong"))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (setq call (list dll fn sig args))
                 4)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-recvfrom-inet 7 16)
                       (list "pong" nelisp-os-INADDR-LOOPBACK 9999)))))
    (should (equal writes (list (list 5000 0 nelisp-os--sockaddr-in-len))))
    (should (equal call
                   (list "libc" "recvfrom"
                         [:sint32 :sint32 :pointer :uint32 :sint32
                          :pointer :pointer]
                         (list 7 3000 16 0 4000 5000))))
    (should (equal (sort freed #'<) '(3000 4000 5000)))))

(ert-deftest nelisp-stdlib-os-inet-darwin-bind-connect-accept-use-libc ()
  "Darwin AF_INET bind/connect/accept use libc and decode accepted peers."
  (let ((calls nil)
        (encoded nil)
        (writes nil)
        (freed nil)
        (allocs '(3000 4000 5000 6000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in)
               (lambda (buf host port) (push (list buf host port) encoded)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (buf)
                 (should (= buf 5000))
                 (cons nelisp-os-INADDR-LOOPBACK 54321)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (pcase fn
                   ((or "bind" "connect") 0)
                   ("accept" 77)
                   (_ (error "unexpected libc call %S" fn))))))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-bind-inet
                    7 nelisp-os-INADDR-LOOPBACK 1111)
                   0))
        (should (= (nelisp-os-connect-inet
                    8 nelisp-os-INADDR-LOOPBACK 2222)
                   0))
        (should (equal (nelisp-os-accept-inet 9)
                       (list 77 nelisp-os-INADDR-LOOPBACK 54321)))))
    (should (equal (nreverse encoded)
                   (list
                    (list 3000 nelisp-os-INADDR-LOOPBACK 1111)
                    (list 4000 nelisp-os-INADDR-LOOPBACK 2222))))
    (should (equal writes (list (list 6000 0 nelisp-os--sockaddr-in-len))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "bind"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 7 3000 nelisp-os--sockaddr-in-len))
                    (list "libc" "connect"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 8 4000 nelisp-os--sockaddr-in-len))
                    (list "libc" "accept"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 9 5000 6000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000 6000)))))

(ert-deftest nelisp-stdlib-os-unix-darwin-bind-connect-accept-use-libc ()
  "Darwin AF_UNIX bind/connect/accept use libc sockaddr_un paths."
  (let ((calls nil)
        (encoded nil)
        (writes nil)
        (freed nil)
        (allocs '(3000 4000 5000 6000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-un)
               (lambda (buf path)
                 (push (list buf path) encoded)
                 11))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= ptr 6000))
                 (should (= off 0))
                 2))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (pcase fn
                   ((or "bind" "connect") 0)
                   ("accept" 77)
                   (_ (error "unexpected libc call %S" fn))))))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-bind-unix 7 "/tmp/a.sock") 0))
        (should (= (nelisp-os-connect-unix 8 "/tmp/a.sock") 0))
        (should (equal (nelisp-os-accept-unix 9) '(77 . "")))))
    (should (equal (nreverse encoded)
                   (list (list 3000 "/tmp/a.sock")
                         (list 4000 "/tmp/a.sock"))))
    (should (equal writes (list (list 6000 0 nelisp-os--sockaddr-un-len))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "bind"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 7 3000 11))
                    (list "libc" "connect"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 8 4000 11))
                    (list "libc" "accept"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 9 5000 6000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000 6000)))))

(ert-deftest nelisp-stdlib-os-inet6-darwin-bind-connect-accept-use-libc ()
  "Darwin AF_INET6 bind/connect/accept use libc and decode accepted peers."
  (let ((calls nil)
        (encoded nil)
        (writes nil)
        (freed nil)
        (allocs '(3000 4000 5000 6000))
        (groups nelisp-os-IN6ADDR-LOOPBACK))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os--encode-sockaddr-in6)
               (lambda (buf host port) (push (list buf host port) encoded)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os--decode-sockaddr-in6)
               (lambda (buf)
                 (should (= buf 5000))
                 (cons groups 54321)))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 (pcase fn
                   ((or "bind" "connect") 0)
                   ("accept" 77)
                   (_ (error "unexpected libc call %S" fn))))))
      (let ((system-type 'darwin))
        (should (= (nelisp-os-bind-inet6 7 groups 1111) 0))
        (should (= (nelisp-os-connect-inet6 8 groups 2222) 0))
        (should (equal (nelisp-os-accept-inet6 9)
                       (list 77 groups 54321)))))
    (should (equal (nreverse encoded)
                   (list (list 3000 groups 1111)
                         (list 4000 groups 2222))))
    (should (equal writes (list (list 6000 0 nelisp-os--sockaddr-in6-len))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "bind"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 7 3000 nelisp-os--sockaddr-in6-len))
                    (list "libc" "connect"
                          [:sint32 :sint32 :pointer :uint32]
                          (list 8 4000 nelisp-os--sockaddr-in6-len))
                    (list "libc" "accept"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 9 5000 6000)))))
    (should (equal (sort freed #'<) '(3000 4000 5000 6000)))))

(ert-deftest nelisp-stdlib-os-getname-darwin-uses-libc ()
  "Darwin getsockname/getpeername wrappers use libc for inet and unix sockets."
  (let ((calls nil)
        (writes nil)
        (freed nil)
        (allocs '(3000 4000 5000 6000 7000 8000 9000 10000)))
    (cl-letf (((symbol-function 'nelisp-os--alloc) (lambda (_n) (pop allocs)))
              ((symbol-function 'nelisp-os--free) (lambda (ptr) (push ptr freed)))
              ((symbol-function 'nelisp-os-write-i32)
               (lambda (ptr off val) (push (list ptr off val) writes) val))
              ((symbol-function 'nelisp-os-read-i32)
               (lambda (ptr off)
                 (should (= off 0))
                 (pcase ptr
                   ((or 6000 10000) 12)
                   (_ (error "unexpected getname len ptr %S" ptr)))))
              ((symbol-function 'nelisp-os--decode-sockaddr-in)
               (lambda (buf)
                 (pcase buf
                   (3000 (cons nelisp-os-INADDR-LOOPBACK 1111))
                   (7000 (cons nelisp-os-INADDR-LOOPBACK 2222))
                   (_ (error "unexpected inet getname buffer %S" buf)))))
              ((symbol-function 'nelisp-os--decode-sockaddr-un)
               (lambda (buf len)
                 (pcase (list buf len)
                   (`(5000 12) "/tmp/a.sock")
                   (`(9000 12) "/tmp/b.sock")
                   (_ (error "unexpected unix getname decode %S %S" buf len)))))
              ((symbol-function 'nelisp-os--libc-call)
               (lambda (dll fn sig &rest args)
                 (push (list dll fn sig args) calls)
                 0)))
      (let ((system-type 'darwin))
        (should (equal (nelisp-os-getsockname-inet 7)
                       (list nelisp-os-INADDR-LOOPBACK 1111)))
        (should (equal (nelisp-os-getsockname-unix 8) "/tmp/a.sock"))
        (should (equal (nelisp-os-getpeername-inet 9)
                       (list nelisp-os-INADDR-LOOPBACK 2222)))
        (should (equal (nelisp-os-getpeername-unix 10) "/tmp/b.sock"))))
    (should (equal (nreverse calls)
                   (list
                    (list "libc" "getsockname"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 7 3000 4000))
                    (list "libc" "getsockname"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 8 5000 6000))
                    (list "libc" "getpeername"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 9 7000 8000))
                    (list "libc" "getpeername"
                          [:sint32 :sint32 :pointer :pointer]
                          (list 10 9000 10000)))))
    (should (equal (nreverse writes)
                   (list
                    (list 4000 0 nelisp-os--sockaddr-in-len)
                    (list 6000 0 nelisp-os--sockaddr-un-len)
                    (list 8000 0 nelisp-os--sockaddr-in-len)
                    (list 10000 0 nelisp-os--sockaddr-un-len))))
    (should (equal (sort freed #'<)
                   '(3000 4000 5000 6000 7000 8000 9000 10000)))))

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
