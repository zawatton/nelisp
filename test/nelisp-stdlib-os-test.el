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

(provide 'nelisp-stdlib-os-test)

;;; nelisp-stdlib-os-test.el ends here
