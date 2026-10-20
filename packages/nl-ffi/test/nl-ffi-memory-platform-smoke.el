;;; nl-ffi-memory-platform-smoke.el --- portable ownership checks -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; No compiler or shared-object fixtures are needed. Run from the repository
;; root with a Linux reader or a Windows reader (including under Wine).

;;; Code:

(declare-function nl-ffi-memory-allocate "nl-ffi-memory" (byte-count))
(declare-function nl-ffi-memory-address "nl-ffi-memory" (owner))
(declare-function nl-ffi-memory-release "nl-ffi-memory" (owner))
(declare-function nl-ffi-memory-cstring "nl-ffi-memory" (bytes))
(declare-function nl-ffi-call "ext:nelisp-runtime" (name &rest args))
(declare-function ptr-read-u8 "ext:nelisp-runtime" (pointer offset))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (pointer offset byte))

(load (or (getenv "NL_FFI_MEMORY_SOURCE")
          "packages/nl-ffi/src/nl-ffi-memory.el") nil t)

(defvar nl-ffi-memory-platform--checks 0)
(defvar nl-ffi-memory-platform--mode nil)
(defvar nl-ffi-memory-platform--calls nil)
(defvar nl-ffi-memory-platform--writes 0)

(defun nl-ffi-memory-platform--check (value name)
  (unless value (error "FFI-MEMORY: %S" name))
  (setq nl-ffi-memory-platform--checks (1+ nl-ffi-memory-platform--checks)))

(defun nl-ffi-memory-platform--error (thunk)
  (condition-case data (progn (funcall thunk) nil) (error data)))

(defun nl-ffi-memory-platform--ffi (name &rest args)
  (push (cons name args) nl-ffi-memory-platform--calls)
  (cond ((equal name "VirtualAlloc")
         (cond ((eq nl-ffi-memory-platform--mode 'allocation-failure) 0)
               ((memq nl-ffi-memory-platform--mode '(low low-cleanup-failure)) 1024)
               (t 8192)))
        ((equal name "VirtualFree")
         (if (memq nl-ffi-memory-platform--mode '(release-failure low-cleanup-failure)) 0 1))
        ((equal name "GetLastError") 5)
        (t (error "Unexpected FFI call %s" name))))

(defun nl-ffi-memory-platform--write (_pointer _offset _byte)
  (setq nl-ffi-memory-platform--writes (1+ nl-ffi-memory-platform--writes))
  (when (= nl-ffi-memory-platform--writes 2)
    (if (eq nl-ffi-memory-platform--mode 'throw)
        (throw 'nl-ffi-memory-platform--exit 'escaped)
      (error "injected write failure"))))

;; Real OS allocations: zero length, page rounding, arbitrary bytes, GC,
;; release, double release, and non-local exit through the owner's caller.
(dolist (size '(0 1 4095 4096 4097))
  (let* ((owner (nl-ffi-memory-allocate size))
         (base (nl-ffi-memory-address owner)))
    (unwind-protect
        (progn
          (nl-ffi-memory-platform--check
           (and (>= base 4096) (= (aref owner 3) size)
                (= (aref owner 2) (* (/ (+ (max 1 size) 4095) 4096) 4096)))
           'size-and-owner)
          (ptr-write-u8 base 0 255)
          (garbage-collect)
          (nl-ffi-memory-platform--check (= (ptr-read-u8 base 0) 255) 'survives-gc))
      (nl-ffi-memory-release owner))
    (nl-ffi-memory-platform--check (= (aref owner 1) 0) 'released)
    (nl-ffi-memory-platform--check
     (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-release owner)))
     'double-release)
    (nl-ffi-memory-platform--check
     (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-address owner)))
     'released-address)))

(let* ((bytes (unibyte-string 65 0 128 255))
       (owner (nl-ffi-memory-cstring bytes))
       (base (nl-ffi-memory-address owner)))
  (unwind-protect
      (dotimes (i 5)
        (nl-ffi-memory-platform--check
         (= (ptr-read-u8 base i) (if (= i 4) 0 (aref bytes i))) 'cstring-bytes))
    (nl-ffi-memory-release owner)))

(dolist (bad '(-1 1.5 nil "1" 9223372036854771713))
  (nl-ffi-memory-platform--check
   (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-allocate bad))) 'invalid-size))
(dolist (bad '(nil 42 [nl-ffi-memory-owner 0 4096 1] [wrong 8192 4096 1]))
  (nl-ffi-memory-platform--check
   (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-release bad))) 'invalid-owner))
(dolist (bad '(nil 42 "日本語"))
  (nl-ffi-memory-platform--check
   (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-cstring bad))) 'invalid-cstring))

(let (owner)
  (catch 'nl-ffi-memory-platform--exit
    (unwind-protect
        (progn (setq owner (nl-ffi-memory-allocate 8))
               (throw 'nl-ffi-memory-platform--exit t))
      (nl-ffi-memory-release owner)))
  (nl-ffi-memory-platform--check (= (aref owner 1) 0) 'caller-throw-cleanup))

;; Inject the Windows OS boundary on either host. Check actual Win32 flags,
;; BOOL inversion, failed-release liveness, and cleanup before error/throw.
(let ((old-ffi (symbol-function 'nl-ffi-call))
      (old-write (symbol-function 'ptr-write-u8))
      (system-type 'windows-nt)
      (system-configuration "x86_64-w64-mingw32"))
  (unwind-protect
      (progn
        (fset 'nl-ffi-call #'nl-ffi-memory-platform--ffi)
        (setq nl-ffi-memory-platform--mode nil nl-ffi-memory-platform--calls nil)
        (let ((owner (nl-ffi-memory-allocate 4097)))
          (nl-ffi-memory-release owner)
          (nl-ffi-memory-platform--check
           (equal (reverse nl-ffi-memory-platform--calls)
                  '(("VirtualAlloc" 0 8192 12288 4) ("VirtualFree" 8192 0 32768)))
           'windows-flags))
        (setq nl-ffi-memory-platform--mode 'allocation-failure)
        (nl-ffi-memory-platform--check
         (equal (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-allocate 8)))
                '(error "nl-ffi-memory: mmap of 4096 bytes failed (-5)")) 'allocation-failed)
        (dolist (mode '(low low-cleanup-failure))
          (setq nl-ffi-memory-platform--mode mode nl-ffi-memory-platform--calls nil)
          (nl-ffi-memory-platform--check
           (equal (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-allocate 8)))
                  (list 'error (if (eq mode 'low)
                                   "nl-ffi-memory: mmap returned low address 1024"
                                 "nl-ffi-memory: rejected mmap address 1024; cleanup failed (-5)")))
           'low-rejected)
          (nl-ffi-memory-platform--check
           (member '("VirtualFree" 1024 0 32768) nl-ffi-memory-platform--calls) 'low-cleaned))
        (setq nl-ffi-memory-platform--mode 'release-failure)
        (let ((owner (nl-ffi-memory-allocate 8)))
          (nl-ffi-memory-platform--check
           (equal (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-release owner)))
                  '(error "nl-ffi-memory: munmap failed (-5)")) 'release-failed)
          (nl-ffi-memory-platform--check (= (nl-ffi-memory-address owner) 8192) 'still-live)
          (setq nl-ffi-memory-platform--mode nil)
          (nl-ffi-memory-release owner)
          (nl-ffi-memory-platform--check (= (aref owner 1) 0) 'release-retried))
        (fset 'ptr-write-u8 #'nl-ffi-memory-platform--write)
        (dolist (mode '(error throw))
          (setq nl-ffi-memory-platform--mode mode nl-ffi-memory-platform--writes 0
                nl-ffi-memory-platform--calls nil)
          (let ((result (catch 'nl-ffi-memory-platform--exit
                          (nl-ffi-memory-platform--error
                           (lambda () (nl-ffi-memory-cstring "abc"))))))
            (nl-ffi-memory-platform--check
             (if (eq mode 'throw) (eq result 'escaped) (eq (car result) 'error)) 'fill-exit)
            (nl-ffi-memory-platform--check
             (equal (car nl-ffi-memory-platform--calls) '("VirtualFree" 8192 0 32768))
             'fill-cleanup)))
        (let ((system-configuration "aarch64-w64-mingw32"))
          (setq nl-ffi-memory-platform--calls nil)
          (nl-ffi-memory-platform--check
           (nl-ffi-memory-platform--error (lambda () (nl-ffi-memory-allocate 8))) 'unsupported-arch)
          (nl-ffi-memory-platform--check (null nl-ffi-memory-platform--calls) 'no-os-call)))
    (fset 'nl-ffi-call old-ffi)
    (fset 'ptr-write-u8 old-write)))

(princ (format "FFI-MEMORY: PASS %d checks\n" nl-ffi-memory-platform--checks))

;;; nl-ffi-memory-platform-smoke.el ends here
