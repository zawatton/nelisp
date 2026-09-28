;;; nl-ffi-memory-standalone-smoke.el --- external buffer ownership smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(load "packages/nl-ffi/src/nl-ffi.el")
(load "packages/nl-ffi/src/nl-ffi-memory.el")
(load "packages/nl-ffi/src/nl-ffi-loader.el")

(defun nl-ffi-memory-smoke-check (condition detail)
  (unless condition (error "nl-ffi-memory smoke assertion failed: %S" detail)))

(defun nl-ffi-memory-smoke--bytes-equal-p (address bytes)
  (let ((i 0) (n (length bytes)) (same t))
    (while (<= i n)
      (unless (= (ptr-read-u8 address i)
                 (if (< i n) (aref bytes i) 0))
        (setq same nil))
      (setq i (1+ i)))
    same))

(defvar nl-ffi-memory-smoke--mock-mode nil)
(defvar nl-ffi-memory-smoke--mock-mmap-count 0)
(defvar nl-ffi-memory-smoke--mock-unmap-count 0)
(defvar nl-ffi-memory-smoke--mock-write-count 0)

(defun nl-ffi-memory-smoke--fake-syscall (number _a _b _c _d _e _f)
  (cond
   ((= number nl-ffi-memory--sys-mmap)
    (setq nl-ffi-memory-smoke--mock-mmap-count
          (1+ nl-ffi-memory-smoke--mock-mmap-count))
    (cond ((eq nl-ffi-memory-smoke--mock-mode 'mmap-fail) -12)
          ((eq nl-ffi-memory-smoke--mock-mode 'mmap-low) 1024)
          (t 8192)))
   ((= number nl-ffi-memory--sys-munmap)
    (setq nl-ffi-memory-smoke--mock-unmap-count
          (1+ nl-ffi-memory-smoke--mock-unmap-count))
    (if (eq nl-ffi-memory-smoke--mock-mode 'munmap-fail) -22 0))
   (t -38)))

(defun nl-ffi-memory-smoke--fake-write (_ptr _offset _value)
  (setq nl-ffi-memory-smoke--mock-write-count
        (1+ nl-ffi-memory-smoke--mock-write-count))
  (when (= nl-ffi-memory-smoke--mock-write-count 2)
    (error "nl-ffi-memory smoke injected write failure"))
  0)

(defun nl-ffi-memory-smoke--mock-failures ()
  (let ((old-syscall (symbol-function 'syscall-direct))
        (old-write (symbol-function 'ptr-write-u8))
        (mmap-error nil) (munmap-error nil) (fill-error nil)
        (unknown-platform-error nil) (wrong-arch-error nil) (low-address-error nil)
        (owner nil) (base-before-failed-unmap nil) (unmaps-after-fill 0))
    (unwind-protect
        (progn
          (fset 'syscall-direct 'nl-ffi-memory-smoke--fake-syscall)
          (setq nl-ffi-memory-smoke--mock-mode 'mmap-fail)
          (setq nl-ffi-memory-smoke--mock-mmap-count 0)
          (condition-case nil
              (nl-ffi-memory-allocate 8)
            (error (setq mmap-error t)))
          (nl-ffi-memory-smoke-check mmap-error 'mmap-failure-signaled)
          (let ((system-type 'gnu/linux) (system-configuration nil))
            (setq nl-ffi-memory-smoke--mock-mmap-count 0)
            (condition-case nil
                (nl-ffi-memory-allocate 8)
              (error (setq unknown-platform-error t)))
            (nl-ffi-memory-smoke-check
             (and unknown-platform-error
                  (= nl-ffi-memory-smoke--mock-mmap-count 0))
             'unknown-platform-refused-before-syscall))
          (let ((system-type 'gnu/linux)
                (system-configuration "aarch64-unknown-linux-gnu"))
            (setq nl-ffi-memory-smoke--mock-mmap-count 0)
            (condition-case nil
                (nl-ffi-memory-allocate 8)
              (error (setq wrong-arch-error t)))
            (nl-ffi-memory-smoke-check
             (and wrong-arch-error
                  (= nl-ffi-memory-smoke--mock-mmap-count 0))
             'wrong-architecture-refused-before-syscall))

          (setq nl-ffi-memory-smoke--mock-mode 'mmap-low)
          (setq nl-ffi-memory-smoke--mock-unmap-count 0)
          (condition-case nil
              (nl-ffi-memory-allocate 8)
            (error (setq low-address-error t)))
          (nl-ffi-memory-smoke-check
           (and low-address-error
                (= nl-ffi-memory-smoke--mock-unmap-count 1))
           'low-mmap-address-unmapped-before-error)

          (setq nl-ffi-memory-smoke--mock-mode 'munmap-fail)
          (setq owner (nl-ffi-memory-allocate 8))
          (condition-case nil
              (nl-ffi-memory-release owner)
            (error (setq munmap-error t)))
          (setq base-before-failed-unmap (aref owner 1))
          (nl-ffi-memory-smoke-check
           (and munmap-error (= base-before-failed-unmap 8192))
           'munmap-failure-preserves-owner)
          (setq nl-ffi-memory-smoke--mock-mode 'munmap-ok)
          (nl-ffi-memory-release owner)
          (nl-ffi-memory-smoke-check (= (aref owner 1) 0)
                                     'successful-release-invalidates-owner)

          (setq nl-ffi-memory-smoke--mock-mode 'fill-fail)
          (setq nl-ffi-memory-smoke--mock-unmap-count 0)
          (setq nl-ffi-memory-smoke--mock-write-count 0)
          (fset 'ptr-write-u8 'nl-ffi-memory-smoke--fake-write)
          (condition-case nil
              (nl-ffi-memory-cstring "abcd")
            (error (setq fill-error t)))
          (setq unmaps-after-fill nl-ffi-memory-smoke--mock-unmap-count)
          (nl-ffi-memory-smoke-check
           (and fill-error (= unmaps-after-fill 1))
           'cstring-fill-error-releases-mapping))
      (fset 'syscall-direct old-syscall)
      (fset 'ptr-write-u8 old-write))))

(let* ((dir (getenv "NL_FFI_RUNTIME_PROVIDER_DIR"))
       (_dir-check (nl-ffi-memory-smoke-check (and dir (> (length dir) 0))
                                              'fixture-directory-required))
       (consumer (concat dir "/nl-ffi-loader-runtime-consumer.so"))
       (path (concat dir "/nl-ffi-loader-runtime-provider-a.so"))
       (caught (condition-case data
                   (nl-ffi-loader-open consumer)
                 (nl-ffi-loader-unsupported data))))
  (nl-ffi-memory-smoke-check
   (and (consp caught) (eq (car caught) 'nl-ffi-loader-unsupported)
        (memq :undefined-symbol caught))
   'expected-loader-miss)
  (let* ((owner (nl-ffi-memory-cstring path))
           (address (nl-ffi-memory-address owner))
           (open-rc nil)
           (same-after-gc nil)
           (double-release-caught nil)
           (error-owner nil)
           (error-release-caught nil))
      (nl-ffi-memory-smoke-check
       (= (aref owner 3) (1+ (length path))) 'requested-length)
      (unwind-protect
          (progn
            (garbage-collect)
            (setq same-after-gc
                  (nl-ffi-memory-smoke--bytes-equal-p address path))
            (setq open-rc
                  (syscall-direct nl-ffi-loader--sys-openat
                                  nl-ffi-loader--at-fdcwd address
                                  nl-ffi-loader--o-rdonly 0 0 0))
            (nl-ffi-memory-smoke-check same-after-gc 'bytes-survive-gc)
            (nl-ffi-memory-smoke-check (and (integerp open-rc) (>= open-rc 0))
                                       'openat-exact-path)
            (syscall-direct nl-ffi-loader--sys-close open-rc 0 0 0 0 0))
        (nl-ffi-memory-release owner))
      (nl-ffi-memory-smoke-check (= (aref owner 1) 0) 'normal-release)
      (condition-case nil
          (nl-ffi-memory-release owner)
        (error (setq double-release-caught t)))
      (nl-ffi-memory-smoke-check double-release-caught 'double-release-errors)
      (condition-case nil
          (unwind-protect
              (progn
                (setq error-owner (nl-ffi-memory-allocate 17))
                (error "nl-ffi-memory smoke cleanup"))
            (when error-owner (nl-ffi-memory-release error-owner)))
        (error (setq error-release-caught t)))
      (nl-ffi-memory-smoke-check
       (and error-release-caught error-owner (= (aref error-owner 1) 0))
       'error-unwind-release)
      (nl-ffi-memory-smoke--mock-failures)
      (princ "NL-FFI-MEMORY-SMOKE-PASS\n")))

;;; nl-ffi-memory-standalone-smoke.el ends here
