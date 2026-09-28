;;; nl-ffi-memory.el --- explicitly owned external byte mappings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These buffers live outside NeLisp's managed arena. The collector never
;; reclaims or scans them, so each owner must be released explicitly (usually
;; with `unwind-protect') after native code no longer uses its address.
;; This initial implementation targets Linux x86_64 syscall numbers and page
;; size, matching the standalone FFI loader profile.

;;; Code:

(declare-function syscall-direct "ext:nelisp-runtime"
                  (number a b c d e f))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))

(defconst nl-ffi-memory--page-bytes 4096)
(defconst nl-ffi-memory--sys-mmap 9)
(defconst nl-ffi-memory--sys-munmap 11)
(defconst nl-ffi-memory--map-private-anonymous 34)
(defconst nl-ffi-memory--prot-read-write 3)
(defconst nl-ffi-memory--owner-marker 'nl-ffi-memory-owner)
(defconst nl-ffi-memory--max-size 9223372036854771712)

(defun nl-ffi-memory--supported-p ()
  (and (eq system-type 'gnu/linux)
       (boundp 'system-configuration)
       (stringp system-configuration)
       (string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration)
       (fboundp 'syscall-direct)))

(defun nl-ffi-memory--valid-owner-p (owner)
  (and (vectorp owner)
       (= (length owner) 4)
       (eq (aref owner 0) nl-ffi-memory--owner-marker)
       (integerp (aref owner 1))
       (> (aref owner 1) 0)
       (integerp (aref owner 2))
       (> (aref owner 2) 0)
       (integerp (aref owner 3))
       (>= (aref owner 3) 0)))

(defun nl-ffi-memory-allocate (byte-count)
  "Return an owner for BYTE-COUNT writable external bytes.
The allocation is anonymous mmap memory and is not visible to NeLisp GC.
Call `nl-ffi-memory-release' exactly once when native use has ended."
  (unless (and (integerp byte-count)
               (>= byte-count 0)
               (<= byte-count nl-ffi-memory--max-size))
    (error "nl-ffi-memory: invalid byte count %S" byte-count))
  (unless (nl-ffi-memory--supported-p)
    (error "nl-ffi-memory: requires Linux x86_64 syscall-direct"))
  (let* ((owner (vector nl-ffi-memory--owner-marker 0 0 byte-count))
         (needed (if (< byte-count 1) 1 byte-count))
         (mapped-size (* (/ (+ needed 4095) 4096) 4096))
         (base (syscall-direct nl-ffi-memory--sys-mmap 0 mapped-size
                               nl-ffi-memory--prot-read-write
                               nl-ffi-memory--map-private-anonymous -1 0)))
    (when (< base 0)
      (error "nl-ffi-memory: mmap of %d bytes failed (%d)"
             mapped-size base))
    (when (< base nl-ffi-memory--page-bytes)
      (let ((unmap-rc (syscall-direct nl-ffi-memory--sys-munmap
                                      base mapped-size 0 0 0 0)))
        (unless (= unmap-rc 0)
          (error "nl-ffi-memory: rejected mmap address %d; cleanup failed (%d)"
                 base unmap-rc)))
      (error "nl-ffi-memory: mmap returned low address %d" base))
    (aset owner 1 base)
    (aset owner 2 mapped-size)
    owner))

(defun nl-ffi-memory-address (owner)
  "Return the live base address held by OWNER."
  (unless (nl-ffi-memory--valid-owner-p owner)
    (error "nl-ffi-memory: invalid or released owner"))
  (aref owner 1))

(defun nl-ffi-memory-release (owner)
  "Release OWNER's mapping once; leave OWNER live if `munmap' fails."
  (unless (nl-ffi-memory--valid-owner-p owner)
    (error "nl-ffi-memory: invalid or already released owner"))
  (let ((rc (syscall-direct nl-ffi-memory--sys-munmap
                           (aref owner 1) (aref owner 2) 0 0 0 0)))
    (unless (= rc 0)
      (error "nl-ffi-memory: munmap failed (%d)" rc))
    (aset owner 1 0)
    (aset owner 2 0)
    t))

(defun nl-ffi-memory-cstring (bytes)
  "Return an owner for a NUL-terminated copy of byte string BYTES.
Each character must be an integer byte. Release the returned owner after
the native call that consumes its address."
  (unless (stringp bytes)
    (error "nl-ffi-memory: expected a byte string, got %S" bytes))
  (let ((i 0) (n (length bytes)))
    (while (< i n)
      (let ((byte (aref bytes i)))
        (unless (and (integerp byte) (<= 0 byte) (< byte 256))
          (error "nl-ffi-memory: character at %d is not a byte" i)))
      (setq i (1+ i))))
  (let* ((owner (nl-ffi-memory-allocate (1+ (length bytes))))
         (base (nl-ffi-memory-address owner))
         (i 0)
         (complete nil))
    (unwind-protect
        (progn
          (while (< i (length bytes))
            (ptr-write-u8 base i (aref bytes i))
            (setq i (1+ i)))
          (ptr-write-u8 base i 0)
          (setq complete t)
          owner)
      (unless complete
        (nl-ffi-memory-release owner)))))

(provide 'nl-ffi-memory)

;;; nl-ffi-memory.el ends here
