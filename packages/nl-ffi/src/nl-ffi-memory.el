;;; nl-ffi-memory.el --- explicitly owned external byte mappings -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; These buffers live outside NeLisp's managed arena. The collector never
;; reclaims or scans them, so each owner must be released explicitly (usually
;; with `unwind-protect') after native code no longer uses its address.
;; Linux x86_64 uses anonymous mmap; Windows x86_64 uses VirtualAlloc through
;; the runtime's existing fixed FFI table. Only the two OS adapters differ;
;; ownership, validation, page rounding and cleanup are shared.

;;; Code:

(declare-function syscall-direct "ext:nelisp-runtime"
                  (number a b c d e f))
(declare-function nl-ffi-call "ext:nelisp-runtime" (name &rest args))
(declare-function ptr-write-u8 "ext:nelisp-runtime" (ptr offset value))

(defconst nl-ffi-memory--page-bytes 4096)
(defconst nl-ffi-memory--sys-mmap 9)
(defconst nl-ffi-memory--sys-munmap 11)
(defconst nl-ffi-memory--map-private-anonymous 34)
(defconst nl-ffi-memory--prot-read-write 3)
(defconst nl-ffi-memory--owner-marker 'nl-ffi-memory-owner)
(defconst nl-ffi-memory--max-size 9223372036854771712)

(defun nl-ffi-memory--supported-p ()
  (and (boundp 'system-configuration)
       (stringp system-configuration)
       (string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration)
       (or (and (eq system-type 'gnu/linux) (fboundp 'syscall-direct))
           (and (eq system-type 'windows-nt) (fboundp 'nl-ffi-call)))))

(defun nl-ffi-memory--map (size)
  "Map SIZE external bytes; return an address or a negative OS error."
  (if (eq system-type 'windows-nt)
      ;; MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE. The fixed table already
      ;; imports these kernel32 functions; no DLL loader or new native is
      ;; needed. Normalize the Win32 NULL failure to mmap's negative result.
      (let ((base (nl-ffi-call "VirtualAlloc" 0 size 12288 4)))
        (if (= base 0) (- (max 1 (nl-ffi-call "GetLastError"))) base))
    (syscall-direct nl-ffi-memory--sys-mmap 0 size
                    nl-ffi-memory--prot-read-write
                    nl-ffi-memory--map-private-anonymous -1 0)))

(defun nl-ffi-memory--unmap (base size)
  "Release BASE and SIZE; return zero on success or a negative OS error."
  (if (eq system-type 'windows-nt)
      ;; MEM_RELEASE requires the original base and a zero size. Win32 BOOL
      ;; success is nonzero, whereas the shared owner protocol uses zero.
      (if (= (nl-ffi-call "VirtualFree" base 0 32768) 0)
          (- (max 1 (nl-ffi-call "GetLastError")))
        0)
    (syscall-direct nl-ffi-memory--sys-munmap base size 0 0 0 0)))

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
The allocation is anonymous OS memory and is not visible to NeLisp GC.
Call `nl-ffi-memory-release' exactly once when native use has ended."
  (unless (and (integerp byte-count)
               (>= byte-count 0)
               (<= byte-count nl-ffi-memory--max-size))
    (error "nl-ffi-memory: invalid byte count %S" byte-count))
  (unless (nl-ffi-memory--supported-p)
    (if (eq system-type 'windows-nt)
        (error "nl-ffi-memory: requires Windows x86_64 nl-ffi-call")
      (error "nl-ffi-memory: requires Linux x86_64 syscall-direct")))
  (let* ((owner (vector nl-ffi-memory--owner-marker 0 0 byte-count))
         (needed (if (< byte-count 1) 1 byte-count))
         (mapped-size (* (/ (+ needed 4095) 4096) 4096))
         (base (nl-ffi-memory--map mapped-size)))
    (when (< base 0)
      (error "nl-ffi-memory: mmap of %d bytes failed (%d)"
             mapped-size base))
    (when (< base nl-ffi-memory--page-bytes)
      (let ((unmap-rc (nl-ffi-memory--unmap base mapped-size)))
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
  "Release OWNER's mapping once; leave OWNER live if OS release fails."
  (unless (nl-ffi-memory--valid-owner-p owner)
    (error "nl-ffi-memory: invalid or already released owner"))
  (let ((rc (nl-ffi-memory--unmap (aref owner 1) (aref owner 2))))
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
