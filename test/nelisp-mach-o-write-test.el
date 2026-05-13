;;; nelisp-mach-o-write-test.el --- ERT tests for Mach-O writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 §100.D Stage 2/3 — byte-structure tests for the minimal
;; Mach-O arm64 ET_REL writer.  These parse the emitted .o directly on
;; Linux, without requiring macOS toolchain binaries.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-mach-o-write)

(defun nelisp-mach-o-write-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-mach-o-write-test--read-le16 (bytes offset)
  "Read an unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

(defun nelisp-mach-o-write-test--read-le32 (bytes offset)
  "Read an unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-mach-o-write-test--read-le64 (bytes offset)
  "Read an unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 0))
    (while (< i 8)
      (setq acc (logior acc (ash (aref bytes (+ offset i)) (* i 8))))
      (setq i (1+ i)))
    acc))

(defconst nelisp-mach-o-write-test--sample-text
  (unibyte-string #x20 #x00 #x80 #xD2 #xC0 #x03 #x5F #xD6)
  "A tiny arm64 text payload used by the round-trip tests.")

(defconst nelisp-mach-o-write-test--sample-sections
  (list :text nelisp-mach-o-write-test--sample-text
        :symbols (list (list :name "nelisp_jit_add2"
                             :value 4
                             :size 4
                             :section 'text
                             :bind 'global
                             :type 'func))
        :machine 'aarch64
        :entry-sym "nelisp_jit_add2")
  "Sample Mach-O input plist.")

(defun nelisp-mach-o-write-test--emit-sample ()
  "Emit the sample object and return its raw bytes."
  (let ((path (make-temp-file "nelisp-mach-o-test-" nil ".o")))
    (unwind-protect
        (progn
          (nelisp-mach-o-write-binary path nelisp-mach-o-write-test--sample-sections)
          (nelisp-mach-o-write-test--read-file-bytes path))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-mach-o-write-binary-magic-bytes ()
  "The output starts with the little-endian MH_MAGIC_64 bytes."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (equal (substring bytes 0 4)
                   (unibyte-string #xCF #xFA #xED #xFE)))))

(ert-deftest nelisp-mach-o-write-binary-arm64-cputype ()
  "The header encodes CPU_TYPE_ARM64 at offset 4."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 4) #x0100000C))))

(ert-deftest nelisp-mach-o-write-binary-ncmds-and-filetype ()
  "The header reports MH_OBJECT with two load commands."
  (let ((bytes (nelisp-mach-o-write-test--emit-sample)))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 12) 1))
    (should (= (nelisp-mach-o-write-test--read-le32 bytes 16) 2))))

(ert-deftest nelisp-mach-o-write-binary-symbol-entry-shape ()
  "The first nlist_64 entry is a global __text symbol with the right value."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (symoff (nelisp-mach-o-write-test--read-le32 bytes 192))
         (nsyms (nelisp-mach-o-write-test--read-le32 bytes 196)))
    (should (= nsyms 1))
    (should (= (aref bytes (+ symoff 4)) #x0F))
    (should (= (aref bytes (+ symoff 5)) 1))
    (should (= (nelisp-mach-o-write-test--read-le16 bytes (+ symoff 6)) 0))
    (should (= (nelisp-mach-o-write-test--read-le64 bytes (+ symoff 8)) 4))))

(ert-deftest nelisp-mach-o-write-binary-string-table-prefixes-underscore ()
  "The string table contains the leading-underscore symbol spelling."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (stroff (nelisp-mach-o-write-test--read-le32 bytes 200))
         (strsize (nelisp-mach-o-write-test--read-le32 bytes 204))
         (strtab (substring bytes stroff (+ stroff strsize))))
    (should (= (aref strtab 0) 0))
    (should (string-match-p "_nelisp_jit_add2\0" strtab))))

(ert-deftest nelisp-mach-o-write-binary-text-size-matches-input ()
  "The __text section size field and raw payload length match the input."
  (let* ((bytes (nelisp-mach-o-write-test--emit-sample))
         (section-off 104)
         (text-size (nelisp-mach-o-write-test--read-le64 bytes (+ section-off 40)))
         (text-offset (nelisp-mach-o-write-test--read-le32 bytes (+ section-off 48)))
         (raw (substring bytes text-offset (+ text-offset text-size))))
    (should (= text-size (length nelisp-mach-o-write-test--sample-text)))
    (should (equal raw nelisp-mach-o-write-test--sample-text))))

;;; nelisp-mach-o-write-test.el ends here
