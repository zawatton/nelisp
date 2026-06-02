;;; nelisp-windows-build-test.el --- ERT tests for Windows PE build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 3/4/5/6/7/8 — structure tests for Phase47 -> Win64 PE32+ EXE emit.

;;; Code:

(require 'ert)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (root (and test-dir (expand-file-name ".." test-dir))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "src" root))
    (add-to-list 'load-path (expand-file-name "scripts" root))))

(require 'nelisp-windows-build)

(defun nelisp-windows-build-test--read-le32 (bytes offset)
  "Read unsigned 32-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16)
          (ash (aref bytes (+ offset 3)) 24)))

(defun nelisp-windows-build-test--read-le64 (bytes offset)
  "Read unsigned 64-bit little-endian integer from BYTES at OFFSET."
  (let ((acc 0)
        (i 7))
    (while (>= i 0)
      (setq acc (logior (ash acc 8) (aref bytes (+ offset i))))
      (setq i (1- i)))
    acc))

(defun nelisp-windows-build-test--read-cstr (bytes offset)
  "Read a NUL-terminated ASCII string from BYTES at OFFSET."
  (let ((end offset))
    (while (and (< end (length bytes))
                (/= (aref bytes end) 0))
      (setq end (1+ end)))
    (substring bytes offset end)))

(defun nelisp-windows-build-test--rva-to-raw (bytes rva)
  "Translate RVA to file offset using section headers in BYTES."
  (let* ((peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (num-sections (logior (aref bytes (+ peoff 6))
                               (ash (aref bytes (+ peoff 7)) 8)))
         (opt-size (logior (aref bytes (+ peoff 20))
                           (ash (aref bytes (+ peoff 21)) 8)))
         (section-off (+ peoff 24 opt-size))
         (i 0)
         (found nil))
    (while (and (< i num-sections) (not found))
      (let* ((off (+ section-off (* i 40)))
             (virtual-size (nelisp-windows-build-test--read-le32 bytes (+ off 8)))
             (virtual-address (nelisp-windows-build-test--read-le32 bytes (+ off 12)))
             (raw-size (nelisp-windows-build-test--read-le32 bytes (+ off 16)))
             (raw-ptr (nelisp-windows-build-test--read-le32 bytes (+ off 20)))
             (span (max virtual-size raw-size)))
        (when (and (<= virtual-address rva)
                   (< rva (+ virtual-address span)))
          (setq found (+ raw-ptr (- rva virtual-address)))))
      (setq i (1+ i)))
    (or found
        (error "RVA not covered by any section: %#x" rva))))

(defun nelisp-windows-build-test--phase47-exe (&optional sexp)
  "Return PE32+ EXE bytes for SEXP, defaulting to `(exit 42)'."
  (nelisp-windows-build--phase47-executable-bytes (or sexp '(exit 42))))

(defun nelisp-windows-build-test--import-name-at (bytes rva)
  "Read IMAGE_IMPORT_BY_NAME function name at RVA from BYTES."
  (nelisp-windows-build-test--read-cstr
   bytes (+ (nelisp-windows-build-test--rva-to-raw bytes rva) 2)))

(defun nelisp-windows-build-test--kernel32-import-names (bytes)
  "Return KERNEL32 import names from EXE BYTES."
  (let* ((peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (import-off (nelisp-windows-build-test--rva-to-raw bytes import-rva))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (thunk-off (nelisp-windows-build-test--rva-to-raw bytes oft))
         (names nil)
         (cursor thunk-off)
         (rva nil))
    (while (not (zerop (setq rva (nelisp-windows-build-test--read-le64
                                  bytes cursor))))
      (push (nelisp-windows-build-test--import-name-at bytes rva) names)
      (setq cursor (+ cursor 8)))
    (nreverse names)))

(defun nelisp-windows-build-test--iat-call-targets (bytes start end)
  "Return target RVAs for `call qword [rip+disp32]' between START and END."
  (let ((targets nil)
        (i start)
        (text-rva #x1000)
        (text-raw #x200))
    (while (< (+ i 6) end)
      (when (and (= (aref bytes i) #xff)
                 (= (aref bytes (+ i 1)) #x15))
        (let* ((disp (nelisp-windows-build-test--read-le32 bytes (+ i 2)))
               (text-off (- i text-raw)))
          (push (+ text-rva text-off 6 disp) targets)))
      (setq i (1+ i)))
    (nreverse targets)))

(ert-deftest nelisp-windows-build-phase47-exit42-imports-exitprocess ()
  "Phase47 Windows EXE imports KERNEL32.dll!ExitProcess."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (name-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 12)))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (hint-name-rva (nelisp-windows-build-test--read-le64
                         bytes (+ rdata-raw (- oft rdata-rva)))))
    (should (= import-rva #x2000))
    (should (= iat-rva #x2038))
    (should (equal (nelisp-windows-build-test--read-cstr
                    bytes (+ rdata-raw (- name-rva rdata-rva)))
                   "KERNEL32.dll"))
    (should (equal (nelisp-windows-build-test--read-cstr
                    bytes (+ rdata-raw (- hint-name-rva rdata-rva) 2))
                   "ExitProcess"))))

(ert-deftest nelisp-windows-build-phase47-exit42-text-calls-iat ()
  "Phase47 `(exit 42)' emits Win64 ExitProcess call, not a Linux syscall."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe))
         (text-raw #x200)
         (text-rva #x1000)
         (iat-rva #x2038)
         (text (substring bytes text-raw (+ text-raw 16)))
         (disp (nelisp-windows-build-test--read-le32 bytes (+ text-raw 11))))
    (should (equal (substring text 0 4)
                   (unibyte-string #x48 #x83 #xec #x28)))
    (should (= (aref text 4) #xb9))
    (should (= (nelisp-windows-build-test--read-le32 text 5) 42))
    (should (equal (substring text 9 11)
                   (unibyte-string #xff #x15)))
    (should (= (+ text-rva 15 disp) iat-rva))
    (should (= (aref text 15) #xcc))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-dynamic-exit-moves-rax-to-rcx ()
  "Computed Phase47 exits pass rax to ExitProcess via Win64 rcx."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq (defun id (x) x) (exit (id 42)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 80))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #xff #x15))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-write-imports-console-apis ()
  "Phase47 Windows `write' EXE imports ExitProcess, GetStdHandle and WriteFile."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq (write "hi\n") (exit 42))))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (import-size (nelisp-windows-build-test--read-le32 bytes (+ opt 124)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (name0-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva))))
         (name1-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva) 8)))
         (name2-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva) 16))))
    (should (= import-rva #x2000))
    (should (= iat-rva #x2048))
    (should (> import-size 120))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name0-rva)
                   "ExitProcess"))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name1-rva)
                   "GetStdHandle"))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name2-rva)
                   "WriteFile"))
    (should (equal (substring bytes (+ rdata-raw import-size)
                              (+ rdata-raw import-size 3))
                   "hi\n"))))

(ert-deftest nelisp-windows-build-phase47-write-text-calls-writefile ()
  "Phase47 Windows `write' emits Win64 WriteFile sequence, not Linux syscall."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq (write "hi\n") (exit 42))))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-size (nelisp-windows-build-test--read-le32 bytes (+ opt 124)))
         (text-raw #x200)
         (text-rva #x1000)
         (string-rva (+ #x2000 import-size))
         (getstdhandle-iat-rva #x2050)
         (writefile-iat-rva #x2058)
         (text (substring bytes text-raw (+ text-raw 80)))
         (getstdhandle-disp (nelisp-windows-build-test--read-le32
                             bytes (+ text-raw 11)))
         (string-disp (nelisp-windows-build-test--read-le32
                       bytes (+ text-raw 21)))
         (writefile-disp (nelisp-windows-build-test--read-le32
                          bytes (+ text-raw 47))))
    (should (equal (substring text 0 4)
                   (unibyte-string #x48 #x83 #xec #x38)))
    (should (equal (substring text 4 9)
                   (unibyte-string #xb9 #xf5 #xff #xff #xff)))
    (should (= (+ text-rva 15 getstdhandle-disp) getstdhandle-iat-rva))
    (should (equal (substring text 18 21)
                   (unibyte-string #x48 #x8d #x15)))
    (should (= (+ text-rva 25 string-disp) string-rva))
    (should (equal (substring text 25 31)
                   (unibyte-string #x41 #xb8 #x03 #x00 #x00 #x00)))
    (should (equal (substring text 31 36)
                   (unibyte-string #x4c #x8d #x4c #x24 #x28)))
    (should (= (+ text-rva 51 writefile-disp) writefile-iat-rva))
    (should (equal (substring text 51 55)
                   (unibyte-string #x48 #x83 #xc4 #x38)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-alloc-imports-virtualalloc ()
  "Phase47 Windows `alloc-bytes' EXE imports VirtualAlloc."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(exit (if (= (alloc-bytes 4096 8) 0) 13 42))))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (name0-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva))))
         (name1-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva) 8))))
    (should (= iat-rva #x2040))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name0-rva)
                   "ExitProcess"))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name1-rva)
                   "VirtualAlloc"))))

(ert-deftest nelisp-windows-build-phase47-alloc-text-calls-virtualalloc ()
  "Phase47 Windows `alloc-bytes' emits a VirtualAlloc IAT call."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(exit (if (= (alloc-bytes 4096 8) 0) 13 42))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 120)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 120))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x31 #xc9))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb8 #x00 #x30 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb9 #x04 #x00 #x00 #x00))
             text))
    (should (member #x2048 targets))
    (should (member #x2040 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-dealloc-imports-virtualfree ()
  "Windows Phase47 `dealloc-bytes' imports VirtualFree."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun alloc_free_probe ()
                     (let* ((p (alloc-bytes 4096 8)))
                       (if (= p 0)
                           13
                         (seq (dealloc-bytes p 4096 8) 42))))
                   (exit (alloc_free_probe)))))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (import-off (+ rdata-raw (- import-rva rdata-rva)))
         (oft (nelisp-windows-build-test--read-le32 bytes import-off))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (name0-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva))))
         (name1-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva) 8)))
         (name2-rva (nelisp-windows-build-test--read-le64
                     bytes (+ rdata-raw (- oft rdata-rva) 16))))
    (should (= iat-rva #x2048))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name0-rva)
                   "ExitProcess"))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name1-rva)
                   "VirtualAlloc"))
    (should (equal (nelisp-windows-build-test--import-name-at bytes name2-rva)
                   "VirtualFree"))))

(ert-deftest nelisp-windows-build-phase47-dealloc-text-calls-virtualfree ()
  "Phase47 Windows `dealloc-bytes' emits a VirtualFree IAT call."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun alloc_free_probe ()
                     (let* ((p (alloc-bytes 4096 8)))
                       (if (= p 0)
                           13
                         (seq (dealloc-bytes p 4096 8) 42))))
                   (exit (alloc_free_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 260)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 260))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x31 #xd2))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb8 #x00 #x80 #x00 #x00))
             text))
    (should (member #x2050 targets))
    (should (member #x2058 targets))
    (should (member #x2048 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-syscall-read-imports-readfile ()
  "Windows stdio `syscall-direct' read imports ReadFile."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun sysread_probe ()
                     (let* ((p (alloc-bytes 1 1)))
                       (if (= p 0)
                           13
                         (let* ((n (syscall-direct 0 0 p 1 0 0 0))
                                (b (ptr-read-u8 p 0)))
                           (seq
                            (dealloc-bytes p 1 1)
                            (if (= n 1) b 14))))))
                   (exit (sysread_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (member "ExitProcess" imports))
    (should (member "GetStdHandle" imports))
    (should (member "ReadFile" imports))
    (should (member "VirtualAlloc" imports))
    (should (member "VirtualFree" imports))))

(ert-deftest nelisp-windows-build-phase47-syscall-read-text-calls-readfile ()
  "Windows stdio `syscall-direct' read emits ReadFile IAT calls."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun sysread_probe ()
                     (let* ((p (alloc-bytes 1 1)))
                       (if (= p 0)
                           13
                         (let* ((n (syscall-direct 0 0 p 1 0 0 0))
                                (b (ptr-read-u8 p 0)))
                           (seq
                            (dealloc-bytes p 1 1)
                            (if (= n 1) b 14))))))
                   (exit (sysread_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 360)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 360))))
    (should (string-match-p
             (regexp-quote (unibyte-string #xb9 #xf6 #xff #xff #xff))
             text))
    (should (member #x2060 targets))
    (should (member #x2068 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-syscall-write-imports-writefile ()
  "Windows stdio `syscall-direct' write imports WriteFile."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun syswrite_probe ()
                     (let* ((p (alloc-bytes 1 1)))
                       (if (= p 0)
                           13
                         (seq
                          (ptr-write-u8 p 0 88)
                          (syscall-direct 1 1 p 1 0 0 0)
                          (dealloc-bytes p 1 1)
                          42))))
                   (exit (syswrite_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (member "ExitProcess" imports))
    (should (member "GetStdHandle" imports))
    (should (member "WriteFile" imports))
    (should (member "VirtualAlloc" imports))
    (should (member "VirtualFree" imports))))

(ert-deftest nelisp-windows-build-phase47-syscall-write-text-calls-writefile ()
  "Windows stdio `syscall-direct' write emits WriteFile IAT calls."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun syswrite_probe ()
                     (let* ((p (alloc-bytes 1 1)))
                       (if (= p 0)
                           13
                         (seq
                          (ptr-write-u8 p 0 88)
                          (syscall-direct 1 1 p 1 0 0 0)
                          (dealloc-bytes p 1 1)
                          42))))
                   (exit (syswrite_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 340)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 340))))
    (should (string-match-p
             (regexp-quote (unibyte-string #xb9 #xf5 #xff #xff #xff))
             text))
    (should (member #x2060 targets))
    (should (member #x2068 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-unsupported-syscall-rejected ()
  "Windows rejects `syscall-direct' forms that are not mapped to Win32 APIs."
  (should-error
   (nelisp-windows-build--phase47-executable-bytes
    '(exit (syscall-direct 9 0 4096 3 34 -1 0)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-windows-build-phase47-file-read-imports-file-apis ()
  "Windows file read syscall chain imports CreateFileA/ReadFile/CloseHandle."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun file_read_probe ()
                     (let* ((path (alloc-bytes 2 1))
                            (buf (alloc-bytes 1 1)))
                       (seq
                        (ptr-write-u8 path 0 120)
                        (ptr-write-u8 path 1 0)
                        (let* ((fd (syscall-direct 2 path 0 0 0 0 0))
                               (n (syscall-direct 0 fd buf 1 0 0 0))
                               (b (ptr-read-u8 buf 0)))
                          (seq
                           (syscall-direct 3 fd 0 0 0 0 0)
                           (if (= n 1) b 15))))))
                   (exit (file_read_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (member "ExitProcess" imports))
    (should (member "CreateFileA" imports))
    (should (member "ReadFile" imports))
    (should (member "CloseHandle" imports))
    (should (member "VirtualAlloc" imports))))

(ert-deftest nelisp-windows-build-phase47-file-read-text-calls-file-apis ()
  "Windows file read syscall chain emits CreateFileA/ReadFile/CloseHandle calls."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun file_read_probe ()
                     (let* ((path (alloc-bytes 2 1))
                            (buf (alloc-bytes 1 1)))
                       (seq
                        (ptr-write-u8 path 0 120)
                        (ptr-write-u8 path 1 0)
                        (let* ((fd (syscall-direct 2 path 0 0 0 0 0))
                               (n (syscall-direct 0 fd buf 1 0 0 0))
                               (b (ptr-read-u8 buf 0)))
                          (seq
                           (syscall-direct 3 fd 0 0 0 0 0)
                           (if (= n 1) b 15))))))
                   (exit (file_read_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 520)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 520))))
    (should (string-match-p
             (regexp-quote (unibyte-string #xba #x00 #x00 #x00 #x80))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                            #x03 #x00 #x00 #x00))
             text))
    (should (member #x2070 targets))
    (should (member #x2068 targets))
    (should (member #x2078 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-file-write-imports-file-apis ()
  "Windows file write syscall chain imports CreateFileA/WriteFile/CloseHandle."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun file_write_probe ()
                     (let* ((path (alloc-bytes 2 1))
                            (buf (alloc-bytes 1 1)))
                       (seq
                        (ptr-write-u8 path 0 120)
                        (ptr-write-u8 path 1 0)
                        (ptr-write-u8 buf 0 90)
                        (let* ((fd (syscall-direct 2 path 577 420 0 0 0))
                               (n (syscall-direct 1 fd buf 1 0 0 0)))
                          (seq
                           (syscall-direct 3 fd 0 0 0 0 0)
                           (if (= n 1) 42 16))))))
                   (exit (file_write_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (member "ExitProcess" imports))
    (should (member "CreateFileA" imports))
    (should (member "WriteFile" imports))
    (should (member "CloseHandle" imports))
    (should (member "VirtualAlloc" imports))))

(ert-deftest nelisp-windows-build-phase47-file-write-text-calls-file-apis ()
  "Windows file write syscall chain emits CreateFileA/WriteFile/CloseHandle calls."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun file_write_probe ()
                     (let* ((path (alloc-bytes 2 1))
                            (buf (alloc-bytes 1 1)))
                       (seq
                        (ptr-write-u8 path 0 120)
                        (ptr-write-u8 path 1 0)
                        (ptr-write-u8 buf 0 90)
                        (let* ((fd (syscall-direct 2 path 577 420 0 0 0))
                               (n (syscall-direct 1 fd buf 1 0 0 0)))
                          (seq
                           (syscall-direct 3 fd 0 0 0 0 0)
                           (if (= n 1) 42 16))))))
                   (exit (file_write_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 520)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 520))))
    (should (string-match-p
             (regexp-quote (unibyte-string #xba #x00 #x00 #x00 #x40))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #x44 #x24 #x20
                                            #x02 #x00 #x00 #x00))
             text))
    (should (member #x2070 targets))
    (should (member #x2068 targets))
    (should (member #x2078 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(provide 'nelisp-windows-build-test)

;;; nelisp-windows-build-test.el ends here
