;;; nelisp-windows-build-test.el --- ERT tests for Windows PE build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/31/32/33/34/35/36/37/38/39 — structure tests for Phase47 -> Win64 PE32+ EXE emit.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (root (and test-dir (expand-file-name ".." test-dir))))
  (when root
    (add-to-list 'load-path (expand-file-name "lisp" root))
    (add-to-list 'load-path (expand-file-name "src" root))
    (add-to-list 'load-path (expand-file-name "scripts" root))))

(require 'nelisp-windows-build)

(defun nelisp-windows-build-test--read-le16 (bytes offset)
  "Read unsigned 16-bit little-endian integer from BYTES at OFFSET."
  (logior (aref bytes offset)
          (ash (aref bytes (+ offset 1)) 8)))

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

(defun nelisp-windows-build-test--section-raw-bounds (bytes name)
  "Return `(RAW-START . RAW-END)' for PE section NAME in BYTES."
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
             (sect-name (nelisp-windows-build-test--read-cstr bytes off))
             (raw-size (nelisp-windows-build-test--read-le32 bytes (+ off 16)))
             (raw-ptr (nelisp-windows-build-test--read-le32 bytes (+ off 20))))
        (when (equal sect-name name)
          (setq found (cons raw-ptr (+ raw-ptr raw-size)))))
      (setq i (1+ i)))
    (or found
        (error "PE section not found: %s" name))))

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

(defun nelisp-windows-build-test--kernel32-iat-rvas (bytes)
  "Return KERNEL32 IAT slot RVAs from EXE BYTES."
  (let* ((peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-rva (nelisp-windows-build-test--read-le32 bytes (+ opt 120)))
         (import-off (nelisp-windows-build-test--rva-to-raw bytes import-rva))
         (iat-rva (nelisp-windows-build-test--read-le32 bytes (+ import-off 16)))
         (iat-off (nelisp-windows-build-test--rva-to-raw bytes iat-rva))
         (rvas nil)
         (cursor iat-off)
         (slot-rva iat-rva))
    (while (not (zerop (nelisp-windows-build-test--read-le64 bytes cursor)))
      (push slot-rva rvas)
      (setq cursor (+ cursor 8))
      (setq slot-rva (+ slot-rva 8)))
    (nreverse rvas)))

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

(ert-deftest nelisp-windows-build-createthread-probe-imports-thread-apis ()
  "Stage 11 CreateThread probe imports the required KERNEL32 APIs."
  (let* ((bytes (nelisp-windows-build--createthread-probe-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports
                   '("ExitProcess" "CreateThread" "WaitForSingleObject"
                     "GetExitCodeThread" "CloseHandle")))))

(ert-deftest nelisp-windows-build-createthread-probe-text-calls-thread-apis ()
  "Stage 11 CreateThread probe calls thread APIs and avoids Linux syscall."
  (let* ((bytes (nelisp-windows-build--createthread-probe-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 180)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 180))))
    (should (member (cdr (assoc "CreateThread" iat-map)) targets))
    (should (member (cdr (assoc "WaitForSingleObject" iat-map)) targets))
    (should (member (cdr (assoc "GetExitCodeThread" iat-map)) targets))
    (should (member (cdr (assoc "CloseHandle" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (string-match-p
             (regexp-quote (unibyte-string #x4c #x8d #x05))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #xb8 #x2a #x00 #x00 #x00 #xc3))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-commandline-probe-imports-commandline-apis ()
  "Stage 17 GetCommandLineW probe imports the required KERNEL32 APIs."
  (let* ((bytes (nelisp-windows-build--commandline-probe-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-commandline-probe-text-calls-commandline-apis ()
  "Stage 17 GetCommandLineW probe calls command-line APIs and exits."
  (let* ((bytes (nelisp-windows-build--commandline-probe-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 96)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 96))))
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x85 #xc0 #x74 #x0c))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #xb9 #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #xb9 #x0d #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-linked-call-imports-exitprocess ()
  "Stage 13 linked-unit PE imports ExitProcess."
  (let* ((bytes (nelisp-windows-build--linked-call42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess")))))

(ert-deftest nelisp-windows-build-linked-call-text-links-helper ()
  "Stage 13 linked-unit PE resolves a rel32 helper call in .text."
  (let* ((bytes (nelisp-windows-build--linked-call42-bytes))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 120)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (helper-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 120))))
    (should call-off)
    (should helper-off)
    (should (> target-rva text-rva))
    (should (< target-rva (+ text-rva 120)))
    (should (equal (substring text helper-off (+ helper-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (member #x2038 targets))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1 #xff #x15))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-linked-rodata-imports-exitprocess ()
  "Stage 14 linked-unit rodata PE imports ExitProcess."
  (let* ((bytes (nelisp-windows-build--linked-rodata42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess")))))

(ert-deftest nelisp-windows-build-linked-rodata-carries-and-loads-rodata ()
  "Stage 14 linked-unit PE carries .rodata and patches its RIP load."
  (let* ((bytes (nelisp-windows-build--linked-rodata42-bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-size (nelisp-windows-build-test--read-le32 bytes (+ opt 124)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (answer-rva (+ rdata-rva import-size))
         (answer-raw (+ rdata-raw import-size))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 160)))
         (load-off (string-match-p
                    (regexp-quote (unibyte-string #x8b #x05))
                    text))
         (disp (and load-off
                    (nelisp-windows-build-test--read-le32
                     text (+ load-off 2))))
         (target-rva (and disp (+ text-rva load-off 6 disp)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 160))))
    (should (equal (substring bytes answer-raw (+ answer-raw 4))
                   (unibyte-string #x2a #x00 #x00 #x00)))
    (should load-off)
    (should (= target-rva answer-rva))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-linked-data-imports-exitprocess ()
  "Stage 15 linked-unit data PE imports ExitProcess."
  (let* ((bytes (nelisp-windows-build--linked-data42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess")))))

(ert-deftest nelisp-windows-build-linked-data-carries-and-loads-data ()
  "Stage 15 linked-unit PE carries .data and patches its RIP load."
  (let* ((bytes (nelisp-windows-build--linked-data42-bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (sect0 (+ opt 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-rva (nelisp-windows-build-test--read-le32 bytes (+ sect2 12)))
         (data-raw (nelisp-windows-build-test--read-le32 bytes (+ sect2 20)))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 160)))
         (load-off (string-match-p
                    (regexp-quote (unibyte-string #x03 #x05))
                    text))
         (disp (and load-off
                    (nelisp-windows-build-test--read-le32
                     text (+ load-off 2))))
         (target-rva (and disp (+ text-rva load-off 6 disp)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 160))))
    (should (= (nelisp-windows-build-test--read-le16 bytes (+ peoff 6)) 3))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= data-rva #x3000))
    (should (= data-raw #x600))
    (should (equal (substring bytes data-raw (+ data-raw 4))
                   (unibyte-string #x28 #x00 #x00 #x00)))
    (should load-off)
    (should (= target-rva data-rva))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-start-imports-exitprocess ()
  "Stage 16 standalone-shaped PE start imports ExitProcess."
  (let* ((bytes (nelisp-windows-build--standalone-start-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess")))))

(ert-deftest nelisp-windows-build-standalone-start-calls-driver ()
  "Stage 16 PE `_start' calls `driver' and exits with its return value."
  (let* ((bytes (nelisp-windows-build--standalone-start-driver42-bytes))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 120)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 120))))
    (should call-off)
    (should driver-off)
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1 #xff #x15))
             text))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-imports-apis ()
  "Stage 18 standalone command-line bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-passes-driver-arg ()
  "Stage 18 `_start' passes GetCommandLineW's result to `driver'."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 180)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 180))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (string-match-p
             (regexp-quote (unibyte-string #xff #x15))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1 #xe8))
             text))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x0d #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-len-imports-apis ()
  "Stage 19 standalone command-line-length bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-len-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-len-passes-driver-args ()
  "Stage 19 `_start' passes command line pointer and length to `driver'."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-len-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 220)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 220))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #x44 #x24 #x28))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1 #xff #x15))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x8b #x4c #x24 #x28
                                            #x48 #x89 #xc2 #xe8))
             text))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x0d #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x0e #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-read-imports-apis ()
  "Stage 20 standalone command-line read bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-read-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-read-derefs-buffer ()
  "Stage 20 `driver' reads the UTF-16 command-line buffer."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-read-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 260)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 260))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x0f #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-scan-imports-apis ()
  "Stage 21 standalone command-line scan bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-scan-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-scan-loops-buffer ()
  "Stage 21 `driver' scans the UTF-16 command-line buffer for a marker."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-scan-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 700)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 700))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x21 #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x10 #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-imports-apis ()
  "Stage 22 standalone argv1 bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-parses-marker ()
  "Stage 22 `driver' skips exe token and checks first argv marker."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 1400)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 1400))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (dolist (imm '(#x22 #x20 #x09 #x21))
      (should (string-match-p
               (regexp-quote (unibyte-string imm #x00 #x00 #x00))
               text)))
    (dolist (status '(#x2a #x10 #x11))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-exact-imports-apis ()
  "Stage 23 standalone exact argv1 bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-exact-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-exact-bounds-token ()
  "Stage 23 `driver' computes argv1 bounds and checks exact marker."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-exact-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (text (substring bytes text-raw (+ text-raw 2200)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (disp (and call-off
                    (nelisp-windows-build-test--read-le32
                     text (+ call-off 1))))
         (target-rva (and disp (+ text-rva call-off 5 disp)))
         (driver-off (and target-rva (- target-rva text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 2200))))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (dolist (imm '(#x22 #x20 #x09 #x21 #x01))
      (should (string-match-p
               (regexp-quote (unibyte-string imm #x00 #x00 #x00))
               text)))
    (dolist (status '(#x2a #x10 #x11))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-cstr-imports-apis ()
  "Stage 24 standalone argv1 C-string bridge imports the required APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-cstr-driver-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "GetCommandLineW" "lstrlenW")))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-cstr-data-buffer ()
  "Stage 24 PE carries a .data argv1 byte buffer."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-cstr-driver-bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (sect0 (+ opt 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-rva (nelisp-windows-build-test--read-le32 bytes (+ sect2 12)))
         (data-raw (nelisp-windows-build-test--read-le32 bytes (+ sect2 20)))
         (data-size (nelisp-windows-build-test--read-le32 bytes (+ sect2 16))))
    (should (= (nelisp-windows-build-test--read-le16 bytes (+ peoff 6)) 3))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= data-rva #x3000))
    (should (> data-raw 0))
    (should (>= data-size 260))
    (should (equal (substring bytes data-raw (+ data-raw 16))
                   (make-string 16 0)))))

(ert-deftest nelisp-windows-build-standalone-commandline-argv1-cstr-passes-buffer ()
  "Stage 24 `_start' passes argv1_cstr as driver arg2 in r8."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-argv1-cstr-driver-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (data-rva #x3000)
         (text (substring bytes text-raw (+ text-raw 2600)))
         (lea-off (string-match-p
                   (regexp-quote (unibyte-string #x4c #x8d #x05))
                   text))
         (disp (and lea-off
                    (nelisp-windows-build-test--read-le32
                     text (+ lea-off 3))))
         (target-rva (and disp (+ text-rva lea-off 7 disp)))
         (call-off (string-match-p
                    (regexp-quote (unibyte-string #xe8))
                    text))
         (driver-target (and call-off
                             (+ text-rva call-off 5
                                (nelisp-windows-build-test--read-le32
                                 text (+ call-off 1)))))
         (driver-off (and driver-target (- driver-target text-rva)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 2600))))
    (should lea-off)
    (should (= target-rva data-rva))
    (should call-off)
    (should driver-off)
    (should (member (cdr (assoc "GetCommandLineW" iat-map)) targets))
    (should (member (cdr (assoc "lstrlenW" iat-map)) targets))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (equal (substring text driver-off (+ driver-off 4))
                   (unibyte-string #x55 #x48 #x89 #xe5)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x88 #x14 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb6 #x04 #x37))
             text))
    (dolist (status '(#x0d #x0e #x0f #x11))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-file-read-imports-apis ()
  "Stage 25 argv1 file-read bridge imports command-line and file APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-file-read-byte-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports
                   '("ExitProcess" "GetCommandLineW" "lstrlenW"
                     "CreateFileA" "ReadFile" "CloseHandle")))))

(ert-deftest nelisp-windows-build-standalone-commandline-file-read-data-buffers ()
  "Stage 25 PE carries argv1 path and file read buffers in .data."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-file-read-byte-bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (sect0 (+ opt 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-rva (nelisp-windows-build-test--read-le32 bytes (+ sect2 12)))
         (data-raw (nelisp-windows-build-test--read-le32 bytes (+ sect2 20)))
         (data-size (nelisp-windows-build-test--read-le32 bytes (+ sect2 16))))
    (should (= (nelisp-windows-build-test--read-le16 bytes (+ peoff 6)) 3))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= data-rva #x3000))
    (should (> data-raw 0))
    (should (>= data-size 276))
    (should (equal (substring bytes data-raw (+ data-raw 16))
                   (make-string 16 0)))
    (should (equal (substring bytes (+ data-raw 260) (+ data-raw 276))
                   (make-string 16 0)))))

(ert-deftest nelisp-windows-build-standalone-commandline-file-read-calls-file-apis ()
  "Stage 25 linked driver calls Win32 file APIs through correctly based IAT calls."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-file-read-byte-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (data-rva #x3000)
         (text-end (min (length bytes) (+ text-raw 3600)))
         (text (substring bytes text-raw text-end))
         (lea-r8-off (string-match-p
                      (regexp-quote (unibyte-string #x4c #x8d #x05))
                      text))
         (r8-target (and lea-r8-off
                         (+ text-rva lea-r8-off 7
                            (nelisp-windows-build-test--read-le32
                             text (+ lea-r8-off 3)))))
         (lea-r9-off (string-match-p
                      (regexp-quote (unibyte-string #x4c #x8d #x0d))
                      text))
         (r9-target (and lea-r9-off
                         (+ text-rva lea-r9-off 7
                            (nelisp-windows-build-test--read-le32
                             text (+ lea-r9-off 3)))))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should lea-r8-off)
    (should lea-r9-off)
    (should (= r8-target data-rva))
    (should (= r9-target (+ data-rva 260)))
    (dolist (name '("GetCommandLineW" "lstrlenW" "ExitProcess"
                    "CreateFileA" "ReadFile" "CloseHandle"))
      (should (member (cdr (assoc name iat-map)) targets)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb7 #x04 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x88 #x14 #x37))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x0f #xb6 #x04 #x37))
             text))
    (dolist (status '(#x0d #x0e #x0f #x10 #x11 #x12 #x13))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-commandline-dual-source-imports-apis ()
  "Stage 26 dual-mode source bridge imports command-line and file APIs."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-dual-source-byte-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports
                   '("ExitProcess" "GetCommandLineW" "lstrlenW"
                     "CreateFileA" "ReadFile" "CloseHandle")))))

(ert-deftest nelisp-windows-build-standalone-commandline-dual-source-data-buffers ()
  "Stage 26 PE carries argv1 path and file read buffers in .data."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-dual-source-byte-bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (sect0 (+ opt 240))
         (sect1 (+ sect0 40))
         (sect2 (+ sect1 40))
         (data-rva (nelisp-windows-build-test--read-le32 bytes (+ sect2 12)))
         (data-raw (nelisp-windows-build-test--read-le32 bytes (+ sect2 20)))
         (data-size (nelisp-windows-build-test--read-le32 bytes (+ sect2 16))))
    (should (= (nelisp-windows-build-test--read-le16 bytes (+ peoff 6)) 3))
    (should (string-prefix-p ".data" (substring bytes sect2 (+ sect2 8))))
    (should (= data-rva #x3000))
    (should (> data-raw 0))
    (should (>= data-size 276))
    (should (equal (substring bytes data-raw (+ data-raw 16))
                   (make-string 16 0)))
    (should (equal (substring bytes (+ data-raw 260) (+ data-raw 276))
                   (make-string 16 0)))))

(ert-deftest nelisp-windows-build-standalone-commandline-dual-source-calls-file-apis ()
  "Stage 26 dual-mode driver branches to embedded or Win32 file-load paths."
  (let* ((bytes (nelisp-windows-build--standalone-commandline-dual-source-byte-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-rva #x1000)
         (data-rva #x3000)
         (text-end (min (length bytes) (+ text-raw 3800)))
         (text (substring bytes text-raw text-end))
         (lea-r8-off (string-match-p
                      (regexp-quote (unibyte-string #x4c #x8d #x05))
                      text))
         (r8-target (and lea-r8-off
                         (+ text-rva lea-r8-off 7
                            (nelisp-windows-build-test--read-le32
                             text (+ lea-r8-off 3)))))
         (lea-r9-off (string-match-p
                      (regexp-quote (unibyte-string #x4c #x8d #x0d))
                      text))
         (r9-target (and lea-r9-off
                         (+ text-rva lea-r9-off 7
                            (nelisp-windows-build-test--read-le32
                             text (+ lea-r9-off 3)))))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should lea-r8-off)
    (should lea-r9-off)
    (should (= r8-target data-rva))
    (should (= r9-target (+ data-rva 260)))
    (dolist (name '("GetCommandLineW" "lstrlenW" "ExitProcess"
                    "CreateFileA" "ReadFile" "CloseHandle"))
      (should (member (cdr (assoc name iat-map)) targets)))
    (dolist (status '(#x0d #x0e #x0f #x10 #x11 #x12 #x13 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-arena-imports-virtualalloc ()
  "Stage 27 standalone arena PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-arena-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-arena-text-calls-virtualalloc ()
  "Stage 27 linked arena unit calls VirtualAlloc through a correctly based IAT."
  (let* ((bytes (nelisp-windows-build--standalone-arena-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x0d #x0e #x0f #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb8 #x00 #x30 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb9 #x04 #x00 #x00 #x00))
             text))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-arena-checks-fixed-slots ()
  "Stage 27 driver checks the initialized standalone arena slots."
  (let* ((bytes (nelisp-windows-build--standalone-arena-driver42-bytes))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x10000000 #x00000100 #x10000100))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-arena-alloc-imports-virtualalloc ()
  "Stage 28 standalone arena allocation PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-arena-alloc-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-arena-alloc-text-calls-virtualalloc ()
  "Stage 28 linked arena allocation calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-arena-alloc-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x0d #x0e #x0f #x10 #x11 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-arena-alloc-checks-invariants ()
  "Stage 28 driver checks allocation pointer, bump, header and byte IO."
  (let* ((bytes (nelisp-windows-build--standalone-arena-alloc-driver42-bytes))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x10000110 #x00000120 #x00000020 #x0000005a))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-gc-reuse-imports-virtualalloc ()
  "Stage 29 standalone GC reuse PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-gc-reuse-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-gc-reuse-text-calls-virtualalloc ()
  "Stage 29 linked GC reuse calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-gc-reuse-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x12 #x13 #x14 #x15 #x16 #x17 #x18 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-gc-reuse-checks-invariants ()
  "Stage 29 driver checks GC free-list insertion and reuse invariants."
  (let* ((bytes (nelisp-windows-build--standalone-gc-reuse-driver42-bytes))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x10000060 #x000002b0 #x00000002))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-cons-imports-virtualalloc ()
  "Stage 30 standalone Cons construction PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-cons-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-cons-text-calls-virtualalloc ()
  "Stage 30 linked Cons construction calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-cons-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x19 #x1a #x1b #x1c #x1d #x1e #x1f #x20 #x21 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-cons-checks-invariants ()
  "Stage 30 driver checks Cons tag, car/cdr payloads and refcount."
  (let* ((bytes (nelisp-windows-build--standalone-cons-driver42-bytes))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000007 #x00000002 #x0000000b #x0000001f
                   #x00000020 #x00000040 #x00000048))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-consbox-clone-imports-virtualalloc ()
  "Stage 31 standalone NlConsBox clone PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-clone-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-consbox-clone-text-calls-virtualalloc ()
  "Stage 31 linked NlConsBox clone calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-clone-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x22 #x23 #x24 #x25 #x26 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-consbox-clone-checks-invariants ()
  "Stage 31 driver checks clone return identity and refcount increment."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-clone-driver42-bytes))
         (text-raw #x200)
         (text-end (nelisp-windows-build-test--rva-to-raw bytes #x2000))
         (text (substring bytes text-raw text-end)))
    (should (string-match-p
             (regexp-quote (unibyte-string #xf0 #x48 #x0f #xc1 #x07))
             text))
    (dolist (imm '(#x00000002 #x00000020 #x00000040 #x00000048))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-sexp-clone-imports-virtualalloc ()
  "Stage 32 standalone Sexp clone PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-sexp-clone-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-sexp-clone-text-calls-virtualalloc ()
  "Stage 32 linked Sexp clone calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-sexp-clone-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x27 #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f #x30))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-sexp-clone-checks-invariants ()
  "Stage 32 driver checks inline Int clone and boxed Cons rc clone."
  (let* ((bytes (nelisp-windows-build--standalone-sexp-clone-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (should (string-match-p
             (regexp-quote (unibyte-string #xf0 #x48 #x0f #xc1 #x07))
             text))
    (dolist (imm '(#x00000002 #x00000007 #x00000040 #x0000007b
                   #x000001c8 #x00000315))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-consbox-set-imports-virtualalloc ()
  "Stage 33 standalone NlConsBox set PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-set-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-consbox-set-text-calls-virtualalloc ()
  "Stage 33 linked NlConsBox set calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-set-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38
                      #x39 #x3a #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-consbox-set-checks-invariants ()
  "Stage 33 driver checks car/cdr word copies and refcount preservation."
  (let* ((bytes (nelisp-windows-build--standalone-consbox-set-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000002 #x00000008 #x00000010 #x00000018
                   #x00000020 #x00000028 #x00000030 #x00000038
                   #x00000040 #x0000006f #x000000de #x0000014d
                   #x000001bc #x0000022b #x0000029a))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-vector-imports-virtualalloc ()
  "Stage 34 standalone NlVector PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-vector-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-vector-text-calls-virtualalloc ()
  "Stage 34 linked NlVector allocation calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-vector-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x3b #x3c #x3d #x3e #x3f #x40 #x41 #x42
                      #x43 #x44 #x45 #x46 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-vector-checks-invariants ()
  "Stage 34 driver checks vector header, Nil fill, and element copy."
  (let* ((bytes (nelisp-windows-build--standalone-vector-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000001 #x00000002 #x00000003
                   #x00000008 #x00000010 #x00000018 #x00000020
                   #x00000028 #x00000030 #x00000038 #x00000040
                   #x00000309 #x00000378 #x000003e7))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-str-imports-virtualalloc ()
  "Stage 35 standalone Str/Symbol PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-str-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-str-text-calls-virtualalloc ()
  "Stage 35 linked Str/Symbol allocation calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-str-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x47 #x48 #x49 #x4a #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-str-checks-invariants ()
  "Stage 35 driver checks Str/Symbol tags, lengths, and byte payloads."
  (let* ((bytes (nelisp-windows-build--standalone-str-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000004 #x00000005 #x00000008 #x00000010 #x00000018
                   #x00000065 #x00000068 #x0000006c #x0000006f))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-mut-str-imports-virtualalloc ()
  "Stage 36 standalone MutStr PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-mut-str-text-calls-virtualalloc ()
  "Stage 36 linked MutStr allocation/finalize calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x55 #x56 #x57 #x58 #x59 #x5a #x5b #x5c
                      #x5d #x5e #x5f #x60 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-mut-str-checks-invariants ()
  "Stage 36 driver checks MutStr box layout, length, and finalized Str bytes."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000001 #x00000005 #x00000006 #x00000008
                   #x00000010 #x00000018 #x00000032 #x00000033
                   #x00000069 #x0000006e #x00000077))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-record-imports-virtualalloc ()
  "Stage 37 standalone NlRecord PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-record-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-record-text-calls-virtualalloc ()
  "Stage 37 linked NlRecord allocation calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-record-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x61 #x62 #x63 #x64 #x65 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-record-checks-invariants ()
  "Stage 37 driver checks Record type tag, slots Vec, and set-slot clone."
  (let* ((bytes (nelisp-windows-build--standalone-record-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (should (string-match-p
             (regexp-quote (unibyte-string #xf0 #x48 #x0f #xc1 #x07))
             text))
    (dolist (imm '(#x00000001 #x00000002 #x00000020 #x00000028
                   #x00000030 #x00000038 #x0000004d #x00000058
                   #x00000063 #x000004d2 #x0000162e #x00002334))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-mut-str-push-imports-virtualalloc ()
  "Stage 38 standalone MutStr push PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-push-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-mut-str-push-text-calls-virtualalloc ()
  "Stage 38 linked MutStr push/grow path calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-push-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x66 #x67 #x68 #x69 #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-mut-str-push-checks-invariants ()
  "Stage 38 driver checks MutStr growth and UTF-8 byte emission."
  (let* ((bytes (nelisp-windows-build--standalone-mut-str-push-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000001 #x00000005 #x00000007 #x00000008
                   #x00000010 #x00000018 #x00000041 #x00000042
                   #x00000082 #x000000a2 #x000000ac #x000000c2
                   #x000000e2 #x000020ac))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

(ert-deftest nelisp-windows-build-standalone-str-utf8-imports-virtualalloc ()
  "Stage 39 standalone nlstr UTF-8 PE imports ExitProcess and VirtualAlloc."
  (let* ((bytes (nelisp-windows-build--standalone-str-utf8-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (equal imports '("ExitProcess" "VirtualAlloc")))))

(ert-deftest nelisp-windows-build-standalone-str-utf8-text-calls-virtualalloc ()
  "Stage 39 linked nlstr UTF-8 helper probe calls VirtualAlloc through IAT."
  (let* ((bytes (nelisp-windows-build--standalone-str-utf8-driver42-bytes))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (iat-rvas (nelisp-windows-build-test--kernel32-iat-rvas bytes))
         (iat-map (cl-mapcar #'cons imports iat-rvas))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw text-end)))
    (should (member (cdr (assoc "ExitProcess" iat-map)) targets))
    (should (member (cdr (assoc "VirtualAlloc" iat-map)) targets))
    (dolist (status '(#x6a #x6b #x6c #x6d #x2a))
      (should (string-match-p
               (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                              status #x00 #x00 #x00))
               text)))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-standalone-str-utf8-checks-invariants ()
  "Stage 39 driver checks Str/MutStr char count and UTF-8 codepoint decode."
  (let* ((bytes (nelisp-windows-build--standalone-str-utf8-driver42-bytes))
         (text-bounds (nelisp-windows-build-test--section-raw-bounds
                       bytes ".text"))
         (text-raw (car text-bounds))
         (text-end (cdr text-bounds))
         (text (substring bytes text-raw text-end)))
    (dolist (imm '(#x00000001 #x00000002 #x00000003 #x00000004
                   #x00000006 #x00000007 #x00000008 #x00000010
                   #x00000018 #x00000041 #x00000042 #x00000082
                   #x000000a2 #x000000ac #x000000c0 #x000000c2
                   #x000000e2 #x000020ac))
      (should (string-match-p
               (regexp-quote
                (unibyte-string (logand imm #xff)
                                (logand (ash imm -8) #xff)
                                (logand (ash imm -16) #xff)
                                (logand (ash imm -24) #xff)))
               text)))))

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
    '(exit (syscall-direct 56 768 0 0 0 0 0)))
   :type 'nelisp-phase47-compiler-error))

(ert-deftest nelisp-windows-build-phase47-syscall-exit-calls-exitprocess ()
  "Windows exit-shaped `syscall-direct' emits ExitProcess, not Linux syscall."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(exit (syscall-direct 60 42 0 0 0 0 0))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 96)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 96))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2a #x00 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #x89 #xc1))
             text))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-syscall-exit-group-calls-exitprocess ()
  "Windows exit_group-shaped `syscall-direct' emits ExitProcess."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(exit (syscall-direct 231 43 0 0 0 0 0))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 96)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 96))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x48 #xc7 #xc0
                                            #x2b #x00 #x00 #x00))
             text))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-static-table-rdata-and-lookup ()
  "Windows Phase47 carries static imm32 tables into PE .rdata."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (static-imm32-table-define "t" (7 42 99))
                   (defun table_probe ()
                     (static-imm32-table-lookup "t" 1))
                   (exit (table_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes))
         (peoff (nelisp-windows-build-test--read-le32 bytes #x3c))
         (opt (+ peoff 24))
         (import-size (nelisp-windows-build-test--read-le32 bytes (+ opt 124)))
         (rdata-rva #x2000)
         (rdata-raw #x400)
         (table-rva (+ rdata-rva import-size))
         (table-raw (+ rdata-raw import-size))
         (table-vaddr (+ nelisp-pe--image-base-x86-64 table-rva))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 180)))
         (mov-rdi (string-match-p
                   (regexp-quote (unibyte-string #x48 #xbf))
                   text))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 180))))
    (should (equal imports '("ExitProcess")))
    (should (equal (substring bytes table-raw (+ table-raw 12))
                   (unibyte-string #x07 #x00 #x00 #x00
                                   #x2a #x00 #x00 #x00
                                   #x63 #x00 #x00 #x00)))
    (should mov-rdi)
    (should (= (nelisp-windows-build-test--read-le64
                text (+ mov-rdi 2))
               table-vaddr))
    (should (member #x2038 targets))
    (should-not (string-match-p
                 (regexp-quote (unibyte-string #x0f #x05))
                 text))))

(ert-deftest nelisp-windows-build-phase47-mmap-imports-virtualalloc-free ()
  "Windows mmap/munmap-shaped syscalls import VirtualAlloc/VirtualFree."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun mmap_probe ()
                     (let* ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                       (if (= p 0)
                           13
                         (seq
                          (ptr-write-u8 p 0 77)
                          (syscall-direct 11 p 4096 0 0 0 0)
                          42))))
                   (exit (mmap_probe)))))
         (imports (nelisp-windows-build-test--kernel32-import-names bytes)))
    (should (member "ExitProcess" imports))
    (should (member "VirtualAlloc" imports))
    (should (member "VirtualFree" imports))))

(ert-deftest nelisp-windows-build-phase47-mmap-text-calls-virtualalloc-free ()
  "Windows mmap/munmap-shaped syscalls emit VirtualAlloc/VirtualFree calls."
  (let* ((bytes (nelisp-windows-build-test--phase47-exe
                 '(seq
                   (defun mmap_probe ()
                     (let* ((p (syscall-direct 9 0 4096 3 34 -1 0)))
                       (if (= p 0)
                           13
                         (seq
                          (ptr-write-u8 p 0 77)
                          (syscall-direct 11 p 4096 0 0 0 0)
                          42))))
                   (exit (mmap_probe)))))
         (text-raw #x200)
         (text (substring bytes text-raw (+ text-raw 260)))
         (targets (nelisp-windows-build-test--iat-call-targets
                   bytes text-raw (+ text-raw 260))))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb8 #x00 #x30 #x00 #x00))
             text))
    (should (string-match-p
             (regexp-quote (unibyte-string #x41 #xb9 #x04 #x00 #x00 #x00))
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
