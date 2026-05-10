;;; nelisp-crt0-arm64-test.el --- ERT tests for crt0 §94.b  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 94 §94.b — pure-elisp ert tests for `nelisp-crt0-arm64'.
;; Covers (1) raw byte-pattern of each stub against hand-encoded
;; Arm ARM expectations, (2) length invariants, (3) the MOVZ / BL /
;; ADR encoders, (4) the hello-world emitter which produces an
;; EM_AARCH64 ELF and (when the host is aarch64) execs it to assert
;; stdout + exit code.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-crt0-arm64)
(require 'nelisp-elf-write)

;; ---------------------------------------------------------------- helpers

(defun nelisp-crt0-arm64-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-crt0-arm64-test--aarch64-host-p ()
  "Return non-nil when running on aarch64 Linux (= can exec the binary)."
  (and (memq system-type '(gnu/linux gnu))
       (let ((machine (or (getenv "MACHINE")
                          (and (boundp 'system-configuration)
                               system-configuration)
                          "")))
         (string-match-p "aarch64\\|arm64" machine))))

;; ---------------------------------------------------------------- §1 encoders

(ert-deftest nelisp-crt0-arm64-encode-inst-shape ()
  "32-bit instruction WORD emits 4 little-endian bytes."
  (let ((b (nelisp-crt0-arm64--encode-inst #x12345678)))
    (should (= (length b) 4))
    (should (= (aref b 0) #x78))
    (should (= (aref b 1) #x56))
    (should (= (aref b 2) #x34))
    (should (= (aref b 3) #x12))))

(ert-deftest nelisp-crt0-arm64-encode-inst-truncates ()
  "`nelisp-crt0-arm64--encode-inst' keeps only the low 32 bits."
  (let ((b (nelisp-crt0-arm64--encode-inst #x123456789ABCDEF0)))
    (should (= (aref b 0) #xF0))
    (should (= (aref b 1) #xDE))
    (should (= (aref b 2) #xBC))
    (should (= (aref b 3) #x9A))))

(ert-deftest nelisp-crt0-arm64-movz-shape ()
  "MOVZ X8, #93 encodes to 0xD2800BA8 (= bytes A8 0B 80 D2)."
  (let* ((word (nelisp-crt0-arm64--movz 8 93))
         (b    (nelisp-crt0-arm64--encode-inst word)))
    (should (= word #xD2800BA8))
    (should (= (aref b 0) #xA8))
    (should (= (aref b 1) #x0B))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))))

(ert-deftest nelisp-crt0-arm64-movz-x0-1 ()
  "MOVZ X0, #1 = 0xD2800020 (= bytes 20 00 80 D2)."
  (let ((word (nelisp-crt0-arm64--movz 0 1)))
    (should (= word #xD2800020))))

(ert-deftest nelisp-crt0-arm64-movz-x8-64 ()
  "MOVZ X8, #64 (= SYS_write) = 0xD2800808."
  (let ((word (nelisp-crt0-arm64--movz 8 64)))
    (should (= word #xD2800808))))

(ert-deftest nelisp-crt0-arm64-bl-zero ()
  "`bl 0' encodes to 0x94000000 (= 4 zero+0x94 bytes)."
  (let ((word (nelisp-crt0-arm64--bl 0)))
    (should (= word #x94000000))))

(ert-deftest nelisp-crt0-arm64-bl-positive ()
  "`bl #16' encodes to 0x94000004 (= imm26 = 16 / 4 = 4)."
  (let ((word (nelisp-crt0-arm64--bl 16)))
    (should (= word #x94000004))))

(ert-deftest nelisp-crt0-arm64-bl-rejects-misalignment ()
  "`bl' rejects byte offsets not divisible by 4."
  (should-error (nelisp-crt0-arm64--bl 3)))

(ert-deftest nelisp-crt0-arm64-adr-zero ()
  "`adr x1, +0' encodes to 0x10000001 (= immlo=0, immhi=0, Rd=1)."
  (let ((word (nelisp-crt0-arm64--adr 1 0)))
    (should (= word #x10000001))))

(ert-deftest nelisp-crt0-arm64-adr-positive ()
  "`adr x1, +28' (= for hello-world rodata) encodes to 0x100000E1.
immlo = 28 & 3 = 0; immhi = 28 >> 2 = 7; (7 << 5) | 1 = 0xE1."
  (let ((word (nelisp-crt0-arm64--adr 1 28)))
    (should (= word #x100000E1))))

(ert-deftest nelisp-crt0-arm64-adr-immlo-bits ()
  "`adr' splits the low 2 bits into immlo (= bit positions 29..30).
Offset 1 -> immlo = 1, immhi = 0; base 0x10000000 | (1 << 29) | 1 = 0x30000001."
  (let ((word (nelisp-crt0-arm64--adr 1 1)))
    (should (= word #x30000001))))

;; ---------------------------------------------------------------- §2 entry stub

(ert-deftest nelisp-crt0-arm64-entry-bytes-length ()
  "The entry stub is exactly 20 bytes (= 5 × 4-byte instructions)."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (length b) 20))
    (should (= (length b) nelisp-crt0-arm64-entry-size))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-ldr-x0-sp ()
  "Entry stub bytes 0..3 = `ldr x0, [sp]' (= E0 03 40 F9 LE)."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (aref b 0) #xE0))
    (should (= (aref b 1) #x03))
    (should (= (aref b 2) #x40))
    (should (= (aref b 3) #xF9))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-add-x1-sp-8 ()
  "Entry stub bytes 4..7 = `add x1, sp, #8' (= E1 23 00 91 LE)."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (aref b 4) #xE1))
    (should (= (aref b 5) #x23))
    (should (= (aref b 6) #x00))
    (should (= (aref b 7) #x91))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-bl-placeholder ()
  "With no rel26 arg, entry bytes 8..11 = `bl #0' (= 00 00 00 94 LE)."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (aref b 8)  #x00))
    (should (= (aref b 9)  #x00))
    (should (= (aref b 10) #x00))
    (should (= (aref b 11) #x94))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-bl-resolved ()
  "Explicit rel26 = 16 produces imm26 = 4 (= bytes 04 00 00 94 LE)."
  (let ((b (nelisp-crt0-arm64-entry-bytes 16)))
    (should (= (aref b 8)  #x04))
    (should (= (aref b 9)  #x00))
    (should (= (aref b 10) #x00))
    (should (= (aref b 11) #x94))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-mov-x8-93 ()
  "Entry stub bytes 12..15 = `mov x8, #93' (= A8 0B 80 D2 LE).
Spelled-out: MOVZ X8, #93, LSL #0; 0xD2800BA8.  Note: the literal
0xD2800B28 would be `mov x8, #89' (= imm16 = 0x59), not 93."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (aref b 12) #xA8))
    (should (= (aref b 13) #x0B))
    (should (= (aref b 14) #x80))
    (should (= (aref b 15) #xD2))))

(ert-deftest nelisp-crt0-arm64-entry-bytes-svc-0 ()
  "Entry stub bytes 16..19 = `svc #0' (= 01 00 00 D4 LE)."
  (let ((b (nelisp-crt0-arm64-entry-bytes)))
    (should (= (aref b 16) #x01))
    (should (= (aref b 17) #x00))
    (should (= (aref b 18) #x00))
    (should (= (aref b 19) #xD4))))

;; ---------------------------------------------------------------- §3 write stub

(ert-deftest nelisp-crt0-arm64-write-stdout-length ()
  "The write-stdout stub is exactly 20 bytes (= 5 × 4)."
  (let ((b (nelisp-crt0-arm64-write-stdout-bytes 0 6)))
    (should (= (length b) 20))
    (should (= (length b) nelisp-crt0-arm64-write-stdout-size))))

(ert-deftest nelisp-crt0-arm64-write-stdout-shape ()
  "Write stub: mov x0,#1; adr x1,+REL; mov x2,#LEN; mov x8,#64; svc #0."
  (let ((b (nelisp-crt0-arm64-write-stdout-bytes 28 6)))
    ;; mov x0, #1 = 20 00 80 D2
    (should (= (aref b 0) #x20))
    (should (= (aref b 1) #x00))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))
    ;; adr x1, +28 = E1 00 00 10 (= 0x100000E1 LE)
    (should (= (aref b 4) #xE1))
    (should (= (aref b 5) #x00))
    (should (= (aref b 6) #x00))
    (should (= (aref b 7) #x10))
    ;; mov x2, #6 = C2 00 80 D2 (= 0xD28000C2 LE)
    (should (= (aref b 8)  #xC2))
    (should (= (aref b 9)  #x00))
    (should (= (aref b 10) #x80))
    (should (= (aref b 11) #xD2))
    ;; mov x8, #64 = 08 08 80 D2
    (should (= (aref b 12) #x08))
    (should (= (aref b 13) #x08))
    (should (= (aref b 14) #x80))
    (should (= (aref b 15) #xD2))
    ;; svc #0 = 01 00 00 D4
    (should (= (aref b 16) #x01))
    (should (= (aref b 17) #x00))
    (should (= (aref b 18) #x00))
    (should (= (aref b 19) #xD4))))

(ert-deftest nelisp-crt0-arm64-write-stdout-rejects-bad-len ()
  "`write-stdout' rejects LEN-IMM values that don't fit MOVZ's 16-bit imm."
  (should-error (nelisp-crt0-arm64-write-stdout-bytes 0 -1))
  (should-error (nelisp-crt0-arm64-write-stdout-bytes 0 #x10000))
  (should-error (nelisp-crt0-arm64-write-stdout-bytes 0 'not-an-int)))

;; ---------------------------------------------------------------- §4 exit stub

(ert-deftest nelisp-crt0-arm64-exit-bytes-length-nil ()
  "Without STATUS-IMM, exit stub is 8 bytes (= mov x8,#93 + svc #0)."
  (let ((b (nelisp-crt0-arm64-exit-bytes)))
    (should (= (length b) 8))))

(ert-deftest nelisp-crt0-arm64-exit-bytes-shape-nil ()
  "8-byte exit stub bytes: A8 0B 80 D2 + 01 00 00 D4."
  (let ((b (nelisp-crt0-arm64-exit-bytes)))
    ;; mov x8, #93
    (should (= (aref b 0) #xA8))
    (should (= (aref b 1) #x0B))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))
    ;; svc #0
    (should (= (aref b 4) #x01))
    (should (= (aref b 5) #x00))
    (should (= (aref b 6) #x00))
    (should (= (aref b 7) #xD4))))

(ert-deftest nelisp-crt0-arm64-exit-bytes-length-with-status ()
  "With STATUS-IMM = 0, exit stub is 12 bytes."
  (let ((b (nelisp-crt0-arm64-exit-bytes 0)))
    (should (= (length b) 12))))

(ert-deftest nelisp-crt0-arm64-exit-bytes-status-zero ()
  "STATUS-IMM = 0 prepends `mov x0, #0' (= 00 00 80 D2)."
  (let ((b (nelisp-crt0-arm64-exit-bytes 0)))
    (should (= (aref b 0) #x00))
    (should (= (aref b 1) #x00))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))))

(ert-deftest nelisp-crt0-arm64-exit-bytes-status-42 ()
  "STATUS-IMM = 42 (= 0x2A) prepends `mov x0, #42' (= imm16 = 0x2A).
0xD2800540 = MOVZ X0 #42 LSL 0; LE bytes 40 05 80 D2."
  (let ((b (nelisp-crt0-arm64-exit-bytes 42)))
    (should (= (aref b 0) #x40))
    (should (= (aref b 1) #x05))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))))

(ert-deftest nelisp-crt0-arm64-exit-bytes-rejects-bad-status ()
  "STATUS-IMM out of MOVZ range raises an error."
  (should-error (nelisp-crt0-arm64-exit-bytes -1))
  (should-error (nelisp-crt0-arm64-exit-bytes #x10000))
  (should-error (nelisp-crt0-arm64-exit-bytes "nope")))

;; ---------------------------------------------------------------- §5 ret stub

(ert-deftest nelisp-crt0-arm64-ret-bytes ()
  "`ret' (= RET X30) encoded constant matches C0 03 5F D6 LE (= 0xD65F03C0)."
  (let ((b nelisp-crt0-arm64--ret-x30))
    (should (= (length b) 4))
    (should (= (aref b 0) #xC0))
    (should (= (aref b 1) #x03))
    (should (= (aref b 2) #x5F))
    (should (= (aref b 3) #xD6))))

;; ---------------------------------------------------------------- §6 hello-world

(ert-deftest nelisp-crt0-arm64-hello-world-text-length ()
  "The arm64 hello-world _start routine is exactly 32 bytes (= 8 × 4)."
  (let ((b (nelisp-crt0-arm64--hello-world-emit-text)))
    (should (= (length b) 32))
    (should (= (length b) nelisp-crt0-arm64--hello-world-text-size))))

(ert-deftest nelisp-crt0-arm64-hello-world-text-first-inst ()
  "Hello-world _start begins with `mov x0, #1' (= fd = stdout)."
  (let ((b (nelisp-crt0-arm64--hello-world-emit-text)))
    (should (= (aref b 0) #x20))
    (should (= (aref b 1) #x00))
    (should (= (aref b 2) #x80))
    (should (= (aref b 3) #xD2))))

(ert-deftest nelisp-crt0-arm64-hello-world-text-adr-rodata ()
  "Hello-world byte 4..7 = ADR X1 with offset 28 -> 0x100000E1 (E1 00 00 10)."
  (let ((b (nelisp-crt0-arm64--hello-world-emit-text)))
    (should (= (aref b 4) #xE1))
    (should (= (aref b 5) #x00))
    (should (= (aref b 6) #x00))
    (should (= (aref b 7) #x10))))

(ert-deftest nelisp-crt0-arm64-hello-world-text-msg-len ()
  "Hello-world byte 8..11 = `mov x2, #6' (= C2 00 80 D2)."
  (let ((b (nelisp-crt0-arm64--hello-world-emit-text)))
    (should (= (aref b 8)  #xC2))
    (should (= (aref b 9)  #x00))
    (should (= (aref b 10) #x80))
    (should (= (aref b 11) #xD2))))

(ert-deftest nelisp-crt0-arm64-hello-world-text-exit-tail ()
  "Hello-world bytes 20..31 = mov x0,#0; mov x8,#93; svc #0."
  (let ((b (nelisp-crt0-arm64--hello-world-emit-text)))
    ;; mov x0, #0 = 00 00 80 D2
    (should (= (aref b 20) #x00))
    (should (= (aref b 21) #x00))
    (should (= (aref b 22) #x80))
    (should (= (aref b 23) #xD2))
    ;; mov x8, #93 = A8 0B 80 D2
    (should (= (aref b 24) #xA8))
    (should (= (aref b 25) #x0B))
    (should (= (aref b 26) #x80))
    (should (= (aref b 27) #xD2))
    ;; svc #0 = 01 00 00 D4
    (should (= (aref b 28) #x01))
    (should (= (aref b 29) #x00))
    (should (= (aref b 30) #x00))
    (should (= (aref b 31) #xD4))))

(ert-deftest nelisp-crt0-arm64-emit-hello-world-magic ()
  "`emit-hello-world' writes an ELF file with the right magic + EM_AARCH64."
  (let ((path (make-temp-file "nelisp-crt0-arm64-hello-emit-")))
    (unwind-protect
        (progn
          (nelisp-crt0-arm64-emit-hello-world path)
          (should (file-exists-p path))
          (let ((bytes (nelisp-crt0-arm64-test--read-file-bytes path)))
            ;; ELF magic.
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            ;; ELFCLASS64 + ELFDATA2LSB.
            (should (= (aref bytes 4) 2))
            (should (= (aref bytes 5) 1))
            ;; e_machine at offset 0x12: 0xB7 0x00 = 183 = EM_AARCH64.
            (should (= (aref bytes #x12) #xB7))
            (should (= (aref bytes #x13) #x00))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-crt0-arm64-emit-hello-world-text-on-disk ()
  "Emitted binary has the .text block at file offset 0x78 starting with
`mov x0, #1' (= 20 00 80 D2) and the message bytes `hello\\n' at 0x78+32."
  (let ((path (make-temp-file "nelisp-crt0-arm64-hello-text-")))
    (unwind-protect
        (progn
          (nelisp-crt0-arm64-emit-hello-world path)
          (let ((bytes (nelisp-crt0-arm64-test--read-file-bytes path)))
            ;; .text first instruction = `mov x0, #1'.
            (should (= (aref bytes #x78) #x20))
            (should (= (aref bytes (+ #x78 1)) #x00))
            (should (= (aref bytes (+ #x78 2)) #x80))
            (should (= (aref bytes (+ #x78 3)) #xD2))
            ;; `hello\n' immediately after the 32-byte .text.
            (should (equal
                     (substring bytes (+ #x78 32) (+ #x78 32 6))
                     (unibyte-string ?h ?e ?l ?l ?o ?\n)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-crt0-arm64-emit-hello-world-readelf-h ()
  "`readelf -h' reports EXEC + AArch64 for the emitted binary."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-crt0-arm64-readelf-")))
    (unwind-protect
        (progn
          (nelisp-crt0-arm64-emit-hello-world path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-h" path)))))
            (should (string-match-p "ELF64" out))
            (should (string-match-p "EXEC" out))
            (should (string-match-p "AArch64\\|aarch64\\|ARM" out))
            ;; Entry point lives in the loaded segment (>= 0x400000).
            (should (string-match-p "Entry point address:[ \t]+0x40" out))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-crt0-arm64-emit-hello-world-exec ()
  "End-to-end: exec the emitted hello-world binary on aarch64 host.
Skipped on x86_64 / non-Linux because the kernel cannot load
EM_AARCH64 binaries without qemu-aarch64 binfmt."
  (skip-unless (nelisp-crt0-arm64-test--aarch64-host-p))
  (let ((path   (make-temp-file "nelisp-crt0-arm64-hello-exec-"))
        (output (make-temp-file "nelisp-crt0-arm64-hello-out-")))
    (unwind-protect
        (progn
          (nelisp-crt0-arm64-emit-hello-world path)
          (let ((rc (call-process path nil (list :file output) nil)))
            (should (eq rc 0))
            (should
             (equal
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally output)
                (buffer-substring-no-properties (point-min) (point-max)))
              (unibyte-string ?h ?e ?l ?l ?o ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(provide 'nelisp-crt0-arm64-test)

;;; nelisp-crt0-arm64-test.el ends here
