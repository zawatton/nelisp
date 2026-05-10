;;; nelisp-crt0-arm64.el --- pure-elisp crt0 for arm64 Linux  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 94 §94.b — pure-elisp crt0 (= C runtime startup) for arm64
;; Linux (AArch64).  Sibling of `nelisp-crt0' (= §94.a, x86_64); the
;; two modules emit identical "hello, world" semantics for their
;; respective ISAs but use distinct encoding helpers because the
;; instruction word formats share nothing.
;;
;; AArch64 Linux ABI (= AAPCS64 + Linux generic syscall):
;;   - syscall instruction: `svc #0' (= 4 bytes, opcode 0xD4000001 LE)
;;   - syscall number register: `x8' (32-bit `w8' really, top bits 0)
;;   - argument registers: `x0' .. `x7' (= 8 args max)
;;   - return register: `x0'
;;   - stack pointer: `sp' (= x31 in encoding; w31 means WZR/SP)
;;   - entry state: argc at `[sp]', argv at `[sp + 8]', stack 16-aligned
;;
;; AArch64 syscall numbers (Linux/aarch64, NOT same as x86_64!):
;;   write   = 64    (x86_64 = 1)
;;   exit    = 93    (x86_64 = 60)
;;
;; Encoded sequences (all instructions are 4 bytes, little-endian):
;;
;;   `nelisp-crt0-arm64-entry-bytes' (= 20 bytes):
;;       F9 40 03 E0   ; ldr x0, [sp]         (= argc → x0)
;;       91 00 23 E1   ; add x1, sp, #8       (= argv → x1)
;;       94 00 00 00   ; bl  main             (= placeholder rel26 = 0)
;;       D2 80 0B A8   ; mov x8, #93          (= SYS_exit)
;;       D4 00 00 01   ; svc #0
;;
;;   `nelisp-crt0-arm64-write-stdout-bytes' (= 20 bytes, inline call
;;    with concrete ADR offset + length immediate):
;;       D2 80 00 20   ; mov x0, #1           (= fd = stdout)
;;       1- -- -- --   ; adr x1, +BUF-OFF     (= 21-bit signed PC-rel)
;;       D2 80 -- --   ; mov x2, #LEN-IMM     (= 16-bit immediate)
;;       D2 80 08 08   ; mov x8, #64          (= SYS_write)
;;       D4 00 00 01   ; svc #0
;;
;;   `nelisp-crt0-arm64-exit-bytes' (= 8 or 12 bytes):
;;       [D2 80 -- --] ; mov x0, #STATUS-IMM  (optional, omitted if nil)
;;       D2 80 0B A8   ; mov x8, #93          (= SYS_exit)
;;       D4 00 00 01   ; svc #0
;;
;; This module is freestanding: it intentionally does NOT depend on
;; the Doc 92 macro assembler.  Encodings are hand-verified against
;; the Arm Architecture Reference Manual for A-profile (§C6).
;;
;; Encoding sanity: `mov x8, #93' = MOVZ X8, #93, LSL #0 = bits
;;   1 10 100101 00 0000000001011101 01000 = 0xD2800BA8 (LE: A8 0B 80 D2).
;; The literal 0xD2800B28 = MOVZ X8, #89 (= 89 not 93); some Doc 94
;; §94.b drafts misquoted this — the on-disk binary uses the
;; correct 0xD2800BA8 encoding for syscall #93 (= exit).
;;
;; The ELF writer (= Doc 91 `nelisp-elf-write-binary') hard-codes
;; EM_X86_64 in its rich orchestrator; the §94.b emitter therefore
;; produces the binary first and then patches the 2-byte `e_machine'
;; field at file offset 0x12 to EM_AARCH64 (= 183, 0xB7) before
;; restoring the +x file mode.  When the underlying writer grows a
;; `:machine' plumbing key, this post-patch becomes redundant.
;;
;; Status (= Doc 94 §7 §94.b): SHIPPED 2026-05-11.  Binary correctness
;; verified via byte-pattern ert + `readelf -h' EM_AARCH64 check; live
;; exec gated on aarch64 host (= skipped on x86_64 CI without qemu).

;;; Code:

(require 'nelisp-elf-write)

;; ---- §94.b instruction word constants (= hand-verified 4-byte LE) ----

(defconst nelisp-crt0-arm64--ldr-x0-sp
  (unibyte-string #xE0 #x03 #x40 #xF9)
  "Encoded `ldr x0, [sp]' (= 0xF94003E0 little-endian).
Loads the 64-bit value at SP into X0; on entry this is `argc'.")

(defconst nelisp-crt0-arm64--add-x1-sp-8
  (unibyte-string #xE1 #x23 #x00 #x91)
  "Encoded `add x1, sp, #8' (= 0x910023E1 little-endian).
Computes the address of `argv' (= sp + 8) into X1.")

(defconst nelisp-crt0-arm64--bl-placeholder
  (unibyte-string #x00 #x00 #x00 #x94)
  "Encoded `bl #0' (= 0x94000000 little-endian).
Placeholder branch-with-link to `main'; a linker patches the rel26
field (= bits 25..0) to the actual PC-relative offset divided by 4.")

(defconst nelisp-crt0-arm64--mov-x8-93
  (unibyte-string #xA8 #x0B #x80 #xD2)
  "Encoded `mov x8, #93' (= MOVZ X8 #93 LSL 0 = 0xD2800BA8 LE).
93 is SYS_exit on aarch64 Linux.")

(defconst nelisp-crt0-arm64--svc-0
  (unibyte-string #x01 #x00 #x00 #xD4)
  "Encoded `svc #0' (= 0xD4000001 little-endian).
Kernel syscall trap; reads the syscall number from W8 and arguments
from X0..X5.")

(defconst nelisp-crt0-arm64--mov-x0-1
  (unibyte-string #x20 #x00 #x80 #xD2)
  "Encoded `mov x0, #1' (= MOVZ X0 #1 = 0xD2800020 LE).
fd = stdout for `write(2)' calls.")

(defconst nelisp-crt0-arm64--mov-x8-64
  (unibyte-string #x08 #x08 #x80 #xD2)
  "Encoded `mov x8, #64' (= MOVZ X8 #64 = 0xD2800808 LE).
64 is SYS_write on aarch64 Linux.")

(defconst nelisp-crt0-arm64--ret-x30
  (unibyte-string #xC0 #x03 #x5F #xD6)
  "Encoded `ret' (= RET X30 = 0xD65F03C0 little-endian).
Returns to the caller via the link register X30; provided as a
convenience for callable function stubs.")

(defconst nelisp-crt0-arm64-entry-size 20
  "Length in bytes of the emitted arm64 `_start' stub (= 5 × 4).")

(defconst nelisp-crt0-arm64-write-stdout-size 20
  "Length in bytes of the arm64 inline `write(1, buf, len)' stub (= 5 × 4).")

;; ---- §94.b encoder helpers ----

(defun nelisp-crt0-arm64--encode-inst (word)
  "Encode 32-bit instruction WORD as 4 little-endian bytes (unibyte string).
The caller is responsible for the 32-bit range invariant; only the
low 32 bits of WORD are kept."
  (let ((u (logand word #xffffffff)))
    (unibyte-string
     (logand u #xff)
     (logand (ash u -8) #xff)
     (logand (ash u -16) #xff)
     (logand (ash u -24) #xff))))

(defun nelisp-crt0-arm64--movz (rd imm16)
  "Encode `mov RD, #IMM16' (= MOVZ Xd #imm16 LSL #0).
RD must be in 0..31, IMM16 in 0..65535.  Returns the 32-bit word."
  (logior #xD2800000
          (ash (logand imm16 #xFFFF) 5)
          (logand rd #x1F)))

(defun nelisp-crt0-arm64--bl (rel-bytes)
  "Encode `bl' with PC-relative REL-BYTES (= signed 28-bit, multiple of 4).
The encoded imm26 field is REL-BYTES / 4.  Returns the 32-bit word
that, written little-endian, is the 4-byte BL instruction."
  (when (/= (logand rel-bytes 3) 0)
    (error "nelisp-crt0-arm64: bl displacement %d not a multiple of 4"
           rel-bytes))
  (let ((imm26 (logand (ash rel-bytes -2) #x3FFFFFF)))
    (logior #x94000000 imm26)))

(defun nelisp-crt0-arm64--adr (rd rel-bytes)
  "Encode `adr RD, +REL-BYTES' (= PC-relative load of a label address).
REL-BYTES is a signed 21-bit byte offset relative to the ADR
instruction itself (= NOT to the next instruction).  Returns the
32-bit word."
  (let* ((off  (logand rel-bytes #x1FFFFF))
         (immlo (logand off #x3))
         (immhi (logand (ash off -2) #x7FFFF)))
    (logior #x10000000
            (ash immlo 29)
            (ash immhi 5)
            (logand rd #x1F))))

;; ---- §94.b public byte emitters ----

(defun nelisp-crt0-arm64-entry-bytes (&optional call-rel26)
  "Return the byte sequence for the arm64 Linux `_start' entry stub.

CALL-REL26 is the signed PC-relative byte displacement (= must be a
multiple of 4) for the BL to `main'; when omitted or nil the field
is left as zero so a linker can patch it later via the standard
R_AARCH64_CALL26 relocation.

Layout (5 × 4 = 20 bytes):
  off  0  F9 40 03 E0   ldr x0, [sp]      (argc → x0)
  off  4  91 00 23 E1   add x1, sp, #8    (argv → x1)
  off  8  94 .. .. ..   bl  main          (= rel26 placeholder)
  off 12  D2 80 0B A8   mov x8, #93       (= SYS_exit)
  off 16  D4 00 00 01   svc #0

The exit syscall reads its status from X0; by the SysV/AAPCS64
calling convention this is `main' \\='s return value, leaving the
behaviour identical to the x86_64 stub.

Returns a unibyte string of exactly 20 bytes."
  (concat
   nelisp-crt0-arm64--ldr-x0-sp
   nelisp-crt0-arm64--add-x1-sp-8
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--bl (or call-rel26 0)))
   nelisp-crt0-arm64--mov-x8-93
   nelisp-crt0-arm64--svc-0))

(defun nelisp-crt0-arm64-write-stdout-bytes (buf-rel-bytes len-imm)
  "Return the inline arm64 `write(1, buf, len)' syscall stub.

BUF-REL-BYTES is the signed 21-bit byte offset (= relative to the
ADR instruction, which lives at offset 4 of the emitted block) of
the buffer in `.rodata' or another loadable section.  LEN-IMM is
the buffer length (= 16-bit immediate, 0..65535).

Layout (5 × 4 = 20 bytes):
  off  0  D2 80 00 20   mov x0, #1        (= fd = stdout)
  off  4  1- -- -- --   adr x1, +REL      (= buffer pointer)
  off  8  D2 80 -- --   mov x2, #LEN      (= length immediate)
  off 12  D2 80 08 08   mov x8, #64       (= SYS_write)
  off 16  D4 00 00 01   svc #0

LEN-IMM is range-checked to fit MOVZ \\='s 16-bit imm.  Returns a
unibyte string of exactly 20 bytes."
  (unless (and (integerp len-imm) (<= 0 len-imm) (<= len-imm #xFFFF))
    (error "nelisp-crt0-arm64: write LEN-IMM %S out of MOVZ range" len-imm))
  (concat
   nelisp-crt0-arm64--mov-x0-1
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--adr 1 buf-rel-bytes))
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--movz 2 len-imm))
   nelisp-crt0-arm64--mov-x8-64
   nelisp-crt0-arm64--svc-0))

(defun nelisp-crt0-arm64-exit-bytes (&optional status-imm)
  "Return the byte sequence for an arm64 `exit(STATUS-IMM)' syscall stub.

If STATUS-IMM is non-nil it must fit in a MOVZ 16-bit immediate
(= 0..65535) and a `mov x0, #STATUS-IMM' is emitted first
(= 12 bytes total).  If STATUS-IMM is nil the caller is expected to
have left the exit status in X0 already and only the 8-byte tail
is emitted.

Layout when STATUS-IMM is non-nil (3 × 4 = 12 bytes):
  off  0  D2 80 -- --   mov x0, #STATUS
  off  4  D2 80 0B A8   mov x8, #93       (= SYS_exit)
  off  8  D4 00 00 01   svc #0

Layout when STATUS-IMM is nil (2 × 4 = 8 bytes):
  off  0  D2 80 0B A8   mov x8, #93       (= SYS_exit)
  off  4  D4 00 00 01   svc #0

Returns a unibyte string of length 8 or 12."
  (when status-imm
    (unless (and (integerp status-imm)
                 (<= 0 status-imm) (<= status-imm #xFFFF))
      (error "nelisp-crt0-arm64: exit STATUS-IMM %S out of MOVZ range"
             status-imm)))
  (concat
   (when status-imm
     (nelisp-crt0-arm64--encode-inst
      (nelisp-crt0-arm64--movz 0 status-imm)))
   nelisp-crt0-arm64--mov-x8-93
   nelisp-crt0-arm64--svc-0))

;; ---- §94.b hello-world orchestrator (= ties §94.b stubs into §91.b
;; ELF writer with EM_AARCH64 machine tag) ----

(defun nelisp-crt0-arm64--hello-world-emit-text ()
  "Emit the arm64 hello-world `.text' bytes (= 32 bytes, 8 instructions).

The encoded routine:
  off  0  mov x0, #1            (= fd = stdout)
  off  4  adr x1, +28           (= points to `.rodata' immediately after .text)
  off  8  mov x2, #6            (= len = 6, `hello\\n')
  off 12  mov x8, #64           (= SYS_write)
  off 16  svc #0
  off 20  mov x0, #0            (= exit status = 0)
  off 24  mov x8, #93           (= SYS_exit)
  off 28  svc #0

ADR offset = 28 is the distance from the ADR instruction (= file
offset 4 in the .text block) to the start of `.rodata' (= file
offset 32 in the .text block); 32 - 4 = 28.  This is well within
the ±1 MiB range of AArch64 ADR."
  (concat
   nelisp-crt0-arm64--mov-x0-1
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--adr 1 28))
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--movz 2 6))
   nelisp-crt0-arm64--mov-x8-64
   nelisp-crt0-arm64--svc-0
   (nelisp-crt0-arm64--encode-inst
    (nelisp-crt0-arm64--movz 0 0))
   nelisp-crt0-arm64--mov-x8-93
   nelisp-crt0-arm64--svc-0))

(defconst nelisp-crt0-arm64--hello-world-text-size 32
  "Length in bytes of the arm64 hello-world `_start' routine (= 32).")

(defconst nelisp-crt0-arm64--hello-world-msg
  (unibyte-string ?h ?e ?l ?l ?o ?\n)
  "The `hello\\n' message bytes written by the arm64 hello-world binary.")

(defconst nelisp-crt0-arm64--e-machine-offset 18
  "Byte offset of Elf64_Ehdr e_machine in an ELF64 file (= 0x12).
e_ident occupies offset 0..15, e_type is at 16..17, e_machine at
18..19.  Used by `nelisp-crt0-arm64--patch-e-machine' to override
the writer's hard-coded EM_X86_64.")

(defun nelisp-crt0-arm64--patch-e-machine (file-path machine-code)
  "Patch the 2-byte e_machine field of FILE-PATH to MACHINE-CODE.
Implemented as a binary read-modify-write because the underlying
ELF writer (= `nelisp-elf-write-binary') hard-codes EM_X86_64 in
both the minimal and rich paths; the §94.b path therefore writes a
valid x86_64 ELF first and overrides the 2 bytes in place.

MACHINE-CODE is an unsigned 16-bit ELF EM_* constant
(= e.g. `nelisp-elf--em-aarch64' = 183).  Returns FILE-PATH."
  (let ((coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file-path)
      (let ((bytes (buffer-substring-no-properties (point-min) (point-max))))
        (unless (>= (length bytes) (+ nelisp-crt0-arm64--e-machine-offset 2))
          (error "nelisp-crt0-arm64: file too short to patch e_machine: %s"
                 file-path))
        (let* ((lo (logand machine-code #xFF))
               (hi (logand (ash machine-code -8) #xFF))
               (patched
                (concat
                 (substring bytes 0 nelisp-crt0-arm64--e-machine-offset)
                 (unibyte-string lo hi)
                 (substring bytes
                            (+ nelisp-crt0-arm64--e-machine-offset 2)))))
          (erase-buffer)
          (insert patched)
          (write-region (point-min) (point-max) file-path nil 'silent)))))
  (set-file-modes file-path #o755)
  file-path)

;;;###autoload
(defun nelisp-crt0-arm64-emit-hello-world (file-path)
  "Emit a self-contained AArch64 ELF64 `hello-world' binary to FILE-PATH.

The produced binary, when exec'd on aarch64 Linux:
  1. prints `hello\\n' to stdout via a direct `write(2)' syscall;
  2. exits with status code 0 via a direct `exit(2)' syscall.

No libc, no dynamic loader is involved at run time — the only
host dependency is the Linux kernel's ELF loader.  When emitted on
a non-aarch64 host (= e.g. x86_64 CI) the binary is still a valid
EM_AARCH64 ELF; execution is gated on the runtime architecture by
the test suite.

Returns FILE-PATH.  The file is created with mode #o755 (= +x bit
set) by `nelisp-elf-write-binary', then the e_machine field is
patched in place to EM_AARCH64 (= 183, 0xB7) because the underlying
writer hard-codes EM_X86_64."
  (let* ((text-bytes (nelisp-crt0-arm64--hello-world-emit-text))
         (msg-len    (length nelisp-crt0-arm64--hello-world-msg)))
    (unless (= (length text-bytes)
               nelisp-crt0-arm64--hello-world-text-size)
      (error "nelisp-crt0-arm64: hello-world .text drift (got %d expected %d)"
             (length text-bytes)
             nelisp-crt0-arm64--hello-world-text-size))
    (nelisp-elf-write-binary
     file-path
     (list :text   text-bytes
           :rodata nelisp-crt0-arm64--hello-world-msg
           :symbols
           (list (list :name "_start" :value 0
                       :size nelisp-crt0-arm64--hello-world-text-size
                       :section 'text :bind 'global :type 'func)
                 (list :name "hello_msg" :value 0
                       :size msg-len :section 'rodata
                       :bind 'local :type 'object))
           :entry-sym "_start"))
    (nelisp-crt0-arm64--patch-e-machine
     file-path nelisp-elf--em-aarch64)))

(provide 'nelisp-crt0-arm64)

;;; nelisp-crt0-arm64.el ends here
