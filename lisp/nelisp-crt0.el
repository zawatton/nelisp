;;; nelisp-crt0.el --- pure-elisp crt0 (Phase 47 spike)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 94 §94.a — pure-elisp crt0 (= C runtime startup) for x86_64
;; Linux.  Emits raw machine-code byte sequences for the kernel `_start'
;; entry point, the `write(2)' / `exit(2)' syscall stubs, and an
;; integrated "hello, world" orchestrator that wires Doc 91's ELF
;; writer (`nelisp-elf-write-binary') into a complete static-PIE
;; executable.
;;
;; The byte-emit helpers are self-contained: they intentionally do
;; NOT depend on the Doc 92 macro assembler (= shipping in parallel
;; on a sibling branch).  Each stub is a short, manually-encoded
;; sequence verifiable against `objdump' / Intel SDM Vol 2.
;;
;; Encoded sequences (x86_64 SysV ABI):
;;
;;   `nelisp-crt0-x86_64-entry-bytes' (= 23 bytes):
;;       48 83 e4 f0       ; and    rsp, -16     (stack align)
;;       5f                ; pop    rdi          (argc → rdi)
;;       48 89 e6          ; mov    rsi, rsp     (argv → rsi)
;;       e8 RR RR RR RR    ; call   main         (rel32 = REL or 0)
;;       48 89 c7          ; mov    rdi, rax     (exit status)
;;       b8 3c 00 00 00    ; mov    eax, 60      (SYS_exit)
;;       0f 05             ; syscall
;;
;;   `nelisp-crt0-x86_64-write-stdout-bytes' (= 19 bytes, callable,
;;    expects rdi = buf, rsi = len):
;;       48 89 fe          ; mov    rsi, rdi     (buf → 2nd arg)
;;       48 89 f2          ; mov    rdx, rsi     (len → 3rd arg) — but
;;       ...              wait: we need to save len BEFORE clobbering
;;       rsi, so the actual emit order is:
;;
;;       48 89 f2          ; mov    rdx, rsi     (len → rdx; do this first)
;;       48 89 fe          ; mov    rsi, rdi     (buf → rsi)
;;       bf 01 00 00 00    ; mov    edi, 1       (fd = 1, stdout)
;;       b8 01 00 00 00    ; mov    eax, 1       (SYS_write)
;;       0f 05             ; syscall
;;       c3                ; ret
;;
;;   `nelisp-crt0-x86_64-exit-bytes' (= 7 bytes, rdi already holds
;;    status):
;;       b8 3c 00 00 00    ; mov    eax, 60      (SYS_exit)
;;       0f 05             ; syscall
;;
;; Status (= Doc 94 §7 §94.a SHIPPED): x86_64 Linux only.  arm64
;; (§94.b) and argv/envp/auxv unpack (§94.c) are deferred per Doc 94
;; §7.2 / §7.3.

;;; Code:

(require 'nelisp-elf-write)

;; ---- §94.a byte sequence constants (= encoded by hand from Intel
;; SDM Vol 2; lengths must stay in sync with the disassembly above) ----

(defconst nelisp-crt0--x86_64-entry-prologue
  (unibyte-string #x48 #x83 #xe4 #xf0   ; and    rsp, -16
                  #x5f                  ; pop    rdi    (argc)
                  #x48 #x89 #xe6)       ; mov    rsi, rsp (argv)
  "Bytes for the `_start' prologue up to (but not including) `call main'.
Length = 8 bytes.  After this sequence, the next instruction emitted
is the relative call (= 5 bytes: opcode `e8' + 4-byte rel32).")

(defconst nelisp-crt0--x86_64-entry-epilogue
  (unibyte-string #x48 #x89 #xc7        ; mov    rdi, rax (main's rc)
                  #xb8 #x3c #x00 #x00 #x00 ; mov  eax, 60 (SYS_exit)
                  #x0f #x05)            ; syscall
  "Bytes for the `_start' epilogue after `main' returns.
Length = 10 bytes.  Reads `rax' (= main's return value) into `rdi'
and traps to the kernel via the `exit' syscall (= number 60).")

(defconst nelisp-crt0-x86_64-entry-prologue-size 8
  "Length in bytes of the prologue placed before the call instruction.")

(defconst nelisp-crt0-x86_64-call-size 5
  "Length in bytes of the `call rel32' instruction (= opcode + rel32).")

(defconst nelisp-crt0-x86_64-entry-epilogue-size 10
  "Length in bytes of the epilogue placed after the call instruction.")

(defconst nelisp-crt0-x86_64-entry-size
  (+ nelisp-crt0-x86_64-entry-prologue-size
     nelisp-crt0-x86_64-call-size
     nelisp-crt0-x86_64-entry-epilogue-size)
  "Total length in bytes of the emitted x86_64 `_start' stub (= 23).")

;; ---- §94.a public byte emitters ----

(defun nelisp-crt0--encode-rel32 (rel)
  "Encode REL (signed 32-bit int) as 4 little-endian bytes (unibyte string).
REL is the rel32 displacement for `call'/`jmp' instructions; the
caller is responsible for the +/-2^31 range invariant."
  (let ((u (logand rel #xffffffff)))
    (unibyte-string
     (logand u #xff)
     (logand (ash u -8) #xff)
     (logand (ash u -16) #xff)
     (logand (ash u -24) #xff))))

(defun nelisp-crt0-x86_64-entry-bytes (&optional call-rel32)
  "Return the byte sequence for the x86_64 Linux `_start' entry stub.

CALL-REL32 is the signed 32-bit displacement for the call to `main'
(= relative to the instruction *after* the `call' opcode); when
omitted or nil, four `00' bytes are emitted as a placeholder that a
linker can patch later via `R_X86_64_PLT32'/`R_X86_64_PC32'.

Returns a unibyte string of exactly 23 bytes."
  (concat nelisp-crt0--x86_64-entry-prologue
          (unibyte-string #xe8)
          (nelisp-crt0--encode-rel32 (or call-rel32 0))
          nelisp-crt0--x86_64-entry-epilogue))

(defun nelisp-crt0-x86_64-write-stdout-bytes ()
  "Return the byte sequence for a callable `write(1, buf, len)' stub.

The emitted function expects:
  rdi = pointer to buffer (= argument 1)
  rsi = length in bytes   (= argument 2)
and clobbers rax/rdi/rsi/rdx/rcx/r11 per the syscall ABI.  Returns
the kernel's `write(2)' result in rax and falls through to `ret'.

Returns a unibyte string of exactly 19 bytes."
  (unibyte-string
   #x48 #x89 #xf2                       ; mov    rdx, rsi (len → 3rd arg)
   #x48 #x89 #xfe                       ; mov    rsi, rdi (buf → 2nd arg)
   #xbf #x01 #x00 #x00 #x00             ; mov    edi, 1   (fd = stdout)
   #xb8 #x01 #x00 #x00 #x00             ; mov    eax, 1   (SYS_write)
   #x0f #x05                            ; syscall
   #xc3))                               ; ret

(defun nelisp-crt0-x86_64-exit-bytes ()
  "Return the byte sequence for an inline `exit(rdi)' syscall.

The caller must place the desired status in `rdi' before this
sequence; for example, after `mov rdi, rax' the exit code is
whatever `main' just returned.

Returns a unibyte string of exactly 7 bytes."
  (unibyte-string
   #xb8 #x3c #x00 #x00 #x00             ; mov    eax, 60 (SYS_exit)
   #x0f #x05))                          ; syscall

;; ---- §94.a hello-world orchestrator (= ties §94.a stubs into §91.b
;; ELF writer for an end-to-end smoke binary) ----

(defun nelisp-crt0--hello-world-emit-text (msg-len text-vaddr rodata-vaddr)
  "Emit hello-world .text bytes with a resolved RIP-relative load.
MSG-LEN is the .rodata buffer length.  TEXT-VADDR is the runtime
virtual address of `_start' (= start of .text).  RODATA-VADDR is the
runtime virtual address of the message buffer (= start of .rodata).

Layout returned (= 37 bytes total, = 4+5+7+5+5+2+2+5+2):
  off  0  48 83 e4 f0          and  rsp, -16
  off  4  bf 01 00 00 00       mov  edi, 1
  off  9  48 8d 35 RR RR RR RR lea  rsi, [rip+REL]
  off 16  ba LL 00 00 00       mov  edx, LEN
  off 21  b8 01 00 00 00       mov  eax, 1
  off 26  0f 05                syscall
  off 28  31 ff                xor  edi, edi
  off 30  b8 3c 00 00 00       mov  eax, 60
  off 35  0f 05                syscall

This intentionally bypasses the `call main' pattern (= no relocation
required) so the orchestrator can produce a runnable ET_EXEC without
depending on Doc 93's linker."
  (let* (;; rel32 for `lea rsi, [rip+rel]' is relative to the next
         ;; instruction (= the byte after the lea, which is at offset
         ;; 16 in .text).  RODATA-VADDR - (TEXT-VADDR + 16) = rel32.
         (next-after-lea (+ text-vaddr 16))
         (rel32 (- rodata-vaddr next-after-lea))
         (len-imm (logand msg-len #xffffffff)))
    (concat
     (unibyte-string #x48 #x83 #xe4 #xf0)                  ; and rsp, -16
     (unibyte-string #xbf #x01 #x00 #x00 #x00)             ; mov edi, 1
     (unibyte-string #x48 #x8d #x35)                       ; lea rsi, [rip+
     (nelisp-crt0--encode-rel32 rel32)                     ;   rel32]
     (unibyte-string #xba)                                 ; mov edx, imm32
     (nelisp-crt0--encode-rel32 len-imm)
     (unibyte-string #xb8 #x01 #x00 #x00 #x00)             ; mov eax, 1
     (unibyte-string #x0f #x05)                            ; syscall
     (unibyte-string #x31 #xff)                            ; xor edi, edi
     (unibyte-string #xb8 #x3c #x00 #x00 #x00)             ; mov eax, 60
     (unibyte-string #x0f #x05))))                         ; syscall

(defconst nelisp-crt0--hello-world-text-size 37
  "Length in bytes of the hello-world `_start' routine (= 37).
Used by the orchestrator to compute the .rodata vaddr before
emitting the RIP-relative displacement.")

(defconst nelisp-crt0--hello-world-msg
  (unibyte-string ?h ?e ?l ?l ?o ?\n)
  "The `hello\\n' message bytes written by the hello-world binary.
Length = 6 (= `h' `e' `l' `l' `o' `\\n').")

;;;###autoload
(defun nelisp-crt0-emit-hello-world (file-path)
  "Emit a self-contained ELF64 `hello-world' binary to FILE-PATH.
The produced binary, when exec'd on x86_64 Linux:
  1. prints `hello\\n' to stdout via a direct `write(2)' syscall;
  2. exits with status code 0 via a direct `exit(2)' syscall.
No libc, no dynamic loader, no Rust runtime is involved at run time
— the only host dependency is the Linux kernel's ELF loader.

Returns FILE-PATH.  The file is created with mode #o755 (= +x bit
set) by `nelisp-elf-write-binary'."
  (let* (;; Phdr layout in `nelisp-elf--build-rich': Ehdr (64) + Phdr
         ;; (56) at file offset 0, then .text at offset 0x78, then
         ;; .rodata immediately after.
         (vaddr-base #x400000)
         (text-off  (+ 64 56))           ; = 0x78
         (text-vaddr (+ vaddr-base text-off))
         (msg-len   (length nelisp-crt0--hello-world-msg))
         (rodata-off (+ text-off nelisp-crt0--hello-world-text-size))
         (rodata-vaddr (+ vaddr-base rodata-off))
         (text-bytes (nelisp-crt0--hello-world-emit-text
                      msg-len text-vaddr rodata-vaddr)))
    ;; Sanity: emit-text length must match the published constant.
    (unless (= (length text-bytes) nelisp-crt0--hello-world-text-size)
      (error "nelisp-crt0: hello-world .text drift (got %d expected %d)"
             (length text-bytes) nelisp-crt0--hello-world-text-size))
    (nelisp-elf-write-binary
     file-path
     (list :text  text-bytes
           :rodata nelisp-crt0--hello-world-msg
           :symbols
           (list (list :name "_start" :value 0
                       :size nelisp-crt0--hello-world-text-size
                       :section 'text :bind 'global :type 'func)
                 (list :name "hello_msg" :value 0
                       :size msg-len :section 'rodata
                       :bind 'local :type 'object))
           :entry-sym "_start"))))

(provide 'nelisp-crt0)

;;; nelisp-crt0.el ends here
