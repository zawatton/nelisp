;;; nelisp-crt0.el --- pure-elisp crt0 (AOT spike)  -*- lexical-binding: t; -*-

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
;; Status (= Doc 94 §7 §94.a + §94.c SHIPPED): x86_64 Linux only.
;; arm64 (§94.b) lives in `lisp/nelisp-crt0-arm64.el'.
;;
;; §94.c additions (= argv / envp / auxv unpacking):
;;
;;   `nelisp-crt0-x86_64-full-entry-bytes' (= 44 bytes, optional
;;    rel32):
;;       48 8b 3c 24       ; mov    rdi, [rsp]    (argc → 1st arg)
;;       48 8d 74 24 08    ; lea    rsi, [rsp+8]  (argv → 2nd arg)
;;       48 89 f9          ; mov    rcx, rdi      (argc copy)
;;       48 ff c1          ; inc    rcx           (argc+1)
;;       48 8d 14 ce       ; lea    rdx, [rsi+rcx*8] (envp → 3rd arg)
;;       48 89 e5          ; mov    rbp, rsp      (save rsp)
;;       48 83 e4 f0       ; and    rsp, -16      (16-byte align)
;;       e8 RR RR RR RR    ; call   main          (rel32 placeholder)
;;       48 89 ec          ; mov    rsp, rbp      (restore rsp)
;;       48 89 c7          ; mov    rdi, rax      (exit status)
;;       b8 3c 00 00 00    ; mov    eax, 60       (SYS_exit)
;;       0f 05             ; syscall
;;
;;   `nelisp-crt0-x86_64-find-auxv-bytes' (= 16 bytes, callable,
;;    rdi = envp ptr, returns auxv ptr in rax):
;;       48 89 f8          ; mov    rax, rdi
;;     L0:
;;       48 8b 08          ; mov    rcx, [rax]
;;       48 83 c0 08       ; add    rax, 8
;;       48 85 c9          ; test   rcx, rcx
;;       75 f4             ; jne    L0
;;       c3                ; ret
;;
;;   `nelisp-crt0-x86_64-getauxval-bytes' (= 27 bytes, callable,
;;    rdi = auxv ptr, rsi = type wanted, returns value in rax):
;;     L0:
;;       48 8b 07          ; mov    rax, [rdi]
;;       48 85 c0          ; test   rax, rax
;;       74 10             ; je     LNOT
;;       48 39 f0          ; cmp    rax, rsi
;;       74 06             ; je     LFOUND
;;       48 83 c7 10       ; add    rdi, 16
;;       eb ed             ; jmp    L0
;;     LFOUND:
;;       48 8b 47 08       ; mov    rax, [rdi+8]
;;       c3                ; ret
;;     LNOT:
;;       31 c0             ; xor    eax, eax
;;       c3                ; ret
;;
;; The §94.c `nelisp-crt0-emit-hello-with-argc' orchestrator wires
;; the full-entry pattern into a self-contained ELF that prints
;; `argc=<N>\\n' to stdout, exercising the argc unpack end-to-end.

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

;; ---- §94.c byte sequence constants (x86_64 Linux) ----

(defconst nelisp-crt0--x86_64-full-entry-prologue
  (unibyte-string
   #x48 #x8b #x3c #x24                  ; mov    rdi, [rsp]   (argc)
   #x48 #x8d #x74 #x24 #x08             ; lea    rsi, [rsp+8] (argv)
   #x48 #x89 #xf9                       ; mov    rcx, rdi     (argc copy)
   #x48 #xff #xc1                       ; inc    rcx          (argc+1)
   #x48 #x8d #x14 #xce                  ; lea    rdx,         (envp =
                                        ;                     [rsi+rcx*8])
   #x48 #x89 #xe5                       ; mov    rbp, rsp     (save rsp)
   #x48 #x83 #xe4 #xf0)                 ; and    rsp, -16     (align)
  "Bytes for the §94.c full-entry prologue before `call main'.
Length = 26 bytes.  Establishes the SysV ABI register state
\(rdi=argc, rsi=argv, rdx=envp), saves rsp into rbp (= callee-save),
then forces 16-byte stack alignment for the upcoming call.")

(defconst nelisp-crt0--x86_64-full-entry-epilogue
  (unibyte-string
   #x48 #x89 #xec                       ; mov    rsp, rbp     (restore)
   #x48 #x89 #xc7                       ; mov    rdi, rax     (exit code)
   #xb8 #x3c #x00 #x00 #x00             ; mov    eax, 60      (SYS_exit)
   #x0f #x05)                           ; syscall
  "Bytes for the §94.c full-entry epilogue after `main' returns.
Length = 13 bytes.  Restores rsp from rbp, hands main's rax to the
exit syscall.")

(defconst nelisp-crt0-x86_64-full-entry-prologue-size 26
  "Length in bytes of the §94.c full-entry prologue (= 26).")

(defconst nelisp-crt0-x86_64-full-entry-epilogue-size 13
  "Length in bytes of the §94.c full-entry epilogue (= 13).")

(defconst nelisp-crt0-x86_64-full-entry-size
  (+ nelisp-crt0-x86_64-full-entry-prologue-size
     nelisp-crt0-x86_64-call-size
     nelisp-crt0-x86_64-full-entry-epilogue-size)
  "Total length in bytes of the §94.c full-entry stub (= 44).")

(defun nelisp-crt0-x86_64-full-entry-bytes (&optional call-rel32)
  "Return bytes for the §94.c x86_64 Linux `_start' entry stub.

Unlike `nelisp-crt0-x86_64-entry-bytes' (= §94.a, 23 bytes), the §94.c
variant performs the full SysV AMD64 ABI process-entry handoff:
  rdi ← argc          (= 1st arg per psABI §3.4.1)
  rsi ← argv          (= 2nd arg, NULL-terminated pointer array)
  rdx ← envp          (= 3rd arg, computed as argv + (argc+1)*8)
  rbp ← rsp           (= callee-save, used to restore on return)
  rsp &= -16          (= 16-byte align before `call')

CALL-REL32 is the signed 32-bit displacement for the call to `main';
when omitted or nil, four `00' bytes are emitted as a placeholder
that a linker can patch later via `R_X86_64_PLT32'/`R_X86_64_PC32'.

After main returns, the stub restores rsp from rbp and traps to the
kernel via `mov rdi, rax; mov eax, 60; syscall' (= SYS_exit).

Returns a unibyte string of exactly 44 bytes."
  (concat nelisp-crt0--x86_64-full-entry-prologue
          (unibyte-string #xe8)
          (nelisp-crt0--encode-rel32 (or call-rel32 0))
          nelisp-crt0--x86_64-full-entry-epilogue))

(defun nelisp-crt0-x86_64-find-auxv-bytes ()
  "Return bytes for a §94.c callable `find_auxv(envp)' helper.

The emitted function expects rdi = envp pointer (= base of the
NULL-terminated envp array as placed by the kernel), and returns
rax = auxv pointer (= the byte immediately after envp's NULL
terminator, where the kernel begins the ELF auxiliary vector).

Clobbers rax + rcx (= caller-save per SysV) and falls through to
`ret'.

Returns a unibyte string of exactly 16 bytes."
  (unibyte-string
   #x48 #x89 #xf8                       ; mov  rax, rdi
   ;; L0: scan envp[] until NULL.
   #x48 #x8b #x08                       ; mov  rcx, [rax]
   #x48 #x83 #xc0 #x08                  ; add  rax, 8
   #x48 #x85 #xc9                       ; test rcx, rcx
   #x75 #xf4                            ; jne  L0 (rel8 = -12)
   #xc3))                               ; ret

(defun nelisp-crt0-x86_64-getauxval-bytes ()
  "Return bytes for a §94.c callable `getauxval(auxv, type)' helper.

The emitted function expects rdi = auxv pointer, rsi = type tag (=
one of `AT_*' constants, e.g. AT_RANDOM = 25, AT_PHDR = 3) and
returns rax = the matching value, or 0 when the requested type is
absent.  Iterates Elf64_auxv_t entries (= 16 bytes each: u64 type +
u64 value) until matching type or AT_NULL.

Clobbers rax + rdi (= caller-save) and falls through to `ret'.

Returns a unibyte string of exactly 27 bytes."
  (unibyte-string
   ;; L0:
   #x48 #x8b #x07                       ; mov  rax, [rdi]   (= type)
   #x48 #x85 #xc0                       ; test rax, rax     (AT_NULL?)
   #x74 #x10                            ; je   LNOT (rel8 = 16)
   #x48 #x39 #xf0                       ; cmp  rax, rsi
   #x74 #x06                            ; je   LFOUND (rel8 = 6)
   #x48 #x83 #xc7 #x10                  ; add  rdi, 16
   #xeb #xed                            ; jmp  L0 (rel8 = -19)
   ;; LFOUND:
   #x48 #x8b #x47 #x08                  ; mov  rax, [rdi+8] (= value)
   #xc3                                 ; ret
   ;; LNOT:
   #x31 #xc0                            ; xor  eax, eax
   #xc3))                               ; ret

;; ---- §94.c hello-with-argc orchestrator ----

(defun nelisp-crt0--hello-argc-emit-text (text-vaddr rodata-vaddr)
  "Emit §94.c hello-with-argc .text bytes with a resolved RIP-relative load.
TEXT-VADDR is the runtime virtual address of `_start' (= start of
.text).  RODATA-VADDR is the runtime virtual address of the `argc='
prefix buffer (= start of .rodata).

Layout returned (= 108 bytes total).  The routine reads argc from
[rsp], writes `argc=' via syscall write, formats argc as decimal
ASCII (= div-by-10 loop on a stack scratch buffer), writes the
digits + a trailing `\\n', and exits with status 0."
  (let* (;; rel32 for `lea rsi, [rip+rel]' is relative to the
         ;; instruction *after* the lea.  The lea opcode starts at
         ;; offset 9 and is 7 bytes long, so the next-instruction
         ;; address is text_vaddr + 16.
         (next-after-lea (+ text-vaddr 16))
         (rel32 (- rodata-vaddr next-after-lea)))
    (concat
     ;; --- save argc + write "argc=" ---
     (unibyte-string #x48 #x8b #x1c #x24)              ; mov rbx,[rsp]
     (unibyte-string #xbf #x01 #x00 #x00 #x00)         ; mov edi, 1
     (unibyte-string #x48 #x8d #x35)                   ; lea rsi,[rip+
     (nelisp-crt0--encode-rel32 rel32)                 ;   rel32]
     (unibyte-string #xba #x05 #x00 #x00 #x00)         ; mov edx, 5
     (unibyte-string #xb8 #x01 #x00 #x00 #x00)         ; mov eax, 1
     (unibyte-string #x0f #x05)                        ; syscall
     ;; --- convert argc → ASCII on a 32-byte stack scratch ---
     (unibyte-string #x48 #x83 #xec #x20)              ; sub rsp, 32
     (unibyte-string #x4c #x8d #x44 #x24 #x0f)         ; lea r8,[rsp+15]
     (unibyte-string #x41 #xc6 #x00 #x0a)              ; mov [r8],'\n'
     (unibyte-string #x49 #xff #xc8)                   ; dec r8
     (unibyte-string #x48 #x89 #xd8)                   ; mov rax, rbx
     (unibyte-string #xb9 #x0a #x00 #x00 #x00)         ; mov ecx, 10
     ;; .digit_loop: (offset 52)
     (unibyte-string #x31 #xd2)                        ; xor edx, edx
     (unibyte-string #x48 #xf7 #xf1)                   ; div rcx
     (unibyte-string #x80 #xc2 #x30)                   ; add dl, '0'
     (unibyte-string #x41 #x88 #x10)                   ; mov [r8], dl
     (unibyte-string #x49 #xff #xc8)                   ; dec r8
     (unibyte-string #x48 #x85 #xc0)                   ; test rax, rax
     (unibyte-string #x75 #xed)                        ; jne -19
     ;; --- write digits + newline ---
     (unibyte-string #x49 #x8d #x70 #x01)              ; lea rsi,[r8+1]
     (unibyte-string #x48 #x8d #x54 #x24 #x10)         ; lea rdx,[rsp+16]
     (unibyte-string #x48 #x29 #xf2)                   ; sub rdx, rsi
     (unibyte-string #xbf #x01 #x00 #x00 #x00)         ; mov edi, 1
     (unibyte-string #xb8 #x01 #x00 #x00 #x00)         ; mov eax, 1
     (unibyte-string #x0f #x05)                        ; syscall
     (unibyte-string #x48 #x83 #xc4 #x20)              ; add rsp, 32
     ;; --- exit(0) ---
     (unibyte-string #x31 #xff)                        ; xor edi, edi
     (unibyte-string #xb8 #x3c #x00 #x00 #x00)         ; mov eax, 60
     (unibyte-string #x0f #x05))))                     ; syscall

(defconst nelisp-crt0--hello-argc-text-size 108
  "Length in bytes of the §94.c hello-with-argc `_start' routine (= 108).
Used by the orchestrator to compute the .rodata vaddr before
emitting the RIP-relative displacement.")

(defconst nelisp-crt0--hello-argc-prefix
  (unibyte-string ?a ?r ?g ?c ?=)
  "The `argc=' prefix bytes written by the hello-with-argc binary.
Length = 5 (= `a' `r' `g' `c' `=').  The digits + newline are
formatted on the stack at run time and written by a second `write(2)'
syscall.")

;;;###autoload
(defun nelisp-crt0-emit-hello-with-argc (file-path)
  "Emit a §94.c ELF64 binary to FILE-PATH that prints `argc=<N>\\n'.
The produced binary, when exec'd on x86_64 Linux:
  1. reads argc from the kernel-supplied stack at `[rsp]';
  2. writes `argc=<N>\\n' (= where <N> is argc in decimal ASCII)
     to stdout via two direct `write(2)' syscalls;
  3. exits with status code 0 via a direct `exit(2)' syscall.
No libc, no dynamic loader, no Rust runtime is involved at run time.

This demonstrates the §94.c argv-stack unpacking end-to-end: e.g.
`./bin' prints `argc=1', `./bin a b c' prints `argc=4'.

Returns FILE-PATH.  The file is created with mode #o755 by
`nelisp-elf-write-binary'."
  (let* ((vaddr-base #x400000)
         (text-off  (+ 64 56))           ; = 0x78
         (text-vaddr (+ vaddr-base text-off))
         (prefix-len (length nelisp-crt0--hello-argc-prefix))
         (rodata-off (+ text-off nelisp-crt0--hello-argc-text-size))
         (rodata-vaddr (+ vaddr-base rodata-off))
         (text-bytes (nelisp-crt0--hello-argc-emit-text
                      text-vaddr rodata-vaddr)))
    (unless (= (length text-bytes) nelisp-crt0--hello-argc-text-size)
      (error "nelisp-crt0: hello-argc .text drift (got %d expected %d)"
             (length text-bytes) nelisp-crt0--hello-argc-text-size))
    (nelisp-elf-write-binary
     file-path
     (list :text  text-bytes
           :rodata nelisp-crt0--hello-argc-prefix
           :symbols
           (list (list :name "_start" :value 0
                       :size nelisp-crt0--hello-argc-text-size
                       :section 'text :bind 'global :type 'func)
                 (list :name "argc_prefix" :value 0
                       :size prefix-len :section 'rodata
                       :bind 'local :type 'object))
           :entry-sym "_start"))))

(provide 'nelisp-crt0)

;;; nelisp-crt0.el ends here
