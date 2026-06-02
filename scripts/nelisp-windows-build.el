;;; nelisp-windows-build.el --- native Windows x86_64 build entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 138 Stage 1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/16/17/18/19/20/21/22/23/24/25/26/27/28/29/30/31/32.  Build native Windows PE32+ executables through
;; the pure-elisp PE writer, starting with ExitProcess and VirtualAlloc
;; import-table probes, then wiring Phase47 `(exit ...)' through Win64
;; KERNEL32.dll!ExitProcess, `(write ...)' through WriteFile, and
;; `alloc-bytes' / `dealloc-bytes' through VirtualAlloc / VirtualFree.
;; Stage 7 maps stdio-shaped `syscall-direct' read/write calls to
;; GetStdHandle + ReadFile/WriteFile.
;; Stage 8 maps file-shaped open/read/write/close calls to
;; CreateFileA + ReadFile/WriteFile + CloseHandle.
;; Stage 9 maps mmap/munmap-shaped `syscall-direct' calls to
;; VirtualAlloc/VirtualFree.
;; Stage 10 maps exit/exit_group-shaped `syscall-direct' calls to
;; ExitProcess.
;; Stage 11 adds a native CreateThread/WaitForSingleObject/GetExitCodeThread
;; probe for de-risking Windows thread API mechanics before replacing clone(2).
;; Stage 12 carries Phase47 static imm32 table rodata into PE .rdata so
;; table lookup users no longer trip the Windows table rejection gate.
;; Stage 13 proves multiple linked Phase47 units can feed a PE32+ EXE,
;; which is the bridge from single-probe PE emit toward standalone units.
;; Stage 14 carries linked-unit .rodata through the PE writer so static
;; linker rodata sections can ship in PE .rdata.
;; Stage 15 carries linked-unit .data through the PE writer so read-write
;; linker data sections no longer block PE manifest assembly.
;; Stage 16 adds a standalone-shaped PE start unit that calls `driver' and
;; exits through ExitProcess, replacing the Linux raw-syscall `_start' shape.
;; Stage 17 proves a freestanding PE entry can recover the process command
;; line through GetCommandLineW without relying on CRT argv startup.
;; Stage 18 passes that command-line pointer through the standalone-shaped
;; `_start' -> `driver' bridge.
;; Stage 19 passes both the command-line pointer and its UTF-16 length through
;; that bridge.
;; Stage 20 proves the Phase47 driver can dereference the UTF-16 command-line
;; buffer via `ptr-read-u16'.
;; Stage 21 scans that buffer for a marker argument, de-risking the argv parser
;; loop shape before full reader integration.
;; Stage 22 skips the executable token and checks the first argument marker,
;; adding the minimal Windows argv parser shape needed by reader startup.
;; Stage 23 computes argv1 bounds and requires the first argument token to be
;; exactly the marker.
;; Stage 24 copies argv1 from the UTF-16 command line into a NUL-terminated
;; byte buffer in .data, which is the hand-off shape the reader/file path needs.
;; Stage 25 lets linked driver units call Win32 file APIs through correctly
;; based IAT-relative calls, then reads one byte from argv1 as a file path.
;; Stage 26 makes that startup path dual-mode: no argv1 takes the embedded
;; source branch, argv1 takes the file-load branch.
;; Stage 27 links the standalone arena unit as Win64 PE code and proves
;; `nl_arena_init' routes its mmap-shaped allocation through VirtualAlloc.
;; Stage 28 proves the same arena unit can allocate a real block via
;; `nl_alloc_bytes' and preserve the header/bump invariants on Windows.
;; Stage 29 extends Win64 GP defun/call stack args, then links the standalone
;; GC unit and proves mark/sweep can free and reuse an arena block.
;; Stage 30 links the consbox allocator, cons constructor, and cons accessors,
;; proving boxed Cons construction works over the Windows arena.
;; Stage 31 links the NlConsBox clone kernel and proves its inline
;; `atomic-fetch-add' refcount bump works in a Windows standalone PE.
;; Stage 32 links the `nl_sexp_clone_into' dispatcher plus every clone
;; target it references, proving inline atom and boxed Cons clone paths
;; work in a Windows standalone PE.

;;; Code:

(require 'nelisp-pe-write)
(require 'nelisp-asm-x86_64)
(require 'nelisp-phase47-compiler)
(require 'nelisp-static-linker)
(require 'nelisp-standalone-build)
(require 'nelisp-cc-cons-construct)
(require 'nelisp-cc-sexp-clone-into)
(require 'nelisp-cc-jit-cons-car-ptr)
(require 'nelisp-cc-jit-cons-cdr-ptr)
(require 'nelisp-cc-nlconsbox-alloc)
(require 'nelisp-cc-nlconsbox-clone)
(require 'nelisp-cc-nlstr-direct-ops)
(require 'nelisp-cc-nlstr-clone)
(require 'nelisp-cc-nlvector-clone)
(require 'nelisp-cc-nlcell-clone)
(require 'nelisp-cc-nlchartable-clone)
(require 'nelisp-cc-nlboolvector-clone)
(require 'nelisp-cc-nlrecord-clone)

(defun nelisp-windows-build-exitprocess (out-path exit-code)
  "Write OUT-PATH as a PE32+ EXE calling ExitProcess(EXIT-CODE)."
  (nelisp-pe-write-exitprocess-executable out-path exit-code)
  (message "nelisp-windows-build: wrote %s (ExitProcess %d)"
           out-path exit-code)
  out-path)

(defun nelisp-windows-build-exit42 ()
  "Batch entry: build target/nelisp-windows-exit42.exe."
  (nelisp-windows-build-exitprocess "target/nelisp-windows-exit42.exe" 42))

(defun nelisp-windows-build-virtualalloc-probe (out-path)
  "Write OUT-PATH as a PE32+ EXE probing VirtualAlloc.
The program exits 42 on allocation success and 13 on failure."
  (nelisp-pe-write-virtualalloc-executable out-path 42 13)
  (message "nelisp-windows-build: wrote %s (VirtualAlloc probe)" out-path)
  out-path)

(defun nelisp-windows-build-virtualalloc42 ()
  "Batch entry: build target/nelisp-windows-virtualalloc42.exe."
  (nelisp-windows-build-virtualalloc-probe
   "target/nelisp-windows-virtualalloc42.exe"))

(defun nelisp-windows-build--emit-mov-qword-rsp-imm32 (buf disp imm)
  "Emit `mov qword ptr [rsp+DISP], IMM32' to BUF."
  (unless (and (integerp disp) (<= 0 disp 127))
    (error "nelisp-windows-build: rsp disp8 required: %S" disp))
  (nelisp-asm-x86_64-emit-bytes
   buf (unibyte-string #x48 #xc7 #x44 #x24 disp))
  (nelisp-phase47-compiler--emit-le32-signed buf imm))

(defun nelisp-windows-build--emit-mov-dword-rsp-imm32 (buf disp imm)
  "Emit `mov dword ptr [rsp+DISP], IMM32' to BUF."
  (unless (and (integerp disp) (<= 0 disp 127))
    (error "nelisp-windows-build: rsp disp8 required: %S" disp))
  (nelisp-asm-x86_64-emit-bytes
   buf (unibyte-string #xc7 #x44 #x24 disp))
  (nelisp-phase47-compiler--emit-le32-signed buf imm))

(defun nelisp-windows-build--emit-mov-qword-rsp-rax (buf disp)
  "Emit `mov qword ptr [rsp+DISP], rax' to BUF."
  (unless (and (integerp disp) (<= 0 disp 127))
    (error "nelisp-windows-build: rsp disp8 required: %S" disp))
  (nelisp-asm-x86_64-emit-bytes
   buf (unibyte-string #x48 #x89 #x44 #x24 disp)))

(defun nelisp-windows-build--emit-lea-reg-rsp-disp (buf reg disp)
  "Emit `lea REG, [rsp+DISP]' to BUF for the small probe subset."
  (unless (memq reg '(rax rdx))
    (error "nelisp-windows-build: unsupported lea rsp register: %S" reg))
  (unless (and (integerp disp) (<= 0 disp 127))
    (error "nelisp-windows-build: rsp disp8 required: %S" disp))
  (nelisp-asm-x86_64-emit-bytes
   buf (unibyte-string #x48 #x8d
                       (if (eq reg 'rax) #x44 #x54)
                       #x24 disp)))

(defun nelisp-windows-build--createthread-probe-text
    (text-rva iat-rvas _rdata-rva)
  "Return .text bytes for the Stage 11 CreateThread probe."
  (let ((create-thread (cdr (assoc "CreateThread" iat-rvas)))
        (wait (cdr (assoc "WaitForSingleObject" iat-rvas)))
        (get-exit-code (cdr (assoc "GetExitCodeThread" iat-rvas)))
        (close-handle (cdr (assoc "CloseHandle" iat-rvas)))
        (exit-process (cdr (assoc "ExitProcess" iat-rvas))))
    (unless (and create-thread wait get-exit-code close-handle exit-process)
      (error "nelisp-windows-build: missing CreateThread probe import"))
    (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
          (buf (nelisp-asm-x86_64-make-buffer 'win64)))
      ;; Frame layout after `sub rsp, 0x50':
      ;;   +20 arg5 dwCreationFlags, +28 arg6 lpThreadId,
      ;;   +30 DWORD exit code, +38 DWORD thread id, +40 HANDLE thread.
      (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x50)
      (nelisp-asm-x86_64-xor-reg-reg buf 'rcx 'rcx)
      (nelisp-asm-x86_64-xor-reg-reg buf 'rdx 'rdx)
      (nelisp-asm-x86_64-lea-reg-rip-label buf 'r8 'thread-entry)
      (nelisp-asm-x86_64-xor-reg-reg buf 'r9 'r9)
      (nelisp-windows-build--emit-mov-qword-rsp-imm32 buf #x20 0)
      (nelisp-windows-build--emit-lea-reg-rsp-disp buf 'rax #x38)
      (nelisp-windows-build--emit-mov-qword-rsp-rax buf #x28)
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf create-thread)
      (nelisp-windows-build--emit-mov-qword-rsp-rax buf #x40)
      (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #xba #xff #xff #xff #xff)) ; mov edx, INFINITE
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf wait)
      (nelisp-windows-build--emit-mov-dword-rsp-imm32 buf #x30 0)
      (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx #x40)
      (nelisp-windows-build--emit-lea-reg-rsp-disp buf 'rdx #x30)
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf get-exit-code)
      (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx #x40)
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf close-handle)
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x8b #x4c #x24 #x30)) ; mov ecx, [rsp+0x30]
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf exit-process)
      (nelisp-asm-x86_64-int3 buf)
      (nelisp-asm-x86_64-define-label buf 'thread-entry)
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #xb8 #x2a #x00 #x00 #x00)) ; mov eax, 42
      (nelisp-asm-x86_64-ret buf)
      (nelisp-asm-x86_64-resolve-fixups buf))))

(defun nelisp-windows-build--createthread-probe-bytes ()
  "Return a PE32+ EXE byte string for the Stage 11 CreateThread probe."
  (nelisp-pe-write-build-kernel32-executable
   '("ExitProcess" "CreateThread" "WaitForSingleObject"
     "GetExitCodeThread" "CloseHandle")
   #'nelisp-windows-build--createthread-probe-text))

(defun nelisp-windows-build-createthread-probe (out-path)
  "Write OUT-PATH as a PE32+ EXE probing CreateThread.
The program exits 42 when the thread entry runs and its exit code is
successfully recovered through GetExitCodeThread."
  (let ((bytes (nelisp-windows-build--createthread-probe-bytes))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent))
  (message "nelisp-windows-build: wrote %s (CreateThread probe)" out-path)
  out-path)

(defun nelisp-windows-build-createthread42 ()
  "Batch entry: build target/nelisp-windows-createthread42.exe."
  (nelisp-windows-build-createthread-probe
   "target/nelisp-windows-createthread42.exe"))

(defun nelisp-windows-build--commandline-probe-text
    (text-rva iat-rvas _rdata-rva)
  "Return .text bytes for the Stage 17 GetCommandLineW probe.
The program exits 42 when GetCommandLineW returns a non-empty UTF-16
command line, and 13 otherwise."
  (let ((exit-process (cdr (assoc "ExitProcess" iat-rvas)))
        (get-command-line (cdr (assoc "GetCommandLineW" iat-rvas)))
        (lstrlenw (cdr (assoc "lstrlenW" iat-rvas))))
    (unless (and exit-process get-command-line lstrlenw)
      (error "nelisp-windows-build: missing command-line probe import"))
    (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
          (buf (nelisp-asm-x86_64-make-buffer 'win64)))
      (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x28)
      (nelisp-phase47-compiler--emit-windows-call-iat-rva
       buf get-command-line)
      (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf lstrlenw)
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x85 #xc0)) ; test eax, eax
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x74 #x0c)) ; jz fail
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #xb9 #x2a #x00 #x00 #x00)) ; mov ecx, 42
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf exit-process)
      (nelisp-asm-x86_64-int3 buf)
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #xb9 #x0d #x00 #x00 #x00)) ; mov ecx, 13
      (nelisp-phase47-compiler--emit-windows-call-iat-rva buf exit-process)
      (nelisp-asm-x86_64-int3 buf)
      (nelisp-asm-x86_64-resolve-fixups buf))))

(defun nelisp-windows-build--commandline-probe-bytes ()
  "Return a PE32+ EXE byte string for the Stage 17 command-line probe."
  (nelisp-pe-write-build-kernel32-executable
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   #'nelisp-windows-build--commandline-probe-text))

(defun nelisp-windows-build-commandline-probe (out-path)
  "Write OUT-PATH as a PE32+ EXE probing GetCommandLineW.
The program exits 42 when the process command line is visible through
KERNEL32.dll!GetCommandLineW."
  (let ((bytes (nelisp-windows-build--commandline-probe-bytes))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent))
  (message "nelisp-windows-build: wrote %s (GetCommandLineW probe)" out-path)
  out-path)

(defun nelisp-windows-build-commandline42 ()
  "Batch entry: build target/nelisp-windows-commandline42.exe."
  (nelisp-windows-build-commandline-probe
   "target/nelisp-windows-commandline42.exe"))

(defun nelisp-windows-build--compile-defuns-to-unit (name source
                                                         &optional text-rva
                                                         iat-rvas)
  "Compile Phase47 defun SOURCE to a Win64 link-unit named NAME."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch 'x86_64)
         (nelisp-phase47-compiler--os 'windows)
         (nelisp-phase47-compiler--abi 'win64)
         (nelisp-phase47-compiler--windows-text-rva (or text-rva 0))
         (nelisp-phase47-compiler--windows-exitprocess-iat-rva
          (cdr (assoc "ExitProcess" iat-rvas)))
         (nelisp-phase47-compiler--windows-getstdhandle-iat-rva
          (cdr (assoc "GetStdHandle" iat-rvas)))
         (nelisp-phase47-compiler--windows-writefile-iat-rva
          (cdr (assoc "WriteFile" iat-rvas)))
         (nelisp-phase47-compiler--windows-readfile-iat-rva
          (cdr (assoc "ReadFile" iat-rvas)))
         (nelisp-phase47-compiler--windows-createfilea-iat-rva
          (cdr (assoc "CreateFileA" iat-rvas)))
         (nelisp-phase47-compiler--windows-closehandle-iat-rva
          (cdr (assoc "CloseHandle" iat-rvas)))
         (nelisp-phase47-compiler--windows-virtualalloc-iat-rva
          (cdr (assoc "VirtualAlloc" iat-rvas)))
         (nelisp-phase47-compiler--windows-virtualfree-iat-rva
          (cdr (assoc "VirtualFree" iat-rvas)))
         (nelisp-phase47-compiler--allow-external-user-calls t)
         (ir (nelisp-phase47-compiler--parse source nil))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    (unless defuns
      (signal 'nelisp-phase47-compiler-error
              (list :windows-link-unit-needs-defuns source)))
    (dolist (d defuns)
      (nelisp-phase47-compiler--emit-defun d buf))
    (let* ((text (nelisp-asm-x86_64-resolve-fixups buf))
           (labels (nelisp-asm-x86_64-buffer-labels buf))
           (relocs0 (nelisp-asm-x86_64-extract-relocs buf))
           (exported
            (mapcar (lambda (d)
                      (let ((nm (nelisp-phase47-compiler--ir-get d :name)))
                        (if (stringp nm) nm (symbol-name nm))))
                    defuns))
           (symbols nil)
           (relocs
            (mapcar (lambda (r)
                      (list :offset (plist-get r :offset)
                            :type (plist-get r :type)
                            :symbol (plist-get r :symbol)
                            :addend (or (plist-get r :addend) 0)
                            :section 'text))
                    relocs0)))
      (dolist (cell labels)
        (let ((nm (if (stringp (car cell)) (car cell)
                    (symbol-name (car cell)))))
          (when (member nm exported)
            (push (nelisp-link-symbol
                   nm (cdr cell) :section 'text :bind 'global :type 'func)
                  symbols))))
      (nelisp-link-unit-make name (list (cons 'text text))
                             (nreverse symbols) relocs))))

(defun nelisp-windows-build--linked-start-unit (text-rva exitprocess-iat-rva)
  "Return a `_start' unit that calls add40(2), then ExitProcess(result)."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; Keep `_start' at byte 0; the PE executable entry is .text RVA 0.
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x28)
    (nelisp-asm-x86_64-emit-bytes
     buf (unibyte-string #xb9 #x02 #x00 #x00 #x00)) ; mov ecx, 2
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "add40" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--standalone-start-unit (text-rva exitprocess-iat-rva)
  "Return a PE `_start' unit that calls `driver', then ExitProcess(result)."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; Keep `_start' at byte 0; the PE executable entry is .text RVA 0.
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x28)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "driver" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--standalone-commandline-start-unit
    (text-rva exitprocess-iat-rva getcommandline-iat-rva)
  "Return a PE `_start' unit that calls `driver(GetCommandLineW())'."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; Keep `_start' at byte 0; the PE executable entry is .text RVA 0.
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x28)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf getcommandline-iat-rva)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "driver" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--standalone-commandline-len-start-unit
    (text-rva exitprocess-iat-rva getcommandline-iat-rva lstrlenw-iat-rva)
  "Return a PE `_start' unit that calls `driver(cmdline, lstrlenW(cmdline))'."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; Frame: Win64 shadow space plus one pointer local at [rsp+0x28].
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x38)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf getcommandline-iat-rva)
    (nelisp-windows-build--emit-mov-qword-rsp-rax buf #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva buf lstrlenw-iat-rva)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "driver" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--standalone-commandline-data-start-unit
    (text-rva exitprocess-iat-rva getcommandline-iat-rva lstrlenw-iat-rva)
  "Return a PE `_start' unit calling `driver(cmdline, len, argv_buf)'."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; Frame: Win64 shadow space plus one pointer local at [rsp+0x28].
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x38)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf getcommandline-iat-rva)
    (nelisp-windows-build--emit-mov-qword-rsp-rax buf #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva buf lstrlenw-iat-rva)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'rax)
    ;; lea r8, [rip+argv1_cstr]
    (let ((reloc-off (nelisp-asm-x86_64-buffer-pos buf)))
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x4c #x8d #x05 0 0 0 0))
      (nelisp-asm-x86_64-emit-reloc
       buf 'pc32 "argv1_cstr" 0 :section 'text)
      (let* ((relocs (aref buf 4))
             (last (car (last relocs))))
        (setq last (plist-put last :offset (+ reloc-off 3)))
        (aset buf 4 (append (butlast relocs) (list last)))))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "driver" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--standalone-commandline-file-start-unit
    (text-rva exitprocess-iat-rva getcommandline-iat-rva lstrlenw-iat-rva)
  "Return a PE `_start' unit calling `driver(cmdline, len, path_buf, file_buf)'."
  (let ((nelisp-phase47-compiler--windows-text-rva text-rva)
        (buf (nelisp-asm-x86_64-make-buffer 'win64)))
    (nelisp-asm-x86_64-sub-imm32 buf 'rsp #x38)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf getcommandline-iat-rva)
    (nelisp-windows-build--emit-mov-qword-rsp-rax buf #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva buf lstrlenw-iat-rva)
    (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rcx #x28)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'rax)
    ;; lea r8, [rip+argv1_cstr]
    (let ((reloc-off (nelisp-asm-x86_64-buffer-pos buf)))
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x4c #x8d #x05 0 0 0 0))
      (nelisp-asm-x86_64-emit-reloc
       buf 'pc32 "argv1_cstr" 0 :section 'text)
      (let* ((relocs (aref buf 4))
             (last (car (last relocs))))
        (setq last (plist-put last :offset (+ reloc-off 3)))
        (aset buf 4 (append (butlast relocs) (list last)))))
    ;; lea r9, [rip+file_read_buf]
    (let ((reloc-off (nelisp-asm-x86_64-buffer-pos buf)))
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x4c #x8d #x0d 0 0 0 0))
      (nelisp-asm-x86_64-emit-reloc
       buf 'pc32 "file_read_buf" 0 :section 'text)
      (let* ((relocs (aref buf 4))
             (last (car (last relocs))))
        (setq last (plist-put last :offset (+ reloc-off 3)))
        (aset buf 4 (append (butlast relocs) (list last)))))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xe8))
    (nelisp-asm-x86_64-reloc-plt32-here buf "driver" 0 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'rax)
    (nelisp-phase47-compiler--emit-windows-call-iat-rva
     buf exitprocess-iat-rva)
    (nelisp-asm-x86_64-int3 buf)
    (nelisp-link-unit-make
     "start.o"
     (list (cons 'text (nelisp-asm-x86_64-buffer-bytes buf)))
     (list (nelisp-link-symbol "_start" 0 :section 'text
                               :bind 'global :type 'func))
     (nelisp-asm-x86_64-extract-relocs buf))))

(defun nelisp-windows-build--unit-text-length (unit)
  "Return the length of UNIT's .text payload."
  (length (or (cdr (assq 'text (plist-get unit :sections))) "")))

(defun nelisp-windows-build--link-units-executable-bytes
    (imports unit-builder &optional extra-rdata)
  "Return a PE32+ EXE from linked units produced by UNIT-BUILDER.
IMPORTS is the KERNEL32 import list.  UNIT-BUILDER is called as
`(UNIT-BUILDER TEXT-RVA IAT-RVAS RDATA-RVA)' or
`(UNIT-BUILDER TEXT-RVA IAT-RVAS RDATA-RVA DATA-RVA)' and returns
link-units."
  (nelisp-pe-write-build-kernel32-executable
   imports
   (lambda (text-rva iat-rvas rdata-rva data-rva)
     (let* ((arity (func-arity unit-builder))
            (maxargs (cdr arity))
            (units (if (or (eq maxargs 'many)
                           (eq maxargs t)
                           (and (integerp maxargs) (>= maxargs 4)))
                       (funcall unit-builder text-rva iat-rvas
                                rdata-rva data-rva)
                     (funcall unit-builder text-rva iat-rvas rdata-rva)))
            (layout `((text . ,(+ nelisp-pe--image-base-x86-64 text-rva))
                      (rodata . ,(+ nelisp-pe--image-base-x86-64 rdata-rva))
                      (data . ,(+ nelisp-pe--image-base-x86-64 data-rva))))
            (link-result (nelisp-link-units-2pass units layout))
            (bytes (plist-get link-result :bytes))
            (text (nelisp-link--bytes-or-empty bytes 'text))
            (rodata (nelisp-link--bytes-or-empty bytes 'rodata))
            (data (nelisp-link--bytes-or-empty bytes 'data))
            (bss-size (or (cdr (assq 'bss bytes)) 0)))
       (if (or (> (length rodata) 0)
               (> (length data) 0)
               (> bss-size 0))
           (list :text text
                 :extra-rdata rodata
                 :extra-data data
                 :bss-size bss-size)
         text)))
   extra-rdata))

(defun nelisp-windows-build--linked-call42-bytes ()
  "Return a PE32+ EXE proving multi-unit PE linking."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--linked-start-unit
       text-rva (cdr (assoc "ExitProcess" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "helper.o" '(defun add40 (x) (+ x 40)))))))

(defun nelisp-windows-build--linked-rodata42-bytes ()
  "Return a PE32+ EXE proving linked-unit .rodata is emitted."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--linked-start-unit
       text-rva (cdr (assoc "ExitProcess" iat-rvas)))
      (let* ((rodata (unibyte-string #x2a #x00 #x00 #x00))
             (helper-text
              (let ((buf (nelisp-asm-x86_64-make-buffer 'win64)))
                ;; mov eax, dword ptr [rip+disp32], resolved by static linker.
                (nelisp-asm-x86_64-emit-bytes
                 buf (unibyte-string #x8b #x05 #x00 #x00 #x00 #x00))
                (nelisp-asm-x86_64-ret buf)
                (nelisp-asm-x86_64-buffer-bytes buf))))
        (nelisp-link-unit-make
         "rodata-helper.o"
         (list (cons 'text helper-text)
               (cons 'rodata rodata))
         (list (nelisp-link-symbol
                "add40" 0 :section 'text :bind 'global :type 'func)
               (nelisp-link-symbol
                "answer" 0 :section 'rodata :bind 'global :type 'object))
         (list (list :offset 2 :type 'pc32 :symbol "answer"
                     :addend 0 :section 'text))))))))

(defun nelisp-windows-build--linked-data42-bytes ()
  "Return a PE32+ EXE proving linked-unit .data is emitted."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess")
   (lambda (text-rva iat-rvas _rdata-rva _data-rva)
     (list
      (nelisp-windows-build--linked-start-unit
       text-rva (cdr (assoc "ExitProcess" iat-rvas)))
      (let* ((data (unibyte-string #x28 #x00 #x00 #x00))
             (helper-text
              (let ((buf (nelisp-asm-x86_64-make-buffer 'win64)))
                ;; mov eax, ecx; add eax, dword ptr [rip+disp32]; ret.
                (nelisp-asm-x86_64-emit-bytes
                 buf (unibyte-string #x89 #xc8
                                     #x03 #x05 #x00 #x00 #x00 #x00))
                (nelisp-asm-x86_64-ret buf)
                (nelisp-asm-x86_64-buffer-bytes buf))))
        (nelisp-link-unit-make
         "data-helper.o"
         (list (cons 'text helper-text)
               (cons 'data data))
         (list (nelisp-link-symbol
                "add40" 0 :section 'text :bind 'global :type 'func)
               (nelisp-link-symbol
                "delta" 0 :section 'data :bind 'global :type 'object))
         (list (list :offset 4 :type 'pc32 :symbol "delta"
                     :addend 0 :section 'text))))))))

(defun nelisp-windows-build--standalone-start-driver42-bytes ()
  "Return a PE32+ EXE proving standalone-shaped `_start' calls `driver'."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-start-unit
       text-rva (cdr (assoc "ExitProcess" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o" '(defun driver () 42))))))

(defun nelisp-windows-build--standalone-commandline-driver42-bytes ()
  "Return a PE32+ EXE proving `_start' passes GetCommandLineW to `driver'."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline)
          (if (= cmdline 0) 13 42)))))))

(defun nelisp-windows-build--standalone-commandline-len-driver42-bytes ()
  "Return a PE32+ EXE proving `_start' passes command line pointer and length."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-len-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len)
          (if (= cmdline 0)
              13
            (if (= len 0) 14 42))))))))

(defun nelisp-windows-build--standalone-commandline-read-driver42-bytes ()
  "Return a PE32+ EXE proving `driver' can read the UTF-16 command line."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-len-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len)
          (if (= cmdline 0)
              13
            (if (= len 0)
                14
              (if (= (ptr-read-u16 cmdline 0) 0) 15 42)))))))))

(defun nelisp-windows-build--standalone-commandline-scan-driver42-bytes ()
  "Return a PE32+ EXE proving `driver' can scan the UTF-16 command line."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-len-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len)
          (if (= cmdline 0)
              13
            (if (= len 0)
                14
              (let* ((i 0)
                     (found 0))
                (seq
                 (while (and (< i len) (= found 0))
                   (seq
                    (if (= (ptr-read-u16 cmdline (* i 2)) 33)
                        (setq found 1)
                      0)
                    (setq i (+ i 1))))
                 (if (= found 1) 42 16)))))))))))

(defun nelisp-windows-build--standalone-commandline-argv1-driver42-bytes ()
  "Return a PE32+ EXE proving `driver' can find the first argv marker."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-len-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len)
          (if (= cmdline 0)
              13
            (if (= len 0)
                14
              (let* ((i 0)
                     (ch 0))
                (seq
                 (setq ch (ptr-read-u16 cmdline 0))
                 (if (= ch 34)
                     (seq
                      (setq i 1)
                      (while (and (< i len)
                                  (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                        (setq i (+ i 1)))
                      (if (< i len) (setq i (+ i 1)) 0))
                   (while (and (< i len)
                               (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                    (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                     (setq i (+ i 1))))
                 (while (and (< i len)
                             (or (= (ptr-read-u16 cmdline (* i 2)) 32)
                                 (= (ptr-read-u16 cmdline (* i 2)) 9)))
                   (setq i (+ i 1)))
                 (if (< i len)
                     (if (= (ptr-read-u16 cmdline (* i 2)) 33) 42 16)
                   17)))))))))))

(defun nelisp-windows-build--standalone-commandline-argv1-exact-driver42-bytes ()
  "Return a PE32+ EXE proving `driver' can bound and check argv1 exactly."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva)
     (list
      (nelisp-windows-build--standalone-commandline-len-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len)
          (if (= cmdline 0)
              13
            (if (= len 0)
                14
              (let* ((i 0)
                     (ch 0)
                     (start 0)
                     (end 0))
                (seq
                 (setq ch (ptr-read-u16 cmdline 0))
                 (if (= ch 34)
                     (seq
                      (setq i 1)
                      (while (and (< i len)
                                  (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                        (setq i (+ i 1)))
                      (if (< i len) (setq i (+ i 1)) 0))
                   (while (and (< i len)
                               (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                    (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                     (setq i (+ i 1))))
                 (while (and (< i len)
                             (or (= (ptr-read-u16 cmdline (* i 2)) 32)
                                 (= (ptr-read-u16 cmdline (* i 2)) 9)))
                   (setq i (+ i 1)))
                 (if (< i len)
                     (seq
                      (if (= (ptr-read-u16 cmdline (* i 2)) 34)
                          (seq
                           (setq i (+ i 1))
                           (setq start i)
                           (while (and (< i len)
                                       (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                             (setq i (+ i 1)))
                           (setq end i))
                        (seq
                         (setq start i)
                         (while (and (< i len)
                                     (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                          (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                           (setq i (+ i 1)))
                         (setq end i)))
                      (if (and (= (- end start) 1)
                               (= (ptr-read-u16 cmdline (* start 2)) 33))
                          42
                        16))
                   17)))))))))))

(defun nelisp-windows-build--argv1-cstr-data-unit ()
  "Return a .data unit containing the Stage 24 argv1 byte buffer."
  (nelisp-link-unit-make
   "argv1-cstr.o"
   (list (cons 'data (make-string 260 0)))
   (list (nelisp-link-symbol
          "argv1_cstr" 0 :section 'data :bind 'global :type 'object))
   nil))

(defun nelisp-windows-build--argv1-file-data-unit ()
  "Return a .data unit containing argv1 path and file read buffers."
  (nelisp-link-unit-make
   "argv1-file-bufs.o"
   (list (cons 'data (make-string 276 0)))
   (list (nelisp-link-symbol
          "argv1_cstr" 0 :section 'data :bind 'global :type 'object)
         (nelisp-link-symbol
          "file_read_buf" 260 :section 'data :bind 'global :type 'object))
   nil))

(defun nelisp-windows-build--standalone-commandline-argv1-cstr-driver-bytes ()
  "Return a PE32+ EXE proving argv1 can be copied to a C string buffer."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW")
   (lambda (text-rva iat-rvas _rdata-rva _data-rva)
     (list
      (nelisp-windows-build--standalone-commandline-data-start-unit
       text-rva
       (cdr (assoc "ExitProcess" iat-rvas))
       (cdr (assoc "GetCommandLineW" iat-rvas))
       (cdr (assoc "lstrlenW" iat-rvas)))
      (nelisp-windows-build--compile-defuns-to-unit
       "driver.o"
       '(defun driver (cmdline len out)
          (if (= cmdline 0)
              13
            (if (= len 0)
                14
              (if (= out 0)
                  15
                (let* ((i 0)
                       (ch 0)
                       (start 0)
                       (end 0)
                       (j 0))
                  (seq
                   (setq ch (ptr-read-u16 cmdline 0))
                   (if (= ch 34)
                       (seq
                        (setq i 1)
                        (while (and (< i len)
                                    (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                          (setq i (+ i 1)))
                        (if (< i len) (setq i (+ i 1)) 0))
                     (while (and (< i len)
                                 (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                      (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                       (setq i (+ i 1))))
                   (while (and (< i len)
                               (or (= (ptr-read-u16 cmdline (* i 2)) 32)
                                   (= (ptr-read-u16 cmdline (* i 2)) 9)))
                     (setq i (+ i 1)))
                   (if (< i len)
                       (seq
                        (if (= (ptr-read-u16 cmdline (* i 2)) 34)
                            (seq
                             (setq i (+ i 1))
                             (setq start i)
                             (while (and (< i len)
                                         (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                               (setq i (+ i 1)))
                             (setq end i))
                          (seq
                           (setq start i)
                           (while (and (< i len)
                                       (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                            (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                             (setq i (+ i 1)))
                           (setq end i)))
                        (while (< start end)
                          (seq
                           (ptr-write-u8 out j (ptr-read-u16 cmdline (* start 2)))
                           (setq start (+ start 1))
                           (setq j (+ j 1))))
                        (ptr-write-u8 out j 0)
                        (ptr-read-u8 out 0))
                     17))))))))
      (nelisp-windows-build--argv1-cstr-data-unit)))))

(defun nelisp-windows-build--standalone-commandline-file-read-byte-bytes ()
  "Return a PE32+ EXE that reads one byte from the argv1 file path."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW"
     "CreateFileA" "ReadFile" "CloseHandle")
   (lambda (text-rva iat-rvas _rdata-rva _data-rva)
     (let* ((start (nelisp-windows-build--standalone-commandline-file-start-unit
                    text-rva
                    (cdr (assoc "ExitProcess" iat-rvas))
                    (cdr (assoc "GetCommandLineW" iat-rvas))
                    (cdr (assoc "lstrlenW" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start))))
       (list
        start
        (nelisp-windows-build--compile-defuns-to-unit
         "driver.o"
         '(seq
           (defun wargv_copy_argv1 (cmdline len out)
             (let* ((i 0)
                    (ch 0)
                    (start 0)
                    (end 0)
                    (j 0))
               (seq
                (setq ch (ptr-read-u16 cmdline 0))
                (if (= ch 34)
                    (seq
                     (setq i 1)
                     (while (and (< i len)
                                 (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                       (setq i (+ i 1)))
                     (if (< i len) (setq i (+ i 1)) 0))
                  (while (and (< i len)
                              (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                   (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                    (setq i (+ i 1))))
                (while (and (< i len)
                            (or (= (ptr-read-u16 cmdline (* i 2)) 32)
                                (= (ptr-read-u16 cmdline (* i 2)) 9)))
                  (setq i (+ i 1)))
                (if (< i len)
                    (seq
                     (if (= (ptr-read-u16 cmdline (* i 2)) 34)
                         (seq
                          (setq i (+ i 1))
                          (setq start i)
                          (while (and (< i len)
                                      (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                            (setq i (+ i 1)))
                          (setq end i))
                       (seq
                        (setq start i)
                        (while (and (< i len)
                                    (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                         (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                          (setq i (+ i 1)))
                        (setq end i)))
                     (while (< start end)
                       (seq
                        (ptr-write-u8 out j (ptr-read-u16 cmdline (* start 2)))
                        (setq start (+ start 1))
                        (setq j (+ j 1))))
                     (ptr-write-u8 out j 0)
                     1)
                  17))))
           (defun driver (cmdline len path-buf file-buf)
             (if (= cmdline 0)
                 13
               (if (= len 0)
                   14
                 (if (= path-buf 0)
                     15
                   (if (= file-buf 0)
                       16
                     (let* ((copied (wargv_copy_argv1 cmdline len path-buf)))
                       (if (= copied 1)
                           (let* ((fd (syscall-direct 2 path-buf 0 0 0 0 0)))
                             (if (< fd 0)
                                 18
                               (let* ((n (syscall-direct 0 fd file-buf 1 0 0 0))
                                      (b (ptr-read-u8 file-buf 0)))
                                 (seq
                                  (syscall-direct 3 fd 0 0 0 0 0)
                                  (if (= n 1) b 19)))))
                         copied))))))))
         driver-rva iat-rvas)
        (nelisp-windows-build--argv1-file-data-unit))))))

(defun nelisp-windows-build--standalone-commandline-dual-source-byte-bytes ()
  "Return a PE32+ EXE proving embedded-vs-file source selection."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "GetCommandLineW" "lstrlenW"
     "CreateFileA" "ReadFile" "CloseHandle")
   (lambda (text-rva iat-rvas _rdata-rva _data-rva)
     (let* ((start (nelisp-windows-build--standalone-commandline-file-start-unit
                    text-rva
                    (cdr (assoc "ExitProcess" iat-rvas))
                    (cdr (assoc "GetCommandLineW" iat-rvas))
                    (cdr (assoc "lstrlenW" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start))))
       (list
        start
        (nelisp-windows-build--compile-defuns-to-unit
         "driver.o"
         '(seq
           (defun wargv_copy_argv1 (cmdline len out)
             (let* ((i 0)
                    (ch 0)
                    (start 0)
                    (end 0)
                    (j 0))
               (seq
                (setq ch (ptr-read-u16 cmdline 0))
                (if (= ch 34)
                    (seq
                     (setq i 1)
                     (while (and (< i len)
                                 (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                       (setq i (+ i 1)))
                     (if (< i len) (setq i (+ i 1)) 0))
                  (while (and (< i len)
                              (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                   (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                    (setq i (+ i 1))))
                (while (and (< i len)
                            (or (= (ptr-read-u16 cmdline (* i 2)) 32)
                                (= (ptr-read-u16 cmdline (* i 2)) 9)))
                  (setq i (+ i 1)))
                (if (< i len)
                    (seq
                     (if (= (ptr-read-u16 cmdline (* i 2)) 34)
                         (seq
                          (setq i (+ i 1))
                          (setq start i)
                          (while (and (< i len)
                                      (= (= (ptr-read-u16 cmdline (* i 2)) 34) 0))
                            (setq i (+ i 1)))
                          (setq end i))
                       (seq
                        (setq start i)
                        (while (and (< i len)
                                    (and (= (= (ptr-read-u16 cmdline (* i 2)) 32) 0)
                                         (= (= (ptr-read-u16 cmdline (* i 2)) 9) 0)))
                          (setq i (+ i 1)))
                        (setq end i)))
                     (while (< start end)
                       (seq
                        (ptr-write-u8 out j (ptr-read-u16 cmdline (* start 2)))
                        (setq start (+ start 1))
                        (setq j (+ j 1))))
                     (ptr-write-u8 out j 0)
                     1)
                  17))))
           (defun driver (cmdline len path-buf file-buf)
             (if (= cmdline 0)
                 13
               (if (= len 0)
                   14
                 (if (= path-buf 0)
                     15
                   (if (= file-buf 0)
                       16
                     (let* ((copied (wargv_copy_argv1 cmdline len path-buf)))
                       (if (= copied 17)
                           42
                         (if (= copied 1)
                             (let* ((fd (syscall-direct 2 path-buf 0 0 0 0 0)))
                               (if (< fd 0)
                                   18
                                 (let* ((n (syscall-direct 0 fd file-buf 1 0 0 0))
                                        (b (ptr-read-u8 file-buf 0)))
                                   (seq
                                    (syscall-direct 3 fd 0 0 0 0 0)
                                    (if (= n 1) b 19)))))
                           copied)))))))))
         driver-rva iat-rvas)
        (nelisp-windows-build--argv1-file-data-unit))))))

(defun nelisp-windows-build--standalone-arena-driver42-bytes ()
  "Return a PE32+ EXE proving the standalone arena unit links on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              13
                            (if (= (ptr-read-u64 268435456 0) 256)
                                (if (= (ptr-read-u64 268435568 0) 268435712)
                                    42
                                  15)
                              14))))
                     driver-rva iat-rvas))
            (arena-rva (+ driver-rva
                          (nelisp-windows-build--unit-text-length driver)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas)))
       (list start driver arena)))))

(defun nelisp-windows-build--standalone-arena-alloc-driver42-bytes ()
  "Return a PE32+ EXE proving standalone arena allocation works on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              13
                            (let* ((p (nl_alloc_bytes 16 8)))
                              (if (= p 0)
                                  14
                                (if (= p 268435728)
                                    (if (= (ptr-read-u64 268435456 0) 288)
                                        (if (= (ptr-read-u64 (- p 16) 0) 32)
                                            (seq
                                             (ptr-write-u8 p 0 90)
                                             (if (= (ptr-read-u8 p 0) 90) 42 17))
                                          16)
                                      16)
                                  15))))))
                     driver-rva iat-rvas))
            (arena-rva (+ driver-rva
                          (nelisp-windows-build--unit-text-length driver)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas)))
       (list start driver arena)))))

(defun nelisp-windows-build--standalone-gc-reuse-driver42-bytes ()
  "Return a PE32+ EXE proving standalone GC reuse works on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              18
                            (let* ((ctx (nl_alloc_bytes 96 8))
                                   (result (nl_alloc_bytes 32 8))
                                   (out (nl_alloc_bytes 32 8))
                                   (pool (nl_alloc_bytes 32 8))
                                   (src (nl_alloc_bytes 32 8))
                                   (cursor (nl_alloc_bytes 32 8))
                                   (bsym (nl_alloc_bytes 32 8)))
                              (if (= bsym 0)
                                  19
                                (let* ((garbage (nl_alloc_bytes 16 8)))
                                  (if (= garbage 0)
                                      20
                                    (seq
                                     (nl_gc_mark_block ctx)
                                     (nl_gc_mark_block result)
                                     (nl_gc_mark_block out)
                                     (nl_gc_mark_block pool)
                                     (nl_gc_mark_block src)
                                     (nl_gc_mark_block cursor)
                                     (nl_gc_mark_block bsym)
                                     (nl_gc_sweep)
                                     (if (= (ptr-read-u64 268435552 0) garbage)
                                         (if (= (ptr-read-u64 (- garbage 8) 0) 2)
                                             (let* ((again (nl_alloc_bytes 16 8)))
                                               (if (= again garbage)
                                                   (if (= (ptr-read-u64 268435552 0) 0)
                                                       (if (= (ptr-read-u64 (- again 8) 0) 0)
                                                           (if (= (ptr-read-u64 268435456 0) 688)
                                                               42
                                                             24)
                                                         23)
                                                     22)
                                                 21))
                                           23)
                                       21)))))))))
                     driver-rva iat-rvas))
            (arena-rva (+ driver-rva
                          (nelisp-windows-build--unit-text-length driver)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas))
            (gc-rva (+ arena-rva
                       (nelisp-windows-build--unit-text-length arena)))
            (gc (nelisp-windows-build--compile-defuns-to-unit
                 "gc.o" nelisp-standalone--gc-source
                 gc-rva iat-rvas)))
       (list start driver arena gc)))))

(defun nelisp-windows-build--standalone-cons-driver42-bytes ()
  "Return a PE32+ EXE proving standalone cons construction works on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              25
                            (let* ((car-slot (nl_alloc_bytes 32 8))
                                   (cdr-slot (nl_alloc_bytes 32 8))
                                   (out-slot (nl_alloc_bytes 32 8)))
                              (if (= out-slot 0)
                                  26
                                (seq
                                 (ptr-write-u64 car-slot 0 2)
                                 (ptr-write-u64 (+ car-slot 8) 0 11)
                                 (ptr-write-u64 cdr-slot 0 2)
                                 (ptr-write-u64 (+ cdr-slot 8) 0 31)
                                 (nelisp_cons_construct car-slot cdr-slot out-slot)
                                 (if (= (ptr-read-u64 out-slot 0) 7)
                                     (let* ((box (ptr-read-u64 out-slot 8))
                                            (car-ptr (nl_cons_car_ptr out-slot))
                                            (cdr-ptr (nl_cons_cdr_ptr out-slot)))
                                       (if (= car-ptr box)
                                           (if (= cdr-ptr (+ box 32))
                                               (if (= (ptr-read-u64 (+ box 64) 0) 1)
                                                   (if (= (ptr-read-u64 car-ptr 0) 2)
                                                       (if (= (ptr-read-u64 (+ car-ptr 8) 0) 11)
                                                           (if (= (ptr-read-u64 cdr-ptr 0) 2)
                                                               (if (= (ptr-read-u64 (+ cdr-ptr 8) 0) 31)
                                                                   42
                                                                 33)
                                                             32)
                                                         31)
                                                     30)
                                                 29)
                                             28)
                                         27))
                                   26)))))))
                     driver-rva iat-rvas))
            (cons-rva (+ driver-rva
                         (nelisp-windows-build--unit-text-length driver)))
            (cons (nelisp-windows-build--compile-defuns-to-unit
                   "cons-ctor.o" nelisp-cc-cons-construct--source
                   cons-rva iat-rvas))
            (consbox-rva (+ cons-rva
                            (nelisp-windows-build--unit-text-length cons)))
            (consbox (nelisp-windows-build--compile-defuns-to-unit
                      "consbox.o" nelisp-cc-nlconsbox-alloc--source
                      consbox-rva iat-rvas))
            (car-rva (+ consbox-rva
                        (nelisp-windows-build--unit-text-length consbox)))
            (car-ptr (nelisp-windows-build--compile-defuns-to-unit
                      "car-ptr.o" nelisp-cc-jit-cons-car-ptr--source
                      car-rva iat-rvas))
            (cdr-rva (+ car-rva
                        (nelisp-windows-build--unit-text-length car-ptr)))
            (cdr-ptr (nelisp-windows-build--compile-defuns-to-unit
                      "cdr-ptr.o" nelisp-cc-jit-cons-cdr-ptr--source
                      cdr-rva iat-rvas))
            (arena-rva (+ cdr-rva
                          (nelisp-windows-build--unit-text-length cdr-ptr)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas)))
       (list start driver cons consbox car-ptr cdr-ptr arena)))))

(defun nelisp-windows-build--standalone-consbox-clone-driver42-bytes ()
  "Return a PE32+ EXE proving NlConsBox clone works on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              34
                            (let* ((box (nl_alloc_consbox)))
                              (if (= box 0)
                                  35
                                (let* ((ret (nelisp_nlconsbox_clone box)))
                                  (if (= ret box)
                                      (if (= (ptr-read-u64 (+ box 64) 0) 2)
                                          (if (= (ptr-read-u64 box 0) 0)
                                              (if (= (ptr-read-u64 (+ box 32) 0) 0)
                                                  42
                                                38)
                                            37)
                                        36)
                                    35)))))))
                     driver-rva iat-rvas))
            (clone-rva (+ driver-rva
                          (nelisp-windows-build--unit-text-length driver)))
            (clone (nelisp-windows-build--compile-defuns-to-unit
                    "consbox-clone.o" nelisp-cc-nlconsbox-clone--source
                    clone-rva iat-rvas))
            (consbox-rva (+ clone-rva
                            (nelisp-windows-build--unit-text-length clone)))
            (consbox (nelisp-windows-build--compile-defuns-to-unit
                      "consbox.o" nelisp-cc-nlconsbox-alloc--source
                      consbox-rva iat-rvas))
            (arena-rva (+ consbox-rva
                          (nelisp-windows-build--unit-text-length consbox)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas)))
       (list start driver clone consbox arena)))))

(defun nelisp-windows-build--standalone-sexp-clone-driver42-bytes ()
  "Return a PE32+ EXE proving `nl_sexp_clone_into' works on Windows."
  (nelisp-windows-build--link-units-executable-bytes
   '("ExitProcess" "VirtualAlloc")
   (lambda (text-rva iat-rvas _rdata-rva)
     (let* ((start (nelisp-windows-build--standalone-start-unit
                    text-rva (cdr (assoc "ExitProcess" iat-rvas))))
            (driver-rva (+ text-rva
                           (nelisp-windows-build--unit-text-length start)))
            (driver (nelisp-windows-build--compile-defuns-to-unit
                     "driver.o"
                     '(defun driver ()
                        (let* ((arena (nl_arena_init)))
                          (if (= arena 0)
                              39
                            (let* ((src (nl_alloc_bytes 32 8))
                                   (dst (nl_alloc_bytes 32 8))
                                   (cons-src (nl_alloc_bytes 32 8))
                                   (cons-dst (nl_alloc_bytes 32 8))
                                   (box (nl_alloc_consbox)))
                              (if (= box 0)
                                  40
                                (seq
                                 (ptr-write-u64 src 0 2)
                                 (ptr-write-u64 (+ src 8) 0 123)
                                 (ptr-write-u64 (+ src 16) 0 456)
                                 (ptr-write-u64 (+ src 24) 0 789)
                                 (nl_sexp_clone_into src dst)
                                 (if (= (ptr-read-u64 dst 0) 2)
                                     (if (= (ptr-read-u64 (+ dst 8) 0) 123)
                                         (if (= (ptr-read-u64 (+ dst 16) 0) 456)
                                             (if (= (ptr-read-u64 (+ dst 24) 0) 789)
                                                 (seq
                                                  (ptr-write-u64 cons-src 0 7)
                                                  (ptr-write-u64 (+ cons-src 8) 0 box)
                                                  (ptr-write-u64 (+ cons-src 16) 0 0)
                                                  (ptr-write-u64 (+ cons-src 24) 0 0)
                                                  (nl_sexp_clone_into cons-src cons-dst)
                                                  (if (= (ptr-read-u64 cons-dst 0) 7)
                                                      (if (= (ptr-read-u64 (+ cons-dst 8) 0) box)
                                                          (if (= (ptr-read-u64 (+ box 64) 0) 2)
                                                              42
                                                            47)
                                                       46)
                                                    45))
                                               44)
                                           43)
                                       48)
                                   41)))))))
                     driver-rva iat-rvas))
            (clone-rva (+ driver-rva
                          (nelisp-windows-build--unit-text-length driver)))
            (clone (nelisp-windows-build--compile-defuns-to-unit
                    "clone.o" nelisp-cc-sexp-clone-into--source
                    clone-rva iat-rvas))
            (consbox-clone-rva
             (+ clone-rva (nelisp-windows-build--unit-text-length clone)))
            (consbox-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "consbox-clone.o" nelisp-cc-nlconsbox-clone--source
              consbox-clone-rva iat-rvas))
            (str-clone-rva
             (+ consbox-clone-rva
                (nelisp-windows-build--unit-text-length consbox-clone)))
            (str-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "str-clone.o" nelisp-cc-nlstr-clone--source
              str-clone-rva iat-rvas))
            (vector-clone-rva
             (+ str-clone-rva
                (nelisp-windows-build--unit-text-length str-clone)))
            (vector-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "vec-clone.o" nelisp-cc-nlvector-clone--source
              vector-clone-rva iat-rvas))
            (chartable-clone-rva
             (+ vector-clone-rva
                (nelisp-windows-build--unit-text-length vector-clone)))
            (chartable-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "chartable-clone.o" nelisp-cc-nlchartable-clone--source
              chartable-clone-rva iat-rvas))
            (boolvector-clone-rva
             (+ chartable-clone-rva
                (nelisp-windows-build--unit-text-length chartable-clone)))
            (boolvector-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "boolvec-clone.o" nelisp-cc-nlboolvector-clone--source
              boolvector-clone-rva iat-rvas))
            (cell-clone-rva
             (+ boolvector-clone-rva
                (nelisp-windows-build--unit-text-length boolvector-clone)))
            (cell-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "cell-clone.o" nelisp-cc-nlcell-clone--source
              cell-clone-rva iat-rvas))
            (record-clone-rva
             (+ cell-clone-rva
                (nelisp-windows-build--unit-text-length cell-clone)))
            (record-clone
             (nelisp-windows-build--compile-defuns-to-unit
              "record-clone.o" nelisp-cc-nlrecord-clone--source
              record-clone-rva iat-rvas))
            (alloc-str-rva
             (+ record-clone-rva
                (nelisp-windows-build--unit-text-length record-clone)))
            (alloc-str
             (nelisp-windows-build--compile-defuns-to-unit
              "alloc-str.o" nelisp-cc-nlstr-direct-ops--alloc-str-source
              alloc-str-rva iat-rvas))
            (consbox-rva
             (+ alloc-str-rva
                (nelisp-windows-build--unit-text-length alloc-str)))
            (consbox
             (nelisp-windows-build--compile-defuns-to-unit
              "consbox.o" nelisp-cc-nlconsbox-alloc--source
              consbox-rva iat-rvas))
            (arena-rva
             (+ consbox-rva
                (nelisp-windows-build--unit-text-length consbox)))
            (arena (nelisp-windows-build--compile-defuns-to-unit
                    "arena.o" nelisp-standalone--arena-source
                    arena-rva iat-rvas)))
       (list start driver clone consbox-clone str-clone vector-clone
             chartable-clone boolvector-clone cell-clone record-clone
             alloc-str consbox arena)))))

(defun nelisp-windows-build-linked-call42 ()
  "Batch entry: build target/nelisp-windows-linked-call42.exe."
  (let ((bytes (nelisp-windows-build--linked-call42-bytes))
        (out-path "target/nelisp-windows-linked-call42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (linked add40 -> ExitProcess)"
             out-path)
    out-path))

(defun nelisp-windows-build-linked-rodata42 ()
  "Batch entry: build target/nelisp-windows-linked-rodata42.exe."
  (let ((bytes (nelisp-windows-build--linked-rodata42-bytes))
        (out-path "target/nelisp-windows-linked-rodata42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (linked rodata -> ExitProcess)"
             out-path)
    out-path))

(defun nelisp-windows-build-linked-data42 ()
  "Batch entry: build target/nelisp-windows-linked-data42.exe."
  (let ((bytes (nelisp-windows-build--linked-data42-bytes))
        (out-path "target/nelisp-windows-linked-data42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (linked data -> ExitProcess)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-start-driver42 ()
  "Batch entry: build target/nelisp-windows-standalone-start-driver42.exe."
  (let ((bytes (nelisp-windows-build--standalone-start-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-start-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone start -> driver)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-driver42 ()
  "Batch entry: build target/nelisp-windows-standalone-commandline-driver42.exe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (GetCommandLineW -> driver)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-len-driver42 ()
  "Batch entry: build the standalone command-line length probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-len-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-len-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (GetCommandLineW/lstrlenW -> driver)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-read-driver42 ()
  "Batch entry: build the standalone command-line read probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-read-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-read-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (read UTF-16 command line)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-scan-driver42 ()
  "Batch entry: build the standalone command-line scan probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-scan-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-scan-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (scan UTF-16 command line)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-argv1-driver42 ()
  "Batch entry: build the standalone command-line argv1 probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-argv1-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-argv1-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (argv1 UTF-16 command line)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-argv1-exact-driver42 ()
  "Batch entry: build the standalone command-line exact argv1 probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-argv1-exact-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-argv1-exact-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (exact argv1 UTF-16 command line)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-argv1-cstr ()
  "Batch entry: build the standalone command-line argv1 C-string probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-argv1-cstr-driver-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-argv1-cstr.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (argv1 UTF-16 -> C string)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-file-read-byte ()
  "Batch entry: build the standalone command-line file-read probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-file-read-byte-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-file-read-byte.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (argv1 path -> first byte)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-commandline-dual-source-byte ()
  "Batch entry: build the standalone command-line dual source probe."
  (let ((bytes (nelisp-windows-build--standalone-commandline-dual-source-byte-bytes))
        (out-path "target/nelisp-windows-standalone-commandline-dual-source-byte.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (embedded-or-file source byte)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-arena-driver42 ()
  "Batch entry: build the standalone arena initialization probe."
  (let ((bytes (nelisp-windows-build--standalone-arena-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-arena-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone arena -> VirtualAlloc)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-arena-alloc-driver42 ()
  "Batch entry: build the standalone arena allocation probe."
  (let ((bytes (nelisp-windows-build--standalone-arena-alloc-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-arena-alloc-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone arena allocation)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-gc-reuse-driver42 ()
  "Batch entry: build the standalone GC free-list reuse probe."
  (let ((bytes (nelisp-windows-build--standalone-gc-reuse-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-gc-reuse-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone GC reuse)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-cons-driver42 ()
  "Batch entry: build the standalone cons construction probe."
  (let ((bytes (nelisp-windows-build--standalone-cons-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-cons-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone Cons construct)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-consbox-clone-driver42 ()
  "Batch entry: build the standalone NlConsBox clone probe."
  (let ((bytes (nelisp-windows-build--standalone-consbox-clone-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-consbox-clone-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone NlConsBox clone)"
             out-path)
    out-path))

(defun nelisp-windows-build-standalone-sexp-clone-driver42 ()
  "Batch entry: build the standalone Sexp clone probe."
  (let ((bytes (nelisp-windows-build--standalone-sexp-clone-driver42-bytes))
        (out-path "target/nelisp-windows-standalone-sexp-clone-driver42.exe")
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent)
    (message "nelisp-windows-build: wrote %s (standalone Sexp clone)"
             out-path)
    out-path))

(defun nelisp-windows-build--sexp-contains-symbol-p (sexp symbol)
  "Return non-nil when SEXP contains SYMBOL as a list head."
  (cond
   ((atom sexp) nil)
   ((eq (car sexp) symbol) t)
   (t (let ((tail sexp)
            (found nil))
        (while (and tail (not found))
          (setq found (nelisp-windows-build--sexp-contains-symbol-p
                       (car tail) symbol))
          (setq tail (cdr tail)))
        found))))

(defun nelisp-windows-build--sexp-contains-syscall-direct-p (sexp nr fds)
  "Return non-nil when SEXP contains `(syscall-direct NR FD ...)'.
FDS is the list of accepted immediate fd values."
  (cond
   ((atom sexp) nil)
   ((and (eq (car sexp) 'syscall-direct)
         (= (length sexp) 8)
         (equal (nth 1 sexp) nr)
         (member (nth 2 sexp) fds)
         (equal (nth 5 sexp) 0)
         (equal (nth 6 sexp) 0)
         (equal (nth 7 sexp) 0))
    t)
   (t (let ((tail sexp)
            (found nil))
        (while (and tail (not found))
          (setq found
                (nelisp-windows-build--sexp-contains-syscall-direct-p
                 (car tail) nr fds))
          (setq tail (cdr tail)))
        found))))

(defun nelisp-windows-build--sexp-contains-syscall-nr-p (sexp nr)
  "Return non-nil when SEXP contains `(syscall-direct NR ...)'."
  (cond
   ((atom sexp) nil)
   ((and (eq (car sexp) 'syscall-direct)
         (= (length sexp) 8)
         (equal (nth 1 sexp) nr))
    t)
   (t (let ((tail sexp)
            (found nil))
        (while (and tail (not found))
          (setq found
                (nelisp-windows-build--sexp-contains-syscall-nr-p
                 (car tail) nr))
          (setq tail (cdr tail)))
        found))))

(defun nelisp-windows-build--append-import-names (names additions)
  "Return NAMES with ADDITIONS appended once, preserving order."
  (let ((out names))
    (dolist (name additions)
      (unless (member name out)
        (setq out (append out (list name)))))
    out))

(defun nelisp-windows-build--phase47-import-names (sexp str-rodata-bytes)
  "Return KERNEL32 import names needed by SEXP and STR-RODATA-BYTES."
  (let ((names '("ExitProcess")))
    (when (> (length str-rodata-bytes) 0)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "WriteFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-direct-p sexp 0 '(0))
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "ReadFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 0)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("ReadFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-direct-p sexp 1 '(1 2))
      (setq names
            (nelisp-windows-build--append-import-names
             names '("GetStdHandle" "WriteFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 1)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("WriteFile"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 2)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("CreateFileA"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 3)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("CloseHandle"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 9)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualAlloc"))))
    (when (nelisp-windows-build--sexp-contains-syscall-nr-p sexp 11)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualFree"))))
    (when (nelisp-windows-build--sexp-contains-symbol-p sexp 'alloc-bytes)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualAlloc"))))
    (when (nelisp-windows-build--sexp-contains-symbol-p sexp 'dealloc-bytes)
      (setq names
            (nelisp-windows-build--append-import-names
             names '("VirtualFree"))))
    names))

(defun nelisp-windows-build--phase47-rodata-bytes (sexp)
  "Return Phase47 string rodata bytes for SEXP."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir)))
    (cdr collected)))

(defun nelisp-windows-build--phase47-extra-rdata-bytes (sexp)
  "Return Phase47 string rodata followed by static table rodata for SEXP."
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (strings (nelisp-phase47-compiler--collect-strings ir))
         (tables (nelisp-phase47-compiler--collect-tables ir)))
    (concat (cdr strings) (cdr tables))))

(defun nelisp-windows-build--phase47-text (sexp text-rva iat-rvas rodata-rva)
  "Return Win64 .text bytes for Phase47 SEXP.
TEXT-RVA is the PE .text RVA, IAT-RVAS maps imported functions to IAT
slot RVAs, and RODATA-RVA is byte 0 of the appended string rodata."
  (let ((exitprocess-iat (cdr (assoc "ExitProcess" iat-rvas)))
        (getstdhandle-iat (cdr (assoc "GetStdHandle" iat-rvas)))
        (writefile-iat (cdr (assoc "WriteFile" iat-rvas)))
        (readfile-iat (cdr (assoc "ReadFile" iat-rvas)))
        (createfilea-iat (cdr (assoc "CreateFileA" iat-rvas)))
        (closehandle-iat (cdr (assoc "CloseHandle" iat-rvas))))
    (unless exitprocess-iat
      (error "nelisp-windows-build: missing ExitProcess IAT RVA"))
    (let* ((nelisp-phase47-compiler--label-counter 0)
           (nelisp-phase47-compiler--arch 'x86_64)
           (nelisp-phase47-compiler--os 'windows)
           (nelisp-phase47-compiler--abi 'win64)
           (nelisp-phase47-compiler--windows-text-rva text-rva)
           (nelisp-phase47-compiler--windows-exitprocess-iat-rva exitprocess-iat)
           (nelisp-phase47-compiler--windows-getstdhandle-iat-rva getstdhandle-iat)
           (nelisp-phase47-compiler--windows-writefile-iat-rva writefile-iat)
           (nelisp-phase47-compiler--windows-readfile-iat-rva readfile-iat)
           (nelisp-phase47-compiler--windows-createfilea-iat-rva createfilea-iat)
           (nelisp-phase47-compiler--windows-closehandle-iat-rva closehandle-iat)
           (nelisp-phase47-compiler--windows-virtualalloc-iat-rva
            (cdr (assoc "VirtualAlloc" iat-rvas)))
           (nelisp-phase47-compiler--windows-virtualfree-iat-rva
            (cdr (assoc "VirtualFree" iat-rvas)))
           (ir (nelisp-phase47-compiler--parse sexp nil))
           (collected (nelisp-phase47-compiler--collect-strings ir))
           (str-offsets (car collected))
           (table-collected (nelisp-phase47-compiler--collect-tables ir))
           (table-offsets (car table-collected))
           (str-rodata-len (length (cdr collected)))
           (defuns (nelisp-phase47-compiler--collect-defuns ir)))
      (let* ((pass1-table-vaddrs
              (mapcar (lambda (entry) (cons (car entry) 0)) table-offsets))
             (pass1 (nelisp-phase47-compiler--pass
                     ir defuns str-offsets 0 pass1-table-vaddrs))
             (text-size (nelisp-asm-x86_64-buffer-pos pass1))
             (table-vaddrs
              (mapcar (lambda (entry)
                        (let* ((name (car entry))
                               (info (cdr entry))
                               (offset (plist-get info :offset)))
                          (cons name
                                (+ nelisp-pe--image-base-x86-64
                                   rodata-rva
                                   str-rodata-len
                                   offset))))
                      table-offsets))
             (pass2 (nelisp-phase47-compiler--pass
                     ir defuns str-offsets rodata-rva table-vaddrs))
             (text-bytes (nelisp-asm-x86_64-resolve-fixups pass2)))
        (ignore pass1)
        (unless (= (length text-bytes) text-size)
          (signal 'nelisp-phase47-compiler-error
                  (list :pass-length-mismatch
                        :pass1 text-size
                        :pass2 (length text-bytes))))
        text-bytes))))

(defun nelisp-windows-build--phase47-executable-bytes (sexp)
  "Return a PE32+ EXE byte string for Phase47 SEXP."
  (let* ((str-rodata-bytes
          (nelisp-windows-build--phase47-rodata-bytes sexp))
         (extra-rdata-bytes
          (nelisp-windows-build--phase47-extra-rdata-bytes sexp))
         (imports (nelisp-windows-build--phase47-import-names
                   sexp str-rodata-bytes)))
    (nelisp-pe-write-build-kernel32-executable
     imports
     (lambda (text-rva iat-rvas rodata-rva)
       (nelisp-windows-build--phase47-text
        sexp text-rva iat-rvas rodata-rva))
     extra-rdata-bytes)))

(defun nelisp-windows-build-phase47-exe (sexp out-path)
  "Write OUT-PATH as a PE32+ EXE compiled from Phase47 SEXP."
  (let ((bytes (nelisp-windows-build--phase47-executable-bytes sexp))
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil out-path nil 'silent))
  (message "nelisp-windows-build: wrote %s (Phase47 %S)" out-path sexp)
  out-path)

(defun nelisp-windows-build-phase47-exit42 ()
  "Batch entry: build target/nelisp-windows-phase47-exit42.exe."
  (nelisp-windows-build-phase47-exe
   '(exit 42)
   "target/nelisp-windows-phase47-exit42.exe"))

(defun nelisp-windows-build-phase47-hello ()
  "Batch entry: build target/nelisp-windows-phase47-hello.exe."
  (nelisp-windows-build-phase47-exe
   '(seq (write "hello\n") (exit 42))
   "target/nelisp-windows-phase47-hello.exe"))

(defun nelisp-windows-build-phase47-alloc42 ()
  "Batch entry: build target/nelisp-windows-phase47-alloc42.exe."
  (nelisp-windows-build-phase47-exe
   '(exit (if (= (alloc-bytes 4096 8) 0) 13 42))
   "target/nelisp-windows-phase47-alloc42.exe"))

(defun nelisp-windows-build-phase47-alloc-free42 ()
  "Batch entry: build target/nelisp-windows-phase47-alloc-free42.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (defun alloc_free_probe ()
       (let* ((p (alloc-bytes 4096 8)))
         (if (= p 0)
             13
           (seq (dealloc-bytes p 4096 8) 42))))
     (exit (alloc_free_probe)))
   "target/nelisp-windows-phase47-alloc-free42.exe"))

(defun nelisp-windows-build-phase47-syswrite42 ()
  "Batch entry: build target/nelisp-windows-phase47-syswrite42.exe."
  (nelisp-windows-build-phase47-exe
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
     (exit (syswrite_probe)))
   "target/nelisp-windows-phase47-syswrite42.exe"))

(defun nelisp-windows-build-phase47-sysread-byte ()
  "Batch entry: build target/nelisp-windows-phase47-sysread-byte.exe."
  (nelisp-windows-build-phase47-exe
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
     (exit (sysread_probe)))
   "target/nelisp-windows-phase47-sysread-byte.exe"))

(defun nelisp-windows-build--cstr-fill-forms (ptr bytes)
  "Return Phase47 forms that write BYTES plus NUL into PTR."
  (let ((i 0)
        (forms nil))
    (dolist (b bytes)
      (push `(ptr-write-u8 ,ptr ,i ,b) forms)
      (setq i (1+ i)))
    (push `(ptr-write-u8 ,ptr ,i 0) forms)
    (nreverse forms)))

(defun nelisp-windows-build--ascii-bytes (string)
  "Return list of ASCII byte values in STRING."
  (let ((i 0)
        (bytes nil))
    (while (< i (length string))
      (push (aref string i) bytes)
      (setq i (1+ i)))
    (nreverse bytes)))

(defun nelisp-windows-build-phase47-file-read-byte ()
  "Batch entry: build target/nelisp-windows-phase47-file-read-byte.exe."
  (let* ((path "nelisp-win-read.txt")
         (path-bytes (nelisp-windows-build--ascii-bytes path)))
    (nelisp-windows-build-phase47-exe
     `(seq
       (defun file_read_probe ()
         (let* ((path (alloc-bytes ,(1+ (length path-bytes)) 1))
                (buf (alloc-bytes 1 1)))
           (if (= path 0)
               13
             (if (= buf 0)
                 14
               (seq
                ,@(nelisp-windows-build--cstr-fill-forms 'path path-bytes)
                (let* ((fd (syscall-direct 2 path 0 0 0 0 0))
                       (n (syscall-direct 0 fd buf 1 0 0 0))
                       (b (ptr-read-u8 buf 0)))
                  (seq
                   (syscall-direct 3 fd 0 0 0 0 0)
                   (dealloc-bytes path ,(1+ (length path-bytes)) 1)
                   (dealloc-bytes buf 1 1)
                   (if (= n 1) b 15))))))))
       (exit (file_read_probe)))
     "target/nelisp-windows-phase47-file-read-byte.exe")))

(defun nelisp-windows-build-phase47-file-write42 ()
  "Batch entry: build target/nelisp-windows-phase47-file-write42.exe."
  (let* ((path "nelisp-win-write.txt")
         (path-bytes (nelisp-windows-build--ascii-bytes path)))
    (nelisp-windows-build-phase47-exe
     `(seq
       (defun file_write_probe ()
         (let* ((path (alloc-bytes ,(1+ (length path-bytes)) 1))
                (buf (alloc-bytes 1 1)))
           (if (= path 0)
               13
             (if (= buf 0)
                 14
               (seq
                ,@(nelisp-windows-build--cstr-fill-forms 'path path-bytes)
                (ptr-write-u8 buf 0 90)
                (let* ((fd (syscall-direct 2 path 577 420 0 0 0))
                       (n (syscall-direct 1 fd buf 1 0 0 0)))
                  (seq
                   (syscall-direct 3 fd 0 0 0 0 0)
                   (dealloc-bytes path ,(1+ (length path-bytes)) 1)
                   (dealloc-bytes buf 1 1)
                   (if (= n 1) 42 16))))))))
       (exit (file_write_probe)))
     "target/nelisp-windows-phase47-file-write42.exe")))

(defun nelisp-windows-build-phase47-mmap42 ()
  "Batch entry: build target/nelisp-windows-phase47-mmap42.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (defun mmap_probe ()
       (let* ((p (syscall-direct 9 0 4096 3 34 -1 0)))
         (if (= p 0)
             13
           (seq
            (ptr-write-u8 p 0 77)
            (syscall-direct 11 p 4096 0 0 0 0)
            42))))
     (exit (mmap_probe)))
   "target/nelisp-windows-phase47-mmap42.exe"))

(defun nelisp-windows-build-phase47-sysexit42 ()
  "Batch entry: build target/nelisp-windows-phase47-sysexit42.exe."
  (nelisp-windows-build-phase47-exe
   '(exit (syscall-direct 60 42 0 0 0 0 0))
   "target/nelisp-windows-phase47-sysexit42.exe"))

(defun nelisp-windows-build-phase47-table42 ()
  "Batch entry: build target/nelisp-windows-phase47-table42.exe."
  (nelisp-windows-build-phase47-exe
   '(seq
     (static-imm32-table-define "t" (7 42 99))
     (defun table_probe ()
       (static-imm32-table-lookup "t" 1))
     (exit (table_probe)))
   "target/nelisp-windows-phase47-table42.exe"))

(provide 'nelisp-windows-build)

;;; nelisp-windows-build.el ends here
