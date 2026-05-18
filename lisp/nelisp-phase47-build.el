;;; nelisp-phase47-build.el --- Phase 47 build orchestrator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 96 — chain demonstrator that ties together the five Phase 47
;; pure-elisp components into a single, callable orchestrator plus
;; three pre-built demo functions that produce runnable Linux
;; binaries.
;;
;; This module is /minimal/ by design — ~180 elisp LOC, no new file
;; format, no new ABI.  It just wires the existing Doc 91-94 entry
;; points into the canonical sequence and adds three demos that
;; exercise representative shapes:
;;
;;   1. hello-world      = fixed-message, write(2) + exit(2)
;;   2. exit-with-status = configurable code, exit(2) only
;;   3. argc-printer     = SysV AMD64 argv-stack unpack
;;
;; The orchestrator is freestanding — not wired into the production
;; STDLIB load.  A future "Doc 97 Sexp source compiler" would
;; supply ASM-FN values that lower Sexp source through Doc 92's
;; macro assembler before calling the orchestrator; for Doc 96, the
;; three demos hard-code their ASM-FN to call Doc 94's crt0 helpers
;; (= already-verified byte tables from Intel SDM Vol 2 / SysV
;; AMD64 psABI).
;;
;; Status: SHIPPED.  Three demos emit + exec on x86_64 Linux to
;; their expected stdout / exit code.  arm64 demo exec is deferred
;; (= tag-byte check only).

;;; Code:

(require 'cl-lib)
(require 'nelisp-elf-write)
(require 'nelisp-crt0)
(require 'nelisp-static-linker)

(define-error 'nelisp-phase47-build-error
  "nelisp Phase 47 build orchestrator error")

;; ---- §3.1 main orchestrator ----

(defun nelisp-phase47--coerce-arch (arch)
  "Validate ARCH (= `x86_64' or `aarch64') and return the symbol.
Signals `nelisp-phase47-build-error' on any other value."
  (unless (memq arch '(x86_64 aarch64))
    (signal 'nelisp-phase47-build-error
            (list :unknown-arch arch)))
  arch)

(defun nelisp-phase47--prebuilt-plist-p (plist)
  "Return non-nil when PLIST is a `:prebuilt' short-circuit response.
A prebuilt plist carries `:text' / `:rodata' / `:symbols' /
`:entry-sym' directly = no Doc 93 linker pass needed."
  (and (listp plist) (eq (plist-get plist :units) :prebuilt)))

;;;###autoload
(cl-defun nelisp-phase47-build-program
    (asm-fn file-path &key (arch 'x86_64) (entry-sym "_start"))
  "Drive the Phase 47 chain to emit a static-linked ELF at FILE-PATH.

ASM-FN is a 0-arg function returning a plist with keys:
  :units      list of Doc 93 compile-units OR symbol `:prebuilt'
              (= short-circuit for Doc 96 demos)
  :text       (prebuilt path) unibyte bytes = `.text' payload
  :rodata     (prebuilt path) unibyte bytes = `.rodata' payload
  :symbols    (prebuilt path) list of symbol plists for Doc 91
  :data       optional, unibyte bytes = `.data' payload
  :bss-size   optional, integer = `.bss' zero-fill size

ARCH defaults to `x86_64'; `aarch64' is accepted and threaded into
the ELF writer's `:machine' field (= cross-arch tag validation
without exec-time codegen yet).

ENTRY-SYM defaults to `_start' (= Doc 94 crt0 convention).

Returns FILE-PATH on success."
  (nelisp-phase47--coerce-arch arch)
  (let ((plist (funcall asm-fn)))
    (unless (listp plist)
      (signal 'nelisp-phase47-build-error
              (list :asm-fn-return-not-plist plist)))
    (cond
     ((nelisp-phase47--prebuilt-plist-p plist)
      (nelisp-phase47--emit-prebuilt plist file-path arch entry-sym))
     (t
      (nelisp-phase47--emit-linked plist file-path arch entry-sym)))))

(defun nelisp-phase47--emit-prebuilt (plist file-path arch entry-sym)
  "Hand a `:prebuilt' PLIST directly to the Doc 91 ELF writer.
ARCH is threaded as `:machine'; ENTRY-SYM overrides the supplied
`:entry-sym' when the caller plist omits it."
  (let* ((text   (plist-get plist :text))
         (rodata (plist-get plist :rodata))
         (data   (plist-get plist :data))
         (bss    (plist-get plist :bss-size))
         (syms   (plist-get plist :symbols))
         (esym   (or (plist-get plist :entry-sym) entry-sym))
         (sections (list :text text :rodata rodata
                         :symbols syms :entry-sym esym
                         :machine arch)))
    (when data
      (setq sections (plist-put sections :data data)))
    (when bss
      (setq sections (plist-put sections :bss-size bss)))
    (nelisp-elf-write-binary file-path sections)
    file-path))

(defun nelisp-phase47--emit-linked (plist file-path arch entry-sym)
  "Run Doc 93 linker on PLIST's `:units' then emit via Doc 91.
ARCH is threaded as `:machine'; ENTRY-SYM names the entry symbol."
  (let* ((units (plist-get plist :units))
         (layout (or (plist-get plist :section-layout)
                     ;; Default layout = Doc 91 single-PT_LOAD
                     ;; convention: Ehdr+Phdr (0x78) at vaddr+0x78,
                     ;; .rodata immediately after .text.
                     '((text   . #x400078)
                       (rodata . #x401000)
                       (data   . #x402000)
                       (bss    . #x403000))))
         (result (nelisp-link-units-2pass units layout))
         (bytes-alist (plist-get result :bytes))
         (text   (cdr (assq 'text   bytes-alist)))
         (rodata (cdr (assq 'rodata bytes-alist)))
         (data   (cdr (assq 'data   bytes-alist)))
         (bss    (cdr (assq 'bss    bytes-alist)))
         ;; Symtab → list of symbol plists for Doc 91.
         (syms '()))
    (maphash (lambda (_k v)
               (push (list :name (plist-get v :name)
                           :value (plist-get v :value)
                           :size  (or (plist-get v :size) 0)
                           :section (or (plist-get v :section) 'text)
                           :bind  (or (plist-get v :bind) 'global)
                           :type  (or (plist-get v :type) 'func))
                     syms))
             (plist-get result :symtab))
    (let ((sections (list :text text :rodata rodata
                          :symbols syms :entry-sym entry-sym
                          :machine arch)))
      (when (and data (> (length data) 0))
        (setq sections (plist-put sections :data data)))
      (when (and bss (> bss 0))
        (setq sections (plist-put sections :bss-size bss)))
      (nelisp-elf-write-binary file-path sections))
    file-path))

;; ---- §3.2 hello-world demo ----

;;;###autoload
(cl-defun nelisp-phase47-build-hello-world (file-path &key (arch 'x86_64))
  "Emit a hello-world static ELF to FILE-PATH.
On exec, the binary prints `hello\\n' to stdout and exits with
status code 0.  ARCH defaults to `x86_64'; `aarch64' produces a
valid EM_AARCH64-tagged ELF (= tag-byte check; aarch64 code
generation deferred to a sibling agent).  Returns FILE-PATH."
  (nelisp-phase47--coerce-arch arch)
  (cond
   ((eq arch 'x86_64)
    ;; Doc 94 §94.a helper already does the full chain (= text+rodata
    ;; payload + Doc 91 writer); just delegate.
    (nelisp-crt0-emit-hello-world file-path))
   ((eq arch 'aarch64)
    ;; Cross-arch tag-only ELF: 4-byte NOP text + EM_AARCH64 machine.
    (nelisp-elf-write-binary
     file-path
     (list :text   (unibyte-string #x1f #x20 #x03 #xd5) ; aarch64 NOP
           :rodata (unibyte-string)
           :symbols (list (list :name "_start" :value 0 :size 4
                                :section 'text :bind 'global :type 'func))
           :entry-sym "_start"
           :machine 'aarch64))
    file-path)))

;; ---- §3.3 exit-with-status demo ----

(defun nelisp-phase47--exit-with-status-text (status)
  "Return the 7-byte `_start' stub that exits with STATUS.
Encoding (= Intel SDM Vol 2):
  bf SS 00 00 00       mov  edi, STATUS
  b8 3c 00 00 00       mov  eax, 60       (SYS_exit)
  0f 05                syscall
Length total = 12 bytes (= 5 + 5 + 2)."
  (unless (and (integerp status) (<= 0 status 255))
    (signal 'nelisp-phase47-build-error
            (list :status-out-of-range status)))
  (concat
   (unibyte-string #xbf
                   (logand status #xff) #x00 #x00 #x00)
   (unibyte-string #xb8 #x3c #x00 #x00 #x00)
   (unibyte-string #x0f #x05)))

;;;###autoload
(cl-defun nelisp-phase47-build-exit-with-status
    (file-path status &key (arch 'x86_64))
  "Emit a minimal static ELF to FILE-PATH that exits with STATUS.
STATUS is an integer 0..255 (= Linux exit-status byte range).
ARCH defaults to `x86_64'; `aarch64' signals (= aarch64 syscall
encoding not yet wired into Doc 96).  Returns FILE-PATH."
  (nelisp-phase47--coerce-arch arch)
  (unless (eq arch 'x86_64)
    (signal 'nelisp-phase47-build-error
            (list :exit-with-status-arch-unsupported arch)))
  (let ((text (nelisp-phase47--exit-with-status-text status)))
    (nelisp-elf-write-binary
     file-path
     (list :text text
           :rodata (unibyte-string)
           :symbols
           (list (list :name "_start" :value 0
                       :size (length text)
                       :section 'text :bind 'global :type 'func))
           :entry-sym "_start"
           :machine arch))
    file-path))

;; ---- §3.4 argc-printer demo ----

;;;###autoload
(cl-defun nelisp-phase47-build-argc-printer (file-path &key (arch 'x86_64))
  "Emit a static ELF to FILE-PATH that prints `argc=<N>\\n' to stdout.
<N> is the kernel-supplied argv count = 1 when invoked with no
trailing args, 1+K when invoked with K trailing args.  Exercises
the SysV AMD64 process-entry handoff (= Doc 94 §94.c argv-stack
unpack).

ARCH defaults to `x86_64'; `aarch64' signals (= aarch64 argv
unpack deferred to Doc 94 §94.b sibling work).  Returns FILE-PATH."
  (nelisp-phase47--coerce-arch arch)
  (unless (eq arch 'x86_64)
    (signal 'nelisp-phase47-build-error
            (list :argc-printer-arch-unsupported arch)))
  ;; Doc 94 §94.c helper already wires the 108-byte argv-unpack
  ;; routine + Doc 91 writer; just delegate.
  (nelisp-crt0-emit-hello-with-argc file-path))

(provide 'nelisp-phase47-build)

;;; nelisp-phase47-build.el ends here
