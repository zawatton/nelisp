;;; nelisp-phase47-compiler.el --- Phase 47 Sexp -> asm compiler (Doc 97)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 97 — Phase 47 production engagement entry point.
;;
;; This module is a *frontend* for the existing Doc 91-94 Phase 47
;; chain.  It consumes a Sexp source program (= an elisp form treated
;; as DATA, not code) and emits a static-linked Linux ELF64
;; executable via:
;;
;;   1. nelisp-asm-x86_64-* (Doc 92) for instruction encoding
;;   2. nelisp-elf-write-binary (Doc 91) for the final ELF on disk
;;
;; No new asm helpers, no new linker logic, no new ELF format work
;; lives here — Doc 97 is purely a Sexp-to-IR-to-asm walker that
;; calls existing chain entry points.
;;
;; v1 source grammar (= minimal):
;;
;;   (write "STRING")     -> write(1, addr, len) syscall
;;   (exit N)             -> exit(N) syscall
;;   (seq EXPR ...)       -> emit each EXPR in order
;;   (let ((VAR VAL)) B)  -> bind VAR to constant VAL in B
;;   (+ A B) / (- A B) / (* A B) -> compile-time constant folding
;;   integer literal       -> evaluates to the integer
;;   symbol reference      -> let-environment lookup (constants only)
;;
;; Anything else signals `nelisp-phase47-compiler-error' at parse
;; time.  Function calls, control flow, dynamic typing, heap
;; allocation all defer to Doc 97.b / 97.c / 97.d / 97.e.
;;
;; Architecture:
;;
;;   parse -> IR -> collect strings -> pass-1 emit (size-only) ->
;;   address resolution -> pass-2 emit (with real vaddrs) ->
;;   nelisp-elf-write-binary -> ELF on disk
;;
;; The two-pass emit is the design choice that keeps v1 register-
;; allocator-free: every Doc 92 instruction emits a fixed byte
;; length regardless of immediate values, so pass-1 measures
;; text-size deterministically and pass-2 uses real addresses.
;;
;; Not wired into nelisp-baker.

;;; Code:

(require 'cl-lib)
(require 'nelisp-asm-x86_64)
(require 'nelisp-elf-write)

(define-error 'nelisp-phase47-compiler-error
  "Doc 97 Phase 47 Sexp compiler error")

;; ---- §97.0 layout constants ----
;;
;; Match the Doc 91 single-PT_LOAD path: Ehdr (64) + Phdr (56) at file
;; offset 0, .text at offset 0x78, .rodata immediately after .text
;; with no padding.  These constants mirror nelisp-elf--build-rich's
;; phnum=1 layout — keep them in sync if §91.b is ever rearranged.

(defconst nelisp-phase47-compiler--vaddr-base #x400000
  "Default ELF64 ET_EXEC load base = 0x400000.
Matches `nelisp-elf--minimal-vaddr-base'.")

(defconst nelisp-phase47-compiler--text-off #x78
  "File offset of the .text section in the single-PT_LOAD layout.
Equals Ehdr(64) + Phdr(56) = 120 = 0x78.")

(defconst nelisp-phase47-compiler--text-vaddr
  (+ nelisp-phase47-compiler--vaddr-base
     nelisp-phase47-compiler--text-off)
  "Absolute virtual address of byte 0 of .text (= 0x400078).")

;; ---- §97.1 frontend = parser + IR builder ----
;;
;; The IR is a tagged plist.  Each node has shape
;;
;;   (:kind KIND <key val>...)
;;
;; where KIND is one of `write' / `exit' / `seq' / `let'.  Arithmetic
;; (`+', `-', `*') and bare integers + symbols are *not* IR nodes —
;; they fold to integer constants at parse time.

(defun nelisp-phase47-compiler--parse-int (sexp env)
  "Parse SEXP as an integer-typed expression in ENV, return an integer.
ENV is an alist `((SYM . VALUE) ...)' mapping let-bound symbols to
their constant values.  Signals `nelisp-phase47-compiler-error' if
SEXP cannot be reduced to a compile-time integer."
  (cond
   ((integerp sexp) sexp)
   ((symbolp sexp)
    (let ((cell (assq sexp env)))
      (unless cell
        (signal 'nelisp-phase47-compiler-error
                (list :free-symbol sexp :env-keys (mapcar #'car env))))
      (cdr cell)))
   ((and (consp sexp) (memq (car sexp) '(+ - *)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :arith-arity (car sexp) sexp)))
    (let ((op (car sexp))
          (a (nelisp-phase47-compiler--parse-int (nth 1 sexp) env))
          (b (nelisp-phase47-compiler--parse-int (nth 2 sexp) env)))
      (cond
       ((eq op '+) (+ a b))
       ((eq op '-) (- a b))
       ((eq op '*) (* a b)))))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :not-int-expr sexp)))))

(defun nelisp-phase47-compiler--parse (sexp &optional env)
  "Parse SEXP into Doc 97 IR using ENV (= let-binding alist).
Returns one of:
  (:kind write :str S)
  (:kind exit :status N)
  (:kind seq :forms (NODE NODE ...))
  (:kind let :var V :value N :body NODE)
Or signals `nelisp-phase47-compiler-error' on unsupported forms."
  (cond
   ;; (write "STRING")
   ((and (consp sexp) (eq (car sexp) 'write))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :write-arity sexp)))
    (let ((arg (nth 1 sexp)))
      (unless (stringp arg)
        (signal 'nelisp-phase47-compiler-error
                (list :write-not-string arg)))
      (list :kind 'write :str arg)))
   ;; (exit N)
   ((and (consp sexp) (eq (car sexp) 'exit))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :exit-arity sexp)))
    (let ((n (nelisp-phase47-compiler--parse-int (nth 1 sexp) env)))
      (unless (and (integerp n) (<= 0 n 255))
        (signal 'nelisp-phase47-compiler-error
                (list :status-out-of-range n)))
      (list :kind 'exit :status n)))
   ;; (seq EXPR...)
   ((and (consp sexp) (eq (car sexp) 'seq))
    (let ((children (cdr sexp)))
      (when (null children)
        (signal 'nelisp-phase47-compiler-error
                (list :empty-seq sexp)))
      (list :kind 'seq
            :forms (mapcar (lambda (c)
                             (nelisp-phase47-compiler--parse c env))
                           children))))
   ;; (let ((VAR VAL)) BODY)
   ((and (consp sexp) (eq (car sexp) 'let))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :let-arity sexp)))
    (let ((bindings (nth 1 sexp))
          (body (nth 2 sexp)))
      (unless (and (consp bindings) (= (length bindings) 1))
        (signal 'nelisp-phase47-compiler-error
                (list :let-multi-binding bindings)))
      (let* ((binding (car bindings))
             (var (car binding))
             (val-sexp (cadr binding)))
        (unless (symbolp var)
          (signal 'nelisp-phase47-compiler-error
                  (list :let-var-not-symbol var)))
        (let* ((val (nelisp-phase47-compiler--parse-int val-sexp env))
               (new-env (cons (cons var val) env))
               (body-ir (nelisp-phase47-compiler--parse body new-env)))
          (list :kind 'let :var var :value val :body body-ir)))))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :unknown-form sexp)))))

;; ---- §97.2 string collector ----
;;
;; Walks the IR collecting every distinct (= `equal'-deduped) string
;; literal referenced by a `write' node.  Assigns each a byte offset
;; within the eventual .rodata buffer.

(defun nelisp-phase47-compiler--collect-strings (ir)
  "Walk IR, return alist `((STR . (:offset N :len L)) ...)' + bytes.
The return shape is (STR-OFFSETS . RODATA-BYTES) where STR-OFFSETS
is an alist keyed by the literal string (= `equal' dedup) and
RODATA-BYTES is the concatenated unibyte payload.  String emit
order = first-encountered-in-walk order."
  (let ((offsets nil)
        (rodata "")
        (cursor 0))
    (cl-labels
        ((walk (node)
           (pcase (plist-get node :kind)
             ('write
              (let ((s (plist-get node :str)))
                (unless (assoc s offsets)
                  (let ((bs (encode-coding-string s 'utf-8 t)))
                    (setq offsets
                          (append offsets
                                  (list (cons s
                                              (list :offset cursor
                                                    :len (length bs))))))
                    (setq rodata (concat rodata bs))
                    (setq cursor (+ cursor (length bs)))))))
             ('exit nil)
             ('seq
              (mapc #'walk (plist-get node :forms)))
             ('let
              (walk (plist-get node :body))))))
      (walk ir))
    (cons offsets rodata)))

;; ---- §97.3 emit walker ----
;;
;; Walks the IR appending bytes to a Doc 92 macro-asm buffer.  Called
;; twice: pass-1 with RODATA-VADDR = 0 (= dry size), pass-2 with the
;; real address.

(defun nelisp-phase47-compiler--emit-write (buf str str-offsets rodata-vaddr)
  "Emit a write(1, addr, len) syscall for STR to BUF.
STR-OFFSETS is the alist from `--collect-strings'.  RODATA-VADDR is
the absolute virtual address of byte 0 of .rodata."
  (let* ((entry (cdr (or (assoc str str-offsets)
                         (signal 'nelisp-phase47-compiler-error
                                 (list :missing-string-entry str)))))
         (offset (plist-get entry :offset))
         (len    (plist-get entry :len))
         (addr   (+ rodata-vaddr offset)))
    ;; mov rax, 1     (SYS_write)         = 7 bytes
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    ;; mov rdi, 1     (fd = stdout)        = 7 bytes
    (nelisp-asm-x86_64-mov-imm32 buf 'rdi 1)
    ;; mov rsi, addr  (buffer absolute VA) = 10 bytes
    (nelisp-asm-x86_64-mov-imm64 buf 'rsi addr)
    ;; mov rdx, len   (byte count)         = 7 bytes
    (nelisp-asm-x86_64-mov-imm32 buf 'rdx len)
    ;; syscall                              = 2 bytes
    (nelisp-asm-x86_64-syscall buf)))

(defun nelisp-phase47-compiler--emit-exit (buf status)
  "Emit an exit(STATUS) syscall to BUF.  STATUS is an integer 0..255."
  ;; mov rax, 60   (SYS_exit)  = 7 bytes
  (nelisp-asm-x86_64-mov-imm32 buf 'rax 60)
  ;; mov rdi, status            = 7 bytes
  (nelisp-asm-x86_64-mov-imm32 buf 'rdi status)
  ;; syscall                    = 2 bytes
  (nelisp-asm-x86_64-syscall buf))

(defun nelisp-phase47-compiler--emit (ir buf str-offsets rodata-vaddr)
  "Walk IR appending instructions to BUF.
STR-OFFSETS maps literal string -> (:offset N :len L).
RODATA-VADDR is the absolute vaddr of byte 0 of .rodata (= 0 during
pass-1 sizing, real value during pass-2)."
  (pcase (plist-get ir :kind)
    ('write
     (nelisp-phase47-compiler--emit-write
      buf (plist-get ir :str) str-offsets rodata-vaddr))
    ('exit
     (nelisp-phase47-compiler--emit-exit
      buf (plist-get ir :status)))
    ('seq
     (dolist (child (plist-get ir :forms))
       (nelisp-phase47-compiler--emit child buf str-offsets rodata-vaddr)))
    ('let
     ;; Body has already been parsed with the binding folded into ENV
     ;; so emit walks straight into the body — no runtime store.
     (nelisp-phase47-compiler--emit
      (plist-get ir :body) buf str-offsets rodata-vaddr))
    (kind
     (signal 'nelisp-phase47-compiler-error
             (list :unknown-ir-kind kind)))))

;; ---- §97.4 orchestrator ----

(defun nelisp-phase47-compiler--pass (ir str-offsets rodata-vaddr)
  "Run a fresh emit pass returning the buffer.
IR is the parsed program; STR-OFFSETS is the dedup-offset table;
RODATA-VADDR is the absolute vaddr of .rodata (= 0 during pass-1)."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (nelisp-phase47-compiler--emit ir buf str-offsets rodata-vaddr)
    buf))

;;;###autoload
(cl-defun nelisp-phase47-compile-sexp
    (sexp file-path &key (arch 'x86_64) (entry-sym "_start"))
  "Compile SEXP to a static-linked ELF64 executable at FILE-PATH.

SEXP is a Doc 97 v1 source program (= a single top-level form).
The accepted grammar is documented in Doc 97 §1 — briefly:

  (write \"STRING\")
  (exit N)                       (N is integer 0..255 after folding)
  (seq EXPR...)
  (let ((VAR VAL)) BODY)
  (+ A B) / (- A B) / (* A B)    (compile-time folded)
  integer / let-bound symbol

FILE-PATH is the output binary path.  The file is written with mode
#o755 (= +x bit set) by `nelisp-elf-write-binary'.

ARCH defaults to `x86_64'.  v1 signals
`nelisp-phase47-compiler-error' for any other ARCH (= aarch64
deferred to Doc 97.b).

ENTRY-SYM defaults to `_start' (= the kernel-recognised entry name).

Returns FILE-PATH on success.  Signals on parse error, free symbol
reference, out-of-range integer, or any pass-1/pass-2 byte-length
drift (= a Doc 92 emitter invariant violation)."
  (unless (eq arch 'x86_64)
    (signal 'nelisp-phase47-compiler-error
            (list :unsupported-arch arch)))
  (let* ((ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (str-offsets (car collected))
         (rodata-bytes (cdr collected))
         ;; Pass 1: dry size measurement.  RODATA-VADDR = 0 is just a
         ;; placeholder; every Doc 92 instruction's byte length is
         ;; immediate-independent so the size is invariant.
         (pass1 (nelisp-phase47-compiler--pass ir str-offsets 0))
         (text-size (nelisp-asm-x86_64-buffer-pos pass1))
         ;; Resolve .rodata vaddr now that text size is known.
         (rodata-vaddr (+ nelisp-phase47-compiler--text-vaddr text-size))
         ;; Pass 2: real emit with the resolved address.
         (pass2 (nelisp-phase47-compiler--pass ir str-offsets rodata-vaddr))
         (text-bytes (nelisp-asm-x86_64-buffer-bytes pass2)))
    ;; Invariant: pass-1 and pass-2 must emit byte-identical lengths.
    ;; Any drift means a Doc 92 emitter started using a variable
    ;; length encoding (= immediate-dependent), which would break the
    ;; address resolution.  Surface it loudly.
    (unless (= (length text-bytes) text-size)
      (signal 'nelisp-phase47-compiler-error
              (list :pass-length-mismatch
                    :pass1 text-size
                    :pass2 (length text-bytes))))
    (let* ((have-rodata (> (length rodata-bytes) 0))
           (symbols
            (cons (list :name entry-sym :value 0
                        :size (length text-bytes)
                        :section 'text :bind 'global :type 'func)
                  (if have-rodata
                      (list (list :name "rodata_blob" :value 0
                                  :size (length rodata-bytes)
                                  :section 'rodata
                                  :bind 'local :type 'object))
                    nil)))
           (sections (list :text text-bytes
                           :rodata (if have-rodata
                                       rodata-bytes
                                     (unibyte-string))
                           :symbols symbols
                           :entry-sym entry-sym
                           :machine arch)))
      (nelisp-elf-write-binary file-path sections))
    file-path))

(provide 'nelisp-phase47-compiler)

;;; nelisp-phase47-compiler.el ends here
