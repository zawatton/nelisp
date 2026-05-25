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
;; Doc 97.b extensions:
;;
;;   (defun NAME (PARAMS...) BODY) -> SysV AMD64 callee
;;   (NAME ARG ...)        -> call previously-defun'd function
;;   (+ A B) / (- A B) / (* A B) -> runtime emit when operands not
;;                                   compile-time constants
;;   symbol reference      -> param register lookup inside function
;;
;; Doc 97.c extensions:
;;
;;   (if TEST THEN ELSE)   -> TEST != 0 ? THEN : ELSE
;;   (while TEST BODY...)  -> while TEST != 0 do BODY; returns 0
;;   (cond (P1 B1) ... (t Bn)) -> first matching clause; t = always
;;   (< A B) / (> A B) / (<= A B) / (>= A B) / (= A B) -> 0 or 1
;;   (and EXPR ...)        -> first 0 short-circuit, else last value
;;   (or EXPR ...)         -> first non-0 short-circuit, else 0
;;
;; Calling convention follows SysV AMD64: args 1..6 in rdi, rsi, rdx,
;; rcx, r8, r9; return value in rax.  Functions establish a minimal
;; stack frame (push rbp; mov rbp, rsp; ...; pop rbp; ret) so callers
;; honour the 16-byte alignment ABI requirement at the call boundary
;; (= _start's RSP is 16-byte aligned by the kernel; one push rbp
;; before each call site keeps subsequent call sites 16-aligned).
;;
;; Anything else signals `nelisp-phase47-compiler-error' at parse
;; time.  Dynamic typing and heap allocation defer to Doc 97.d /
;; 97.e; Doc 97.c lands control flow and integer comparison.
;;
;; Architecture:
;;
;;   parse -> IR -> collect strings -> collect defuns ->
;;   pass-1 emit (size-only) -> address resolution ->
;;   pass-2 emit (with real vaddrs) -> resolve intra-text fixups ->
;;   nelisp-elf-write-binary -> ELF on disk
;;
;; The two-pass emit is the design choice that keeps v1 register-
;; allocator-free: every Doc 92 instruction emits a fixed byte
;; length regardless of immediate values, so pass-1 measures
;; text-size deterministically and pass-2 uses real addresses.
;;
;; Doc 97.b note on byte-length invariance: runtime arithmetic and
;; intra-text `call' use only fixed-width Doc 92 emitters (mov-imm32,
;; mov-imm64, add-reg-reg, sub-reg-reg, mov-reg-reg, push/pop, ret,
;; call-rel32, emit-bytes for imul) so the pass-1/pass-2 byte-length
;; invariant continues to hold.
;;
;; Doc 97.c control-flow emitters use only fixed-width Doc 92 helpers
;; (cmp-reg-reg, cmp-imm32, jz-rel32, jnz-rel32, jmp-rel32, setcc-al,
;; movzx-eax-al) and reuse `call-rel32''s rel32 fixup model for the
;; intra-function branch targets, so byte invariance is preserved.
;; Labels emitted for if/while/cond/and/or are stamped with a parse-
;; time unique id so pass-1 and pass-2 see identical label names.
;;
;; Not wired into the production STDLIB load.

;;; Code:

(require 'cl-lib)
(require 'macroexp)
(require 'nelisp-asm-arm64)
(require 'nelisp-asm-x86_64)
(require 'nelisp-elf-write)
(require 'nelisp-sexp-layout)

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

(defconst nelisp-phase47-compiler--arg-regs
  '(rdi rsi rdx rcx r8 r9)
  "SysV AMD64 integer argument registers (= positional, 1..6).
Doc 97.b started with up to 6 args; Doc 129.7E opens the first
SysV stack-argument path for GP extern calls and boxed-boundary
defuns that receive 7+ GP params.")

(defconst nelisp-phase47-compiler--xmm-arg-regs
  '(xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7)
  "SysV AMD64 / aarch64 floating-point argument registers (positional 1..8).
Doc 110 §110.A f64 ABI groundwork.  Names map to the x86_64 xmm
register table; aarch64 emit translates to d0..d7 via a parallel
table when that arch lands the f64 emit pass (Doc 110 §110.D).
Doc 110 MVP supports up to 8 f64 args.")

(defvar nelisp-phase47-compiler--label-counter 0
  "Parse-time monotonic counter for control-flow label uniqueness.
Bound fresh to 0 by `nelisp-phase47-compile-sexp' before parsing
each top-level form so the generated label names are reproducible
within one compile but never collide between control-flow nodes.")

(defvar nelisp-phase47-compiler--arch 'x86_64
  "Target arch bound by the public compile entry points before emit.
Emit helpers consult this dynvar to pick the x86_64 or aarch64 code
path while sharing the same parsed IR.")

(defvar nelisp-phase47-compiler--abi 'sysv
  "Calling convention ABI bound by the public compile entry points.
Doc 101 §101.B Wave 5: selects SysV vs Win64 emit paths for x86_64.
  'sysv  — System V AMD64 (Linux / macOS); default
  'win64 — Microsoft x64 (Windows x86_64)
Emit helpers consult this dynvar for arg-register selection, shadow
space allocation, and prologue/epilogue layout.  The aarch64 path
ignores this var (= AAPCS64 only).")

(defvar nelisp-phase47-compiler--next-rt-let-slot nil
  "Cons cell `(N)' holding the next free runtime-let frame slot index.
Bound by the `defun' parser around body parsing; N starts at the
defun's param count (= first slot after param spill area).  Each
`let-rt' parse bumps N by 1 and records the old N as the slot for
that binding.  Nil at top level (= outside any defun parse), where
runtime `let' is not yet supported.")

(defvar nelisp-phase47-compiler--table-vaddrs nil
  "Alist `((NAME . VADDR) ...)' of absolute vaddrs for static-imm32 tables.
Bound by `--pass' before emit so `--emit-table-lookup' can resolve
the table name to its absolute virtual address.  During pass-1
sizing every VADDR is 0 (= placeholder); during pass-2 the values
are the real `.rodata' vaddrs.  Nil outside compile passes (=
default top-level binding).  See Doc 49 Wave 11.1.")

;; ---- Doc 49 Wave 11.2 hash-table primitive desugar ----
;;
;; Wave 11.2 introduces four runtime hash-table primitives — `hash-
;; table-make' / `hash-table-put' / `hash-table-get' / `hash-table-
;; contains-p' — implemented as **parse-time macros** that desugar
;; into compositions of existing Phase 47 primitives (`record-make',
;; `record-slot-ref-ptr', `record-slot-set', `cons-make-with-clone',
;; `sexp-payload-ptr', `extern-call nelisp_fnv1a', `str-eq', etc.).
;;
;; No new emit functions, no new asm helpers, no new collector
;; passes are required — the desugar produces nested sexp forms that
;; the existing parser handles directly.  This keeps the Rust LOC
;; delta = 0 (= NlRecord / NlConsBox alloc paths re-used unchanged)
;; and the elisp delta minimal.
;;
;; Storage layout (= simpler than the bucket-array hashtable in
;; `nelisp-stdlib-fast-hash.el' — the record IS the bucket array):
;;
;;   Sexp::Record(tag = HT-TAG-PTR, slots = capacity)
;;     slot[i] = Sexp::Cons(bucket-head) or Sexp::Nil
;;       Each bucket cell = ((KEY . VALUE) . NEXT) where KEY is the
;;       lookup pointer (Sexp::Symbol / Sexp::Str) and NEXT is the
;;       next bucket cell or Sexp::Nil.
;;
;; The bucket index = `(logand (extern-call nelisp_fnv1a KEY)
;; (- CAPACITY 1))' — CAPACITY must be a power of two (caller is
;; responsible).  This matches the env-mirror fast-path mask
;; pattern already used by `nelisp_mirror_lookup_entry'.
;;
;; Caller-owned scratch slot convention (= mirrors `record-make' /
;; `cons-make' / `cons-make-with-clone'): every constructor takes
;; an explicit `SLOT' parameter (= `*mut Sexp') for the result.

(defun nelisp-phase47-compiler--ht-desugar-make (cap-expr slot-expr tag-ptr-expr)
  "Desugar `(hash-table-make TAG-PTR CAP SLOT)' to a `record-make' form.
TAG-PTR-EXPR is a value-form producing a `*const Sexp' that
points at the type-tag symbol for the new record (caller
typically passes a static `Sexp::Symbol' allocated elsewhere).
CAP-EXPR evaluates to the bucket count (= power of two for the
fast-path mask) and SLOT-EXPR is the `*mut Sexp' destination.

Returns the equivalent sexp `(record-make TAG-PTR CAP SLOT)' — the
new record's slots are already pre-filled with `Sexp::Nil' by
`nl_alloc_record', which is exactly the empty-bucket sentinel."
  `(record-make ,tag-ptr-expr ,cap-expr ,slot-expr))

(defun nelisp-phase47-compiler--ht-desugar-bucket-idx (ht-expr key-expr)
  "Build the sexp computing `(fnv1a(KEY) & (CAP - 1))'.
HT-EXPR is the hash-table `*const Sexp', KEY-EXPR is the lookup
key `*const Sexp'.  Capacity comes from the record's slot count
(= `record-slot-count' returns the live slot count).  Power-of-
two CAP is the caller's responsibility — non-pow2 capacity
yields a uniform hash but the index will not span all buckets."
  `(logand (extern-call nelisp_fnv1a ,key-expr)
           (- (record-slot-count ,ht-expr) 1)))

(defun nelisp-phase47-compiler--ht-desugar-put
    (ht-expr key-expr val-expr cons-slot-expr pair-slot-expr)
  "Desugar `(hash-table-put HT KEY VAL CONS-SLOT PAIR-SLOT)'.
Conceptually evaluates to:

  bucket_idx = fnv1a(KEY) & (CAP - 1)
  pair      = cons-with-clone(KEY, VAL)           ; PAIR-SLOT
  new_head  = cons-with-clone(pair, HT.slot[idx]) ; CONS-SLOT
  record-slot-set(HT, bucket_idx, new_head)
  return HT

CONS-SLOT-EXPR and PAIR-SLOT-EXPR are caller-owned scratch `*mut
Sexp' slots used as the destination for the two intermediate
cons cells.  Both must be distinct (= the put writes through
both).  The returned form is itself a value-producing IR that
evaluates to the HT pointer.

Implementation note: the bucket index gets re-computed twice (=
once for `record-slot-ref' to read the existing head, once for
`record-slot-set' to write the new head) because nesting a
runtime `let' here would require an enclosing defun context that
the desugar cannot enforce.  The duplicated fnv1a is acceptable
(= rest of the operation is already dominated by the two cons
clones); a future optimisation may inline a let when the desugar
runs inside a defun body."
  (let ((idx-expr (nelisp-phase47-compiler--ht-desugar-bucket-idx
                   ht-expr key-expr)))
    `(record-slot-set
      ,ht-expr
      ,idx-expr
      (cons-make-with-clone
       (cons-make-with-clone ,key-expr ,val-expr ,pair-slot-expr)
       (record-slot-ref ,ht-expr ,idx-expr ,cons-slot-expr)
       ,cons-slot-expr))))

(defun nelisp-phase47-compiler--ht-desugar-get
    (ht-expr key-expr slot-expr)
  "Desugar `(hash-table-get HT KEY SLOT)'.
Walks the bucket chain at `HT.slots[fnv1a(KEY) & (CAP-1)]'.  On
hit, copies the matching VALUE into SLOT and returns SLOT.  On
miss, writes `Sexp::Nil' into SLOT and returns SLOT.

Calls the helper defun `nelisp_ht_walk' (= shared with `hash-
table-contains-p') which iterates the bucket chain; the helper
must be present in the compile unit (= the user must include
`(hash-table-helpers)' once, or include the helper defun
explicitly).  Returns a value-producing IR.

The walk returns the `*const Sexp' of the matching VALUE slot on
hit, or 0 on miss; the wrapper copies that pointer into SLOT
via `sexp-write-nil' + memcpy-style — simpler at MVP: the walk
returns the value pointer directly and SLOT is unused (= the
caller treats the return value as the lookup result)."
  ;; MVP: the get returns the matching value's *const Sexp directly
  ;; in rax (= 0 means miss).  SLOT is the destination for the
  ;; `Sexp::Nil' sentinel on miss; on hit the caller dereferences
  ;; the returned pointer.  This matches the `nelisp_mirror_lookup_
  ;; entry' ABI shape.
  ;;
  ;; The walk helper is called by name; the compile unit must
  ;; include the helper defun via `(hash-table-helpers)' sugar.
  `(nelisp_ht_walk
    (sexp-payload-ptr
     (record-slot-ref-ptr ,ht-expr
                          ,(nelisp-phase47-compiler--ht-desugar-bucket-idx
                            ht-expr key-expr)))
    ,key-expr
    ,slot-expr))

(defun nelisp-phase47-compiler--ht-desugar-contains-p
    (ht-expr key-expr slot-expr)
  "Desugar `(hash-table-contains-p HT KEY SLOT)' → 1 if KEY found else 0.
Re-uses the `nelisp_ht_walk' helper (= same defun `hash-table-
get' calls); returns 1 when the returned pointer is non-zero,
else 0.  SLOT is the same throwaway scratch the `get' variant
uses."
  `(if (= (nelisp_ht_walk
           (sexp-payload-ptr
            (record-slot-ref-ptr ,ht-expr
                                 ,(nelisp-phase47-compiler--ht-desugar-bucket-idx
                                   ht-expr key-expr)))
           ,key-expr
           ,slot-expr)
          0)
       0
     1))

(defconst nelisp-phase47-compiler--ht-helpers-source
  '(seq
    ;; `nelisp_ht_walk' is the shared bucket-chain walker used by
    ;; `hash-table-get' and `hash-table-contains-p'.  Mirrors the
    ;; `nelisp_mirror_walk_bucket' impl in
    ;; `lisp/nelisp-cc-mirror-lookup-entry.el' but specialised for
    ;; the W11.2 layout (= bucket cell = ((KEY . VALUE) . NEXT),
    ;; KEY at box+0, VALUE at box+32 inside the inner pair).
    ;;
    ;; box-ptr: i64.  0 = end-of-bucket.  Otherwise a live
    ;;          `NlConsBox*' addressing the current bucket cell
    ;;          (= car holds the (KEY . VALUE) pair, cdr holds the
    ;;          next bucket cell or Sexp::Nil).
    ;; key-ptr: `*const Sexp' for the lookup key.
    ;; nil-slot: caller-owned `*mut Sexp' used as the Sexp::Nil
    ;;           sentinel scratch on miss (= future-proof; the MVP
    ;;           impl ignores it but the helper accepts it so the
    ;;           caller's stack discipline is uniform between hit
    ;;           and miss paths).
    ;;
    ;; Returns: i64.  On hit, the `*const Sexp' of the matching
    ;; VALUE (= the pair box's cdr slot, at offset 32 inside the
    ;; inner pair).  On miss / end-of-bucket, 0.
    (defun nelisp_ht_walk (box-ptr key-ptr nil-slot)
      (if (= box-ptr 0)
          0
        (let ((pair-box (sexp-payload-ptr box-ptr)))
          (if (= (str-eq pair-box key-ptr) 1)
              (+ pair-box 32)
            (nelisp_ht_walk
             (cons-cdr-raw-from-box box-ptr)
             key-ptr
             nil-slot)))))))

(defvar nelisp-phase47-compiler--current-defun-arity nil
  "Arity of the defun currently being emitted, or nil during main `_start'.
Bound dynamically by `--emit-defun' so inner emit helpers (= those
that issue `call' / `extern-call') can compute the correct stack
alignment correction.  The body-entry rsp alignment depends on the
parity of the param-spill count: even arity → rsp ≡ 0 mod 16 (= call
ready), odd arity → rsp ≡ 8 mod 16 (= one extra `sub rsp, 8' before
each call to restore alignment).

Doc 111 §111.E surfaced this bug when 1-arg helpers
(`nelisp_frame_push' / `nelisp_frame_pop') crashed inside the Rust
shim's SSE-aligned stack accesses; the pre-existing fix is the
manual `push %rsi' inside `--emit-record-slot-ref' (= 3-arg defun,
also odd) which the design notes call out as a stack-alignment
band-aid.  This dynvar generalises the pattern.")

(defsubst nelisp-phase47-compiler--current-arg-regs ()
  "Return the GP argument register list for the current ABI.
Doc 101 §101.B Wave 5: dispatches on `nelisp-phase47-compiler--abi'."
  (if (eq nelisp-phase47-compiler--abi 'win64)
      nelisp-asm-x86_64--abi-win64-arg-regs
    nelisp-phase47-compiler--arg-regs))

(defun nelisp-phase47-compiler--gensym (prefix)
  "Return a fresh symbol `PREFIX-N' for control-flow labelling.
N is the next value of `nelisp-phase47-compiler--label-counter'.
The result is interned (= comparable with `eq') so it slots
straight into Doc 92's labels alist."
  (setq nelisp-phase47-compiler--label-counter
        (1+ nelisp-phase47-compiler--label-counter))
  (intern (format "%s-%d" prefix nelisp-phase47-compiler--label-counter)))

;; ---- §97.1 frontend = parser + IR builder ----
;;
;; The IR is a tagged plist.  Each node has shape
;;
;;   (:kind KIND <key val>...)
;;
;; v1 kinds: `write' / `exit' / `seq' / `let'.
;; §97.b adds: `defun' / `call' / `ref' / `arith'.
;;
;; Arithmetic with all-constant operands folds to an integer literal
;; at parse time (= legacy v1 path).  Arithmetic involving a `ref'
;; (= function parameter) emits an `:kind arith' node compiled to
;; runtime instructions in §97.5.

(defun nelisp-phase47-compiler--int-foldable-p (sexp env fenv)
  "Return non-nil if SEXP is compile-time foldable in ENV/FENV.
ENV is the let-binding alist; FENV is the function-parameter alist
(symbols whose values live in registers and therefore prevent
folding).  Falls through cheaply (= no recursion into call args)
because §97.b only folds the same shapes v1 did."
  (cond
   ((integerp sexp) t)
   ((symbolp sexp)
    (and (not (assq sexp fenv))
         (assq sexp env)))
   ((and (consp sexp) (memq (car sexp) '(+ - *))
         (= (length sexp) 3))
    (and (nelisp-phase47-compiler--int-foldable-p (nth 1 sexp) env fenv)
         (nelisp-phase47-compiler--int-foldable-p (nth 2 sexp) env fenv)))
   (t nil)))

(defun nelisp-phase47-compiler--fold-int (sexp env)
  "Fold SEXP to a compile-time integer using ENV (= let alist).
Caller has already verified foldability via
`nelisp-phase47-compiler--int-foldable-p'."
  (cond
   ((integerp sexp) sexp)
   ((symbolp sexp) (cdr (assq sexp env)))
   ((consp sexp)
    (let ((op (car sexp))
          (a (nelisp-phase47-compiler--fold-int (nth 1 sexp) env))
          (b (nelisp-phase47-compiler--fold-int (nth 2 sexp) env)))
      (cond
       ((eq op '+) (+ a b))
       ((eq op '-) (- a b))
       ((eq op '*) (* a b)))))))

(defconst nelisp-phase47-compiler--cmp-ops
  '(< > <= >= =)
  "Doc 97.c comparison operators (= produce 0 or 1 in rax).")

(defun nelisp-phase47-compiler--parse-extern-call-args
    (op name raw-args env fenv defuns)
  "Parse Doc 122 §122.C extern-call arg list into IR descriptors.
OP is the head op symbol (= `extern-call' or `extern-call-f64').
NAME is the extern symbol (for diagnostics).  RAW-ARGS is the
list following NAME in the source form; each entry is either a
bare value expression (= legacy `:i64' default), a `(:i64 EXPR)'
plist, a `(:f64 EXPR)' plist, or a single trailing `(:varargs
ARG ...)' marker that itself wraps the same per-arg plist shape.
ENV / FENV / DEFUNS are forwarded to `--parse-value' for each
underlying expression.

Returns a plist `(:args (PARSED-NODE ...) :varargs-p BOOL
:f64-count N)' where each PARSED-NODE is the same value IR plus
a `:cls' (= `gp' or `f64') tag and a `:varargs-p' flag for emit
to inspect when fixing register placement + the SysV AMD64 AL
register before the call instruction.  f64 args are validated
against the xmm register budget (= 8).  GP args may exceed the
six-register budget on x86_64 SysV, where the emitter can place
trivial seventh-and-later args on the call stack; other ABIs still
raise `:extern-call-too-many-gp-args'.

Per-class budget is total across the fixed + varargs sets — SysV
AMD64 still passes variadic args through the same register pool
as fixed args, just with the addition of the AL count.

OP is recorded for diagnostics; the caller's `:ret-class' flag
already distinguishes `extern-call' (i64 return) from
`extern-call-f64' (f64 return)."
  (let ((fixed-args nil)
        (varargs nil)
        (varargs-p nil))
    ;; Split raw-args into fixed + optional trailing (:varargs ...).
    (dolist (a raw-args)
      (cond
       ((and (consp a) (eq (car a) :varargs))
        (when varargs-p
          (signal 'nelisp-phase47-compiler-error
                  (list :extern-call-multiple-varargs name op)))
        (setq varargs-p t
              varargs (cdr a)))
       (varargs-p
        ;; Anything after the (:varargs ...) form is an error.
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-trailing-after-varargs name a)))
       (t
        (push a fixed-args))))
    (setq fixed-args (nreverse fixed-args))
    (let* ((parse-one
            (lambda (form is-varargs)
              (let* ((cls-expr
                      (cond
                       ;; (:i64 EXPR) / (:f64 EXPR) plist form.
                       ((and (consp form)
                             (memq (car form) '(:i64 :f64))
                             (= (length form) 2))
                        (cons (if (eq (car form) :f64) 'f64 'gp)
                              (cadr form)))
                       ;; (:i64) / (:f64) without an expression is bad.
                       ((and (consp form)
                             (memq (car form) '(:i64 :f64)))
                        (signal 'nelisp-phase47-compiler-error
                                (list :extern-call-bad-arg-shape
                                      name form)))
                       ;; Bare value expr → default i64.
                       (t (cons 'gp form))))
                     (cls (car cls-expr))
                     (expr (cdr cls-expr))
                     (parsed (nelisp-phase47-compiler--parse-value
                              expr env fenv defuns)))
                ;; A33.4 — PARSED is now the flat key-value vector built by
                ;; `--make-ir', so extend it with `vconcat' (not `append',
                ;; which would coerce the vector back to a list and move the
                ;; kind tag off slot 0).  The result is still a vector with
                ;; the kind at slot 0 and `:cls' / `:varargs-p' added as two
                ;; more key-value pairs, readable via `--ir-get'.
                (vconcat parsed
                         (vector :cls cls :varargs-p is-varargs)))))
           (parsed-fixed (mapcar (lambda (a) (funcall parse-one a nil))
                                 fixed-args))
           (parsed-var (mapcar (lambda (a) (funcall parse-one a t))
                               varargs))
           (all-args (append parsed-fixed parsed-var))
           (gp-args (cl-remove-if-not
                     (lambda (n) (eq (nelisp-phase47-compiler--ir-get n :cls) 'gp))
                     all-args))
           (f64-args (cl-remove-if-not
                      (lambda (n) (eq (nelisp-phase47-compiler--ir-get n :cls) 'f64))
                      all-args)))
      (when (and (> (length gp-args)
                    (length (nelisp-phase47-compiler--current-arg-regs)))
                 (not (and (eq nelisp-phase47-compiler--arch 'x86_64)
                           (eq nelisp-phase47-compiler--abi 'sysv))))
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-too-many-gp-args name
                      (length gp-args))))
      (when (> (length f64-args)
               (length nelisp-phase47-compiler--xmm-arg-regs))
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-too-many-f64-args name
                      (length f64-args))))
      (list :args all-args
            :varargs-p varargs-p
            :f64-count (length f64-args)))))

(defsubst nelisp-phase47-compiler--make-ir (kind &rest plist)
  "Construct an IR node tagged KIND carrying PLIST.
A33.4 — the internal representation is now a flat key-value vector
`[KIND K1 V1 K2 V2 ...]' (slot 0 = the `:kind' symbol, then the PLIST
keys/values copied in their construction order) instead of the
A33.1-A33.3 symbol-keyed list `(:kind KIND . PLIST)'.  The construction
order of PLIST is consistent per kind across every call site (verified
in A33.4), so each (kind, field) pair lands at a fixed vector offset —
which is what the A33.N native-emit path needs: it reads slot 0 with
`vector-ref' to dispatch and reads each field at its kind-fixed offset
without a heap-walking `plist-get'.  Because all 90 constructors and all
247 reads were already funnelled through `--make-ir' / `--ir-kind' /
`--ir-get' (A33.1-A33.3), swapping list -> vector here is invisible to
the rest of the compiler and, since the emit path computes identical
bytes either way, the produced `.o' objects are byte-identical."
  (apply #'vector kind plist))

(defsubst nelisp-phase47-compiler--ir-kind (node)
  "Read the :kind tag of IR NODE.
A33.4 — NODE is now the flat key-value vector built by `--make-ir', so
the kind tag lives at slot 0 and this is a single `aref' (the future
A33.N native dispatch reads the same slot with `vector-ref').  Reads
were centralized here in A33.2, so the list -> vector swap touches only
this body; the emitted `.o' bytes are unchanged."
  (aref node 0))

(defsubst nelisp-phase47-compiler--ir-get (node field)
  "Read FIELD of IR NODE.
A33.4 — NODE is the flat key-value vector `[KIND K1 V1 K2 V2 ...]', so
the lookup scans the key slots (1, 3, 5, ...) for FIELD and returns the
following value slot.  This mirrors the previous `plist-get' linear scan
(same O(N) over the same key set), so behaviour and the emitted `.o'
bytes are unchanged; the A33.N native path can instead read each field
at its kind-fixed offset since per-kind layout is constant."
  (let ((len (length node))
        (i 1)
        (result nil))
    (while (< i len)
      (when (eq (aref node i) field)
        (setq result (aref node (1+ i))
              i len))
      (setq i (+ i 2)))
    result))

(defconst nelisp-phase47-compiler--ir-kind-tags
  '((alloc-bytes . 0)
    (arith . 1)
    (aot-root-scope . 92)
    (atomic-compare-exchange . 2)
    (atomic-fetch-add . 3)
    (bits-to-f64 . 4)
    (call . 5)
    (cell-make . 6)
    (cell-null-p . 7)
    (cell-set-value . 8)
    (cell-value . 9)
    (cmp . 10)
    (cond . 11)
    (cons-car . 12)
    (cons-cdr . 13)
    (cons-cdr-raw . 14)
    (cons-make . 15)
    (cons-make-with-clone . 16)
    (cons-null-p . 17)
    (cons-set-car . 18)
    (cons-set-cdr . 19)
    (dealloc-bytes . 20)
    (defun . 21)
    (exit . 22)
    (extern-call . 23)
    (f64-binop . 24)
    (f64-call . 25)
    (f64-cmp . 26)
    (f64-to-i64-trunc . 27)
    (i64-to-f64 . 28)
    (if . 29)
    (imm . 30)
    (let . 31)
    (let-rt . 32)
    (let-rt-n . 89)
    (logic . 33)
    (mut-str-finalize . 34)
    (mut-str-len . 35)
    (mut-str-make-empty . 36)
    (mut-str-push-byte . 37)
    (mut-str-push-codepoint . 38)
    (ptr-read-u16 . 39)
    (ptr-read-u32 . 40)
    (ptr-read-u64 . 41)
    (ptr-read-u8 . 42)
    (ptr-write-u16 . 43)
    (ptr-write-u32 . 44)
    (ptr-write-u64 . 45)
    (ptr-write-u8 . 46)
    (record-make . 47)
    (record-slot-count . 48)
    (record-slot-ref . 49)
    (record-slot-ref-ptr . 50)
    (record-slot-set . 51)
    (record-type-tag . 52)
    (ref . 53)
    (seq . 54)
    (sexp-float-unwrap . 55)
    (sexp-int-make . 56)
    (sexp-int-unwrap . 57)
    (sexp-name-eq . 58)
    (sexp-payload-ptr . 59)
    (sexp-payload-ptr-record . 60)
    (sexp-tag . 61)
    (sexp-write-float . 62)
    (sexp-write-nil . 63)
    (sexp-write-str . 64)
    (sexp-write-str-lit . 91)
    (sexp-write-symbol . 65)
    (sexp-write-symbol-lit . 90)
    (sexp-write-t . 66)
    (shift . 67)
    (str-byte-at . 68)
    (str-bytes . 69)
    (str-bytes-ptr . 70)
    (str-char-count . 71)
    (str-codepoint-at . 72)
    (str-eq . 73)
    (str-is-alphanumeric-at . 74)
    (str-len . 75)
    (symbol-eq . 76)
    (symbol-name-eq . 77)
    (syscall-direct . 78)
    (table-define . 79)
    (table-lookup . 80)
    (value-seq . 88)
    (vector-len . 81)
    (vector-make . 82)
    (vector-ref . 83)
    (vector-ref-ptr . 84)
    (vector-slot-set . 85)
    (while . 86)
    (write . 87))
  "Maps each IR :kind symbol to a small integer tag.
A33.2 defines this table only (no behavior change); A33.3 uses it to
turn the symbol-keyed `pcase'/`cond' emit dispatch into integer
dispatch.  The entries cover every kind produced by
`nelisp-phase47-compiler--make-ir'.  Tag values are stable; later
extensions append new tags instead of renumbering existing values.")

(defsubst nelisp-phase47-compiler--ir-kind-tag (node)
  "Return the small-integer tag for IR NODE's `:kind' (A33.2 table).
A33.3 — looks the `:kind' symbol up in `--ir-kind-tags' so the emit
dispatch sites can compare integers (`(= tag N)') instead of symbols.
This is a behaviour-preserving change: every dispatched kind maps to a
distinct stable tag, so the emitted `.o' bytes are unchanged."
  (cdr (assq (nelisp-phase47-compiler--ir-kind node)
             nelisp-phase47-compiler--ir-kind-tags)))

(defsubst nelisp-phase47-compiler--ir-node-p (value)
  "Return non-nil when VALUE is a Phase 47 IR node vector."
  (and (vectorp value)
       (> (length value) 0)
       (symbolp (aref value 0))
       (assq (aref value 0) nelisp-phase47-compiler--ir-kind-tags)))

(defun nelisp-phase47-compiler--walk-ir (ir fn)
  "Walk IR and nested IR children, calling FN for each node."
  (cl-labels
      ((walk-value
        (value)
        (cond
         ((nelisp-phase47-compiler--ir-node-p value)
          (funcall fn value)
          (cl-loop for i from 1 below (length value)
                   do (walk-value (aref value i))))
         ((consp value)
          (walk-value (car value))
          (walk-value (cdr value)))
         ((vectorp value)
          (cl-loop for i from 0 below (length value)
                   do (walk-value (aref value i)))))))
    (walk-value ir)))

(defconst nelisp-phase47-compiler--allocating-kinds
  '(alloc-bytes
    cell-make
    cons-make
    cons-make-with-clone
    mut-str-make-empty
    record-make
    sexp-write-str
    sexp-write-str-lit
    sexp-write-symbol-lit
    sexp-write-symbol
    vector-make)
  "IR kinds that allocate heap or heap-like storage.")

(defun nelisp-phase47-compiler--ir-contains-allocation-p (ir)
  "Return non-nil when IR contains an allocation-producing node."
  (let ((found nil))
    (nelisp-phase47-compiler--walk-ir
     ir
     (lambda (node)
       (when (memq (nelisp-phase47-compiler--ir-kind node)
                   nelisp-phase47-compiler--allocating-kinds)
         (setq found t))))
    found))

(defun nelisp-phase47-compiler--gc-root-slots-for-defun (body-ir)
  "Return conservative static GC root slots for a defun BODY-IR.
Stage 129.5A only records explicitly annotated Sexp GP refs and only
when the body contains an allocation site.  Runtime registration of
these slots is a later 129.5 step."
  (when (nelisp-phase47-compiler--ir-contains-allocation-p body-ir)
    (let ((slots nil))
      (nelisp-phase47-compiler--walk-ir
       body-ir
       (lambda (node)
         (when (and (eq (nelisp-phase47-compiler--ir-kind node) 'ref)
                    (eq (nelisp-phase47-compiler--ir-get node :class) 'gp)
                    (nelisp-phase47-compiler--ir-get node :root-p))
           (push (nelisp-phase47-compiler--ir-get node :slot) slots))))
      (sort (delete-dups slots) #'<))))

(defun nelisp-phase47-compiler--gc-root-descriptor-for-defun (defun-ir)
  "Return the call-boundary GC root descriptor for DEFUN-IR, or nil.
The descriptor is the compiler-side handoff for Doc 99 / 129.5C:
`:slots' names the frame slots that must be copied into a runtime AOT
root vector while the compiled function is active."
  (when (and (nelisp-phase47-compiler--ir-node-p defun-ir)
             (eq (nelisp-phase47-compiler--ir-kind defun-ir) 'defun))
    (let ((slots (nelisp-phase47-compiler--ir-get defun-ir :gc-root-slots)))
      (when slots
        (list :name (nelisp-phase47-compiler--ir-get defun-ir :name)
              :slots slots
              :param-count
              (length (nelisp-phase47-compiler--ir-get defun-ir :params))
              :rt-slot-count
              (nelisp-phase47-compiler--ir-get defun-ir :rt-slot-count))))))

(defun nelisp-phase47-compiler--gc-root-descriptors (ir)
  "Return all non-empty GC root descriptors found in top-level IR."
  (let ((descriptors nil))
    (nelisp-phase47-compiler--walk-ir
     ir
     (lambda (node)
       (let ((descriptor
              (nelisp-phase47-compiler--gc-root-descriptor-for-defun node)))
         (when descriptor
           (push descriptor descriptors)))))
    (nreverse descriptors)))

(defun nelisp-phase47-compiler--auto-aot-root-boundary-slots (fenv)
  "Return Doc 129.5F boundary slots from FENV, or nil.

The automatic root-frame MVP is intentionally opt-in: a defun must
already expose the Doc 99 / 129.5 boundary locals `out', `mirror',
`frames', `scratch', and `roots'.  `roots' is the frame slot that
will hold the materialized AOT root vector."
  (let ((slots nil)
        (ok t))
    (dolist (sym '(out mirror frames scratch roots))
      (let ((info (cdr (assq sym fenv))))
        (if (and info
                 (eq (or (plist-get info :class) 'gp) 'gp)
                 (integerp (plist-get info :slot)))
            (push (cons sym (plist-get info :slot)) slots)
          (setq ok nil))))
    (when ok
      (nreverse slots))))

(defun nelisp-phase47-compiler--symbols-for-root-slots (fenv root-slots)
  "Return FENV symbols corresponding to ROOT-SLOTS."
  (mapcar
   (lambda (slot)
     (let ((entry
            (cl-find-if
             (lambda (pair)
               (= (or (plist-get (cdr pair) :slot) -1) slot))
             fenv)))
       (unless entry
         (signal 'nelisp-phase47-compiler-error
                 (list :aot-root-slot-symbol-missing
                       slot :root-slots root-slots)))
       (car entry)))
   root-slots))

(defun nelisp-phase47-compiler--maybe-wrap-aot-root-scope
    (body-ir gc-root-slots env fenv defuns)
  "Wrap BODY-IR in automatic Doc 129.5F root push/pop when possible."
  (let* ((boundary-slots
          (and gc-root-slots
               (nelisp-phase47-compiler--auto-aot-root-boundary-slots fenv)))
         (root-symbols
          (and boundary-slots
               (nelisp-phase47-compiler--symbols-for-root-slots
                fenv gc-root-slots))))
    (if boundary-slots
        (nelisp-phase47-compiler--make-ir
              'aot-root-scope
              :root-slots gc-root-slots
              :root-symbols root-symbols
              :boundary-slots boundary-slots
              :materialize-ir
              (nelisp-phase47-compiler--parse-value
               `(extern-call nelisp_aot_materialize_roots
                             mirror frames ,(length root-symbols)
                             out scratch ,@root-symbols)
               env fenv defuns)
              :push-ir
              (nelisp-phase47-compiler--parse-value
               '(extern-call nelisp_aot_push_roots
                             mirror frames roots out scratch)
               env fenv defuns)
              :body body-ir
              :pop-ir
              (nelisp-phase47-compiler--parse-value
               '(extern-call nelisp_aot_pop_roots
                             mirror frames roots out scratch)
               env fenv defuns))
      body-ir)))

(defun nelisp-phase47-compiler--parse-let-var (var-form)
  "Return `(VAR ROOT-P)' for a Phase 47 `let' VAR-FORM."
  (cond
   ((symbolp var-form)
    (list var-form nil))
   ((and (consp var-form)
         (= (length var-form) 3)
         (symbolp (car var-form))
         (eq (nth 1 var-form) :type)
         (eq (nth 2 var-form) 'sexp))
    (list (car var-form) t))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :let-var-shape var-form)))))

(defun nelisp-phase47-compiler--validate-let-binding (binding)
  "Return `(VAR VALUE ROOT-P)' for one Phase 47 `let' BINDING."
  (unless (and (consp binding)
               (consp (cdr binding))
               (null (cddr binding)))
    (signal 'nelisp-phase47-compiler-error
            (list :let-binding-shape binding)))
  (let ((parsed-var (nelisp-phase47-compiler--parse-let-var (car binding))))
    (list (car parsed-var) (cadr binding) (cadr parsed-var))))

(defun nelisp-phase47-compiler--check-let-vars-unique (bindings)
  "Signal when BINDINGS contains duplicate variables."
  (let ((seen nil))
    (dolist (binding bindings)
      (let ((var (car (nelisp-phase47-compiler--validate-let-binding binding))))
        (when (memq var seen)
          (signal 'nelisp-phase47-compiler-error
                  (list :let-duplicate-var var)))
        (push var seen)))))

(defun nelisp-phase47-compiler--check-let-var-lexical (var)
  "Signal when VAR is known to require special binding semantics."
  (when (memq var nelisp-phase47-compiler--special-vars)
    (signal 'nelisp-phase47-compiler-error
            (list :let-special-binding-pending var))))

(defun nelisp-phase47-compiler--special-var-p (var)
  "Return non-nil when VAR has top-level special binding semantics."
  (memq var nelisp-phase47-compiler--special-vars))

(defun nelisp-phase47-compiler--check-let-vars-lexical (bindings)
  "Signal when BINDINGS contain a known special variable."
  (dolist (binding bindings)
    (let ((var (car (nelisp-phase47-compiler--validate-let-binding binding))))
      (nelisp-phase47-compiler--check-let-var-lexical var))))

(defun nelisp-phase47-compiler--parse-multi-let
    (bindings body-sexp env fenv defuns parse-body-fn)
  "Parse a multi-binding `let' in value or statement context.
BINDINGS are evaluated in the original ENV/FENV, matching ordinary
parallel `let' semantics.  Compile-time-foldable bindings extend ENV
only for BODY.  Runtime bindings allocate frame slots together and
extend FENV only for BODY.  PARSE-BODY-FN is either
`nelisp-phase47-compiler--parse-value' or `--parse-stmt'."
  (unless (consp bindings)
    (signal 'nelisp-phase47-compiler-error
            (list :let-empty-bindings bindings)))
  (nelisp-phase47-compiler--check-let-vars-unique bindings)
  (nelisp-phase47-compiler--check-let-vars-lexical bindings)
  (let ((new-env env)
        (new-fenv fenv)
        (rt-bindings nil))
    (dolist (binding bindings)
      (let* ((pair (nelisp-phase47-compiler--validate-let-binding binding))
             (var (nth 0 pair))
             (val-sexp (nth 1 pair))
             (root-p (nth 2 pair)))
        (if (nelisp-phase47-compiler--int-foldable-p val-sexp env fenv)
            (let ((val (nelisp-phase47-compiler--fold-int val-sexp env)))
              (push (cons var val) new-env))
          (unless nelisp-phase47-compiler--next-rt-let-slot
            (signal 'nelisp-phase47-compiler-error
                    (list :let-rt-requires-defun-context var)))
          (let* ((slot (car nelisp-phase47-compiler--next-rt-let-slot))
                 (_ (setcar nelisp-phase47-compiler--next-rt-let-slot
                            (1+ slot)))
                 (val-ir (nelisp-phase47-compiler--parse-value
                          val-sexp env fenv defuns)))
            (push (list var slot val-ir root-p) rt-bindings)
            (push (cons var (list :slot slot :class 'gp :root-p root-p))
                  new-fenv)))))
    (let ((body-ir (funcall parse-body-fn body-sexp new-env new-fenv defuns)))
      (if rt-bindings
          (nelisp-phase47-compiler--make-ir 'let-rt-n
                :bindings (nreverse rt-bindings)
                :body body-ir)
        body-ir))))

;; ---- Doc 129.1 frontend macro expansion ----
;;
;; Phase 47 parses a DSL, not host elisp.  Running `macroexpand-all'
;; over a whole program would rewrite DSL `defun' into host `defalias',
;; so macro expansion is selective: preserve Phase 47 structural forms,
;; expand user macros in value positions, then normalize the small host
;; core forms (`progn', 3-arg `if', `nil') that common macros produce.

(defun nelisp-phase47-compiler--top-level-defmacro-p (form)
  "Return non-nil when FORM is a top-level `defmacro' form."
  (and (consp form)
       (eq (car form) 'defmacro)
       (symbolp (nth 1 form))
       (listp (nth 2 form))
       (>= (length form) 4)))

(defun nelisp-phase47-compiler--top-level-module-form-p (form)
  "Return non-nil when FORM is a compile-time top-level module form."
  (and (consp form)
       (memq (car form) '(require provide))))

(defun nelisp-phase47-compiler--top-level-var-form-p (form)
  "Return non-nil when FORM is a top-level variable declaration form."
  (and (consp form)
       (memq (car form) '(defvar defconst defcustom))))

(defun nelisp-phase47-compiler--top-level-special-var-name (form)
  "Return FORM's top-level special variable name."
  (when (nelisp-phase47-compiler--top-level-var-form-p form)
    (let ((name (nth 1 form)))
      (unless (symbolp name)
        (signal 'nelisp-phase47-compiler-error
                (list :top-level-var-name-not-symbol form)))
      name)))

(defun nelisp-phase47-compiler--link-name-fragment (symbol)
  "Return a deterministic linker-friendly fragment for SYMBOL."
  (let* ((raw (symbol-name symbol))
         (frag (replace-regexp-in-string "[^A-Za-z0-9_]" "_" raw)))
    (if (string-empty-p frag) "sym" frag)))

(defun nelisp-phase47-compiler--top-level-var-helper-name (kind symbol index)
  "Return the generated AOT init helper name for KIND/SYMBOL at INDEX."
  (intern (format "nelisp_aot_%s_%d_%s"
                  (substring (symbol-name kind) 3)
                  index
                  (nelisp-phase47-compiler--link-name-fragment symbol))))

(defun nelisp-phase47-compiler--defcustom-metadata-descriptor (form index)
  "Return the custom metadata descriptor for top-level defcustom FORM.
INDEX must match the top-level variable declaration index used for the
generated AOT init helper."
  (unless (and (consp form) (eq (car form) 'defcustom))
    (signal 'nelisp-phase47-compiler-error
            (list :not-defcustom-form form)))
  (unless (>= (length form) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :defcustom-arity form)))
  (let ((name (nth 1 form))
        (options (nthcdr 4 form)))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :top-level-var-name-not-symbol form)))
    (unless (stringp (nth 3 form))
      (signal 'nelisp-phase47-compiler-error
              (list :defcustom-docstring-not-string form)))
    (unless (zerop (% (length options) 2))
      (signal 'nelisp-phase47-compiler-error
              (list :defcustom-keyword-value-shape form)))
    (let ((rest options))
      (while rest
        (unless (keywordp (car rest))
          (signal 'nelisp-phase47-compiler-error
                  (list :defcustom-key-not-keyword (car rest))))
        (setq rest (cddr rest))))
    (list :name name
          :helper (nelisp-phase47-compiler--top-level-var-helper-name
                   'defcustom name index)
          :standard (nth 2 form)
          :docstring (nth 3 form)
          :options options)))

(defun nelisp-phase47-compiler--top-level-var-init-descriptor (form index)
  "Return the AOT init descriptor for top-level variable FORM.
Returns nil for declaration-only `defvar' forms with no initializer."
  (let ((kind (car form))
        (name (nth 1 form)))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :top-level-var-name-not-symbol form)))
    (pcase kind
      ('defvar
       (unless (<= 2 (length form) 4)
         (signal 'nelisp-phase47-compiler-error
                 (list :defvar-arity form)))
       (when (= (length form) 4)
         (unless (stringp (nth 3 form))
           (signal 'nelisp-phase47-compiler-error
                   (list :defvar-docstring-not-string form))))
       (when (>= (length form) 3)
         (list :kind 'defvar
               :name name
               :helper (nelisp-phase47-compiler--top-level-var-helper-name
                        kind name index)
               :index index)))
      ('defconst
       (unless (<= 3 (length form) 4)
         (signal 'nelisp-phase47-compiler-error
                 (list :defconst-arity form)))
       (when (= (length form) 4)
         (unless (stringp (nth 3 form))
           (signal 'nelisp-phase47-compiler-error
                   (list :defconst-docstring-not-string form))))
       (list :kind 'defconst
             :name name
             :helper (nelisp-phase47-compiler--top-level-var-helper-name
                      kind name index)
             :index index))
      ('defcustom
       (nelisp-phase47-compiler--defcustom-metadata-descriptor form index)
       (list :kind 'defcustom
             :name name
             :helper (nelisp-phase47-compiler--top-level-var-helper-name
                      kind name index)
             :index index))
      (_
       (signal 'nelisp-phase47-compiler-error
               (list :unknown-top-level-var-form form))))))

(defun nelisp-phase47-compiler--lower-top-level-var-form (form index)
  "Lower top-level FORM into an explicit AOT init helper.
Returns nil for declaration-only `defvar' forms with no initializer."
  (let ((kind (car form))
        (name (nth 1 form)))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :top-level-var-name-not-symbol form)))
    (pcase kind
      ('defvar
       (unless (<= 2 (length form) 4)
         (signal 'nelisp-phase47-compiler-error
                 (list :defvar-arity form)))
       (when (= (length form) 4)
         (unless (stringp (nth 3 form))
           (signal 'nelisp-phase47-compiler-error
                   (list :defvar-docstring-not-string form))))
       (when (>= (length form) 3)
         `(defun-sexp-int-defvar-symbol
              ,(nelisp-phase47-compiler--top-level-var-helper-name kind name index)
              ,name
              (out mirror frames scratch name_slot)
            ,(nth 2 form))))
      ('defconst
       (unless (<= 3 (length form) 4)
         (signal 'nelisp-phase47-compiler-error
                 (list :defconst-arity form)))
       (when (= (length form) 4)
         (unless (stringp (nth 3 form))
           (signal 'nelisp-phase47-compiler-error
                   (list :defconst-docstring-not-string form))))
       `(defun-sexp-int-defconst-symbol
            ,(nelisp-phase47-compiler--top-level-var-helper-name kind name index)
            ,name
            (out mirror frames scratch name_slot)
          ,(nth 2 form)))
      ('defcustom
       (nelisp-phase47-compiler--defcustom-metadata-descriptor form index)
       `(defun-sexp-int-defvar-symbol
            ,(nelisp-phase47-compiler--top-level-var-helper-name kind name index)
            ,name
            (out mirror frames scratch name_slot)
          ,(nth 2 form)))
      (_
       (signal 'nelisp-phase47-compiler-error
               (list :unknown-top-level-var-form form))))))

(defun nelisp-phase47-compiler--literal-arg (form)
  "Return FORM's literal value for compile-time top-level forms."
  (if (and (consp form)
           (eq (car form) 'quote)
           (= (length form) 2))
      (cadr form)
    form))

(defun nelisp-phase47-compiler--eval-top-level-module-form (form)
  "Evaluate one compile-time top-level module FORM.
`require' is run during compilation so macros and helper definitions
needed by later forms can be loaded.  `provide' is only stripped: the
compiled object does not yet carry module registration side effects."
  (pcase (car form)
    ('require
     (unless (<= 2 (length form) 4)
       (signal 'nelisp-phase47-compiler-error
               (list :require-arity form)))
     (let ((feature (nelisp-phase47-compiler--literal-arg (nth 1 form)))
           (filename (when (>= (length form) 3)
                       (nelisp-phase47-compiler--literal-arg (nth 2 form))))
           (noerror (when (>= (length form) 4)
                      (nelisp-phase47-compiler--literal-arg (nth 3 form)))))
       (unless (symbolp feature)
         (signal 'nelisp-phase47-compiler-error
                 (list :require-feature-not-symbol feature)))
       (unless (or (null filename) (stringp filename))
         (signal 'nelisp-phase47-compiler-error
                 (list :require-filename-not-string filename)))
       (require feature filename noerror)))
    ('provide
     (unless (<= 2 (length form) 3)
       (signal 'nelisp-phase47-compiler-error
               (list :provide-arity form)))
     (let ((feature (nelisp-phase47-compiler--literal-arg (nth 1 form))))
       (unless (symbolp feature)
         (signal 'nelisp-phase47-compiler-error
                 (list :provide-feature-not-symbol feature)))))
    (_
     (signal 'nelisp-phase47-compiler-error
             (list :unknown-module-form form)))))

(defun nelisp-phase47-compiler--extract-defmacros (sexp)
  "Return plist for compile-time top-level forms in SEXP.
Only a top-level `defmacro', or `defmacro' children directly inside
`seq', are treated as compile-time macro definitions.  Top-level
`require' and `provide' are also compile-time-only forms.  Top-level
`defvar' / `defconst' / `defcustom' with integer-compatible
initializers lower to generated AOT init helpers so object output can
expose a callable runtime entry for those declarations.  Top-level
`defcustom' forms also produce custom metadata descriptors keyed to
the same generated helper names."
  (cond
   ((nelisp-phase47-compiler--top-level-defmacro-p sexp)
    (list :source '(seq)
          :defmacros (list sexp)
          :module-forms nil
          :init-helpers nil
          :custom-metadata nil
          :special-vars nil))
   ((nelisp-phase47-compiler--top-level-module-form-p sexp)
    (list :source '(seq)
          :defmacros nil
          :module-forms (list sexp)
          :init-helpers nil
          :custom-metadata nil
          :special-vars nil))
   ((nelisp-phase47-compiler--top-level-var-form-p sexp)
    (let ((descriptor
           (nelisp-phase47-compiler--top-level-var-init-descriptor sexp 0))
          (lowered (nelisp-phase47-compiler--lower-top-level-var-form sexp 0))
          (custom-metadata
           (when (eq (car sexp) 'defcustom)
             (list (nelisp-phase47-compiler--defcustom-metadata-descriptor
                    sexp 0)))))
      (list :source (or lowered '(seq))
            :defmacros nil
            :module-forms nil
            :init-helpers (when descriptor (list descriptor))
            :custom-metadata custom-metadata
            :special-vars
            (list (nelisp-phase47-compiler--top-level-special-var-name
                   sexp)))))
   ((and (consp sexp) (eq (car sexp) 'seq))
    (let ((kept nil)
          (defs nil)
          (module-forms nil)
          (init-helpers nil)
          (custom-metadata nil)
          (special-vars nil)
          (var-index 0))
      (dolist (child (cdr sexp))
        (cond
         ((nelisp-phase47-compiler--top-level-defmacro-p child)
          (push child defs))
         ((nelisp-phase47-compiler--top-level-module-form-p child)
          (push child module-forms))
         ((nelisp-phase47-compiler--top-level-var-form-p child)
          (push (nelisp-phase47-compiler--top-level-special-var-name child)
                special-vars)
          (let ((lowered
                 (nelisp-phase47-compiler--lower-top-level-var-form
                  child var-index))
                (descriptor
                 (nelisp-phase47-compiler--top-level-var-init-descriptor
                  child var-index)))
            (when descriptor
              (push descriptor init-helpers))
            (when (eq (car child) 'defcustom)
              (push (nelisp-phase47-compiler--defcustom-metadata-descriptor
                     child var-index)
                    custom-metadata))
            (setq var-index (1+ var-index))
            (when lowered
              (push lowered kept))))
         (t
          (push child kept))))
      (list :source (cons 'seq (nreverse kept))
            :defmacros (nreverse defs)
            :module-forms (nreverse module-forms)
            :init-helpers (nreverse init-helpers)
            :custom-metadata (nreverse custom-metadata)
            :special-vars (nreverse special-vars))))
   (t
    (list :source sexp
          :defmacros nil
          :module-forms nil
          :init-helpers nil
          :custom-metadata nil
          :special-vars nil))))

(defun nelisp-phase47-compiler--init-helper-descriptors (sexp)
  "Return top-level variable AOT init helper descriptors in SEXP.
Declaration-only `defvar' forms are omitted because they do not emit a
callable helper."
  (plist-get (nelisp-phase47-compiler--extract-defmacros sexp)
             :init-helpers))

(defun nelisp-phase47-compiler--custom-metadata-descriptors (sexp)
  "Return top-level `defcustom' metadata descriptors in SEXP.
This is the compiler-side handoff for Doc 99 scheduling: each
descriptor records the variable name, generated init helper, standard
value expression, docstring, and customization keyword plist."
  (plist-get (nelisp-phase47-compiler--extract-defmacros sexp)
             :custom-metadata))

(defun nelisp-phase47-compiler--with-defmacros (defs thunk)
  "Temporarily install DEFS while calling THUNK.
Existing function cells are restored afterwards so compile-time user
macros do not leak into the host Emacs session."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (def defs)
            (let ((name (nth 1 def)))
              (push (list name (fboundp name)
                          (when (fboundp name) (symbol-function name)))
                    saved)
              (eval def t)))
          (funcall thunk))
      (dolist (entry saved)
        (let ((name (nth 0 entry))
              (had-binding (nth 1 entry))
              (old-fn (nth 2 entry)))
          (if had-binding
              (fset name old-fn)
            (fmakunbound name)))))))

(defvar nelisp-phase47-compiler--lambda-lift-counter 0
  "Counter for Doc 129.7K synthetic non-capturing lambda defuns.")

(defvar nelisp-phase47-compiler--lambda-lift-hoists nil
  "Dynamically collected Doc 129.7K synthetic defun forms.")

(defvar nelisp-phase47-compiler--lambda-lift-names nil
  "Top-level and synthetic names reserved during Doc 129.7K lambda lifting.")

(defvar nelisp-phase47-compiler--special-vars nil
  "Top-level special variables declared in the current Doc 129 compile unit.")

(defun nelisp-phase47-compiler--top-level-defun-names (sexp)
  "Return top-level defun names in SEXP."
  (let (names)
    (cond
     ((and (consp sexp) (eq (car sexp) 'defun) (symbolp (nth 1 sexp)))
      (push (nth 1 sexp) names))
     ((and (consp sexp) (eq (car sexp) 'seq))
      (dolist (child (cdr sexp))
        (when (and (consp child)
                   (eq (car child) 'defun)
                   (symbolp (nth 1 child)))
          (push (nth 1 child) names)))))
    names))

(defun nelisp-phase47-compiler--lambda-literal-form (form)
  "Return FORM's literal lambda form, or nil."
  (cond
   ((and (consp form) (eq (car form) 'lambda))
    form)
   ((and (consp form)
         (eq (car form) 'function)
         (= (length form) 2)
         (consp (cadr form))
         (eq (caadr form) 'lambda))
    (cadr form))
   (t nil)))

(defun nelisp-phase47-compiler--lambda-lift-name ()
  "Return a fresh Doc 129.7K synthetic defun name."
  (let (name)
    (while
        (progn
          (setq name
                (intern (format "nelisp_aot_lambda_%d"
                                nelisp-phase47-compiler--lambda-lift-counter)))
          (setq nelisp-phase47-compiler--lambda-lift-counter
                (1+ nelisp-phase47-compiler--lambda-lift-counter))
          (memq name nelisp-phase47-compiler--lambda-lift-names)))
    (push name nelisp-phase47-compiler--lambda-lift-names)
    name))

(defun nelisp-phase47-compiler--lambda-lift-call (lambda-form arg-forms)
  "Return a direct call to a synthetic defun for LAMBDA-FORM and ARG-FORMS."
  (unless (and (>= (length lambda-form) 3)
               (listp (nth 1 lambda-form))
               (cl-every #'symbolp (nth 1 lambda-form)))
    (signal 'nelisp-phase47-compiler-error
            (list :lambda-lift-param-shape lambda-form)))
  (let* ((name (nelisp-phase47-compiler--lambda-lift-name))
         (params (nth 1 lambda-form))
         (body (nelisp-phase47-compiler--body->form
                (cddr lambda-form)))
         (args (mapcar #'nelisp-phase47-compiler--preprocess-source
                       arg-forms)))
    (push `(defun ,name ,params ,body)
          nelisp-phase47-compiler--lambda-lift-hoists)
    (cons name args)))

(defun nelisp-phase47-compiler--lambda-lift-designator (lambda-form)
  "Return a function designator for a synthetic defun from LAMBDA-FORM."
  (unless (and (>= (length lambda-form) 3)
               (listp (nth 1 lambda-form))
               (cl-every #'symbolp (nth 1 lambda-form)))
    (signal 'nelisp-phase47-compiler-error
            (list :lambda-lift-param-shape lambda-form)))
  (let* ((name (nelisp-phase47-compiler--lambda-lift-name))
         (params (nth 1 lambda-form))
         (body (nelisp-phase47-compiler--body->form
                (cddr lambda-form))))
    (push `(defun ,name ,params ,body)
          nelisp-phase47-compiler--lambda-lift-hoists)
    `(function ,name)))

(defun nelisp-phase47-compiler--preprocess-funcall-lambda (sexp)
  "Lambda-lift a literal lambda designator in `(funcall ...)'.
Only non-capturing lambdas are supported: lifted bodies are compiled as
ordinary top-level defuns, so any free outer variable remains a normal
Phase 47 free-symbol error."
  (let ((lambda-form (nelisp-phase47-compiler--lambda-literal-form
                      (nth 1 sexp))))
    (if (not lambda-form)
        (cons 'funcall
              (mapcar #'nelisp-phase47-compiler--preprocess-source
                      (cdr sexp)))
      (nelisp-phase47-compiler--lambda-lift-call
       lambda-form (nthcdr 2 sexp)))))

(defun nelisp-phase47-compiler--preprocess-map-lambda (sexp)
  "Lambda-lift a literal function argument in map-family SEXP."
  (let ((lambda-form (nelisp-phase47-compiler--lambda-literal-form
                      (nth 1 sexp))))
    (if (not lambda-form)
        (cons (car sexp)
              (mapcar #'nelisp-phase47-compiler--preprocess-source
                      (cdr sexp)))
      (cons (car sexp)
            (cons (nelisp-phase47-compiler--lambda-lift-designator
                   lambda-form)
                  (mapcar #'nelisp-phase47-compiler--preprocess-source
                          (nthcdr 2 sexp)))))))

(defun nelisp-phase47-compiler--body->form (body)
  "Normalize BODY forms into one Phase 47 form."
  (cond
   ((null body) 0)
   ((null (cdr body))
    (nelisp-phase47-compiler--preprocess-source (car body)))
   (t
    (nelisp-phase47-compiler--preprocess-source (cons 'progn body)))))

(defun nelisp-phase47-compiler--preprocess-let*-bindings (bindings body)
  "Desugar `let*' BINDINGS and BODY into nested Phase 47 `let' forms."
  (if (null bindings)
      (nelisp-phase47-compiler--body->form body)
    (let ((binding (car bindings)))
      (unless (and (consp binding)
                   (consp (cdr binding))
                   (null (cddr binding)))
        (signal 'nelisp-phase47-compiler-error
                (list :let-binding-shape binding)))
      `(let (,(list (car binding)
                    (nelisp-phase47-compiler--preprocess-source
                     (cadr binding))))
         ,(nelisp-phase47-compiler--preprocess-let*-bindings
           (cdr bindings) body)))))

(defun nelisp-phase47-compiler--preprocess-cond-clause (clause)
  "Normalize one `cond' CLAUSE for the Phase 47 parser."
  (unless (consp clause)
    (signal 'nelisp-phase47-compiler-error
            (list :cond-clause-not-list clause)))
  (let ((pred (car clause))
        (body (cdr clause)))
    (list (if (eq pred t)
              t
            (nelisp-phase47-compiler--preprocess-source pred))
          (nelisp-phase47-compiler--body->form body))))

(defun nelisp-phase47-compiler--preprocess-sexp-int-expr (expr sexp-vars)
  "Lower monomorphic Sexp::Int EXPR using SEXP-VARS as boxed inputs.
This is the Stage 129.2 boxed-boundary MVP: values stay raw i64
inside the existing Phase 47 compiler, while Sexp input parameters
are unwrapped at use sites and the caller-owned output slot is boxed
by the surrounding `defun-sexp-int' lowering."
  (cond
   ((integerp expr) expr)
   ((and (symbolp expr) (memq expr sexp-vars))
    `(sexp-int-unwrap ,expr))
   ((symbolp expr) expr)
   ((and (consp expr) (memq (car expr) '(+ - * logior logand logxor)))
    (unless (= (length expr) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-binop-arity expr)))
    (list (car expr)
          (nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 1 expr) sexp-vars)
          (nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 2 expr) sexp-vars)))
   ((and (consp expr) (memq (car expr) '(< > <= >= =)))
    (unless (= (length expr) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-cmp-arity expr)))
    (list (car expr)
          (nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 1 expr) sexp-vars)
          (nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 2 expr) sexp-vars)))
   ((and (consp expr) (eq (car expr) 'if))
    (unless (= (length expr) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-if-arity expr)))
    `(if ,(nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 1 expr) sexp-vars)
         ,(nelisp-phase47-compiler--preprocess-sexp-int-expr
           (nth 2 expr) sexp-vars)
       ,(nelisp-phase47-compiler--preprocess-sexp-int-expr
         (nth 3 expr) sexp-vars)))
   ((and (consp expr) (eq (car expr) 'seq))
    (cons 'seq
          (mapcar (lambda (child)
                    (nelisp-phase47-compiler--preprocess-sexp-int-expr
                     child sexp-vars))
                  (cdr expr))))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-unsupported expr)))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-int (sexp)
  "Lower `(defun-sexp-int NAME (OUT ARG...) BODY...)' to Phase 47 DSL.
OUT is a caller-owned `*mut Sexp' slot.  ARGs are `*const Sexp'
inputs assumed to hold Int values.  BODY is a monomorphic integer
expression; the lowered function writes `Sexp::Int(result)' into OUT
and returns OUT."
  (unless (>= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-arity sexp)))
  (let* ((name (nth 1 sexp))
         (params (nth 2 sexp))
         (body (nelisp-phase47-compiler--body->form (nthcdr 3 sexp))))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-name-not-symbol name)))
    (unless (and (consp params) (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-params params)))
    (let* ((out (car params))
           (sexp-vars (cdr params))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params))
           (raw-body
            (nelisp-phase47-compiler--preprocess-sexp-int-expr
             body sexp-vars)))
      `(defun ,name ,annotated-params
         (sexp-int-make ,out ,raw-body)))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-int-setq (sexp)
  "Lower `(defun-sexp-int-setq NAME PARAMS BODY...)' to Phase 47 DSL.
This is the Doc 129.3A boxed-boundary symbol-cell MVP.  PARAMS must be
`(OUT MIRROR FRAMES SCRATCH NAME ARG...)':

  OUT     — caller-owned `*mut Sexp' result slot
  MIRROR  — `Env::globals_record' pointer
  FRAMES  — `Env::frames_record' pointer
  SCRATCH — standard 11-slot set-value scratch vector pointer
  NAME    — `Sexp::Symbol' pointer naming the variable to set
  ARG...  — boxed `Sexp::Int' inputs available to BODY

BODY is compiled as a raw integer expression using ARG... as boxed
inputs.  The lowered function writes the boxed Int into OUT, delegates
the symbol-cell write to `nelisp_env_set_value', and returns OUT."
  (unless (>= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-setq-arity sexp)))
  (let* ((name (nth 1 sexp))
         (params (nth 2 sexp))
         (body (nelisp-phase47-compiler--body->form (nthcdr 3 sexp))))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-setq-name-not-symbol name)))
    (unless (and (listp params)
                 (>= (length params) 5)
                 (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-setq-params params)))
    (let* ((out (nth 0 params))
           (mirror (nth 1 params))
           (frames (nth 2 params))
           (scratch (nth 3 params))
           (sym (nth 4 params))
           (sexp-vars (nthcdr 5 params))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params))
           (raw-body
            (nelisp-phase47-compiler--preprocess-sexp-int-expr
             body sexp-vars)))
      `(defun ,name ,annotated-params
         (seq
          (sexp-int-make ,out ,raw-body)
          (extern-call nelisp_env_set_value
                       ,mirror ,frames ,sym ,out ,scratch 0)
          ,out)))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-int-setq-symbol (sexp)
  "Lower boxed Int setq with a compile-time symbol literal.
Surface form:

  (defun-sexp-int-setq-symbol NAME SYMBOL
      (OUT MIRROR FRAMES SCRATCH SYM-SLOT ARG...)
    BODY...)

SYM-SLOT is caller-owned `*mut Sexp' storage used to materialize
SYMBOL before delegating to `nelisp_env_set_value'."
  (unless (>= (length sexp) 5)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-setq-symbol-arity sexp)))
  (let* ((name (nth 1 sexp))
         (sym-lit (nth 2 sexp))
         (params (nth 3 sexp))
         (body (nelisp-phase47-compiler--body->form (nthcdr 4 sexp))))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-setq-symbol-name-not-symbol name)))
    (unless (or (symbolp sym-lit) (stringp sym-lit))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-setq-symbol-literal sym-lit)))
    (unless (and (listp params)
                 (>= (length params) 6)
                 (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-setq-symbol-params params)))
    (let* ((out (nth 0 params))
           (mirror (nth 1 params))
           (frames (nth 2 params))
           (scratch (nth 3 params))
           (sym-slot (nth 4 params))
           (sexp-vars (nthcdr 5 params))
           (symbol-name (if (symbolp sym-lit) (symbol-name sym-lit) sym-lit))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params))
           (raw-body
            (nelisp-phase47-compiler--preprocess-sexp-int-expr
             body sexp-vars)))
      `(defun ,name ,annotated-params
         (seq
          (sexp-write-symbol-lit ,sym-slot ,symbol-name)
          (sexp-int-make ,out ,raw-body)
          (extern-call nelisp_env_set_value
                       ,mirror ,frames ,sym-slot ,out ,scratch 0)
          ,out)))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-int-defvar-symbol (sexp)
  "Lower boxed Int defvar with a compile-time symbol literal.
Surface form:

  (defun-sexp-int-defvar-symbol NAME SYMBOL
      (OUT MIRROR FRAMES SCRATCH SYM-SLOT ARG...)
    BODY...)

SYM-SLOT is caller-owned `*mut Sexp' storage used to materialize
SYMBOL.  OUT must initially hold the runtime's unbound marker and is
therefore also passed to `nelisp_mirror_is_bound'.  If SYMBOL is
already bound, the lowered function returns SYM-SLOT without
overwriting the value cell.  Otherwise it boxes BODY into OUT,
delegates to `nelisp_env_set_value', and returns SYM-SLOT."
  (unless (>= (length sexp) 5)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-defvar-symbol-arity sexp)))
  (let* ((name (nth 1 sexp))
         (sym-lit (nth 2 sexp))
         (params (nth 3 sexp))
         (body (nelisp-phase47-compiler--body->form (nthcdr 4 sexp))))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defvar-symbol-name-not-symbol name)))
    (unless (or (symbolp sym-lit) (stringp sym-lit))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defvar-symbol-literal sym-lit)))
    (unless (and (listp params)
                 (>= (length params) 5)
                 (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defvar-symbol-params params)))
    (let* ((out (nth 0 params))
           (mirror (nth 1 params))
           (frames (nth 2 params))
           (scratch (nth 3 params))
           (sym-slot (nth 4 params))
           (sexp-vars (nthcdr 5 params))
           (symbol-name (if (symbolp sym-lit) (symbol-name sym-lit) sym-lit))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params))
           (raw-body
            (nelisp-phase47-compiler--preprocess-sexp-int-expr
             body sexp-vars)))
      `(defun ,name ,annotated-params
         (seq
          (sexp-write-symbol-lit ,sym-slot ,symbol-name)
          (if (= (extern-call nelisp_mirror_is_bound
                              ,mirror ,sym-slot ,out)
                 1)
              ,sym-slot
            (seq
             (sexp-int-make ,out ,raw-body)
             (extern-call nelisp_env_set_value
                          ,mirror ,frames ,sym-slot ,out ,scratch 0)
             ,sym-slot)))))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-int-defconst-symbol (sexp)
  "Lower boxed Int defconst with a compile-time symbol literal.
Surface form:

  (defun-sexp-int-defconst-symbol NAME SYMBOL
      (OUT MIRROR FRAMES SCRATCH SYM-SLOT ARG...)
    BODY...)

SYM-SLOT is caller-owned `*mut Sexp' storage used to materialize
SYMBOL.  OUT is reused as a transient flag/value slot: Nil clears an
existing constant flag, BODY is boxed into OUT for `nelisp_env_set_value',
then T is written into OUT before `nelisp_mirror_set_constant' marks the
symbol constant.  The lowered function returns SYM-SLOT."
  (unless (>= (length sexp) 5)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-int-defconst-symbol-arity sexp)))
  (let* ((name (nth 1 sexp))
         (sym-lit (nth 2 sexp))
         (params (nth 3 sexp))
         (body (nelisp-phase47-compiler--body->form (nthcdr 4 sexp))))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defconst-symbol-name-not-symbol name)))
    (unless (or (symbolp sym-lit) (stringp sym-lit))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defconst-symbol-literal sym-lit)))
    (unless (and (listp params)
                 (>= (length params) 5)
                 (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-int-defconst-symbol-params params)))
    (let* ((out (nth 0 params))
           (mirror (nth 1 params))
           (frames (nth 2 params))
           (scratch (nth 3 params))
           (sym-slot (nth 4 params))
           (sexp-vars (nthcdr 5 params))
           (symbol-name (if (symbolp sym-lit) (symbol-name sym-lit) sym-lit))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params))
           (raw-body
            (nelisp-phase47-compiler--preprocess-sexp-int-expr
             body sexp-vars)))
      `(defun ,name ,annotated-params
         (seq
          (sexp-write-symbol-lit ,sym-slot ,symbol-name)
          (sexp-write-nil ,out)
          (extern-call nelisp_mirror_set_constant
                       ,mirror ,sym-slot ,out)
          (sexp-int-make ,out ,raw-body)
          (extern-call nelisp_env_set_value
                       ,mirror ,frames ,sym-slot ,out ,scratch 0)
          (sexp-write-t ,out)
          (extern-call nelisp_mirror_set_constant
                       ,mirror ,sym-slot ,out)
          ,sym-slot)))))

(defun nelisp-phase47-compiler--preprocess-defun-sexp-builtin1-symbol (sexp)
  "Lower a one-argument Sexp builtin delegation helper.
Surface form:

  (defun-sexp-builtin1-symbol NAME BUILTIN
      (OUT MIRROR FRAMES SCRATCH NAME-SLOT ARG))

NAME-SLOT is caller-owned `*mut Sexp' storage used to materialize the
BUILTIN symbol.  ARG is a `*const Sexp' argument.  The lowered helper
delegates to the Doc 129.6 runtime dispatcher ABI
`nelisp_aot_builtin_call1', which writes its result to OUT and returns
OUT."
  (unless (= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-sexp-builtin1-symbol-arity sexp)))
  (let ((name (nth 1 sexp))
        (builtin-lit (nth 2 sexp))
        (params (nth 3 sexp)))
    (unless (symbolp name)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-builtin1-symbol-name-not-symbol name)))
    (unless (or (symbolp builtin-lit) (stringp builtin-lit))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-builtin1-symbol-literal builtin-lit)))
    (unless (and (listp params)
                 (= (length params) 6)
                 (cl-every #'symbolp params))
      (signal 'nelisp-phase47-compiler-error
              (list :defun-sexp-builtin1-symbol-params params)))
    (let* ((out (nth 0 params))
           (mirror (nth 1 params))
           (frames (nth 2 params))
           (scratch (nth 3 params))
           (name-slot (nth 4 params))
           (arg (nth 5 params))
           (builtin-name (if (symbolp builtin-lit)
                             (symbol-name builtin-lit)
                           builtin-lit))
           (annotated-params
            (mapcar (lambda (p) (list p :type 'sexp)) params)))
      `(defun ,name ,annotated-params
         (seq
          (sexp-write-symbol-lit ,name-slot ,builtin-name)
          (extern-call nelisp_aot_builtin_call1
                       ,mirror ,frames ,name-slot ,arg ,out ,scratch)
          ,out)))))

(defun nelisp-phase47-compiler--preprocess-source (sexp)
  "Macroexpand and normalize SEXP before Phase 47 parsing.
This is Doc 129.1's host-frontend step.  It deliberately preserves
Phase 47 structural forms instead of applying `macroexpand-all' to
the whole program."
  (cond
   ;; Host `nil' is the false/zero value in the current raw-i64 Phase
   ;; 47 grammar.  Do not rewrite `t' globally; `cond' keeps its
   ;; existing `(t BODY)' sentinel handling.
   ((null sexp) 0)
   ((atom sexp) sexp)
   ((eq (car sexp) 'quote) sexp)
   ((eq (car sexp) 'function) sexp)
   ((eq (car sexp) 'funcall)
    (nelisp-phase47-compiler--preprocess-funcall-lambda sexp))
   ((memq (car sexp) '(mapcar mapc mapconcat))
    (nelisp-phase47-compiler--preprocess-map-lambda sexp))
   ((nelisp-phase47-compiler--lambda-literal-form (car sexp))
    (nelisp-phase47-compiler--lambda-lift-call
     (nelisp-phase47-compiler--lambda-literal-form (car sexp))
     (cdr sexp)))
   ((eq (car sexp) 'progn)
    (let ((body (mapcar #'nelisp-phase47-compiler--preprocess-source
                        (cdr sexp))))
      (cond
       ((null body) 0)
       ((null (cdr body)) (car body))
       (t (cons 'seq body)))))
   ((eq (car sexp) 'seq)
    (cons 'seq (mapcar #'nelisp-phase47-compiler--preprocess-source
                       (cdr sexp))))
   ((eq (car sexp) 'defun-sexp-int)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-int sexp)))
   ((eq (car sexp) 'defun-sexp-int-setq)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-int-setq sexp)))
   ((eq (car sexp) 'defun-sexp-int-setq-symbol)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-int-setq-symbol sexp)))
   ((eq (car sexp) 'defun-sexp-int-defvar-symbol)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-int-defvar-symbol sexp)))
   ((eq (car sexp) 'defun-sexp-int-defconst-symbol)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-int-defconst-symbol sexp)))
   ((eq (car sexp) 'defun-sexp-builtin1-symbol)
    (nelisp-phase47-compiler--preprocess-source
     (nelisp-phase47-compiler--preprocess-defun-sexp-builtin1-symbol sexp)))
   ((eq (car sexp) 'defun)
    (unless (>= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-arity sexp)))
    `(defun ,(nth 1 sexp)
       ,(nth 2 sexp)
       ,(nelisp-phase47-compiler--body->form (nthcdr 3 sexp))))
   ((eq (car sexp) 'defmacro)
    ;; Top-level defmacros are stripped before this point.  Nested
    ;; defmacro has no Phase 47 runtime meaning.
    (signal 'nelisp-phase47-compiler-error
            (list :nested-defmacro sexp)))
   ((eq (car sexp) 'let)
    (unless (>= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :let-arity sexp)))
    `(let ,(mapcar
            (lambda (binding)
              (unless (and (consp binding)
                           (consp (cdr binding))
                           (null (cddr binding)))
                (signal 'nelisp-phase47-compiler-error
                        (list :let-binding-shape binding)))
              (list (car binding)
                    (nelisp-phase47-compiler--preprocess-source
                     (cadr binding))))
            (nth 1 sexp))
       ,(nelisp-phase47-compiler--body->form (nthcdr 2 sexp))))
   ((eq (car sexp) 'let*)
    (nelisp-phase47-compiler--preprocess-let*-bindings
     (nth 1 sexp) (nthcdr 2 sexp)))
   ((eq (car sexp) 'if)
    (unless (<= 3 (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :if-arity sexp)))
    `(if ,(nelisp-phase47-compiler--preprocess-source (nth 1 sexp))
         ,(nelisp-phase47-compiler--preprocess-source (nth 2 sexp))
       ,(nelisp-phase47-compiler--preprocess-source
         (if (= (length sexp) 4) (nth 3 sexp) 0))))
   ((eq (car sexp) 'while)
    (cons 'while (mapcar #'nelisp-phase47-compiler--preprocess-source
                         (cdr sexp))))
   ((eq (car sexp) 'cond)
    (cons 'cond (mapcar #'nelisp-phase47-compiler--preprocess-cond-clause
                        (cdr sexp))))
   ((memq (car sexp) '(and or))
    (cons (car sexp)
          (mapcar #'nelisp-phase47-compiler--preprocess-source
                  (cdr sexp))))
   ((memq (car sexp) '(write static-imm32-table-define))
    sexp)
   (t
    (let ((expanded (macroexpand sexp)))
      (if (not (equal expanded sexp))
          (nelisp-phase47-compiler--preprocess-source expanded)
        (cons (car sexp)
              (mapcar #'nelisp-phase47-compiler--preprocess-source
                      (cdr sexp))))))))

(defun nelisp-phase47-compiler--prepare-source (sexp)
  "Apply Doc 129.1 compile-time macro handling to SEXP."
  (let* ((extracted (nelisp-phase47-compiler--extract-defmacros sexp))
         (source (plist-get extracted :source))
         (defs (plist-get extracted :defmacros))
         (module-forms (plist-get extracted :module-forms))
         (special-vars (plist-get extracted :special-vars)))
    (dolist (form module-forms)
      (nelisp-phase47-compiler--eval-top-level-module-form form))
    (nelisp-phase47-compiler--with-defmacros
     defs
     (lambda ()
       (let* ((nelisp-phase47-compiler--lambda-lift-counter 0)
              (nelisp-phase47-compiler--lambda-lift-hoists nil)
              (nelisp-phase47-compiler--lambda-lift-names
               (nelisp-phase47-compiler--top-level-defun-names source))
              (nelisp-phase47-compiler--special-vars special-vars)
              (processed
               (nelisp-phase47-compiler--preprocess-source source))
              (hoists (nreverse
                       nelisp-phase47-compiler--lambda-lift-hoists)))
         (cond
          ((null hoists) processed)
          ((and (consp processed) (eq (car processed) 'seq))
           (cons 'seq (append hoists (cdr processed))))
          (t
           (cons 'seq (append hoists (list processed))))))))))

(defconst nelisp-phase47-compiler--aot-builtin1-delegation-symbols
  '(identity ignore
    not null atom consp listp symbolp keywordp numberp integerp
    float stringp vectorp
    car cdr car-safe cdr-safe
    length reverse nreverse copy-sequence
    1+ 1- abs
    symbol-name prin1-to-string number-to-string string-to-number
    upcase downcase)
  "One-argument builtins that may lower through Doc 129.6 delegation.
These names are direct-call candidates only when no same-named Phase 47
defun is visible in the current compile unit.")

(defconst nelisp-phase47-compiler--aot-builtinn-delegation-symbols
  '(list vector concat append
    cons eq eql equal
    memq member assq assv assoc
    nth elt
    string= string< string-lessp
    format message
    string-match string-match-p
    substring string-prefix-p string-suffix-p
    replace-regexp-in-string
    mapcar mapc mapconcat)
  "Vararg builtins that may lower through Doc 129.6 calln delegation.
These names are direct-call candidates only when no same-named Phase 47
defun is visible in the current compile unit.")

(defun nelisp-phase47-compiler--fenv-has-symbol-p (fenv sym)
  "Return non-nil when FENV binds SYM."
  (and (symbolp sym) (assq sym fenv)))

(defun nelisp-phase47-compiler--aot-name-slot-symbol (fenv sexp)
  "Return the caller-owned function-name slot symbol in FENV.
The boxed-boundary MVP accepts either `name-slot' or `name_slot' to
match the spelling already used by older Doc 129 helpers."
  (let ((name-slot (cond
                    ((nelisp-phase47-compiler--fenv-has-symbol-p
                      fenv 'name-slot)
                     'name-slot)
                    ((nelisp-phase47-compiler--fenv-has-symbol-p
                      fenv 'name_slot)
                     'name_slot)
                    (t nil))))
    (unless name-slot
      (signal 'nelisp-phase47-compiler-error
              (list :aot-function-designator-boundary-missing
                    '(name-slot)
                    :form sexp)))
    name-slot))

(defun nelisp-phase47-compiler--aot-function-designator-symbol (form)
  "Return FORM's quoted/function symbol designator, or nil.
Recognizes `(quote SYMBOL)' and `(function SYMBOL)' only.  Lambda
closures and arbitrary value forms stay on the normal runtime-dispatch
path and must already be materialized as boxed Sexp values."
  (when (and (consp form)
             (= (length form) 2)
             (memq (car form) '(quote function))
             (symbolp (cadr form)))
    (cadr form)))

(defun nelisp-phase47-compiler--aot-function-designator-lowering
    (fn-form fenv sexp)
  "Return plist describing lowered FN-FORM, or nil for dynamic values.
For quoted/function symbol designators, materialize a `Sexp::Symbol' in
the caller-owned name slot before dispatching through the AOT bridge."
  (let ((sym (nelisp-phase47-compiler--aot-function-designator-symbol
              fn-form)))
    (when sym
      (let ((name-slot
             (nelisp-phase47-compiler--aot-name-slot-symbol fenv sexp)))
        (list :fn-expr name-slot
              :prefix `((sexp-write-symbol-lit
                         ,name-slot
                         ,(symbol-name sym))))))))

(defun nelisp-phase47-compiler--aot-builtin1-boundary-symbols (fenv sexp)
  "Return boundary symbols for direct builtin1 lowering in FENV.
SEXP is used only for diagnostics.  The MVP requires a fixed set of
caller-owned boundary params in the current defun:
`out', `mirror', `frames', `scratch', and either `name-slot' or
`name_slot'."
  (let* ((out 'out)
         (mirror 'mirror)
         (frames 'frames)
         (scratch 'scratch)
         (name-slot (cond
                     ((nelisp-phase47-compiler--fenv-has-symbol-p
                       fenv 'name-slot)
                      'name-slot)
                     ((nelisp-phase47-compiler--fenv-has-symbol-p
                       fenv 'name_slot)
                      'name_slot)
                     (t nil)))
         (missing nil))
    (dolist (sym (list out mirror frames scratch))
      (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
        (push sym missing)))
    (unless name-slot
      (push 'name-slot missing))
    (when missing
      (signal 'nelisp-phase47-compiler-error
              (list :aot-builtin1-boundary-missing
                    (nreverse missing)
                    :form sexp)))
    (list :out out
          :mirror mirror
          :frames frames
          :scratch scratch
          :name-slot name-slot)))

(defun nelisp-phase47-compiler--parse-aot-builtin1-call
    (sexp env fenv defuns)
  "Lower a direct one-argument builtin call SEXP through the AOT dispatcher."
  (let ((builtin (car sexp)))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :aot-builtin1-call-arity sexp)))
    (let* ((arg (nth 1 sexp))
           (boundary
            (nelisp-phase47-compiler--aot-builtin1-boundary-symbols
             fenv sexp))
           (out (plist-get boundary :out))
           (mirror (plist-get boundary :mirror))
           (frames (plist-get boundary :frames))
           (scratch (plist-get boundary :scratch))
           (name-slot (plist-get boundary :name-slot)))
      (nelisp-phase47-compiler--parse-value
       `(seq
         (sexp-write-symbol-lit ,name-slot ,(symbol-name builtin))
         (extern-call nelisp_aot_builtin_call1
                      ,mirror ,frames ,name-slot ,arg ,out ,scratch)
         ,out)
       env fenv defuns))))

(defun nelisp-phase47-compiler--parse-aot-builtinn-call
    (sexp env fenv defuns)
  "Lower a direct vararg builtin call SEXP through the AOT dispatcher."
  (let* ((builtin (car sexp))
         (args (cdr sexp))
         (argc (length args))
         (boundary
          (nelisp-phase47-compiler--aot-builtin1-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (name-slot (plist-get boundary :name-slot))
         (fn-designator
          (and (memq builtin '(mapcar mapc mapconcat))
               args
               (nelisp-phase47-compiler--aot-function-designator-symbol
                (car args))))
         (lowered-args (if fn-designator
                           (cons scratch (cdr args))
                         args))
         (arg-prefix (when fn-designator
                       `((sexp-write-symbol-lit
                          ,scratch
                          ,(symbol-name fn-designator))))))
    (nelisp-phase47-compiler--parse-value
     `(seq
       (sexp-write-symbol-lit ,name-slot ,(symbol-name builtin))
       ,@arg-prefix
       (extern-call nelisp_aot_builtin_calln
                    ,mirror ,frames ,name-slot ,argc ,out ,scratch
                    ,@lowered-args)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--aot-funcall1-boundary-symbols (fenv sexp)
  "Return boundary symbols for direct `(funcall FN ARG)' lowering in FENV."
  (let ((missing nil))
    (dolist (sym '(out mirror frames scratch))
      (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
        (push sym missing)))
    (when missing
      (signal 'nelisp-phase47-compiler-error
              (list :aot-funcall1-boundary-missing
                    (nreverse missing)
                    :form sexp)))
    (list :out 'out
          :mirror 'mirror
          :frames 'frames
          :scratch 'scratch)))

(defun nelisp-phase47-compiler--parse-aot-funcall1
    (sexp env fenv defuns)
  "Lower `(funcall FN ARG)' through the Doc 129.7 one-arg dispatcher."
  (unless (= (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-funcall1-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-funcall1-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (fn-lowering
          (nelisp-phase47-compiler--aot-function-designator-lowering
           (nth 1 sexp) fenv sexp))
         (fn (or (plist-get fn-lowering :fn-expr)
                 (nth 1 sexp)))
         (prefix (plist-get fn-lowering :prefix))
         (arg (nth 2 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_funcall1
                    ,mirror ,frames ,fn ,arg ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-funcall2
    (sexp env fenv defuns)
  "Lower `(funcall FN ARG0 ARG1)' through the Doc 129.7 two-arg dispatcher."
  (unless (= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-funcall2-arity sexp)))
  (let* ((missing nil)
         (_ (dolist (sym '(out mirror frames))
              (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
                (push sym missing))))
         (_ (when missing
              (signal 'nelisp-phase47-compiler-error
                      (list :aot-funcall2-boundary-missing
                            (nreverse missing)
                            :form sexp))))
         (out 'out)
         (mirror 'mirror)
         (frames 'frames)
         (fn-lowering
          (nelisp-phase47-compiler--aot-function-designator-lowering
           (nth 1 sexp) fenv sexp))
         (fn (or (plist-get fn-lowering :fn-expr)
                 (nth 1 sexp)))
         (prefix (plist-get fn-lowering :prefix))
         (arg0 (nth 2 sexp))
         (arg1 (nth 3 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_funcall2
                    ,mirror ,frames ,fn ,arg0 ,arg1 ,out)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-funcall3
    (sexp env fenv defuns)
  "Lower `(funcall FN ARG0 ARG1 ARG2)' through the Doc 129.7 dispatcher."
  (unless (= (length sexp) 5)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-funcall3-arity sexp)))
  (let* ((missing nil)
         (_ (dolist (sym '(out mirror frames))
              (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
                (push sym missing))))
         (_ (when missing
              (signal 'nelisp-phase47-compiler-error
                      (list :aot-funcall3-boundary-missing
                            (nreverse missing)
                            :form sexp))))
         (out 'out)
         (mirror 'mirror)
         (frames 'frames)
         (fn-lowering
          (nelisp-phase47-compiler--aot-function-designator-lowering
           (nth 1 sexp) fenv sexp))
         (fn (or (plist-get fn-lowering :fn-expr)
                 (nth 1 sexp)))
         (prefix (plist-get fn-lowering :prefix))
         (arg0 (nth 2 sexp))
         (arg1 (nth 3 sexp))
         (arg2 (nth 4 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_funcall3
                    ,mirror ,frames ,fn ,arg0 ,arg1 ,arg2 ,out)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-funcalln
    (sexp env fenv defuns)
  "Lower `(funcall FN ARG...)' through the Doc 129.7 calln dispatcher."
  (when (< (length sexp) 6)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-funcalln-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-funcall1-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (fn-lowering
          (nelisp-phase47-compiler--aot-function-designator-lowering
           (nth 1 sexp) fenv sexp))
         (fn (or (plist-get fn-lowering :fn-expr)
                 (nth 1 sexp)))
         (prefix (plist-get fn-lowering :prefix))
         (args (nthcdr 2 sexp))
         (argc (length args)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_funcalln
                    ,mirror ,frames ,fn ,argc ,out ,scratch ,@args)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-apply
    (sexp env fenv defuns)
  "Lower `apply' through the Doc 129.7 apply dispatchers."
  (when (< (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-apply-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-funcall1-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (fn-lowering
          (nelisp-phase47-compiler--aot-function-designator-lowering
           (nth 1 sexp) fenv sexp))
         (fn (or (plist-get fn-lowering :fn-expr)
                 (nth 1 sexp)))
         (prefix (plist-get fn-lowering :prefix))
         (apply-args (nthcdr 2 sexp))
         (argc (length apply-args)))
    (nelisp-phase47-compiler--parse-value
     (if (= argc 1)
         `(seq
           ,@prefix
           (extern-call nelisp_aot_apply
                        ,mirror ,frames ,fn ,(car apply-args) ,out ,scratch)
           ,out)
       `(seq
         ,@prefix
         (extern-call nelisp_aot_applyn
                      ,mirror ,frames ,fn ,argc ,out ,scratch ,@apply-args)
         ,out))
     env fenv defuns)))

(defun nelisp-phase47-compiler--aot-exception-boundary-symbols (fenv sexp)
  "Return boundary symbols for Doc 129.8 throw/signal lowering in FENV."
  (let ((missing nil))
    (dolist (sym '(out mirror frames scratch))
      (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
        (push sym missing)))
    (when missing
      (signal 'nelisp-phase47-compiler-error
              (list :aot-exception-boundary-missing
                    (nreverse missing)
                    :form sexp)))
    (list :out 'out
          :mirror 'mirror
          :frames 'frames
          :scratch 'scratch)))

(defun nelisp-phase47-compiler--aot-quoted-symbol (form)
  "Return FORM's `(quote SYMBOL)' payload, or nil."
  (when (and (consp form)
             (eq (car form) 'quote)
             (= (length form) 2)
             (symbolp (cadr form)))
    (cadr form)))

(defun nelisp-phase47-compiler--aot-tag-lowering (tag-form fenv sexp)
  "Return plist for lowering TAG-FORM as a Doc 129.8 tag value.
Quoted symbol tags are materialized into the caller-owned name slot.
Dynamic tag values are forwarded as ordinary value expressions."
  (let ((sym (nelisp-phase47-compiler--aot-quoted-symbol tag-form)))
    (if sym
        (let ((name-slot
               (nelisp-phase47-compiler--aot-name-slot-symbol fenv sexp)))
          (list :tag-expr name-slot
                :prefix `((sexp-write-symbol-lit
                           ,name-slot
                           ,(symbol-name sym)))))
      (list :tag-expr tag-form
            :prefix nil))))

(defun nelisp-phase47-compiler--parse-aot-throw
    (sexp env fenv defuns)
  "Lower `(throw TAG VALUE)' through the Doc 129.8 throw bridge."
  (unless (= (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-throw-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (tag-lowering
          (nelisp-phase47-compiler--aot-tag-lowering
           (nth 1 sexp) fenv sexp))
         (tag (plist-get tag-lowering :tag-expr))
         (prefix (plist-get tag-lowering :prefix))
         (value (nth 2 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_throw
                    ,mirror ,frames ,tag ,value ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-signal
    (sexp env fenv defuns)
  "Lower `(signal TAG DATA)' through the Doc 129.8 signal bridge."
  (unless (= (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-signal-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (tag-lowering
          (nelisp-phase47-compiler--aot-tag-lowering
           (nth 1 sexp) fenv sexp))
         (tag (plist-get tag-lowering :tag-expr))
         (prefix (plist-get tag-lowering :prefix))
         (data (nth 2 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_signal
                    ,mirror ,frames ,tag ,data ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-errorn
    (sexp env fenv defuns)
  "Lower formatted `(error ARG...)' through the Doc 129.8 errorn bridge."
  (when (< (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-errorn-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (raw-args (nthcdr 1 sexp))
         (first (car raw-args))
         (args (if (stringp first)
                   (cons scratch (cdr raw-args))
                 raw-args))
         (prefix (when (stringp first)
                   `((sexp-write-str-lit ,scratch ,first))))
         (argc (length args)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_errorn
                    ,mirror ,frames ,argc ,out ,scratch ,@args)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-error
    (sexp env fenv defuns)
  "Lower source `error' through the Doc 129.8 signal/error bridges."
  (cond
   ((= (length sexp) 2)
    (nelisp-phase47-compiler--parse-aot-signal
     `(signal 'error ,(nth 1 sexp))
     env fenv defuns))
   ((> (length sexp) 2)
    (nelisp-phase47-compiler--parse-aot-errorn
     sexp env fenv defuns))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :aot-error-arity sexp)))))

(defun nelisp-phase47-compiler--parse-aot-push-handler
    (sexp env fenv defuns)
  "Lower Doc 129.8 explicit AOT handler push forms.
Accepted forms:
  (aot-push-catch TAG LANDING-PAD SAVED-SP)
  (aot-push-condition CONDITIONS LANDING-PAD SAVED-SP)
  (aot-push-unwind CLEANUP LANDING-PAD SAVED-SP)"
  (unless (= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-push-handler-arity sexp)))
  (let* ((op (car sexp))
         (extern (pcase op
                   ('aot-push-catch 'nelisp_aot_push_catch)
                   ('aot-push-condition 'nelisp_aot_push_condition)
                   ('aot-push-unwind 'nelisp_aot_push_unwind)
                   (_ (signal 'nelisp-phase47-compiler-error
                              (list :aot-push-handler-op op)))))
         (boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (selector-lowering
          (if (memq op '(aot-push-catch aot-push-condition))
              (nelisp-phase47-compiler--aot-tag-lowering
               (nth 1 sexp) fenv sexp)
            (list :tag-expr (nth 1 sexp) :prefix nil)))
         (selector (plist-get selector-lowering :tag-expr))
         (prefix (plist-get selector-lowering :prefix))
         (landing-pad (nth 2 sexp))
         (saved-sp (nth 3 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call ,extern
                    ,mirror ,frames ,selector ,landing-pad ,saved-sp ,scratch))
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-pop-handler
    (sexp env fenv defuns)
  "Lower `(aot-pop-handler EXPECTED-KIND)' through the Doc 129.8 pop bridge."
  (unless (= (length sexp) 2)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-pop-handler-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (kind-lowering
          (nelisp-phase47-compiler--aot-tag-lowering
           (nth 1 sexp) fenv sexp))
         (kind (plist-get kind-lowering :tag-expr))
         (prefix (plist-get kind-lowering :prefix)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_pop_handler
                    ,mirror ,frames ,kind ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--aot-root-boundary-symbols (fenv sexp)
  "Return boundary symbols for Doc 129.5 AOT root push/pop lowering."
  (let ((missing nil))
    (dolist (sym '(out mirror frames scratch))
      (unless (nelisp-phase47-compiler--fenv-has-symbol-p fenv sym)
        (push sym missing)))
    (when missing
      (signal 'nelisp-phase47-compiler-error
              (list :aot-root-boundary-missing
                    (nreverse missing)
                    :form sexp)))
    (list :out 'out
          :mirror 'mirror
          :frames 'frames
          :scratch 'scratch)))

(defun nelisp-phase47-compiler--parse-aot-push-roots
    (sexp env fenv defuns)
  "Lower `(aot-push-roots ROOTS)' through the Doc 129.5 push bridge."
  (unless (= (length sexp) 2)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-push-roots-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-root-boundary-symbols fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (roots (nth 1 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       (extern-call nelisp_aot_push_roots
                    ,mirror ,frames ,roots ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-pop-roots
    (sexp env fenv defuns)
  "Lower `(aot-pop-roots ROOTS)' through the Doc 129.5 pop bridge."
  (unless (= (length sexp) 2)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-pop-roots-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-root-boundary-symbols fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (roots (nth 1 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       (extern-call nelisp_aot_pop_roots
                    ,mirror ,frames ,roots ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-push-special
    (sexp env fenv defuns)
  "Lower `(aot-push-special NAME VALUE)' through the Doc 129.4 bridge."
  (unless (= (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-push-special-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (name-lowering
          (nelisp-phase47-compiler--aot-tag-lowering
           (nth 1 sexp) fenv sexp))
         (name (plist-get name-lowering :tag-expr))
         (prefix (plist-get name-lowering :prefix))
         (value (nth 2 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       ,@prefix
       (extern-call nelisp_aot_push_special
                    ,mirror ,frames ,name ,value ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-pop-special
    (sexp env fenv defuns)
  "Lower `(aot-pop-special HANDLE)' through the Doc 129.4 bridge."
  (unless (= (length sexp) 2)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-pop-special-arity sexp)))
  (let* ((boundary
          (nelisp-phase47-compiler--aot-exception-boundary-symbols
           fenv sexp))
         (out (plist-get boundary :out))
         (mirror (plist-get boundary :mirror))
         (frames (plist-get boundary :frames))
         (scratch (plist-get boundary :scratch))
         (handle (nth 1 sexp)))
    (nelisp-phase47-compiler--parse-value
     `(seq
       (extern-call nelisp_aot_pop_special
                    ,mirror ,frames ,handle ,out ,scratch)
       ,out)
     env fenv defuns)))

(defun nelisp-phase47-compiler--aot-nonlocal-source-form-p (sexp)
  "Return non-nil when SEXP contains a not-yet-safe non-local form.
Doc 129.8E only synthesizes balanced handler push/pop for bodies that
return normally.  Until native landing-pad jumps exist, compiling a body
that explicitly contains `throw', `signal', `error', `condition-case',
or `unwind-protect' would leave ordinary fallthrough code after a
simulated non-local exit."
  (cond
   ((atom sexp) nil)
   ((memq (car sexp) '(quote function)) nil)
   ((memq (car sexp) '(throw signal error condition-case unwind-protect)) t)
   (t (cl-some #'nelisp-phase47-compiler--aot-nonlocal-source-form-p
               (cdr sexp)))))

(defun nelisp-phase47-compiler--parse-aot-special-let-normal-exit
    (var val-sexp body-sexp env fenv defuns source-form)
  "Lower one source-level special `let' binding for the normal exit path.
VAR is a top-level special variable.  VAL-SEXP is evaluated before the
push bridge, BODY-SEXP runs while the value cell is rebound, then the
saved BODY value is returned after the pop bridge.  Non-local exits
through BODY remain pending on the 129.8 landing-pad path."
  (when (nelisp-phase47-compiler--aot-nonlocal-source-form-p body-sexp)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-special-let-nonlocal-body source-form)))
  ;; Force boundary diagnostics to point at the source `let'.
  (nelisp-phase47-compiler--aot-exception-boundary-symbols fenv source-form)
  (nelisp-phase47-compiler--aot-name-slot-symbol fenv source-form)
  (let ((value-slot (nelisp-phase47-compiler--gensym
                     "aot-special-value")))
    (nelisp-phase47-compiler--parse-value
     `(seq
       (aot-push-special ',var ,val-sexp)
       (let (((,value-slot :type sexp) ,body-sexp))
         (seq
          (aot-pop-special 0)
          ,value-slot)))
     env fenv defuns)))

(defun nelisp-phase47-compiler--parse-aot-catch-normal-exit
    (sexp env fenv defuns)
  "Lower a source-level `(catch TAG BODY...)' normal-exit path.
The lowering installs a catch handler, evaluates BODY, saves the BODY
value in a frame slot, pops the handler, then returns the saved value.
Bodies containing explicit non-local exit forms are rejected until the
native landing-pad jump path is available."
  (unless (>= (length sexp) 3)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-catch-arity sexp)))
  (let ((body-forms (cddr sexp)))
    (when (cl-some #'nelisp-phase47-compiler--aot-nonlocal-source-form-p
                   body-forms)
      (signal 'nelisp-phase47-compiler-error
              (list :aot-catch-nonlocal-body sexp)))
    ;; Force the boundary diagnostics to point at the source `catch'
    ;; instead of the generated `aot-*' forms.
    (nelisp-phase47-compiler--aot-exception-boundary-symbols fenv sexp)
    (nelisp-phase47-compiler--aot-name-slot-symbol fenv sexp)
    (let ((value-slot (nelisp-phase47-compiler--gensym
                       "aot-catch-value"))
          (tag (nth 1 sexp))
          (body (nelisp-phase47-compiler--body->form body-forms)))
      (nelisp-phase47-compiler--parse-value
       `(seq
         (aot-push-catch ,tag 0 0)
         (let (((,value-slot :type sexp) ,body))
           (seq
            (aot-pop-handler 'catch)
            ,value-slot)))
       env fenv defuns))))

(defun nelisp-phase47-compiler--aot-condition-case-selectors (sexp)
  "Return condition symbols handled by source condition-case SEXP.
Doc 129.8F started with the common `(condition BODY...)' handler shape;
Doc 129.8J also accepts `(CONDITION...)' list specs by installing one
condition handler per symbol with the same landing metadata.  Doc
129.8K walks every handler clause on the normal-exit path; actual
handler-body dispatch still waits for native landing pads."
  (let ((clauses (nthcdr 3 sexp)))
    (unless clauses
      (signal 'nelisp-phase47-compiler-error
              (list :aot-condition-case-no-handlers sexp)))
    (apply
     #'append
     (mapcar
      (lambda (clause)
        (let ((selector (car clause)))
          (cond
           ((symbolp selector)
            (list selector))
           ((and (consp selector)
                 (cl-every #'symbolp selector))
            selector)
           (t
            (signal 'nelisp-phase47-compiler-error
                    (list :aot-condition-case-handler-shape
                          clause))))))
      clauses))))

(defun nelisp-phase47-compiler--parse-aot-condition-case-normal-exit
    (sexp env fenv defuns)
  "Lower source `(condition-case VAR BODY HANDLER...)' normal exit.
The MVP installs a condition handler, evaluates BODY, saves BODY's
result, pops the handler, then returns the saved value.  Actual handler
landing-pad jumps for signalled conditions remain later Doc 129.8 work."
  (unless (>= (length sexp) 4)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-condition-case-arity sexp)))
  (let ((var (nth 1 sexp))
        (body (nth 2 sexp)))
    (unless (or (null var) (symbolp var))
      (signal 'nelisp-phase47-compiler-error
              (list :aot-condition-case-var-shape var)))
    (when (nelisp-phase47-compiler--aot-nonlocal-source-form-p body)
      (signal 'nelisp-phase47-compiler-error
              (list :aot-condition-case-nonlocal-body sexp)))
    ;; Force diagnostics to point at the source form.
    (nelisp-phase47-compiler--aot-exception-boundary-symbols fenv sexp)
    (nelisp-phase47-compiler--aot-name-slot-symbol fenv sexp)
    (let ((value-slot (nelisp-phase47-compiler--gensym
                       "aot-condition-value"))
          (selectors
           (nelisp-phase47-compiler--aot-condition-case-selectors sexp)))
      (nelisp-phase47-compiler--parse-value
       `(seq
         ,@(mapcar (lambda (selector)
                     `(aot-push-condition ',selector 0 0))
                   selectors)
         (let (((,value-slot :type sexp) ,body))
           (seq
            ,@(make-list (length selectors)
                         `(aot-pop-handler 'condition))
            ,value-slot)))
       env fenv defuns))))

(defun nelisp-phase47-compiler--parse-aot-unwind-protect-normal-exit
    (sexp env fenv defuns)
  "Lower source `(unwind-protect BODY CLEANUP...)' normal exit.
The MVP evaluates BODY, saves its result in a runtime Sexp slot, runs
CLEANUP forms, then returns the saved BODY result.  Non-local exits
crossing the protected body still require native landing-pad support."
  (unless (>= (length sexp) 2)
    (signal 'nelisp-phase47-compiler-error
            (list :aot-unwind-protect-arity sexp)))
  (let ((body (nth 1 sexp))
        (cleanups (nthcdr 2 sexp)))
    (when (or (nelisp-phase47-compiler--aot-nonlocal-source-form-p body)
              (cl-some
               #'nelisp-phase47-compiler--aot-nonlocal-source-form-p
               cleanups))
      (signal 'nelisp-phase47-compiler-error
              (list :aot-unwind-protect-nonlocal-form sexp)))
    (let ((value-slot (nelisp-phase47-compiler--gensym
                       "aot-unwind-value")))
      (nelisp-phase47-compiler--parse-value
       `(let (((,value-slot :type sexp) ,body))
          (seq
           ,@cleanups
           ,value-slot))
       env fenv defuns))))

(defun nelisp-phase47-compiler--normalize-defun-params (param-forms sexp)
  "Normalize PARAM-FORMS, accepting a single `&rest' marker.
The returned plist contains `:params' with the marker removed,
`:fixed-count', and `:rest-p'.  The current native ABI still receives
the rest list as one ordinary final argument; call-site rest-list
construction is handled separately for direct calls."
  (unless (listp param-forms)
    (signal 'nelisp-phase47-compiler-error
            (list :defun-params-not-list param-forms)))
  (let ((before nil)
        (rest-tail param-forms)
        (rest-p nil)
        (rest-form nil))
    (while (and rest-tail (not (eq (car rest-tail) '&rest)))
      (push (car rest-tail) before)
      (setq rest-tail (cdr rest-tail)))
    (if rest-tail
        (progn
          (setq rest-p t)
          (unless (and (= (length rest-tail) 2)
                       (not (eq (cadr rest-tail) '&rest)))
            (signal 'nelisp-phase47-compiler-error
                    (list :defun-rest-param-shape sexp)))
          (setq rest-form (cadr rest-tail)))
      (setq rest-p nil))
    (let ((fixed (nreverse before)))
      (list :params (if rest-p
                        (append fixed (list rest-form))
                      fixed)
            :fixed-count (length fixed)
            :rest-p rest-p))))

(defun nelisp-phase47-compiler--defun-signature (param-forms sexp)
  "Return call-site signature metadata for DEFUN PARAM-FORMS."
  (let* ((norm (nelisp-phase47-compiler--normalize-defun-params
                param-forms sexp))
         (params (plist-get norm :params)))
    (if (plist-get norm :rest-p)
        (list :arity (length params)
              :fixed-count (plist-get norm :fixed-count)
              :rest-p t)
      (length params))))

(defun nelisp-phase47-compiler--defun-signature-arity (signature)
  "Return the ABI arity for SIGNATURE."
  (if (integerp signature)
      signature
    (plist-get signature :arity)))

(defun nelisp-phase47-compiler--defun-signature-rest-p (signature)
  "Return non-nil when SIGNATURE describes a rest-param defun."
  (and (consp signature) (plist-get signature :rest-p)))

(defun nelisp-phase47-compiler--defun-signature-fixed-count (signature)
  "Return the source fixed argument count for SIGNATURE."
  (if (integerp signature)
      signature
    (plist-get signature :fixed-count)))

(defun nelisp-phase47-compiler--parse-value (sexp env fenv defuns)
  "Parse SEXP as a value-producing expression.
Returns an IR node of one of these kinds:
  (:kind imm   :value N)
  (:kind ref   :var V :reg R)
  (:kind arith :op OP :a IR :b IR)
  (:kind call  :name N :args (IR ...))
  (:kind cmp   :op OP :a IR :b IR)              [§97.c]
  (:kind if    :test IR :then IR :else IR :id N) [§97.c]
  (:kind while :test IR :body IR :id N)         [§97.c]
  (:kind cond  :clauses ((PRED . BODY) ...) :id N) [§97.c]
  (:kind logic :op OP :forms (IR ...) :id N)    [§97.c and/or]
ENV is the let-alist (constant bindings), FENV is the function-
parameter alist `((SYM . REG) ...)' for the enclosing function (=
nil at top level), DEFUNS is the alist of already-defined
functions `((NAME . ARITY) ...)'."
  (cond
   ;; Compile-time foldable -> immediate.
   ((nelisp-phase47-compiler--int-foldable-p sexp env fenv)
    (nelisp-phase47-compiler--make-ir 'imm
          :value (nelisp-phase47-compiler--fold-int sexp env)))
   ;; Parameter reference.
   ((symbolp sexp)
    (let ((pcell (assq sexp fenv)))
      (unless pcell
        (signal 'nelisp-phase47-compiler-error
                (list :free-symbol sexp
                      :env-keys (mapcar #'car env)
                      :params (mapcar #'car fenv))))
      ;; FENV cell is `(sym . (:reg R :slot S :class CLASS))' so the
      ;; ref node carries the incoming arg-reg (= used at call sites
      ;; for documentation), the rbp-relative spill slot (= used
      ;; by `--emit-value' / `--emit-f64-ref-load' to read the
      ;; stable copy) and the reg class (= gp or f64, picks the
      ;; load instruction MOV vs MOVSD).
      (let ((info (cdr pcell)))
        (nelisp-phase47-compiler--make-ir 'ref :var sexp
              :reg (plist-get info :reg)
              :slot (plist-get info :slot)
              :class (or (plist-get info :class) 'gp)
              :root-p (plist-get info :root-p)))))
   ;; Arithmetic with at least one non-constant operand.  Doc 100
   ;; §100.D extends the op set with 3 bitwise binops (logior /
   ;; logand / logxor) for the `nl_jit_arith_log*' swap; they share
   ;; the same MR-form emit shape so no new IR kind is needed.
   ((and (consp sexp) (memq (car sexp) '(+ - * logior logand logxor)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :arith-arity (car sexp) sexp)))
    (nelisp-phase47-compiler--make-ir 'arith
          :op (car sexp)
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; Doc 110 §110.E.1 f64 arithmetic — flat binop form only
   ;; (`f64-add' / `f64-sub' / `f64-mul' / `f64-div').  Each arm
   ;; is `(f64-OP A B)' where A, B are themselves value-producing
   ;; IR (= ref to f64 param at MVP; nested binops are deferred
   ;; until xmm spill machinery lands in Doc 112).  Result class
   ;; = f64; produced in xmm0 by `--emit-f64-binop'.
   ((and (consp sexp) (memq (car sexp) '(f64-add f64-sub f64-mul f64-div)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :f64-binop-arity (car sexp) sexp)))
    (nelisp-phase47-compiler--make-ir 'f64-binop
          :op (car sexp)
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; Doc 110 §110.C.2.a — flat f64 ordered comparison (NaN →
   ;; false matching Rust's `<' / `>' / `<=' / `>=' semantics).
   ;; Doc 110 §110.C.2.b — `f64-eq-eps' (= `(a - b).abs() <
   ;; 1e-15') uses a different emit sequence (SUBSD + ANDPD
   ;; abs-mask + UCOMISD vs 1e-15 + SETB/SETNP AND mask) but
   ;; shares the `:kind f64-cmp' IR shape with the ordered
   ;; cases.  Result is i64 0 or 1 produced in rax (= GP
   ;; convention, bridges to the `extern \"C\" fn(f64, f64) ->
   ;; i64' float.rs trampoline shape).
   ((and (consp sexp)
         (memq (car sexp) '(f64-lt f64-gt f64-le f64-ge f64-eq-eps)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :f64-cmp-arity (car sexp) sexp)))
    (nelisp-phase47-compiler--make-ir 'f64-cmp
          :op (car sexp)
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; Doc 100 §100.D shifts — variable count goes through CL, so the
   ;; emit shape differs from `arith' (= an extra mov rcx, r10 before
   ;; the shift opcode).  Only signed-extending arithmetic shift
   ;; (`sar') and logical-left shift (`shl') are needed for the `ash'
   ;; swap; `ash' is composed in elisp as
   ;;   (if (< c 0) (sar n (- 0 c)) (shl n c))
   ;; so unsigned-right (`shr') is not required.
   ((and (consp sexp) (memq (car sexp) '(shl sar)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :shift-arity (car sexp) sexp)))
    (nelisp-phase47-compiler--make-ir 'shift
          :op (car sexp)
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; Comparison op (= < > <= >= =).
   ((and (consp sexp) (memq (car sexp) nelisp-phase47-compiler--cmp-ops))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cmp-arity (car sexp) sexp)))
    (nelisp-phase47-compiler--make-ir 'cmp
          :op (car sexp)
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; (if TEST THEN ELSE) — control-flow value form.
   ((and (consp sexp) (eq (car sexp) 'if))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :if-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'if
          :id (nelisp-phase47-compiler--gensym "if")
          :test (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :then (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)
          :else (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ;; (while TEST BODY...) — returns 0 after loop exit.
   ((and (consp sexp) (eq (car sexp) 'while))
    (unless (>= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :while-arity sexp)))
    (let ((test (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns))
          ;; Multiple body forms wrap as an implicit seq of value
          ;; forms; each result is discarded except the last (=
          ;; rax) which `while' itself overwrites with 0 on exit.
          (body-forms (mapcar
                       (lambda (e)
                         (nelisp-phase47-compiler--parse-value
                          e env fenv defuns))
                       (cddr sexp))))
      (nelisp-phase47-compiler--make-ir 'while
            :id (nelisp-phase47-compiler--gensym "while")
            :test test
            :body body-forms)))
   ;; (cond (PRED1 BODY1) ... (t BODY-N)).
   ((and (consp sexp) (eq (car sexp) 'cond))
    (let ((raw-clauses (cdr sexp)))
      (when (null raw-clauses)
        (signal 'nelisp-phase47-compiler-error
                (list :cond-empty sexp)))
      (let ((clauses
             (mapcar
              (lambda (cl)
                (unless (and (consp cl) (= (length cl) 2))
                  (signal 'nelisp-phase47-compiler-error
                          (list :cond-clause-shape cl)))
                (let ((pred (car cl))
                      (body (cadr cl)))
                  (cons
                   (if (eq pred t)
                       'always
                     (nelisp-phase47-compiler--parse-value
                      pred env fenv defuns))
                   (nelisp-phase47-compiler--parse-value
                    body env fenv defuns))))
              raw-clauses)))
        (nelisp-phase47-compiler--make-ir 'cond
              :id (nelisp-phase47-compiler--gensym "cond")
              :clauses clauses))))
   ;; (and EXPR ...) / (or EXPR ...) short-circuit.
   ((and (consp sexp) (memq (car sexp) '(and or)))
    (let ((op (car sexp))
          (args (cdr sexp)))
      (when (null args)
        (signal 'nelisp-phase47-compiler-error
                (list :logic-empty sexp)))
      (nelisp-phase47-compiler--make-ir 'logic
            :op op
            :id (nelisp-phase47-compiler--gensym (symbol-name op))
            :forms (mapcar (lambda (e)
                             (nelisp-phase47-compiler--parse-value
                              e env fenv defuns))
                           args))))
   ;; Doc 129.1: value-context sequence, produced by macro-expanded
   ;; multi-form `progn'.  Every child must itself be value-producing;
   ;; statement-only forms like top-level `write' remain outside this
   ;; IR because `--emit-value' intentionally has no rodata context.
   ((and (consp sexp) (eq (car sexp) 'seq))
    (let ((children (cdr sexp)))
      (when (null children)
        (signal 'nelisp-phase47-compiler-error
                (list :empty-value-seq sexp)))
      (nelisp-phase47-compiler--make-ir 'value-seq
            :forms (mapcar (lambda (e)
                             (nelisp-phase47-compiler--parse-value
                              e env fenv defuns))
                           children))))
   ;; Doc 129.6D — first direct user-call lowering for one-argument
   ;; builtins.  The surrounding defun must expose the boxed-boundary
   ;; slots used by the 129.6B helper, so ordinary `(symbol-name arg)'
   ;; can lower to the same dispatcher sequence without a custom surface
   ;; form.
   ((and (consp sexp)
         (memq (car sexp)
               nelisp-phase47-compiler--aot-builtin1-delegation-symbols)
         (not (assq (car sexp) defuns)))
    (nelisp-phase47-compiler--parse-aot-builtin1-call
     sexp env fenv defuns))
   ;; Doc 129.6F — direct vararg builtin calls lower to the calln
   ;; dispatcher.  This uses the same boundary/name-slot convention as
   ;; builtin1 and relies on Doc 129.7G stack-GP spill support once the
   ;; fixed ABI prefix plus user args exceeds six GP arguments.
   ((and (consp sexp)
         (memq (car sexp)
               nelisp-phase47-compiler--aot-builtinn-delegation-symbols)
         (not (assq (car sexp) defuns)))
    (nelisp-phase47-compiler--parse-aot-builtinn-call
     sexp env fenv defuns))
   ;; Doc 129.7A — first higher-order dispatch surface.  This does not
   ;; build closures yet; it delegates an already-materialized function
   ;; designator/value plus one boxed Sexp argument to the runtime.
   ((and (consp sexp)
         (eq (car sexp) 'funcall)
         (not (assq 'funcall defuns)))
    (pcase (length sexp)
      (3 (nelisp-phase47-compiler--parse-aot-funcall1
          sexp env fenv defuns))
      (4 (nelisp-phase47-compiler--parse-aot-funcall2
          sexp env fenv defuns))
      (5 (nelisp-phase47-compiler--parse-aot-funcall3
          sexp env fenv defuns))
      (_ (if (> (length sexp) 5)
             (nelisp-phase47-compiler--parse-aot-funcalln
              sexp env fenv defuns)
           (signal 'nelisp-phase47-compiler-error
                   (list :aot-funcall-arity sexp))))))
   ;; Doc 129.7C — `apply' delegates a function designator/value plus an
   ;; already-materialized argument list to the runtime dispatcher.
   ((and (consp sexp)
         (eq (car sexp) 'apply)
         (not (assq 'apply defuns)))
    (nelisp-phase47-compiler--parse-aot-apply
     sexp env fenv defuns))
   ;; Doc 129.8B — first compiler surface for non-local exit forms.
   ;; These are still dispatcher bridges: the runtime returns a landing
   ;; descriptor in OUT on the elisp side, while native lowering will
   ;; eventually restore SP and jump to the returned landing pad.
   ((and (consp sexp)
         (eq (car sexp) 'throw)
         (not (assq 'throw defuns)))
    (nelisp-phase47-compiler--parse-aot-throw
     sexp env fenv defuns))
   ((and (consp sexp)
         (eq (car sexp) 'signal)
         (not (assq 'signal defuns)))
    (nelisp-phase47-compiler--parse-aot-signal
     sexp env fenv defuns))
   ;; Doc 129.8H/I — user-facing `error' is a specialised signal with
   ;; the standard `error' condition tag.  Multi-argument formatted
   ;; errors lower through the errorn bridge.
   ((and (consp sexp)
         (eq (car sexp) 'error)
         (not (assq 'error defuns)))
    (nelisp-phase47-compiler--parse-aot-error
     sexp env fenv defuns))
   ;; Doc 129.8E — first source-level handler synthesis.  This covers
   ;; normal-exit `catch' bodies only: the compiler emits balanced
   ;; push/body/pop control flow and rejects explicit non-local forms
   ;; until native landing-pad jumps are available.
   ((and (consp sexp)
         (eq (car sexp) 'catch)
         (not (assq 'catch defuns)))
    (nelisp-phase47-compiler--parse-aot-catch-normal-exit
     sexp env fenv defuns))
   ;; Doc 129.8F — source-level condition-case normal path.  Real
   ;; handler landing pads for signalled conditions remain pending.
   ((and (consp sexp)
         (eq (car sexp) 'condition-case)
         (not (assq 'condition-case defuns)))
    (nelisp-phase47-compiler--parse-aot-condition-case-normal-exit
     sexp env fenv defuns))
   ;; Doc 129.8G — source-level unwind-protect normal path.  Cleanup
   ;; forms run after BODY and the saved BODY value remains the result.
   ((and (consp sexp)
         (eq (car sexp) 'unwind-protect)
         (not (assq 'unwind-protect defuns)))
    (nelisp-phase47-compiler--parse-aot-unwind-protect-normal-exit
     sexp env fenv defuns))
   ;; Doc 129.8C — explicit bridge forms for installing handler-stack
   ;; records.  These are internal AOT forms used to make the native
   ;; handler ABI visible before full catch/condition-case/unwind-protect
   ;; source lowering lands.
   ((and (consp sexp)
         (memq (car sexp)
               '(aot-push-catch aot-push-condition aot-push-unwind)))
    (nelisp-phase47-compiler--parse-aot-push-handler
     sexp env fenv defuns))
   ((and (consp sexp)
         (eq (car sexp) 'aot-pop-handler))
    (nelisp-phase47-compiler--parse-aot-pop-handler
     sexp env fenv defuns))
   ;; Doc 129.5E — explicit native root-frame push/pop bridge forms.
   ;; Automatic prologue/epilogue insertion is a later backend step;
   ;; these internal forms make the ABI and relocations visible now.
   ((and (consp sexp)
         (eq (car sexp) 'aot-push-roots))
    (nelisp-phase47-compiler--parse-aot-push-roots
     sexp env fenv defuns))
   ((and (consp sexp)
         (eq (car sexp) 'aot-pop-roots))
    (nelisp-phase47-compiler--parse-aot-pop-roots
     sexp env fenv defuns))
   ;; Doc 129.4C — explicit special binding push/pop bridge forms.
   ;; Source `let' lowering still waits for full dynamic binding, but
   ;; the native ABI and object relocs are visible through these forms.
   ((and (consp sexp)
         (eq (car sexp) 'aot-push-special))
    (nelisp-phase47-compiler--parse-aot-push-special
     sexp env fenv defuns))
   ((and (consp sexp)
         (eq (car sexp) 'aot-pop-special))
    (nelisp-phase47-compiler--parse-aot-pop-special
     sexp env fenv defuns))
   ;; Doc 100 v2 §100.B Sexp ABI direct-access ops.  Each maps to a
   ;; fixed instruction template against `[base]' / `[base + 8]', with
   ;; the byte offset coming from `nelisp-sexp--offset-*' constants
   ;; defined in `lisp/nelisp-sexp-layout.el'.
   ;;
   ;; (sexp-tag PTR)            — read tag byte at offset 0,
   ;;                              zero-extended to i64 in rax.
   ;; (sexp-int-unwrap PTR)     — read i64 payload at offset 8 in rax.
   ;; (sexp-int-make SLOT N)    — write Sexp::Int(N) into SLOT,
   ;;                              return SLOT pointer in rax.
   ;;
   ;; Doc 49 Wave 11.2: hash-table primitives (parse-time desugar).
   ;;
   ;; `(hash-table-make TAG-PTR CAP SLOT)' → `(record-make TAG-PTR CAP SLOT)'
   ;; `(hash-table-put HT KEY VAL CONS-SLOT PAIR-SLOT)' → record-slot-set
   ;;     of (cons-make-with-clone (cons-make-with-clone KEY VAL)
   ;;                              (record-slot-ref ...))
   ;; `(hash-table-get HT KEY SLOT)' → call to `nelisp_ht_walk' helper
   ;; `(hash-table-contains-p HT KEY SLOT)' → (if (= (call ...) 0) 0 1)
   ;;
   ;; The desugar produces nested sexps that the existing parser
   ;; handles directly — no new emit functions or asm helpers are
   ;; required, and the Rust LOC delta = 0.  See the
   ;; `--ht-desugar-*' helpers above.  The bucket index is computed
   ;; via `(extern-call nelisp_fnv1a KEY)' so the `nelisp-cc-fnv1a'
   ;; defun must be linked into the compile unit (= shipped via the
   ;; existing `nelisp-cc-fnv1a' object) or the caller must inline
   ;; `(nelisp_ht_helpers-source)' source which depends on str-eq.
   ((and (consp sexp) (eq (car sexp) 'hash-table-make))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :hash-table-make-arity sexp)))
    (nelisp-phase47-compiler--parse-value
     (nelisp-phase47-compiler--ht-desugar-make
      (nth 2 sexp) (nth 3 sexp) (nth 1 sexp))
     env fenv defuns))
   ((and (consp sexp) (eq (car sexp) 'hash-table-put))
    (unless (= (length sexp) 6)
      (signal 'nelisp-phase47-compiler-error
              (list :hash-table-put-arity sexp)))
    (nelisp-phase47-compiler--parse-value
     (nelisp-phase47-compiler--ht-desugar-put
      (nth 1 sexp) (nth 2 sexp) (nth 3 sexp)
      (nth 4 sexp) (nth 5 sexp))
     env fenv defuns))
   ((and (consp sexp) (eq (car sexp) 'hash-table-get))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :hash-table-get-arity sexp)))
    (nelisp-phase47-compiler--parse-value
     (nelisp-phase47-compiler--ht-desugar-get
      (nth 1 sexp) (nth 2 sexp) (nth 3 sexp))
     env fenv defuns))
   ((and (consp sexp) (eq (car sexp) 'hash-table-contains-p))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :hash-table-contains-p-arity sexp)))
    (nelisp-phase47-compiler--parse-value
     (nelisp-phase47-compiler--ht-desugar-contains-p
      (nth 1 sexp) (nth 2 sexp) (nth 3 sexp))
     env fenv defuns))
   ;; Doc 49 Wave 11.1: (static-imm32-table-lookup NAME INDEX-EXPR)
   ;;
   ;; Runtime O(1) load of the u32 at `TABLE_NAME[INDEX]'.  NAME must
   ;; match a `static-imm32-table-define' declared in the same compile
   ;; unit.  INDEX-EXPR is any value-producing expr (= integer in rax).
   ;; Result class = gp (u32 zero-extended to i64 in rax).  Bounds
   ;; check is not emitted — caller responsibility (= Wave 11.2+ may
   ;; add a safe variant).
   ((and (consp sexp) (eq (car sexp) 'static-imm32-table-lookup))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :static-imm32-table-lookup-arity sexp)))
    (let ((name (nth 1 sexp)))
      (unless (stringp name)
        (signal 'nelisp-phase47-compiler-error
                (list :static-imm32-table-lookup-name-not-string name)))
      (nelisp-phase47-compiler--make-ir 'table-lookup
            :name name
            :index (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns))))
   ((and (consp sexp) (eq (car sexp) 'sexp-tag))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-tag-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-tag
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; (i64-to-f64 INT-EXPR) — G6 grammar bridge.  Converts a gp-class
   ;; signed i64 to an f64-class value via `CVTSI2SD xmm-dst, rax'.
   ;; Inverse of `f64-to-i64-trunc'.  Only meaningful as an f64-leaf
   ;; in f64-call / sexp-write-float / f64-to-i64-trunc contexts.
   ((and (consp sexp) (eq (car sexp) 'i64-to-f64))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :i64-to-f64-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'i64-to-f64
          :class 'f64
          :int-expr (nelisp-phase47-compiler--parse-value
                     (nth 1 sexp) env fenv defuns)))
   ;; (bits-to-f64 INT-EXPR) — G5 grammar bridge.  Lifts a gp-class
   ;; i64 (= raw bit pattern, typically from `sexp-float-unwrap') into
   ;; an f64-class value via `MOVQ xmm-dst, rax'.  Only meaningful as
   ;; an f64-leaf in f64-call / sexp-write-float / f64-to-i64-trunc
   ;; contexts; using it as a plain gp-class value-expr is an error.
   ((and (consp sexp) (eq (car sexp) 'bits-to-f64))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :bits-to-f64-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'bits-to-f64
          :class 'f64
          :int-expr (nelisp-phase47-compiler--parse-value
                     (nth 1 sexp) env fenv defuns)))
   ;; (f64-to-i64-trunc F64-EXPR) — G5 grammar bridge.  Truncates an
   ;; f64-class value to a signed i64 in rax via `CVTTSD2SI'.  F64-EXPR
   ;; must be an f64-leaf-into target (= ref :class f64, bits-to-f64,
   ;; f64-call, etc.).  Result class: gp.
   ((and (consp sexp) (eq (car sexp) 'f64-to-i64-trunc))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :f64-to-i64-trunc-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'f64-to-i64-trunc
          :f64-expr (nelisp-phase47-compiler--parse-value
                     (nth 1 sexp) env fenv defuns)))
   ;; (sexp-float-unwrap PTR) — read the 8-byte f64 payload of a
   ;; `Sexp::Float(f)' as raw bits, returned as i64 in rax (= xmm0
   ;; bit-pattern reinterpreted via MOVQ).  No tag check — caller
   ;; must ensure PTR points at a `Sexp::Float' variant.  Returns the
   ;; bit pattern (= can be transferred to xmm0 via a future MOVQ
   ;; grammar op for f64 arithmetic chains, or compared as raw u64
   ;; for NaN-bit equality).  Companion to the existing
   ;; `sexp-write-float SLOT VALUE' op (Doc 122.G).  G4 grammar.
   ((and (consp sexp) (eq (car sexp) 'sexp-float-unwrap))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-float-unwrap-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-float-unwrap
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-int-unwrap))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-int-unwrap-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-int-unwrap
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-int-make))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-int-make-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-int-make
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ;; ---- Doc 101 §101.B Cons read ops ----
   ((and (consp sexp) (eq (car sexp) 'cons-null-p))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-null-p-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-null-p
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-car))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-car-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-car
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-cdr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-cdr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-cdr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (memq (car sexp) '(cons-cdr-raw cons-cdr-raw-from-box)))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-cdr-raw-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-cdr-raw
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :from-box (eq (car sexp) 'cons-cdr-raw-from-box)))
   ((and (consp sexp) (eq (car sexp) 'sexp-payload-ptr))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-payload-ptr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-payload-ptr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-payload-ptr-record))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-payload-ptr-record-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-payload-ptr-record
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; ---- Doc 111 §111.B Record read+write ops ----
   ((and (consp sexp) (eq (car sexp) 'record-type-tag))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :record-type-tag-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-type-tag
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'record-slot-count))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :record-slot-count-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-slot-count
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'record-slot-ref))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :record-slot-ref-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-slot-ref
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'record-slot-ref-ptr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :record-slot-ref-ptr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-slot-ref-ptr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'record-slot-set))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :record-slot-set-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-slot-set
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 3 sexp) env fenv defuns)))
   ;; ---- Doc 111 §111.C Vector read ops ----
   ((and (consp sexp) (eq (car sexp) 'vector-len))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :vector-len-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'vector-len
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'vector-ref))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :vector-ref-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'vector-ref
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'vector-ref-ptr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :vector-ref-ptr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'vector-ref-ptr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ;; Doc 111 §111.E — `(vector-slot-set H N VAL-PTR)' refcount-safe
   ;; vector element overwrite.  Parallel to `record-slot-set' (§111.B).
   ;; Delegates to the Rust extern `nl_vector_set_slot' which drops the
   ;; old slot value (refcount-aware) then writes the new one.  Used by
   ;; `mirror_install_entry' (= Phase 47 helper #12) to prepend a fresh
   ;; cons cell onto a bucket without leaking the prior bucket head.
   ((and (consp sexp) (eq (car sexp) 'vector-slot-set))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :vector-slot-set-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'vector-slot-set
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 3 sexp) env fenv defuns)))
   ;; ---- Doc 111 §111.D Cell read+write ops ----
   ;; (cell-value H SLOT)     — copy the cell's current value (inline
   ;;                           Sexp at NlCell offset 0) into the
   ;;                           caller-owned SLOT.  Returns SLOT.
   ;; (cell-set-value H VAL)  — delegate to the Rust `nl_cell_set_value'
   ;;                           extern which does refcount-aware
   ;;                           drop-then-write on the cell's `value'.
   ;;                           Returns the original H pointer.
   ;; (cell-make VAL SLOT)    — allocate a fresh `NlCell' via
   ;;                           `nl_alloc_cell(VAL)' and write
   ;;                           `Sexp::Cell(box)' into SLOT.  Returns
   ;;                           SLOT.
   ;; (cell-null-p H)         — predicate: 1 iff the cell's value slot
   ;;                           currently holds `Sexp::Nil', else 0.
   ((and (consp sexp) (eq (car sexp) 'cell-value))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cell-value-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cell-value
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cell-set-value))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cell-set-value-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cell-set-value
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cell-make))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cell-make-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cell-make
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cell-null-p))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :cell-null-p-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cell-null-p
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; Doc 115 §115.1 — `(vector-make CAP SLOT)' allocates a fresh
   ;; `NlVector' via `nl_alloc_vector(CAP)' (Doc 111 §111.E) and
   ;; writes `Sexp::Vector(box)' (= tag byte + payload pointer) into
   ;; the caller-owned SLOT.  Returns SLOT in rax.  Parallel to
   ;; `cell-make' / `cons-make' for `Sexp::Cell' / `Sexp::Cons'.
   ;; CAP is evaluated as an i64; the new vector is pre-filled with
   ;; `Sexp::Nil' elements by `nl_alloc_vector'.
   ((and (consp sexp) (eq (car sexp) 'vector-make))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :vector-make-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'vector-make
          :cap (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ;; Doc 115 §115.3 — `(record-make TAG-PTR SLOT-COUNT SLOT)' allocates
   ;; a fresh `NlRecord' via `nl_alloc_record(TAG-PTR, SLOT-COUNT)' (Doc
   ;; 111 §111.E) and writes `Sexp::Record(box)' (= tag byte 12 + payload
   ;; pointer) into the caller-owned SLOT.  Returns SLOT in rax.  Parallel
   ;; to `vector-make' / `cell-make' for `Sexp::Vector' / `Sexp::Cell'.
   ;; TAG-PTR is a `*const Sexp' (typically a `Sexp::Symbol' identifying
   ;; the record kind); SLOT-COUNT is evaluated as an i64; the new record
   ;; has SLOT-COUNT slots all pre-filled with `Sexp::Nil' by
   ;; `nl_alloc_record'.
   ;; (cons-make-with-clone CAR-PTR CDR-PTR SLOT)
   ;;   — Doc 120.E fused constructor: allocate fresh `NlConsBox' via
   ;;     `nl_alloc_consbox', deep-clone `*CAR-PTR' into `box->car' and
   ;;     `*CDR-PTR' into `box->cdr' via the existing
   ;;     `nl_sexp_clone_into' extern (= variant-aware refcount inc /
   ;;     String deep copy), then write `Sexp::Cons(box)' into SLOT.
   ;;   Required because the simpler `cons-make' op does raw 32-byte
   ;;   SIMD copies that double-free the inner payloads when CAR/CDR
   ;;   are refcounted variants.  Replaces the Rust `nl_jit_cons_make'
   ;;   trampoline (= -24 LOC `build-tool/src/jit/cons.rs').
   ((and (consp sexp) (eq (car sexp) 'cons-make-with-clone))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-make-with-clone-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-make-with-clone
          :car-ptr (nelisp-phase47-compiler--parse-value
                    (nth 1 sexp) env fenv defuns)
          :cdr-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'record-make))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :record-make-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'record-make
          :tag-ptr (nelisp-phase47-compiler--parse-value
                    (nth 1 sexp) env fenv defuns)
          :slot-count (nelisp-phase47-compiler--parse-value
                       (nth 2 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ;; ---- Doc 101 §101.C Symbol/Str read ops ----
   ;; (str-len H)       — read String::len at offset 24 from a
   ;;                     `Sexp::Str' / `Sexp::Symbol' slot.
   ;; (str-bytes H)     — read String::ptr at offset 8.
   ;; (str-byte-at H N) — read byte `N' from the String::ptr buffer,
   ;;                     zero-extended to i64.
   ;; (str-eq H1 H2)    — compare lengths then bytes, return i64 0/1.
   ;; (symbol-eq H1 H2) — require both tags = `Sexp::Symbol', then
   ;;                     compare the underlying String payload bytes.
   ;; (sexp-write-nil SLOT) / (sexp-write-t SLOT)
   ;;                   — write only the tag byte of a caller-owned
   ;;                     result slot and return the slot pointer.
   ((and (consp sexp) (eq (car sexp) 'str-len))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :str-len-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-len
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-bytes))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :str-bytes-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-bytes
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; Doc 122 §122.H — `(str-bytes-ptr STR-PTR)' returns the raw
   ;; `*const u8' data pointer of a Sexp::Str / Sexp::Symbol /
   ;; Sexp::MutStr.  Unlike `str-bytes' (which is layout-coupled —
   ;; inline read at `nelisp-string--offset-ptr' that only works for
   ;; the inline-String variants), this op dispatches through the
   ;; Rust `nl_str_bytes_ptr' extern which uses the official
   ;; `String::as_ptr()' / `NlStr.value.as_ptr()' accessors and
   ;; covers all three string variants.  Pair with `str-len' to
   ;; get the matching byte count.  Unblocks the Doc 117 Tier B
   ;; I/O syscall sweep (= `write_stdout' / `write_stderr_line' /
   ;; `read_stdin' / `read_file' / `write_file' family).
   ((and (consp sexp) (eq (car sexp) 'str-bytes-ptr))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :str-bytes-ptr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-bytes-ptr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-byte-at))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :str-byte-at-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-byte-at
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :str-eq-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-eq
          :id (nelisp-phase47-compiler--gensym "str-eq")
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'symbol-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :symbol-eq-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'symbol-eq
          :id (nelisp-phase47-compiler--gensym "symbol-eq")
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ;; (symbol-name-eq SYM_PTR LITERAL_STR)
   ;;   — Tag-check SYM_PTR == `Sexp::Symbol' (offset 0 byte),
   ;;     length-check vs `(length LITERAL_STR)' bytes (at offset 24),
   ;;     then byte-loop compare against compile-time literal bytes
   ;;     loaded via `[r9]; add r9, 1'.  Returns i64 0 or 1.
   ;;   Allows multi-entry symbol-table dispatch (= `bi_syscall' 25-entry
   ;;   name→nr table) without forcing the caller to allocate N
   ;;   `Sexp::Symbol' stack literals.  LITERAL_STR is encoded UTF-8 at
   ;;   compile time and emitted inline as `cmp imm32' against each
   ;;   payload byte.
   ((and (consp sexp) (eq (car sexp) 'symbol-name-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :symbol-name-eq-arity sexp)))
    (let ((lit (nth 2 sexp)))
      (unless (stringp lit)
        (signal 'nelisp-phase47-compiler-error
                (list :symbol-name-eq-literal-must-be-string sexp)))
      (nelisp-phase47-compiler--make-ir 'symbol-name-eq
            :id (nelisp-phase47-compiler--gensym "symbol-name-eq")
            :ptr (nelisp-phase47-compiler--parse-value
                  (nth 1 sexp) env fenv defuns)
            :bytes (string-to-list (encode-coding-string lit 'utf-8 t)))))
   ;; (sexp-name-eq SEXP_PTR LITERAL_STR)
   ;;   — Like `symbol-name-eq' but accepts both `Sexp::Symbol' (tag 4) and
   ;;     `Sexp::Str' (tag 5) inputs.  Tag check is: byte[rdi]==4 OR byte[rdi]==5.
   ;;     Used to dispatch on names that callers pass either as quoted symbols
   ;;     or as unquoted string literals.
   ((and (consp sexp) (eq (car sexp) 'sexp-name-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-name-eq-arity sexp)))
    (let ((lit (nth 2 sexp)))
      (unless (stringp lit)
        (signal 'nelisp-phase47-compiler-error
                (list :sexp-name-eq-literal-must-be-string sexp)))
      (nelisp-phase47-compiler--make-ir 'sexp-name-eq
            :id (nelisp-phase47-compiler--gensym "sexp-name-eq")
            :ptr (nelisp-phase47-compiler--parse-value
                  (nth 1 sexp) env fenv defuns)
            :bytes (string-to-list (encode-coding-string lit 'utf-8 t)))))
   ((and (consp sexp) (eq (car sexp) 'sexp-write-nil))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-nil-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-write-nil
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-write-t))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-t-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-write-t
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)))
   ;; Doc 122 §122.A — `(sexp-write-str SLOT BYTES-PTR LEN)' allocates a
   ;; fresh `Sexp::Str(String)' via the Rust extern `nl_alloc_str' (=
   ;; copies LEN bytes from BYTES-PTR into a new `String', writes
   ;; `Sexp::Str(s)' into the caller-owned SLOT).  Returns SLOT in rax.
   ;; Parallel to `vector-make' / `cell-make' / `record-make' but the
   ;; allocator extern is 3-arg (= bytes_ptr, len, result_slot) and
   ;; writes the full 40-byte Sexp inline (= `Sexp::Str' carries its
   ;; `String' header inline at payload offset 8..32, not via an
   ;; `*mut NlStr' indirection — see `lisp/nelisp-sexp-layout.el'
   ;; `nelisp-string--offset-*' constants and the comment above
   ;; `nl_alloc_str' in `build-tool/src/eval/nlstr.rs').
   ((and (consp sexp) (eq (car sexp) 'sexp-write-str))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-str-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-write-str
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :bytes-ptr (nelisp-phase47-compiler--parse-value
                      (nth 2 sexp) env fenv defuns)
          :len (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ;; Doc 122 §122.A — `(sexp-write-symbol SLOT BYTES-PTR LEN)' — same
   ;; shape as `sexp-write-str' but the allocator extern is
   ;; `nl_alloc_symbol' (= writes `Sexp::Symbol(s)' instead of
   ;; `Sexp::Str(s)').  Does NOT consult any intern table — see Doc
   ;; 122 §5 open question.
   ((and (consp sexp) (eq (car sexp) 'sexp-write-symbol))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-symbol-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-write-symbol
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :bytes-ptr (nelisp-phase47-compiler--parse-value
                      (nth 2 sexp) env fenv defuns)
          :len (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ;; Doc 129.3B — `(sexp-write-symbol-lit SLOT LITERAL)' materializes
   ;; a `Sexp::Symbol' from compile-time UTF-8 bytes without using
   ;; `.rodata'.  The emitter builds a temporary stack byte buffer so
   ;; object-mode ET_REL output can still link cleanly.
   ((and (consp sexp) (eq (car sexp) 'sexp-write-symbol-lit))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-symbol-lit-arity sexp)))
    (let ((lit (nth 2 sexp)))
      (unless (stringp lit)
        (signal 'nelisp-phase47-compiler-error
                (list :sexp-write-symbol-lit-literal lit)))
      (nelisp-phase47-compiler--make-ir 'sexp-write-symbol-lit
            :slot (nelisp-phase47-compiler--parse-value
                   (nth 1 sexp) env fenv defuns)
            :bytes (string-to-list (encode-coding-string lit 'utf-8 t)))))
   ;; Doc 129.8I — `(sexp-write-str-lit SLOT LITERAL)' is the string
   ;; sibling used by formatted error lowering.  Like symbol literals,
   ;; it avoids `.rodata' by passing a temporary stack byte buffer to
   ;; the allocator.
   ((and (consp sexp) (eq (car sexp) 'sexp-write-str-lit))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-str-lit-arity sexp)))
    (let ((lit (nth 2 sexp)))
      (unless (stringp lit)
        (signal 'nelisp-phase47-compiler-error
                (list :sexp-write-str-lit-literal lit)))
      (nelisp-phase47-compiler--make-ir 'sexp-write-str-lit
            :slot (nelisp-phase47-compiler--parse-value
                   (nth 1 sexp) env fenv defuns)
            :bytes (string-to-list (encode-coding-string lit 'utf-8 t)))))
   ;; Doc 122 §122.G — `(sexp-write-float SLOT VALUE)' allocates a
   ;; `Sexp::Float(f64)' value inline (= tag byte 3, f64 payload at
   ;; offset 8, no heap box).  VALUE is f64-class — must be an f64
   ;; param ref (= the same flat-leaf constraint as `f64-call'
   ;; / `extern-call-f64' f64 args at MVP).  SLOT is i64-class
   ;; (= `*mut Sexp' pointer).  Lowers to `nl_sexp_write_float'
   ;; extern (SysV AMD64: rdi = slot, xmm0 = value) and returns SLOT
   ;; in rax.  Parallel to `sexp-write-str' / `sexp-int-make' but the
   ;; payload is f64 and the helper extern takes one i64 + one f64
   ;; rather than three i64 args.
   ((and (consp sexp) (eq (car sexp) 'sexp-write-float))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-float-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'sexp-write-float
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :value (nelisp-phase47-compiler--parse-value
                  (nth 2 sexp) env fenv defuns)))
   ;; Doc 122 §122.B — Mutable string builder grammar.
   ;; ------------------------------------------------
   ;; `(mut-str-make-empty SLOT CAP)' — call `nl_alloc_mut_str(cap, slot)'
   ;; which allocates a fresh `NlStrRef::new(String::with_capacity(cap))'
   ;; and writes `Sexp::MutStr(rc)' (= tag at offset 0, box ptr at
   ;; offset 8) into the caller-owned SLOT.  Returns SLOT in rax.
   ((and (consp sexp) (eq (car sexp) 'mut-str-make-empty))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :mut-str-make-empty-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'mut-str-make-empty
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :cap (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ;; `(mut-str-push-byte PTR BYTE)' — append a single byte (low 8
   ;; bits of BYTE) to the MutStr at PTR (= `*mut Sexp' slot
   ;; carrying `Sexp::MutStr').  Returns rax = 1 sentinel so the op
   ;; composes cleanly in `(and SIDE-EFFECT VALUE)' chains.
   ((and (consp sexp) (eq (car sexp) 'mut-str-push-byte))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :mut-str-push-byte-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'mut-str-push-byte
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :byte (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ;; `(mut-str-push-codepoint PTR CP)' — UTF-8 encode CP into 1-4
   ;; bytes and append.  Out-of-range / surrogate codepoints clamp
   ;; to U+FFFD inside the Rust extern.  Returns rax = 1 sentinel.
   ((and (consp sexp) (eq (car sexp) 'mut-str-push-codepoint))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :mut-str-push-codepoint-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'mut-str-push-codepoint
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :cp (nelisp-phase47-compiler--parse-value
               (nth 2 sexp) env fenv defuns)))
   ;; `(mut-str-len PTR)' — current byte length of the MutStr at
   ;; PTR.  Returns the i64 length in rax (= `String::len').
   ((and (consp sexp) (eq (car sexp) 'mut-str-len))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :mut-str-len-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'mut-str-len
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; `(mut-str-finalize PTR SLOT)' — clone the MutStr's current
   ;; bytes into a fresh `Sexp::Str(String)' and write to the
   ;; caller-owned SLOT.  The source MutStr remains live + push-able
   ;; (= clone semantics, not move) so the Reader lexer can take
   ;; intermediate token snapshots.  Returns SLOT in rax.
   ((and (consp sexp) (eq (car sexp) 'mut-str-finalize))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :mut-str-finalize-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'mut-str-finalize
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ;; Doc 122 §122.D — UTF-8 helper grammar.
   ;; ---------------------------------------
   ;; `(str-char-count STR)' — count UTF-8 codepoints (NOT bytes)
   ;; in the `Sexp::Str' at STR.  Calls `nl_str_char_count(str_ptr)
   ;; -> i64' which delegates to `s.chars().count()'.  Used by the
   ;; elisp `length' shim's String arm (= `(length "藤澤")' = 2,
   ;; not 6).
   ((and (consp sexp) (eq (car sexp) 'str-char-count))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :str-char-count-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-char-count
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ;; `(str-codepoint-at STR I CP-SLOT WIDTH-SLOT)' — decode the
   ;; codepoint at byte index I in STR and write codepoint /
   ;; byte-width via the two caller-supplied i64 out-slots.
   ;; Returns rax = 1 on success, 0 on invalid I / malformed UTF-8.
   ;; The dual-slot return convention avoids needing tuple-typed
   ;; Sexps for the common (cp, width) decode case.
   ((and (consp sexp) (eq (car sexp) 'str-codepoint-at))
    (unless (= (length sexp) 5)
      (signal 'nelisp-phase47-compiler-error
              (list :str-codepoint-at-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-codepoint-at
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)
          :cp-slot (nelisp-phase47-compiler--parse-value
                    (nth 3 sexp) env fenv defuns)
          :width-slot (nelisp-phase47-compiler--parse-value
                       (nth 4 sexp) env fenv defuns)))
   ;; `(str-is-alphanumeric-at STR I)' — predicate.  ASCII fast
   ;; path checks the byte at I directly; multi-byte slow path
   ;; decodes the codepoint and calls `char::is_alphanumeric'.
   ;; Returns i64 1/0 in rax.  Used by `nl_jit_split_by_non_alnum'
   ;; / Reader lexer char-class checks.
   ((and (consp sexp) (eq (car sexp) 'str-is-alphanumeric-at))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :str-is-alphanumeric-at-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'str-is-alphanumeric-at
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ;; Doc 122 §122.E — Atomic + raw memory primitives.
   ;; ------------------------------------------------
   ;; `(atomic-fetch-add PTR DELTA)' — SeqCst atomic fetch-and-add on
   ;; the `*mut i64' slot at PTR.  Returns the pre-add value in rax
   ;; (= match the underlying Rust `AtomicI64::fetch_add' contract).
   ;; Substrate gate for Doc 123 refcount elisp化.
   ((and (consp sexp) (eq (car sexp) 'atomic-fetch-add))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :atomic-fetch-add-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'atomic-fetch-add
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :delta (nelisp-phase47-compiler--parse-value
                  (nth 2 sexp) env fenv defuns)))
   ;; `(atomic-compare-exchange PTR EXPECTED NEW)' — SeqCst CAS on
   ;; the `*mut i64' slot at PTR.  Returns 1 on success / 0 on
   ;; mismatch.  Substrate gate for Doc 123 Bacon-Rajan promote.
   ((and (consp sexp) (eq (car sexp) 'atomic-compare-exchange))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :atomic-compare-exchange-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'atomic-compare-exchange
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :expected (nelisp-phase47-compiler--parse-value
                     (nth 2 sexp) env fenv defuns)
          :new-val (nelisp-phase47-compiler--parse-value
                    (nth 3 sexp) env fenv defuns)))
   ;; `(ptr-read-u64 PTR OFFSET)' — raw `u64' read at `*(u64*)(ptr +
   ;; offset)'.  Returns the value re-cast to `i64' in rax.
   ;; Substrate gate for Doc 124 nl*.rs header walks.
   ((and (consp sexp) (eq (car sexp) 'ptr-read-u64))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-read-u64-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-read-u64
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)))
   ;; `(ptr-write-u64 PTR OFFSET VAL)' — raw `u64' store at
   ;; `*(u64*)(ptr + offset)'.  Returns rax = 1 sentinel.
   ((and (consp sexp) (eq (car sexp) 'ptr-write-u64))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-write-u64-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-write-u64
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ;; `(ptr-read-u8 PTR OFFSET)' — raw `u8' read; zero-extends to i64
   ;; (= no sign extension).
   ((and (consp sexp) (eq (car sexp) 'ptr-read-u8))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-read-u8-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-read-u8
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)))
   ;; `(ptr-write-u8 PTR OFFSET VAL)' — raw `u8' store.  Returns 1
   ;; sentinel.
   ((and (consp sexp) (eq (car sexp) 'ptr-write-u8))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-write-u8-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-write-u8
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ;; Doc 122 §122.J — width-{2,4} raw-mem primitives.  Fill the SIZE
   ;; gap between `_u8' and `_u64' for marshalling libc structs whose
   ;; fields are mixed-width integers (= `winsize.ws_row' / `pollfd.fd'
   ;; / `termios.c_iflag').  Same shape as `_u8' / `_u64' modulo helper
   ;; name + accessor width.  All loads zero-extend to i64 (= no sign
   ;; extension); all stores write the low N bits of `val'.  Unaligned
   ;; access tolerated (= libc struct fields are not guaranteed to be
   ;; naturally aligned inside a raw byte buffer).
   ((and (consp sexp) (eq (car sexp) 'ptr-read-u16))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-read-u16-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-read-u16
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'ptr-write-u16))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-write-u16-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-write-u16
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'ptr-read-u32))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-read-u32-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-read-u32
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'ptr-write-u32))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :ptr-write-u32-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'ptr-write-u32
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :offset (nelisp-phase47-compiler--parse-value
                   (nth 2 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 3 sexp) env fenv defuns)))
   ;; Doc 122 §122.J — struct-by-value sugar.  Desugar to existing
   ;; primitives at parse time:
   ;;
   ;;   (struct-make TAG SIZE ALIGN) → (alloc-bytes SIZE ALIGN)
   ;;     TAG is a symbol literal used for diagnostic printing only;
   ;;     no layout effect.  SIZE / ALIGN must be compile-time integer
   ;;     constants (= same restriction as `alloc-bytes' itself).
   ;;
   ;;   (struct-field-set BUF OFFSET SIZE VALUE)
   ;;     → (ptr-write-uN BUF OFFSET VALUE) where N ∈ {8, 16, 32, 64}
   ;;     chosen by the compile-time SIZE constant (1, 2, 4, 8 bytes).
   ;;
   ;;   (struct-field-get BUF OFFSET SIZE)
   ;;     → (ptr-read-uN BUF OFFSET) chosen the same way.
   ;;
   ;; SIZE constants outside {1, 2, 4, 8} signal `struct-field-bad-size'.
   ;; Caller is responsible for SIZE / OFFSET correctness against the
   ;; libc struct's layout — no runtime check.
   ((and (consp sexp) (eq (car sexp) 'struct-make))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :struct-make-arity sexp)))
    ;; TAG (nth 1) is ignored at code-gen — diagnostic only.  We don't
    ;; validate it past arity to keep the surface forgiving (callers
    ;; routinely pass quoted symbols like ''sigaction).
    (nelisp-phase47-compiler--parse-value
     (list 'alloc-bytes (nth 2 sexp) (nth 3 sexp))
     env fenv defuns))
   ((and (consp sexp) (eq (car sexp) 'struct-field-set))
    (unless (= (length sexp) 5)
      (signal 'nelisp-phase47-compiler-error
              (list :struct-field-set-arity sexp)))
    (let ((size (nth 3 sexp)))
      (unless (nelisp-phase47-compiler--int-foldable-p size env fenv)
        (signal 'nelisp-phase47-compiler-error
                (list :struct-field-set-size-not-const sexp)))
      (let* ((size-val (nelisp-phase47-compiler--fold-int size env))
             (op (pcase size-val
                   (1 'ptr-write-u8)
                   (2 'ptr-write-u16)
                   (4 'ptr-write-u32)
                   (8 'ptr-write-u64)
                   (_ (signal 'nelisp-phase47-compiler-error
                              (list :struct-field-bad-size size-val sexp))))))
        (nelisp-phase47-compiler--parse-value
         (list op (nth 1 sexp) (nth 2 sexp) (nth 4 sexp))
         env fenv defuns))))
   ((and (consp sexp) (eq (car sexp) 'struct-field-get))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :struct-field-get-arity sexp)))
    (let ((size (nth 3 sexp)))
      (unless (nelisp-phase47-compiler--int-foldable-p size env fenv)
        (signal 'nelisp-phase47-compiler-error
                (list :struct-field-get-size-not-const sexp)))
      (let* ((size-val (nelisp-phase47-compiler--fold-int size env))
             (op (pcase size-val
                   (1 'ptr-read-u8)
                   (2 'ptr-read-u16)
                   (4 'ptr-read-u32)
                   (8 'ptr-read-u64)
                   (_ (signal 'nelisp-phase47-compiler-error
                              (list :struct-field-bad-size size-val sexp))))))
        (nelisp-phase47-compiler--parse-value
         (list op (nth 1 sexp) (nth 2 sexp))
         env fenv defuns))))
   ;; Doc 125 §125.A — alloc / dealloc grammar primitives.
   ;; ------------------------------------------------
   ;; `(alloc-bytes SIZE ALIGN)' — generic byte-level allocator
   ;; wrapping `std::alloc::alloc(Layout::from_size_align(size, align))'.
   ;; Returns the freshly-allocated `*mut u8' re-cast to `i64' in rax
   ;; (= 0 on layout error or OOM).  Substrate gate for Doc 126-128
   ;; bridge GC arena code; the Doc 124 NlBox Drop kernels use the
   ;; per-type §125.B-F externs instead.
   ((and (consp sexp) (eq (car sexp) 'alloc-bytes))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :alloc-bytes-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'alloc-bytes
          :size (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :align (nelisp-phase47-compiler--parse-value
                  (nth 2 sexp) env fenv defuns)))
   ;; `(dealloc-bytes PTR SIZE ALIGN)' — generic byte-level deallocator
   ;; wrapping `std::alloc::dealloc(ptr, Layout::from_size_align(size,
   ;; align))'.  Returns rax = 1 sentinel for `and'-chain composition.
   ;; Substrate gate for Doc 124.G-K NlBox Drop kernels (= the
   ;; if-zero-refcount free branch).  Layout must match the matching
   ;; `alloc-bytes' call; mismatch is UB per `std::alloc::dealloc'.
   ((and (consp sexp) (eq (car sexp) 'dealloc-bytes))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :dealloc-bytes-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'dealloc-bytes
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :size (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)
          :align (nelisp-phase47-compiler--parse-value
                  (nth 3 sexp) env fenv defuns)))
   ;; Doc 125 §125.B — inline Linux syscall.
   ;; `(syscall-direct NR A0 A1 A2 A3 A4 A5)' — emit inline SYSCALL
   ;; instruction with rax=NR, rdi=A0, rsi=A1, rdx=A2, r10=A3, r8=A4,
   ;; r9=A5.  Returns kernel return value in rax (-errno on error).
   ;; All seven arguments are mandatory; pass 0 for unused trailing args.
   ;; align is ignored (mmap is always page-aligned).  This op bypasses
   ;; Rust's global allocator entirely.  Linux x86_64 only.
   ((and (consp sexp) (eq (car sexp) 'syscall-direct))
    (unless (= (length sexp) 8)
      (signal 'nelisp-phase47-compiler-error
              (list :syscall-direct-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'syscall-direct
          :nr   (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :a0   (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)
          :a1   (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)
          :a2   (nelisp-phase47-compiler--parse-value
                 (nth 4 sexp) env fenv defuns)
          :a3   (nelisp-phase47-compiler--parse-value
                 (nth 5 sexp) env fenv defuns)
          :a4   (nelisp-phase47-compiler--parse-value
                 (nth 6 sexp) env fenv defuns)
          :a5   (nelisp-phase47-compiler--parse-value
                 (nth 7 sexp) env fenv defuns)))
   ;; ---- Doc 101 §101.D Cons construction ops ----
   ;; MVP refcount note: these ops byte-copy whole 32-byte `Sexp'
   ;; payloads into a fresh `NlConsBox' and therefore assume the input
   ;; values are caller-owned / already cloned on the Rust side.  They
   ;; do not perform `nl_rc_inc' on nested boxed variants yet; that
   ;; lands in a follow-up §101.D.2 stage if needed.
   ((and (consp sexp) (eq (car sexp) 'cons-make))
    (unless (= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-make-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-make
          :car-ptr (nelisp-phase47-compiler--parse-value
                    (nth 1 sexp) env fenv defuns)
          :cdr-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 3 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-set-car))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-set-car-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-set-car
          :handle (nelisp-phase47-compiler--parse-value
                   (nth 1 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-set-cdr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-set-cdr-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'cons-set-cdr
          :handle (nelisp-phase47-compiler--parse-value
                   (nth 1 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)))
   ;; (f64-call SYM ARG) — Doc 110 §110.E.2 / Doc 110 §3.F.
   ;; 1-arg f64→f64 extern call (= the shape `exp' / `log' / other
   ;; libm unary functions use).  ARG must be an f64-class value
   ;; (= ref to f64 param at MVP).  Result returns in xmm0 / d0
   ;; per SysV / AAPCS f64 ABI; no further materialisation needed.
   ;; Records a PLT32 (x86_64) / R_AARCH64_CALL26 (aarch64) reloc
   ;; against SYM so the linker resolves to the matching libm /
   ;; static archive entry.
   ((and (consp sexp) (eq (car sexp) 'f64-call))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :f64-call-arity sexp)))
    (let ((name (nth 1 sexp))
          (arg (nth 2 sexp)))
      (unless (symbolp name)
        (signal 'nelisp-phase47-compiler-error
                (list :f64-call-name-not-symbol name)))
      (nelisp-phase47-compiler--make-ir 'f64-call
            :name name
            :arg (nelisp-phase47-compiler--parse-value
                  arg env fenv defuns))))
   ;; (extern-call SYM ARG...) — Doc 100 §100.A / Doc 122 §122.C call
   ;; into a C-callable extern symbol.  SYM becomes an SHN_UNDEF entry
   ;; in the output .o's symtab; the rel32 placeholder gets an
   ;; R_X86_64_PLT32 relocation that the linker resolves at static-
   ;; link time against another object (typically a Rust `.rlib'
   ;; exporting `#[no_mangle] pub extern "C"' helpers, or libc /
   ;; libm).
   ;;
   ;; Doc 122 §122.C — args may be bare (= legacy `:i64', backward-
   ;; compatible with all existing `extern-call' call sites) or
   ;; type-annotated `(:i64 EXPR)' / `(:f64 EXPR)' / `(:varargs
   ;; (:TYPE V) ...)' plists.  The classifier dispatches i64 → rdi-
   ;; r9 GP regs, f64 → xmm0-7 per SysV AMD64.  Varargs entries are
   ;; appended after the fixed args and the f64-count is recorded so
   ;; emit can set the AL register before the `call' instruction
   ;; (= SysV ABI: AL = number of vector args for variadic
   ;; functions).  Return type is i64 (= read from rax); use
   ;; `extern-call-f64' for f64 return (= read from xmm0).
   ((and (consp sexp) (memq (car sexp) '(extern-call extern-call-f64)))
    (when (< (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :extern-call-needs-symbol sexp)))
    (let* ((op (car sexp))
           (name (nth 1 sexp))
           (raw-args (nthcdr 2 sexp))
           (parsed (nelisp-phase47-compiler--parse-extern-call-args
                    op name raw-args env fenv defuns)))
      (unless (symbolp name)
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-name-not-symbol name)))
      (nelisp-phase47-compiler--make-ir 'extern-call
            :name name
            :ret-class (if (eq op 'extern-call-f64) 'f64 'gp)
            :args (plist-get parsed :args)
            :varargs-p (plist-get parsed :varargs-p)
            :f64-count (plist-get parsed :f64-count))))
   ;; (syscall-direct NR A0 A1 A2 A3 A4 A5) — Linux x86_64 raw SYSCALL.
   ;;
   ;; Evaluates 7 i64 value expressions and emits the Linux SYSCALL
   ;; instruction with the kernel ABI register placement:
   ;;   rax = NR (syscall number)
   ;;   rdi = A0, rsi = A1, rdx = A2, r10 = A3, r8 = A4, r9 = A5
   ;; Returns the kernel's result in rax (negative = -errno on error).
   ;;
   ;; Linux x86_64 only.  Signals `:syscall-direct-aarch64-unsupported'
   ;; at compile time on aarch64 targets (matching the `--emit-aarch64-
   ;; unsupported' pattern used by other x86_64-only ops).
   ;;
   ;; Byte-count is fixed across pass-1 and pass-2: 7 × (emit_arg + push)
   ;; + 7 × pop + SYSCALL = deterministic sizes independent of arg content.
   ((and (consp sexp) (eq (car sexp) 'syscall-direct))
    (unless (= (length sexp) 8)
      (signal 'nelisp-phase47-compiler-error
              (list :syscall-direct-arity sexp)))
    (nelisp-phase47-compiler--make-ir 'syscall-direct
          :nr (nelisp-phase47-compiler--parse-value
               (nth 1 sexp) env fenv defuns)
          :a0 (nelisp-phase47-compiler--parse-value
               (nth 2 sexp) env fenv defuns)
          :a1 (nelisp-phase47-compiler--parse-value
               (nth 3 sexp) env fenv defuns)
          :a2 (nelisp-phase47-compiler--parse-value
               (nth 4 sexp) env fenv defuns)
          :a3 (nelisp-phase47-compiler--parse-value
               (nth 5 sexp) env fenv defuns)
          :a4 (nelisp-phase47-compiler--parse-value
               (nth 6 sexp) env fenv defuns)
          :a5 (nelisp-phase47-compiler--parse-value
               (nth 7 sexp) env fenv defuns)))
   ;; Function call (= head is a defined function name).
   ((and (consp sexp) (symbolp (car sexp))
         (assq (car sexp) defuns))
    (let* ((name (car sexp))
           (signature (cdr (assq name defuns)))
           (arity (nelisp-phase47-compiler--defun-signature-arity
                   signature))
           (args (cdr sexp)))
      (when (> arity (length nelisp-phase47-compiler--arg-regs))
        (signal 'nelisp-phase47-compiler-error
                (list :too-many-args name arity)))
      (if (nelisp-phase47-compiler--defun-signature-rest-p signature)
          (let* ((fixed-count
                  (nelisp-phase47-compiler--defun-signature-fixed-count
                   signature))
                 (given (length args)))
            (when (< given fixed-count)
              (signal 'nelisp-phase47-compiler-error
                      (list :call-arity-mismatch name
                            :expected-at-least fixed-count
                            :got given)))
            (let* ((boundary
                    (nelisp-phase47-compiler--aot-funcall1-boundary-symbols
                     fenv sexp))
                   (out (plist-get boundary :out))
                   (mirror (plist-get boundary :mirror))
                   (frames (plist-get boundary :frames))
                   (scratch (plist-get boundary :scratch))
                   (fixed-args (cl-subseq args 0 fixed-count))
                   (rest-args (nthcdr fixed-count args))
                   (list-node
                    (nelisp-phase47-compiler--parse-value
                     `(extern-call nelisp_aot_listn
                                   ,mirror ,frames ,(length rest-args)
                                   ,scratch ,out ,@rest-args)
                     env fenv defuns))
                   (call-node
                    (nelisp-phase47-compiler--make-ir 'call
                          :name name
                          :args (mapcar
                                 (lambda (a)
                                   (nelisp-phase47-compiler--parse-value
                                    a env fenv defuns))
                                 (append fixed-args (list scratch))))))
              (nelisp-phase47-compiler--make-ir 'value-seq
                    :forms (list list-node call-node))))
        (unless (= (length args) arity)
          (signal 'nelisp-phase47-compiler-error
                  (list :call-arity-mismatch name
                        :expected arity :got (length args))))
        (nelisp-phase47-compiler--make-ir 'call
              :name name
              :args (mapcar (lambda (a)
                              (nelisp-phase47-compiler--parse-value
                               a env fenv defuns))
                            args)))))
   ;; (let ((VAR VAL)) BODY) — value context (= inside defun body).
   ;; Compile-time-foldable values fold into ENV (= compile-time fold
   ;; path, same as `--parse-stmt'); non-foldable values allocate a
   ;; runtime frame slot (= `let-rt' IR node) so the var is reachable
   ;; via the existing `ref' slot-load mechanism.  Requires an enclosing
   ;; `defun' parse to have established `--next-rt-let-slot'.
   ((and (consp sexp) (eq (car sexp) 'let))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :let-arity sexp)))
    (let ((bindings (nth 1 sexp))
          (body-sexp (nth 2 sexp)))
      (if (not (and (consp bindings) (= (length bindings) 1)))
          (nelisp-phase47-compiler--parse-multi-let
           bindings body-sexp env fenv defuns
           #'nelisp-phase47-compiler--parse-value)
        (let* ((pair (nelisp-phase47-compiler--validate-let-binding
                      (car bindings)))
               (var (nth 0 pair))
               (val-sexp (nth 1 pair))
               (root-p (nth 2 pair)))
          (if (nelisp-phase47-compiler--special-var-p var)
              (nelisp-phase47-compiler--parse-aot-special-let-normal-exit
               var val-sexp body-sexp env fenv defuns sexp)
            (nelisp-phase47-compiler--check-let-var-lexical var)
            (if (nelisp-phase47-compiler--int-foldable-p val-sexp env fenv)
              ;; Compile-time path: fold and extend ENV as before.
              (let* ((val (nelisp-phase47-compiler--fold-int val-sexp env))
                     (new-env (cons (cons var val) env)))
                (nelisp-phase47-compiler--parse-value
                 body-sexp new-env fenv defuns))
            ;; Runtime path: allocate a frame slot, emit a `let-rt' node.
            (unless nelisp-phase47-compiler--next-rt-let-slot
              (signal 'nelisp-phase47-compiler-error
                      (list :let-rt-requires-defun-context var)))
            (let* ((slot (car nelisp-phase47-compiler--next-rt-let-slot))
                   (_ (setcar nelisp-phase47-compiler--next-rt-let-slot
                              (1+ slot)))
                   (val-ir (nelisp-phase47-compiler--parse-value
                            val-sexp env fenv defuns))
                   ;; Extend FENV: var → slot (gp class, no reg = not
                   ;; a param but loads via the same `ref' mechanism).
                   (new-fenv (cons (cons var (list :slot slot :class 'gp
                                                   :root-p root-p))
                                   fenv))
                   (body-ir (nelisp-phase47-compiler--parse-value
                             body-sexp env new-fenv defuns)))
              (nelisp-phase47-compiler--make-ir 'let-rt
                    :var var
                    :slot slot
                    :value-ir val-ir
                    :body body-ir))))))))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :not-value-expr sexp)))))

(defun nelisp-phase47-compiler--parse-stmt (sexp env fenv defuns)
  "Parse SEXP as a statement-form into Doc 97 IR.
ENV is the let-alist of constants, FENV is the param->register
alist for the enclosing function (= nil at top level), DEFUNS is
the alist of `((NAME . ARITY) ...)' for already-defined functions.
Returns one of:
  (:kind write :str S)
  (:kind exit  :value IR)        ; IR is a value-producing node
  (:kind seq   :forms (NODE...))
  (:kind let   :var V :value N :body NODE)
  (:kind defun :name N :params P :param-regs R :body NODE)
  (:kind call  :name N :args (...))  ; void context = discard rax"
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
      (nelisp-phase47-compiler--make-ir 'write :str arg)))
   ;; Doc 49 Wave 11.1: (static-imm32-table-define NAME (E1 E2 ...))
   ;;
   ;; Declares a build-time u32 array in .rodata, indexable by NAME.
   ;; Each element must be an integer literal in [-2^31, 2^32 - 1]
   ;; (= fits one signed or unsigned i32).  NAME is a string (= the
   ;; lookup key); duplicate defines raise an error during the
   ;; collector pass.  Emits no text bytes — purely a rodata
   ;; declaration.  Statement-only (= no value-producing form).
   ((and (consp sexp) (eq (car sexp) 'static-imm32-table-define))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :static-imm32-table-define-arity sexp)))
    (let ((name (nth 1 sexp))
          (elements (nth 2 sexp)))
      (unless (stringp name)
        (signal 'nelisp-phase47-compiler-error
                (list :static-imm32-table-define-name-not-string name)))
      (unless (and (listp elements) (consp elements))
        (signal 'nelisp-phase47-compiler-error
                (list :static-imm32-table-define-elements-not-list
                      elements)))
      (dolist (e elements)
        (unless (and (integerp e)
                     (<= (- (ash 1 31)) e (1- (ash 1 32))))
          (signal 'nelisp-phase47-compiler-error
                  (list :static-imm32-table-define-element-out-of-range
                        e))))
      (nelisp-phase47-compiler--make-ir 'table-define :name name :elements elements)))
   ;; (exit VALUE-EXPR)
   ((and (consp sexp) (eq (car sexp) 'exit))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :exit-arity sexp)))
    (let* ((arg (nth 1 sexp))
           (vnode
            (nelisp-phase47-compiler--parse-value arg env fenv defuns)))
      (when (eq (nelisp-phase47-compiler--ir-kind vnode) 'imm)
        (let ((n (nelisp-phase47-compiler--ir-get vnode :value)))
          (unless (and (integerp n) (<= 0 n 255))
            (signal 'nelisp-phase47-compiler-error
                    (list :status-out-of-range n)))))
      (nelisp-phase47-compiler--make-ir 'exit :value vnode)))
   ;; (seq EXPR...)
   ((and (consp sexp) (eq (car sexp) 'seq))
    (let ((children (cdr sexp)))
      (when (null children)
        (signal 'nelisp-phase47-compiler-error
                (list :empty-seq sexp)))
      ;; Pre-scan for defun forms so later children can call them.
      ;; Defun bodies parse with the same updated defuns table so
      ;; one defun can call another defined earlier in the same seq.
      (let ((acc nil)
            (cur-defuns defuns))
        (dolist (c children)
          (when (and (consp c) (eq (car c) 'defun))
            (unless (>= (length c) 4)
              (signal 'nelisp-phase47-compiler-error
                      (list :defun-arity c)))
            (let ((nm (nth 1 c))
                  (ps (nth 2 c)))
              (unless (symbolp nm)
                (signal 'nelisp-phase47-compiler-error
                        (list :defun-name-not-symbol nm)))
              (unless (listp ps)
                (signal 'nelisp-phase47-compiler-error
                        (list :defun-params-not-list ps)))
              (when (assq nm cur-defuns)
                (signal 'nelisp-phase47-compiler-error
                        (list :duplicate-defun nm)))
              (setq cur-defuns
                    (cons (cons nm
                                (nelisp-phase47-compiler--defun-signature
                                 ps c))
                          cur-defuns)))))
        (dolist (c children)
          (push (nelisp-phase47-compiler--parse-stmt
                 c env fenv cur-defuns)
                acc))
        (nelisp-phase47-compiler--make-ir 'seq :forms (nreverse acc)))))
   ;; (let ((VAR VAL)) BODY)
   ((and (consp sexp) (eq (car sexp) 'let))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :let-arity sexp)))
    (let ((bindings (nth 1 sexp))
          (body (nth 2 sexp)))
      (if (not (and (consp bindings) (= (length bindings) 1)))
          (nelisp-phase47-compiler--parse-multi-let
           bindings body env fenv defuns
           #'nelisp-phase47-compiler--parse-stmt)
        (let* ((pair (nelisp-phase47-compiler--validate-let-binding
                      (car bindings)))
               (var (nth 0 pair))
               (val-sexp (nth 1 pair))
               (root-p (nth 2 pair)))
          (if (nelisp-phase47-compiler--special-var-p var)
              (nelisp-phase47-compiler--parse-aot-special-let-normal-exit
               var val-sexp body env fenv defuns sexp)
            (nelisp-phase47-compiler--check-let-var-lexical var)
            (if (nelisp-phase47-compiler--int-foldable-p val-sexp env fenv)
              (let* ((val (nelisp-phase47-compiler--fold-int val-sexp env))
                     (new-env (cons (cons var val) env))
                     (body-ir (nelisp-phase47-compiler--parse-stmt
                               body new-env fenv defuns)))
                (nelisp-phase47-compiler--make-ir 'let :var var :value val :body body-ir))
            ;; Runtime path: mirrors the `--parse-value' `let-rt' path.
            ;; Statement-context `let-rt' also requires an enclosing defun.
            (unless nelisp-phase47-compiler--next-rt-let-slot
              (signal 'nelisp-phase47-compiler-error
                      (list :let-rt-requires-defun-context var)))
            (let* ((slot (car nelisp-phase47-compiler--next-rt-let-slot))
                   (_ (setcar nelisp-phase47-compiler--next-rt-let-slot
                              (1+ slot)))
                   (val-ir (nelisp-phase47-compiler--parse-value
                            val-sexp env fenv defuns))
                   (new-fenv (cons (cons var (list :slot slot :class 'gp
                                                   :root-p root-p))
                                   fenv))
                   (body-ir (nelisp-phase47-compiler--parse-stmt
                             body env new-fenv defuns)))
              (nelisp-phase47-compiler--make-ir 'let-rt
                    :var var
                    :slot slot
                    :value-ir val-ir
                    :body body-ir))))))))
   ;; (defun NAME (PARAMS...) BODY)
   ;;
   ;; Doc 110 §110.E.1: PARAMS accept either bare symbols (= GP /
   ;; i64 class, existing) or annotated plists `(SYM :type f64)'
   ;; (= xmm register class).  Mixed-class defuns are rejected at
   ;; MVP scope — all params must share one class.  The defun's
   ;; class governs which reg pool the prologue allocates from
   ;; (`--arg-regs' for GP, `--xmm-arg-regs' for f64).
   ((and (consp sexp) (eq (car sexp) 'defun))
    (unless (>= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-arity sexp)))
    (let* ((name (nth 1 sexp))
           (param-info (nelisp-phase47-compiler--normalize-defun-params
                        (nth 2 sexp) sexp))
           (param-forms (plist-get param-info :params))
           (body (nth 3 sexp))
           (arity (length param-forms))
           ;; Extract (sym class root-p) triples.  Class is `gp' for
           ;; bare symbols (= legacy / i64) and for `(SYM :type sexp)'
           ;; (= ABI GP register, but static GC root candidate), and
           ;; `f64' for `(SYM :type f64)'.
           (param-pairs
            (mapcar
             (lambda (p)
               (cond
                ((symbolp p) (list p 'gp nil))
                ((and (consp p) (= (length p) 3)
                      (symbolp (car p))
                      (eq (nth 1 p) :type)
                      (memq (nth 2 p) '(gp f64 sexp)))
                 (list (car p)
                       (if (eq (nth 2 p) 'sexp) 'gp (nth 2 p))
                       (eq (nth 2 p) 'sexp)))
                (t (signal 'nelisp-phase47-compiler-error
                           (list :defun-param-shape p)))))
             param-forms))
           (params (mapcar #'car param-pairs))
           (classes (mapcar #'cadr param-pairs))
           (root-flags (mapcar (lambda (p) (nth 2 p)) param-pairs))
           (uniform-class (car classes)))
      (let ((all-uniform t) (cs classes))
        (while (and all-uniform cs)
          (unless (eq (car cs) uniform-class) (setq all-uniform nil))
          (setq cs (cdr cs)))
        (unless all-uniform
          (signal 'nelisp-phase47-compiler-error
                  (list :defun-mixed-param-classes name classes))))
      (let* ((max-arity
              (cond
               ((eq uniform-class 'f64)
                (length nelisp-phase47-compiler--xmm-arg-regs))
               ((and (eq uniform-class 'gp)
                     (eq nelisp-phase47-compiler--arch 'x86_64)
                     (eq nelisp-phase47-compiler--abi 'sysv))
                ;; GP ref loads use an rbp+disp8 local slot.  Slot 13
                ;; is the current compiler-wide upper bound.
                14)
               (t
                (length (nelisp-phase47-compiler--current-arg-regs))))))
        (when (> arity max-arity)
          (signal 'nelisp-phase47-compiler-error
                  (list :defun-too-many-params name arity uniform-class))))
      (let* ((reg-pool (if (eq uniform-class 'f64)
                           nelisp-phase47-compiler--xmm-arg-regs
                         (nelisp-phase47-compiler--current-arg-regs)))
             (reg-budget (length reg-pool))
             (param-regs
              (if (and (eq uniform-class 'gp)
                       (eq nelisp-phase47-compiler--arch 'x86_64)
                       (eq nelisp-phase47-compiler--abi 'sysv)
                       (> arity reg-budget))
                  (cl-loop for i below arity
                           collect (if (< i reg-budget)
                                       (nth i reg-pool)
                                     (list :stack (- i reg-budget))))
                (cl-subseq reg-pool 0 arity)))
             ;; FENV: each param maps to plist
             ;;   `(:reg R :slot S :class CLASS)'
             ;; where SLOT is the param's 0-based index, used by
             ;; `--emit-value' to compute the rbp-relative spill
             ;; offset `-8*(slot+1)'.  CLASS gates which load
             ;; instruction the `ref' emitter uses (`mov' for gp,
             ;; `movsd' for f64).
             (new-fenv
              (let ((idx -1))
                (cl-mapcar
                 (lambda (p r)
                   (setq idx (1+ idx))
                   (cons p (list :reg r :slot idx
                                 :class uniform-class
                                 :root-p (nth idx root-flags))))
                 params param-regs)))
             ;; Body is a value-producing expression (= implicit return).
             ;; Bind `--next-rt-let-slot' starting at arity so runtime
             ;; `let-rt' bindings occupy slots arity, arity+1, ...
             ;; The slot counter is a mutable cons cell; the final value
             ;; gives us `rt-slot-count = (car cell) - arity'.
             (rt-slot-cell (list arity))
             (body-ir (let ((nelisp-phase47-compiler--next-rt-let-slot
                             rt-slot-cell))
                        (nelisp-phase47-compiler--parse-value
                         body env new-fenv defuns)))
             (rt-slot-count (- (car rt-slot-cell) arity))
             (gc-root-slots
              (nelisp-phase47-compiler--gc-root-slots-for-defun body-ir))
             (root-managed-body-ir
              (nelisp-phase47-compiler--maybe-wrap-aot-root-scope
               body-ir gc-root-slots env new-fenv defuns)))
        (nelisp-phase47-compiler--make-ir 'defun
              :name name
              :params params
              :param-regs param-regs
              :param-class uniform-class
              :rest-p (plist-get param-info :rest-p)
              :fixed-param-count (plist-get param-info :fixed-count)
              :rt-slot-count rt-slot-count
              :gc-root-slots gc-root-slots
              :body root-managed-body-ir))))
   ;; Bare call in statement position (= side-effect; value discarded).
   ((and (consp sexp) (symbolp (car sexp))
         (assq (car sexp) defuns))
    (nelisp-phase47-compiler--parse-value sexp env fenv defuns))
   ;; §97.c control-flow forms as statements (= value discarded).
   ;; `while' in statement position is the common case (= loop with
   ;; side-effects).  `if'/`cond'/`and'/`or' compute a value that
   ;; the surrounding statement context drops on the floor.
   ((and (consp sexp)
         (memq (car sexp) '(if while cond and or)))
    (nelisp-phase47-compiler--parse-value sexp env fenv defuns))
   ;; Doc 49 Wave 11.2 hash-table primitives in statement position.
   ;; Each desugars to a value-producing form whose result the stmt
   ;; context discards (= same shape as the `if'/`while' arm above).
   ((and (consp sexp)
         (memq (car sexp)
               '(hash-table-make hash-table-put
                                 hash-table-get hash-table-contains-p)))
    (nelisp-phase47-compiler--parse-value sexp env fenv defuns))
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :unknown-form sexp)))))

(defun nelisp-phase47-compiler--parse (sexp &optional env)
  "Public parser entry: parse SEXP into a top-level IR node.
ENV is the optional let-environment for testing helpers."
  (let ((nelisp-phase47-compiler--special-vars
         (plist-get (nelisp-phase47-compiler--extract-defmacros sexp)
                    :special-vars)))
    (nelisp-phase47-compiler--parse-stmt
     (nelisp-phase47-compiler--prepare-source sexp)
     env nil nil)))

;; ---- §97.2 string collector ----
;;
;; Walks the IR collecting every distinct (= `equal'-deduped) string
;; literal referenced by a `write' node.  Assigns each a byte offset
;; within the eventual .rodata buffer.

(defun nelisp-phase47-compiler--collect-strings (ir)
  "Walk IR, return alist `((STR . (:offset N :len L)) ...)' + bytes.
The return shape is (STR-OFFSETS . RODATA-BYTES).  Walks through
defun bodies too so functions can call `write'."
  (let ((offsets nil)
        (rodata "")
        (cursor 0))
    (cl-labels
        ((walk (node)
           (when node
             ;; A33.3 — integer-tag dispatch (= `pcase' arms turned into a
             ;; `cond' over `--ir-kind-tag'); behaviour-preserving so the
             ;; emitted `.o' bytes are unchanged.
             (let ((tag (nelisp-phase47-compiler--ir-kind-tag node)))
              (cond
               ((= tag 87)            ; write
                (let ((s (nelisp-phase47-compiler--ir-get node :str)))
                  (unless (assoc s offsets)
                    (let ((bs (encode-coding-string s 'utf-8 t)))
                      (setq offsets
                            (append offsets
                                    (list (cons s
                                                (list :offset cursor
                                                      :len (length bs))))))
                      (setq rodata (concat rodata bs))
                      (setq cursor (+ cursor (length bs)))))))
               ((= tag 22)            ; exit
                (walk (nelisp-phase47-compiler--ir-get node :value)))
               ((= tag 54)            ; seq
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms)))
               ((= tag 31)            ; let
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 21)            ; defun
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 1)             ; arith
                (walk (nelisp-phase47-compiler--ir-get node :a))
                (walk (nelisp-phase47-compiler--ir-get node :b)))
               ((= tag 67)            ; shift
                (walk (nelisp-phase47-compiler--ir-get node :a))
                (walk (nelisp-phase47-compiler--ir-get node :b)))
               ((= tag 5)             ; call
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :args)))
               ((= tag 10)            ; cmp
                (walk (nelisp-phase47-compiler--ir-get node :a))
                (walk (nelisp-phase47-compiler--ir-get node :b)))
               ((= tag 29)            ; if
                (walk (nelisp-phase47-compiler--ir-get node :test))
                (walk (nelisp-phase47-compiler--ir-get node :then))
                (walk (nelisp-phase47-compiler--ir-get node :else)))
               ((= tag 86)            ; while
                (walk (nelisp-phase47-compiler--ir-get node :test))
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 11)            ; cond
                (dolist (cl (nelisp-phase47-compiler--ir-get node :clauses))
                  (unless (eq (car cl) 'always)
                    (walk (car cl)))
                  (walk (cdr cl))))
               ((= tag 33)            ; logic
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms)))
               ((= tag 88)            ; value-seq
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms)))
               ((= tag 92)            ; aot-root-scope
                (walk (nelisp-phase47-compiler--ir-get node :materialize-ir))
                (walk (nelisp-phase47-compiler--ir-get node :push-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body))
                (walk (nelisp-phase47-compiler--ir-get node :pop-ir)))
               ((= tag 32)            ; let-rt
                (walk (nelisp-phase47-compiler--ir-get node :value-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 89)            ; let-rt-n
                (dolist (binding (nelisp-phase47-compiler--ir-get node :bindings))
                  (walk (nth 2 binding)))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               (t nil))))))
      (walk ir))
    (cons offsets rodata)))

;; ---- Doc 49 Wave 11.1 §11.1.1 static-imm32-table collector ----
;;
;; Walks the IR collecting every `static-imm32-table-define' node and
;; assigns each a byte offset within the table-rodata sub-buffer
;; (= placed *after* the string rodata so existing string vaddrs do
;; not shift).  Each element is encoded as 4 little-endian bytes (=
;; u32 zero-extended to i64 at runtime by `MOV EAX, [RDI+RSI]').
;; Duplicate names raise an error.  No-op when no defines exist.

(defun nelisp-phase47-compiler--collect-tables (ir)
  "Walk IR, return alist `((NAME . (:offset N :len L)) ...)' + bytes.
The return shape is (TABLE-OFFSETS . TABLE-BYTES).  Each NAME
maps to a plist with :offset (= byte offset within TABLE-BYTES,
NOT the overall .rodata buffer) and :len (= byte length = 4 *
element count).  TABLE-BYTES is the concatenation of all tables
in declaration-encounter order.

Walks through `defun' bodies, `let' bodies, `seq' children, etc.
recursively to mirror `--collect-strings'.  Signals
`nelisp-phase47-compiler-error' with `:duplicate-static-imm32-table'
when two `table-define' nodes share a NAME."
  (let ((offsets nil)
        (bytes "")
        (cursor 0))
    (cl-labels
        ((encode-u32 (v)
           (let ((u (logand v #xFFFFFFFF)))
             (unibyte-string (logand u #xFF)
                             (logand (ash u  -8) #xFF)
                             (logand (ash u -16) #xFF)
                             (logand (ash u -24) #xFF))))
         (walk (node)
           (when node
             ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
             ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
             (let ((tag (nelisp-phase47-compiler--ir-kind-tag node)))
              (cond
               ((= tag 79)            ; table-define
                (let ((name (nelisp-phase47-compiler--ir-get node :name))
                      (elements (nelisp-phase47-compiler--ir-get node :elements)))
                  (when (assoc name offsets)
                    (signal 'nelisp-phase47-compiler-error
                            (list :duplicate-static-imm32-table name)))
                  (let ((blob ""))
                    (dolist (e elements)
                      (setq blob (concat blob (encode-u32 e))))
                    (setq offsets
                          (append offsets
                                  (list (cons name
                                              (list :offset cursor
                                                    :len (length blob))))))
                    (setq bytes (concat bytes blob))
                    (setq cursor (+ cursor (length blob))))))
               ((= tag 54)            ; seq
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms)))
               ((= tag 31)            ; let
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 21)            ; defun
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 32)            ; let-rt
                (walk (nelisp-phase47-compiler--ir-get node :value-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 89)            ; let-rt-n
                (dolist (binding (nelisp-phase47-compiler--ir-get node :bindings))
                  (walk (nth 2 binding)))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 88)            ; value-seq
                (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms)))
               ((= tag 92)            ; aot-root-scope
                (walk (nelisp-phase47-compiler--ir-get node :materialize-ir))
                (walk (nelisp-phase47-compiler--ir-get node :push-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body))
                (walk (nelisp-phase47-compiler--ir-get node :pop-ir)))
               ;; table-lookup is value-producing; the `:index' child
               ;; is itself an IR node walked for nested defines (=
               ;; defensive, normal source has table-define at top
               ;; level only).
               ((= tag 80)            ; table-lookup
                (walk (nelisp-phase47-compiler--ir-get node :index)))
               (t nil))))))
      (walk ir))
    (cons offsets bytes)))

;; ---- §97.3 defun collector ----
;;
;; Walks the top-level IR pulling out every `:kind defun' node so the
;; emitter can place them after `_start' with deterministic labels.
;; Order matters because pass-1/pass-2 byte invariance depends on
;; consistent walk order.

(defun nelisp-phase47-compiler--collect-defuns (ir)
  "Walk IR returning a list of defun IR nodes in encounter order.
Defuns are pulled out so the emitter can place them sequentially
after the main `_start' body.  Defuns are removed from the inline
walk; the emitter substitutes a no-op for the original site."
  (let ((acc nil))
    (cl-labels
        ((walk (node)
           (when node
             ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
             ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
             (let ((tag (nelisp-phase47-compiler--ir-kind-tag node)))
              (cond
               ((= tag 21) (push node acc)) ; defun
               ((= tag 54) (mapc #'walk (nelisp-phase47-compiler--ir-get node :forms))) ; seq
               ((= tag 31) (walk (nelisp-phase47-compiler--ir-get node :body))) ; let
               ((= tag 32)            ; let-rt
                (walk (nelisp-phase47-compiler--ir-get node :value-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 89)            ; let-rt-n
                (dolist (binding (nelisp-phase47-compiler--ir-get node :bindings))
                  (walk (nth 2 binding)))
                (walk (nelisp-phase47-compiler--ir-get node :body)))
               ((= tag 92)            ; aot-root-scope
                (walk (nelisp-phase47-compiler--ir-get node :materialize-ir))
                (walk (nelisp-phase47-compiler--ir-get node :push-ir))
                (walk (nelisp-phase47-compiler--ir-get node :body))
                (walk (nelisp-phase47-compiler--ir-get node :pop-ir)))
               (t nil))))))
      (walk ir))
    (nreverse acc)))

;; ---- §97.4 emit walker — value-producing ----
;;
;; Every emit-value variant computes its result into rax.  Callers
;; that need the result elsewhere `mov-reg-reg' from rax.

(defun nelisp-phase47-compiler--imul-rax-imm32 (buf imm)
  "Emit `imul rax, rax, imm32' (= REX.W 0x69 /0 + imm32 = 7 bytes).
Uses `emit-bytes' because Doc 92 has no imul helper; the encoding
is fixed-width so the pass-1/pass-2 invariant holds."
  ;; REX.W=48, opcode 69, ModR/M = 11_000_000 (= reg=rax, rm=rax) = C0.
  (nelisp-asm-x86_64-emit-bytes
   buf (concat (unibyte-string #x48 #x69 #xC0)
               (nelisp-asm-x86_64--imm32-bytes imm))))

(defun nelisp-phase47-compiler--imul-reg-reg (buf dst src)
  "Emit `imul DST, SRC' (= REX.W 0x0F 0xAF /r = 4 bytes).
DST is the destination = ModR/M.reg; SRC is the source = ModR/M.rm.
Uses `emit-bytes' for the 0F-prefix opcode form."
  (let* ((rex (nelisp-asm-x86_64--rex
               1
               (nelisp-asm-x86_64--reg-ext dst)
               0
               (nelisp-asm-x86_64--reg-ext src)))
         (modrm (nelisp-asm-x86_64--modrm
                 3
                 (nelisp-asm-x86_64--reg-low3 dst)
                 (nelisp-asm-x86_64--reg-low3 src))))
    (nelisp-asm-x86_64-emit-bytes
     buf (unibyte-string rex #x0F #xAF modrm))))

(defun nelisp-phase47-compiler--emit-aarch64-unsupported (kind &optional detail)
  "Signal that KIND is not yet emitted on aarch64 in Phase 47.
DETAIL carries the offending IR node or operator when useful."
  (signal 'nelisp-phase47-compiler-error
          (list :unsupported-aarch64-emit kind
                :detail detail)))

(defun nelisp-phase47-compiler--arm64-emit-word (buf word)
  "Append one 32-bit little-endian instruction WORD to arm64 BUF."
  (nelisp-asm-arm64--emit-word buf word))

(defun nelisp-phase47-compiler--arm64-emit-ldur (buf dst base imm9)
  "Emit `LDUR DST, [BASE, #IMM9]' into BUF.
IMM9 is a signed unscaled byte offset in the range -256..255."
  (unless (and (integerp imm9) (<= -256 imm9) (<= imm9 255))
    (signal 'nelisp-phase47-compiler-error
            (list :arm64-ldur-imm9-out-of-range imm9)))
  (let* ((t-reg (logand (nelisp-asm-arm64--reg-num dst) #x1F))
         (n-reg (logand (nelisp-asm-arm64--reg-num base) #x1F))
         (imm9-u (logand imm9 #x1FF)))
    (nelisp-phase47-compiler--arm64-emit-word
     buf (logior #xF8400000
                 (ash imm9-u 12)
                 (ash n-reg 5)
                 t-reg))))

(defun nelisp-phase47-compiler--emit-ref-load (buf slot)
  "Emit `mov rax, [rbp - 8*(SLOT+1)]' (= 4 bytes, fixed).
Used by `:kind ref' (GP class) to load a spilled parameter off
the local frame.  Encoding: REX.W (= 0x48), MOV r64, r/m64 opcode
(= 0x8B), ModR/M = mod=01 reg=000 (rax) rm=101 (rbp) = 0x45, then
disp8 = (- 8*(SLOT+1)) as a signed byte.  SLOT ranges over 0..5
so the displacement is always in disp8 range; signals if SLOT is
out of range for the current Doc 97 arity cap."
  ;; Slot 0..5 = GP params (SysV AMD64 arity cap).  Slots 6+ are
  ;; runtime `let-rt' bindings allocated beyond the param spill area.
  ;; Upper bound 13 keeps the disp8 in [-128,0] range (= 8*(13+1)=112).
  (unless (and (integerp slot) (<= 0 slot 13))
    (signal 'nelisp-phase47-compiler-error
            (list :ref-slot-out-of-range slot)))
  (if (eq nelisp-phase47-compiler--arch 'aarch64)
      (let ((disp (- (* 16 (1+ slot)))))
        (nelisp-phase47-compiler--arm64-emit-ldur buf 'x0 'x29 disp))
    (let* ((disp (- (* 8 (1+ slot))))
           (disp8 (logand disp #xFF)))
      (nelisp-asm-x86_64-emit-bytes
       buf (unibyte-string #x48 #x8B #x45 disp8)))))

(defun nelisp-phase47-compiler--emit-f64-ref-load (buf slot fp-dst)
  "Emit code that loads a spilled f64 parameter into FP-DST.
Doc 110 §110.E.1 / §110.D — `FP-DST' is `xmm0' / `xmm1' on
x86_64 (= MOVSD load from `[rbp - 8*(slot+1)]') and `d0' / `d1'
on aarch64 (= LDUR D load from `[x29 - 16*(slot+1)]', 16-byte
stride matching the GP path that uses `str-pre-sp-16').  SLOT
must be in 0..7 (= f64 ABI arity cap)."
  (unless (and (integerp slot) (<= 0 slot 7))
    (signal 'nelisp-phase47-compiler-error
            (list :f64-ref-slot-out-of-range slot)))
  (if (eq nelisp-phase47-compiler--arch 'aarch64)
      (let ((disp (- (* 16 (1+ slot)))))
        (nelisp-asm-arm64-ldur-d-base-disp buf fp-dst 'x29 disp))
    (let ((disp (- (* 8 (1+ slot)))))
      (nelisp-asm-x86_64-movsd-xmm-mem-disp8 buf fp-dst 'rbp disp))))

(defun nelisp-phase47-compiler--emit-f64-binop (node buf)
  "Emit a flat f64-class binop NODE (Doc 110 §110.E.1).
Result lands in xmm0 — bypassing the rax convention used by the
GP `--emit-value' contract.  Strategy at MVP scope is `flat-only':

  1. Evaluate B into xmm1 directly (= MUST be a leaf ref or
     literal; nested binops would clobber xmm0 mid-flight and
     are rejected with a clear signal).
  2. Evaluate A into xmm0 directly (= same constraint).
  3. ADDSD/SUBSD/MULSD/DIVSD xmm0, xmm1 → result in xmm0.

This avoids the xmm spill / fill dance the GP path uses
(`push rax; ...; pop r10') because xmm has no single-byte push
opcode + would force a 16-byte stack alignment per call.  Doc
112 (= xmm spill) re-enables nested binops; until then the
parser must reject anything that would force xmm spill."
  (let* ((op (nelisp-phase47-compiler--ir-get node :op))
         (a (nelisp-phase47-compiler--ir-get node :a))
         (b (nelisp-phase47-compiler--ir-get node :b))
         (aarch64-p (eq nelisp-phase47-compiler--arch 'aarch64))
         (xmm0 (if aarch64-p 'd0 'xmm0))
         (xmm1 (if aarch64-p 'd1 'xmm1)))
    (nelisp-phase47-compiler--emit-f64-leaf-into b buf xmm1)
    (nelisp-phase47-compiler--emit-f64-leaf-into a buf xmm0)
    (if aarch64-p
        (cond
         ((eq op 'f64-add)
          (nelisp-asm-arm64-fadd-reg-reg buf 'd0 'd0 'd1))
         ((eq op 'f64-sub)
          (nelisp-asm-arm64-fsub-reg-reg buf 'd0 'd0 'd1))
         ((eq op 'f64-mul)
          (nelisp-asm-arm64-fmul-reg-reg buf 'd0 'd0 'd1))
         ((eq op 'f64-div)
          (nelisp-asm-arm64-fdiv-reg-reg buf 'd0 'd0 'd1))
         (t
          (signal 'nelisp-phase47-compiler-error
                  (list :unknown-f64-binop op))))
      (cond
       ((eq op 'f64-add)
        (nelisp-asm-x86_64-addsd-reg-reg buf 'xmm0 'xmm1))
       ((eq op 'f64-sub)
        (nelisp-asm-x86_64-subsd-reg-reg buf 'xmm0 'xmm1))
       ((eq op 'f64-mul)
        (nelisp-asm-x86_64-mulsd-reg-reg buf 'xmm0 'xmm1))
       ((eq op 'f64-div)
        (nelisp-asm-x86_64-divsd-reg-reg buf 'xmm0 'xmm1))
       (t
        (signal 'nelisp-phase47-compiler-error
                (list :unknown-f64-binop op)))))))

(defun nelisp-phase47-compiler--emit-f64-call (node buf)
  "Emit a 1-arg `f64 → f64' extern call (Doc 110 §110.E.2 / §3.F).
NODE is `:kind f64-call :name SYM :arg IR-NODE'.  Strategy:

  1. Place ARG in xmm0 / d0 via `--emit-f64-leaf-into' (= MVP
     constraint: ARG must be a leaf f64 ref).
  2. Emit `CALL rel32' (x86_64) / `BL imm26' (aarch64) with a
     PLT32 / R_AARCH64_CALL26 reloc against NAME.  4-byte
     placeholder; the linker patches the displacement at static-
     link time.
  3. Result lands in xmm0 / d0 per SysV / AAPCS f64 return ABI.
     No further work — the caller's epilogue's RET preserves
     xmm0 / d0 untouched, so the f64 return value reaches
     this defun's caller intact.

Stack alignment: the prologue rounds the f64 frame to 16-byte
multiples so rsp / sp at the moment of CALL / BL satisfies the
SysV / AAPCS alignment contract (= rsp % 16 == 0 just before
the call instruction)."
  (let ((name (nelisp-phase47-compiler--ir-get node :name))
        (arg (nelisp-phase47-compiler--ir-get node :arg))
        (aarch64-p (eq nelisp-phase47-compiler--arch 'aarch64)))
    (nelisp-phase47-compiler--emit-f64-leaf-into
     arg buf (if aarch64-p 'd0 'xmm0))
    (if aarch64-p
        (let ((slot (nelisp-asm-arm64-buffer-pos buf)))
          ;; Emit BL imm26 placeholder (= base 0x94000000, imm26
          ;; field zeroed).  The linker patches imm26 at static-
          ;; link time via R_AARCH64_CALL26.
          (nelisp-asm-arm64--emit-word buf #x94000000)
          (nelisp-asm-arm64-emit-reloc
           buf 'b26-pc (symbol-name name)))
      ;; x86_64: CALL rel32 = 0xE8 + 4-byte placeholder.  Reloc
      ;; addend -4 matches the GCC / clang convention (= the
      ;; CALL instruction's relative offset is computed against
      ;; the END of the disp32 field, so the PLT32 entry needs
      ;; an addend of -4 to land on the call target).
      (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
      (nelisp-asm-x86_64-reloc-plt32-here
       buf (symbol-name name) -4 'text))))

(defun nelisp-phase47-compiler--emit-f64-leaf-into (node buf xmm-dst)
  "Emit code that places f64-class NODE into XMM-DST.
MVP flat-only constraint: NODE must be `:kind ref' with `:class
f64' (= a direct f64 parameter reference).  Nested f64-binops /
f64-cmps are rejected so the xmm0/xmm1 register schedule stays
trivial until xmm spill machinery lands."
  (let ((kind (nelisp-phase47-compiler--ir-kind node)))
    (cond
     ((and (eq kind 'ref) (eq (nelisp-phase47-compiler--ir-get node :class) 'f64))
      (nelisp-phase47-compiler--emit-f64-ref-load
       buf (nelisp-phase47-compiler--ir-get node :slot) xmm-dst))
     ((eq kind 'bits-to-f64)
      ;; Evaluate INT-EXPR → rax (= gp-class), then MOVQ xmm-dst, rax.
      (nelisp-phase47-compiler--emit-value
       (nelisp-phase47-compiler--ir-get node :int-expr) buf)
      (nelisp-asm-x86_64-movq-xmm-r64 buf xmm-dst 'rax))
     ((eq kind 'i64-to-f64)
      ;; Evaluate INT-EXPR → rax (= gp-class signed i64), then
      ;; CVTSI2SD xmm-dst, rax (= f64 representation of the int).
      (nelisp-phase47-compiler--emit-value
       (nelisp-phase47-compiler--ir-get node :int-expr) buf)
      (nelisp-asm-x86_64-cvtsi2sd-xmm-r64 buf xmm-dst 'rax))
     ((eq kind 'f64-call)
      ;; Existing f64-call ABI lands its result in xmm0.  If XMM-DST is
      ;; xmm0, just emit the call; otherwise pre-call-shuffle is
      ;; needed (= not implemented at MVP, signal error).
      (unless (eq xmm-dst 'xmm0)
        (signal 'nelisp-phase47-compiler-error
                (list :f64-call-leaf-into-nonzero-dst xmm-dst)))
      (nelisp-phase47-compiler--emit-f64-call node buf))
     ((memq kind '(f64-binop f64-cmp))
      (signal 'nelisp-phase47-compiler-error
              (list :nested-f64-binop-needs-doc-112 node)))
     (t
      (signal 'nelisp-phase47-compiler-error
              (list :f64-leaf-shape-unsupported kind node))))))

(defun nelisp-phase47-compiler--emit-f64-cmp (node buf)
  "Emit a flat f64-class ordered comparison NODE (Doc 110 §110.C.2.a).
Result is `0' or `1' in rax via the canonical 10-byte sequence:

  UCOMISD <xmmL>, <xmmR>   ; flags ← cmp(L, R)
  SET{A|AE} al             ; al ← 1 iff ordered relation holds
  MOVZX eax, al            ; rax ← zext(al); implicit
                              RAX[63:32] = 0 from 32-bit write

Operand order is *swapped* for `f64-lt' / `f64-le' so the SETA /
SETAE result naturally matches Rust's NaN-→-false semantics
without an explicit AND-with-SETNP mask.  Concretely:

  (a < b)  = UCOMISD xmm1, xmm0 ; SETA   al  (= b above a, ordered)
  (a > b)  = UCOMISD xmm0, xmm1 ; SETA   al  (= a above b, ordered)
  (a <= b) = UCOMISD xmm1, xmm0 ; SETAE  al  (= b above-or-eq a, ordered)
  (a >= b) = UCOMISD xmm0, xmm1 ; SETAE  al  (= a above-or-eq b, ordered)

Under unordered (= NaN involved), UCOMISD sets CF=1 ZF=1 PF=1.
SETA tests `CF=0 AND ZF=0' → 0; SETAE tests `CF=0' → 0.  Both
yield 0, matching Rust's `(NaN OP x) = false' rule.  NaN-aware
EQ-EPS is more involved (needs PF mask AND-ed with SETB) and
ships in §110.C.2.b."
  (let* ((op (nelisp-phase47-compiler--ir-get node :op))
         (a (nelisp-phase47-compiler--ir-get node :a))
         (b (nelisp-phase47-compiler--ir-get node :b))
         (aarch64-p (eq nelisp-phase47-compiler--arch 'aarch64))
         (fp0 (if aarch64-p 'd0 'xmm0))
         (fp1 (if aarch64-p 'd1 'xmm1)))
    ;; Operand placement: A → fp0, B → fp1 (= same convention as
    ;; `--emit-f64-binop').  Comparison ops then swap operand order
    ;; for LT / LE (= the AArch64 / x86_64 trick that makes SETA /
    ;; CSET-GT inherently NaN-correct).
    (nelisp-phase47-compiler--emit-f64-leaf-into b buf fp1)
    (nelisp-phase47-compiler--emit-f64-leaf-into a buf fp0)
    (cond
     ((eq op 'f64-eq-eps)
      (nelisp-phase47-compiler--emit-f64-eq-eps buf))
     (aarch64-p
      ;; FCMP + CSET with operand-swap for LT / LE — same NaN-mask
      ;; trick as x86_64 (= compare b vs a, materialise via GT / GE
      ;; condition codes which inherently yield 0 on unordered
      ;; because V=1 + Z=1 from NaN make N!=V and Z=1 both false).
      (let ((fcmp-args
             (cond
              ((memq op '(f64-lt f64-le)) (list fp1 fp0))  ; swap
              ((memq op '(f64-gt f64-ge)) (list fp0 fp1))  ; direct
              (t (signal 'nelisp-phase47-compiler-error
                         (list :unknown-f64-cmp-op op)))))
            (cset-cond
             (cond
              ((memq op '(f64-lt f64-gt)) 'gt)
              ((memq op '(f64-le f64-ge)) 'ge)
              (t (signal 'nelisp-phase47-compiler-error
                         (list :unknown-f64-cmp-op op))))))
        (nelisp-asm-arm64-fcmp-reg-reg
         buf (nth 0 fcmp-args) (nth 1 fcmp-args))
        (nelisp-asm-arm64-cset buf 'x0 cset-cond)))
     (t
      (let ((ucomisd-args
             (cond
              ;; LT / LE: compare b vs a (= swap order so SETA / SETAE
              ;; tests "b above a ordered" = "a below b ordered").
              ((memq op '(f64-lt f64-le)) (list 'xmm1 'xmm0))
              ;; GT / GE: compare a vs b directly.
              ((memq op '(f64-gt f64-ge)) (list 'xmm0 'xmm1))
              (t (signal 'nelisp-phase47-compiler-error
                         (list :unknown-f64-cmp-op op)))))
            (setcc-mnemonic
             (cond
              ((memq op '(f64-lt f64-gt)) 'seta)   ; strict ordered above
              ((memq op '(f64-le f64-ge)) 'setae)  ; ordered above-or-equal
              (t (signal 'nelisp-phase47-compiler-error
                         (list :unknown-f64-cmp-op op))))))
        (nelisp-asm-x86_64-ucomisd-reg-reg
         buf (nth 0 ucomisd-args) (nth 1 ucomisd-args))
        (nelisp-asm-x86_64-setcc-al buf setcc-mnemonic)
        (nelisp-asm-x86_64-movzx-eax-al buf))))))

;; Doc 110 §110.C.2.b — EQ-EPS (= `(a - b).abs() < 1e-15') constants.
;; The 1e-15 bit pattern is the IEEE 754 double-precision representation
;; of the f64 literal `1e-15' (= the abs tolerance hardcoded in
;; `build-tool/src/jit/float.rs::nl_jit_float_eq_eps' as `1e-15').
;; Verified by `tests/elisp_cc_jit_float_probe.rs::float_eq_eps_*' against
;; Rust's own `1e-15_f64' so any drift between the two surfaces in CI.

(defconst nelisp-phase47-compiler--f64-1e-15-bits
  #x3CD203AF9EE75616
  "IEEE 754 double-precision bit pattern for the f64 literal `1e-15'.
Sign=0, biased exponent = 0x3CD (= 973 unbiased -50, so 2^-50 *
mantissa), mantissa = 0x203AF9EE75616.  Cross-verified against
Rust's `1e-15_f64.to_bits()' by the §110.C.2.b probe.")

(defconst nelisp-phase47-compiler--f64-abs-mask
  #x7FFFFFFFFFFFFFFF
  "IEEE 754 double-precision sign-clear mask (= clear bit 63).
ANDed against `(a - b)' produces `|a - b|' for the EQ-EPS
predicate.  NaN inputs propagate through (= NaN bit pattern with
sign cleared is still NaN), so the subsequent UCOMISD against
1e-15 sets PF=1 → the SETNP cl branch masks the result to 0.")

(defun nelisp-phase47-compiler--emit-f64-eq-eps (buf)
  "Emit the EQ-EPS body sequence (Doc 110 §110.C.2.b / §110.D).
Pre: xmm0 (x86_64) / d0 (aarch64) holds A, xmm1 / d1 holds B
(caller is `--emit-f64-cmp').  Post: rax / x0 holds 1 iff
|A - B| < 1e-15 AND ordered (= match Rust `(a - b).abs() < 1e-15'
including NaN → 0).

Avoids `.rodata' entirely on both archs — the 1e-15 constant is
materialised via inline `imm64' + GP→FP transfer.  Cost on
x86_64 ~63 body bytes, on aarch64 ~52 (= 4-instr MOV/MOVK
chain + 1 FMOV + FCMP + CSET).  Rodata path is future Doc 112
work.

x86_64 sequence:
  SUBSD xmm0, xmm1; MOV r10,abs-mask; MOVQ xmm1,r10; ANDPD;
  MOV r10,1e-15; MOVQ; UCOMISD; SETB al; SETNP cl; AND al,cl;
  MOVZX eax, al

aarch64 sequence (= FABS replaces the ANDPD abs-mask dance; the
FCMP swap + CSET GT inherently yields 0 on NaN, so no SETNP-AND
mask is needed):
  FSUB d0, d0, d1            ; d0 = a - b
  FABS d0, d0                ; d0 = |a - b|
  MOV  x10, #1e-15 (= 4-instr MOV/MOVK chain)
  FMOV d1, x10               ; d1 = 1e-15
  FCMP d1, d0                ; flags reflect 1e-15 vs |a-b|
  CSET x0, gt                ; x0 = 1 iff 1e-15 > |a-b| ordered"
  (if (eq nelisp-phase47-compiler--arch 'aarch64)
      (progn
        (nelisp-asm-arm64-fsub-reg-reg buf 'd0 'd0 'd1)
        (nelisp-asm-arm64-fabs-reg-reg buf 'd0 'd0)
        (nelisp-asm-arm64-mov-imm64
         buf 'x10 nelisp-phase47-compiler--f64-1e-15-bits)
        (nelisp-asm-arm64-fmov-d-from-x buf 'd1 'x10)
        ;; FCMP d1, d0 then CSET x0, gt (= 1 iff 1e-15 > |a-b|
        ;; ordered, with NaN giving V=1 → GT cond false).
        (nelisp-asm-arm64-fcmp-reg-reg buf 'd1 'd0)
        (nelisp-asm-arm64-cset buf 'x0 'gt))
    (nelisp-asm-x86_64-subsd-reg-reg buf 'xmm0 'xmm1)
    (nelisp-asm-x86_64-mov-imm64 buf 'r10
                                  nelisp-phase47-compiler--f64-abs-mask)
    (nelisp-asm-x86_64-movq-xmm-r64 buf 'xmm1 'r10)
    (nelisp-asm-x86_64-andpd-reg-reg buf 'xmm0 'xmm1)
    (nelisp-asm-x86_64-mov-imm64 buf 'r10
                                  nelisp-phase47-compiler--f64-1e-15-bits)
    (nelisp-asm-x86_64-movq-xmm-r64 buf 'xmm1 'r10)
    (nelisp-asm-x86_64-ucomisd-reg-reg buf 'xmm0 'xmm1)
    (nelisp-asm-x86_64-setcc-al buf 'setb)
    (nelisp-asm-x86_64-setcc-byte-r8 buf 'setnp 'cl)
    (nelisp-asm-x86_64-and-r8-r8 buf 'al 'cl)
    (nelisp-asm-x86_64-movzx-eax-al buf)))

(defun nelisp-phase47-compiler--emit-let-rt-n-bindings (node buf)
  "Emit NODE's runtime `let-rt-n' initializers and spill them to slots."
  (dolist (binding (nelisp-phase47-compiler--ir-get node :bindings))
    (let* ((slot (nth 1 binding))
           (value-ir (nth 2 binding))
           (disp (- (* 8 (1+ slot)))))
      (nelisp-phase47-compiler--emit-value value-ir buf)
      (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rbp disp 'rax))))

(defun nelisp-phase47-compiler--emit-frame-slot-into-reg (buf slot reg)
  "Emit a GP load from frame SLOT into REG."
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf reg 'rbp (- (* 8 (1+ slot)))))

(defun nelisp-phase47-compiler--emit-aot-root-bridge-call (node name buf)
  "Emit a direct SysV call to Doc 129.5 root bridge NAME for NODE."
  (unless (and (eq nelisp-phase47-compiler--arch 'x86_64)
               (eq nelisp-phase47-compiler--abi 'sysv))
    (signal 'nelisp-phase47-compiler-error
            (list :aot-root-scope-unsupported-target
                  nelisp-phase47-compiler--arch
                  nelisp-phase47-compiler--abi)))
  (let* ((slots (nelisp-phase47-compiler--ir-get node :boundary-slots))
         (slot-of (lambda (sym)
                    (or (cdr (assq sym slots))
                        (signal 'nelisp-phase47-compiler-error
                                (list :aot-root-scope-missing-slot
                                      sym slots))))))
    ;; nelisp_aot_{push,pop}_roots(mirror, frames, roots, out, scratch)
    (nelisp-phase47-compiler--emit-frame-slot-into-reg
     buf (funcall slot-of 'mirror) 'rdi)
    (nelisp-phase47-compiler--emit-frame-slot-into-reg
     buf (funcall slot-of 'frames) 'rsi)
    (nelisp-phase47-compiler--emit-frame-slot-into-reg
     buf (funcall slot-of 'roots) 'rdx)
    (nelisp-phase47-compiler--emit-frame-slot-into-reg
     buf (funcall slot-of 'out) 'rcx)
    (nelisp-phase47-compiler--emit-frame-slot-into-reg
     buf (funcall slot-of 'scratch) 'r8)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf (symbol-name name) -4 'text)))

(defun nelisp-phase47-compiler--emit-aot-root-scope (node buf)
  "Emit automatic Doc 129.5 root push/pop around NODE's body."
  (let* ((materialize-ir
          (nelisp-phase47-compiler--ir-get node :materialize-ir))
         (slots (nelisp-phase47-compiler--ir-get node :boundary-slots))
         (roots-slot (cdr (assq 'roots slots))))
    (unless roots-slot
      (signal 'nelisp-phase47-compiler-error
              (list :aot-root-scope-missing-slot 'roots slots)))
    (nelisp-phase47-compiler--emit-value materialize-ir buf)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rbp (- (* 8 (1+ roots-slot))) 'rax))
  (nelisp-phase47-compiler--emit-aot-root-bridge-call
   node 'nelisp_aot_push_roots buf)
  (nelisp-phase47-compiler--emit-value
   (nelisp-phase47-compiler--ir-get node :body)
   buf)
  ;; Preserve the body's GP return value across the pop bridge.  The
  ;; body-entry stack is 16-byte aligned; `push rax' misaligns it, so
  ;; insert one fixed 8-byte pad before the bridge call and remove it
  ;; before restoring rax.
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp 8)
  (nelisp-phase47-compiler--emit-aot-root-bridge-call
   node 'nelisp_aot_pop_roots buf)
  (nelisp-asm-x86_64-add-imm32 buf 'rsp 8)
  (nelisp-asm-x86_64-pop buf 'rax))

(defun nelisp-phase47-compiler--emit-value (node buf)
  "Emit code that computes value NODE into rax (or xmm0 for f64 nodes).
NODE is one of `imm' / `ref' / `arith' / `call' / `cmp' / `if' /
`while' / `cond' / `logic' / `f64-binop'.  Most variants preserve
callee-saved registers (= we use only rax + r10 + r11 for scratch
and the arg regs rdi..r9 which are caller-saved anyway).
Doc 110 §110.E.1 f64 nodes (`f64-binop' + `ref' with :class f64)
return their result in xmm0 instead of rax — caller must know
the node's class to consume the result correctly."
  (if (eq nelisp-phase47-compiler--arch 'aarch64)
      ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
      ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
      (let ((tag (nelisp-phase47-compiler--ir-kind-tag node)))
       (cond
        ((= tag 30)             ; imm
         (nelisp-asm-arm64-mov-imm64 buf 'x0 (nelisp-phase47-compiler--ir-get node :value)))
        ((= tag 53)             ; ref
         ;; GP class → LDUR Xn (existing path); f64 class → LDUR Dn
         ;; into d0 (= default destination for top-level body ref).
         (if (eq (nelisp-phase47-compiler--ir-get node :class) 'f64)
             (nelisp-phase47-compiler--emit-f64-ref-load
              buf (nelisp-phase47-compiler--ir-get node :slot) 'd0)
           (nelisp-phase47-compiler--emit-ref-load
            buf (nelisp-phase47-compiler--ir-get node :slot))))
        ((= tag 1)              ; arith
         (nelisp-phase47-compiler--emit-arith node buf))
        ((= tag 67)             ; shift
         (nelisp-phase47-compiler--emit-shift node buf))
        ((= tag 10)             ; cmp
         (nelisp-phase47-compiler--emit-cmp node buf))
        ((= tag 29)             ; if
         (nelisp-phase47-compiler--emit-if node buf))
        ((= tag 88)             ; value-seq
         (dolist (child (nelisp-phase47-compiler--ir-get node :forms))
           (nelisp-phase47-compiler--emit-value child buf)))
        ((= tag 24)             ; f64-binop
         (nelisp-phase47-compiler--emit-f64-binop node buf))
        ((= tag 26)             ; f64-cmp
         (nelisp-phase47-compiler--emit-f64-cmp node buf))
        ((= tag 25)             ; f64-call
         (nelisp-phase47-compiler--emit-f64-call node buf))
        ((memq tag '(5 23 61 57 56 55 27 ; call extern-call sexp-tag sexp-int-unwrap sexp-int-make sexp-float-unwrap f64-to-i64-trunc
                     17 12 13 14         ; cons-null-p cons-car cons-cdr cons-cdr-raw
                     59 60               ; sexp-payload-ptr sexp-payload-ptr-record
                     52 48 49            ; record-type-tag record-slot-count record-slot-ref
                     50 51               ; record-slot-ref-ptr record-slot-set
                     81 83 84 85         ; vector-len vector-ref vector-ref-ptr vector-slot-set
                     82                  ; vector-make
                     47                  ; record-make
                     9 8 6 7             ; cell-value cell-set-value cell-make cell-null-p
                     75 69 70 68 73 76   ; str-len str-bytes str-bytes-ptr str-byte-at str-eq symbol-eq
                     77 58               ; symbol-name-eq sexp-name-eq
                     63 66               ; sexp-write-nil sexp-write-t
                     64 65 90 91 62      ; sexp-write-str sexp-write-symbol sexp-write-symbol-lit sexp-write-str-lit sexp-write-float
                     36 37 38            ; mut-str-make-empty mut-str-push-byte mut-str-push-codepoint
                     35 34               ; mut-str-len mut-str-finalize
                     71 72 74            ; str-char-count str-codepoint-at str-is-alphanumeric-at
                     3 2                 ; atomic-fetch-add atomic-compare-exchange
                     41 45               ; ptr-read-u64 ptr-write-u64
                     42 46               ; ptr-read-u8 ptr-write-u8
                     39 43               ; ptr-read-u16 ptr-write-u16
                     40 44               ; ptr-read-u32 ptr-write-u32
                     0 20 78             ; alloc-bytes dealloc-bytes syscall-direct
                     15 16 18 19         ; cons-make cons-make-with-clone cons-set-car cons-set-cdr
                     86 11 33            ; while cond logic
                     89                  ; let-rt-n
                     92                  ; aot-root-scope
                     78))               ; syscall-direct
         (nelisp-phase47-compiler--emit-aarch64-unsupported
          (nelisp-phase47-compiler--ir-kind node) node))
        (t
         (let ((kind (nelisp-phase47-compiler--ir-kind node)))
           (signal 'nelisp-phase47-compiler-error
                   (list :unknown-value-kind kind))))))
    ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
    ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
    (let ((tag (nelisp-phase47-compiler--ir-kind-tag node)))
     (cond
      ((= tag 30)               ; imm
       ;; mov rax, imm32                                = 7 bytes
       (nelisp-asm-x86_64-mov-imm32 buf 'rax (nelisp-phase47-compiler--ir-get node :value)))
      ((= tag 53)               ; ref
       ;; gp class → `mov rax, [rbp - 8*(slot+1)]'; f64 class →
       ;; `movsd xmm0, [rbp - 8*(slot+1)]'.  Both read the spilled
       ;; param from the frame slot allocated by the prologue;
       ;; `:class' on the ref node dispatches the instruction.
       (if (eq (nelisp-phase47-compiler--ir-get node :class) 'f64)
           (nelisp-phase47-compiler--emit-f64-ref-load
            buf (nelisp-phase47-compiler--ir-get node :slot) 'xmm0)
         (nelisp-phase47-compiler--emit-ref-load
          buf (nelisp-phase47-compiler--ir-get node :slot))))
      ((= tag 24)               ; f64-binop
       (nelisp-phase47-compiler--emit-f64-binop node buf))
      ((= tag 26)               ; f64-cmp
       (nelisp-phase47-compiler--emit-f64-cmp node buf))
      ((= tag 25)               ; f64-call
       (nelisp-phase47-compiler--emit-f64-call node buf))
      ((= tag 1)                ; arith
       (nelisp-phase47-compiler--emit-arith node buf))
      ((= tag 67)               ; shift
       (nelisp-phase47-compiler--emit-shift node buf))
      ((= tag 5)                ; call
       (nelisp-phase47-compiler--emit-call node buf))
      ((= tag 23)               ; extern-call
       (nelisp-phase47-compiler--emit-extern-call node buf))
      ((= tag 61)               ; sexp-tag
       (nelisp-phase47-compiler--emit-sexp-tag node buf))
      ((= tag 57)               ; sexp-int-unwrap
       (nelisp-phase47-compiler--emit-sexp-int-unwrap node buf))
      ((= tag 55)               ; sexp-float-unwrap
       (nelisp-phase47-compiler--emit-sexp-float-unwrap node buf))
      ((= tag 27)               ; f64-to-i64-trunc
       (nelisp-phase47-compiler--emit-f64-to-i64-trunc node buf))
      ((= tag 56)               ; sexp-int-make
       (nelisp-phase47-compiler--emit-sexp-int-make node buf))
      ((= tag 17)               ; cons-null-p
       (nelisp-phase47-compiler--emit-cons-null-p node buf))
      ((= tag 12)               ; cons-car
       (nelisp-phase47-compiler--emit-cons-slot-copy
        node buf nelisp-nlconsbox--offset-car))
      ((= tag 13)               ; cons-cdr
       (nelisp-phase47-compiler--emit-cons-slot-copy
        node buf nelisp-nlconsbox--offset-cdr))
      ((= tag 14)               ; cons-cdr-raw
       (nelisp-phase47-compiler--emit-cons-cdr-raw node buf))
      ((= tag 59)               ; sexp-payload-ptr
       (nelisp-phase47-compiler--emit-sexp-payload-ptr node buf))
      ((= tag 60)               ; sexp-payload-ptr-record
       (nelisp-phase47-compiler--emit-sexp-payload-ptr-record node buf))
      ((= tag 52)               ; record-type-tag
       (nelisp-phase47-compiler--emit-record-type-tag node buf))
      ((= tag 48)               ; record-slot-count
       (nelisp-phase47-compiler--emit-record-slot-count node buf))
      ((= tag 49)               ; record-slot-ref
       (nelisp-phase47-compiler--emit-record-slot-ref node buf))
      ((= tag 50)               ; record-slot-ref-ptr
       (nelisp-phase47-compiler--emit-record-slot-ref-ptr node buf))
      ((= tag 51)               ; record-slot-set
       (nelisp-phase47-compiler--emit-record-slot-set node buf))
      ((= tag 81)               ; vector-len
       (nelisp-phase47-compiler--emit-vector-len node buf))
      ((= tag 83)               ; vector-ref
       (nelisp-phase47-compiler--emit-vector-ref node buf))
      ((= tag 84)               ; vector-ref-ptr
       (nelisp-phase47-compiler--emit-vector-ref-ptr node buf))
      ((= tag 85)               ; vector-slot-set
       (nelisp-phase47-compiler--emit-vector-slot-set node buf))
      ((= tag 82)               ; vector-make
       (nelisp-phase47-compiler--emit-vector-make node buf))
      ((= tag 47)               ; record-make
       (nelisp-phase47-compiler--emit-record-make node buf))
      ((= tag 9)                ; cell-value
       (nelisp-phase47-compiler--emit-cell-value node buf))
      ((= tag 8)                ; cell-set-value
       (nelisp-phase47-compiler--emit-cell-set-value node buf))
      ((= tag 6)                ; cell-make
       (nelisp-phase47-compiler--emit-cell-make node buf))
      ((= tag 7)                ; cell-null-p
       (nelisp-phase47-compiler--emit-cell-null-p node buf))
      ((= tag 75)               ; str-len
       (nelisp-phase47-compiler--emit-str-len node buf))
      ((= tag 69)               ; str-bytes
       (nelisp-phase47-compiler--emit-str-bytes node buf))
      ((= tag 70)               ; str-bytes-ptr
       (nelisp-phase47-compiler--emit-str-bytes-ptr node buf))
      ((= tag 68)               ; str-byte-at
       (nelisp-phase47-compiler--emit-str-byte-at node buf))
      ((= tag 73)               ; str-eq
       (nelisp-phase47-compiler--emit-str-eq node buf))
      ((= tag 76)               ; symbol-eq
       (nelisp-phase47-compiler--emit-symbol-eq node buf))
      ((= tag 77)               ; symbol-name-eq
       (nelisp-phase47-compiler--emit-symbol-name-eq node buf))
      ((= tag 58)               ; sexp-name-eq
       (nelisp-phase47-compiler--emit-sexp-name-eq node buf))
      ((= tag 63)               ; sexp-write-nil
       (nelisp-phase47-compiler--emit-sexp-write-tag
        node buf nelisp-sexp--tag-nil))
      ((= tag 66)               ; sexp-write-t
       (nelisp-phase47-compiler--emit-sexp-write-tag
        node buf nelisp-sexp--tag-t))
      ((= tag 64)               ; sexp-write-str
       (nelisp-phase47-compiler--emit-sexp-write-alloc
        node buf "nl_alloc_str"))
      ((= tag 65)               ; sexp-write-symbol
       (nelisp-phase47-compiler--emit-sexp-write-alloc
        node buf "nl_alloc_symbol"))
      ((= tag 90)               ; sexp-write-symbol-lit
       (nelisp-phase47-compiler--emit-sexp-write-symbol-lit node buf))
      ((= tag 91)               ; sexp-write-str-lit
       (nelisp-phase47-compiler--emit-sexp-write-str-lit node buf))
      ((= tag 62)               ; sexp-write-float
       (nelisp-phase47-compiler--emit-sexp-write-float node buf))
      ((= tag 36)               ; mut-str-make-empty
       (nelisp-phase47-compiler--emit-mut-str-make-empty node buf))
      ((= tag 37)               ; mut-str-push-byte
       (nelisp-phase47-compiler--emit-mut-str-push-2arg
        node buf "nl_mut_str_push_byte" :byte))
      ((= tag 38)               ; mut-str-push-codepoint
       (nelisp-phase47-compiler--emit-mut-str-push-2arg
        node buf "nl_mut_str_push_codepoint" :cp))
      ((= tag 35)               ; mut-str-len
       (nelisp-phase47-compiler--emit-mut-str-len node buf))
      ((= tag 34)               ; mut-str-finalize
       (nelisp-phase47-compiler--emit-mut-str-finalize node buf))
      ((= tag 71)               ; str-char-count
       (nelisp-phase47-compiler--emit-str-char-count node buf))
      ((= tag 72)               ; str-codepoint-at
       (nelisp-phase47-compiler--emit-str-codepoint-at node buf))
      ((= tag 74)               ; str-is-alphanumeric-at
       (nelisp-phase47-compiler--emit-str-is-alphanumeric-at node buf))
      ((= tag 3)                ; atomic-fetch-add
       (nelisp-phase47-compiler--emit-atomic-fetch-add node buf))
      ((= tag 2)                ; atomic-compare-exchange
       (nelisp-phase47-compiler--emit-atomic-compare-exchange node buf))
      ((= tag 41)               ; ptr-read-u64
       (nelisp-phase47-compiler--emit-ptr-read node buf "nl_ptr_read_u64"))
      ((= tag 45)               ; ptr-write-u64
       (nelisp-phase47-compiler--emit-ptr-write node buf "nl_ptr_write_u64"))
      ((= tag 42)               ; ptr-read-u8
       (nelisp-phase47-compiler--emit-ptr-read node buf "nl_ptr_read_u8"))
      ((= tag 46)               ; ptr-write-u8
       (nelisp-phase47-compiler--emit-ptr-write node buf "nl_ptr_write_u8"))
      ((= tag 39)               ; ptr-read-u16
       (nelisp-phase47-compiler--emit-ptr-read node buf "nl_ptr_read_u16"))
      ((= tag 43)               ; ptr-write-u16
       (nelisp-phase47-compiler--emit-ptr-write node buf "nl_ptr_write_u16"))
      ((= tag 40)               ; ptr-read-u32
       (nelisp-phase47-compiler--emit-ptr-read node buf "nl_ptr_read_u32"))
      ((= tag 44)               ; ptr-write-u32
       (nelisp-phase47-compiler--emit-ptr-write node buf "nl_ptr_write_u32"))
      ((= tag 0)                ; alloc-bytes
       (nelisp-phase47-compiler--emit-alloc-bytes node buf))
      ((= tag 20)               ; dealloc-bytes
       (nelisp-phase47-compiler--emit-dealloc-bytes node buf))
      ((= tag 78)               ; syscall-direct
       (nelisp-phase47-compiler--emit-syscall-direct node buf))
      ((= tag 15)               ; cons-make
       (nelisp-phase47-compiler--emit-cons-make node buf))
      ((= tag 16)               ; cons-make-with-clone
       (nelisp-phase47-compiler--emit-cons-make-with-clone node buf))
      ((= tag 18)               ; cons-set-car
       (nelisp-phase47-compiler--emit-cons-set-slot
        node buf 'nl_consbox_set_car))
      ((= tag 19)               ; cons-set-cdr
       (nelisp-phase47-compiler--emit-cons-set-slot
        node buf 'nl_consbox_set_cdr))
      ((= tag 10)               ; cmp
       (nelisp-phase47-compiler--emit-cmp node buf))
      ((= tag 29)               ; if
       (nelisp-phase47-compiler--emit-if node buf))
      ((= tag 88)               ; value-seq
       (dolist (child (nelisp-phase47-compiler--ir-get node :forms))
         (nelisp-phase47-compiler--emit-value child buf)))
      ((= tag 86)               ; while
       (nelisp-phase47-compiler--emit-while node buf))
      ((= tag 11)               ; cond
       (nelisp-phase47-compiler--emit-cond node buf))
      ((= tag 33)               ; logic
       (nelisp-phase47-compiler--emit-logic node buf))
      ((= tag 32)               ; let-rt
       ;; Runtime let in value context: evaluate value-ir → rax, spill
       ;; to frame slot, then evaluate body → rax (= function return).
       (let* ((slot (nelisp-phase47-compiler--ir-get node :slot))
              (value-ir (nelisp-phase47-compiler--ir-get node :value-ir))
              (disp (- (* 8 (1+ slot)))))
         (nelisp-phase47-compiler--emit-value value-ir buf)
         (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rbp disp 'rax)
         (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :body) buf)))
      ((= tag 89)               ; let-rt-n
       ;; Parallel multi-binding `let': initializer IR was parsed in
       ;; the original env/fenv, then the body sees all spilled slots.
       (nelisp-phase47-compiler--emit-let-rt-n-bindings node buf)
       (nelisp-phase47-compiler--emit-value
        (nelisp-phase47-compiler--ir-get node :body) buf))
      ((= tag 92)               ; aot-root-scope
       (nelisp-phase47-compiler--emit-aot-root-scope node buf))
      ;; (tag 78 syscall-direct already handled above; duplicate `pcase'
      ;; arm was dead, dropped under `cond' first-match semantics)
      ((= tag 80)               ; table-lookup
       ;; Doc 49 Wave 11.1: static-imm32-table-lookup → u32 in rax.
       (nelisp-phase47-compiler--emit-table-lookup node buf))
      (t
       (let ((kind (nelisp-phase47-compiler--ir-kind node)))
         (signal 'nelisp-phase47-compiler-error
                 (list :unknown-value-kind kind))))))))

(defun nelisp-phase47-compiler--emit-arith (node buf)
  "Emit a runtime arithmetic op, result in rax.
Strategy: evaluate B into rax, push, evaluate A into rax, pop into
r10, then OP rax, r10.  Push/pop are byte-fixed so pass invariance
holds.  r10 is caller-saved per SysV AND not in the arg-reg list so
  the scratch never aliases a parameter register (= the bug seen in
chained calls where rcx held both `d' param and a scratch value)."
  (let ((op (nelisp-phase47-compiler--ir-get node :op))
        (a (nelisp-phase47-compiler--ir-get node :a))
        (b (nelisp-phase47-compiler--ir-get node :b)))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (progn
          (nelisp-phase47-compiler--emit-value b buf)
          (nelisp-asm-arm64-str-pre-sp-16 buf 'x0)
          (nelisp-phase47-compiler--emit-value a buf)
          (nelisp-asm-arm64-ldr-post-sp-16 buf 'x9)
          (cond
           ((eq op '+) (nelisp-asm-arm64-add-reg-reg buf 'x0 'x0 'x9))
           ((eq op '-) (nelisp-asm-arm64-sub-reg-reg buf 'x0 'x0 'x9))
           ((eq op '*) (nelisp-asm-arm64-mul-reg-reg buf 'x0 'x0 'x9))
           ((eq op 'logior) (nelisp-asm-arm64-orr-reg-reg buf 'x0 'x0 'x9))
           ((eq op 'logand) (nelisp-asm-arm64-and-reg-reg buf 'x0 'x0 'x9))
           ((eq op 'logxor) (nelisp-asm-arm64-eor-reg-reg buf 'x0 'x0 'x9))
           (t
            (signal 'nelisp-phase47-compiler-error
                    (list :unknown-arith-op op)))))
      ;; Compute B -> rax.
      (nelisp-phase47-compiler--emit-value b buf)
      ;; push rax (save B on stack).
      (nelisp-asm-x86_64-push buf 'rax)
      ;; Compute A -> rax.
      (nelisp-phase47-compiler--emit-value a buf)
      ;; pop r10 (= recover B into r10; r10 not in arg-regs).
      (nelisp-asm-x86_64-pop buf 'r10)
      (cond
       ((eq op '+) (nelisp-asm-x86_64-add-reg-reg buf 'rax 'r10))
       ((eq op '-) (nelisp-asm-x86_64-sub-reg-reg buf 'rax 'r10))
       ((eq op '*)
        (nelisp-phase47-compiler--imul-reg-reg buf 'rax 'r10))
       ;; Doc 100 §100.D bitwise binops.  Same MR-form shape as ADD/SUB,
       ;; just a different opcode byte.
       ((eq op 'logior) (nelisp-asm-x86_64-or-reg-reg buf 'rax 'r10))
       ((eq op 'logand) (nelisp-asm-x86_64-and-reg-reg buf 'rax 'r10))
       ((eq op 'logxor) (nelisp-asm-x86_64-xor-reg-reg buf 'rax 'r10))
       (t
        (signal 'nelisp-phase47-compiler-error
                (list :unknown-arith-op op)))))))

(defun nelisp-phase47-compiler--emit-shift (node buf)
  "Emit a variable-count shift NODE; result in rax (Doc 100 §100.D).
Strategy mirrors `--emit-arith' for the operand evaluation but
diverges at the final op: x86_64 SHL / SAR by a variable count
require the count to live in CL (= low 8 bits of RCX).  Sequence:

  <emit B>            -> rax           (= count)
  push rax            (save count on stack)
  <emit A>            -> rax           (= value)
  pop r10             (count into r10)
  mov rcx, r10        (count into rcx so cl carries the low byte)
  shl/sar rax, cl

RCX is caller-saved per SysV and not in the arg-reg list (= same
property `--emit-arith' relies on for r10), so it cannot alias a
live parameter register in the surrounding defun."
  (let ((op (nelisp-phase47-compiler--ir-get node :op))
        (a (nelisp-phase47-compiler--ir-get node :a))
        (b (nelisp-phase47-compiler--ir-get node :b)))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (progn
          (nelisp-phase47-compiler--emit-value b buf)
          (nelisp-asm-arm64-str-pre-sp-16 buf 'x0)
          (nelisp-phase47-compiler--emit-value a buf)
          (nelisp-asm-arm64-ldr-post-sp-16 buf 'x9)
          (cond
           ((eq op 'shl) (nelisp-asm-arm64-lslv buf 'x0 'x0 'x9))
           ((eq op 'sar) (nelisp-asm-arm64-asrv buf 'x0 'x0 'x9))
           (t
            (signal 'nelisp-phase47-compiler-error
                    (list :unknown-shift-op op)))))
      ;; Compute B -> rax.
      (nelisp-phase47-compiler--emit-value b buf)
      ;; push rax (save B on stack).
      (nelisp-asm-x86_64-push buf 'rax)
      ;; Compute A -> rax.
      (nelisp-phase47-compiler--emit-value a buf)
      ;; pop r10 (= recover B into r10).
      (nelisp-asm-x86_64-pop buf 'r10)
      ;; mov rcx, r10 (= count into rcx; cl = rcx[0:8]).
      (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'r10)
      (cond
       ((eq op 'shl) (nelisp-asm-x86_64-shl-rax-cl buf))
       ((eq op 'sar) (nelisp-asm-x86_64-sar-rax-cl buf))
       (t
        (signal 'nelisp-phase47-compiler-error
                (list :unknown-shift-op op)))))))

(defun nelisp-phase47-compiler--call-arg-trivial-p (node)
  "Return non-nil if NODE is a Phase 47 IR value that emits to a GP reg
without clobbering any other register.

A `trivial' arg is one whose `--emit-value' realization in `rax' is
equivalent to a single `mov target-reg, <something>' once we steer
the destination away from `rax'.  Specifically:

  (:kind imm   :value INT)    -> `mov target, imm32' (= 7 bytes)
  (:kind ref   :class gp/nil  -> `mov target, [rbp - 8*(slot+1)]'
         :slot 0..13)            (= 4 bytes, disp8 in range)

Anything else (arith, call, extern-call, cmp, if, while, cond, logic,
nested forms, or an `f64'-class ref) is *complex* — its emit may
touch rax / r10 / r11 / xmm regs and must spill via push/pop before
the trivial-suffix landing pad runs.

Byte-length invariance (Doc 92): pass1 and pass2 see identical
parse-time-fixed `:kind' / `:value' / `:slot' / `:class' fields, so
the classification is deterministic and emits the same byte count
across both passes for the same NODE."
  (let ((kind (nelisp-phase47-compiler--ir-kind node)))
    (cond
     ((eq kind 'imm)
      (let ((v (nelisp-phase47-compiler--ir-get node :value)))
        ;; mov-imm32 accepts signed [-2^31, 2^31-1] or unsigned [0, 2^32-1];
        ;; reject out-of-range integers conservatively (fall back to the
        ;; existing emit-value path which may use a wider form).
        (and (integerp v)
             (>= v (- (ash 1 31)))
             (< v (ash 1 32)))))
     ((eq kind 'ref)
      (and (memq (or (nelisp-phase47-compiler--ir-get node :class) 'gp) '(gp nil))
           (let ((s (nelisp-phase47-compiler--ir-get node :slot)))
             (and (integerp s) (<= 0 s 13))))))))

(defun nelisp-phase47-compiler--emit-trivial-into-reg (node target buf)
  "Emit NODE's value directly into TARGET register (no spill).
NODE must satisfy `nelisp-phase47-compiler--call-arg-trivial-p'."
  (let ((kind (nelisp-phase47-compiler--ir-kind node)))
    (cond
     ((eq kind 'imm)
      ;; mov TARGET, imm32   = 7 bytes (REX.W + 0xC7 /0 + imm32)
      (nelisp-asm-x86_64-mov-imm32 buf target (nelisp-phase47-compiler--ir-get node :value)))
     ((eq kind 'ref)
      ;; mov TARGET, [rbp - 8*(slot+1)]  = 4 bytes (REX.W + 0x8B + ModR/M + disp8)
      (let ((disp (- (* 8 (1+ (nelisp-phase47-compiler--ir-get node :slot))))))
        (nelisp-asm-x86_64-mov-reg-mem-disp8 buf target 'rbp disp)))
     (t
      (signal 'nelisp-phase47-compiler-error
              (list :trivial-emit-unexpected-kind kind))))))

(defun nelisp-phase47-compiler--emit-call (node buf)
  "Emit a call to NODE's named function using the current ABI.

Strategy (ABI-agnostic), with W7.6a trivial-suffix optimization:
  1. Classify args into a *complex* prefix and a *trivial* suffix
     (trailing run of `imm' / GP-class `ref' nodes).
  2. Complex prefix: evaluate each into rax, then push rax (stack-save
     order).  After all complex args are pushed, pop into their target
     ABI registers in reverse (= last pushed is first popped).  This
     preserves the original spill semantics for args that may clobber
     each other's regs.
  3. Trivial suffix: emit each directly into its target ABI register
     via `mov target, imm32' (= imm) or `mov target, [rbp - disp8]'
     (= ref).  No stack spill — trivial emits don't touch other regs.
  4. Win64: sub rsp, 32 (shadow space) before call; add rsp, 32 after.
  5. call <NAME> (intra-text rel32 fixup).
  6. Return value already in rax.

The number of pushes equals the *complex* arg count, which is
parse-time fixed, so net rsp change is still zero across the call
sequence.  Pre-call `needs-align' / `shadow' bookkeeping is
unaffected.  Byte-length invariance holds (Doc 92): for any given
NODE, pass1 and pass2 traverse the same branch (each arg's kind /
class / slot / value is parse-time fixed) and emit the same byte
count."
  (let* ((name (nelisp-phase47-compiler--ir-get node :name))
         (args (nelisp-phase47-compiler--ir-get node :args))
         (n (length args))
         (cur-arg-regs (nelisp-phase47-compiler--current-arg-regs))
         (regs (cl-subseq cur-arg-regs 0 n))
         ;; Stack alignment correction (Doc 111 §111.E fix).
         (arity (or nelisp-phase47-compiler--current-defun-arity 0))
         (needs-align (= (logand arity 1) 1))
         ;; Win64 shadow space: 32 bytes reserved by caller before CALL.
         (shadow (if (eq nelisp-phase47-compiler--abi 'win64) 32 0))
         ;; W7.6a: split args into [complex-prefix | trivial-suffix].
         ;; Count trailing trivial args (right-to-left, stop at first
         ;; complex).  Walking the reverse of args lets us count
         ;; without consing the full reverse list twice.
         (trivial-suffix-len
          (let ((count 0)
                (rest (reverse args))
                (done nil))
            (while (and rest (not done))
              (if (nelisp-phase47-compiler--call-arg-trivial-p (car rest))
                  (progn (setq count (1+ count))
                         (setq rest (cdr rest)))
                (setq done t)))
            count))
         (complex-count (- n trivial-suffix-len))
         (complex-args (cl-subseq args 0 complex-count))
         (trivial-args (cl-subseq args complex-count))
         (complex-regs (cl-subseq regs 0 complex-count))
         (trivial-regs (cl-subseq regs complex-count)))
    ;; (1) Complex prefix: evaluate -> push rax.
    (dolist (a complex-args)
      (nelisp-phase47-compiler--emit-value a buf)
      (nelisp-asm-x86_64-push buf 'rax))
    ;; (2) Pop complex args into their target regs in reverse order.
    (dolist (r (reverse complex-regs))
      (nelisp-asm-x86_64-pop buf r))
    ;; (3) Trivial suffix: emit each directly into its target reg.
    ;; Order is free since trivial emits never clobber other arg regs;
    ;; we walk source order for deterministic byte layout.
    (cl-mapc (lambda (a r)
               (nelisp-phase47-compiler--emit-trivial-into-reg a r buf))
             trivial-args trivial-regs)
    (when needs-align
      (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8))
    ;; Allocate Win64 shadow space (0 for SysV).
    (when (> shadow 0)
      (nelisp-asm-x86_64-sub-imm32 buf 'rsp shadow))
    ;; call <NAME> — intra-text rel32 resolved at finalize time.
    (nelisp-asm-x86_64-call-rel32 buf name)
    ;; Reclaim shadow space.
    (when (> shadow 0)
      (nelisp-asm-x86_64-add-imm32 buf 'rsp shadow))
    (when needs-align
      (nelisp-asm-x86_64-add-imm32 buf 'rsp 8))))

(defun nelisp-phase47-compiler--emit-extern-call (node buf)
  "Emit a SysV AMD64 call to an extern symbol NODE.
Doc 100 §100.A introduced the all-i64 form; Doc 122 §122.C extends
this to mixed i64 / f64 args (= per-arg `:cls' tag) + f64 return
(= `:ret-class' = `f64') + variadic calls (= `:varargs-p' t with
`:f64-count' = AL register value).

Same strategy as `--emit-call' but emits the `call' opcode bytes
directly + records a `plt32' reloc against an external symbol
instead of an intra-text label fixup.  The ELF writer's
`.rela.text' machinery surfaces the reloc for `ld' to resolve
against the final linked binary (= libc / libm / Rust `.rlib'
exporting matching `#[no_mangle] pub extern \"C\"' helpers).

Arg placement strategy (W7.6b trivial-suffix opt):
  1. Classify args into a *complex* prefix and a *trivial* suffix.
     A trailing arg qualifies for the suffix iff it is `:cls' gp
     AND `--call-arg-trivial-p' (= literal int / GP-class ref).
     f64 args and non-trivial gp args belong to the complex prefix,
     and any non-trivial / f64 arg stops the suffix walk.
  2. Complex prefix: evaluate each in source order, transferring an
     f64 result from xmm0 → rax via MOVQ before the unified `push
     rax' save.  This keeps the spill pipeline uniform so the next
     arg's emit can clobber xmm0 / rax freely.
  3. Pop the complex prefix in reverse, dispatching each value into
     rdi-r9 (gp args) or xmm0-7 (f64 args) per its `:cls' tag.  f64
     pops first land in rax, then MOVQ rax → target xmm reg.
  4. Trivial suffix: emit each directly into its target gp-reg via
     `mov target, imm32' (= imm) or `mov target, [rbp - disp8]'
     (= ref).  No stack spill — trivial emits don't touch other
     regs, and trivial suffix targets are all distinct gp-regs
     that the prefix pops did not write.
  5. SysV GP stack args, when present, are pushed right-to-left after
     register args have been materialized.  When any stack-GP arg is
     non-trivial, 129.7G evaluates all args left-to-right into a
     temporary call-spill area, reloads register args from that area,
     then pushes outgoing stack args from it.
  6. For variadic calls, materialise `:f64-count' in AL via
     `mov eax, imm32' (= zero-extended, satisfies SysV ABI §3.5.7
     contract that AL contains the upper bound on f64 args).  This
     runs after register and stack placement so rax is free to
     clobber by then.

Unlike `--emit-call' the target name is NOT validated against the
compile-time defuns alist; the parser already accepted SYM as a
bare symbol literal under `(extern-call SYM ...)'.  Out-of-budget
args are rejected at parse time per class.

Byte-length invariance (Doc 92): the suffix-vs-prefix split is
deterministic from parse-time-fixed fields (`:cls', `:kind',
`:value', `:slot', `:class') so pass1 and pass2 traverse the
same branch and emit the same byte count."
  (let* ((name (nelisp-phase47-compiler--ir-get node :name))
         (args (nelisp-phase47-compiler--ir-get node :args))
         (ret-class (or (nelisp-phase47-compiler--ir-get node :ret-class) 'gp))
         (varargs-p (nelisp-phase47-compiler--ir-get node :varargs-p))
         (f64-count (or (nelisp-phase47-compiler--ir-get node :f64-count) 0))
         (arg-count (length args))
         ;; Per-class register pools, sliced to the number of args
         ;; of that class.  Iteration order matches source order:
         ;; the Nth gp arg → Nth GP reg, the Nth f64 arg → Nth xmm
         ;; reg, both pools indexed independently.
         (gp-args (cl-remove-if-not
                   (lambda (a) (eq (nelisp-phase47-compiler--ir-get a :cls) 'gp))
                   args))
         (f64-args (cl-remove-if-not
                    (lambda (a) (eq (nelisp-phase47-compiler--ir-get a :cls) 'f64))
                    args))
         (gp-reg-budget (length (nelisp-phase47-compiler--current-arg-regs)))
         (gp-regs (cl-subseq (nelisp-phase47-compiler--current-arg-regs)
                             0 (min (length gp-args) gp-reg-budget)))
         (xmm-regs (cl-subseq nelisp-phase47-compiler--xmm-arg-regs
                              0 (length f64-args)))
         ;; Map each arg back to its target register so the reverse-
         ;; pop loop knows where to deposit the popped value.  GP args
         ;; beyond the register budget become `(:stack N)' SysV stack
         ;; targets; those are emitted separately after register args
         ;; are in place.
         (gp-cursor 0)
         (f64-cursor 0)
         (arg-targets
          (mapcar (lambda (a)
                    (if (eq (nelisp-phase47-compiler--ir-get a :cls) 'f64)
                        (let ((r (nth f64-cursor xmm-regs)))
                          (setq f64-cursor (1+ f64-cursor))
                          r)
                      (let ((r (if (< gp-cursor gp-reg-budget)
                                   (nth gp-cursor gp-regs)
                                 (list :stack (- gp-cursor gp-reg-budget)))))
                        (setq gp-cursor (1+ gp-cursor))
                        r)))
                  args))
         (register-args
          (cl-loop for a in args
                   for target in arg-targets
                   unless (and (consp target) (eq (car target) :stack))
                   collect a))
         (register-targets
          (cl-loop for target in arg-targets
                   unless (and (consp target) (eq (car target) :stack))
                   collect target))
         (stack-args
          (cl-loop for a in args
                   for target in arg-targets
                   when (and (consp target) (eq (car target) :stack))
                   collect a))
         (stack-indexed
          (cl-loop for a in args
                   for target in arg-targets
                   for idx from 0
                   when (and (consp target) (eq (car target) :stack))
                   collect (list idx a target)))
         ;; W7.6b: split args into [complex-prefix | trivial-suffix].
         ;; Suffix eligibility is :cls gp AND --call-arg-trivial-p;
         ;; an f64 arg or a non-trivial gp arg stops the walk.  Walk
         ;; the reverse of register-bound args to count trailing
         ;; trivial gp args.  Stack-bound args are handled below.
         (trivial-suffix-len
          (let ((count 0)
                (rest (reverse register-args))
                (done nil))
            (while (and rest (not done))
              (let ((a (car rest)))
                (if (and (eq (nelisp-phase47-compiler--ir-get a :cls) 'gp)
                         (nelisp-phase47-compiler--call-arg-trivial-p a))
                    (progn (setq count (1+ count))
                           (setq rest (cdr rest)))
                  (setq done t))))
            count))
         (complex-count (- (length register-args) trivial-suffix-len))
         (complex-args (cl-subseq register-args 0 complex-count))
         (trivial-args (cl-subseq register-args complex-count))
         (complex-targets (cl-subseq register-targets 0 complex-count))
         (trivial-targets (cl-subseq register-targets complex-count))
         ;; Stack alignment correction (Doc 111 §111.E fix).
         ;; Post-prologue body-entry rsp:
         ;;   - even arity (= 0, 2, 4, 6): rsp ≡ 0 mod 16 (good for call)
         ;;   - odd arity (= 1, 3, 5):     rsp ≡ 8 mod 16 (needs +8 sub)
         ;; f64-class defuns round to even arity in the prologue so
         ;; rsp is already aligned post-spill (= see `--emit-defun');
         ;; only gp-class defuns need the runtime correction below.
         (arity (or nelisp-phase47-compiler--current-defun-arity 0))
         (needs-align (= (logand (+ arity (length stack-args)) 1) 1))
         (general-stack-spill-p
          (cl-some
           (lambda (a)
             (not (nelisp-phase47-compiler--call-arg-trivial-p a)))
           stack-args))
         (spill-needs-align
          (= (logand (+ arity (length stack-args) arg-count) 1) 1))
         (call-temp-save-count 0)
         (call-needs-align needs-align))
    (when (and stack-args
               (not (and (eq nelisp-phase47-compiler--arch 'x86_64)
                         (eq nelisp-phase47-compiler--abi 'sysv))))
      (signal 'nelisp-phase47-compiler-error
              (list :extern-call-stack-gp-args-unsupported name
                    nelisp-phase47-compiler--arch
                    nelisp-phase47-compiler--abi)))
    (dolist (a stack-args)
      (unless (eq (nelisp-phase47-compiler--ir-get a :cls) 'gp)
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-stack-arg-not-trivial name))))
    (if general-stack-spill-p
        (progn
          (setq call-temp-save-count arg-count
                call-needs-align spill-needs-align)
          ;; Evaluate every arg left-to-right into temporary stack saves.
          ;; The saves remain above the outgoing call frame until after
          ;; CALL returns; only the actual outgoing stack args are pushed
          ;; below them.
          (dolist (a args)
            (nelisp-phase47-compiler--emit-value a buf)
            (when (eq (nelisp-phase47-compiler--ir-get a :cls) 'f64)
              (nelisp-asm-x86_64-movq-r64-xmm buf 'rax 'xmm0))
            (nelisp-asm-x86_64-push buf 'rax))
          (cl-loop for a in args
                   for target in arg-targets
                   for idx from 0
                   unless (and (consp target) (eq (car target) :stack))
                   do
                   (let ((disp (* 8 (- (1- arg-count) idx))))
                     (if (memq target nelisp-phase47-compiler--xmm-arg-regs)
                         (progn
                           (nelisp-asm-x86_64-mov-reg-mem-rsp-disp
                            buf 'rax disp)
                           (nelisp-asm-x86_64-movq-xmm-r64
                            buf target 'rax))
                       (nelisp-asm-x86_64-mov-reg-mem-rsp-disp
                        buf target disp))))
          (when call-needs-align
            (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8))
          (let ((pushed-stack 0))
            (dolist (entry (reverse stack-indexed))
              (let* ((idx (nth 0 entry))
                     (source-disp (* 8 (- (1- arg-count) idx)))
                     (align-disp (if call-needs-align 8 0))
                     (disp (+ source-disp
                              align-disp
                              (* 8 pushed-stack))))
                (nelisp-asm-x86_64-mov-reg-mem-rsp-disp
                 buf 'r10 disp)
                (nelisp-asm-x86_64-push buf 'r10)
                (setq pushed-stack (1+ pushed-stack))))))
      (dolist (a stack-args)
        (unless (nelisp-phase47-compiler--call-arg-trivial-p a)
          (signal 'nelisp-phase47-compiler-error
                  (list :extern-call-stack-arg-not-trivial name))))
      ;; (1) Complex prefix: push each evaluated arg.  f64 args land
      ;; in xmm0 (per `--emit-value' contract for f64 nodes) so we
      ;; transfer to rax first before the unified push.
      (dolist (a complex-args)
        (nelisp-phase47-compiler--emit-value a buf)
        (when (eq (nelisp-phase47-compiler--ir-get a :cls) 'f64)
          ;; xmm0 → rax (64-bit bit pattern, preserves the f64 value).
          (nelisp-asm-x86_64-movq-r64-xmm buf 'rax 'xmm0))
        (nelisp-asm-x86_64-push buf 'rax))
      ;; (2) Pop in reverse (= last pushed → first popped) and dispatch.
      (dolist (target (reverse complex-targets))
        (if (memq target nelisp-phase47-compiler--xmm-arg-regs)
            ;; f64 target — pop into rax then MOVQ → xmm.
            (progn
              (nelisp-asm-x86_64-pop buf 'rax)
              (nelisp-asm-x86_64-movq-xmm-r64 buf target 'rax))
          ;; GP target — pop directly.
          (nelisp-asm-x86_64-pop buf target)))
      ;; (3) Trivial suffix: emit each directly into its target gp-reg.
      ;; Order is free since trivial emits never clobber other arg regs;
      ;; we walk source order for deterministic byte layout.  All targets
      ;; here are gp-regs (suffix gating restricts to :cls gp).
      (cl-mapc (lambda (a r)
                 (nelisp-phase47-compiler--emit-trivial-into-reg a r buf))
               trivial-args trivial-targets)
      ;; Insert the 8-byte alignment correction before stack args are
      ;; pushed.  That keeps the first stack arg at the ABI-visible top
      ;; of the outgoing argument area while any pad lives below it.
      (when call-needs-align
        (nelisp-asm-x86_64-sub-imm32 buf 'rsp 8))
      ;; SysV outgoing stack args are pushed right-to-left so the first
      ;; stack arg ends up closest to the return address in the callee.
      (dolist (a (reverse stack-args))
        (nelisp-phase47-compiler--emit-trivial-into-reg a 'rax buf)
        (nelisp-asm-x86_64-push buf 'rax)))
    ;; Materialise AL = f64-count for variadic calls (SysV ABI §3.5.7).
    ;; `mov eax, imm32' is a 5-byte sequence (= REX-less; the imm32
    ;; zero-extends into RAX, clearing the upper 32 bits which is
    ;; exactly what AL = N requires).  Note: this clobbers rax, so
    ;; it MUST happen AFTER any rax-bound arg was popped into its
    ;; GP target and after stack args have been copied out.
    (when varargs-p
      (nelisp-asm-x86_64-mov-imm32 buf 'rax f64-count))
    ;; Win64: allocate 32-byte shadow space before CALL.
    ;; SysV: shadow = 0, this is a no-op.
    (let ((shadow (if (eq nelisp-phase47-compiler--abi 'win64) 32 0)))
      (when (> shadow 0)
        (nelisp-asm-x86_64-sub-imm32 buf 'rsp shadow))
      ;; Emit the `call rel32' opcode (0xE8) + 4-byte zero placeholder
      ;; + record a PLT32 reloc at the placeholder offset.  Section is
      ;; `text' (default) since we are inside an `.text' defun body.
      (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
      (nelisp-asm-x86_64-reloc-plt32-here
       buf (symbol-name name) -4 'text)
      ;; Reclaim shadow space.
      (when (> shadow 0)
        (nelisp-asm-x86_64-add-imm32 buf 'rsp shadow)))
    ;; Reclaim outgoing SysV stack arguments, preserving rax/xmm0.
    (when stack-args
      (nelisp-asm-x86_64-add-imm32 buf 'rsp (* 8 (length stack-args))))
    ;; Undo the alignment correction.  rax (= i64 return) or xmm0
    ;; (= f64 return) is preserved because `add rsp, 8' doesn't
    ;; touch any GPR / xmm.
    (when call-needs-align
      (nelisp-asm-x86_64-add-imm32 buf 'rsp 8))
    (when (> call-temp-save-count 0)
      (nelisp-asm-x86_64-add-imm32 buf 'rsp (* 8 call-temp-save-count)))
    ;; Caller convention: extern-call result is i64 in rax (default)
    ;; or f64 in xmm0 (when :ret-class = f64).  Both ABIs agree on the
    ;; return register convention for scalar values.
    (ignore ret-class)))

(defun nelisp-phase47-compiler--emit-syscall-direct (node buf)
  "Emit a Linux x86_64 raw SYSCALL instruction for NODE into BUF.
NODE is a `:kind syscall-direct' IR node with keys :nr, :a0, :a1,
:a2, :a3, :a4, :a5 (all value-producing sub-nodes returning i64).

Evaluation strategy (push/pop avoids clobbering param spill slots):
  1. Evaluate each argument in left-to-right (NR, A0..A5) order,
     pushing rax after each evaluation (7 pushes total; TOS = A5
     after the final push, NR is deepest).
  2. Pop in reverse into the Linux SYSCALL ABI registers:
       pop r9   ← A5   pop r8   ← A4   pop r10  ← A3
       pop rdx  ← A2   pop rsi  ← A1   pop rdi  ← A0
       pop rax  ← NR
  3. Emit SYSCALL (0F 05).
  Returns the kernel's raw i64 in rax (negative = -errno on error).

Byte-count is fixed per pass so the pass-1/pass-2 invariant holds:
  7 × (emit_arg: variable, but same in both passes) + 7 × push +
  7 × pop + SYSCALL = deterministic per defun.

x86_64 only — callers should gate with `:requires-arch x86_64' in
the compile-elisp-objects manifest."
  (let ((nr (nelisp-phase47-compiler--ir-get node :nr))
        (a0 (nelisp-phase47-compiler--ir-get node :a0))
        (a1 (nelisp-phase47-compiler--ir-get node :a1))
        (a2 (nelisp-phase47-compiler--ir-get node :a2))
        (a3 (nelisp-phase47-compiler--ir-get node :a3))
        (a4 (nelisp-phase47-compiler--ir-get node :a4))
        (a5 (nelisp-phase47-compiler--ir-get node :a5)))
    ;; 1. Evaluate and push each arg onto the stack (NR first = deepest).
    (dolist (arg (list nr a0 a1 a2 a3 a4 a5))
      (nelisp-phase47-compiler--emit-value arg buf)
      (nelisp-asm-x86_64-push buf 'rax))
    ;; 2. Pop into Linux SYSCALL ABI regs (reverse order = TOS = A5 first).
    ;;    Linux SYSCALL: rax=nr, rdi=a0, rsi=a1, rdx=a2, r10=a3, r8=a4, r9=a5.
    (nelisp-asm-x86_64-pop buf 'r9)
    (nelisp-asm-x86_64-pop buf 'r8)
    (nelisp-asm-x86_64-pop buf 'r10)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-pop buf 'rax)
    ;; 3. Execute SYSCALL.
    (nelisp-asm-x86_64-syscall buf)))

;; ---- Doc 100 v2 §100.B Sexp ABI direct-access emit ----

(defun nelisp-phase47-compiler--emit-sexp-tag (node buf)
  "Emit `movzx rax, byte ptr [rdi]' after computing NODE's :ptr into rdi.
Result: the tag byte at offset `nelisp-sexp--offset-tag' (= 0)
zero-extended to a 64-bit value in rax.  See `docs/arch/sexp-abi.md'
§5.1."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    ;; Compute :ptr into rax, then move into rdi as the base register.
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)))

(defun nelisp-phase47-compiler--emit-f64-to-i64-trunc (node buf)
  "Emit `CVTTSD2SI rax, xmm0' after computing NODE's :f64-expr into xmm0.
Result: signed i64 truncation of the f64 value, returned in rax (= gp
class).  Per Intel SDM, out-of-range f64 inputs (NaN / +- inf /
magnitude > INT64_MAX) yield the indefinite integer sentinel
0x8000000000000000 — same behaviour as Rust's `f as i64' / C
`(int64_t) f' cast.  Caller is responsible for range checking when
semantic differs from this default.

F64-EXPR is emitted via `--emit-f64-leaf-into' which now accepts
`bits-to-f64' / `f64-call' / `ref :class f64' as valid producers."
  (let ((f64-expr (nelisp-phase47-compiler--ir-get node :f64-expr)))
    (nelisp-phase47-compiler--emit-f64-leaf-into f64-expr buf 'xmm0)
    (nelisp-asm-x86_64-cvttsd2si-r64-xmm buf 'rax 'xmm0)))

(defun nelisp-phase47-compiler--emit-sexp-float-unwrap (node buf)
  "Emit f64-payload read for a `Sexp::Float(f)' value, returning the
raw 8-byte bit pattern as i64 in rax.

Strategy:
  1. Evaluate NODE's :ptr into rax (= gp class default).
  2. mov rdi, rax  — preserve the pointer for the load.
  3. mov rax, qword ptr [rdi + 8]  — read the f64 bit pattern
     directly as i64 from the Sexp payload offset (= same offset 8
     used by `sexp-int-unwrap' since Sexp::Int and Sexp::Float share
     the payload starting position; only the tag byte differs).

Returns f64 bits in rax — callers needing actual f64 arithmetic
can MOVQ to xmm0 once the bits-to-f64 grammar op (G5 TBD) ships,
or pass the i64 bits to a Rust extern via `extern-call' that does
the bit-cast internally.  No tag check — caller responsibility.

Mirror of `--emit-sexp-int-unwrap' (= same byte sequence, same
offset); separated as a distinct op for emit-time class clarity
and for future f64-class composition with `--emit-f64-leaf-into'."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-sexp--offset-payload)))

(defun nelisp-phase47-compiler--emit-sexp-int-unwrap (node buf)
  "Emit `mov rax, qword ptr [rdi + 8]' after computing NODE's :ptr into rdi.
Result: the i64 payload of a `Sexp::Int(n)' value, read from offset
`nelisp-sexp--offset-int-payload' (= 8).  No tag check — caller
must ensure :ptr points at a `Sexp::Int' variant.  See
`docs/arch/sexp-abi.md' §5.2."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-sexp--offset-int-payload)))

(defun nelisp-phase47-compiler--emit-sexp-int-make (node buf)
  "Emit the 3-instruction `Sexp::Int' constructor sequence into a caller slot.
NODE's :slot is the `*mut Sexp' destination, :val is the i64 payload.
Both sub-expressions are evaluated in turn and pushed; the saved
values are then popped into rdi (= slot) and rsi (= payload) before
the writes happen.  Emits, in order:

  mov byte ptr [rdi], `nelisp-sexp--tag-int'  (= SEXP_TAG_INT)
  mov qword ptr [rdi + `nelisp-sexp--offset-payload'], rsi
  mov rax, rdi

The bytes at `[rdi + 1, rdi + 8)' (= padding) and
`[rdi + 16, rdi + 32)' (= unused tail of the 32-byte Sexp slot) are
left unmodified — `Sexp::Int' does not use them and Rust's drop
glue dispatches solely off the tag byte.  See `docs/arch/sexp-abi.md'
§5.3."
  (let ((slot (nelisp-phase47-compiler--ir-get node :slot))
        (val (nelisp-phase47-compiler--ir-get node :val)))
    ;; Evaluate :slot then :val and push each result so the standard
    ;; pop-into-arg-reg dance places slot in rdi, val in rsi.  This
    ;; mirrors `--emit-call' / `--emit-extern-call' so future arg
    ;; expressions that themselves clobber rdi/rsi don't race.
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Pop in reverse push order: last pushed (= val) → rsi, first
    ;; pushed (= slot) → rdi.
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Write tag byte, payload, then return the slot pointer.
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rdi nelisp-sexp--tag-int)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rdi nelisp-sexp--offset-payload 'rsi)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rdi)))

;; ---- Doc 101 §101.B Cons read ops emit ----

(defun nelisp-phase47-compiler--emit-cons-null-p (node buf)
  "Emit a tag==Nil predicate for NODE's `:ptr' Sexp pointer.
Returns 1 in rax iff the tag byte at `[ptr + 0]' equals
`nelisp-sexp--tag-nil'; else returns 0."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-nil)
    (nelisp-asm-x86_64-setcc-al buf 'sete)
    (nelisp-asm-x86_64-movzx-eax-al buf)))

(defun nelisp-phase47-compiler--emit-cons-slot-copy (node buf field-off)
  "Emit the Doc 101 §2.1 boxed-slot copy for `car' / `cdr'.
NODE carries `:ptr' (= `*const Sexp') and `:slot' (= `*mut Sexp').
FIELD-OFF is 0 for `car' and 32 for `cdr'.  The emitted x86_64 path:

  1. Reads the `NlConsBox*' payload from `[ptr + 8]'.
  2. Copies 32 bytes from `[box + FIELD-OFF, box + FIELD-OFF + 32)'
     into SLOT using two 16-byte `movdqu' load/store pairs.
  3. Returns SLOT in rax.

Caller must guarantee PTR points at `Sexp::Cons(_)'."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r10 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'r10 field-off)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 0 'xmm0)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'r10 (+ field-off 16))
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 16 'xmm0)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cons-cdr-raw (node buf)
  "Emit the Doc 101 §2.1 raw cdr walker primitive.
If NODE's `:from-box' is nil, `:ptr' is a `*const Sexp' and we first
load the `NlConsBox*' payload from `[ptr + 8]'.  If `:from-box' is t,
`:ptr' already is the `NlConsBox*'.  Then:

  1. Read the cdr tag byte at `[box + 32 + 0]'.
  2. If tag == `SEXP_TAG_CONS', return `[box + 32 + 8]'
     (= next `NlConsBox*').
  3. Else return 0.

Used by the §101.B `length' list walk to follow proper-list cons
chains without materialising intermediate `Sexp' values."
  (let* ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
         (from-box (nelisp-phase47-compiler--ir-get node :from-box))
         (id (nelisp-phase47-compiler--gensym "cons-cdr-raw"))
         (nil-lbl (intern (format "%s-nil" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (if from-box
        (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
      (progn
        (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
        (nelisp-asm-x86_64-mov-reg-mem-disp8
         buf 'rdi 'rdi nelisp-sexp--offset-payload)))
    (nelisp-asm-x86_64-mov-reg-reg buf 'r11 'rdi)
    (nelisp-asm-x86_64-add-imm32 buf 'r11 nelisp-nlconsbox--offset-cdr)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'r11)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-cons)
    (nelisp-asm-x86_64-jnz-rel32 buf nil-lbl)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'r11 nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf nil-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-sexp-payload-ptr (node buf)
  "Emit a boxed-payload pointer read for NODE's `:ptr' Sexp pointer.
Doc 101 §2.3 uses this for list walks: `Cons' returns the
`NlConsBox*'; `Nil' returns 0.  Other tags also return 0 so the op is
safe to use as the loop seed in the length walker."
  (let* ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
         (id (nelisp-phase47-compiler--gensym "sexp-payload-ptr"))
         (zero-lbl (intern (format "%s-zero" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-cons)
    (nelisp-asm-x86_64-jnz-rel32 buf zero-lbl)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf zero-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-sexp-payload-ptr-record (node buf)
  "Emit a Record-only boxed-payload pointer read for NODE's `:ptr' Sexp pointer.
Doc 49 R6g (Gate-segv fix): identical to `--emit-sexp-payload-ptr' but
tag-guards on `Sexp::Record' (tag=12) instead of `Sexp::Cons' (tag=7).
Non-Record inputs return 0 (NULL) - caller must tag-check before
deref.  Used by `nl_record_type_tag_ptr' to read NlRecord*."
  (let* ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
         (id (nelisp-phase47-compiler--gensym "sexp-payload-ptr-record"))
         (zero-lbl (intern (format "%s-zero" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-record)
    (nelisp-asm-x86_64-jnz-rel32 buf zero-lbl)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf zero-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

;; ---- Doc 111 §111.B Record read+write ops emit ----

(defun nelisp-phase47-compiler--emit-record-slot-ptr-core (ptr idx buf)
  "Leave the raw `*const Sexp' for record slot IDX in rax.

Doc 111 §111.E #1 fix (two bugs uncovered by the first user of
`record-slot-ref-ptr', = `mirror_lookup_entry'):

  1. Per-Sexp slot stride was `nelisp-sexp--size' (= undefined
     symbol); the canonical constant in `nelisp-sexp-layout.el' is
     `--slot-size'.
  2. The slots `Vec<Sexp>' data-pointer read used
     `nelisp-nlrecord--offset-slots-vec' (= 32), which on the
     pinned repo Rust toolchain is the Vec's `capacity' field, not
     the data pointer.  The actual layout is `(capacity, ptr,
     length)' (= same convention as `vector-ref-ptr-core' over
     `NlVector'); the data pointer lives at offset `+40' inside
     NlRecord (= `nelisp-nlrecord--offset-slots-capacity' — name
     left from a pre-merge `(ptr, cap, len)' assumption that no
     longer holds).

Both bugs stayed dormant because the §111.B `recordp' probe only
reads the tag byte and the §111.C `aref-vector' probe goes through
the NlVector path which has its own correctly-named offset
constants.  The first composer of `record-slot-ref-ptr'
(= `mirror_lookup_entry') is the first caller to exercise this
code, so the test there is the regression gate."
  (nelisp-phase47-compiler--emit-value ptr buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value idx buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-asm-x86_64-pop buf 'rax)
  (nelisp-phase47-compiler--imul-rax-imm32 buf nelisp-sexp--slot-size)
  (nelisp-asm-x86_64-pop buf 'rdi)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'r10 'rdi nelisp-sexp--offset-payload)
  ;; Read the Vec<Sexp>'s data pointer, which on the pinned toolchain
  ;; lives at byte +40 inside NlRecord (= the `--offset-slots-capacity'
  ;; constant; the name dates from a (ptr, cap, len) assumption that
  ;; no longer matches actual layout).  See the comment in
  ;; `--emit-vector-slot-ptr-core' for the matching NlVector reasoning.
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'r10 'r10 nelisp-nlrecord--offset-slots-capacity)
  (nelisp-asm-x86_64-add-reg-reg buf 'rax 'r10))

(defun nelisp-phase47-compiler--emit-record-type-tag (node buf)
  "Copy a record's inline `type_tag' Sexp into the caller-owned slot."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r10 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8
     buf 'xmm0 'r10 nelisp-nlrecord--offset-type-tag)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 0 'xmm0)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8
     buf 'xmm0 'r10 (+ nelisp-nlrecord--offset-type-tag 16))
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 16 'xmm0)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-record-slot-count (node buf)
  "Read `record.slots.len' into rax."
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :ptr) buf)
  (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rdi 'rdi nelisp-sexp--offset-payload)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rax 'rdi nelisp-nlrecord--offset-slots-length))

(defun nelisp-phase47-compiler--emit-record-slot-ref-ptr (node buf)
  "Leave the raw `*const Sexp' for NODE's record slot in rax."
  (nelisp-phase47-compiler--emit-record-slot-ptr-core
   (nelisp-phase47-compiler--ir-get node :ptr) (nelisp-phase47-compiler--ir-get node :idx) buf))

(defun nelisp-phase47-compiler--emit-record-slot-ref (node buf)
  "Copy a record slot into the caller-owned destination slot.

Doc 111 §111.C v3 fix: the previous inline SIMD 32-byte slot copy
was NOT refcount-aware — copying a box-tagged Sexp (Cons / Symbol /
Str / Record / Vector / Cell / BoolVector / CharTable) via raw
bytes left both the source slot and the destination slot pointing
at the same `NlXxx' heap object with refcount=1, so subsequent
`Drop' on either side caused a double-free.

The slot copy now delegates to the `nl_sexp_clone_into' Rust helper
which performs a refcount-aware `Sexp::clone' before writing into
the destination.  The destination is treated as uninitialized
(`core::ptr::write' without Drop), matching the Phase 47 invariant
that result slots start as `Sexp::Nil' bit-pattern."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    ;; Compute the source `*const Sexp' pointer for record slot N.
    (nelisp-phase47-compiler--emit-record-slot-ptr-core ptr idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Compute the destination `*mut Sexp' pointer (= result_slot).
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; SysV AMD64 arg order: rdi = src, rsi = dst.  Pop in reverse
    ;; push order: last pushed (= dst) -> rsi, first pushed (= src)
    ;; pointer is still on the stack — pop into rdi.
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Stack alignment + dst preservation:
    ;; Body entry rsp ≡ 8 (mod 16) (= post-prologue, post-param-pushes
    ;; for a 3-arg GP defun).  After the two pops above we are back
    ;; at body entry alignment.  A single `push rsi' brings rsp to
    ;; ≡ 0 (mod 16) which is what `call' requires, AND it doubles as
    ;; the dst-preservation save (rsi is caller-saved and may be
    ;; clobbered inside `nl_sexp_clone_into').
    (nelisp-asm-x86_64-push buf 'rsi)
    ;; call nl_sexp_clone_into
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_sexp_clone_into" -4 'text)
    ;; Restore dst into rax (= the conventional return register for
    ;; record-slot-ref ops; callers treat rax as the destination
    ;; pointer they just wrote into).
    (nelisp-asm-x86_64-pop buf 'rax)))

(defun nelisp-phase47-compiler--emit-record-slot-set (node buf)
  "Call the Rust helper that refcount-safely overwrites a record slot.

Dynamic rsp alignment around the call: `nl_record_set_slot' uses
`Vec::index_mut' which dispatches through SSE on some Rust targets
and requires `rsp ≡ 0 mod 16' at the `call' site per SysV AMD64.
Phase 47 callers can have either body-entry alignment (= depends
on outer-defun param count: even → 0 mod 16, odd → 8 mod 16), so
this op aligns rsp via `mov rbx, rsp; and rsp, -16; ...; mov rsp,
rbx' regardless of inbound state.

`rbx' is callee-saved in SysV AMD64, so its value across our `call'
is preserved by `nl_record_set_slot'.  Pre-existing emits that
depend on `rbx' (= currently none in Phase 47) would need updating;
the constraint is documented here as the local convention for this
op.

After the helper call, materialise `rax = 1' so this op composes
cleanly inside `(and SIDE-EFFECT VALUE)' value-form chains (= the
Phase 47 idiom for sequencing side effects when no `progn'-shape
value form exists).  `nl_record_set_slot' returns Rust unit, which
leaves rax in an unspecified state across the call boundary; the
explicit `mov rax, 1' makes the op produce a stable truthy value.

This is a Doc 111 §111.E #7-#12 enabler: those helpers chain
`record-slot-set' with `(and ... 1)' to express \"overwrite slot
then return 1 on hit\".  No callers depended on the prior
undefined-rax behaviour."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx))
        (val-ptr (nelisp-phase47-compiler--ir-get node :val-ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rdi 'rdi nelisp-sexp--offset-payload)
    ;; Dynamic rsp alignment around the call.  Save old rsp on the
    ;; aligned stack (via caller-saved r10, plus a stack mirror so we
    ;; survive the callee's r10 clobber).  Sequence:
    ;;   mov r10, rsp          ; 49 89 e2 — r10 = old rsp
    ;;   and rsp, -16          ; 48 83 e4 f0 — rsp aligned to 16
    ;;   push r10              ; 41 52 — save old rsp on stack (rsp -= 8, now 8 off)
    ;;   sub rsp, 8            ; 48 83 ec 08 — pad to 0 mod 16 for call
    ;;   call nl_record_set_slot
    ;;   add rsp, 8            ; 48 83 c4 08 — remove pad
    ;;   pop r10               ; 41 5a — reload old rsp (callee may have clobbered r10)
    ;;   mov rsp, r10          ; 4c 89 d4 — restore rsp
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x49 #x89 #xE2))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x48 #x83 #xE4 #xF0))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x41 #x52))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x48 #x83 #xEC #x08))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_record_set_slot" -4 'text)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x48 #x83 #xC4 #x08))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x41 #x5A))
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #x4C #x89 #xD4))
    ;; rax = 1 (truthy sentinel for value-form chaining).
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)))

;; ---- Doc 111 §111.C Vector read ops emit ----

(defun nelisp-phase47-compiler--emit-vector-slot-ptr-core (ptr idx buf)
  "Leave the raw `*const Sexp' for vector element IDX in rax."
  (nelisp-phase47-compiler--emit-value ptr buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value idx buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-asm-x86_64-pop buf 'rax)
  (nelisp-phase47-compiler--imul-rax-imm32 buf nelisp-sexp--slot-size)
  (nelisp-asm-x86_64-pop buf 'rdi)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rsi 'rdi nelisp-sexp--offset-payload)
  ;; Pinned repo toolchain lays out `Vec<Sexp>' as (capacity, ptr, len).
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rcx 'rsi nelisp-nlvector--offset-value-capacity)
  (nelisp-asm-x86_64-add-reg-reg buf 'rax 'rcx))

(defun nelisp-phase47-compiler--emit-vector-len (node buf)
  "Read `vector.value.len' into rax."
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :ptr) buf)
  (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rsi 'rdi nelisp-sexp--offset-payload)
  (nelisp-asm-x86_64-mov-reg-mem-disp8
   buf 'rax 'rsi nelisp-nlvector--offset-value-length))

(defun nelisp-phase47-compiler--emit-vector-ref-ptr (node buf)
  "Leave the raw `*const Sexp' for NODE's vector element in rax."
  (nelisp-phase47-compiler--emit-vector-slot-ptr-core
   (nelisp-phase47-compiler--ir-get node :ptr) (nelisp-phase47-compiler--ir-get node :idx) buf))

(defun nelisp-phase47-compiler--emit-vector-slot-set (node buf)
  "Call the Rust helper that refcount-safely overwrites a vector slot.

Doc 111 §111.E — parallel to `--emit-record-slot-set' (§111.B); the
Sexp payload at offset 8 is an `NlVector*' (vs. `NlRecord*' for
record-slot-set) and the called helper is `nl_vector_set_slot'.  The
helper signature is identical: `(vec, n, val_ptr)' in `rdi / rsi /
rdx', refcount-aware drop of the old element via `Vec::index_mut'
assignment, refcount-aware clone of `*val_ptr' into the slot.

Doc 115 §115.1 — materialise `rax = 1' after the helper call so this
op composes cleanly in `(and SIDE-EFFECT VALUE)' value-form chains,
matching the convention `--emit-record-slot-set' established for
§111.B's record overwrite (= `Vec::index_mut' leaves rax in an
unspecified state across the call boundary; explicit `mov rax, 1'
yields a stable truthy value)."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx))
        (val-ptr (nelisp-phase47-compiler--ir-get node :val-ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rdi 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_vector_set_slot" -4 'text)
    ;; rax = 1 (truthy sentinel for value-form chaining).
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)))

(defun nelisp-phase47-compiler--emit-vector-ref (node buf)
  "Copy a vector element into the caller-owned destination slot.

Doc 111 §111.C v3 fix: the previous inline SIMD 32-byte slot copy
was NOT refcount-aware (see `--emit-record-slot-ref' for the full
rationale).  Test `aref_vector_heap_backed_element_is_stable'
reproduced a double-free on `[Sexp::Str(...)]' inputs because the
raw byte copy left both the vector's interior storage and the
caller's result slot holding the same `NlStr' pointer at refcount
1, then both invoked `Drop' at scope-exit.

The slot copy now delegates to the `nl_sexp_clone_into' Rust helper
which performs a refcount-aware `Sexp::clone' (= one `fetch_add'
on box-tagged variants) before writing into the destination."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    ;; Compute source `*const Sexp' (vec_ptr + offset + idx*32) -> rax.
    (nelisp-phase47-compiler--emit-vector-slot-ptr-core ptr idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Compute destination `*mut Sexp' -> rax.
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; SysV AMD64: rdi = src, rsi = dst.  Pop in reverse push order.
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Stack alignment + dst preservation (see `--emit-record-slot-
    ;; ref' comment for the full rsp accounting): one `push rsi'
    ;; brings rsp to ≡ 0 (mod 16) for the call AND saves dst since
    ;; rsi is caller-saved across `nl_sexp_clone_into'.
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_sexp_clone_into" -4 'text)
    ;; Restore dst into rax — convention for vector-ref / record-
    ;; slot-ref: rax = the destination pointer the op just wrote into.
    (nelisp-asm-x86_64-pop buf 'rax)))

;; ---- Doc 111 §111.D Cell read+write ops emit ----
;;
;; All four ops operate on a `*const Sexp' that points at a
;; `Sexp::Cell(_)' slot.  The boxed `NlCell*' lives at
;; `[ptr + nelisp-sexp--offset-payload]' (= same payload-offset
;; convention as `Sexp::Cons').  `value' is the inline Sexp at offset
;; 0 of the NlCell (= 32-byte slot, asserted by §111.A layout consts).
;;
;; MVP refcount note (cell-value): we currently inline-copy the 32-byte
;; `value' Sexp into the caller-owned SLOT without bumping refcounts on
;; nested boxed payloads.  This is the SAME safety contract Doc 101
;; §101.B `cons-car' / `cons-cdr' use today (= caller-owned copy with
;; the implicit assumption that the slot won't outlive the cell).
;; Agent A is shipping a refcount-aware `nl_sexp_clone_into' helper for
;; vector-ref; once that lands on main this op should switch to call
;; that helper too — see TODO below.  For now `cell-value' is safe iff
;; the consumer holds the cell pointer for the entire lifetime of the
;; copied SLOT, which is the §111.E env_lexframe usage pattern.

(defun nelisp-phase47-compiler--emit-cell-value (node buf)
  "Emit `cell-value' — copy NlCell.value into NODE's caller-owned SLOT.
NODE carries `:ptr' (= `*const Sexp' pointing at a `Sexp::Cell(_)') and
`:slot' (= `*mut Sexp').  Strategy:

  1. Evaluate PTR / SLOT and stash both on the stack.
  2. Load the `NlCell*' from `[ptr + 8]' into r10.
  3. Copy 32 bytes from `[r10 + 0, r10 + 32)' into `[rsi + 0)' via
     two 16-byte `movdqu' pairs.
  4. Return SLOT in rax.

TODO (refcount-aware §111.D.2): when Agent A's `nl_sexp_clone_into'
extern lands on main, replace the inline `movdqu' pair with a
`call nl_sexp_clone_into(dst=SLOT, src=NlCell.value_ptr)' to make
this op symmetric with `cell-set-value' (= no double-free on
boxed-tagged values).  Tracking the safety constraint here so the
diff is obvious at swap time."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; r10 = NlCell* (= payload pointer at offset 8 of the Sexp slot).
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r10 'rdi nelisp-sexp--offset-payload)
    ;; value lives at NlCell offset 0 (= nelisp-nlcell--offset-value).
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8
     buf 'xmm0 'r10 nelisp-nlcell--offset-value)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 0 'xmm0)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8
     buf 'xmm0 'r10 (+ nelisp-nlcell--offset-value 16))
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'rsi 16 'xmm0)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cell-set-value (node buf)
  "Emit `cell-set-value' — delegate to `nl_cell_set_value' extern.
NODE carries `:ptr' (= `*const Sexp' pointing at a `Sexp::Cell(_)') and
`:val-ptr' (= `*const Sexp').  The Rust helper does refcount-aware
drop-then-write on `NlCell.value' (= matches `cons-set-car' /
`cons-set-cdr' delegation pattern from Doc 101 §101.D).  Returns the
original H pointer in rax."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (val-ptr (nelisp-phase47-compiler--ir-get node :val-ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Preserve H across the helper call; caller-saved regs are not
    ;; stable, so keep it on the stack and pop it back into rax after.
    ;; Two extra pushes (= H + a scratch pad) keep rsp at a 16-byte
    ;; boundary at the call site (SysV AMD64 alignment).
    (nelisp-asm-x86_64-push buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rsi)
    ;; rdi = NlCell* (= payload pointer at offset 8 of the Sexp slot).
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rdi 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_cell_set_value" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rax)))

(defun nelisp-phase47-compiler--emit-vector-make (node buf)
  "Emit `vector-make' — allocate fresh NlVector and write Sexp::Vector into SLOT.
NODE carries `:cap' (= i64 capacity) and `:slot' (= `*mut Sexp').

Strategy (= literal mirror of `cell-make' alignment idiom, just with
an i64 arg instead of a `*const Sexp' arg):

  1. Evaluate CAP, push.
  2. Evaluate SLOT, push.
  3. Push one extra alignment pad (= same trick `cell-make' uses).
  4. Call `nl_alloc_vector(cap)' — only one arg, so we discard the
     pad with `pop r11' first.  Stack at CALL site matches the
     `cell-make' shape.
  5. Recover slot from stack, write `Sexp::Vector(box)' (tag at offset
     0, ptr at offset 8) and return slot in rax.

The exact push/pop balancing follows `cell-make' so SysV AMD64
alignment holds.  The new vector is pre-filled with `Sexp::Nil'
elements by `nl_alloc_vector' (= refcount-1 box, ready for
`vector-slot-set'-based copy-fill)."
  (let ((cap (nelisp-phase47-compiler--ir-get node :cap))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value cap buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Mirror cell-make's "one extra scratch slot" alignment pad.
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'r11)        ; r11 = pad (discard)
    (nelisp-asm-x86_64-pop buf 'rsi)        ; rsi = slot (will save to stack)
    (nelisp-asm-x86_64-pop buf 'rdi)        ; rdi = cap (= arg 0)
    ;; Save slot across the helper call.  Two extra pushes (= slot +
    ;; one pad) keep the call site at a 16-byte boundary, matching
    ;; cell-make's idiom.
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_vector" -4 'text)
    ;; rax = NlVector*.  Move to r10.
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    ;; Discard alignment pad, recover slot.
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rsi)        ; rsi = slot
    ;; slot = Sexp::Vector(box).  Tag byte at offset 0, payload ptr at
    ;; offset 8 (= `Sexp::Vector' tag = 8, same shape `cell-make' uses
    ;; for `Sexp::Cell').
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rsi nelisp-sexp--tag-vector)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rsi nelisp-sexp--offset-payload 'r10)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-record-make (node buf)
  "Emit `record-make' — allocate fresh NlRecord and write Sexp::Record into SLOT.
NODE carries `:tag-ptr' (= `*const Sexp', the type-tag symbol),
`:slot-count' (= i64 number of slots) and `:slot' (= `*mut Sexp').

Strategy (= literal mirror of `vector-make' alignment idiom, extended
for a 2-arg helper call):

  1. Evaluate TAG-PTR, push.
  2. Evaluate SLOT-COUNT, push.
  3. Evaluate SLOT, push.
  4. Pop slot into rax (save for later), pop slot-count into rsi (= arg
     1), pop tag-ptr into rdi (= arg 0).
  5. Push slot back + one alignment pad to keep rsp 16-byte aligned at
     the call site (= same 2-push pattern `vector-make' uses).
  6. Call `nl_alloc_record(tag_ptr, slot_count)' — rax = `*mut NlRecord'.
  7. Discard the alignment pad, recover slot, write `Sexp::Record(box)'
     (tag at offset 0, ptr at offset 8) and return slot in rax.

The push/pop balancing follows `vector-make' / `cons-make' so SysV
AMD64 alignment holds.  The new record has SLOT-COUNT slots all pre-
filled with `Sexp::Nil' by `nl_alloc_record', and refcount = 1 ready
for `record-slot-set'-based init."
  (let ((tag-ptr (nelisp-phase47-compiler--ir-get node :tag-ptr))
        (slot-count (nelisp-phase47-compiler--ir-get node :slot-count))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value tag-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot-count buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Pop in reverse push order: slot -> rax (will save), slot-count ->
    ;; rsi (= arg 1), tag-ptr -> rdi (= arg 0).
    (nelisp-asm-x86_64-pop buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Save slot across the helper call.  Two pushes (= slot + one pad)
    ;; keep the call site at a 16-byte boundary, matching the idiom in
    ;; `vector-make' / `cons-make'.
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_record" -4 'text)
    ;; rax = *mut NlRecord.  Move to r10.
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    ;; Discard alignment pad, recover slot.
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rsi)        ; rsi = slot
    ;; slot = Sexp::Record(box).  Tag byte at offset 0 (= 12), payload
    ;; ptr at offset 8 (= `nelisp-sexp--offset-payload', same shape
    ;; `vector-make' uses for `Sexp::Vector').
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rsi nelisp-sexp--tag-record)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rsi nelisp-sexp--offset-payload 'r10)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cell-make (node buf)
  "Emit `cell-make' — allocate fresh NlCell and write Sexp::Cell into SLOT.
NODE carries `:val-ptr' (= `*const Sexp', the initial value) and
`:slot' (= `*mut Sexp').

Strategy (= literal mirror of `cons-make' alignment idiom):

  1. Evaluate VAL-PTR, push.
  2. Evaluate SLOT, push.
  3. Push one extra alignment pad (= same trick `cons-make' uses for
     its 3-args + 1-pad = 4 effective pushes).
  4. Call `nl_alloc_cell(val_ptr)' — only one arg, so we discard the
     pad with `pop r11' first.  Stack at CALL site matches the
     `cons-make' shape modulo arity.
  5. Recover slot from stack, write `Sexp::Cell(box)' (tag at offset
     0, ptr at offset 8) and return slot in rax.

The exact push/pop balancing follows `cons-make' / `cons-set-slot' so
SysV AMD64 alignment holds whatever the wrapper arity ends up being.
See `cons-make' comment for the alignment rationale."
  (let ((val-ptr (nelisp-phase47-compiler--ir-get node :val-ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value val-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Mirror cons-make's "one extra scratch slot" alignment pad.  The
    ;; `nl_alloc_cell' helper only takes 1 arg, so we don't need
    ;; multiple live values on the stack across the call — but keeping
    ;; the 1-extra-pad pattern means the call site aligns the same way
    ;; cons-make's does (caller-side rsp%16 invariant).
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Pop the alignment pad + slot off the stack into scratch r11 /
    ;; rsi (slot survives in rsi for the post-call write since rsi is
    ;; clobberable but we save/restore it via the stack below).
    (nelisp-asm-x86_64-pop buf 'r11)        ; r11 = pad (discard)
    (nelisp-asm-x86_64-pop buf 'rsi)        ; rsi = slot (will save to stack)
    (nelisp-asm-x86_64-pop buf 'rdi)        ; rdi = val-ptr (= arg 0)
    ;; Save slot across the helper call.  Two extra pushes (= slot +
    ;; one pad) keep the call site at a 16-byte boundary, matching
    ;; cons-make / cons-set-slot's idiom.
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_cell" -4 'text)
    ;; rax = NlCell*.  Move to r10.
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    ;; Discard alignment pad, recover slot.
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rsi)        ; rsi = slot
    ;; slot = Sexp::Cell(box).  Tag byte at offset 0, payload ptr at
    ;; offset 8 (= `Sexp::Cell' tag = 11, same shape `cons-make' uses
    ;; for `Sexp::Cons').
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rsi nelisp-sexp--tag-cell)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rsi nelisp-sexp--offset-payload 'r10)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cell-null-p (node buf)
  "Emit `cell-null-p' — read NlCell.value's tag byte, compare to Nil.
NODE carries `:ptr' (= `*const Sexp' pointing at a `Sexp::Cell(_)').
Returns 1 in rax iff `NlCell.value.tag == SEXP_TAG_NIL'; else 0.

Strategy (= inline tag check, no extern call):

  1. Evaluate PTR into rax, copy to rdi.
  2. rdi = NlCell* via `mov rdi, [rdi + 8]'.
  3. Load tag byte at `[rdi + 0]' (= NlCell.value tag at offset 0).
  4. Compare to `SEXP_TAG_NIL', setCC AL, movzx to materialise."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    ;; rdi = NlCell* via payload load.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rdi 'rdi nelisp-sexp--offset-payload)
    ;; Read NlCell.value's tag byte (= offset 0 inside the value Sexp,
    ;; which is itself at offset 0 inside NlCell).
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-nil)
    (nelisp-asm-x86_64-setcc-al buf 'sete)
    (nelisp-asm-x86_64-movzx-eax-al buf)))

;; ---- Doc 101 §101.C Symbol/Str read ops emit ----

(defun nelisp-phase47-compiler--emit-str-len (node buf)
  "Emit `mov rax, qword ptr [rdi + 24]' after computing NODE's :ptr."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-length)))

(defun nelisp-phase47-compiler--emit-str-bytes (node buf)
  "Emit `mov rax, qword ptr [rdi + 8]' after computing NODE's :ptr."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-ptr)))

(defun nelisp-phase47-compiler--emit-str-bytes-ptr (node buf)
  "Emit Doc 122 §122.H `str-bytes-ptr' — 1-arg call to `nl_str_bytes_ptr'.

Layout-safe sibling of `str-bytes': dispatches through the Rust extern
`nl_str_bytes_ptr' which covers all three string-y variants
(`Sexp::Str' / `Sexp::Symbol' / `Sexp::MutStr') via the official
`String::as_ptr()' / `NlStr.value.as_ptr()' accessors.  Non-string
inputs return null pointer.  Pair with `str-len' for the matching
byte count.

Strategy (= 1-arg extern call, mirrors §122.D `mut-str-len' /
`str-char-count' pattern):

  1. Evaluate `:ptr' -> rax, copy to rdi (= arg 0).
  2. Push one alignment pad to keep rsp 16-byte aligned at call site.
  3. Call `nl_str_bytes_ptr' — rax = `*const u8' data pointer.
  4. Pop the alignment pad."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_str_bytes_ptr" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-str-byte-at (node buf)
  "Emit byte load from a `Sexp::Str' / `Sexp::Symbol' String buffer.
Result: the selected UTF-8 byte zero-extended into rax."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'r10)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-ptr)
    (nelisp-asm-x86_64-add-reg-reg buf 'rax 'r10)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rax)))

(defun nelisp-phase47-compiler--emit-string-eq-core (buf left right id)
  "Emit length-first byte-loop equality for two String-header slots.
LEFT and RIGHT are GP registers holding `*const Sexp' addresses of
values whose payload layout matches Rust `String' (=`Sexp::Str' or
`Sexp::Symbol').  Result: i64 0/1 in rax."
  (let ((false-lbl (intern (format "%s-false" id)))
        (true-lbl (intern (format "%s-true" id)))
        (loop-lbl (intern (format "%s-loop" id)))
        (end-lbl (intern (format "%s-end" id))))
    ;; r10=len(left), r11=len(right)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r10 left nelisp-string--offset-length)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r11 right nelisp-string--offset-length)
    (nelisp-asm-x86_64-cmp-reg-reg buf 'r10 'r11)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    ;; Empty strings are equal.
    (nelisp-asm-x86_64-cmp-imm32 buf 'r10 0)
    (nelisp-asm-x86_64-jz-rel32 buf true-lbl)
    ;; r8=ptr(left), r9=ptr(right), rcx=remaining byte count.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r8 left nelisp-string--offset-ptr)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r9 right nelisp-string--offset-ptr)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rcx 'r10)
    (nelisp-asm-x86_64-define-label buf loop-lbl)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'r8)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rdx 'r9)
    (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'rdx)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    (nelisp-asm-x86_64-add-imm32 buf 'r8 1)
    (nelisp-asm-x86_64-add-imm32 buf 'r9 1)
    (nelisp-asm-x86_64-sub-imm32 buf 'rcx 1)
    (nelisp-asm-x86_64-jnz-rel32 buf loop-lbl)
    (nelisp-asm-x86_64-define-label buf true-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf false-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-str-eq (node buf)
  "Emit `str-eq' using a byte loop over the two String payloads."
  (let ((a (nelisp-phase47-compiler--ir-get node :a))
        (b (nelisp-phase47-compiler--ir-get node :b))
        (id (nelisp-phase47-compiler--ir-get node :id)))
    (nelisp-phase47-compiler--emit-value a buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value b buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; last pushed (= b) → rsi, first pushed (= a) → rdi
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-phase47-compiler--emit-string-eq-core buf 'rdi 'rsi id)))

(defun nelisp-phase47-compiler--emit-symbol-name-eq (node buf)
  "Emit `symbol-name-eq': tag-check SYM_PTR == Sexp::Symbol, length
check against LITERAL_LEN, then byte-loop compare against compile-time
literal bytes.  Returns i64 0/1 in rax.

Register usage:
  rdi       = `*const Sexp' SYM_PTR (caller-emitted into rax then moved)
  rax/rdx   = scratch (tag / length / per-byte read)
  r9        = current payload byte pointer (NlString::ptr + i)

No PLT calls — entirely inline; the literal bytes are encoded as
`cmp imm32' immediates so each byte costs ~10 bytes of code.  A 4-byte
literal expands to ~50 bytes of code, a 20-byte literal to ~210 bytes."
  (let* ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
         (bytes (nelisp-phase47-compiler--ir-get node :bytes))
         (id (nelisp-phase47-compiler--ir-get node :id))
         (len (length bytes))
         (false-lbl (intern (format "%s-false" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    ;; Tag check: byte [rdi] == Sexp::Symbol tag (= 4).
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-symbol)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    ;; Length check: NlString::len at offset 24 == LITERAL_LEN.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-length)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax len)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    ;; r9 = NlString::ptr at offset 16; this is the start of the
    ;; payload byte buffer that we'll walk byte-by-byte below.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r9 'rdi nelisp-string--offset-ptr)
    ;; Per-byte: movzx rax, byte [r9]; cmp rax, byte_i; jnz false-lbl;
    ;; add r9, 1.  The final byte's `add r9, 1' is harmless dead code
    ;; but kept for symmetry / simpler emitter.
    (dolist (b bytes)
      (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'r9)
      (nelisp-asm-x86_64-cmp-imm32 buf 'rax b)
      (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
      (nelisp-asm-x86_64-add-imm32 buf 'r9 1))
    ;; All bytes match -> rax = 1.
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf false-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-sexp-name-eq (node buf)
  "Emit `sexp-name-eq': tag-check SEXP_PTR == Symbol (4) OR Str (5), length
check against LITERAL_LEN, then byte-loop compare against compile-time
literal bytes.  Returns i64 0/1 in rax.

Extends `symbol-name-eq' to accept both Sexp::Symbol (tag 4) and
Sexp::Str (tag 5) inputs.  The tag check emits:
  movzx rax, byte [rdi]
  cmp rax, 4       ; Symbol?
  jz <bytes-check>
  cmp rax, 5       ; Str?
  jnz <false-lbl>
  <bytes-check>: ...

Register usage matches `symbol-name-eq' exactly; only the tag-check block
differs (adds one cmp+jnz for the Str arm)."
  (let* ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
         (bytes (nelisp-phase47-compiler--ir-get node :bytes))
         (id (nelisp-phase47-compiler--ir-get node :id))
         (len (length bytes))
         (bytes-check-lbl (intern (format "%s-bytes" id)))
         (false-lbl (intern (format "%s-false" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    ;; Tag check: byte [rdi] == 4 (Symbol) → bytes_check
    ;;            byte [rdi] == 5 (Str)    → bytes_check
    ;;            anything else             → false_lbl
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-symbol)
    (nelisp-asm-x86_64-jz-rel32 buf bytes-check-lbl)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-str)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    (nelisp-asm-x86_64-define-label buf bytes-check-lbl)
    ;; Length check: NlString::len at offset 24 == LITERAL_LEN.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-length)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax len)
    (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
    ;; r9 = NlString::ptr at offset 16.
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'r9 'rdi nelisp-string--offset-ptr)
    ;; Per-byte comparison.
    (dolist (b bytes)
      (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'r9)
      (nelisp-asm-x86_64-cmp-imm32 buf 'rax b)
      (nelisp-asm-x86_64-jnz-rel32 buf false-lbl)
      (nelisp-asm-x86_64-add-imm32 buf 'r9 1))
    ;; All bytes match -> rax = 1.
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf false-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-symbol-eq (node buf)
  "Emit `symbol-eq': tag-check both inputs, then compare name bytes."
  (let ((a (nelisp-phase47-compiler--ir-get node :a))
        (b (nelisp-phase47-compiler--ir-get node :b))
        (id (nelisp-phase47-compiler--ir-get node :id))
        (tag-false-lbl (intern (format "%s-tag-false" (nelisp-phase47-compiler--ir-get node :id))))
        (end-lbl (intern (format "%s-tag-end" (nelisp-phase47-compiler--ir-get node :id)))))
    (nelisp-phase47-compiler--emit-value a buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value b buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-symbol)
    (nelisp-asm-x86_64-jnz-rel32 buf tag-false-lbl)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rsi)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax nelisp-sexp--tag-symbol)
    (nelisp-asm-x86_64-jnz-rel32 buf tag-false-lbl)
    (nelisp-phase47-compiler--emit-string-eq-core buf 'rdi 'rsi id)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf tag-false-lbl)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-sexp-write-tag (node buf tag)
  "Emit `mov byte ptr [rdi], TAG; mov rax, rdi' for NODE's :slot."
  (let ((slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rdi tag)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rdi)))

(defun nelisp-phase47-compiler--emit-sexp-write-alloc (node buf helper-name)
  "Emit `sexp-write-str' / `sexp-write-symbol' — call alloc HELPER-NAME.
NODE carries `:slot' (= `*mut Sexp'), `:bytes-ptr' (= `*const u8'),
and `:len' (= i64).  HELPER-NAME is `\"nl_alloc_str\"' or
`\"nl_alloc_symbol\"' (see Doc 122 §122.A).

Strategy (= 3-arg extern call, mirrors `record-make' alignment idiom):

  1. Evaluate BYTES-PTR, push.
  2. Evaluate LEN, push.
  3. Evaluate SLOT, push.
  4. Pop SLOT -> rdx (= arg 2), LEN -> rsi (= arg 1),
     BYTES-PTR -> rdi (= arg 0).
  5. Push one alignment pad to keep rsp 16-byte aligned at the call
     site (= 3 saved + call addr = 32 bytes; an extra pad makes it
     40 = 16*2 + 8 which then `call' pushes 8 more for rip → 48 = 16*3).
  6. Call `nl_alloc_str' / `nl_alloc_symbol' — the helper writes a
     full `Sexp::Str' / `Sexp::Symbol' (40 bytes incl. inline String
     header at payload offset 8..32) into `*SLOT' and returns SLOT
     in rax.
  7. Pop the alignment pad.

The helper's return value (= SLOT pointer) is left in rax, which is
exactly the contract for value-returning Phase 47 ops.  No
post-call tag/payload writes are needed because the extern
populates the full slot inline (= the key divergence from
`vector-make' / `cell-make' / `record-make' which return a `*mut
NlXXX' and require the emit code to write tag + payload offset
separately)."
  (let ((slot (nelisp-phase47-compiler--ir-get node :slot))
        (bytes-ptr (nelisp-phase47-compiler--ir-get node :bytes-ptr))
        (len (nelisp-phase47-compiler--ir-get node :len)))
    (nelisp-phase47-compiler--emit-value bytes-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value len buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Pop in reverse push order: slot -> rdx (= arg 2), len -> rsi
    ;; (= arg 1), bytes-ptr -> rdi (= arg 0).
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; One alignment pad to keep rsp 16-byte aligned at the call
    ;; site.  Pre-prologue rsp is misaligned by 8 (= function entry
    ;; has 8 mod 16 due to the return address pushed by the caller);
    ;; our 3 push + 3 pop sequence is balanced, so we need exactly
    ;; one extra push to bring rsp to (8 + 8) mod 16 = 0 before the
    ;; new `call' instruction.
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf helper-name -4 'text)
    ;; Helper returned the slot pointer in rax.  Discard the
    ;; alignment pad; rax is already the desired return value.
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--bytes->u64-chunks (bytes)
  "Pack BYTES into little-endian u64 chunks for stack materialization."
  (let ((rest bytes)
        (chunks nil))
    (while rest
      (let ((value 0)
            (shift 0)
            (i 0))
        (while (and rest (< i 8))
          (setq value (logior value (ash (car rest) shift))
                shift (+ shift 8)
                rest (cdr rest)
                i (1+ i)))
        (push value chunks)))
    (nreverse chunks)))

(defun nelisp-phase47-compiler--emit-sexp-write-lit (node buf helper-name)
  "Emit a literal string/symbol allocation through HELPER-NAME.
This uses a temporary stack byte buffer and avoids `.rodata' so the
form remains valid in ET_REL object mode."
  (let* ((slot (nelisp-phase47-compiler--ir-get node :slot))
         (bytes (nelisp-phase47-compiler--ir-get node :bytes))
         (chunks (nelisp-phase47-compiler--bytes->u64-chunks bytes))
         (chunk-count (length chunks))
         (pad-p (= 1 (logand chunk-count 1)))
         (stack-slots (+ chunk-count (if pad-p 1 0))))
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    (when pad-p
      (nelisp-asm-x86_64-push buf 'rax))
    (dolist (chunk (reverse chunks))
      (nelisp-asm-x86_64-mov-imm64 buf 'rax chunk)
      (nelisp-asm-x86_64-push buf 'rax))
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rsp)
    (nelisp-asm-x86_64-mov-imm32 buf 'rsi (length bytes))
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'r10)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf helper-name -4 'text)
    (when (> stack-slots 0)
      (nelisp-asm-x86_64-add-imm32 buf 'rsp (* 8 stack-slots)))))

(defun nelisp-phase47-compiler--emit-sexp-write-symbol-lit (node buf)
  "Emit `sexp-write-symbol-lit' using a temporary stack byte buffer."
  (nelisp-phase47-compiler--emit-sexp-write-lit
   node buf "nl_alloc_symbol"))

(defun nelisp-phase47-compiler--emit-sexp-write-str-lit (node buf)
  "Emit `sexp-write-str-lit' using a temporary stack byte buffer."
  (nelisp-phase47-compiler--emit-sexp-write-lit
   node buf "nl_alloc_str"))

(defun nelisp-phase47-compiler--emit-sexp-write-float (node buf)
  "Emit `sexp-write-float' — call `nl_sexp_write_float(slot, val: f64)'.
NODE carries `:slot' (= `*mut Sexp', supplied as either gp-class or
f64-class bit-cast pointer) and `:value' (= f64-class flat-leaf
ref).  Per SysV AMD64: slot → rdi (= GP arg 0), value → xmm0
(= f64 arg 0).  Helper returns slot in rax.

Strategy (= mirrors `--emit-extern-call' f64 path with one gp + one
f64 arg, balanced pushes for stack alignment):

  1. Evaluate VALUE — emits an f64-leaf load into xmm0.
  2. Transfer xmm0 → rax (= MOVQ), push.  Stack carries the f64
     bit-pattern as 8 bytes.
  3. Evaluate SLOT — lands in rax (gp-class default) or xmm0
     (f64-class ref bit-pattern of pointer).  If f64-class,
     MOVQ rax, xmm0 first.  Push rax.
  4. Pop in reverse: SLOT → rdi (= arg 0), VALUE → rax then
     MOVQ → xmm0 (= f64 arg 0).
  5. Push one alignment pad.  Function entry has rsp mod 16 = 8;
     our balanced 2 push + 2 pop net to 0, so we need one extra
     push to bring rsp to (8 + 8) mod 16 = 0 before the `call'
     instruction.
  6. Call `nl_sexp_write_float' — rax = slot pointer on return.
  7. Pop the alignment pad.

SLOT f64-class is the workaround for the Phase 47 MVP requirement
that defun params be uniform-class.  Test harnesses bit-cast the
pointer via `f64::from_bits(ptr as u64)' and pass it as an f64
param alongside VALUE; the bit pattern survives unchanged through
the xmm0 spill / unspill round trip."
  (let ((slot (nelisp-phase47-compiler--ir-get node :slot))
        (value (nelisp-phase47-compiler--ir-get node :value)))
    ;; Step 1-2: VALUE → xmm0, then xmm0 → rax, push.  The
    ;; `emit-f64-leaf-into' helper enforces the flat-ref shape so
    ;; this op composes only at the same MVP level as `f64-call'.
    (nelisp-phase47-compiler--emit-f64-leaf-into value buf 'xmm0)
    (nelisp-asm-x86_64-movq-r64-xmm buf 'rax 'xmm0)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Step 3: SLOT.  If the IR node is an f64-class ref, the
    ;; emit-value path lands the bit pattern in xmm0; transfer to
    ;; rax via MOVQ.  Otherwise (= gp-class ref / imm / call), the
    ;; result is already in rax.
    (nelisp-phase47-compiler--emit-value slot buf)
    (when (and (eq (nelisp-phase47-compiler--ir-kind slot) 'ref)
               (eq (nelisp-phase47-compiler--ir-get slot :class) 'f64))
      (nelisp-asm-x86_64-movq-r64-xmm buf 'rax 'xmm0))
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Step 4: pop in reverse — slot (last pushed) → rdi, then
    ;; value (first pushed) → rax → xmm0.
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-pop buf 'rax)
    (nelisp-asm-x86_64-movq-xmm-r64 buf 'xmm0 'rax)
    ;; Step 5: alignment pad.
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Step 6: CALL rel32 = 0xE8 + 4-byte placeholder + PLT32 reloc.
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_sexp_write_float" -4 'text)
    ;; Step 7: discard the alignment pad.  rax = slot pointer is
    ;; the value-form result.
    (nelisp-asm-x86_64-pop buf 'r11)))


;; ---- Doc 122 §122.B — Mutable string builder grammar emit ----

(defun nelisp-phase47-compiler--emit-mut-str-make-empty (node buf)
  "Emit `mut-str-make-empty' — call `nl_alloc_mut_str(cap, slot)'.
NODE carries `:cap' (= i64 byte-capacity) and `:slot' (= `*mut Sexp').
The Rust extern allocates a fresh `NlStrRef::new(String::with_capacity(cap))'
and writes `Sexp::MutStr(rc)' (= tag at offset 0, box ptr at offset 8)
into `*slot'.  Returns SLOT in rax.

Strategy (= 2-arg extern call, mirrors `sexp-write-alloc' shape):

  1. Evaluate CAP, push.
  2. Evaluate SLOT, push.
  3. Pop SLOT -> rsi (= arg 1), CAP -> rdi (= arg 0).
  4. Push one alignment pad to keep rsp 16-byte aligned at call site.
  5. Call `nl_alloc_mut_str' — rax = slot pointer.
  6. Pop the alignment pad."
  (let ((cap (nelisp-phase47-compiler--ir-get node :cap))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value cap buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Alignment pad: 2 push + 2 pop balanced, function entry has
    ;; rsp mod 16 = 8; one extra push aligns the call site.
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_mut_str" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-mut-str-push-2arg (node buf helper-name arg-key)
  "Emit a 2-arg in-place push op — `nl_mut_str_push_byte' / `_push_codepoint'.
NODE carries `:ptr' (= `*mut Sexp' slot for MutStr) and ARG-KEY's
value (= i64 byte or codepoint).  HELPER-NAME selects the extern.
Returns rax = 1 sentinel (= matches `vector-slot-set' /
`record-slot-set' convention for value-form `and'-chain composition;
the extern itself is `void' so we materialise the truthy 1 explicitly
post-call to leave rax in a defined state).

Strategy (= 2-arg extern call, no return value):

  1. Evaluate PTR, push.
  2. Evaluate ARG, push.
  3. Pop ARG -> rsi (= arg 1), PTR -> rdi (= arg 0).
  4. Push one alignment pad.
  5. Call HELPER-NAME — clobbers rax (= `void' return).
  6. Pop pad, set rax = 1 sentinel."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (arg (nelisp-phase47-compiler--ir-get node arg-key)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value arg buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf helper-name -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    ;; rax = 1 sentinel (helper return is `void').
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)))

(defun nelisp-phase47-compiler--emit-mut-str-len (node buf)
  "Emit `mut-str-len' — call `nl_mut_str_len(ptr) -> i64' extern.
NODE carries `:ptr' (= `*const Sexp' slot for MutStr).  Returns the
i64 byte length in rax (= `String::len' on the MutStr's inner value).

Strategy (= 1-arg extern call with i64 return):

  1. Evaluate PTR -> rax, copy to rdi (= arg 0).
  2. Push one alignment pad to keep rsp 16-byte aligned at call site.
  3. Call `nl_mut_str_len' — rax = i64 length.
  4. Pop the alignment pad."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_mut_str_len" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-mut-str-finalize (node buf)
  "Emit `mut-str-finalize' — call `nl_mut_str_finalize(ptr, slot)' extern.
NODE carries `:ptr' (= `*const Sexp' source MutStr slot) and `:slot'
(= `*mut Sexp' destination Str slot).  The Rust extern clones the
MutStr's inner `String' and writes `Sexp::Str(s)' into `*slot'; the
source remains live + push-able.  Returns SLOT in rax.

Strategy (= 2-arg extern call, mirrors `mut-str-make-empty' shape):

  1. Evaluate PTR, push.
  2. Evaluate SLOT, push.
  3. Pop SLOT -> rsi (= arg 1), PTR -> rdi (= arg 0).
  4. Push one alignment pad.
  5. Call `nl_mut_str_finalize' — rax = slot pointer.
  6. Pop the alignment pad."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_mut_str_finalize" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))


;; ---- Doc 122 §122.D — UTF-8 helper grammar emit ----

(defun nelisp-phase47-compiler--emit-str-char-count (node buf)
  "Emit `str-char-count' — call `nl_str_char_count(str_ptr) -> i64'."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_str_char_count" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-str-codepoint-at (node buf)
  "Emit `str-codepoint-at' — 4-arg call to `nl_str_codepoint_at'."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx))
        (cp-slot (nelisp-phase47-compiler--ir-get node :cp-slot))
        (width-slot (nelisp-phase47-compiler--ir-get node :width-slot)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value cp-slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value width-slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rcx)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_str_codepoint_at" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-str-is-alphanumeric-at (node buf)
  "Emit `str-is-alphanumeric-at' — 2-arg call to `nl_str_is_alphanumeric_at'."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (idx (nelisp-phase47-compiler--ir-get node :idx)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value idx buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_str_is_alphanumeric_at" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

;; ---- Doc 122 §122.E — Atomic + raw memory primitives emit ----

(defun nelisp-phase47-compiler--emit-atomic-fetch-add (node buf)
  "Emit `atomic-fetch-add' — inline `LOCK XADD [RDI], RAX' (no PLT call).
Doc 131 §131.A: ptr → rdi, delta → rax, then LOCK XADD [rdi], rax.
On exit rax = old [rdi] (= the pre-add value), [rdi] = old+delta."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (delta (nelisp-phase47-compiler--ir-get node :delta)))
    ;; Evaluate ptr into rax, save it to rdi via push/pop.
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Evaluate delta into rax (= the XADD source operand).
    (nelisp-phase47-compiler--emit-value delta buf)
    ;; rdi = ptr, rax = delta (XADD src/dst).
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; LOCK XADD [rdi], rax  →  rax = old [rdi], [rdi] = old + delta.
    (nelisp-asm-x86_64-lock-xadd-mem-rax-rdi buf)))

(defun nelisp-phase47-compiler--emit-atomic-compare-exchange (node buf)
  "Emit `atomic-compare-exchange' — inline `LOCK CMPXCHG [RDI], RDX' (no PLT call).
Doc 131 §131.A: ptr → rdi, expected → rax, new-val → rdx.
LOCK CMPXCHG [rdi], rdx: if [rdi]==rax write rdx, set ZF; else clear ZF.
SETE AL + MOVZX RAX, AL: rax = 1 on success, 0 on failure."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (expected (nelisp-phase47-compiler--ir-get node :expected))
        (new-val (nelisp-phase47-compiler--ir-get node :new-val)))
    ;; Evaluate ptr → rax, stash.
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Evaluate expected → rax, stash.
    (nelisp-phase47-compiler--emit-value expected buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Evaluate new-val → rax; pop into rdx (new-val), rax (expected), rdi (ptr).
    (nelisp-phase47-compiler--emit-value new-val buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'rax)   ; rdx = new-val
    (nelisp-asm-x86_64-pop buf 'rax)                 ; rax = expected
    (nelisp-asm-x86_64-pop buf 'rdi)                 ; rdi = ptr
    ;; LOCK CMPXCHG [rdi], rdx  (ZF set on success).
    (nelisp-asm-x86_64-lock-cmpxchg-mem-rdx-rdi buf)
    ;; SETE AL; MOVZX RAX, AL  → rax = 1 (success) or 0 (failure).
    (nelisp-asm-x86_64-sete-al buf)
    (nelisp-asm-x86_64-movzx-eax-al buf)))

(defun nelisp-phase47-compiler--emit-ptr-read (node buf helper-name)
  "Emit `ptr-read-u{8,16,32,64}' — inline MOV/MOVZX at [RDI+RSI] (no PLT call).
Doc 131 §131.A: HELPER-NAME selects the width-specific load instruction:
  nl_ptr_read_u64 → MOV  RAX, QWORD PTR [RDI+RSI]
  nl_ptr_read_u8  → MOVZX RAX, BYTE  PTR [RDI+RSI]
  nl_ptr_read_u16 → MOVZX RAX, WORD  PTR [RDI+RSI]
  nl_ptr_read_u32 → MOV  EAX, DWORD PTR [RDI+RSI]  (CPU zero-extends to RAX)
Result is zero-extended to i64 in rax in all cases."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (offset (nelisp-phase47-compiler--ir-get node :offset)))
    ;; Evaluate ptr → rax, save; evaluate offset → rax (= rsi after pop).
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value offset buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'rax)   ; rsi = offset
    (nelisp-asm-x86_64-pop buf 'rdi)                 ; rdi = ptr
    ;; Inline load — width determined by helper-name.
    (cond
     ((string= helper-name "nl_ptr_read_u64")
      (nelisp-asm-x86_64-mov-rax-qword-rdi-rsi buf))
     ((string= helper-name "nl_ptr_read_u8")
      (nelisp-asm-x86_64-movzx-rax-byte-rdi-rsi buf))
     ((string= helper-name "nl_ptr_read_u16")
      (nelisp-asm-x86_64-movzx-rax-word-rdi-rsi buf))
     ((string= helper-name "nl_ptr_read_u32")
      (nelisp-asm-x86_64-mov-eax-dword-rdi-rsi buf))
     (t (signal 'nelisp-phase47-compiler-error
                (list :unknown-ptr-read-helper helper-name))))))

(defun nelisp-phase47-compiler--emit-ptr-write (node buf helper-name)
  "Emit `ptr-write-u{8,16,32,64}' — inline MOV store at [RDI+RSI] (no PLT call).
Doc 131 §131.A: HELPER-NAME selects the width-specific store instruction:
  nl_ptr_write_u64 → MOV QWORD PTR [RDI+RSI], RDX
  nl_ptr_write_u8  → MOV BYTE  PTR [RDI+RSI], DL
  nl_ptr_write_u16 → MOV WORD  PTR [RDI+RSI], DX
  nl_ptr_write_u32 → MOV DWORD PTR [RDI+RSI], EDX
Returns rax = 1 sentinel for `and'-chain composition (matches the Rust void-extern contract)."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (offset (nelisp-phase47-compiler--ir-get node :offset))
        (val (nelisp-phase47-compiler--ir-get node :val)))
    ;; Evaluate ptr, offset, val; assign to rdi, rsi, rdx.
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value offset buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdx 'rax)   ; rdx = val
    (nelisp-asm-x86_64-pop buf 'rsi)                 ; rsi = offset
    (nelisp-asm-x86_64-pop buf 'rdi)                 ; rdi = ptr
    ;; Inline store — width determined by helper-name.
    (cond
     ((string= helper-name "nl_ptr_write_u64")
      (nelisp-asm-x86_64-mov-qword-rdi-rsi-rdx buf))
     ((string= helper-name "nl_ptr_write_u8")
      (nelisp-asm-x86_64-mov-byte-rdi-rsi-dl buf))
     ((string= helper-name "nl_ptr_write_u16")
      (nelisp-asm-x86_64-mov-word-rdi-rsi-dx buf))
     ((string= helper-name "nl_ptr_write_u32")
      (nelisp-asm-x86_64-mov-dword-rdi-rsi-edx buf))
     (t (signal 'nelisp-phase47-compiler-error
                (list :unknown-ptr-write-helper helper-name))))
    ;; rax = 1 sentinel (matches Rust void extern contract for and-chain).
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)))

;; ---- Doc 125 §125.A — alloc / dealloc primitives emit ----

(defun nelisp-phase47-compiler--emit-alloc-bytes (node buf)
  "Emit `alloc-bytes' — 2-arg call to `nl_alloc_bytes(size, align) -> *mut u8'.
Returns the freshly-allocated pointer re-cast to `i64' in rax (= 0
on layout error or OOM)."
  (let ((size (nelisp-phase47-compiler--ir-get node :size))
        (align (nelisp-phase47-compiler--ir-get node :align)))
    (nelisp-phase47-compiler--emit-value size buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value align buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_bytes" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)))

(defun nelisp-phase47-compiler--emit-dealloc-bytes (node buf)
  "Emit `dealloc-bytes' — 3-arg call to `nl_dealloc_bytes(ptr, size, align)'.
Returns rax = 1 sentinel for `and'-chain composition (= underlying
extern is `void')."
  (let ((ptr (nelisp-phase47-compiler--ir-get node :ptr))
        (size (nelisp-phase47-compiler--ir-get node :size))
        (align (nelisp-phase47-compiler--ir-get node :align)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value size buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value align buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_dealloc_bytes" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)))

;; ---- Doc 125 §125.B inline syscall op ----

(defun nelisp-phase47-compiler--emit-syscall-direct (node buf)
  "Emit `syscall-direct' — inline SYSCALL with 7 fixed args.
ABI: rax=NR rdi=A0 rsi=A1 rdx=A2 r10=A3 r8=A4 r9=A5 → rax.
Strategy: evaluate each arg into rax and push; then pop in reverse
order (A5→r9, A4→r8, A3→r10, A2→rdx, A1→rsi, A0→rdi, NR→rax).
SYSCALL clobbers rcx and r11; the result is left in rax.
Stack alignment: 7 pushes (= 56 bytes offset); SYSCALL itself does
not require 16-byte alignment, so no extra pad is needed."
  ;; Push NR first, then A0..A5 (will be popped in reverse).
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :nr) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a0) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a1) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a2) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a3) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a4) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :a5) buf)
  (nelisp-asm-x86_64-push buf 'rax)
  ;; Pop into syscall registers (reverse of push order).
  (nelisp-asm-x86_64-pop buf 'r9)
  (nelisp-asm-x86_64-pop buf 'r8)
  (nelisp-asm-x86_64-pop buf 'r10)
  (nelisp-asm-x86_64-pop buf 'rdx)
  (nelisp-asm-x86_64-pop buf 'rsi)
  (nelisp-asm-x86_64-pop buf 'rdi)
  (nelisp-asm-x86_64-pop buf 'rax)
  (nelisp-asm-x86_64-syscall buf))

;; ---- Doc 101 §101.D Cons construction ops ----

(defun nelisp-phase47-compiler--emit-cons-make (node buf)
  "Emit a `Sexp::Cons' constructor into NODE's caller-owned slot.
Strategy:

  1. Evaluate CAR-PTR / CDR-PTR / SLOT and save them on the stack.
  2. Call `nl_alloc_consbox()' to allocate a fresh `NlConsBox'
     initialized to `(nil . nil)' with refcount 1.
  3. Copy the 32-byte Sexp at `*CAR-PTR' into `box->car' and the
     32-byte Sexp at `*CDR-PTR' into `box->cdr' via two 16-byte
     `movdqu' pairs each.
  4. Write `SEXP_TAG_CONS' and the box pointer into SLOT.
  5. Return SLOT in rax.

MVP ownership constraint: the copied `Sexp' payloads are assumed to
already be caller-owned / cloned; this op does not yet perform
refcount-aware nested-box increments."
  (let ((car-ptr (nelisp-phase47-compiler--ir-get node :car-ptr))
        (cdr-ptr (nelisp-phase47-compiler--ir-get node :cdr-ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    (nelisp-phase47-compiler--emit-value car-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value cdr-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Keep rsp 16-byte aligned across the extern call: 3 saved
    ;; values would misalign the call site, so reserve one extra
    ;; scratch slot and discard it after the call.
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_consbox" -4 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    (nelisp-asm-x86_64-pop buf 'r11)
    ;; last pushed (= slot) -> rsi, cdr-ptr -> rdx, car-ptr -> rdi
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; box->car = *car-ptr
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'rdi 0)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'r10 nelisp-nlconsbox--offset-car 'xmm0)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'rdi 16)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'r10 (+ nelisp-nlconsbox--offset-car 16) 'xmm0)
    ;; box->cdr = *cdr-ptr
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'rdx 0)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'r10 nelisp-nlconsbox--offset-cdr 'xmm0)
    (nelisp-asm-x86_64-movdqu-xmm-mem-disp8 buf 'xmm0 'rdx 16)
    (nelisp-asm-x86_64-movdqu-mem-disp8-xmm buf 'r10 (+ nelisp-nlconsbox--offset-cdr 16) 'xmm0)
    ;; slot = Sexp::Cons(box)
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rsi nelisp-sexp--tag-cons)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rsi nelisp-sexp--offset-payload 'r10)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cons-make-with-clone (node buf)
  "Emit Doc 120.E fused `cons-make-with-clone' constructor.
Strategy parallels `--emit-cons-make' but replaces the two 32-byte
`movdqu' pairs with two `nl_sexp_clone_into' PLT calls so refcounted
payloads get a deep / refcount-aware clone instead of a raw memcpy.

Stack discipline matches `--emit-cons-make':

  1. Push CAR-PTR / CDR-PTR / SLOT and one alignment scratch (4 pushes
     → rsp ≡ 0 mod 16 for the upcoming extern call).
  2. `call nl_alloc_consbox@PLT' → rax = fresh `NlConsBox*' (car/cdr
     pre-initialised to `Sexp::Nil', refcount = 1).  Move into r10.
  3. Pop alignment / SLOT / CDR-PTR / CAR-PTR (LIFO) into rsi / rdx /
     rdi.
  4. First clone: save rsi+rdx+r10 across the call, then `mov rsi,
     r10; call nl_sexp_clone_into@PLT' (rdi already = CAR-PTR).  3 pushes
     + 1 alignment push keeps rsp ≡ 0 mod 16.  Pop everything back.
  5. Second clone: save rsi+r10, set rdi=rdx (CDR-PTR), rsi=r10+32
     (= `&box->cdr'), 2 pushes + 2 alignment pushes for SysV alignment.
     `call nl_sexp_clone_into@PLT' then restore.
  6. Write `Sexp::Cons(box)' into SLOT: tag byte 7 at offset 0,
     `mov [rsi+8], r10' for the payload pointer.
  7. Return SLOT in rax (matches `cons-make' ABI).

No refcount bumps are needed at this layer — `nl_sexp_clone_into'
performs the variant-aware refcount inc / String deep copy itself,
and the fresh NlConsBox already starts at refcount = 1."
  (let ((car-ptr (nelisp-phase47-compiler--ir-get node :car-ptr))
        (cdr-ptr (nelisp-phase47-compiler--ir-get node :cdr-ptr))
        (slot (nelisp-phase47-compiler--ir-get node :slot)))
    ;; Step 1: spill the 3 input pointers + 1 alignment scratch.
    (nelisp-phase47-compiler--emit-value car-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value cdr-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; Step 2: call nl_alloc_consbox -> rax; stash in r10.
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_alloc_consbox" -4 'text)
    (nelisp-asm-x86_64-mov-reg-reg buf 'r10 'rax)
    ;; Step 3: restore alignment / slot / cdr-ptr / car-ptr.
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Step 4: nl_sexp_clone_into(car-ptr, box).  Save rsi/rdx/r10
    ;; across the call (= all caller-saved) plus a 4th push for the
    ;; 16-byte alignment.
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-push buf 'rdx)
    (nelisp-asm-x86_64-push buf 'r10)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'r10)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_sexp_clone_into" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'r10)
    (nelisp-asm-x86_64-pop buf 'rdx)
    (nelisp-asm-x86_64-pop buf 'rsi)
    ;; Step 5: nl_sexp_clone_into(cdr-ptr, box + 32).  rdi := cdr-ptr,
    ;; rsi := box + 32.  Save rsi/r10 across the call plus 2 alignment
    ;; pushes (= 4 push = 16-byte aligned).
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-push buf 'r10)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rdx)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'r10)
    (nelisp-asm-x86_64-add-imm32 buf 'rsi nelisp-nlconsbox--offset-cdr)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf "nl_sexp_clone_into" -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'r10)
    (nelisp-asm-x86_64-pop buf 'rsi)
    ;; Step 6: write Sexp::Cons(box) into slot.
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rsi nelisp-sexp--tag-cons)
    (nelisp-asm-x86_64-mov-mem-reg-disp8
     buf 'rsi nelisp-sexp--offset-payload 'r10)
    ;; Step 7: return slot in rax (matches cons-make ABI).
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsi)))

(defun nelisp-phase47-compiler--emit-cons-set-slot (node buf helper-name)
  "Emit `cons-set-car' / `cons-set-cdr' via Rust HELPER-NAME.
NODE carries `:handle' (= `*const Sexp' pointing at a Cons slot) and
`:val-ptr' (= `*const Sexp').  The helper does the drop-then-write
mutation on the boxed field; this op only resolves the `NlConsBox*'
payload from the handle and forwards the two pointers.  Returns the
original handle pointer in rax."
  (let ((handle (nelisp-phase47-compiler--ir-get node :handle))
        (val-ptr (nelisp-phase47-compiler--ir-get node :val-ptr)))
    (nelisp-phase47-compiler--emit-value handle buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value val-ptr buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    ;; Preserve H across the helper call; caller-saved regs are not
    ;; stable, so keep it on the stack and pop it back into rax after.
    (nelisp-asm-x86_64-push buf 'rdi)
    ;; Same alignment rule as `cons-make': two extra pushes keep the
    ;; call site at a 16-byte boundary.
    (nelisp-asm-x86_64-push buf 'rsi)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rdi 'rdi nelisp-sexp--offset-payload)
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf (symbol-name helper-name) -4 'text)
    (nelisp-asm-x86_64-pop buf 'r11)
    (nelisp-asm-x86_64-pop buf 'rax)))

;; ---- §97.c emit — comparisons + control flow ----

(defconst nelisp-phase47-compiler--cmp-setcc
  '((< . setl)
    (> . setg)
    (<= . setle)
    (>= . setge)
    (= . sete))
  "Map Doc 97.c comparison op -> setCC mnemonic for AL.
Signed comparisons match SBCL/Elisp integer semantics; the
underlying `cmp' instruction sets SF/OF/ZF and the setCC
opcode reads the right combination.")

(defun nelisp-phase47-compiler--emit-cmp (node buf)
  "Emit signed comparison NODE; result (0 or 1) in rax.
Strategy: compute B -> rax -> push, compute A -> rax, pop r10 (=
B), cmp rax, r10 (= computes A - B flag set), then setCC al +
movzx eax, al to materialise the boolean into rax.  Uses r10 to
  avoid arg-reg aliasing inside chained calls, mirroring the
  Doc 97.b arith convention."
  (let* ((op (nelisp-phase47-compiler--ir-get node :op))
         (a (nelisp-phase47-compiler--ir-get node :a))
         (b (nelisp-phase47-compiler--ir-get node :b))
         (cc (cdr (or (assq op nelisp-phase47-compiler--cmp-setcc)
                      (signal 'nelisp-phase47-compiler-error
                              (list :unknown-cmp-op op))))))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (let ((arm64-cc
               (pcase op
                 ('= 'eq)
                 ('< 'lt)
                 ('> 'gt)
                 ('<= 'le)
                 ('>= 'ge))))
          (nelisp-phase47-compiler--emit-value b buf)
          (nelisp-asm-arm64-str-pre-sp-16 buf 'x0)
          (nelisp-phase47-compiler--emit-value a buf)
          (nelisp-asm-arm64-ldr-post-sp-16 buf 'x9)
          (nelisp-asm-arm64-cmp-reg-reg buf 'x0 'x9)
          (nelisp-asm-arm64-cset buf 'x0 arm64-cc))
      ;; Compute B -> rax, save on stack.
      (nelisp-phase47-compiler--emit-value b buf)
      (nelisp-asm-x86_64-push buf 'rax)
      ;; Compute A -> rax.
      (nelisp-phase47-compiler--emit-value a buf)
      ;; Recover B into r10.
      (nelisp-asm-x86_64-pop buf 'r10)
      ;; cmp rax, r10                          (= A - B sets flags)
      (nelisp-asm-x86_64-cmp-reg-reg buf 'rax 'r10)
      ;; setCC al
      (nelisp-asm-x86_64-setcc-al buf cc)
      ;; movzx eax, al                         (= zero-extends to rax)
      (nelisp-asm-x86_64-movzx-eax-al buf))))

(defun nelisp-phase47-compiler--emit-if (node buf)
  "Emit `if' branching code; result in rax.
Layout (= each emitter call is byte-fixed so pass invariance
holds across the two-pass orchestration):
    <emit TEST>                  -> rax
    cmp rax, 0
    jz   else-LABEL
    <emit THEN>                  -> rax
    jmp  end-LABEL
  else-LABEL:
    <emit ELSE>                  -> rax
  end-LABEL:"
  (let* ((id (nelisp-phase47-compiler--ir-get node :id))
         (else-lbl (intern (format "%s-else" id)))
         (end-lbl (intern (format "%s-end" id))))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (progn
          (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :test) buf)
          (nelisp-asm-arm64-cmp-reg-reg buf 'x0 'xzr)
          (nelisp-asm-arm64-b-cond buf 'eq else-lbl)
          (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :then) buf)
          (nelisp-asm-arm64-b buf end-lbl)
          (nelisp-asm-arm64-define-label buf else-lbl)
          (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :else) buf)
          (nelisp-asm-arm64-define-label buf end-lbl))
      (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :test) buf)
      (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
      (nelisp-asm-x86_64-jz-rel32 buf else-lbl)
      (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :then) buf)
      (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
      (nelisp-asm-x86_64-define-label buf else-lbl)
      (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :else) buf)
      (nelisp-asm-x86_64-define-label buf end-lbl))))

(defun nelisp-phase47-compiler--emit-while (node buf)
  "Emit `while' loop; result = 0 (in rax) after loop exits.
Layout:
  start-LABEL:
    <emit TEST>                  -> rax
    cmp rax, 0
    jz   end-LABEL
    <emit BODY...>               -> rax (= each form, result discarded)
    jmp  start-LABEL
  end-LABEL:
    mov  rax, 0                  -> return 0 sentinel"
  (let* ((id (nelisp-phase47-compiler--ir-get node :id))
         (start-lbl (intern (format "%s-start" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-asm-x86_64-define-label buf start-lbl)
    (nelisp-phase47-compiler--emit-value (nelisp-phase47-compiler--ir-get node :test) buf)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jz-rel32 buf end-lbl)
    (dolist (form (nelisp-phase47-compiler--ir-get node :body))
      (nelisp-phase47-compiler--emit-value form buf))
    (nelisp-asm-x86_64-jmp-rel32 buf start-lbl)
    (nelisp-asm-x86_64-define-label buf end-lbl)
    ;; mov rax, 0 — 7 bytes, fixed; cheaper to use xor in real
    ;; codegen but byte-length parity with mov-imm32 keeps the
    ;; rest of the chain in lockstep.
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)))

(defun nelisp-phase47-compiler--emit-cond (node buf)
  "Emit `cond' first-match-wins dispatch; result in rax.
Clauses with predicate `t' (= already lowered to symbol `always'
during parse) emit their body unconditionally.  Layout per non-
trivial clause:
    <emit PRED>     -> rax
    cmp rax, 0
    jz   next-K
    <emit BODY>     -> rax
    jmp  end
  next-K:
The final `t' clause (= always) elides the cmp/jz and emits the
body straight; `end-LABEL' is then defined immediately after."
  (let* ((id (nelisp-phase47-compiler--ir-get node :id))
         (end-lbl (intern (format "%s-end" id)))
         (clauses (nelisp-phase47-compiler--ir-get node :clauses))
         (k 0))
    (dolist (cl clauses)
      (setq k (1+ k))
      (let ((pred (car cl))
            (body (cdr cl)))
        (cond
         ((eq pred 'always)
          ;; Unconditional clause (= t / fallthrough).
          (nelisp-phase47-compiler--emit-value body buf))
         (t
          (let ((next-lbl (intern (format "%s-next-%d" id k))))
            (nelisp-phase47-compiler--emit-value pred buf)
            (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
            (nelisp-asm-x86_64-jz-rel32 buf next-lbl)
            (nelisp-phase47-compiler--emit-value body buf)
            (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
            (nelisp-asm-x86_64-define-label buf next-lbl))))))
    ;; If no `always' clause and every predicate failed, fall
    ;; through to a 0 sentinel so rax is always defined.
    (unless (eq (car (car (last clauses))) 'always)
      (nelisp-asm-x86_64-mov-imm32 buf 'rax 0))
    (nelisp-asm-x86_64-define-label buf end-lbl)))

(defun nelisp-phase47-compiler--emit-logic (node buf)
  "Emit `and'/`or' short-circuit evaluation; result in rax.
For `and': each form is evaluated; if it produces 0 jump to
fail-end (with rax = 0).  If all forms produced non-0 the last
rax stays as the result.
For `or': each form is evaluated; if it produces non-0 jump to
win-end (= rax already holds the winning value).  If every form
returned 0 the last rax (= 0) stays.
Final emit:
  end-LABEL:"
  (let* ((id (nelisp-phase47-compiler--ir-get node :id))
         (op (nelisp-phase47-compiler--ir-get node :op))
         (end-lbl (intern (format "%s-end" id)))
         (forms (nelisp-phase47-compiler--ir-get node :forms)))
    (dolist (f forms)
      (nelisp-phase47-compiler--emit-value f buf)
      ;; Short-circuit test: cmp rax, 0; then jump on the
      ;; condition that ends the eval chain.
      (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
      (cond
       ((eq op 'and)
        ;; rax = 0 means failure; jump to end with rax already 0.
        (nelisp-asm-x86_64-jz-rel32 buf end-lbl))
       ((eq op 'or)
        ;; rax non-zero means success; jump to end with rax kept.
        (nelisp-asm-x86_64-jnz-rel32 buf end-lbl))
       (t
        (signal 'nelisp-phase47-compiler-error
                (list :unknown-logic-op op)))))
    ;; If no early-exit fired, fall through: rax holds the last
    ;; evaluated value (= last operand for `and' = non-zero last,
    ;; for `or' = 0 last).  No additional emit needed.
    (nelisp-asm-x86_64-define-label buf end-lbl)))

;; ---- §97.5 emit walker — statements ----

(defun nelisp-phase47-compiler--emit-table-lookup (node buf)
  "Emit `static-imm32-table-lookup' value op, result u32 → rax.
NODE is `(:kind table-lookup :name NAME :index INDEX-IR)'.
BUF is the asm buffer.  Sequence (= fixed 20 bytes after INDEX-IR):
  <eval INDEX-IR → rax>     ; varies
  mov rsi, rax              ; 3 bytes (REX.W 89 C6)
  shl rsi, 2                ; 4 bytes (REX.W C1 E6 02)  = index*4
  mov rdi, TABLE_VADDR      ; 10 bytes (REX.W B8+rd imm64)
  mov eax, [rdi+rsi]        ; 3 bytes (8B 04 37)        = u32 load
The CPU zero-extends EAX to RAX on the 32-bit load, so the high
32 bits are guaranteed clear.  TABLE_VADDR is resolved via
`nelisp-phase47-compiler--table-vaddrs' (= bound by `--pass').
Pass-1 sees vaddr=0 (= sentinel); pass-2 sees the real .rodata
absolute address.  Byte width is identical in both passes (= the
imm64 size is fixed) so the byte invariant holds."
  (let* ((name (nelisp-phase47-compiler--ir-get node :name))
         (index-ir (nelisp-phase47-compiler--ir-get node :index))
         (vaddr (or (cdr (assoc name
                                nelisp-phase47-compiler--table-vaddrs))
                    ;; Missing entry only legal during pre-collector
                    ;; invocation (= shouldn't happen at emit time).
                    (signal 'nelisp-phase47-compiler-error
                            (list :static-imm32-table-vaddr-missing
                                  name)))))
    ;; 1. Eval INDEX-IR → rax (= variable size).
    (nelisp-phase47-compiler--emit-value index-ir buf)
    ;; 2. mov rsi, rax (= snapshot index into rsi).
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsi 'rax)
    ;; 3. shl rsi, 2 (= rsi = index * 4 bytes).
    (nelisp-asm-x86_64-shl-reg-imm8 buf 'rsi 2)
    ;; 4. mov rdi, IMM64 (= absolute address of table[0]).
    (nelisp-asm-x86_64-mov-imm64 buf 'rdi vaddr)
    ;; 5. mov eax, [rdi+rsi] (= load u32, zero-extends to rax).
    (nelisp-asm-x86_64-mov-eax-dword-rdi-rsi buf)))

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
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    (nelisp-asm-x86_64-mov-imm32 buf 'rdi 1)
    (nelisp-asm-x86_64-mov-imm64 buf 'rsi addr)
    (nelisp-asm-x86_64-mov-imm32 buf 'rdx len)
    (nelisp-asm-x86_64-syscall buf)))

(defun nelisp-phase47-compiler--emit-exit (buf value-node)
  "Emit an exit(STATUS) syscall to BUF.
VALUE-NODE is a value-producing IR node.  If it's an `imm', emit
the legacy fixed-status path (= 16 bytes).  Otherwise compute the
value into rax then `mov rdi, rax' + syscall."
  ;; A33.3 — integer-tag dispatch (`pcase' arm → `cond' over
  ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
  (let ((tag (nelisp-phase47-compiler--ir-kind-tag value-node)))
   (cond
    ((= tag 30)                 ; imm
     (let ((status (nelisp-phase47-compiler--ir-get value-node :value)))
       (nelisp-asm-x86_64-mov-imm32 buf 'rax 60)
       (nelisp-asm-x86_64-mov-imm32 buf 'rdi status)
       (nelisp-asm-x86_64-syscall buf)))
    (t
     ;; Compute value into rax (= might call functions).
     (nelisp-phase47-compiler--emit-value value-node buf)
     ;; mov rdi, rax (= exit status from computed value).
     (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
     ;; mov rax, 60 (SYS_exit).
     (nelisp-asm-x86_64-mov-imm32 buf 'rax 60)
     (nelisp-asm-x86_64-syscall buf)))))

(defun nelisp-phase47-compiler--emit-stmt (ir buf str-offsets rodata-vaddr)
  "Walk statement IR appending instructions to BUF.
STR-OFFSETS maps literal string -> (:offset N :len L).
RODATA-VADDR is the absolute vaddr of byte 0 of .rodata (= 0 during
pass-1 sizing, real value during pass-2).  `defun' nodes are
skipped here — they're emitted separately by the orchestrator."
  (when ir
    ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
    ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
    (let ((tag (nelisp-phase47-compiler--ir-kind-tag ir)))
     (cond
      ((= tag 87)               ; write
       (nelisp-phase47-compiler--emit-write
        buf (nelisp-phase47-compiler--ir-get ir :str) str-offsets rodata-vaddr))
      ((= tag 22)               ; exit
       (nelisp-phase47-compiler--emit-exit
        buf (nelisp-phase47-compiler--ir-get ir :value)))
      ((= tag 54)               ; seq
       (dolist (child (nelisp-phase47-compiler--ir-get ir :forms))
         (nelisp-phase47-compiler--emit-stmt
          child buf str-offsets rodata-vaddr)))
      ((= tag 31)               ; let
       (nelisp-phase47-compiler--emit-stmt
        (nelisp-phase47-compiler--ir-get ir :body) buf str-offsets rodata-vaddr))
      ((= tag 32)               ; let-rt
       ;; Runtime let: evaluate value-ir → rax, spill to frame slot,
       ;; then walk body as statement.
       (let* ((slot (nelisp-phase47-compiler--ir-get ir :slot))
              (value-ir (nelisp-phase47-compiler--ir-get ir :value-ir))
              (disp (- (* 8 (1+ slot)))))
         (nelisp-phase47-compiler--emit-value value-ir buf)
         (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rbp disp 'rax)
         (nelisp-phase47-compiler--emit-stmt
          (nelisp-phase47-compiler--ir-get ir :body) buf str-offsets rodata-vaddr)))
      ((= tag 89)               ; let-rt-n
       (nelisp-phase47-compiler--emit-let-rt-n-bindings ir buf)
       (nelisp-phase47-compiler--emit-stmt
        (nelisp-phase47-compiler--ir-get ir :body) buf str-offsets rodata-vaddr))
      ((= tag 21)               ; defun
       ;; Skip — handled by `--emit-defun' separately.
       nil)
      ((= tag 79)               ; table-define
       ;; Doc 49 Wave 11.1: skip — placed in `.rodata' by the
       ;; collector pass, no .text bytes.
       nil)
      ((= tag 80)               ; table-lookup
       ;; Statement-context lookup discards rax (= side-effect free,
       ;; but we still emit so any address-of-table refs land).
       (nelisp-phase47-compiler--emit-table-lookup ir buf))
      ((= tag 5)                ; call
       ;; Statement-context call discards rax.
       (nelisp-phase47-compiler--emit-call ir buf))
      ((memq tag '(29 86 11 33 88 10 1 67 90 91)) ; if while cond logic value-seq cmp arith shift sexp-write-symbol-lit sexp-write-str-lit
       ;; §97.c: value-producing control-flow / comparison form
       ;; reached statement position (= `seq' child, top-level).
       ;; Emit the value compute; rax is discarded by the
       ;; surrounding context.
       (nelisp-phase47-compiler--emit-value ir buf))
      (t
       (let ((kind (nelisp-phase47-compiler--ir-kind ir)))
         (signal 'nelisp-phase47-compiler-error
                 (list :unknown-ir-kind kind))))))))

(defun nelisp-phase47-compiler--emit-defun (defun-ir buf)
  "Emit a single function definition into BUF.
The function's label is defined at the entry point; the prologue
saves rbp + every incoming param register onto the stack, the
body computes the return value into rax (= GP class) or xmm0
(= Doc 110 §110.E.1 f64 class), the epilogue restores rsp + rbp
and `ret's.

GP class rationale (= why params spill to memory): callees freely
clobber the SysV arg registers (rdi..r9 are caller-saved), so a
function that calls back into itself or any other function would
lose its own params if they only lived in arg-regs.  Spilling at
prologue and re-reading from `[rbp - 8*(slot+1)]' on every `ref'
makes recursive composition (= factorial) work.  Each `push' is
fixed-width (= 1 byte for low regs, 2 bytes for r8/r9) so the
two-pass byte invariant is preserved per defun.

f64 class rationale (Doc 110 §110.E.1): xmm0-xmm7 are caller-saved
in SysV AMD64 (same as rdi..r9), so the same spill argument
applies.  But xmm has no single-byte push opcode — instead the
prologue allocates the full spill area in one `SUB rsp, 8*N'
(= 7 bytes, fixed) and then writes each xmm via `MOVSD [rbp -
8*(slot+1)], xmmN' (= 5 bytes each).  The epilogue's `mov rsp,
rbp; pop rbp; ret' tears the frame down identically to the GP
path.  Return value lands in xmm0 implicitly (= the SysV f64
return reg, untouched by epilogue)."
  (let* ((name (nelisp-phase47-compiler--ir-get defun-ir :name))
         (param-regs (nelisp-phase47-compiler--ir-get defun-ir :param-regs))
         (param-class (or (nelisp-phase47-compiler--ir-get defun-ir :param-class) 'gp))
         (body (nelisp-phase47-compiler--ir-get defun-ir :body))
         (rt-slot-count (or (nelisp-phase47-compiler--ir-get defun-ir :rt-slot-count) 0))
         ;; Track this defun's arity for `--emit-extern-call' (Doc 111
         ;; §111.E #19-#26 stack alignment fix).  Inner emit helpers
         ;; bound under this `let*' see the param count via the dynvar
         ;; and can issue an extra `sub rsp, 8' before extern `call's
         ;; when arity is odd, restoring rsp ≡ 0 mod 16 SysV alignment.
         ;; For rt-let: by the time the body runs rsp is already aligned
         ;; (= param pad + rt-let pad both applied below), so arity here
         ;; still reflects the param count for the extern-call pad calc.
         (nelisp-phase47-compiler--current-defun-arity (length param-regs)))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (let ((gp-arg-regs '(x0 x1 x2 x3 x4 x5))
              (fp-arg-regs '(d0 d1 d2 d3 d4 d5 d6 d7)))
          (nelisp-asm-arm64-define-label buf name)
          ;; Save LR for forward compatibility, then establish x29.
          (nelisp-asm-arm64-str-pre-sp-16 buf 'x30)
          (nelisp-asm-arm64-str-pre-sp-16 buf 'x29)
          (nelisp-asm-arm64-mov-reg-reg buf 'x29 'sp)
          (cond
           ((eq param-class 'gp)
            (dotimes (i (length param-regs))
              (nelisp-asm-arm64-str-pre-sp-16 buf (nth i gp-arg-regs))))
           ((eq param-class 'f64)
            ;; Allocate `arity*16' bytes (= 16-byte slot per f64
            ;; param matching the GP path's `str-pre-sp-16'
            ;; 16-byte stride).  Each f64 param spills via STUR D
            ;; at `[x29 - 16*(slot+1)]'.  AAPCS stack stays 16-
            ;; byte aligned because every slot is a multiple of 16.
            (let* ((arity (length param-regs))
                   (frame-bytes (* 16 arity)))
              (when (> arity 0)
                (nelisp-asm-arm64-sub-imm buf 'sp 'sp frame-bytes))
              (dotimes (i arity)
                (let ((slot-idx i)
                      (dreg (nth i fp-arg-regs)))
                  (nelisp-asm-arm64-stur-d-base-disp
                   buf dreg 'x29 (- (* 16 (1+ slot-idx))))))))
           (t
            (signal 'nelisp-phase47-compiler-error
                    (list :unknown-defun-param-class param-class))))
          (nelisp-phase47-compiler--emit-value body buf)
          (nelisp-asm-arm64-mov-reg-reg buf 'sp 'x29)
          (nelisp-asm-arm64-ldr-post-sp-16 buf 'x29)
          (nelisp-asm-arm64-ldr-post-sp-16 buf 'x30)
          (nelisp-asm-arm64-ret buf))
      ;; Wave 20: hand-inline the prologue's literal-register emit
      ;; calls so the standalone NeLisp interpreter does not pay the
      ;; per-defun-dispatch cost.  Each inline macro expands to the
      ;; helper's byte-emit body with REX + ModR/M folded to constants
      ;; (DST + SRC are quoted literals here).  Byte output unchanged
      ;; — verified by md5sum on spike-noop.o / fact-i64.o.
      (nelisp-asm-x86_64--define-label-inline buf name)
      ;; Prologue: push rbp; mov rbp, rsp; spill each param reg.
      (nelisp-asm-x86_64--push-inline buf 'rbp)
      (nelisp-asm-x86_64--mov-reg-reg-inline buf 'rbp 'rsp)
      (if (eq nelisp-phase47-compiler--abi 'win64)
          ;; ---- Win64 ABI prologue ----
          ;;
          ;; Microsoft x64 ABI differs from SysV in three ways:
          ;;
          ;; 1. Argument registers: RCX RDX R8 R9 (not RDI RSI RDX RCX R8 R9).
          ;;    The first 4 integer args arrive in RCX/RDX/R8/R9 which we
          ;;    must move into our SysV-convention stack slots so the body's
          ;;    `ref' loads from `[rbp - 8*(slot+1)]' still work correctly.
          ;;
          ;; 2. Shadow space: caller already reserved 32 bytes below RSP
          ;;    for the register home area.  We don't use those bytes but
          ;;    the epilogue's `mov rsp, rbp' pops them for free.
          ;;
          ;; 3. Callee-saved: RDI and RSI are callee-saved under Win64
          ;;    (they are caller-saved in SysV).  Since our arg-pushing
          ;;    strategy pushes the incoming Win64 regs (RCX/RDX/R8/R9)
          ;;    we never write to RDI/RSI in the prologue, so no extra
          ;;    save/restore is needed for them in this simple prologue.
          ;;
          ;; Strategy for GP params:
          ;;   a. `sub rsp, 8*ROUNDED' to allocate the spill frame.
          ;;   b. `mov [rbp - 8*(i+1)], rcx/rdx/r8/r9' per param.
          ;;   The frame layout matches the SysV path so `--emit-ref-load'
          ;;   (= `[rbp - 8*(slot+1)]') reads the right value.
          ;;
          ;; Stack alignment (Win64): call sets rsp ≡ 8 mod 16 (= return
          ;; addr pushed).  `push rbp' brings rsp to 0 mod 16.  We then
          ;; allocate spill slots via `sub rsp, 8*ROUNDED'; ROUNDED must
          ;; be even (= 16-byte aligned allocation) — same rounding rule
          ;; as SysV but win64 arg regs differ.
          (cond
           ((eq param-class 'gp)
            (let* ((arity (length param-regs))
                   (rounded (if (zerop (logand arity 1)) arity (1+ arity)))
                   (frame-bytes (* 8 rounded))
                   (win64-arg-regs nelisp-asm-x86_64--abi-win64-arg-regs))
              ;; Allocate the spill frame in one shot (= deterministic
              ;; byte count regardless of arity, matching pass-1 = pass-2).
              (when (> arity 0)
                (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp frame-bytes))
              ;; Spill each incoming Win64 arg register to its frame slot
              ;; via `mov [rbp - 8*(i+1)], REG'.  disp8 covers [-128,127]
              ;; so slot offsets up to 15 args are in range.
              (let ((i 0))
                (dolist (preg param-regs)
                  (ignore preg)
                  (let ((src-reg (nth i win64-arg-regs))
                        (disp (- (* 8 (1+ i)))))
                    (nelisp-asm-x86_64-mov-mem-reg-disp8
                     buf 'rbp disp src-reg))
                  (setq i (1+ i))))
              ;; Reserve runtime let-rt slots (same as SysV path).
              (when (> rt-slot-count 0)
                (let ((rt-rounded (if (zerop (logand rt-slot-count 1))
                                      rt-slot-count
                                    (1+ rt-slot-count))))
                  (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp (* 8 rt-rounded))))))
           ;; f64 class under Win64: XMM0-XMM3 carry the first 4 float args
           ;; (same xmm reg numbers as SysV, just fewer GP-class args allowed
           ;; concurrently).  The MOVSD spill layout is identical to SysV.
           ((eq param-class 'f64)
            (let* ((arity (length param-regs))
                   (arity-rounded (if (zerop (logand arity 1))
                                      arity
                                    (1+ arity)))
                   (frame-bytes (* 8 arity-rounded))
                   (slot-idx -1))
              (when (> arity 0)
                (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp frame-bytes))
              (dolist (xreg param-regs)
                (setq slot-idx (1+ slot-idx))
                (nelisp-asm-x86_64-movsd-mem-disp8-xmm
                 buf 'rbp (- (* 8 (1+ slot-idx))) xreg))))
           (t
            (signal 'nelisp-phase47-compiler-error
                    (list :unknown-defun-param-class param-class))))
        ;; ---- SysV ABI prologue (unchanged) ----
        (cond
         ;; GP class — for the original six-register surface, keep the
         ;; compact per-param `push reg' prologue.  For Doc 129.7E's
         ;; SysV 7+ param surface, allocate the spill frame explicitly
         ;; and copy register / stack incoming args into the same
         ;; rbp-negative slots that `--emit-ref-load' already reads.
         ;;
         ;; SysV AMD64 requires rsp to be 16-byte aligned at the call
         ;; site.  The function entry has rsp ≡ 8 (mod 16) because the
         ;; caller's `call' pushed an 8-byte return address; the
         ;; standard `push rbp; mov rbp, rsp' brings us to rsp ≡ 0
         ;; (mod 16).  Each subsequent `push' subtracts 8, so after K
         ;; total prologue pushes the residue is (8 - 8*K) mod 16:
         ;;   K=1 (rbp only)         → 0   (no extra spills, aligned)
         ;;   K=2 (rbp + 1 spill)    → 8   (misaligned)
         ;;   K=3 (rbp + 2 spills)   → 0   (aligned)
         ;;   K=4 (rbp + 3 spills)   → 8   (misaligned)
         ;;   ...
         ;; So whenever the spill count (= parameter count) is ODD we
         ;; must add one extra 8-byte gap before the body runs, or the
         ;; first nested `call' (= `--emit-call' / `--emit-extern-call'
         ;; / `--emit-f64-call') lands at rsp ≡ 8 (mod 16) and SIGSEGVs
         ;; inside the callee on the first SSE / movaps instruction.
         ;; The fix mirrors the f64 path's `arity-rounded' trick (line
         ;; 2415 above): an explicit `sub rsp, 8' fixed-width instr
         ;; that the epilogue's `mov rsp, rbp' tears down for free.
         ((eq param-class 'gp)
          (if (cl-some #'consp param-regs)
              (let* ((arity (length param-regs))
                     (rounded (if (zerop (logand arity 1))
                                  arity
                                (1+ arity)))
                     (frame-bytes (* 8 rounded))
                     (i -1))
                (when (> arity 0)
                  (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp frame-bytes))
                (dolist (preg param-regs)
                  (setq i (1+ i))
                  (let ((dst-disp (- (* 8 (1+ i)))))
                    (if (consp preg)
                        (let ((src-disp (+ 16 (* 8 (cadr preg)))))
                          (nelisp-asm-x86_64-mov-reg-mem-disp8
                           buf 'rax 'rbp src-disp)
                          (nelisp-asm-x86_64-mov-mem-reg-disp8
                           buf 'rbp dst-disp 'rax))
                      (nelisp-asm-x86_64-mov-mem-reg-disp8
                       buf 'rbp dst-disp preg)))))
            (dolist (preg param-regs)
              (nelisp-asm-x86_64-push buf preg))
            (when (= 1 (logand (length param-regs) 1))
              (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp 8)))
          ;; Reserve frame slots for runtime `let-rt' bindings.
          ;; Round up to even so the post-prologue rsp stays 16-byte
          ;; aligned (each slot is 8 bytes; 2 slots = 16 bytes).
          (when (> rt-slot-count 0)
            (let ((rt-rounded (if (zerop (logand rt-slot-count 1))
                                  rt-slot-count
                                (1+ rt-slot-count))))
              (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp (* 8 rt-rounded)))))
         ;; f64 class — one bulk `sub rsp, 8*ARITY-ROUNDED', then
         ;; per-param `movsd [rbp - 8*(slot+1)], xmmN'.  ARITY-
         ;; ROUNDED is `arity' rounded up to the next even value
         ;; (= 1→2, 2→2, 3→4, ...) so the post-prologue rsp stays
         ;; 16-byte aligned per SysV AMD64.  The internal CALL
         ;; emitted by `--emit-f64-call' (Doc 110 §3.F) only
         ;; observes a correctly-aligned rsp when this rounding
         ;; happens; arity=1 cases without it would land at rsp ≡
         ;; 8 mod 16 (= ABI violation).  Even rounding wastes 8
         ;; bytes for odd-arity frames; cheap vs the alternative
         ;; of materialising a one-shot pad.
         ((eq param-class 'f64)
          (let* ((arity (length param-regs))
                 (arity-rounded (if (zerop (logand arity 1))
                                    arity
                                  (1+ arity)))
                 (frame-bytes (* 8 arity-rounded))
                 (slot-idx -1))
            (when (> arity 0)
              (nelisp-asm-x86_64--sub-imm32-inline buf 'rsp frame-bytes))
            (dolist (xreg param-regs)
              (setq slot-idx (1+ slot-idx))
              (nelisp-asm-x86_64-movsd-mem-disp8-xmm
               buf 'rbp (- (* 8 (1+ slot-idx))) xreg))))
         (t
          (signal 'nelisp-phase47-compiler-error
                  (list :unknown-defun-param-class param-class)))))
      ;; Body — value walked into rax (gp class) or xmm0 (f64 class).
      (nelisp-phase47-compiler--emit-value body buf)
      ;; Epilogue: deallocate param spill via mov rsp, rbp; pop rbp; ret.
      ;; (Identical for SysV and Win64 — frame pointer teardown is ABI-agnostic.)
      ;; Wave 20: same hand-inline rationale as the prologue above.
      (nelisp-asm-x86_64--mov-reg-reg-inline buf 'rsp 'rbp)
      (nelisp-asm-x86_64--pop-inline buf 'rbp)
      (nelisp-asm-x86_64--ret-inline buf))))

;; ---- §97.6 orchestrator ----

(defun nelisp-phase47-compiler--pass (ir defuns str-offsets rodata-vaddr
                                         &optional table-vaddrs)
  "Run a fresh emit pass returning the buffer.
IR is the parsed program (= main body, with defuns inlined too
but skipped during emit), DEFUNS is the list of defun IR nodes
collected in encounter order, STR-OFFSETS is the dedup table, and
RODATA-VADDR is the absolute vaddr of .rodata (= 0 during pass-1).
The intra-text `call' fixups are *not* resolved here; the caller
finalizes via `resolve-fixups' after measuring.

Doc 49 Wave 11.1: TABLE-VADDRS is an alist `((NAME . VADDR) ...)'
that resolves `static-imm32-table-lookup' name lookups to
absolute virtual addresses.  Bound dynamically as
`nelisp-phase47-compiler--table-vaddrs' so deeply-nested value-
emit dispatchers can read it without plumbing it through every
helper signature.  Pass nil when no static-imm32 tables exist.

Doc 101 §101.B Wave 5: buffer is created with the current
`nelisp-phase47-compiler--abi' so Win64 callers get a 'win64 buffer."
  (let ((nelisp-phase47-compiler--table-vaddrs table-vaddrs)
        (buf (nelisp-asm-x86_64-make-buffer nelisp-phase47-compiler--abi)))
    ;; Main `_start' body first (= the program's entry point).
    (nelisp-phase47-compiler--emit-stmt
     ir buf str-offsets rodata-vaddr)
    ;; Each defun appended after main; their labels become forward-
    ;; resolved by `resolve-fixups'.  The main body must end with a
    ;; syscall (= exit) so execution never falls through into a
    ;; function's prologue; the parser doesn't enforce this beyond
    ;; the legacy v1 demand that the program eventually `exit'.
    (dolist (d defuns)
      (nelisp-phase47-compiler--emit-defun d buf))
    buf))

;;;###autoload
(cl-defun nelisp-phase47-compile-sexp
    (sexp file-path &key (arch 'x86_64) (entry-sym "_start"))
  "Compile SEXP to a static-linked ELF64 executable at FILE-PATH.

SEXP is a Doc 97 v1, 97.b, or 97.c source program (= a single
top-level form).  The accepted grammar is documented in Doc 97
§1, §6.2 and §6.3 — briefly:

  (write \"STRING\")
  (exit VALUE-EXPR)             (compile-time int, or runtime call)
  (seq EXPR...)
  (let ((VAR VAL)) BODY)
  (+ A B) / (- A B) / (* A B)
  (defun NAME (PARAMS...) BODY)
  (NAME ARG ...)                (= call previously-defun'd function)
  (if TEST THEN ELSE)           [§97.c]
  (while TEST BODY...)          [§97.c, returns 0]
  (cond (P1 B1) ... (t Bn))     [§97.c]
  (< A B) / (> A B) / (<= A B) / (>= A B) / (= A B)
  (and EXPR ...) / (or EXPR ...)
  integer / let-bound symbol / function parameter

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
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch arch)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (str-offsets (car collected))
         (str-rodata-bytes (cdr collected))
         ;; Doc 49 Wave 11.1: collect static-imm32 tables; their bytes
         ;; are appended *after* string rodata so existing string
         ;; vaddrs are unchanged (= zero regression for non-table
         ;; programs).  Table offsets are relative to the table sub-
         ;; buffer base, not the overall .rodata buffer.
         (table-collected (nelisp-phase47-compiler--collect-tables ir))
         (table-offsets (car table-collected))
         (table-bytes (cdr table-collected))
         (str-rodata-len (length str-rodata-bytes))
         (rodata-bytes (concat str-rodata-bytes table-bytes))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         ;; Pass 1 placeholder table-vaddrs: every table maps to 0.
         ;; Byte width of the emit sequence is independent of the
         ;; vaddr value (= mov-imm64 is fixed 10 bytes regardless),
         ;; so pass-1 / pass-2 byte invariance holds.
         (pass1-table-vaddrs
          (mapcar (lambda (entry) (cons (car entry) 0)) table-offsets))
         ;; Pass 1: dry size measurement.
         (pass1 (nelisp-phase47-compiler--pass
                 ir defuns str-offsets 0 pass1-table-vaddrs))
         (text-size (nelisp-asm-x86_64-buffer-pos pass1))
         (rodata-vaddr (+ nelisp-phase47-compiler--text-vaddr text-size))
         ;; Resolve each table's absolute vaddr.  Tables live in
         ;; .rodata after the string sub-section, so each table's
         ;; vaddr is `rodata-vaddr + str-rodata-len + table-offset'.
         (table-vaddrs
          (mapcar (lambda (entry)
                    (let* ((name (car entry))
                           (info (cdr entry))
                           (offset (plist-get info :offset)))
                      (cons name (+ rodata-vaddr str-rodata-len offset))))
                  table-offsets))
         ;; Pass 2: real emit with the resolved address.
         (pass2 (nelisp-phase47-compiler--pass
                 ir defuns str-offsets rodata-vaddr table-vaddrs))
         (text-bytes (nelisp-asm-x86_64-resolve-fixups pass2)))
    ;; Pass-1's fixups remain unresolved (= we never inspect them);
    ;; only pass-2's resolved bytes ship to the ELF writer.
    (ignore pass1)
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

;; ---- Doc 99 §99.B: ET_REL emit path (= elisp → .o for C linkage) ----
;;
;; `nelisp-phase47-compile-to-object' is the sibling of `-compile-sexp'
;; that emits a relocatable object instead of a static-linked executable.
;; The defun names become global STT_FUNC symbols callable from C via
;; the SysV AMD64 ABI (= return value in rax).  No `_start' is emitted.
;;
;; Spike scope (= Doc 99 §99.B): defun bodies must NOT reference strings
;; (= no `write' forms) because v1 still bakes rodata vaddrs into pass-2
;; code rather than emitting R_X86_64_PC32 relocations against `.rodata'.
;; Lifting that constraint is Stage 99.C+ work — for §99.B we only need
;; pure-int return values to prove the cargo-build wiring end-to-end.

;;;###autoload
(cl-defun nelisp-phase47-compile-to-object
    (sexp file-path &key (arch 'x86_64) (format 'elf))
  "Compile SEXP (= one or more defuns) to an ET_REL .o at FILE-PATH.

SEXP must be either a single `(defun NAME (PARAMS...) BODY)' form or
a `(seq (defun ...) ...)' wrapping multiple defuns.  Each defun
becomes a GLOBAL STT_FUNC symbol named after the defun (= symbol-name
of the elisp identifier, with underscores preserved for C linkage).

ARCH defaults to `x86_64'.  v1 signals
`nelisp-phase47-compiler-error' for any other ARCH outside
`(x86_64 aarch64)'.

Spike scope: defun bodies must not reference strings.  Signals
`nelisp-phase47-compiler-error' with `:object-mode-no-strings' if
the IR contains a `write' node, because rodata vaddr baking does
not survive linker relocation in v1.

Returns FILE-PATH on success.  Signals on parse error, free symbol
reference, out-of-range integer, or any pass-1/pass-2 byte-length
drift (= a Doc 92 emitter invariant violation)."
  (unless (memq arch '(x86_64 aarch64))
    (signal 'nelisp-phase47-compiler-error
            (list :unsupported-arch arch)))
  (let* ((nelisp-phase47-compiler--label-counter 0)
         (nelisp-phase47-compiler--arch arch)
         (ir (nelisp-phase47-compiler--parse sexp nil))
         (collected (nelisp-phase47-compiler--collect-strings ir))
         (rodata-bytes (cdr collected))
         (defuns (nelisp-phase47-compiler--collect-defuns ir)))
    (unless (zerop (length rodata-bytes))
      (signal 'nelisp-phase47-compiler-error
              (list :object-mode-no-strings
                    :rodata-bytes (length rodata-bytes))))
    (unless defuns
      (signal 'nelisp-phase47-compiler-error
              (list :object-mode-needs-defuns sexp)))
    ;; Validate top-level shape: either a single defun, or a seq of
    ;; defun forms (the parser tolerates seq + main body, but for
    ;; object output we reject anything that would emit a `_start'.)
    ;; A33.3 — integer-tag dispatch (`pcase' arms → `cond' over
    ;; `--ir-kind-tag'); behaviour-preserving, `.o' bytes unchanged.
    (let ((tag (nelisp-phase47-compiler--ir-kind-tag ir)))
     (cond
      ((= tag 21) nil)          ; defun
      ((= tag 54)               ; seq
       (dolist (f (nelisp-phase47-compiler--ir-get ir :forms))
         (unless (eq (nelisp-phase47-compiler--ir-kind f) 'defun)
           (signal 'nelisp-phase47-compiler-error
                   (list :object-mode-non-defun-form f)))))
      (t
       (let ((other (nelisp-phase47-compiler--ir-kind ir)))
         (signal 'nelisp-phase47-compiler-error
                 (list :object-mode-bad-top-form other))))))
    ;; Emit pass: only the defuns, no main `_start' body.  We reuse
    ;; the existing emit-defun helper and call `resolve-fixups' so
    ;; intra-`.text' cross-defun calls bake their rel32 in place
    ;; (= the linker only sees external R_X86_64_PC32 / R_X86_64_PLT32
    ;; entries when we eventually emit them; v1 has none).
    ;;
    ;; Doc 101 §101.B Wave 5: COFF/Windows targets bind --abi to 'win64
    ;; so that --emit-defun / --emit-call use Win64 register conventions.
    (let* ((nelisp-phase47-compiler--abi
            (if (and (eq arch 'x86_64) (eq format 'coff)) 'win64 'sysv))
           (buf (if (eq arch 'aarch64)
                    (nelisp-asm-arm64-make-buffer)
                  (nelisp-asm-x86_64-make-buffer
                   nelisp-phase47-compiler--abi))))
      (dolist (d defuns)
        (nelisp-phase47-compiler--emit-defun d buf))
      (let* ((text-bytes (if (eq arch 'aarch64)
                             (nelisp-asm-arm64-resolve-fixups buf)
                           (nelisp-asm-x86_64-resolve-fixups buf)))
             (labels (if (eq arch 'aarch64)
                         (nelisp-asm-arm64-buffer-labels buf)
                       (nelisp-asm-x86_64-buffer-labels buf)))
             ;; Doc 100 §100.A: extract external relocs (= plt32 ones
             ;; emitted by `--emit-extern-call').  Each surfaced reloc
             ;; must also have a matching SHN_UNDEF symtab entry in
             ;; the output `.o', or the ELF writer's reloc lookup
             ;; loop signals `relocation references unknown symbol'.
             (relocs (if (eq arch 'aarch64)
                         (nelisp-asm-arm64-buffer-relocs buf)
                       (nelisp-asm-x86_64-extract-relocs buf)))
             (extern-names
              (delete-dups
               (mapcar (lambda (r) (plist-get r :symbol))
                       (cl-remove-if-not
                        (lambda (r) (eq (plist-get r :type) 'plt32))
                        relocs))))
             ;; Only the user-defined defun names should appear as
             ;; GLOBAL FUNC symbols.  The control-flow helper labels
             ;; emitted by `--emit-if' / `--emit-while' / `--emit-cond'
             ;; (= `if-N-else' / `while-N-end' / etc.) are purely
             ;; intra-`.text' jump targets resolved in-buffer by
             ;; `resolve-fixups'; exposing them as global symbols
             ;; would pollute the linker's symbol space and risk
             ;; collision if multiple `.o' files happen to pick the
             ;; same label-counter value.
             (exported-names
              (mapcar (lambda (d)
                        (let ((nm (nelisp-phase47-compiler--ir-get d :name)))
                          (if (stringp nm) nm (symbol-name nm))))
                      defuns))
             (label-positions
              (let (acc)
                (dolist (cell labels)
                  (let* ((nm (car cell))
                         (nm-str (if (stringp nm) nm (symbol-name nm))))
                    (when (member nm-str exported-names)
                      (push (cons nm-str (cdr cell)) acc))))
                (sort acc (lambda (a b) (< (cdr a) (cdr b))))))
             (label-size-map
              (let ((pairs label-positions)
                    (acc nil))
                (while pairs
                  (let* ((cur (car pairs))
                         (next (cadr pairs))
                         (start (cdr cur))
                         (end (if next (cdr next) (length text-bytes))))
                    (push (cons (car cur) (- end start)) acc))
                  (setq pairs (cdr pairs)))
                acc))
             (symbols
              (mapcar
               (lambda (cell)
                 (list :name (car cell)
                       :value (cdr cell)
                       :size (or (cdr (assoc (car cell) label-size-map)) 0)
                       :section 'text
                       :bind 'global
                       :type 'func))
               label-positions))
             ;; SHN_UNDEF / STB_GLOBAL / STT_NOTYPE entries for every
             ;; extern symbol the `.text' relocs reference.  Position-
             ;; independent ordering keeps the debug output stable
             ;; across runs; ld doesn't care about symtab order.
             (extern-symbol-plists
              (mapcar (lambda (nm)
                        (list :name nm
                              :value 0
                              :size 0
                              :section 'undef
                              :bind 'global
                              :type 'notype))
                      extern-names))
             (all-symbols (append symbols extern-symbol-plists)))
        (pcase format
          ('elf
           (nelisp-elf-write-binary
            file-path
            (list :e-type 'rel
                  :text text-bytes
                  :symbols all-symbols
                  :relocs relocs
                  :machine arch)))
          ('mach-o
           ;; Doc 100 §100.D Stage 3: macOS uses Mach-O instead of
           ;; ELF.  Reloc surface trimmed because Mach-O writer v1
           ;; does not emit relocation entries — the 12 jit_arith
           ;; trampolines have no external relocs, so this is sound
           ;; for the §100.D Stage 1-3 swap set.
           ;; Wave 3: x86_64 added alongside aarch64.
           (unless (memq arch '(aarch64 x86_64))
             (signal 'nelisp-phase47-compiler-error
                     (list :mach-o-unsupported-arch arch)))
           (when relocs
             (signal 'nelisp-phase47-compiler-error
                     (list :mach-o-no-reloc-support relocs)))
           (require 'nelisp-mach-o-write)
           (nelisp-mach-o-write-binary
            file-path
            (list :text text-bytes
                  :symbols all-symbols
                  :machine arch)))
          ('coff
           ;; Doc 101 §101.A: Windows uses PE32+/COFF instead of ELF.
           ;; Only x86_64 is supported in v1; aarch64 COFF is deferred.
           (unless (eq arch 'x86_64)
             (signal 'nelisp-phase47-compiler-error
                     (list :coff-only-supports-x86_64 arch)))
           (require 'nelisp-pe-write)
           (nelisp-pe-write-binary
            file-path
            (list :text     text-bytes
                  :symbols  all-symbols
                  :relocs   relocs
                  :machine  arch)))
          (other
           (signal 'nelisp-phase47-compiler-error
                   (list :unknown-output-format other))))
        file-path))))

(provide 'nelisp-phase47-compiler)

;;; nelisp-phase47-compiler.el ends here
