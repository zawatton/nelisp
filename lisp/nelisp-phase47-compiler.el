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
;; Not wired into nelisp-baker.

;;; Code:

(require 'cl-lib)
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
Doc 97.b supports up to 6 args; stack spill for 7+ args is
deferred to Doc 97.c.")

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

(defun nelisp-phase47-compiler--gensym (prefix)
  "Return a fresh symbol `PREFIX-N' for control-flow labelling.
N is the next value of `nelisp-phase47-compiler--label-counter'.
The result is interned (= comparable with `eq') so it slots
straight into Doc 92's labels alist."
  (let ((id (cl-incf nelisp-phase47-compiler--label-counter)))
    (intern (format "%s-%d" prefix id))))

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
    (list :kind 'imm
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
        (list :kind 'ref :var sexp
              :reg (plist-get info :reg)
              :slot (plist-get info :slot)
              :class (or (plist-get info :class) 'gp)))))
   ;; Arithmetic with at least one non-constant operand.  Doc 100
   ;; §100.D extends the op set with 3 bitwise binops (logior /
   ;; logand / logxor) for the `nl_jit_arith_log*' swap; they share
   ;; the same MR-form emit shape so no new IR kind is needed.
   ((and (consp sexp) (memq (car sexp) '(+ - * logior logand logxor)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :arith-arity (car sexp) sexp)))
    (list :kind 'arith
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
    (list :kind 'f64-binop
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
    (list :kind 'f64-cmp
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
    (list :kind 'shift
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
    (list :kind 'cmp
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
    (list :kind 'if
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
      (list :kind 'while
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
        (list :kind 'cond
              :id (nelisp-phase47-compiler--gensym "cond")
              :clauses clauses))))
   ;; (and EXPR ...) / (or EXPR ...) short-circuit.
   ((and (consp sexp) (memq (car sexp) '(and or)))
    (let ((op (car sexp))
          (args (cdr sexp)))
      (when (null args)
        (signal 'nelisp-phase47-compiler-error
                (list :logic-empty sexp)))
      (list :kind 'logic
            :op op
            :id (nelisp-phase47-compiler--gensym (symbol-name op))
            :forms (mapcar (lambda (e)
                             (nelisp-phase47-compiler--parse-value
                              e env fenv defuns))
                           args))))
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
   ((and (consp sexp) (eq (car sexp) 'sexp-tag))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-tag-arity sexp)))
    (list :kind 'sexp-tag
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-int-unwrap))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-int-unwrap-arity sexp)))
    (list :kind 'sexp-int-unwrap
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-int-make))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-int-make-arity sexp)))
    (list :kind 'sexp-int-make
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)
          :val (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ;; ---- Doc 101 §101.B Cons read ops ----
   ((and (consp sexp) (eq (car sexp) 'cons-null-p))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-null-p-arity sexp)))
    (list :kind 'cons-null-p
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-car))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-car-arity sexp)))
    (list :kind 'cons-car
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-cdr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-cdr-arity sexp)))
    (list :kind 'cons-cdr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (memq (car sexp) '(cons-cdr-raw cons-cdr-raw-from-box)))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-cdr-raw-arity sexp)))
    (list :kind 'cons-cdr-raw
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :from-box (eq (car sexp) 'cons-cdr-raw-from-box)))
   ((and (consp sexp) (eq (car sexp) 'sexp-payload-ptr))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-payload-ptr-arity sexp)))
    (list :kind 'sexp-payload-ptr
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
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
    (list :kind 'str-len
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-bytes))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :str-bytes-arity sexp)))
    (list :kind 'str-bytes
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-byte-at))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :str-byte-at-arity sexp)))
    (list :kind 'str-byte-at
          :ptr (nelisp-phase47-compiler--parse-value
                (nth 1 sexp) env fenv defuns)
          :idx (nelisp-phase47-compiler--parse-value
                (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'str-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :str-eq-arity sexp)))
    (list :kind 'str-eq
          :id (nelisp-phase47-compiler--gensym "str-eq")
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'symbol-eq))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :symbol-eq-arity sexp)))
    (list :kind 'symbol-eq
          :id (nelisp-phase47-compiler--gensym "symbol-eq")
          :a (nelisp-phase47-compiler--parse-value
              (nth 1 sexp) env fenv defuns)
          :b (nelisp-phase47-compiler--parse-value
              (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-write-nil))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-nil-arity sexp)))
    (list :kind 'sexp-write-nil
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'sexp-write-t))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :sexp-write-t-arity sexp)))
    (list :kind 'sexp-write-t
          :slot (nelisp-phase47-compiler--parse-value
                 (nth 1 sexp) env fenv defuns)))
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
    (list :kind 'cons-make
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
    (list :kind 'cons-set-car
          :handle (nelisp-phase47-compiler--parse-value
                   (nth 1 sexp) env fenv defuns)
          :val-ptr (nelisp-phase47-compiler--parse-value
                    (nth 2 sexp) env fenv defuns)))
   ((and (consp sexp) (eq (car sexp) 'cons-set-cdr))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :cons-set-cdr-arity sexp)))
    (list :kind 'cons-set-cdr
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
      (list :kind 'f64-call
            :name name
            :arg (nelisp-phase47-compiler--parse-value
                  arg env fenv defuns))))
   ;; (extern-call SYM ARG...) — Doc 100 §100.A call into a C-callable
   ;; extern symbol.  SYM becomes an SHN_UNDEF entry in the output .o's
   ;; symtab; the rel32 placeholder gets an R_X86_64_PLT32 relocation
   ;; that the linker resolves at static-link time against another
   ;; object (typically a Rust `.rlib' exporting `#[no_mangle] pub
   ;; extern "C"' helpers).  Up to 6 args, all i64-shaped.
   ((and (consp sexp) (eq (car sexp) 'extern-call))
    (when (< (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :extern-call-needs-symbol sexp)))
    (let* ((name (nth 1 sexp))
           (args (nthcdr 2 sexp)))
      (unless (symbolp name)
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-name-not-symbol name)))
      (when (> (length args) (length nelisp-phase47-compiler--arg-regs))
        (signal 'nelisp-phase47-compiler-error
                (list :extern-call-too-many-args name (length args))))
      (list :kind 'extern-call
            :name name
            :args (mapcar (lambda (a)
                            (nelisp-phase47-compiler--parse-value
                             a env fenv defuns))
                          args))))
   ;; Function call (= head is a defined function name).
   ((and (consp sexp) (symbolp (car sexp))
         (assq (car sexp) defuns))
    (let* ((name (car sexp))
           (arity (cdr (assq name defuns)))
           (args (cdr sexp)))
      (unless (= (length args) arity)
        (signal 'nelisp-phase47-compiler-error
                (list :call-arity-mismatch name
                      :expected arity :got (length args))))
      (when (> arity (length nelisp-phase47-compiler--arg-regs))
        (signal 'nelisp-phase47-compiler-error
                (list :too-many-args name arity)))
      (list :kind 'call
            :name name
            :args (mapcar (lambda (a)
                            (nelisp-phase47-compiler--parse-value
                             a env fenv defuns))
                          args))))
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
      (list :kind 'write :str arg)))
   ;; (exit VALUE-EXPR)
   ((and (consp sexp) (eq (car sexp) 'exit))
    (unless (= (length sexp) 2)
      (signal 'nelisp-phase47-compiler-error
              (list :exit-arity sexp)))
    (let* ((arg (nth 1 sexp))
           (vnode
            (nelisp-phase47-compiler--parse-value arg env fenv defuns)))
      (when (eq (plist-get vnode :kind) 'imm)
        (let ((n (plist-get vnode :value)))
          (unless (and (integerp n) (<= 0 n 255))
            (signal 'nelisp-phase47-compiler-error
                    (list :status-out-of-range n)))))
      (list :kind 'exit :value vnode)))
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
                    (cons (cons nm (length ps)) cur-defuns)))))
        (dolist (c children)
          (push (nelisp-phase47-compiler--parse-stmt
                 c env fenv cur-defuns)
                acc))
        (list :kind 'seq :forms (nreverse acc)))))
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
        (unless (nelisp-phase47-compiler--int-foldable-p
                 val-sexp env fenv)
          (signal 'nelisp-phase47-compiler-error
                  (list :let-non-const val-sexp)))
        (let* ((val (nelisp-phase47-compiler--fold-int val-sexp env))
               (new-env (cons (cons var val) env))
               (body-ir (nelisp-phase47-compiler--parse-stmt
                         body new-env fenv defuns)))
          (list :kind 'let :var var :value val :body body-ir)))))
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
           (param-forms (nth 2 sexp))
           (body (nth 3 sexp))
           (arity (length param-forms))
           ;; Extract (sym . class) pairs.  Class is `gp' for bare
           ;; symbols (= legacy / i64) and `f64' for `(SYM :type f64)'.
           (param-pairs
            (mapcar
             (lambda (p)
               (cond
                ((symbolp p) (cons p 'gp))
                ((and (consp p) (= (length p) 3)
                      (symbolp (car p))
                      (eq (nth 1 p) :type)
                      (memq (nth 2 p) '(gp f64)))
                 (cons (car p) (nth 2 p)))
                (t (signal 'nelisp-phase47-compiler-error
                           (list :defun-param-shape p)))))
             param-forms))
           (params (mapcar #'car param-pairs))
           (classes (mapcar #'cdr param-pairs))
           (uniform-class (car classes)))
      (unless (cl-every (lambda (c) (eq c uniform-class)) classes)
        (signal 'nelisp-phase47-compiler-error
                (list :defun-mixed-param-classes name classes)))
      (let* ((max-arity
              (length (if (eq uniform-class 'f64)
                          nelisp-phase47-compiler--xmm-arg-regs
                        nelisp-phase47-compiler--arg-regs))))
        (when (> arity max-arity)
          (signal 'nelisp-phase47-compiler-error
                  (list :defun-too-many-params name arity uniform-class))))
      (let* ((reg-pool (if (eq uniform-class 'f64)
                           nelisp-phase47-compiler--xmm-arg-regs
                         nelisp-phase47-compiler--arg-regs))
             (param-regs (cl-subseq reg-pool 0 arity))
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
                   (cons p (list :reg r :slot idx :class uniform-class)))
                 params param-regs)))
             ;; Body is a value-producing expression (= implicit return).
             (body-ir (nelisp-phase47-compiler--parse-value
                       body env new-fenv defuns)))
        (list :kind 'defun
              :name name
              :params params
              :param-regs param-regs
              :param-class uniform-class
              :body body-ir))))
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
   (t
    (signal 'nelisp-phase47-compiler-error
            (list :unknown-form sexp)))))

(defun nelisp-phase47-compiler--parse (sexp &optional env)
  "Public parser entry: parse SEXP into a top-level IR node.
ENV is the optional let-environment for testing helpers."
  (nelisp-phase47-compiler--parse-stmt sexp env nil nil))

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
               ('exit (walk (plist-get node :value)))
               ('seq
                (mapc #'walk (plist-get node :forms)))
               ('let
                (walk (plist-get node :body)))
               ('defun
                (walk (plist-get node :body)))
               ('arith
                (walk (plist-get node :a))
                (walk (plist-get node :b)))
               ('shift
                (walk (plist-get node :a))
                (walk (plist-get node :b)))
               ('call
                (mapc #'walk (plist-get node :args)))
               ('cmp
                (walk (plist-get node :a))
                (walk (plist-get node :b)))
               ('if
                (walk (plist-get node :test))
                (walk (plist-get node :then))
                (walk (plist-get node :else)))
               ('while
                (walk (plist-get node :test))
                (mapc #'walk (plist-get node :body)))
               ('cond
                (dolist (cl (plist-get node :clauses))
                  (unless (eq (car cl) 'always)
                    (walk (car cl)))
                  (walk (cdr cl))))
               ('logic
                (mapc #'walk (plist-get node :forms)))
               (_ nil)))))
      (walk ir))
    (cons offsets rodata)))

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
             (pcase (plist-get node :kind)
               ('defun (push node acc))
               ('seq (mapc #'walk (plist-get node :forms)))
               ('let (walk (plist-get node :body)))
               (_ nil)))))
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
  (unless (and (integerp slot) (<= 0 slot 5))
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
  (let* ((op (plist-get node :op))
         (a (plist-get node :a))
         (b (plist-get node :b))
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
  (let ((name (plist-get node :name))
        (arg (plist-get node :arg))
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
  (let ((kind (plist-get node :kind)))
    (cond
     ((and (eq kind 'ref) (eq (plist-get node :class) 'f64))
      (nelisp-phase47-compiler--emit-f64-ref-load
       buf (plist-get node :slot) xmm-dst))
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
  (let* ((op (plist-get node :op))
         (a (plist-get node :a))
         (b (plist-get node :b))
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
      (pcase (plist-get node :kind)
        ('imm
         (nelisp-asm-arm64-mov-imm64 buf 'x0 (plist-get node :value)))
        ('ref
         ;; GP class → LDUR Xn (existing path); f64 class → LDUR Dn
         ;; into d0 (= default destination for top-level body ref).
         (if (eq (plist-get node :class) 'f64)
             (nelisp-phase47-compiler--emit-f64-ref-load
              buf (plist-get node :slot) 'd0)
           (nelisp-phase47-compiler--emit-ref-load
            buf (plist-get node :slot))))
        ('arith
         (nelisp-phase47-compiler--emit-arith node buf))
        ('shift
         (nelisp-phase47-compiler--emit-shift node buf))
        ('cmp
         (nelisp-phase47-compiler--emit-cmp node buf))
        ('if
         (nelisp-phase47-compiler--emit-if node buf))
        ('f64-binop
         (nelisp-phase47-compiler--emit-f64-binop node buf))
        ('f64-cmp
         (nelisp-phase47-compiler--emit-f64-cmp node buf))
        ('f64-call
         (nelisp-phase47-compiler--emit-f64-call node buf))
        ((or 'call 'extern-call 'sexp-tag 'sexp-int-unwrap 'sexp-int-make
             'cons-null-p 'cons-car 'cons-cdr 'cons-cdr-raw
             'sexp-payload-ptr
             'str-len 'str-bytes 'str-byte-at 'str-eq 'symbol-eq
             'sexp-write-nil 'sexp-write-t
             'cons-make 'cons-set-car 'cons-set-cdr
             'while 'cond 'logic)
         (nelisp-phase47-compiler--emit-aarch64-unsupported
          (plist-get node :kind) node))
        (kind
         (signal 'nelisp-phase47-compiler-error
                 (list :unknown-value-kind kind))))
    (pcase (plist-get node :kind)
      ('imm
       ;; mov rax, imm32                                = 7 bytes
       (nelisp-asm-x86_64-mov-imm32 buf 'rax (plist-get node :value)))
      ('ref
       ;; gp class → `mov rax, [rbp - 8*(slot+1)]'; f64 class →
       ;; `movsd xmm0, [rbp - 8*(slot+1)]'.  Both read the spilled
       ;; param from the frame slot allocated by the prologue;
       ;; `:class' on the ref node dispatches the instruction.
       (if (eq (plist-get node :class) 'f64)
           (nelisp-phase47-compiler--emit-f64-ref-load
            buf (plist-get node :slot) 'xmm0)
         (nelisp-phase47-compiler--emit-ref-load
          buf (plist-get node :slot))))
      ('f64-binop
       (nelisp-phase47-compiler--emit-f64-binop node buf))
      ('f64-cmp
       (nelisp-phase47-compiler--emit-f64-cmp node buf))
      ('f64-call
       (nelisp-phase47-compiler--emit-f64-call node buf))
      ('arith
       (nelisp-phase47-compiler--emit-arith node buf))
      ('shift
       (nelisp-phase47-compiler--emit-shift node buf))
      ('call
       (nelisp-phase47-compiler--emit-call node buf))
      ('extern-call
       (nelisp-phase47-compiler--emit-extern-call node buf))
      ('sexp-tag
       (nelisp-phase47-compiler--emit-sexp-tag node buf))
      ('sexp-int-unwrap
       (nelisp-phase47-compiler--emit-sexp-int-unwrap node buf))
      ('sexp-int-make
       (nelisp-phase47-compiler--emit-sexp-int-make node buf))
      ('cons-null-p
       (nelisp-phase47-compiler--emit-cons-null-p node buf))
      ('cons-car
       (nelisp-phase47-compiler--emit-cons-slot-copy
        node buf nelisp-nlconsbox--offset-car))
      ('cons-cdr
       (nelisp-phase47-compiler--emit-cons-slot-copy
        node buf nelisp-nlconsbox--offset-cdr))
      ('cons-cdr-raw
       (nelisp-phase47-compiler--emit-cons-cdr-raw node buf))
      ('sexp-payload-ptr
       (nelisp-phase47-compiler--emit-sexp-payload-ptr node buf))
      ('str-len
       (nelisp-phase47-compiler--emit-str-len node buf))
      ('str-bytes
       (nelisp-phase47-compiler--emit-str-bytes node buf))
      ('str-byte-at
       (nelisp-phase47-compiler--emit-str-byte-at node buf))
      ('str-eq
       (nelisp-phase47-compiler--emit-str-eq node buf))
      ('symbol-eq
       (nelisp-phase47-compiler--emit-symbol-eq node buf))
      ('sexp-write-nil
       (nelisp-phase47-compiler--emit-sexp-write-tag
        node buf nelisp-sexp--tag-nil))
      ('sexp-write-t
       (nelisp-phase47-compiler--emit-sexp-write-tag
        node buf nelisp-sexp--tag-t))
      ('cons-make
       (nelisp-phase47-compiler--emit-cons-make node buf))
      ('cons-set-car
       (nelisp-phase47-compiler--emit-cons-set-slot
        node buf 'nl_consbox_set_car))
      ('cons-set-cdr
       (nelisp-phase47-compiler--emit-cons-set-slot
        node buf 'nl_consbox_set_cdr))
      ('cmp
       (nelisp-phase47-compiler--emit-cmp node buf))
      ('if
       (nelisp-phase47-compiler--emit-if node buf))
      ('while
       (nelisp-phase47-compiler--emit-while node buf))
      ('cond
       (nelisp-phase47-compiler--emit-cond node buf))
      ('logic
       (nelisp-phase47-compiler--emit-logic node buf))
      (kind
       (signal 'nelisp-phase47-compiler-error
               (list :unknown-value-kind kind))))))

(defun nelisp-phase47-compiler--emit-arith (node buf)
  "Emit a runtime arithmetic op, result in rax.
Strategy: evaluate B into rax, push, evaluate A into rax, pop into
r10, then OP rax, r10.  Push/pop are byte-fixed so pass invariance
holds.  r10 is caller-saved per SysV AND not in the arg-reg list so
  the scratch never aliases a parameter register (= the bug seen in
chained calls where rcx held both `d' param and a scratch value)."
  (let ((op (plist-get node :op))
        (a (plist-get node :a))
        (b (plist-get node :b)))
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
  (let ((op (plist-get node :op))
        (a (plist-get node :a))
        (b (plist-get node :b)))
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

(defun nelisp-phase47-compiler--emit-call (node buf)
  "Emit a SysV AMD64 call to NODE's named function.
Strategy:
  1. Evaluate each arg into rax then push rax (= reverse order so
     first arg sits on top of the saved stack).
  2. Pop each arg into its assigned register (rdi, rsi, rdx, rcx,
     r8, r9) so concurrent args don't clobber each other.
  3. call <NAME> (intra-text rel32 fixup).
  4. Return value already in rax."
  (let* ((name (plist-get node :name))
         (args (plist-get node :args))
         (n (length args))
         (regs (cl-subseq nelisp-phase47-compiler--arg-regs 0 n)))
    ;; Push each evaluated arg.
    (dolist (a args)
      (nelisp-phase47-compiler--emit-value a buf)
      (nelisp-asm-x86_64-push buf 'rax))
    ;; Pop into arg-regs in reverse (= last pushed is first popped, =
    ;; the last arg; so iterate regs reversed).
    (dolist (r (reverse regs))
      (nelisp-asm-x86_64-pop buf r))
    ;; call <NAME> — intra-text rel32 resolved at finalize time.
    (nelisp-asm-x86_64-call-rel32 buf name)))

(defun nelisp-phase47-compiler--emit-extern-call (node buf)
  "Emit a SysV AMD64 call to an extern symbol NODE (= Doc 100 §100.A).
Same strategy as `--emit-call' but emits the `call' opcode bytes
directly + records a `plt32' reloc against an external symbol
instead of an intra-text label fixup.  The ELF writer's
`.rela.text' machinery surfaces the reloc for `ld' to resolve
against the final linked binary (= typically a Rust `.rlib'
exporting matching `#[no_mangle] pub extern \"C\"' helpers).

Unlike `--emit-call' the target name is NOT validated against the
compile-time defuns alist; the parser already accepted SYM as a
bare symbol literal under `(extern-call SYM ...)'.  Out-of-budget
args are rejected at parse time."
  (let* ((name (plist-get node :name))
         (args (plist-get node :args))
         (n (length args))
         (regs (cl-subseq nelisp-phase47-compiler--arg-regs 0 n)))
    ;; Push each evaluated arg.
    (dolist (a args)
      (nelisp-phase47-compiler--emit-value a buf)
      (nelisp-asm-x86_64-push buf 'rax))
    ;; Pop into arg-regs in reverse (= last pushed first popped).
    (dolist (r (reverse regs))
      (nelisp-asm-x86_64-pop buf r))
    ;; Emit the `call rel32' opcode (0xE8) + 4-byte zero placeholder
    ;; + record a PLT32 reloc at the placeholder offset.  Section is
    ;; `text' (default) since we are inside an `.text' defun body.
    (nelisp-asm-x86_64-emit-bytes buf (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-plt32-here
     buf (symbol-name name) -4 'text)))

;; ---- Doc 100 v2 §100.B Sexp ABI direct-access emit ----

(defun nelisp-phase47-compiler--emit-sexp-tag (node buf)
  "Emit `movzx rax, byte ptr [rdi]' after computing NODE's :ptr into rdi.
Result: the tag byte at offset `nelisp-sexp--offset-tag' (= 0)
zero-extended to a 64-bit value in rax.  See `docs/arch/sexp-abi.md'
§5.1."
  (let ((ptr (plist-get node :ptr)))
    ;; Compute :ptr into rax, then move into rdi as the base register.
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-movzx-reg-byte-mem buf 'rax 'rdi)))

(defun nelisp-phase47-compiler--emit-sexp-int-unwrap (node buf)
  "Emit `mov rax, qword ptr [rdi + 8]' after computing NODE's :ptr into rdi.
Result: the i64 payload of a `Sexp::Int(n)' value, read from offset
`nelisp-sexp--offset-int-payload' (= 8).  No tag check — caller
must ensure :ptr points at a `Sexp::Int' variant.  See
`docs/arch/sexp-abi.md' §5.2."
  (let ((ptr (plist-get node :ptr)))
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
  (let ((slot (plist-get node :slot))
        (val (plist-get node :val)))
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
  (let ((ptr (plist-get node :ptr)))
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
  (let ((ptr (plist-get node :ptr))
        (slot (plist-get node :slot)))
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
  (let* ((ptr (plist-get node :ptr))
         (from-box (plist-get node :from-box))
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
  (let* ((ptr (plist-get node :ptr))
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

;; ---- Doc 101 §101.C Symbol/Str read ops emit ----

(defun nelisp-phase47-compiler--emit-str-len (node buf)
  "Emit `mov rax, qword ptr [rdi + 24]' after computing NODE's :ptr."
  (let ((ptr (plist-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-length)))

(defun nelisp-phase47-compiler--emit-str-bytes (node buf)
  "Emit `mov rax, qword ptr [rdi + 8]' after computing NODE's :ptr."
  (let ((ptr (plist-get node :ptr)))
    (nelisp-phase47-compiler--emit-value ptr buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-reg-mem-disp8
     buf 'rax 'rdi nelisp-string--offset-ptr)))

(defun nelisp-phase47-compiler--emit-str-byte-at (node buf)
  "Emit byte load from a `Sexp::Str' / `Sexp::Symbol' String buffer.
Result: the selected UTF-8 byte zero-extended into rax."
  (let ((ptr (plist-get node :ptr))
        (idx (plist-get node :idx)))
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
  (let ((a (plist-get node :a))
        (b (plist-get node :b))
        (id (plist-get node :id)))
    (nelisp-phase47-compiler--emit-value a buf)
    (nelisp-asm-x86_64-push buf 'rax)
    (nelisp-phase47-compiler--emit-value b buf)
    (nelisp-asm-x86_64-push buf 'rax)
    ;; last pushed (= b) → rsi, first pushed (= a) → rdi
    (nelisp-asm-x86_64-pop buf 'rsi)
    (nelisp-asm-x86_64-pop buf 'rdi)
    (nelisp-phase47-compiler--emit-string-eq-core buf 'rdi 'rsi id)))

(defun nelisp-phase47-compiler--emit-symbol-eq (node buf)
  "Emit `symbol-eq': tag-check both inputs, then compare name bytes."
  (let ((a (plist-get node :a))
        (b (plist-get node :b))
        (id (plist-get node :id))
        (tag-false-lbl (intern (format "%s-tag-false" (plist-get node :id))))
        (end-lbl (intern (format "%s-tag-end" (plist-get node :id)))))
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
  (let ((slot (plist-get node :slot)))
    (nelisp-phase47-compiler--emit-value slot buf)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
    (nelisp-asm-x86_64-mov-mem-imm8 buf 'rdi tag)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rdi)))

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
  (let ((car-ptr (plist-get node :car-ptr))
        (cdr-ptr (plist-get node :cdr-ptr))
        (slot (plist-get node :slot)))
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

(defun nelisp-phase47-compiler--emit-cons-set-slot (node buf helper-name)
  "Emit `cons-set-car' / `cons-set-cdr' via Rust HELPER-NAME.
NODE carries `:handle' (= `*const Sexp' pointing at a Cons slot) and
`:val-ptr' (= `*const Sexp').  The helper does the drop-then-write
mutation on the boxed field; this op only resolves the `NlConsBox*'
payload from the handle and forwards the two pointers.  Returns the
original handle pointer in rax."
  (let ((handle (plist-get node :handle))
        (val-ptr (plist-get node :val-ptr)))
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
  (let* ((op (plist-get node :op))
         (a (plist-get node :a))
         (b (plist-get node :b))
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
  (let* ((id (plist-get node :id))
         (else-lbl (intern (format "%s-else" id)))
         (end-lbl (intern (format "%s-end" id))))
    (if (eq nelisp-phase47-compiler--arch 'aarch64)
        (progn
          (nelisp-phase47-compiler--emit-value (plist-get node :test) buf)
          (nelisp-asm-arm64-cmp-reg-reg buf 'x0 'xzr)
          (nelisp-asm-arm64-b-cond buf 'eq else-lbl)
          (nelisp-phase47-compiler--emit-value (plist-get node :then) buf)
          (nelisp-asm-arm64-b buf end-lbl)
          (nelisp-asm-arm64-define-label buf else-lbl)
          (nelisp-phase47-compiler--emit-value (plist-get node :else) buf)
          (nelisp-asm-arm64-define-label buf end-lbl))
      (nelisp-phase47-compiler--emit-value (plist-get node :test) buf)
      (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
      (nelisp-asm-x86_64-jz-rel32 buf else-lbl)
      (nelisp-phase47-compiler--emit-value (plist-get node :then) buf)
      (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
      (nelisp-asm-x86_64-define-label buf else-lbl)
      (nelisp-phase47-compiler--emit-value (plist-get node :else) buf)
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
  (let* ((id (plist-get node :id))
         (start-lbl (intern (format "%s-start" id)))
         (end-lbl (intern (format "%s-end" id))))
    (nelisp-asm-x86_64-define-label buf start-lbl)
    (nelisp-phase47-compiler--emit-value (plist-get node :test) buf)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jz-rel32 buf end-lbl)
    (dolist (form (plist-get node :body))
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
  (let* ((id (plist-get node :id))
         (end-lbl (intern (format "%s-end" id)))
         (clauses (plist-get node :clauses))
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
  (let* ((id (plist-get node :id))
         (op (plist-get node :op))
         (end-lbl (intern (format "%s-end" id)))
         (forms (plist-get node :forms)))
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
  (pcase (plist-get value-node :kind)
    ('imm
     (let ((status (plist-get value-node :value)))
       (nelisp-asm-x86_64-mov-imm32 buf 'rax 60)
       (nelisp-asm-x86_64-mov-imm32 buf 'rdi status)
       (nelisp-asm-x86_64-syscall buf)))
    (_
     ;; Compute value into rax (= might call functions).
     (nelisp-phase47-compiler--emit-value value-node buf)
     ;; mov rdi, rax (= exit status from computed value).
     (nelisp-asm-x86_64-mov-reg-reg buf 'rdi 'rax)
     ;; mov rax, 60 (SYS_exit).
     (nelisp-asm-x86_64-mov-imm32 buf 'rax 60)
     (nelisp-asm-x86_64-syscall buf))))

(defun nelisp-phase47-compiler--emit-stmt (ir buf str-offsets rodata-vaddr)
  "Walk statement IR appending instructions to BUF.
STR-OFFSETS maps literal string -> (:offset N :len L).
RODATA-VADDR is the absolute vaddr of byte 0 of .rodata (= 0 during
pass-1 sizing, real value during pass-2).  `defun' nodes are
skipped here — they're emitted separately by the orchestrator."
  (when ir
    (pcase (plist-get ir :kind)
      ('write
       (nelisp-phase47-compiler--emit-write
        buf (plist-get ir :str) str-offsets rodata-vaddr))
      ('exit
       (nelisp-phase47-compiler--emit-exit
        buf (plist-get ir :value)))
      ('seq
       (dolist (child (plist-get ir :forms))
         (nelisp-phase47-compiler--emit-stmt
          child buf str-offsets rodata-vaddr)))
      ('let
       (nelisp-phase47-compiler--emit-stmt
        (plist-get ir :body) buf str-offsets rodata-vaddr))
      ('defun
       ;; Skip — handled by `--emit-defun' separately.
       nil)
      ('call
       ;; Statement-context call discards rax.
       (nelisp-phase47-compiler--emit-call ir buf))
      ((or 'if 'while 'cond 'logic 'cmp 'arith 'shift)
       ;; §97.c: value-producing control-flow / comparison form
       ;; reached statement position (= `seq' child, top-level).
       ;; Emit the value compute; rax is discarded by the
       ;; surrounding context.
       (nelisp-phase47-compiler--emit-value ir buf))
      (kind
       (signal 'nelisp-phase47-compiler-error
               (list :unknown-ir-kind kind))))))

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
  (let* ((name (plist-get defun-ir :name))
         (param-regs (plist-get defun-ir :param-regs))
         (param-class (or (plist-get defun-ir :param-class) 'gp))
         (body (plist-get defun-ir :body)))
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
      (nelisp-asm-x86_64-define-label buf name)
      ;; Prologue: push rbp; mov rbp, rsp; spill each param reg.
      (nelisp-asm-x86_64-push buf 'rbp)
      (nelisp-asm-x86_64-mov-reg-reg buf 'rbp 'rsp)
      (cond
       ;; GP class — existing per-param `push reg' path.
       ((eq param-class 'gp)
        (dolist (preg param-regs)
          (nelisp-asm-x86_64-push buf preg)))
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
            (nelisp-asm-x86_64-sub-imm32 buf 'rsp frame-bytes))
          (dolist (xreg param-regs)
            (setq slot-idx (1+ slot-idx))
            (nelisp-asm-x86_64-movsd-mem-disp8-xmm
             buf 'rbp (- (* 8 (1+ slot-idx))) xreg))))
       (t
        (signal 'nelisp-phase47-compiler-error
                (list :unknown-defun-param-class param-class))))
      ;; Body — value walked into rax (gp class) or xmm0 (f64 class).
      (nelisp-phase47-compiler--emit-value body buf)
      ;; Epilogue: deallocate param spill via mov rsp, rbp; pop rbp; ret.
      (nelisp-asm-x86_64-mov-reg-reg buf 'rsp 'rbp)
      (nelisp-asm-x86_64-pop buf 'rbp)
      (nelisp-asm-x86_64-ret buf))))

;; ---- §97.6 orchestrator ----

(defun nelisp-phase47-compiler--pass (ir defuns str-offsets rodata-vaddr)
  "Run a fresh emit pass returning the buffer.
IR is the parsed program (= main body, with defuns inlined too
but skipped during emit), DEFUNS is the list of defun IR nodes
collected in encounter order, STR-OFFSETS is the dedup table, and
RODATA-VADDR is the absolute vaddr of .rodata (= 0 during pass-1).
The intra-text `call' fixups are *not* resolved here; the caller
finalizes via `resolve-fixups' after measuring."
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
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
         (rodata-bytes (cdr collected))
         (defuns (nelisp-phase47-compiler--collect-defuns ir))
         ;; Pass 1: dry size measurement.
         (pass1 (nelisp-phase47-compiler--pass
                 ir defuns str-offsets 0))
         (text-size (nelisp-asm-x86_64-buffer-pos pass1))
         (rodata-vaddr (+ nelisp-phase47-compiler--text-vaddr text-size))
         ;; Pass 2: real emit with the resolved address.
         (pass2 (nelisp-phase47-compiler--pass
                 ir defuns str-offsets rodata-vaddr))
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
    (pcase (plist-get ir :kind)
      ('defun nil)
      ('seq
       (dolist (f (plist-get ir :forms))
         (unless (eq (plist-get f :kind) 'defun)
           (signal 'nelisp-phase47-compiler-error
                   (list :object-mode-non-defun-form f)))))
      (other
       (signal 'nelisp-phase47-compiler-error
               (list :object-mode-bad-top-form other))))
    ;; Emit pass: only the defuns, no main `_start' body.  We reuse
    ;; the existing emit-defun helper and call `resolve-fixups' so
    ;; intra-`.text' cross-defun calls bake their rel32 in place
    ;; (= the linker only sees external R_X86_64_PC32 / R_X86_64_PLT32
    ;; entries when we eventually emit them; v1 has none).
    (let* ((buf (if (eq arch 'aarch64)
                    (nelisp-asm-arm64-make-buffer)
                  (nelisp-asm-x86_64-make-buffer))))
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
                        (let ((nm (plist-get d :name)))
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
           (unless (eq arch 'aarch64)
             (signal 'nelisp-phase47-compiler-error
                     (list :mach-o-only-supports-aarch64 arch)))
           (when relocs
             (signal 'nelisp-phase47-compiler-error
                     (list :mach-o-no-reloc-support relocs)))
           (require 'nelisp-mach-o-write)
           (nelisp-mach-o-write-binary
            file-path
            (list :text text-bytes
                  :symbols all-symbols
                  :machine arch)))
          (other
           (signal 'nelisp-phase47-compiler-error
                   (list :unknown-output-format other))))
        file-path))))

(provide 'nelisp-phase47-compiler)

;;; nelisp-phase47-compiler.el ends here
