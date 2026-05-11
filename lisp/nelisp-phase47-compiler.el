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

(defconst nelisp-phase47-compiler--arg-regs
  '(rdi rsi rdx rcx r8 r9)
  "SysV AMD64 integer argument registers (= positional, 1..6).
Doc 97.b supports up to 6 args; stack spill for 7+ args is
deferred to Doc 97.c.")

(defvar nelisp-phase47-compiler--label-counter 0
  "Parse-time monotonic counter for control-flow label uniqueness.
Bound fresh to 0 by `nelisp-phase47-compile-sexp' before parsing
each top-level form so the generated label names are reproducible
within one compile but never collide between control-flow nodes.")

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
      ;; FENV cell is `(sym . (:reg R :slot S))' so the ref node
      ;; carries both the incoming arg-reg (= used at call sites
      ;; for documentation) and the rbp-relative spill slot (=
      ;; used by `--emit-value' to read the stable copy).
      (let ((info (cdr pcell)))
        (list :kind 'ref :var sexp
              :reg (plist-get info :reg)
              :slot (plist-get info :slot)))))
   ;; Arithmetic with at least one non-constant operand.
   ((and (consp sexp) (memq (car sexp) '(+ - *)))
    (unless (= (length sexp) 3)
      (signal 'nelisp-phase47-compiler-error
              (list :arith-arity (car sexp) sexp)))
    (list :kind 'arith
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
   ((and (consp sexp) (eq (car sexp) 'defun))
    (unless (>= (length sexp) 4)
      (signal 'nelisp-phase47-compiler-error
              (list :defun-arity sexp)))
    (let* ((name (nth 1 sexp))
           (params (nth 2 sexp))
           (body (nth 3 sexp))
           (arity (length params)))
      (when (> arity (length nelisp-phase47-compiler--arg-regs))
        (signal 'nelisp-phase47-compiler-error
                (list :defun-too-many-params name arity)))
      (unless (cl-every #'symbolp params)
        (signal 'nelisp-phase47-compiler-error
                (list :defun-params-not-symbols params)))
      (let* ((param-regs (cl-subseq
                          nelisp-phase47-compiler--arg-regs 0 arity))
             ;; FENV: each param maps to plist `(:reg R :slot S)'
             ;; where SLOT is the param's 0-based index, used by
             ;; `--emit-value' to compute the rbp-relative spill
             ;; offset `-8*(slot+1)'.
             (new-fenv
              (let ((idx -1))
                (cl-mapcar
                 (lambda (p r)
                   (setq idx (1+ idx))
                   (cons p (list :reg r :slot idx)))
                 params param-regs)))
             ;; Body is a value-producing expression (= implicit return).
             (body-ir (nelisp-phase47-compiler--parse-value
                       body env new-fenv defuns)))
        (list :kind 'defun
              :name name
              :params params
              :param-regs param-regs
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

(defun nelisp-phase47-compiler--emit-ref-load (buf slot)
  "Emit `mov rax, [rbp - 8*(SLOT+1)]' (= 4 bytes, fixed).
Used by `:kind ref' to load a spilled parameter off the local
frame.  Encoding: REX.W (= 0x48), MOV r64, r/m64 opcode (= 0x8B),
ModR/M = mod=01 reg=000 (rax) rm=101 (rbp) = 0x45, then disp8 =
(- 8*(SLOT+1)) as a signed byte.  SLOT ranges over 0..5 so the
displacement is always in disp8 range; signals if SLOT is out of
range for the current Doc 97 arity cap."
  (unless (and (integerp slot) (<= 0 slot 5))
    (signal 'nelisp-phase47-compiler-error
            (list :ref-slot-out-of-range slot)))
  (let* ((disp (- (* 8 (1+ slot))))
         (disp8 (logand disp #xFF)))
    (nelisp-asm-x86_64-emit-bytes
     buf (unibyte-string #x48 #x8B #x45 disp8))))

(defun nelisp-phase47-compiler--emit-value (node buf)
  "Emit code that computes value NODE into rax.
NODE is one of `imm' / `ref' / `arith' / `call' / `cmp' / `if' /
`while' / `cond' / `logic'.  All variants preserve callee-saved
registers (= we use only rax + r10 + r11 for scratch and the arg
regs rdi..r9 which are caller-saved anyway)."
  (pcase (plist-get node :kind)
    ('imm
     ;; mov rax, imm32                                = 7 bytes
     (nelisp-asm-x86_64-mov-imm32 buf 'rax (plist-get node :value)))
    ('ref
     ;; mov rax, [rbp - 8*(slot+1)] — read spilled param off stack.
     ;; The defun prologue pushed each incoming arg-reg in order
     ;; right after `push rbp; mov rbp, rsp', so slot 0 sits at
     ;; [rbp-8], slot 1 at [rbp-16], ..., slot 5 at [rbp-48].
     (nelisp-phase47-compiler--emit-ref-load buf
                                             (plist-get node :slot)))
    ('arith
     (nelisp-phase47-compiler--emit-arith node buf))
    ('call
     (nelisp-phase47-compiler--emit-call node buf))
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
             (list :unknown-value-kind kind)))))

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
     (t
      (signal 'nelisp-phase47-compiler-error
              (list :unknown-arith-op op))))))

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
    (nelisp-asm-x86_64-movzx-eax-al buf)))

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
    (nelisp-phase47-compiler--emit-value (plist-get node :test) buf)
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jz-rel32 buf else-lbl)
    (nelisp-phase47-compiler--emit-value (plist-get node :then) buf)
    (nelisp-asm-x86_64-jmp-rel32 buf end-lbl)
    (nelisp-asm-x86_64-define-label buf else-lbl)
    (nelisp-phase47-compiler--emit-value (plist-get node :else) buf)
    (nelisp-asm-x86_64-define-label buf end-lbl)))

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
      ((or 'if 'while 'cond 'logic 'cmp 'arith)
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
body computes the return value into rax, the epilogue restores
rsp + rbp and `ret's.

§97.c rationale (= why params spill to memory): callees freely
clobber the SysV arg registers (rdi..r9 are caller-saved), so a
function that calls back into itself or any other function would
lose its own params if they only lived in arg-regs.  Spilling at
prologue and re-reading from `[rbp - 8*(slot+1)]' on every `ref'
makes recursive composition (= factorial) work.  Each `push' is
fixed-width (= 1 byte for low regs, 2 bytes for r8/r9) so the
two-pass byte invariant is preserved per defun.  The matching
epilogue uses `mov rsp, rbp' (= 3 bytes) to deallocate the
param-spill area in one shot regardless of arity."
  (let* ((name (plist-get defun-ir :name))
         (param-regs (plist-get defun-ir :param-regs))
         (body (plist-get defun-ir :body)))
    (nelisp-asm-x86_64-define-label buf name)
    ;; Prologue: push rbp; mov rbp, rsp; push each param reg.
    (nelisp-asm-x86_64-push buf 'rbp)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rbp 'rsp)
    (dolist (preg param-regs)
      (nelisp-asm-x86_64-push buf preg))
    ;; Body — value walked into rax.
    (nelisp-phase47-compiler--emit-value body buf)
    ;; Epilogue: deallocate param spill via mov rsp, rbp; pop rbp; ret.
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsp 'rbp)
    (nelisp-asm-x86_64-pop buf 'rbp)
    (nelisp-asm-x86_64-ret buf)))

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
    (sexp file-path &key (arch 'x86_64))
  "Compile SEXP (= one or more defuns) to an ET_REL .o at FILE-PATH.

SEXP must be either a single `(defun NAME (PARAMS...) BODY)' form or
a `(seq (defun ...) ...)' wrapping multiple defuns.  Each defun
becomes a GLOBAL STT_FUNC symbol named after the defun (= symbol-name
of the elisp identifier, with underscores preserved for C linkage).

ARCH defaults to `x86_64'.  v1 signals
`nelisp-phase47-compiler-error' for any other ARCH (= aarch64
deferred to Stage 99.A follow-up).

Spike scope: defun bodies must not reference strings.  Signals
`nelisp-phase47-compiler-error' with `:object-mode-no-strings' if
the IR contains a `write' node, because rodata vaddr baking does
not survive linker relocation in v1.

Returns FILE-PATH on success.  Signals on parse error, free symbol
reference, out-of-range integer, or any pass-1/pass-2 byte-length
drift (= a Doc 92 emitter invariant violation)."
  (unless (eq arch 'x86_64)
    (signal 'nelisp-phase47-compiler-error
            (list :unsupported-arch arch)))
  (let* ((nelisp-phase47-compiler--label-counter 0)
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
    (let* ((buf (nelisp-asm-x86_64-make-buffer)))
      (dolist (d defuns)
        (nelisp-phase47-compiler--emit-defun d buf))
      (let* ((text-bytes (nelisp-asm-x86_64-resolve-fixups buf))
             (labels (nelisp-asm-x86_64-buffer-labels buf))
             ;; Each label → GLOBAL STT_FUNC symbol.  `:value' is
             ;; the byte offset within `.text' (= relocatable, no
             ;; vaddr baking, ET_REL convention).
             (symbols
              (mapcar
               (lambda (cell)
                 (let ((nm (car cell))
                       (pos (cdr cell)))
                   (list :name (if (stringp nm) nm (symbol-name nm))
                         :value pos
                         :size 0
                         :section 'text
                         :bind 'global
                         :type 'func)))
               (reverse labels))))
        (nelisp-elf-write-binary
         file-path
         (list :e-type 'rel
               :text text-bytes
               :symbols symbols
               :machine arch))
        file-path))))

(provide 'nelisp-phase47-compiler)

;;; nelisp-phase47-compiler.el ends here
