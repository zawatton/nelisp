;;; nelisp-bytecode.el --- Bytecode VM scaffolding  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 3b skeleton — see docs/design/08-bytecode-vm.org.
;;
;; This sub-phase (3b.1) lands the module boundary and data shapes
;; without any opcodes.  `nelisp-bc-compile' and `nelisp-bc-run' raise
;; `nelisp-bc-unimplemented' so callers get a clean failure until
;; 3b.2 wires up the first five opcodes (CONST / RETURN / STACK-REF /
;; DROP / DUP).
;;
;; Data shape (design doc §4.1):
;;
;;   (nelisp-bcl ENV PARAMS CONSTS CODE STACK-DEPTH SPECIAL-MASK)
;;
;; ENV          lexical env alist captured at closure creation
;; PARAMS       lambda-list (same shape as the interpreter's closures)
;; CONSTS       constant vector referenced by CONST / VARREF / etc.
;; CODE         opcode vector (ints).  3b.1 keeps this a plain vector;
;;              3b.7 may repack into a unibyte string.
;; STACK-DEPTH  maximum operand stack depth the compiler reserves
;; SPECIAL-MASK integer bitmask: bit i set iff PARAMS's i-th positional
;;              parameter must bind dynamically.  Unused until 3b.5.
;;
;; The distinguishing car `nelisp-bcl' (vs the interpreter's
;; `nelisp-closure') lets `nelisp--apply-closure' dispatch to the VM
;; in later sub-phases without tagging changes.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)

;; Forward declaration — Phase 3c owns `nelisp-gc--active-vms' in
;; src/nelisp-gc.el.  We re-declare nil-initialised here so `nelisp-bc-run'
;; can push/pop the active VM state vector without a hard require on
;; nelisp-gc (which would pull the tracker into every bytecode build).
;; Emacs `defvar' with initial value is idempotent — whichever file
;; loads first wins, the other is a no-op.
(defvar nelisp-gc--active-vms nil)

(define-error 'nelisp-bc-error "NeLisp bytecode error")
(define-error 'nelisp-bc-unimplemented
  "NeLisp bytecode feature not implemented yet" 'nelisp-bc-error)

;;; Bytecode closure object ------------------------------------------

(defsubst nelisp-bcl-p (x)
  "Non-nil if X is a NeLisp bytecode closure (see commentary §4.1)."
  (and (consp x) (eq (car x) 'nelisp-bcl)))

(defsubst nelisp-bc-make (env params consts code stack-depth special-mask)
  "Construct a bytecode closure object from its six fields."
  (list 'nelisp-bcl env params consts code stack-depth special-mask))

(defsubst nelisp-bc-env           (bcl) (nth 1 bcl))
(defsubst nelisp-bc-params        (bcl) (nth 2 bcl))
(defsubst nelisp-bc-consts        (bcl) (nth 3 bcl))
(defsubst nelisp-bc-code          (bcl) (nth 4 bcl))
(defsubst nelisp-bc-stack-depth   (bcl) (nth 5 bcl))
(defsubst nelisp-bc-special-mask  (bcl) (nth 6 bcl))

;;; Opcode table ------------------------------------------------------
;;
;; Phase 3b.2 ships the first five opcodes (see docs/design/08
;; §3.3b.2).  Each entry below is (NAME BYTE ARG-BYTES).  ARG-BYTES is
;; the number of inline operand bytes that follow the opcode in CODE.
;;
;; Name            Byte  Arg-bytes  Semantics
;; RETURN            0        0     pop top of stack, return as BCL result
;; CONST             1        1     push CONSTS[uint8] onto stack
;; STACK-REF         2        1     push copy of stack[sp - uint8 - 1]
;; DROP              3        0     pop top, discard
;; DUP               4        0     push copy of top
;; GOTO              5        2     pc = uint16 little-endian absolute
;; GOTO-IF-NIL       6        2     pop top; if nil, jump (uint16 abs)
;; GOTO-IF-NOT-NIL   7        2     pop top; if non-nil, jump (uint16 abs)
;; ADD1              8        0     pop n, push (1+ n)
;; SUB1              9        0     pop n, push (1- n)
;; PLUS             10        0     pop b a, push (+ a b)
;; MINUS            11        0     pop b a, push (- a b)
;; LESS             12        0     pop b a, push (< a b)
;; GREATER          13        0     pop b a, push (> a b)
;; EQ               14        0     pop b a, push (eq a b)
;; NOT              15        0     pop x, push (not x)
;; VARREF           16        1     push CONSTS[uint8]'s global value
;; VARSET           17        1     pop val; set global of CONSTS[uint8] to val
;; VARBIND          18        1     pop val; save old global of CONSTS[uint8]
;;                                   on specpdl, install new value (for `let'
;;                                   bindings of defvar'd specials)
;; UNBIND           19        1     pop uint8 specpdl entries, restoring each
;; STACK-SET        20        1     pop val; write into stack[sp - uint8 - 1]
;; DISCARDN         21        1     drop uint8 values below top, keep TOS
;; CAR              22        0     pop x, push (car x)
;; CDR              23        0     pop x, push (cdr x)
;; CONS             24        0     pop b a, push (cons a b)
;; LIST1            25        0     pop x, push (list x)
;; LIST2            26        0     pop b a, push (list a b)
;; LIST3            27        0     pop c b a, push (list a b c)
;; LIST4            28        0     pop d c b a, push (list a b c d)
;; LISTN            29        1     pop uint8 values, push list (order preserved)
;; CALL             30        1     pop nargs args + 1 fn, apply, push result
;; PUSH-CATCH       31        2     pop tag; nested dispatch wrapped in host
;;                                   `catch' with that tag.  On throw, push
;;                                   thrown value and jump to uint16 target.
;; POP-HANDLER      32        0     exit the innermost nested dispatch
;;                                   (used by PUSH-CATCH / PUSH-UNWIND /
;;                                   PUSH-CC bodies on normal exit)
;; THROW            33        0     pop val, pop tag, invoke host `throw'
;; PUSH-UNWIND      34        2     nested dispatch wrapped in host
;;                                   `unwind-protect'; cleanup code lives
;;                                   at uint16 target and is re-entered
;;                                   both on normal exit and non-local exit
;; PUSH-CC          35        2     nested dispatch wrapped in host
;;                                   `condition-case'; on error, push err
;;                                   and jump to uint16 target
;; MAKE-CLOSURE     36        1     pop uint8 captured values + 1 bcl
;;                                   template; push new closure with
;;                                   ENV = (cap0 cap1 ... capN-1)
;; CAPTURED-REF     37        1     push (nth uint8 closure-env)

(defconst nelisp-bc--opcode-table
  '((RETURN           0 0)
    (CONST            1 1)
    (STACK-REF        2 1)
    (DROP             3 0)
    (DUP              4 0)
    (GOTO             5 2)
    (GOTO-IF-NIL      6 2)
    (GOTO-IF-NOT-NIL  7 2)
    (ADD1             8 0)
    (SUB1             9 0)
    (PLUS            10 0)
    (MINUS           11 0)
    (LESS            12 0)
    (GREATER         13 0)
    (EQ              14 0)
    (NOT             15 0)
    (VARREF          16 1)
    (VARSET          17 1)
    (VARBIND         18 1)
    (UNBIND          19 1)
    (STACK-SET       20 1)
    (DISCARDN        21 1)
    (CAR             22 0)
    (CDR             23 0)
    (CONS            24 0)
    (LIST1           25 0)
    (LIST2           26 0)
    (LIST3           27 0)
    (LIST4           28 0)
    (LISTN           29 1)
    (CALL            30 1)
    (PUSH-CATCH      31 2)
    (POP-HANDLER     32 0)
    (THROW           33 0)
    (PUSH-UNWIND     34 2)
    (PUSH-CC         35 2)
    (MAKE-CLOSURE    36 1)
    (CAPTURED-REF    37 1))
  "Ordered list of (NAME BYTE ARG-BYTES) triples.
Source of truth; the plist, reverse-name, and arg-bytes caches are
derived from this by `nelisp-bc--build-opcode-indexes'.")

(defvar nelisp-bc--opcodes nil
  "Plist mapping opcode name symbol to its byte.
Rebuilt from `nelisp-bc--opcode-table' at load time.")

(defvar nelisp-bc--opcode-names nil
  "Vector mapping opcode byte (0-255) to its name symbol, or nil.")

(defvar nelisp-bc--opcode-arg-bytes nil
  "Vector mapping opcode byte (0-255) to its inline arg byte count.")

(defun nelisp-bc--build-opcode-indexes ()
  "Populate the opcode plist / reverse / arg-bytes caches.
Called at load time from this module's tail."
  (let ((plist '())
        (names (make-vector 256 nil))
        (arg-bytes (make-vector 256 0)))
    (dolist (row nelisp-bc--opcode-table)
      (let ((name (nth 0 row))
            (byte (nth 1 row))
            (args (nth 2 row)))
        (setq plist (plist-put plist name byte))
        (aset names byte name)
        (aset arg-bytes byte args)))
    (setq nelisp-bc--opcodes plist)
    (setq nelisp-bc--opcode-names names)
    (setq nelisp-bc--opcode-arg-bytes arg-bytes)))

(nelisp-bc--build-opcode-indexes)

(defun nelisp-bc-opcode (name)
  "Return the byte for opcode NAME, or signal if undefined."
  (or (plist-get nelisp-bc--opcodes name)
      (signal 'nelisp-bc-error
              (list "unknown opcode" name))))

;; Top-level opcode constants.  Hoisting these out of `nelisp-bc-run'
;; saves ~37 plist-get + setq per VM invocation — measurable on hot
;; recursive bcl loops (fib, fact, etc.) where `nelisp-bc-run' is
;; re-entered for every NeLisp call.
(defconst nelisp-bc--op-RETURN          0)
(defconst nelisp-bc--op-CONST           1)
(defconst nelisp-bc--op-STACK-REF       2)
(defconst nelisp-bc--op-DROP            3)
(defconst nelisp-bc--op-DUP             4)
(defconst nelisp-bc--op-GOTO            5)
(defconst nelisp-bc--op-GOTO-IF-NIL     6)
(defconst nelisp-bc--op-GOTO-IF-NOT-NIL 7)
(defconst nelisp-bc--op-ADD1            8)
(defconst nelisp-bc--op-SUB1            9)
(defconst nelisp-bc--op-PLUS           10)
(defconst nelisp-bc--op-MINUS          11)
(defconst nelisp-bc--op-LESS           12)
(defconst nelisp-bc--op-GREATER        13)
(defconst nelisp-bc--op-EQ             14)
(defconst nelisp-bc--op-NOT            15)
(defconst nelisp-bc--op-VARREF         16)
(defconst nelisp-bc--op-VARSET         17)
(defconst nelisp-bc--op-VARBIND        18)
(defconst nelisp-bc--op-UNBIND         19)
(defconst nelisp-bc--op-STACK-SET      20)
(defconst nelisp-bc--op-DISCARDN       21)
(defconst nelisp-bc--op-CAR            22)
(defconst nelisp-bc--op-CDR            23)
(defconst nelisp-bc--op-CONS           24)
(defconst nelisp-bc--op-LIST1          25)
(defconst nelisp-bc--op-LIST2          26)
(defconst nelisp-bc--op-LIST3          27)
(defconst nelisp-bc--op-LIST4          28)
(defconst nelisp-bc--op-LISTN          29)
(defconst nelisp-bc--op-CALL           30)
(defconst nelisp-bc--op-PUSH-CATCH     31)
(defconst nelisp-bc--op-POP-HANDLER    32)
(defconst nelisp-bc--op-THROW          33)
(defconst nelisp-bc--op-PUSH-UNWIND    34)
(defconst nelisp-bc--op-PUSH-CC        35)
(defconst nelisp-bc--op-MAKE-CLOSURE   36)
(defconst nelisp-bc--op-CAPTURED-REF   37)

;;; Compiler ---------------------------------------------------------
;;
;; Phase 3b.3 rewrites the compiler as a two-pass label-aware emitter.
;; Instructions are accumulated into `nelisp-bc--ctx' as tagged triples:
;;
;;   (BYTE)               — a zero-arg op (opcode byte only)
;;   (BYTE uint8)         — one-byte inline operand (CONST / STACK-REF)
;;   (JUMP OP LABEL)      — opcode byte + placeholder uint16 filled in
;;                          during the resolve pass
;;   (LABEL SYM)          — zero-width marker; records the byte offset
;;                          of the instruction that follows it
;;
;; After every form has been emitted we RETURN (so callers don't need
;; to remember) and then `nelisp-bc--finalize' resolves labels to
;; absolute PC offsets and flattens the triples into the CODE vector.

(cl-defstruct (nelisp-bc--ctx (:constructor nelisp-bc--ctx--make)
                              (:copier nil))
  consts   ;; list in order; index 0 is first element
  insns    ;; list in reverse emit order
  (sp 0)   ;; current tracked stack depth
  (max-sp 0)
  ;; Phase 3b.4a: lexical env — alist of (symbol . absolute-stack-slot).
  ;; A slot is an absolute index into the runtime stack; `sp' is the
  ;; next free position (slot N = sp-value just after pushing it - 1).
  ;; Symbols are looked up newest-first so inner bindings shadow outer.
  (lex-env nil)
  ;; Phase 3b.5a: when compiling a `lambda' body, list of symbols
  ;; that are lexical in some *enclosing* compile context.
  ;; References to them inside the body trigger free-lex capture
  ;; (3b.5b) — they're added to `captures' and emitted as
  ;; CAPTURED-REF instead of being treated as globals.
  (parent-lex-shadow nil)
  ;; Phase 3b.5b: list of (SYM . INDEX) for symbols this ctx has
  ;; captured from outer scopes.  `compile-var-ref' grows this
  ;; on-demand; `compile-lambda' reads it after the body is done to
  ;; emit MAKE-CLOSURE arguments at the parent level.
  (captures nil))

(defun nelisp-bc--ctx-new ()
  (nelisp-bc--ctx--make :consts nil :insns nil :sp 0 :max-sp 0
                        :lex-env nil :parent-lex-shadow nil
                        :captures nil))

(defun nelisp-bc--self-evaluating-p (form)
  "Non-nil if FORM evaluates to itself with no further work."
  (or (null form)
      (eq form t)
      (keywordp form)
      (numberp form)
      (stringp form)
      (vectorp form)))

(defun nelisp-bc--lex-slot (ctx sym)
  "Return SYM's absolute stack slot in CTX, or nil if not lexical."
  (let ((cell (assq sym (nelisp-bc--ctx-lex-env ctx))))
    (and cell (cdr cell))))

(defun nelisp-bc--special-p (sym)
  "Non-nil if SYM is a defvar-declared dynamic special."
  (and (boundp 'nelisp--specials)
       (gethash sym nelisp--specials)))

(defun nelisp-bc--adjust-sp (ctx delta)
  "Track the operand stack depth inside CTX by DELTA."
  (cl-incf (nelisp-bc--ctx-sp ctx) delta)
  (when (> (nelisp-bc--ctx-sp ctx)
           (nelisp-bc--ctx-max-sp ctx))
    (setf (nelisp-bc--ctx-max-sp ctx)
          (nelisp-bc--ctx-sp ctx))))

(defun nelisp-bc--emit (ctx op &rest args)
  "Emit a simple opcode OP with optional inline uint8 ARGS."
  (push (cons (nelisp-bc-opcode op) args) (nelisp-bc--ctx-insns ctx)))

(defun nelisp-bc--emit-jump (ctx op label)
  "Emit a JUMP-class OP targeting LABEL (symbol)."
  (push (list 'JUMP (nelisp-bc-opcode op) label)
        (nelisp-bc--ctx-insns ctx)))

(defun nelisp-bc--emit-label (ctx label)
  "Emit zero-width LABEL marker (symbol) at the current position."
  (push (list 'LABEL label) (nelisp-bc--ctx-insns ctx)))

(defun nelisp-bc--add-const (ctx value)
  "Return index of VALUE in CTX's constants (add with dedupe)."
  (let ((pos (cl-position value (nelisp-bc--ctx-consts ctx)
                          :test #'equal)))
    (or pos
        (let ((new-idx (length (nelisp-bc--ctx-consts ctx))))
          (setf (nelisp-bc--ctx-consts ctx)
                (append (nelisp-bc--ctx-consts ctx) (list value)))
          (when (> new-idx 255)
            (signal 'nelisp-bc-unimplemented
                    (list "const pool > 256 entries pending later phase")))
          new-idx))))

(defun nelisp-bc--resolve (insns-rev)
  "Second pass: turn label-tagged INSNS-REV into a plain int vector.
Returns a cons (CODE-VECTOR . RESOLVED-LABELS-PLIST)."
  (let ((insns (nreverse insns-rev))
        (offset 0)
        (labels nil)
        (emit-queue nil))
    ;; Pass 1: assign each insn its starting byte offset.
    (dolist (insn insns)
      (cond
       ((eq (car insn) 'LABEL)
        (setq labels (plist-put labels (cadr insn) offset)))
       ((eq (car insn) 'JUMP)
        (push (cons offset insn) emit-queue)
        (cl-incf offset 3))    ; op byte + uint16
       (t
        (push (cons offset insn) emit-queue)
        (cl-incf offset (length insn)))))
    ;; Pass 2: walk emit-queue (already in byte order) and flatten.
    (let ((code (make-vector offset 0)))
      (dolist (item (nreverse emit-queue))
        (let ((pos (car item))
              (insn (cdr item)))
          (cond
           ((eq (car insn) 'JUMP)
            (let* ((opbyte (nth 1 insn))
                   (label  (nth 2 insn))
                   (target (plist-get labels label)))
              (unless target
                (signal 'nelisp-bc-error
                        (list "unresolved label" label)))
              (aset code pos       opbyte)
              (aset code (1+ pos)  (logand target #xFF))
              (aset code (+ pos 2) (logand (ash target -8) #xFF))))
           (t
            (let ((i pos))
              (dolist (byte insn)
                (aset code i byte)
                (setq i (1+ i))))))))
      code)))

;;; Form compilation --------------------------------------------------

(defun nelisp-bc--compile-form (ctx form)
  "Compile FORM, leaving exactly one value on the operand stack."
  (cond
   ((nelisp-bc--self-evaluating-p form)
    (let ((idx (nelisp-bc--add-const ctx form)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   ((symbolp form)
    (nelisp-bc--compile-var-ref ctx form))
   ((and (consp form) (eq (car form) 'quote))
    (unless (and (consp (cdr form)) (null (cddr form)))
      (signal 'nelisp-bc-error (list "malformed quote" form)))
    (let ((idx (nelisp-bc--add-const ctx (cadr form))))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   ((and (consp form) (eq (car form) 'progn))
    (nelisp-bc--compile-progn ctx (cdr form)))
   ((and (consp form) (eq (car form) 'if))
    (nelisp-bc--compile-if ctx (nth 1 form) (nth 2 form) (nthcdr 3 form)))
   ((and (consp form) (eq (car form) 'cond))
    (nelisp-bc--compile-cond ctx (cdr form)))
   ((and (consp form) (eq (car form) 'setq))
    (nelisp-bc--compile-setq ctx (cdr form)))
   ((and (consp form) (eq (car form) 'let))
    (nelisp-bc--compile-let ctx (cadr form) (cddr form)))
   ((and (consp form) (eq (car form) 'let*))
    (nelisp-bc--compile-let* ctx (cadr form) (cddr form)))
   ((and (consp form) (eq (car form) 'while))
    (nelisp-bc--compile-while ctx (cadr form) (cddr form)))
   ((and (consp form) (memq (car form) '(+ - < > eq not 1+ 1-)))
    (nelisp-bc--compile-primitive ctx (car form) (cdr form)))
   ((and (consp form) (memq (car form) '(car cdr cons list)))
    (nelisp-bc--compile-list-primitive ctx (car form) (cdr form)))
   ((and (consp form) (eq (car form) 'catch))
    (nelisp-bc--compile-catch ctx (cadr form) (cddr form)))
   ((and (consp form) (eq (car form) 'throw))
    (nelisp-bc--compile-throw ctx (cadr form) (nth 2 form)))
   ((and (consp form) (eq (car form) 'unwind-protect))
    (nelisp-bc--compile-unwind-protect ctx (cadr form) (cddr form)))
   ((and (consp form) (eq (car form) 'condition-case))
    (nelisp-bc--compile-condition-case
     ctx (cadr form) (nth 2 form) (nthcdr 3 form)))
   ((and (consp form) (eq (car form) 'lambda))
    (nelisp-bc--compile-lambda ctx (cadr form) (cddr form)))
   ((and (consp form) (eq (car form) 'function))
    ;; (function FOO) — quote a callable.  Symbol → push the symbol;
    ;; `nelisp--apply' resolves it to the host or NeLisp function at
    ;; call time, matching the interpreter's `nelisp--eval-function'.
    ;; Lambda → compile inline as a closure.
    (let ((arg (cadr form)))
      (cond
       ((and (consp arg) (eq (car arg) 'lambda))
        (nelisp-bc--compile-lambda ctx (cadr arg) (cddr arg)))
       ((symbolp arg)
        (let ((idx (nelisp-bc--add-const ctx arg)))
          (nelisp-bc--emit ctx 'CONST idx)
          (nelisp-bc--adjust-sp ctx 1)))
       (t
        (signal 'nelisp-bc-unimplemented
                (list "(function ...) form not handled" arg))))))
   ((and (consp form)
         (memq (car form)
               '(defun defvar defconst defmacro
                 and or when unless prog1 prog2)))
    ;; Known special forms slated for later sub-phases (or never:
    ;; top-level `defun'/`defvar' belong to the interpreter) — surface
    ;; a clean `nelisp-bc-unimplemented' so callers can skip or fall
    ;; back to the interpreter instead of getting a cryptic "void
    ;; function" at runtime.
    (signal 'nelisp-bc-unimplemented
            (list "special form pending later sub-phase" (car form))))
   ((and (consp form) (consp (car form)) (eq (car (car form)) 'lambda))
    ;; ((lambda PARAMS BODY) ARGS...) — inline call to a literal
    ;; lambda.  Compile the lambda then invoke via CALL.
    (nelisp-bc--compile-call ctx (car form) (cdr form)))
   ((and (consp form) (symbolp (car form)))
    (nelisp-bc--compile-call ctx (car form) (cdr form)))
   (t
    (signal 'nelisp-bc-unimplemented
            (list "compiler cannot handle form yet" form)))))

(defun nelisp-bc--compile-var-ref (ctx sym)
  "Compile a reference to symbol SYM.
Keywords / nil / t are self-evaluating; lexicals emit STACK-REF from
the recorded slot; everything else reads through the global/specials
table via VARREF."
  (cond
   ((eq sym nil)
    (let ((idx (nelisp-bc--add-const ctx nil)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   ((eq sym t)
    (let ((idx (nelisp-bc--add-const ctx t)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   ((keywordp sym)
    (let ((idx (nelisp-bc--add-const ctx sym)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   (t
    (let ((slot (nelisp-bc--lex-slot ctx sym)))
      (cond
       ((and slot (not (nelisp-bc--special-p sym)))
        ;; Lexical slot still live: push a copy via STACK-REF.
        (let ((offset (- (nelisp-bc--ctx-sp ctx) 1 slot)))
          (when (or (< offset 0) (> offset 255))
            (signal 'nelisp-bc-unimplemented
                    (list "lexical slot out of 1-byte range" sym offset)))
          (nelisp-bc--emit ctx 'STACK-REF offset)
          (nelisp-bc--adjust-sp ctx 1)))
       (t
        (let ((cap-cell (assq sym (nelisp-bc--ctx-captures ctx))))
          (cond
           (cap-cell
            ;; Already captured by this ctx — read from closure env.
            (let ((idx (cdr cap-cell)))
              (when (> idx 255)
                (signal 'nelisp-bc-unimplemented
                        (list "captures > 256 entries pending later phase")))
              (nelisp-bc--emit ctx 'CAPTURED-REF idx)
              (nelisp-bc--adjust-sp ctx 1)))
           ((memq sym (nelisp-bc--ctx-parent-lex-shadow ctx))
            ;; Free outer lex — allocate a new capture slot.
            (let ((idx (length (nelisp-bc--ctx-captures ctx))))
              (when (> idx 255)
                (signal 'nelisp-bc-unimplemented
                        (list "captures > 256 entries pending later phase")))
              (setf (nelisp-bc--ctx-captures ctx)
                    (append (nelisp-bc--ctx-captures ctx)
                            (list (cons sym idx))))
              (nelisp-bc--emit ctx 'CAPTURED-REF idx)
              (nelisp-bc--adjust-sp ctx 1)))
           (t
            ;; Global / special — read from `nelisp--globals'.
            (let ((idx (nelisp-bc--add-const ctx sym)))
              (nelisp-bc--emit ctx 'VARREF idx)
              (nelisp-bc--adjust-sp ctx 1)))))))))))

(defun nelisp-bc--compile-setq (ctx args)
  "Compile (setq SYM VAL [SYM VAL ...]).
Result is the last VAL's value."
  (unless args
    (signal 'nelisp-bc-error (list "setq with no args")))
  (let ((remaining args))
    (while remaining
      (unless (cdr remaining)
        (signal 'nelisp-bc-error (list "setq with odd args")))
      (let ((sym (car remaining))
            (val (cadr remaining)))
        (unless (symbolp sym)
          (signal 'nelisp-bc-error (list "setq non-symbol" sym)))
        (when (or (eq sym nil) (eq sym t) (keywordp sym))
          (signal 'nelisp-bc-error
                  (list "cannot setq constant" sym)))
        (nelisp-bc--compile-form ctx val)
        (let ((slot (nelisp-bc--lex-slot ctx sym)))
          (cond
           ((and slot (not (nelisp-bc--special-p sym)))
            ;; Lexical: write into slot, then DUP the slot back so the
            ;; form leaves the value on the stack as setq's result.
            ;; Easiest encoding: DUP, then STACK-SET to (slot+1 offset from
            ;; new top after DUP).  After DUP sp' = sp+1, top is the
            ;; duplicate; STACK-SET offset = new_sp - 1 - slot.
            (nelisp-bc--emit ctx 'DUP)
            (nelisp-bc--adjust-sp ctx 1)
            (let ((offset (- (nelisp-bc--ctx-sp ctx) 1 slot)))
              (when (or (< offset 0) (> offset 255))
                (signal 'nelisp-bc-unimplemented
                        (list "lexical slot out of 1-byte range"
                              sym offset)))
              (nelisp-bc--emit ctx 'STACK-SET offset)
              (nelisp-bc--adjust-sp ctx -1)))
           (t
            ;; Captured-var setq is semantically write-through to the
            ;; closure env, but we don't have a CAPTURED-SET op yet.
            ;; Bail early so the auto-compile hook falls back to the
            ;; interpreter, which handles closure-captured mutation
            ;; correctly via shared cons cells.
            (when (and (not (nelisp-bc--special-p sym))
                       (or (assq sym (nelisp-bc--ctx-captures ctx))
                           (memq sym (nelisp-bc--ctx-parent-lex-shadow ctx))))
              (signal 'nelisp-bc-unimplemented
                      (list "setq on captured lex pending later phase"
                            sym)))
            ;; Global / special: VARSET pops the value, so DUP first.
            (nelisp-bc--emit ctx 'DUP)
            (nelisp-bc--adjust-sp ctx 1)
            (let ((idx (nelisp-bc--add-const ctx sym)))
              (nelisp-bc--emit ctx 'VARSET idx)
              (nelisp-bc--adjust-sp ctx -1)))))
        (setq remaining (cddr remaining))
        (when remaining
          ;; Discard the intermediate value; only the last setq's value
          ;; is the overall result.
          (nelisp-bc--emit ctx 'DROP)
          (nelisp-bc--adjust-sp ctx -1))))))

(defun nelisp-bc--parse-binding (b)
  "Normalize a let binding spec B into (SYM . INIT-FORM)."
  (cond
   ((symbolp b)
    (when (or (eq b nil) (eq b t) (keywordp b))
      (signal 'nelisp-bc-error (list "cannot bind constant" b)))
    (cons b nil))
   ((and (consp b) (symbolp (car b)))
    (when (or (eq (car b) nil) (eq (car b) t) (keywordp (car b)))
      (signal 'nelisp-bc-error (list "cannot bind constant" (car b))))
    (cons (car b) (cadr b)))
   (t
    (signal 'nelisp-bc-error (list "malformed let binding" b)))))

(defun nelisp-bc--compile-let (ctx bindings body)
  "Compile (let BINDINGS BODY...) — parallel binding semantics.
Dynamic (defvar'd) names go via VARBIND/UNBIND; lexicals live in
stack slots."
  (let* ((parsed (mapcar #'nelisp-bc--parse-binding bindings))
         (outer-lex (nelisp-bc--ctx-lex-env ctx))
         (saved-sp (nelisp-bc--ctx-sp ctx))
         (n (length parsed))
         (dyn-count 0)
         (new-lex outer-lex))
    ;; 1. Evaluate every init in outer env, pushing each result.
    (dolist (p parsed)
      (nelisp-bc--compile-form ctx (cdr p)))
    ;; 2. Install bindings.  Dynamic ones are copied to TOS (STACK-REF)
    ;; then VARBIND'd, leaving the original slot as dead weight we'll
    ;; collapse in step 5.  Lexicals record their slot in the compile-
    ;; time env.
    (let ((pos 0))
      (dolist (p parsed)
        (let* ((sym (car p))
               (slot (+ saved-sp pos)))
          (cond
           ((nelisp-bc--special-p sym)
            (let ((offset (- (nelisp-bc--ctx-sp ctx) 1 slot)))
              (when (or (< offset 0) (> offset 255))
                (signal 'nelisp-bc-unimplemented
                        (list "let-binding slot out of range" sym offset)))
              (nelisp-bc--emit ctx 'STACK-REF offset)
              (nelisp-bc--adjust-sp ctx 1)
              (let ((idx (nelisp-bc--add-const ctx sym)))
                (nelisp-bc--emit ctx 'VARBIND idx)
                (nelisp-bc--adjust-sp ctx -1)))
            (cl-incf dyn-count))
           (t
            (setq new-lex (cons (cons sym slot) new-lex))))
          (cl-incf pos))))
    ;; 3. Compile body with extended lex-env.
    (setf (nelisp-bc--ctx-lex-env ctx) new-lex)
    (unwind-protect
        (nelisp-bc--compile-progn ctx body)
      (setf (nelisp-bc--ctx-lex-env ctx) outer-lex))
    ;; 4. Unbind dynamics.
    (when (> dyn-count 0)
      (nelisp-bc--emit ctx 'UNBIND dyn-count))
    ;; 5. Collapse the binding slots under the result.  After body, stack
    ;; top is the result and there are N binding slots below it.
    (when (> n 0)
      (when (> n 255)
        (signal 'nelisp-bc-unimplemented
                (list "let with > 255 bindings" n)))
      (nelisp-bc--emit ctx 'DISCARDN n)
      (nelisp-bc--adjust-sp ctx (- n)))))

(defun nelisp-bc--compile-let* (ctx bindings body)
  "Compile (let* BINDINGS BODY...) — sequential binding.
Each init sees all previously introduced lex/dyn bindings."
  (let* ((outer-lex (nelisp-bc--ctx-lex-env ctx))
         (lex-count 0)
         (dyn-count 0))
    (unwind-protect
        (progn
          (dolist (b bindings)
            (let* ((p (nelisp-bc--parse-binding b))
                   (sym (car p))
                   (init (cdr p)))
              (nelisp-bc--compile-form ctx init)
              (cond
               ((nelisp-bc--special-p sym)
                (let ((idx (nelisp-bc--add-const ctx sym)))
                  (nelisp-bc--emit ctx 'VARBIND idx)
                  (nelisp-bc--adjust-sp ctx -1))
                (cl-incf dyn-count))
               (t
                (let ((slot (- (nelisp-bc--ctx-sp ctx) 1)))
                  (setf (nelisp-bc--ctx-lex-env ctx)
                        (cons (cons sym slot)
                              (nelisp-bc--ctx-lex-env ctx)))
                  (cl-incf lex-count))))))
          (nelisp-bc--compile-progn ctx body)
          (when (> dyn-count 0)
            (nelisp-bc--emit ctx 'UNBIND dyn-count))
          (when (> lex-count 0)
            (when (> lex-count 255)
              (signal 'nelisp-bc-unimplemented
                      (list "let* with > 255 lex bindings" lex-count)))
            (nelisp-bc--emit ctx 'DISCARDN lex-count)
            (nelisp-bc--adjust-sp ctx (- lex-count))))
      (setf (nelisp-bc--ctx-lex-env ctx) outer-lex))))

(defun nelisp-bc--compile-while (ctx test body)
  "Compile (while TEST BODY...) — always returns nil."
  (let ((top-label (cl-gensym "bc-while-top-"))
        (end-label (cl-gensym "bc-while-end-")))
    (nelisp-bc--emit-label ctx top-label)
    (nelisp-bc--compile-form ctx test)
    (nelisp-bc--emit-jump ctx 'GOTO-IF-NIL end-label)
    (nelisp-bc--adjust-sp ctx -1)
    (when body
      (nelisp-bc--compile-progn ctx body)
      (nelisp-bc--emit ctx 'DROP)
      (nelisp-bc--adjust-sp ctx -1))
    (nelisp-bc--emit-jump ctx 'GOTO top-label)
    (nelisp-bc--emit-label ctx end-label)
    (let ((idx (nelisp-bc--add-const ctx nil)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1))))

(defun nelisp-bc--compile-progn (ctx body)
  "Compile BODY (sequence of forms); result of last form on stack."
  (cond
   ((null body)
    (let ((idx (nelisp-bc--add-const ctx nil)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   ((null (cdr body))
    (nelisp-bc--compile-form ctx (car body)))
   (t
    (nelisp-bc--compile-form ctx (car body))
    (nelisp-bc--emit ctx 'DROP)
    (nelisp-bc--adjust-sp ctx -1)
    (nelisp-bc--compile-progn ctx (cdr body)))))

(defun nelisp-bc--compile-if (ctx test then else-body)
  "Compile (if TEST THEN . ELSE-BODY) leaving one value on stack."
  (let ((else-label (cl-gensym "bc-else-"))
        (end-label  (cl-gensym "bc-endif-"))
        (sp-before-branches nil))
    (nelisp-bc--compile-form ctx test)
    (nelisp-bc--emit-jump ctx 'GOTO-IF-NIL else-label)
    (nelisp-bc--adjust-sp ctx -1)  ; GOTO-IF-NIL consumes test
    (setq sp-before-branches (nelisp-bc--ctx-sp ctx))
    (nelisp-bc--compile-form ctx then)
    (nelisp-bc--emit-jump ctx 'GOTO end-label)
    ;; Elise branch starts with the same sp as `sp-before-branches';
    ;; roll sp back before compiling it so max-sp tracking stays sane.
    (setf (nelisp-bc--ctx-sp ctx) sp-before-branches)
    (nelisp-bc--emit-label ctx else-label)
    (if else-body
        (nelisp-bc--compile-progn ctx else-body)
      (let ((idx (nelisp-bc--add-const ctx nil)))
        (nelisp-bc--emit ctx 'CONST idx)
        (nelisp-bc--adjust-sp ctx 1)))
    (nelisp-bc--emit-label ctx end-label)))

(defun nelisp-bc--compile-cond (ctx clauses)
  "Compile (cond . CLAUSES) by desugaring to nested if."
  (cond
   ((null clauses)
    (let ((idx (nelisp-bc--add-const ctx nil)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1)))
   (t
    (let* ((clause (car clauses))
           (test (car clause))
           (body (cdr clause)))
      (cond
       ;; (T BODY...) or bare-test clause with no body
       ((null body)
        (nelisp-bc--compile-if
         ctx test test
         (if (cdr clauses) (list (cons 'cond (cdr clauses))) nil)))
       (t
        (nelisp-bc--compile-if
         ctx test (cons 'progn body)
         (if (cdr clauses) (list (cons 'cond (cdr clauses))) nil))))))))

(defun nelisp-bc--compile-primitive (ctx op args)
  "Compile a supported primitive call OP with ARGS."
  (cl-case op
    ((1+ 1-)
     (unless (= (length args) 1)
       (signal 'nelisp-bc-error (list "1+/1- takes one arg" op args)))
     (nelisp-bc--compile-form ctx (car args))
     (nelisp-bc--emit ctx (if (eq op '1+) 'ADD1 'SUB1)))
    (not
     (unless (= (length args) 1)
       (signal 'nelisp-bc-error (list "not takes one arg" args)))
     (nelisp-bc--compile-form ctx (car args))
     (nelisp-bc--emit ctx 'NOT))
    ((+ -)
     (cond
      ((null args)
       (when (eq op '-)
         (signal 'nelisp-bc-error (list "no-arg - unsupported")))
       (let ((idx (nelisp-bc--add-const ctx 0)))
         (nelisp-bc--emit ctx 'CONST idx)
         (nelisp-bc--adjust-sp ctx 1)))
      ((= (length args) 1)
       (cond
        ((eq op '+)
         (nelisp-bc--compile-form ctx (car args)))
        (t
         ;; (- X) -> push 0, push X, MINUS (= 0 - X)
         (let ((zero-idx (nelisp-bc--add-const ctx 0)))
           (nelisp-bc--emit ctx 'CONST zero-idx)
           (nelisp-bc--adjust-sp ctx 1))
         (nelisp-bc--compile-form ctx (car args))
         (nelisp-bc--emit ctx 'MINUS)
         (nelisp-bc--adjust-sp ctx -1))))
      (t
       ;; Left-fold: compile first; for each rest, compile & emit PLUS/MINUS.
       (nelisp-bc--compile-form ctx (car args))
       (dolist (arg (cdr args))
         (nelisp-bc--compile-form ctx arg)
         (nelisp-bc--emit ctx (if (eq op '+) 'PLUS 'MINUS))
         (nelisp-bc--adjust-sp ctx -1)))))
    ((< > eq)
     (unless (= (length args) 2)
       (signal 'nelisp-bc-unimplemented
               (list "3b.3 only handles 2-arg < > eq" op args)))
     (nelisp-bc--compile-form ctx (car args))
     (nelisp-bc--compile-form ctx (cadr args))
     (nelisp-bc--emit ctx (cl-case op (< 'LESS) (> 'GREATER) (eq 'EQ)))
     (nelisp-bc--adjust-sp ctx -1))))

(defun nelisp-bc--compile-list-primitive (ctx op args)
  "Compile CAR / CDR / CONS / LIST forms directly to dedicated opcodes."
  (cl-case op
    ((car cdr)
     (unless (= (length args) 1)
       (signal 'nelisp-bc-error (list op "takes one arg" args)))
     (nelisp-bc--compile-form ctx (car args))
     (nelisp-bc--emit ctx (if (eq op 'car) 'CAR 'CDR)))
    (cons
     (unless (= (length args) 2)
       (signal 'nelisp-bc-error (list "cons takes two args" args)))
     (nelisp-bc--compile-form ctx (car args))
     (nelisp-bc--compile-form ctx (cadr args))
     (nelisp-bc--emit ctx 'CONS)
     (nelisp-bc--adjust-sp ctx -1))
    (list
     (let ((n (length args)))
       (cond
        ((zerop n)
         (let ((idx (nelisp-bc--add-const ctx nil)))
           (nelisp-bc--emit ctx 'CONST idx)
           (nelisp-bc--adjust-sp ctx 1)))
        (t
         (dolist (a args) (nelisp-bc--compile-form ctx a))
         (cond
          ((= n 1) (nelisp-bc--emit ctx 'LIST1))
          ((= n 2) (nelisp-bc--emit ctx 'LIST2))
          ((= n 3) (nelisp-bc--emit ctx 'LIST3))
          ((= n 4) (nelisp-bc--emit ctx 'LIST4))
          (t
           (when (> n 255)
             (signal 'nelisp-bc-unimplemented
                     (list "list call with > 255 args" n)))
           (nelisp-bc--emit ctx 'LISTN n)))
         (nelisp-bc--adjust-sp ctx (- 1 n))))))))

(defun nelisp-bc--compile-call (ctx head args)
  "Compile a generic call (HEAD ARG...).
HEAD is either a symbol (push as constant for runtime lookup via
`nelisp--apply') or a lambda form (compile to a closure first,
push that).  Args are compiled in order, then CALL nargs is
emitted."
  (let ((nargs (length args)))
    (when (> nargs 255)
      (signal 'nelisp-bc-unimplemented
              (list "call with > 255 args" head nargs)))
    (cond
     ((and (consp head) (eq (car head) 'lambda))
      (nelisp-bc--compile-lambda ctx (cadr head) (cddr head)))
     ((symbolp head)
      (let ((idx (nelisp-bc--add-const ctx head)))
        (nelisp-bc--emit ctx 'CONST idx)
        (nelisp-bc--adjust-sp ctx 1)))
     (t
      (signal 'nelisp-bc-error
              (list "unsupported call head" head))))
    (dolist (a args) (nelisp-bc--compile-form ctx a))
    (nelisp-bc--emit ctx 'CALL nargs)
    (nelisp-bc--adjust-sp ctx (- nargs))))

;;; Error-handling forms (3b.4c) -------------------------------------

(defun nelisp-bc--compile-catch (ctx tag body)
  "Compile (catch TAG BODY...) — host-catch wrapped nested dispatch."
  (let ((handler-label (cl-gensym "bc-catch-handler-"))
        (end-label     (cl-gensym "bc-catch-end-")))
    (nelisp-bc--compile-form ctx tag)
    (nelisp-bc--emit-jump ctx 'PUSH-CATCH handler-label)
    ;; PUSH-CATCH pops TAG (sp -=1 conceptually) and re-enters dispatch;
    ;; the ctx sp must reflect that the tag has been consumed.
    (nelisp-bc--adjust-sp ctx -1)
    (nelisp-bc--compile-progn ctx body)
    (nelisp-bc--emit ctx 'POP-HANDLER)
    (nelisp-bc--emit-jump ctx 'GOTO end-label)
    ;; Handler-label is reached on matching throw — the VM has pushed
    ;; the thrown value (+1 sp) so the compile-time sp is still what it
    ;; was just after PUSH-CATCH ran, then +1 for the pushed value.
    ;; After compile-progn body sp was saved+1; we force it here.
    (nelisp-bc--emit-label ctx handler-label)
    (nelisp-bc--emit-label ctx end-label)))

(defun nelisp-bc--compile-throw (ctx tag value)
  "Compile (throw TAG VALUE) — host `throw' from inside the VM."
  (nelisp-bc--compile-form ctx tag)
  (nelisp-bc--compile-form ctx value)
  (nelisp-bc--emit ctx 'THROW)
  (nelisp-bc--adjust-sp ctx -2)
  ;; THROW never returns; but to keep downstream compile-time sp
  ;; tracking plausible, pretend it pushed one value.
  (nelisp-bc--adjust-sp ctx 1))

(defun nelisp-bc--compile-unwind-protect (ctx bodyform cleanup-forms)
  "Compile (unwind-protect BODYFORM CLEANUP...) via host unwind-protect."
  (let ((cleanup-label (cl-gensym "bc-unwind-cleanup-"))
        (end-label     (cl-gensym "bc-unwind-end-")))
    (nelisp-bc--emit-jump ctx 'PUSH-UNWIND cleanup-label)
    (nelisp-bc--compile-form ctx bodyform)
    (nelisp-bc--emit ctx 'POP-HANDLER)
    ;; After body POP-HANDLER the VM unconditionally re-enters dispatch
    ;; at cleanup-label; no GOTO needed — control just falls through.
    (nelisp-bc--emit-label ctx cleanup-label)
    ;; The cleanup always runs.  On the normal path, body's value is
    ;; underneath; on non-local exit, the VM resets sp to the pre-body
    ;; level before jumping here, so only cleanup values accumulate.
    ;; Either way we drop cleanup's result and exit the cleanup's
    ;; nested dispatch via POP-HANDLER.
    (cond
     ((null cleanup-forms)
      ;; No cleanup forms: still need a value to DROP to keep the
      ;; dispatcher honest.
      (let ((idx (nelisp-bc--add-const ctx nil)))
        (nelisp-bc--emit ctx 'CONST idx)
        (nelisp-bc--adjust-sp ctx 1)))
     (t
      (nelisp-bc--compile-progn ctx cleanup-forms)))
    (nelisp-bc--emit ctx 'DROP)
    (nelisp-bc--adjust-sp ctx -1)
    (nelisp-bc--emit ctx 'POP-HANDLER)
    (nelisp-bc--emit-label ctx end-label)))

(defun nelisp-bc--compile-condition-case (ctx var bodyform handlers)
  "Compile (condition-case VAR BODYFORM HANDLERS...) via host condition-case.

Each handler is (CONDITIONS-SPEC BODY...).  At run time, on error
the VM pushes the error data and jumps to the handler-dispatch
section; each handler tests its spec against the error's
conditions chain and either runs its body (with VAR bound to the
error data) or falls through to the next handler.  No match →
re-signal via a compiled `signal' call."
  (when (and var (not (symbolp var)))
    (signal 'nelisp-bc-error
            (list "condition-case VAR must be symbol or nil" var)))
  (when (and var (or (eq var t) (keywordp var)))
    (signal 'nelisp-bc-error
            (list "condition-case VAR must be an ordinary symbol" var)))
  (let ((handler-entry-label (cl-gensym "bc-cc-entry-"))
        (end-label           (cl-gensym "bc-cc-end-"))
        (re-raise-label      (cl-gensym "bc-cc-reraise-"))
        (saved-outer-sp      (nelisp-bc--ctx-sp ctx)))
    (nelisp-bc--emit-jump ctx 'PUSH-CC handler-entry-label)
    (nelisp-bc--compile-form ctx bodyform)
    (nelisp-bc--emit ctx 'POP-HANDLER)
    (nelisp-bc--emit-jump ctx 'GOTO end-label)
    ;; Handler entry — VM has pushed err onto the stack.
    (nelisp-bc--emit-label ctx handler-entry-label)
    ;; sp tracking: compile-time sp is whatever it was just after we
    ;; emitted the GOTO end-label above.  The VM will set sp to
    ;; pre-body-sp + 1 (the err).  Align the compile ctx accordingly:
    ;; at handler-entry, sp is saved-sp + 1 (err).
    ;; To keep adjust-sp coherent within this arm, we pretend the err
    ;; was just pushed — increment by 1 from the "POP-HANDLER/GOTO"
    ;; level.  Since the previous sp delta from compile-progn left
    ;; body-result on stack (+1), then POP-HANDLER/GOTO left it at the
    ;; same logical depth as handler-entry starts with.
    ;; So: no sp delta adjustment needed — body left a value, handler
    ;; entry starts with a value (err).
    (dolist (handler handlers)
      (let ((conds (car handler))
            (hbody (cdr handler))
            (next-label (cl-gensym "bc-cc-next-")))
        ;; stack: [... err]
        ;; Call (nelisp-bc--cc-match-p CONDS err) — leaves bool above err.
        (let ((idx (nelisp-bc--add-const ctx 'nelisp-bc--cc-match-p)))
          (nelisp-bc--emit ctx 'CONST idx)
          (nelisp-bc--adjust-sp ctx 1))
        (let ((idx (nelisp-bc--add-const ctx conds)))
          (nelisp-bc--emit ctx 'CONST idx)
          (nelisp-bc--adjust-sp ctx 1))
        (nelisp-bc--emit ctx 'STACK-REF 2)
        (nelisp-bc--adjust-sp ctx 1)
        (nelisp-bc--emit ctx 'CALL 2)
        (nelisp-bc--adjust-sp ctx -2)
        ;; stack: [err bool]
        (nelisp-bc--emit-jump ctx 'GOTO-IF-NIL next-label)
        (nelisp-bc--adjust-sp ctx -1)
        ;; Matched.  stack: [err].  Run handler body with VAR bound (if
        ;; VAR is non-nil) to err on its current slot.
        (cond
         ((null var)
          (nelisp-bc--emit ctx 'DROP)
          (nelisp-bc--adjust-sp ctx -1)
          (nelisp-bc--compile-progn ctx hbody))
         ((nelisp-bc--special-p var)
          (let ((cidx (nelisp-bc--add-const ctx var)))
            (nelisp-bc--emit ctx 'VARBIND cidx)
            (nelisp-bc--adjust-sp ctx -1))
          (nelisp-bc--compile-progn ctx hbody)
          (nelisp-bc--emit ctx 'UNBIND 1))
         (t
          ;; Lexical: VAR's slot is the current err slot.
          (let* ((slot (- (nelisp-bc--ctx-sp ctx) 1))
                 (saved-lex (nelisp-bc--ctx-lex-env ctx)))
            (setf (nelisp-bc--ctx-lex-env ctx)
                  (cons (cons var slot) saved-lex))
            (unwind-protect
                (nelisp-bc--compile-progn ctx hbody)
              (setf (nelisp-bc--ctx-lex-env ctx) saved-lex))
            ;; Collapse the err slot under the result.
            (nelisp-bc--emit ctx 'DISCARDN 1)
            (nelisp-bc--adjust-sp ctx -1))))
        (nelisp-bc--emit-jump ctx 'GOTO end-label)
        (nelisp-bc--emit-label ctx next-label)))
    ;; No handler matched: fall through to re-raise.  stack: [err]
    (nelisp-bc--emit-label ctx re-raise-label)
    (let ((idx (nelisp-bc--add-const ctx 'signal)))
      (nelisp-bc--emit ctx 'CONST idx)
      (nelisp-bc--adjust-sp ctx 1))
    (nelisp-bc--emit ctx 'STACK-REF 1)
    (nelisp-bc--adjust-sp ctx 1)
    (nelisp-bc--emit ctx 'CAR)
    (nelisp-bc--emit ctx 'STACK-REF 2)
    (nelisp-bc--adjust-sp ctx 1)
    (nelisp-bc--emit ctx 'CDR)
    (nelisp-bc--emit ctx 'CALL 2)
    (nelisp-bc--adjust-sp ctx -2)
    ;; `signal' never returns.  Every handler branch ends at
    ;; saved-outer-sp + 1 after its GOTO end-label, so force the
    ;; compile-time ctx-sp to match — the re-raise path is
    ;; unreachable past CALL 2 and its transient extra slots
    ;; mustn't leak into the caller's sp tracking.
    (setf (nelisp-bc--ctx-sp ctx) (1+ saved-outer-sp))
    (nelisp-bc--emit-label ctx end-label)))

;;; Lambda + closure (3b.5a) -----------------------------------------

(defun nelisp-bc--parse-params (params)
  "Parse lambda PARAMS list into a property list.
Recognises &optional and &rest.  Returns plist with:
  :positionals — list of param symbols in slot order
  :n-required  — number of required params
  :n-optional  — number of &optional params
  :rest-sym    — &rest symbol or nil
Stack slot N is the Nth :positionals entry; &rest takes one slot
and at runtime holds the remaining-arg list."
  (let ((positionals nil)
        (n-required 0)
        (n-optional 0)
        (rest-sym nil)
        (state 'required)
        (saw-rest nil))
    (while params
      (let ((p (car params)))
        (cond
         ((eq p '&optional)
          (when saw-rest
            (signal 'nelisp-bc-error (list "&optional after &rest")))
          (setq state 'optional)
          (setq params (cdr params)))
         ((eq p '&rest)
          (setq params (cdr params))
          (unless (and params (symbolp (car params)))
            (signal 'nelisp-bc-error (list "&rest without symbol")))
          (setq rest-sym (car params))
          (push rest-sym positionals)
          (setq saw-rest t)
          (setq params nil))
         ((not (symbolp p))
          (signal 'nelisp-bc-error (list "non-symbol param" p)))
         ((or (eq p nil) (eq p t) (keywordp p))
          (signal 'nelisp-bc-error (list "constant cannot be param" p)))
         (t
          (push p positionals)
          (cl-case state
            (required (cl-incf n-required))
            (optional (cl-incf n-optional)))
          (setq params (cdr params))))))
    (list :positionals (nreverse positionals)
          :n-required n-required
          :n-optional n-optional
          :rest-sym rest-sym)))

(defun nelisp-bc--bind-args (params-info args)
  "Materialise ARGS into the per-slot value list dictated by PARAMS-INFO.
Signals nelisp-bc-error for arg-count violations."
  (let* ((n-req (plist-get params-info :n-required))
         (n-opt (plist-get params-info :n-optional))
         (rest-sym (plist-get params-info :rest-sym))
         (n-args (length args))
         (n-min n-req)
         (n-max (if rest-sym most-positive-fixnum (+ n-req n-opt))))
    (when (< n-args n-min)
      (signal 'nelisp-bc-error
              (list "too few args" n-args n-min)))
    (when (> n-args n-max)
      (signal 'nelisp-bc-error
              (list "too many args" n-args n-max)))
    (let ((result nil))
      ;; Required + optional supplied
      (let ((take (min n-args (+ n-req n-opt))))
        (dotimes (i take)
          (push (nth i args) result))
        ;; Optional missing — push nil
        (dotimes (_ (- (+ n-req n-opt) take))
          (push nil result)))
      ;; &rest collects the tail
      (when rest-sym
        (push (nthcdr (+ n-req n-opt) args) result))
      (nreverse result))))

(defun nelisp-bc--compile-lambda (ctx params body)
  "Compile (lambda PARAMS BODY...) into a sub-bcl pushed onto the stack.

3b.5a does not yet capture free lex vars; if BODY references an
outer lex symbol, signal `nelisp-bc-unimplemented' so callers
fall back to the interpreter."
  (let* ((info (nelisp-bc--parse-params params))
         (positionals (plist-get info :positionals))
         (n-positionals (length positionals))
         (sub-ctx (nelisp-bc--ctx-new))
         (lex-env nil)
         (special-mask 0)
         (special-slots nil)
         ;; Sub-ctx's "shadow set" of capturable outer-scope symbols.
         ;; Includes everything the parent ctx can resolve at its
         ;; level: its lexical bindings AND its own captures (which
         ;; chain back to outer enclosing contexts).  Param names
         ;; cancel their shadow, matching let-shadowing semantics.
         (parent-lex-names (mapcar #'car (nelisp-bc--ctx-lex-env ctx)))
         (parent-cap-names (mapcar #'car (nelisp-bc--ctx-captures ctx)))
         (shadow-source (append parent-lex-names parent-cap-names
                                (nelisp-bc--ctx-parent-lex-shadow ctx)))
         (shadow (cl-set-difference shadow-source positionals)))
    (setf (nelisp-bc--ctx-parent-lex-shadow sub-ctx) shadow)
    ;; Sub-ctx starts with all positionals already pushed: sp = N.
    (setf (nelisp-bc--ctx-sp sub-ctx) n-positionals)
    (setf (nelisp-bc--ctx-max-sp sub-ctx) n-positionals)
    ;; Classify each positional.
    (let ((slot 0))
      (dolist (sym positionals)
        (cond
         ((nelisp-bc--special-p sym)
          (setq special-mask (logior special-mask (ash 1 slot)))
          (push (cons sym slot) special-slots))
         (t
          (push (cons sym slot) lex-env)))
        (cl-incf slot)))
    (setf (nelisp-bc--ctx-lex-env sub-ctx) (nreverse lex-env))
    ;; Preamble: VARBIND each special.  STACK-REF the slot to TOS,
    ;; then VARBIND pops it; the original slot stays on the stack as
    ;; dead weight that the postamble's DISCARDN will collapse.
    (dolist (entry (nreverse special-slots))
      (let* ((sym (car entry))
             (slot (cdr entry))
             (offset (- (nelisp-bc--ctx-sp sub-ctx) 1 slot)))
        (when (or (< offset 0) (> offset 255))
          (signal 'nelisp-bc-unimplemented
                  (list "lambda special slot out of range" sym offset)))
        (nelisp-bc--emit sub-ctx 'STACK-REF offset)
        (nelisp-bc--adjust-sp sub-ctx 1)
        (let ((idx (nelisp-bc--add-const sub-ctx sym)))
          (nelisp-bc--emit sub-ctx 'VARBIND idx)
          (nelisp-bc--adjust-sp sub-ctx -1))))
    ;; Body.
    (nelisp-bc--compile-progn sub-ctx body)
    ;; Postamble: UNBIND specials, DISCARDN positional slots.
    (let ((n-dyn (length special-slots)))
      (when (> n-dyn 0)
        (nelisp-bc--emit sub-ctx 'UNBIND n-dyn)))
    (when (> n-positionals 0)
      (when (> n-positionals 255)
        (signal 'nelisp-bc-unimplemented
                (list "lambda with > 255 params" n-positionals)))
      (nelisp-bc--emit sub-ctx 'DISCARDN n-positionals)
      (nelisp-bc--adjust-sp sub-ctx (- n-positionals)))
    (nelisp-bc--emit sub-ctx 'RETURN)
    ;; Build sub-bcl and have the parent push it as a constant.  If
    ;; the body captured any free-lex vars, emit pushes for each (in
    ;; capture-index order) followed by CONST + MAKE-CLOSURE so the
    ;; runtime instantiates a fresh closure with ENV filled in.
    (let* ((code (nelisp-bc--resolve (nelisp-bc--ctx-insns sub-ctx)))
           (consts (apply #'vector (nelisp-bc--ctx-consts sub-ctx)))
           (max-sp (max 1 (nelisp-bc--ctx-max-sp sub-ctx)))
           (captures (nelisp-bc--ctx-captures sub-ctx))
           (template (nelisp-bc-make nil params consts code
                                     max-sp special-mask)))
      (cond
       ((null captures)
        (let ((idx (nelisp-bc--add-const ctx template)))
          (nelisp-bc--emit ctx 'CONST idx)
          (nelisp-bc--adjust-sp ctx 1)))
       (t
        ;; Sort captures by index just to be safe (we always append
        ;; in order so this is a no-op, but stay defensive).
        (let ((sorted (sort (copy-sequence captures)
                            (lambda (a b) (< (cdr a) (cdr b))))))
          (dolist (cap sorted)
            ;; `compile-var-ref' on the parent ctx resolves the sym
            ;; — STACK-REF for parent lex, CAPTURED-REF for parent
            ;; capture, or auto-captures it from grandparent (which
            ;; chains the capture out one more level).
            (nelisp-bc--compile-var-ref ctx (car cap)))
          (let ((idx (nelisp-bc--add-const ctx template)))
            (nelisp-bc--emit ctx 'CONST idx)
            (nelisp-bc--adjust-sp ctx 1))
          (let ((n (length sorted)))
            (when (> n 255)
              (signal 'nelisp-bc-unimplemented
                      (list "captures > 255 entries pending later phase")))
            (nelisp-bc--emit ctx 'MAKE-CLOSURE n)
            ;; MAKE-CLOSURE pops N captures + 1 template, pushes 1
            ;; closure → net delta is -N.
            (nelisp-bc--adjust-sp ctx (- n)))))))))

;;; Auto-compile entry (3b.5c) ---------------------------------------

(defvar nelisp-bc-auto-compile nil
  "When non-nil, the interpreter calls `nelisp-bc-try-compile-lambda'
to materialise top-level `defun' / `lambda' bodies as bytecode
closures.

Default is nil — opt-in for now.  Two reasons:

  1. Three Phase 1/2 ERTs sample the closure tag via
     `(eq (car cl) \\='nelisp-closure)' or expect arity violations
     to signal `nelisp-eval-error' rather than `nelisp-bc-error',
     and one macro test relies on macros defined AFTER a `defun'
     being expanded at call time (a feature compiled bytecode
     cannot recapture without re-compilation).
  2. The self-host probe and trampoline cycle tests evaluate
     entire source files at NeLisp level; auto-compiling every
     defun en route adds enough host stack frames per recursion
     to trip `max-lisp-eval-depth'.

Bind it non-nil for benchmarks (Phase 3b.7) or once the above
items are addressed.")

(defun nelisp-bc-try-compile-lambda (env params body)
  "Attempt to compile (lambda PARAMS BODY) into a `nelisp-bcl'.

Returns a bcl on success and nil on any failure so callers fall
back to the interpreter closure.  Currently only top-level
closures (ENV must be nil) are auto-compiled — closures with a
non-nil interpreter ENV need values lifted into a bcl ENV at
runtime, which a future sub-phase will tackle.

MCP Parameters:
  ENV    — interpreter lexical env at closure-creation time
  PARAMS — lambda-list (required / &optional / &rest)
  BODY   — list of NeLisp forms"
  (when (and nelisp-bc-auto-compile (null env))
    (condition-case nil
        (let ((wrapper (nelisp-bc-compile
                        (cons 'lambda (cons params body)))))
          ;; Evaluating the wrapper materialises the inner template
          ;; (no captures here since ENV was nil) as a bcl, which is
          ;; what we want to install in `nelisp--functions'.
          (nelisp-bc-run wrapper))
      (nelisp-bc-error nil))))

(defun nelisp-bc--cc-match-p (handler-spec err)
  "Return non-nil if ERR matches HANDLER-SPEC per `condition-case' rules."
  (let ((conditions (and (consp err)
                         (symbolp (car err))
                         (get (car err) 'error-conditions))))
    (cond
     ((eq handler-spec t) t)
     ((symbolp handler-spec) (memq handler-spec conditions))
     ((listp handler-spec)
      (catch 'nelisp-bc--cc-done
        (dolist (s handler-spec)
          (when (memq s conditions)
            (throw 'nelisp-bc--cc-done t)))
        nil))
     (t nil))))

;;; Compiler entry ---------------------------------------------------

(defun nelisp-bc-compile (form &optional env)
  "Compile FORM to a `nelisp-bcl'.
Phase 3b.3 adds arithmetic / comparison primitives and `if' / `cond'
/ `progn' special forms on top of 3b.2's literal support.  Forms that
exceed the supported surface signal `nelisp-bc-unimplemented'.

MCP Parameters:
  FORM  — NeLisp source form
  ENV   — optional lexical env alist"
  (let ((ctx (nelisp-bc--ctx-new)))
    (nelisp-bc--compile-form ctx form)
    (nelisp-bc--emit ctx 'RETURN)
    (let* ((code (nelisp-bc--resolve (nelisp-bc--ctx-insns ctx)))
           (consts (apply #'vector (nelisp-bc--ctx-consts ctx)))
           (max-sp (max 1 (nelisp-bc--ctx-max-sp ctx))))
      (nelisp-bc-make env nil consts code max-sp 0))))

;;; VM dispatch loop -------------------------------------------------

;; Indices into the `vm' state vector threaded through
;; `nelisp-bc--dispatch'.  Hoisting the VM state into a flat vector
;; (instead of capturing it as lexicals in a `cl-labels' closure)
;; means the main dispatcher can be a top-level defun — so the
;; hot-loop closure that used to be allocated per `nelisp-bc-run'
;; entry is gone.  fib(20) saves ~21k closure allocations per run.
(defconst nelisp-bc--vm-code        0)
(defconst nelisp-bc--vm-consts      1)
(defconst nelisp-bc--vm-stack       2)
(defconst nelisp-bc--vm-stack-depth 3)
(defconst nelisp-bc--vm-len         4)
(defconst nelisp-bc--vm-closure-env 5)
(defconst nelisp-bc--vm-pc          6)
(defconst nelisp-bc--vm-sp          7)
(defconst nelisp-bc--vm-specpdl     8)

(defun nelisp-bc--dispatch (vm nested)
  "Opcode dispatch loop for the bytecode VM.
VM is the state vector described by `nelisp-bc--vm-*' index
constants.  When NESTED is non-nil, POP-HANDLER cleanly exits this
invocation via `throw ''nelisp-bc--pop-handler'; when nil, POP-HANDLER
is an error because no surrounding dispatcher exists on the host
stack to catch it.

`pc', `sp' and `specpdl' are unpacked into fast locals on entry
and synced back to VM inside `unwind-protect' so every exit path
(normal and non-local) leaves VM with the latest values.  Nested
invocations from PUSH-CATCH / PUSH-UNWIND / PUSH-CC commit before
recursing and reload from VM afterwards."
  (let ((code        (aref vm 0))
        (consts      (aref vm 1))
        (stack       (aref vm 2))
        (stack-depth (aref vm 3))
        (len         (aref vm 4))
        (closure-env (aref vm 5))
        (pc          (aref vm 6))
        (sp          (aref vm 7))
        (specpdl     (aref vm 8)))
    (cl-macrolet ((read-u16 ()
                    `(prog1 (+ (aref code pc)
                               (ash (aref code (1+ pc)) 8))
                       (setq pc (+ pc 2))))
                  (trim-specpdl (target-tail)
                    `(while (not (eq specpdl ,target-tail))
                       (let* ((entry (pop specpdl))
                              (sym (car entry))
                              (old (cdr entry)))
                         (if (eq old nelisp--unbound)
                             (remhash sym nelisp--globals)
                           (puthash sym old nelisp--globals)))))
                  (commit-vm ()
                    '(progn (aset vm 6 pc)
                            (aset vm 7 sp)
                            (aset vm 8 specpdl)))
                  (reload-vm ()
                    '(progn (setq pc      (aref vm 6))
                            (setq sp      (aref vm 7))
                            (setq specpdl (aref vm 8)))))
      (unwind-protect
          (catch 'nelisp-bc--pop-handler
            (while (< pc len)
              (let ((op (aref code pc)))
                (setq pc (1+ pc))
                (pcase op
                 (0
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "RETURN on empty stack" pc)))
              (throw 'nelisp-bc--return (aref stack (1- sp))))
             (1
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error (list "stack overflow at CONST" pc)))
              (let ((idx (aref code pc)))
                (setq pc (1+ pc))
                (aset stack sp (aref consts idx)))
              (setq sp (1+ sp)))
             (2
              (let* ((off (aref code pc))
                     (src (- sp off 1)))
                (setq pc (1+ pc))
                (when (or (< src 0) (>= sp stack-depth))
                  (signal 'nelisp-bc-error
                          (list "STACK-REF out of bounds" off sp)))
                (aset stack sp (aref stack src))
                (setq sp (1+ sp))))
             (3
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "DROP on empty stack" pc)))
              (setq sp (1- sp)))
             (4
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "DUP on empty stack" pc)))
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error (list "stack overflow at DUP" pc)))
              (aset stack sp (aref stack (1- sp)))
              (setq sp (1+ sp)))
             (5
              (setq pc (read-u16)))
             (6
              (when (<= sp 0)
                (signal 'nelisp-bc-error
                        (list "GOTO-IF-NIL on empty stack" pc)))
              (setq sp (1- sp))
              (let ((tgt (read-u16))
                    (v (aref stack sp)))
                (when (null v) (setq pc tgt))))
             (7
              (when (<= sp 0)
                (signal 'nelisp-bc-error
                        (list "GOTO-IF-NOT-NIL on empty stack" pc)))
              (setq sp (1- sp))
              (let ((tgt (read-u16))
                    (v (aref stack sp)))
                (when v (setq pc tgt))))
             (8
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "ADD1 on empty stack" pc)))
              (aset stack (1- sp) (1+ (aref stack (1- sp)))))
             (9
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "SUB1 on empty stack" pc)))
              (aset stack (1- sp) (1- (aref stack (1- sp)))))
             (10
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "PLUS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (+ a b))
                (setq sp (1- sp))))
             (11
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "MINUS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (- a b))
                (setq sp (1- sp))))
             (12
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "LESS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (< a b))
                (setq sp (1- sp))))
             (13
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "GREATER needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (> a b))
                (setq sp (1- sp))))
             (14
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "EQ needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (eq a b))
                (setq sp (1- sp))))
             (15
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "NOT on empty stack" pc)))
              (aset stack (1- sp) (not (aref stack (1- sp)))))
             (16
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error (list "stack overflow at VARREF" pc)))
              (let* ((idx (aref code pc))
                     (sym (aref consts idx))
                     (val (gethash sym nelisp--globals nelisp--unbound)))
                (setq pc (1+ pc))
                (when (eq val nelisp--unbound)
                  (signal 'nelisp-unbound-variable (list sym)))
                (aset stack sp val)
                (setq sp (1+ sp))))
             (17
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "VARSET on empty stack" pc)))
              (let* ((idx (aref code pc))
                     (sym (aref consts idx))
                     (val (aref stack (1- sp))))
                (setq pc (1+ pc))
                (puthash sym val nelisp--globals)
                (setq sp (1- sp))))
             (18
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "VARBIND on empty stack" pc)))
              (let* ((idx (aref code pc))
                     (sym (aref consts idx))
                     (val (aref stack (1- sp)))
                     (old (gethash sym nelisp--globals nelisp--unbound)))
                (setq pc (1+ pc))
                (push (cons sym old) specpdl)
                (puthash sym val nelisp--globals)
                (setq sp (1- sp))))
             (19
              (let ((n (aref code pc)))
                (setq pc (1+ pc))
                (dotimes (_ n)
                  (unless specpdl
                    (signal 'nelisp-bc-error
                            (list "UNBIND with empty specpdl" pc)))
                  (let* ((entry (pop specpdl))
                         (sym (car entry))
                         (old (cdr entry)))
                    (if (eq old nelisp--unbound)
                        (remhash sym nelisp--globals)
                      (puthash sym old nelisp--globals))))))
             (20
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "STACK-SET on empty stack" pc)))
              (let* ((offset (aref code pc))
                     (dest (- sp offset 1)))
                (setq pc (1+ pc))
                (when (or (< dest 0) (>= dest sp))
                  (signal 'nelisp-bc-error
                          (list "STACK-SET out of bounds" offset sp)))
                (aset stack dest (aref stack (1- sp)))
                (setq sp (1- sp))))
             (21
              (let ((n (aref code pc)))
                (setq pc (1+ pc))
                (when (<= sp 0)
                  (signal 'nelisp-bc-error (list "DISCARDN on empty stack" pc)))
                (when (> n (1- sp))
                  (signal 'nelisp-bc-error
                          (list "DISCARDN underflow" n sp)))
                (unless (zerop n)
                  ;; Move TOS down by N, then shrink sp by N.
                  (aset stack (- sp 1 n) (aref stack (1- sp)))
                  (setq sp (- sp n)))))
             (22
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "CAR on empty stack" pc)))
              (aset stack (1- sp) (car (aref stack (1- sp)))))
             (23
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "CDR on empty stack" pc)))
              (aset stack (1- sp) (cdr (aref stack (1- sp)))))
             (24
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "CONS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (cons a b))
                (setq sp (1- sp))))
             (25
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "LIST1 on empty stack" pc)))
              (aset stack (1- sp) (list (aref stack (1- sp)))))
             (26
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "LIST2 needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (list a b))
                (setq sp (1- sp))))
             (27
              (when (< sp 3)
                (signal 'nelisp-bc-error (list "LIST3 needs 3 values" pc)))
              (let ((c (aref stack (1- sp)))
                    (b (aref stack (- sp 2)))
                    (a (aref stack (- sp 3))))
                (aset stack (- sp 3) (list a b c))
                (setq sp (- sp 2))))
             (28
              (when (< sp 4)
                (signal 'nelisp-bc-error (list "LIST4 needs 4 values" pc)))
              (let ((d (aref stack (1- sp)))
                    (c (aref stack (- sp 2)))
                    (b (aref stack (- sp 3)))
                    (a (aref stack (- sp 4))))
                (aset stack (- sp 4) (list a b c d))
                (setq sp (- sp 3))))
             (29
              (let ((n (aref code pc)))
                (setq pc (1+ pc))
                (when (< sp n)
                  (signal 'nelisp-bc-error
                          (list "LISTN underflow" n sp)))
                (let ((acc nil))
                  (dotimes (_ n)
                    (setq sp (1- sp))
                    (push (aref stack sp) acc))
                  (aset stack sp acc)
                  (setq sp (1+ sp)))))
             (30
              (let ((nargs (aref code pc)))
                (setq pc (1+ pc))
                (when (< sp (1+ nargs))
                  (signal 'nelisp-bc-error
                          (list "CALL underflow" nargs sp)))
                (let ((call-args nil))
                  (dotimes (_ nargs)
                    (setq sp (1- sp))
                    (push (aref stack sp) call-args))
                  (setq sp (1- sp))
                  (let* ((fn (aref stack sp))
                         ;; Fast path: resolve symbol → bcl inline so the
                         ;; recursive self-call pattern (fib → fib → …)
                         ;; skips `nelisp--apply' entirely.  gethash here
                         ;; matches `nelisp--apply's symbol arm exactly.
                         (bcl (cond
                               ((and (consp fn) (eq (car fn) 'nelisp-bcl))
                                fn)
                               ((symbolp fn)
                                (let ((nfn (gethash fn nelisp--functions
                                                    nelisp--unbound)))
                                  (and (consp nfn)
                                       (eq (car nfn) 'nelisp-bcl)
                                       nfn)))))
                         (result (if bcl
                                     (nelisp-bc-run bcl call-args)
                                   (nelisp--apply fn call-args))))
                    (aset stack sp result)
                    (setq sp (1+ sp))))))
             (32
              (if nested
                  (throw 'nelisp-bc--pop-handler nil)
                (signal 'nelisp-bc-error
                        (list "POP-HANDLER outside any handler" pc))))
             (33
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "THROW needs 2 values" pc)))
              (let ((val (aref stack (1- sp)))
                    (tag (aref stack (- sp 2))))
                (setq sp (- sp 2))
                (throw tag val)))
             (31
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "PUSH-CATCH on empty stack" pc)))
              (let* ((target (read-u16))
                     (tag (aref stack (1- sp)))
                     (_ (setq sp (1- sp)))
                     (saved-sp sp)
                     (saved-specpdl specpdl)
                     ;; Fresh cons-identity sentinel so user's `throw'
                     ;; can never collide with our "body completed"
                     ;; marker, no matter what value they throw.
                     (normal-sentinel (list 'nelisp-bc--normal-catch))
                     ;; Sync local pc/sp/specpdl into VM before
                     ;; recursing — nested dispatch sees the latest
                     ;; state, and its `unwind-protect' commits its
                     ;; final state back so we can `reload-vm' below.
                     (outcome (catch tag
                                (commit-vm)
                                (nelisp-bc--dispatch vm t)
                                normal-sentinel)))
                (reload-vm)
                (cond
                 ((eq outcome normal-sentinel)
                  ;; Body completed normally: POP-HANDLER ran and the
                  ;; dispatcher returned.  Stack already holds body's
                  ;; value (+1); pc advanced past POP-HANDLER.
                  nil)
                 (t
                  ;; Throw matched tag: unwind specpdl and stack to the
                  ;; pre-body snapshot, push thrown value, jump.
                  (trim-specpdl saved-specpdl)
                  (setq sp saved-sp)
                  (when (>= sp stack-depth)
                    (signal 'nelisp-bc-error
                            (list "stack overflow at CATCH land" pc)))
                  (aset stack sp outcome)
                  (setq sp (1+ sp))
                  (setq pc target)))))
             (34
              (let* ((cleanup-target (read-u16))
                     (saved-sp sp)
                     (saved-specpdl specpdl)
                     (body-done nil))
                (unwind-protect
                    (progn (commit-vm)
                           (nelisp-bc--dispatch vm t)
                           (reload-vm)
                           (setq body-done t))
                  ;; Always-run cleanup.  On non-local exit, reset the
                  ;; operand stack and specpdl to the pre-body state so
                  ;; the cleanup bytecode starts from a clean slate.
                  (unless body-done
                    ;; Body threw: nested dispatch's unwind-protect
                    ;; already synced its final state to VM, so reload
                    ;; into our locals before trimming.
                    (reload-vm)
                    (trim-specpdl saved-specpdl)
                    (setq sp saved-sp))
                  (let ((saved-pc pc))
                    (setq pc cleanup-target)
                    (commit-vm)
                    (nelisp-bc--dispatch vm t)
                    (reload-vm)
                    (when (not body-done)
                      ;; Exception path: propagate by restoring pc
                      ;; (not strictly needed — host will re-raise —
                      ;; but keeps debugger-friendly state).
                      (setq pc saved-pc))))))
             (35
              (let* ((handler-target (read-u16))
                     (saved-sp sp)
                     (saved-specpdl specpdl)
                     (err (condition-case e
                              (progn (commit-vm)
                                     (nelisp-bc--dispatch vm t)
                                     nil)
                            (error e))))
                (reload-vm)
                (when err
                  (trim-specpdl saved-specpdl)
                  (setq sp saved-sp)
                  (when (>= sp stack-depth)
                    (signal 'nelisp-bc-error
                            (list "stack overflow at PUSH-CC land" pc)))
                  (aset stack sp err)
                  (setq sp (1+ sp))
                  (setq pc handler-target))))
             (36
              (let ((n (aref code pc)))
                (setq pc (1+ pc))
                (when (< sp (1+ n))
                  (signal 'nelisp-bc-error
                          (list "MAKE-CLOSURE underflow" n sp)))
                (let* ((template (aref stack (1- sp)))
                       (env nil))
                  (unless (and (consp template)
                               (eq (car template) 'nelisp-bcl))
                    (signal 'nelisp-bc-error
                            (list "MAKE-CLOSURE non-template" template)))
                  ;; Captures lie at stack[sp-2-n+1 .. sp-2] in slot
                  ;; order; build the env list by walking them in
                  ;; reverse so env's car = capture index 0.
                  (dotimes (i n)
                    (push (aref stack (- sp 2 i)) env))
                  (setq env (nreverse env))
                  (setq sp (- sp (1+ n)))
                  (let ((closure (nelisp-bc-make
                                  env
                                  (nelisp-bc-params template)
                                  (nelisp-bc-consts template)
                                  (nelisp-bc-code template)
                                  (nelisp-bc-stack-depth template)
                                  (nelisp-bc-special-mask template))))
                    (aset stack sp closure)
                    (setq sp (1+ sp))))))
             (37
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error
                        (list "stack overflow at CAPTURED-REF" pc)))
              (let ((idx (aref code pc)))
                (setq pc (1+ pc))
                (when (or (< idx 0) (>= idx (length closure-env)))
                  (signal 'nelisp-bc-error
                          (list "CAPTURED-REF out of range"
                                idx (length closure-env))))
                (aset stack sp (nth idx closure-env))
                (setq sp (1+ sp))))
                 (_ (signal 'nelisp-bc-error (list "unknown opcode"
                                (aref nelisp-bc--opcode-names op)
                                op (1- pc)))))))
            (unless nested
              (signal 'nelisp-bc-error
                      (list "fell off code without RETURN" pc))))
        ;; unwind-protect cleanup: sync mutable locals back to VM so
        ;; callers (or `nelisp-bc-run's outer unwind-protect) observe
        ;; the latest pc/sp/specpdl on every exit, normal or non-local.
        (aset vm 6 pc)
        (aset vm 7 sp)
        (aset vm 8 specpdl)))))

(defun nelisp-bc-run (bcl &optional args)
  "Execute the bytecode closure BCL.

Phase 3b.4a/b expand the VM with variable ops (VARREF / VARSET /
VARBIND / UNBIND / STACK-SET / DISCARDN) and list / function-call
ops (CAR / CDR / CONS / LIST1..4 / LISTN / CALL) so `let' / `let*'
/ `setq' / `while' plus arbitrary primitive and NeLisp-defined
function calls can be compiled.  Argument binding for `lambda'
still arrives in 3b.5; passing a non-nil ARGS continues to signal.

MCP Parameters:
  BCL   — `nelisp-bcl' object (see commentary §4.1)
  ARGS  — optional list of positional arguments"
  (unless (nelisp-bcl-p bcl)
    (signal 'nelisp-bc-error (list "not a nelisp-bcl" bcl)))
  (if (eq (nelisp-bc-env bcl) 'nelisp-jit-marker)
      ;; Phase 3b.8 JIT fast-path: env slot carries the marker and the
      ;; code slot is a host-compiled lambda.  Apply directly, skip
      ;; VM setup entirely.  Outer `nelisp--apply' dispatch arm is the
      ;; same (bcl-tagged callable) so self-host path stays unchanged.
      (apply (nelisp-bc-code bcl) args)
    (let* ((code (nelisp-bc-code bcl))
         (consts (nelisp-bc-consts bcl))
         (closure-env (nelisp-bc-env bcl))
         (params (nelisp-bc-params bcl))
         ;; Materialise any args into per-slot values.  No params and
         ;; no args is the "expression" case; otherwise dispatch to
         ;; the parser so &optional / &rest are honoured.
         (slot-values (cond
                       ((and (null params) (null args)) nil)
                       (t (nelisp-bc--bind-args
                           (nelisp-bc--parse-params params)
                           args))))
         (n-slots (length slot-values))
         (body-stack-depth (max 1 (or (nelisp-bc-stack-depth bcl) 1)))
         (stack-depth (max body-stack-depth n-slots))
         (stack (make-vector stack-depth nil))
         ;; Flat state vector hands the bcl context into the top-level
         ;; `nelisp-bc--dispatch' without forcing it to be a `cl-labels'
         ;; closure.  Eliminating that closure removes one heap alloc
         ;; per bcl call (fib(20) ≈ 21k saved per timing run).
         (vm (vector code consts stack stack-depth (length code)
                     closure-env 0 n-slots nil)))
    ;; Pre-fill positional slots with the materialised args so the
    ;; body's STACK-REF / VARBIND preamble finds them at slot 0..N-1.
    (let ((i 0))
      (dolist (v slot-values)
        (aset stack i v)
        (cl-incf i)))
    ;; Push VM onto the Phase 3c active-VM stack so `nelisp-gc-root-set'
    ;; can reach in-flight stack / specpdl from a concurrent mark pass.
    ;; Dynamic `let' + `unwind-protect' below guarantees pop on both
    ;; normal and non-local exit.
    (let ((nelisp-gc--active-vms (cons vm nelisp-gc--active-vms)))
      (unwind-protect
          (catch 'nelisp-bc--return
            (nelisp-bc--dispatch vm nil))
        ;; Restore any outstanding specpdl entries in reverse order so
        ;; duplicate binds layer the same way `let' does.  Reads specpdl
        ;; from VM because `nelisp-bc--dispatch' syncs on exit (normal
        ;; fall-out is a no-op when bytecode is well-formed; non-local
        ;; exit unwinds whatever bindings the body accumulated).
        (let ((specpdl (aref vm 8)))
          (while specpdl
            (let* ((entry (pop specpdl))
                   (sym (car entry))
                   (old (cdr entry)))
              (if (eq old nelisp--unbound)
                  (remhash sym nelisp--globals)
                (puthash sym old nelisp--globals))))))))))

(provide 'nelisp-bytecode)
;;; nelisp-bytecode.el ends here
