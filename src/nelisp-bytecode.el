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
    (NOT             15 0))
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
  (max-sp 0))

(defun nelisp-bc--ctx-new ()
  (nelisp-bc--ctx--make :consts nil :insns nil :sp 0 :max-sp 0))

(defun nelisp-bc--self-evaluating-p (form)
  "Non-nil if FORM evaluates to itself with no further work."
  (or (null form)
      (eq form t)
      (keywordp form)
      (numberp form)
      (stringp form)
      (vectorp form)))

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
   ((and (consp form) (memq (car form) '(+ - < > eq not 1+ 1-)))
    (nelisp-bc--compile-primitive ctx (car form) (cdr form)))
   (t
    (signal 'nelisp-bc-unimplemented
            (list "compiler cannot handle form yet" form)))))

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

(defun nelisp-bc-run (bcl &optional args)
  "Execute the bytecode closure BCL.

Phase 3b.2 VM: operand stack + PC loop, five opcodes.  ARGS is
currently unused — parameter binding arrives in 3b.4 once VARBIND
lands.  Passing a non-nil ARGS to a 3b.2 closure signals, so we
don't silently drop arguments.

MCP Parameters:
  BCL   — `nelisp-bcl' object (see commentary §4.1)
  ARGS  — optional list of positional arguments"
  (unless (nelisp-bcl-p bcl)
    (signal 'nelisp-bc-error (list "not a nelisp-bcl" bcl)))
  (when args
    (signal 'nelisp-bc-unimplemented
            (list "argument binding pending Phase 3b.4")))
  (let* ((code (nelisp-bc-code bcl))
         (consts (nelisp-bc-consts bcl))
         (stack-depth (max 1 (or (nelisp-bc-stack-depth bcl) 1)))
         (stack (make-vector stack-depth nil))
         (sp 0)
         (pc 0)
         (len (length code))
         (op-RETURN          (nelisp-bc-opcode 'RETURN))
         (op-CONST           (nelisp-bc-opcode 'CONST))
         (op-STACK-REF       (nelisp-bc-opcode 'STACK-REF))
         (op-DROP            (nelisp-bc-opcode 'DROP))
         (op-DUP             (nelisp-bc-opcode 'DUP))
         (op-GOTO            (nelisp-bc-opcode 'GOTO))
         (op-GOTO-IF-NIL     (nelisp-bc-opcode 'GOTO-IF-NIL))
         (op-GOTO-IF-NOT-NIL (nelisp-bc-opcode 'GOTO-IF-NOT-NIL))
         (op-ADD1            (nelisp-bc-opcode 'ADD1))
         (op-SUB1            (nelisp-bc-opcode 'SUB1))
         (op-PLUS            (nelisp-bc-opcode 'PLUS))
         (op-MINUS           (nelisp-bc-opcode 'MINUS))
         (op-LESS            (nelisp-bc-opcode 'LESS))
         (op-GREATER         (nelisp-bc-opcode 'GREATER))
         (op-EQ              (nelisp-bc-opcode 'EQ))
         (op-NOT             (nelisp-bc-opcode 'NOT))
         result)
    (cl-flet ((read-u16 ()
                (prog1 (+ (aref code pc)
                          (ash (aref code (1+ pc)) 8))
                  (setq pc (+ pc 2)))))
      (catch 'nelisp-bc--return
        (while (< pc len)
          (let ((op (aref code pc)))
            (setq pc (1+ pc))
            (cond
             ((= op op-RETURN)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "RETURN on empty stack" pc)))
              (setq result (aref stack (1- sp)))
              (setq sp (1- sp))
              (throw 'nelisp-bc--return result))
             ((= op op-CONST)
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error (list "stack overflow at CONST" pc)))
              (let ((idx (aref code pc)))
                (setq pc (1+ pc))
                (aset stack sp (aref consts idx)))
              (setq sp (1+ sp)))
             ((= op op-STACK-REF)
              (let* ((off (aref code pc))
                     (src (- sp off 1)))
                (setq pc (1+ pc))
                (when (or (< src 0) (>= sp stack-depth))
                  (signal 'nelisp-bc-error
                          (list "STACK-REF out of bounds" off sp)))
                (aset stack sp (aref stack src))
                (setq sp (1+ sp))))
             ((= op op-DROP)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "DROP on empty stack" pc)))
              (setq sp (1- sp)))
             ((= op op-DUP)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "DUP on empty stack" pc)))
              (when (>= sp stack-depth)
                (signal 'nelisp-bc-error (list "stack overflow at DUP" pc)))
              (aset stack sp (aref stack (1- sp)))
              (setq sp (1+ sp)))
             ((= op op-GOTO)
              (setq pc (read-u16)))
             ((= op op-GOTO-IF-NIL)
              (when (<= sp 0)
                (signal 'nelisp-bc-error
                        (list "GOTO-IF-NIL on empty stack" pc)))
              (setq sp (1- sp))
              (let ((tgt (read-u16))
                    (v (aref stack sp)))
                (when (null v) (setq pc tgt))))
             ((= op op-GOTO-IF-NOT-NIL)
              (when (<= sp 0)
                (signal 'nelisp-bc-error
                        (list "GOTO-IF-NOT-NIL on empty stack" pc)))
              (setq sp (1- sp))
              (let ((tgt (read-u16))
                    (v (aref stack sp)))
                (when v (setq pc tgt))))
             ((= op op-ADD1)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "ADD1 on empty stack" pc)))
              (aset stack (1- sp) (1+ (aref stack (1- sp)))))
             ((= op op-SUB1)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "SUB1 on empty stack" pc)))
              (aset stack (1- sp) (1- (aref stack (1- sp)))))
             ((= op op-PLUS)
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "PLUS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (+ a b))
                (setq sp (1- sp))))
             ((= op op-MINUS)
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "MINUS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (- a b))
                (setq sp (1- sp))))
             ((= op op-LESS)
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "LESS needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (< a b))
                (setq sp (1- sp))))
             ((= op op-GREATER)
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "GREATER needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (> a b))
                (setq sp (1- sp))))
             ((= op op-EQ)
              (when (< sp 2)
                (signal 'nelisp-bc-error (list "EQ needs 2 values" pc)))
              (let ((b (aref stack (1- sp)))
                    (a (aref stack (- sp 2))))
                (aset stack (- sp 2) (eq a b))
                (setq sp (1- sp))))
             ((= op op-NOT)
              (when (<= sp 0)
                (signal 'nelisp-bc-error (list "NOT on empty stack" pc)))
              (aset stack (1- sp) (not (aref stack (1- sp)))))
             (t
              (signal 'nelisp-bc-error
                      (list "unknown opcode"
                            (aref nelisp-bc--opcode-names op)
                            op (1- pc)))))))
        (signal 'nelisp-bc-error (list "fell off code without RETURN" pc))))
    result))

(provide 'nelisp-bytecode)
;;; nelisp-bytecode.el ends here
