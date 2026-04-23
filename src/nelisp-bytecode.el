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
;; Name       Byte  Arg-bytes  Semantics
;; RETURN       0        0     pop top of stack, return as BCL result
;; CONST        1        1     push CONSTS[uint8] onto stack
;; STACK-REF    2        1     push copy of stack[sp - uint8 - 1]
;; DROP         3        0     pop top, discard
;; DUP          4        0     push copy of top

(defconst nelisp-bc--opcode-table
  '((RETURN    0 0)
    (CONST     1 1)
    (STACK-REF 2 1)
    (DROP      3 0)
    (DUP       4 0))
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

;;; Compiler / VM stubs ----------------------------------------------

;;; Compiler ---------------------------------------------------------

(defun nelisp-bc--self-evaluating-p (form)
  "Non-nil if FORM evaluates to itself with no further work.
Covers the atoms Phase 3b.2 supports: nil, t, keywords, numbers,
strings, vectors."
  (or (null form)
      (eq form t)
      (keywordp form)
      (numberp form)
      (stringp form)
      (vectorp form)))

(defun nelisp-bc-compile (form &optional env)
  "Compile FORM to a `nelisp-bcl'.

Phase 3b.2 handles only self-evaluating atoms and `(quote X)' forms;
the result is a zero-arg closure whose body pushes one constant and
returns.  Other forms signal `nelisp-bc-unimplemented' — subsequent
sub-phases (3b.3+) broaden the surface.

ENV is captured verbatim into the resulting closure's ENV field so
downstream sub-phases can thread lexical bindings through.

MCP Parameters:
  FORM  — NeLisp source form (cons / atom)
  ENV   — optional lexical env alist captured at compile time"
  (let ((value
         (cond
          ((nelisp-bc--self-evaluating-p form) form)
          ((and (consp form) (eq (car form) 'quote)
                (consp (cdr form)) (null (cddr form)))
           (cadr form))
          (t
           (signal 'nelisp-bc-unimplemented
                   (list "compiler handles only literals in 3b.2" form))))))
    (let ((consts (vector value))
          (code (vector (nelisp-bc-opcode 'CONST) 0
                        (nelisp-bc-opcode 'RETURN))))
      (nelisp-bc-make env nil consts code 1 0))))

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
         (op-RETURN    (nelisp-bc-opcode 'RETURN))
         (op-CONST     (nelisp-bc-opcode 'CONST))
         (op-STACK-REF (nelisp-bc-opcode 'STACK-REF))
         (op-DROP      (nelisp-bc-opcode 'DROP))
         (op-DUP       (nelisp-bc-opcode 'DUP))
         result)
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
           (t
            (signal 'nelisp-bc-error
                    (list "unknown opcode"
                          (aref nelisp-bc--opcode-names op)
                          op (1- pc)))))))
      (signal 'nelisp-bc-error (list "fell off code without RETURN" pc)))
    result))

(provide 'nelisp-bytecode)
;;; nelisp-bytecode.el ends here
