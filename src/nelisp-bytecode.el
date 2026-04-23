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

(defconst nelisp-bc--opcodes '()
  "Plist: op name (symbol) -> opcode byte (0-255).
Populated incrementally by 3b.2+ sub-phases.  Phase 3b.1 keeps it
empty so code paths that scan the table still round-trip without
a nil head.")

(defun nelisp-bc-opcode (name)
  "Return the byte for opcode NAME, or signal if undefined."
  (or (plist-get nelisp-bc--opcodes name)
      (signal 'nelisp-bc-error
              (list "unknown opcode" name))))

;;; Compiler / VM stubs ----------------------------------------------

(defun nelisp-bc-compile (_form &optional _env)
  "Compile FORM to a `nelisp-bcl'.  Stub pending 3b.2.

MCP Parameters:
  FORM  — NeLisp source form (cons / atom)
  ENV   — optional lexical env alist captured at compile time"
  (signal 'nelisp-bc-unimplemented
          (list "nelisp-bc-compile pending Phase 3b.2")))

(defun nelisp-bc-run (_bcl &optional _args)
  "Execute the bytecode closure BCL with ARGS.  Stub pending 3b.2.

MCP Parameters:
  BCL   — `nelisp-bcl' object (see commentary §4.1)
  ARGS  — optional list of positional arguments"
  (signal 'nelisp-bc-unimplemented
          (list "nelisp-bc-run pending Phase 3b.2")))

(provide 'nelisp-bytecode)
;;; nelisp-bytecode.el ends here
