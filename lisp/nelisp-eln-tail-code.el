;;; nelisp-eln-tail-code.el --- Verify bounded GNU tail-import code -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'nelisp-eln-leaf-code)
(require 'cl-lib)

(defun nelisp-eln-tail-code--merge (old new)
  "Merge must-equal abstract values from OLD and NEW states."
  (if (eq old :unseen)
      new
    (vector (if (equal (aref old 0) (aref new 0)) (aref old 0) nil)
            (if (equal (aref old 1) (aref new 1)) (aref old 1) nil)
            (if (equal (aref old 2) (aref new 2)) (aref old 2) nil))))

(defun nelisp-eln-tail-code--decode (bytes start size function-vaddr)
  "Decode one instruction at START in BYTES, or return nil if unsupported."
  (let (kind length arg)
    (cond
     ((nelisp-eln-leaf-code--match bytes start '(#x8d #x47))
      (when (< (- size start) 3) (throw 'invalid nil))
      (setq kind 'eax-rdi-add length 3
            arg (aref bytes (+ start 2))))
     ((nelisp-eln-leaf-code--match bytes start '(#xa8))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'test-al length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x74))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x75))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jnz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #xba))
      (when (< (- size start) 10) (throw 'invalid nil))
      (setq kind 'mov-rdx-imm length 10
            arg (logior (nelisp-eln-leaf-code--u32 bytes (+ start 2))
                        (ash (nelisp-eln-leaf-code--u32 bytes (+ start 6)) 32))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xf8))
      (setq kind 'mov-rax-rdi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #xc1 #xf8))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'sar-rax length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x39 #xd0))
      (setq kind 'cmp-rax-rdx length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8d #x04 #x85))
      (when (< (- size start) 8) (throw 'invalid nil))
      (setq kind 'lea-fixnum length 8
            arg (nelisp-eln-leaf-code--s32 bytes (+ start 4))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x05))
      (when (< (- size start) 7) (throw 'invalid nil))
      (let ((got (+ function-vaddr start 7
                    (nelisp-eln-leaf-code--s32 bytes (+ start 3)))))
        (unless (and (>= got 0) (= (logand got 7) 0))
          (throw 'invalid nil))
        (setq kind 'load-got length 7 arg got)))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x00))
      (setq kind 'load-table length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xff #xa0))
      (when (< (- size start) 6) (throw 'invalid nil))
      (setq kind 'tail-jump length 6
            arg (nelisp-eln-leaf-code--s32 bytes (+ start 2))))
     ((nelisp-eln-leaf-code--match bytes start '(#x31 #xc0))
      (setq kind 'nil-rax length 2))
     ((nelisp-eln-leaf-code--match
       bytes start '(#x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00))
      (setq kind 'nop length 10))
     ((= (aref bytes start) #xc3)
      (setq kind 'ret length 1))
     (t (throw 'invalid nil)))
    (list start (+ start length) kind arg)))

(defun nelisp-eln-tail-code-analyze (bytes function-vaddr allowed-slots)
  "Verify BYTES as a bounded GNU unary function with authorized tail imports.

FUNCTION-VADDR is the function's ELF virtual address. ALLOWED-SLOTS is the
set of freloc-link-table indices the caller permits. Return (:SAFE t :IMPORTS
RECORDS :PROOF forward-cfg), or nil. Each import record is a plist containing
:SLOT and :GOT-VADDR. This verifier proves only instruction/control-flow
properties; callers must independently authenticate each GOT address against
the ELF relocation metadata."
  (catch 'invalid
    (unless (and (stringp bytes) (> (length bytes) 0)
                 (integerp function-vaddr) (>= function-vaddr 0)
                 (listp allowed-slots) (proper-list-p allowed-slots)
                 (cl-every (lambda (byte) (and (integerp byte) (<= 0 byte 255)))
                           (string-to-list bytes))
                 (cl-every (lambda (slot) (and (integerp slot) (>= slot 0)))
                           allowed-slots))
      (throw 'invalid nil))
    (let* ((size (length bytes)) (offset 0)
           (instructions (make-hash-table :test #'eql))
           (instruction-order nil)
           (boundaries (make-hash-table :test #'eql))
           (states (make-hash-table :test #'eql))
           (imports nil) (reachable-terminals 0))
      ;; Decode every byte, including unreachable padding; operands never
      ;; become instructions and all branch destinations must be boundaries.
      (while (< offset size)
        (puthash offset t boundaries)
        (let* ((insn (nelisp-eln-tail-code--decode
                      bytes offset size function-vaddr))
               (end (nth 1 insn)) (kind (nth 2 insn)) (arg (nth 3 insn)))
          (when (memq kind '(jz jnz))
            (let ((target (+ end (if (< arg 128) arg (- arg 256)))))
              (unless (and (> target end) (< target size))
                (throw 'invalid nil))
              (setcar (nthcdr 3 insn) target)))
          (when (eq kind 'tail-jump)
            (unless (and (>= arg 0) (= (% arg 8) 0))
              (throw 'invalid nil)))
          (puthash offset insn instructions)
          (push insn instruction-order)
          (setq offset end)))
      (unless (and (= offset size) (gethash 0 boundaries))
        (throw 'invalid nil))
      ;; Verify each branch enters the start of a fully decoded instruction.
      (maphash
       (lambda (_pc insn)
         (when (memq (nth 2 insn) '(jz jnz))
           (unless (gethash (nth 3 insn) boundaries) (throw 'invalid nil))))
       instructions)
      (puthash 0 (vector nil nil nil) states)
      ;; All edges go forward, so ascending instruction order propagates the
      ;; must-state to every join before that instruction is visited.
      (dolist (insn (nreverse instruction-order))
         (let* ((pc (car insn))
                (state (gethash pc states :unseen)))
           (unless (eq state :unseen)
             (let* ((end (nth 1 insn)) (kind (nth 2 insn)) (arg (nth 3 insn))
                    (rax (aref state 0)) (rdx (aref state 1))
                    (flags (aref state 2)) (next nil))
               (pcase kind
                 ('eax-rdi-add
                  ;; `lea -2(%rdi),%eax': the fixnum tag-test always reads
                  ;; the ABI's fixed 2-bit tag offset.  Full-coverage fix
                  ;; (corpus crash gate): this displacement byte used to be
                  ;; decoded but never compared, so any other byte value
                  ;; here still admitted -- pin it to the one genuine value.
                  (unless (= arg #xfe) (throw 'invalid nil))
                  (setq rax :scalar))
                 ('test-al
                  (unless rax (throw 'invalid nil))
                  ;; Full-coverage fix: the tag mask byte was decoded but
                  ;; never compared; genuine code always tests the fixed
                  ;; 2-bit tag mask.
                  (unless (= arg #x3) (throw 'invalid nil))
                  (setq flags :test))
                 ((or 'jz 'jnz)
                  (unless flags (throw 'invalid nil))
                  (let ((branch-state (vector rax rdx nil)))
                    (puthash arg (nelisp-eln-tail-code--merge
                                  (gethash arg states :unseen) branch-state) states))
                  (setq flags nil))
                 ('mov-rdx-imm (setq rdx :scalar))
                 ('mov-rax-rdi (setq rax :argument))
                 ('sar-rax
                  (unless rax (throw 'invalid nil))
                  ;; Full-coverage fix: this used to accept any shift whose
                  ;; low 6 bits were nonzero (and silently ignore the exact
                  ;; amount, and leave state untouched when they were all
                  ;; zero); genuine code always shifts by exactly the
                  ;; fixnum tag width, 2.
                  (unless (= arg 2) (throw 'invalid nil))
                  (setq rax :scalar flags :shift))
                 ('cmp-rax-rdx
                  (unless (and rax rdx) (throw 'invalid nil))
                  (setq flags :compare))
                 ('lea-fixnum
                  (unless rax (throw 'invalid nil))
                  (setq rax (if (= (% arg 4) 2) :fixnum :scalar)))
                 ('nil-rax (setq rax :nil flags :xor))
                 ('load-got (setq rax (list :got-cell arg)))
                 ('load-table
                  (unless (and (consp rax) (eq (car rax) :got-cell))
                    (throw 'invalid nil))
                  (setq rax (list :subr-table (cadr rax))))
                 ('tail-jump
                  (unless (and (consp rax) (eq (car rax) :subr-table)
                               (memq (/ arg 8) allowed-slots))
                    (throw 'invalid nil))
                  (push (list :slot (/ arg 8) :got-vaddr (cadr rax)) imports)
                  (setq reachable-terminals (1+ reachable-terminals)))
                 ('ret
                  (unless (memq rax '(:argument :fixnum :nil))
                    (throw 'invalid nil))
                  (setq reachable-terminals (1+ reachable-terminals)))
                 ('nop nil))
               (setq next (vector rax rdx flags))
               (unless (memq kind '(ret tail-jump))
                 (when (>= end size) (throw 'invalid nil))
                 (puthash end (nelisp-eln-tail-code--merge
                               (gethash end states :unseen) next) states))))))
      (unless (> reachable-terminals 0) (throw 'invalid nil))
      (list :safe t :imports (nreverse imports) :proof :forward-cfg))))

(defun nelisp-eln-tail-code--decode-stack-call (bytes start size function-vaddr)
  "Decode one instruction of the bounded MANY stack-call grammar at START.
This grammar recognizes no branch or loop opcode at all, so any injected
jump, or any byte outside the fixed vocabulary below, throws \\='invalid."
  (let (kind length arg)
    (cond
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xec))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-open length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x05))
      (when (< (- size start) 7) (throw 'invalid nil))
      (let ((got (+ function-vaddr start 7
                    (nelisp-eln-leaf-code--s32 bytes (+ start 3)))))
        (unless (and (>= got 0) (= (logand got 7) 0))
          (throw 'invalid nil))
        (setq kind 'load-got length 7 arg got)))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #x3c #x24))
      (setq kind 'store-array0 length 4))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xe6))
      (setq kind 'array-ptr length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xbf))
      (when (< (- size start) 5) (throw 'invalid nil))
      (setq kind 'argc-imm length 5
            arg (nelisp-eln-leaf-code--u32 bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #xc7 #x44 #x24))
      (when (< (- size start) 9) (throw 'invalid nil))
      (setq kind 'store-array-imm length 9
            arg (cons (aref bytes (+ start 4))
                      (nelisp-eln-leaf-code--u32 bytes (+ start 5)))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x00))
      (setq kind 'load-table length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xff #x90))
      (when (< (- size start) 6) (throw 'invalid nil))
      (setq kind 'call-slot length 6
            arg (nelisp-eln-leaf-code--s32 bytes (+ start 2))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xc4))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-close length 4 arg (aref bytes (+ start 3))))
     ((= (aref bytes start) #xc3)
      (setq kind 'ret length 1))
     (t (throw 'invalid nil)))
    (list start (+ start length) kind arg)))

(defun nelisp-eln-tail-code-analyze-stack-call
    (bytes function-vaddr allowed-slots arity)
  "Verify BYTES as exactly one bounded MANY stack-call GNU leaf.

Unlike `nelisp-eln-tail-code-analyze', which proves a branching tail-JMP
leaf (1+/1-), this admits only a fixed straight-line sequence with no
branches at all, matching the genuine GNU `zerop' shape: open a stack
frame; load the authenticated `freloc_link_table' GOT cell; store the
incoming argument into array slot 0; point the array register at the
frame; load ARITY as the argument count; store ARITY-1 further tagged
fixnum literals into consecutive 8-byte array slots; dereference the
table; issue exactly one non-tail CALL (never a JMP) through a slot in
ALLOWED-SLOTS; close the frame with the same size used to open it; and
return the call's result untouched.  Every byte of BYTES must be
consumed by exactly this sequence in exactly this order; because the
decoder recognizes no jump or loop opcode, any injected branch, any
extra call, or any byte outside this grammar is rejected, and a
loop-free forward CFG holds trivially.  Return (:SAFE T :IMPORTS
\(a single-element list\) :PROOF :straight-line-call) or nil.  As with
`nelisp-eln-tail-code-analyze', this proves only instruction/control-flow
properties; callers must independently authenticate the GOT address
against the ELF relocation metadata."
  (catch 'invalid
    (unless (and (stringp bytes) (> (length bytes) 0)
                 (integerp function-vaddr) (>= function-vaddr 0)
                 (listp allowed-slots) (proper-list-p allowed-slots)
                 (cl-every (lambda (slot) (and (integerp slot) (>= slot 0)))
                           allowed-slots)
                 (integerp arity) (> arity 0)
                 (cl-every (lambda (byte) (and (integerp byte) (<= 0 byte 255)))
                           (string-to-list bytes)))
      (throw 'invalid nil))
    (let* ((size (length bytes)) (offset 0) (import nil) (frame-size nil))
      (cl-flet ((next ()
                  (let* ((insn (nelisp-eln-tail-code--decode-stack-call
                                bytes offset size function-vaddr)))
                    (setq offset (nth 1 insn))
                    insn)))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'frame-open) (throw 'invalid nil))
          (setq frame-size (nth 3 insn)))
        (let ((insn (next)) (got nil))
          (unless (eq (nth 2 insn) 'load-got) (throw 'invalid nil))
          (setq got (nth 3 insn))
          (unless (eq (nth 2 (next)) 'store-array0) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'array-ptr) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'argc-imm) (= (nth 3 insn) arity))
              (throw 'invalid nil)))
          (let ((i 1))
            (while (< i arity)
              (let ((insn (next)))
                (unless (and (eq (nth 2 insn) 'store-array-imm)
                             (= (car (nth 3 insn)) (* i 8))
                             (= (logand (cdr (nth 3 insn)) 3) 2))
                  (throw 'invalid nil)))
              (setq i (1+ i))))
          (unless (eq (nth 2 (next)) 'load-table) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'call-slot)
                         (>= (nth 3 insn) 0) (= (% (nth 3 insn) 8) 0)
                         (memq (/ (nth 3 insn) 8) allowed-slots))
              (throw 'invalid nil))
            (setq import (list :slot (/ (nth 3 insn) 8) :got-vaddr got)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'frame-close)
                         (= (nth 3 insn) frame-size))
              (throw 'invalid nil)))
          (unless (eq (nth 2 (next)) 'ret) (throw 'invalid nil))))
      (unless (= offset size) (throw 'invalid nil))
      (list :safe t :imports (list import) :proof :straight-line-call))))

(defun nelisp-eln-tail-code--decode-chain-call (bytes start size function-vaddr)
  "Decode one instruction of the bounded S4.6 chain-call grammar at START.

This grammar is deliberately much narrower than a general x86-64
decoder: it recognizes exactly the fixed opcode/ModRM encodings the
genuine `(defun nelisp-gnu-chain (g x) (funcall g (1+ x)))' artifact
emits (two callee-saved pushes, an inline fixnum fast path staged off
%rsi, a non-tail authenticated CALL for the Fadd1 slow path, a 2-slot
MANY stack array built from %rbx and the merged %rax, and a second
non-tail authenticated CALL for `Ffuncall'), plus its two short forward
branches (jnz/je to the slow path, jmp to the join point) and a single
3-byte `nopl' alignment pad.  Any other byte throws \\='invalid."
  (let (kind length arg)
    (cond
     ((= (aref bytes start) #x55) (setq kind 'push-rbp length 1))
     ((= (aref bytes start) #x53) (setq kind 'push-rbx length 1))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xfb))
      (setq kind 'mov-rbx-rdi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xf7))
      (setq kind 'mov-rdi-rsi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xec))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-open length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x05))
      (when (< (- size start) 7) (throw 'invalid nil))
      (let ((got (+ function-vaddr start 7
                    (nelisp-eln-leaf-code--s32 bytes (+ start 3)))))
        (unless (and (>= got 0) (= (logand got 7) 0))
          (throw 'invalid nil))
        (setq kind 'load-got length 7 arg got)))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x28))
      (setq kind 'load-table-rbp length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x8d #x46 #xfe))
      (setq kind 'tag-test-rsi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xa8))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'test-al length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x75))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jnz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #xba))
      (when (< (- size start) 10) (throw 'invalid nil))
      (setq kind 'mov-rdx-imm length 10
            arg (logior (nelisp-eln-leaf-code--u32 bytes (+ start 2))
                        (ash (nelisp-eln-leaf-code--u32 bytes (+ start 6)) 32))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xf0))
      (setq kind 'mov-rax-rsi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #xc1 #xf8))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'sar-rax length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x39 #xd0))
      (setq kind 'cmp-rax-rdx length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x74))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8d #x04 #x85))
      (when (< (- size start) 8) (throw 'invalid nil))
      (setq kind 'lea-fixnum length 8
            arg (nelisp-eln-leaf-code--s32 bytes (+ start 4))))
     ((nelisp-eln-leaf-code--match bytes start '(#xeb))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jmp-short length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x0f #x1f #x00))
      (setq kind 'nop3 length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xff #x95))
      (when (< (- size start) 6) (throw 'invalid nil))
      (setq kind 'call-slot-rbp length 6
            arg (nelisp-eln-leaf-code--s32 bytes (+ start 2))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #x1c #x24))
      (setq kind 'store-array0-rbx length 4))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xe6))
      (setq kind 'array-ptr length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xbf))
      (when (< (- size start) 5) (throw 'invalid nil))
      (setq kind 'argc-imm length 5
            arg (nelisp-eln-leaf-code--u32 bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #x44 #x24))
      (when (< (- size start) 5) (throw 'invalid nil))
      (setq kind 'store-array-rax length 5 arg (aref bytes (+ start 4))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xc4))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-close length 4 arg (aref bytes (+ start 3))))
     ((= (aref bytes start) #x5b) (setq kind 'pop-rbx length 1))
     ((= (aref bytes start) #x5d) (setq kind 'pop-rbp length 1))
     ((= (aref bytes start) #xc3) (setq kind 'ret length 1))
     (t (throw 'invalid nil)))
    (list start (+ start length) kind arg)))

(defun nelisp-eln-tail-code-analyze-chain-call
    (bytes function-vaddr unary-allowed-slots unary-bound unary-delta
           many-allowed-slots many-arity)
  "Verify BYTES as exactly the genuine S4.6 two-argument chain shape.

Proves the fixed straight-line-with-one-fork sequence: save both
incoming arguments (g in %rbx, x staged into %rdi and kept live in
%rsi); open the stack frame and load the authenticated
`freloc_link_table' root into %rbp; take the inline fixnum fast path on
x (tag test, then overflow test against UNARY-BOUND, then retag by
UNARY-DELTA) or fall through a single non-tail authenticated CALL
through a slot in UNARY-ALLOWED-SLOTS (the Fadd1 slow path) that joins
back at the exact same address the fast path jumps to; then build a
2-element GNU MANY stack array (array[0]=g, array[1]=the merged
increment result), set %edi to MANY-ARITY (which must be 2 — this
grammar admits only the two-argument chain, not a general N-ary one),
and issue a single further non-tail authenticated CALL through a slot
in MANY-ALLOWED-SLOTS (the `Ffuncall' hop); close the frame and return.
Every byte must be consumed by exactly this sequence in exactly this
order, including the exact 3-byte `nopl' alignment pad between the
unconditional jump and the slow-path call; any other byte, any extra
call, or any other branch target is rejected.  Return (:SAFE T
:IMPORTS (UNARY-IMPORT MANY-IMPORT) :PROOF :chain-call) or nil.  As
with the other verifiers here, this proves only instruction/control-
flow properties; callers must independently authenticate each GOT
address against the ELF relocation metadata and the descriptor table."
  (catch 'invalid
    (unless (and (stringp bytes) (> (length bytes) 0)
                 (integerp function-vaddr) (>= function-vaddr 0)
                 (listp unary-allowed-slots) (proper-list-p unary-allowed-slots)
                 (listp many-allowed-slots) (proper-list-p many-allowed-slots)
                 (cl-every (lambda (slot) (and (integerp slot) (>= slot 0)))
                           unary-allowed-slots)
                 (cl-every (lambda (slot) (and (integerp slot) (>= slot 0)))
                           many-allowed-slots)
                 (integerp unary-bound) (integerp unary-delta)
                 (integerp many-arity) (= many-arity 2)
                 (cl-every (lambda (byte) (and (integerp byte) (<= 0 byte 255)))
                           (string-to-list bytes)))
      (throw 'invalid nil))
    (let* ((size (length bytes)) (offset 0)
           (unary-import nil) (many-import nil) (frame-size nil)
           (slow-target nil) (join-target nil))
      (cl-flet ((next ()
                  (let* ((insn (nelisp-eln-tail-code--decode-chain-call
                                bytes offset size function-vaddr)))
                    (setq offset (nth 1 insn))
                    insn))
                (branch-target (insn)
                  (let ((end (nth 1 insn)) (arg (nth 3 insn)))
                    (+ end (if (< arg 128) arg (- arg 256))))))
        (unless (eq (nth 2 (next)) 'push-rbp) (throw 'invalid nil))
        (unless (eq (nth 2 (next)) 'push-rbx) (throw 'invalid nil))
        (unless (eq (nth 2 (next)) 'mov-rbx-rdi) (throw 'invalid nil))
        (unless (eq (nth 2 (next)) 'mov-rdi-rsi) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'frame-open) (throw 'invalid nil))
          (setq frame-size (nth 3 insn)))
        (let ((got nil))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'load-got) (throw 'invalid nil))
            (setq got (nth 3 insn)))
          (unless (eq (nth 2 (next)) 'load-table-rbp) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'tag-test-rsi) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'test-al) (= (nth 3 insn) 3))
              (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'jnz) (throw 'invalid nil))
            (setq slow-target (branch-target insn))
            (unless (and (> slow-target offset) (< slow-target size))
              (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'mov-rdx-imm) (= (nth 3 insn) unary-bound))
              (throw 'invalid nil)))
          (unless (eq (nth 2 (next)) 'mov-rax-rsi) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'sar-rax) (= (nth 3 insn) 2))
              (throw 'invalid nil)))
          (unless (eq (nth 2 (next)) 'cmp-rax-rdx) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'jz) (throw 'invalid nil))
            (unless (= (branch-target insn) slow-target) (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'lea-fixnum) (= (nth 3 insn) unary-delta))
              (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'jmp-short) (throw 'invalid nil))
            (setq join-target (branch-target insn))
            (unless (and (> join-target offset) (< join-target size))
              (throw 'invalid nil)))
          ;; Zero or more alignment pads, never overshooting the slow path.
          (while (< offset slow-target)
            (let ((insn (next)))
              (unless (eq (nth 2 insn) 'nop3) (throw 'invalid nil))
              (when (> offset slow-target) (throw 'invalid nil))))
          (unless (= offset slow-target) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'call-slot-rbp)
                         (>= (nth 3 insn) 0) (= (% (nth 3 insn) 8) 0)
                         (memq (/ (nth 3 insn) 8) unary-allowed-slots))
              (throw 'invalid nil))
            (setq unary-import (list :slot (/ (nth 3 insn) 8) :got-vaddr got)))
          ;; The slow-path call must land exactly at the fast path's join.
          (unless (= offset join-target) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'store-array0-rbx) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'array-ptr) (throw 'invalid nil))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'argc-imm) (= (nth 3 insn) many-arity))
              (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'store-array-rax) (= (nth 3 insn) 8))
              (throw 'invalid nil)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'call-slot-rbp)
                         (>= (nth 3 insn) 0) (= (% (nth 3 insn) 8) 0)
                         (memq (/ (nth 3 insn) 8) many-allowed-slots))
              (throw 'invalid nil))
            (setq many-import (list :slot (/ (nth 3 insn) 8) :got-vaddr got)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'frame-close)
                         (= (nth 3 insn) frame-size))
              (throw 'invalid nil)))
          (unless (eq (nth 2 (next)) 'pop-rbx) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'pop-rbp) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'ret) (throw 'invalid nil))))
      (unless (= offset size) (throw 'invalid nil))
      (list :safe t :imports (list unary-import many-import) :proof :chain-call))))

(defun nelisp-eln-tail-code--decode-cxr-call (bytes start size function-vaddr)
  "Decode one instruction of the bounded S6 caar/cadr grammar at START.

Recognizes exactly the fixed opcode/ModRM encodings the genuine
`caar'/`cadr' artifacts emit: a cons-tag-guarded load of the outer
argument's car or cdr, a second cons-tag-guarded load of that result's
car, and, on either tag-test failure, a shared error path that loads
one `d_reloc' data-relocation slot (the `listp' predicate symbol) and
makes one non-tail authenticated CALL through freloc slot 0
\(`wrong_type_argument', 2-argument fixed convention -- the object
argument is already sitting in %rsi from the tag test, no separate
load needed\)."
  (let (kind length arg)
    (cond
     ((nelisp-eln-leaf-code--match bytes start '(#x8d #x47 #xfd))
      (setq kind 'tag-test-rdi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xec))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-open length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x89 #xfe))
      (setq kind 'stage-arg-rsi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xa8))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'test-al length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x75))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jnz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x77))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'load-outer-rdi-rsi length 4
            arg (let ((b (aref bytes (+ start 3)))) (if (>= b 128) (- b 256) b))))
     ((nelisp-eln-leaf-code--match bytes start '(#x8d #x46 #xfd))
      (setq kind 'tag-test-rsi length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x46 #xfd))
      (setq kind 'load-inner-car-rsi-rax length 4))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x83 #xc4))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'frame-close length 4 arg (aref bytes (+ start 3))))
     ((= (aref bytes start) #xc3) (setq kind 'ret length 1))
     ((nelisp-eln-leaf-code--match bytes start '(#x66 #x0f #x1f #x44 #x00 #x00))
      (setq kind 'nop6 length 6))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x85 #xf6))
      (setq kind 'test-rsi-nil length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#x74))
      (when (>= (1+ start) size) (throw 'invalid nil))
      (setq kind 'jz length 2 arg (aref bytes (1+ start))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x05))
      (when (< (- size start) 7) (throw 'invalid nil))
      (let ((got (+ function-vaddr start 7
                    (nelisp-eln-leaf-code--s32 bytes (+ start 3)))))
        (unless (and (>= got 0) (= (logand got 7) 0))
          (throw 'invalid nil))
        (setq kind 'load-got length 7 arg got)))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x78))
      (when (< (- size start) 4) (throw 'invalid nil))
      (setq kind 'load-d-reloc-rdi length 4 arg (aref bytes (+ start 3))))
     ((nelisp-eln-leaf-code--match bytes start '(#x48 #x8b #x00))
      (setq kind 'load-table length 3))
     ((nelisp-eln-leaf-code--match bytes start '(#xff #x10))
      (setq kind 'call-slot-zero length 2))
     ((nelisp-eln-leaf-code--match bytes start '(#x31 #xc0))
      (setq kind 'nil-rax length 2))
     (t (throw 'invalid nil)))
    (list start (+ start length) kind arg)))

(defun nelisp-eln-tail-code-analyze-cxr-call
    (bytes function-vaddr first-disp d-reloc-slot many-allowed-slots)
  "Verify BYTES as exactly the genuine S6 caar/cadr shape.

FIRST-DISP is the outer load's displacement (-3 for `caar', 5 for
`cadr' -- both read the inner result's car at the universal, fixed -3
offset).  D-RELOC-SLOT is the expected `d_reloc' index of the `listp'
predicate constant.  MANY-ALLOWED-SLOTS names the allowed freloc slot
for the shared `wrong_type_argument' error call (that call always
targets ModRM-encoded offset 0 -- i.e. exactly slot 0 -- so this is
normally \\='(0), kept as a parameter only to stay fail-closed and
explicit rather than hard-coding 0 in two unrelated files).  Every
byte must be consumed by exactly this straight-line-with-one-shared-
error-path sequence, in exactly this order; any other byte, branch
target, or field value is rejected.  Return (:SAFE T :IMPORTS
\(a single-element list, the freloc call\) :DATA-RELOCATION (a plist,
the `d_reloc' load) :PROOF :cxr-call) or nil.  As with the other
verifiers here, this proves only instruction/control-flow properties;
callers must independently authenticate both GOT addresses against the
ELF relocation metadata, the freloc descriptor table, and D_RELOC-
SLOT's own decoded identity in the artifact's `:data-relocations'."
  (catch 'invalid
    (unless (and (stringp bytes) (> (length bytes) 0)
                 (integerp function-vaddr) (>= function-vaddr 0)
                 (integerp first-disp) (member first-disp '(-3 5))
                 (integerp d-reloc-slot) (>= d-reloc-slot 0)
                 (listp many-allowed-slots) (proper-list-p many-allowed-slots)
                 (cl-every (lambda (slot) (and (integerp slot) (>= slot 0)))
                           many-allowed-slots)
                 (memq 0 many-allowed-slots)
                 (cl-every (lambda (byte) (and (integerp byte) (<= 0 byte 255)))
                           (string-to-list bytes)))
      (throw 'invalid nil))
    (let* ((size (length bytes)) (offset 0) (frame-size nil)
           (slow-target nil) (nil-target nil) (freloc-import nil)
           (data-relocation nil))
      (cl-flet ((next ()
                  (let ((insn (nelisp-eln-tail-code--decode-cxr-call
                               bytes offset size function-vaddr)))
                    (setq offset (nth 1 insn))
                    insn))
                (branch-target (insn)
                  (let ((end (nth 1 insn)) (arg (nth 3 insn)))
                    (+ end (if (< arg 128) arg (- arg 256))))))
        (unless (eq (nth 2 (next)) 'tag-test-rdi) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'frame-open) (throw 'invalid nil))
          (setq frame-size (nth 3 insn)))
        (unless (eq (nth 2 (next)) 'stage-arg-rsi) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (and (eq (nth 2 insn) 'test-al) (= (nth 3 insn) 7))
            (throw 'invalid nil)))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'jnz) (throw 'invalid nil))
          (setq slow-target (branch-target insn))
          (unless (and (> slow-target offset) (< slow-target size))
            (throw 'invalid nil)))
        (let ((insn (next)))
          (unless (and (eq (nth 2 insn) 'load-outer-rdi-rsi)
                       (= (nth 3 insn) first-disp))
            (throw 'invalid nil)))
        (unless (eq (nth 2 (next)) 'tag-test-rsi) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (and (eq (nth 2 insn) 'test-al) (= (nth 3 insn) 7))
            (throw 'invalid nil)))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'jnz) (throw 'invalid nil))
          (unless (= (branch-target insn) slow-target) (throw 'invalid nil)))
        (unless (eq (nth 2 (next)) 'load-inner-car-rsi-rax) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (and (eq (nth 2 insn) 'frame-close)
                       (= (nth 3 insn) frame-size))
            (throw 'invalid nil)))
        (unless (eq (nth 2 (next)) 'ret) (throw 'invalid nil))
        (while (< offset slow-target)
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'nop6) (throw 'invalid nil))
            (when (> offset slow-target) (throw 'invalid nil))))
        (unless (= offset slow-target) (throw 'invalid nil))
        (unless (eq (nth 2 (next)) 'test-rsi-nil) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (eq (nth 2 insn) 'jz) (throw 'invalid nil))
          (setq nil-target (branch-target insn))
          (unless (and (> nil-target offset) (< nil-target size))
            (throw 'invalid nil)))
        (let ((d-reloc-got nil))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'load-got) (throw 'invalid nil))
            (setq d-reloc-got (nth 3 insn)))
          (let ((insn (next)))
            (unless (and (eq (nth 2 insn) 'load-d-reloc-rdi)
                         (= (nth 3 insn) (* d-reloc-slot 8)))
              (throw 'invalid nil)))
          (setq data-relocation (list :slot d-reloc-slot :got-vaddr d-reloc-got)))
        (let ((freloc-got nil))
          (let ((insn (next)))
            (unless (eq (nth 2 insn) 'load-got) (throw 'invalid nil))
            (setq freloc-got (nth 3 insn)))
          (unless (eq (nth 2 (next)) 'load-table) (throw 'invalid nil))
          (unless (eq (nth 2 (next)) 'call-slot-zero) (throw 'invalid nil))
          (setq freloc-import (list :slot 0 :got-vaddr freloc-got)))
        (unless (= offset nil-target) (throw 'invalid nil))
        (unless (eq (nth 2 (next)) 'nil-rax) (throw 'invalid nil))
        (let ((insn (next)))
          (unless (and (eq (nth 2 insn) 'frame-close)
                       (= (nth 3 insn) frame-size))
            (throw 'invalid nil)))
        (unless (eq (nth 2 (next)) 'ret) (throw 'invalid nil)))
      (unless (= offset size) (throw 'invalid nil))
      (list :safe t :imports (list freloc-import)
            :data-relocation data-relocation :proof :cxr-call))))

;;; S6 exact multi-import shapes: fully fixed bodies importing several
;;; distinct freloc slots.

(defconst nelisp-eln-tail-code--multi-import-shapes
  (list
   (list 'fixnum-range
         :template
         [#x55 #x53 #x48 #x89 #xfb #x48 #x83 #xec #x28 #x48 #x8b #x05
      nil nil nil nil #x48 #x8b #x28 #x8d #x47 #xfe #xa8 #x03
      #x75 #x66 #x48 #x8b #x05 nil nil nil nil #x48 #x83 #x78
      #x38 #x00 #x74 #x5f #x48 #x89 #x5c #x24 #x08 #x48 #x89 #xe6
      #xbf #x02 #x00 #x00 #x00 #x48 #xb8 #x02 #x00 #x00 #x00 #x00
      #x00 #x00 #x80 #x48 #x89 #x04 #x24 #xff #x95 #x28 #x29 #x00
      #x00 #x48 #x85 #xc0 #x74 #x39 #x48 #x89 #x5c #x24 #x10 #x48
      #x8d #x74 #x24 #x10 #xbf #x02 #x00 #x00 #x00 #x48 #xb8 #xfe
      #xff #xff #xff #xff #xff #xff #x7f #x48 #x89 #x44 #x24 #x18
      #xff #x95 #x28 #x29 #x00 #x00 #x48 #x83 #xc4 #x28 #x5b #x5d
      #xc3 #x0f #x1f #x80 #x00 #x00 #x00 #x00 #x8d #x47 #xfb #xa8
      #x07 #x74 #x09 #x31 #xc0 #x48 #x83 #xc4 #x28 #x5b #x5d #xc3
      #xbe #x02 #x00 #x00 #x00 #xff #x55 #x08 #x84 #xc0 #x0f #x85
      #x7a #xff #xff #xff #x31 #xc0 #xeb #xe5]
         :gots '((12 . freloc) (29 . d-reloc))
         :imports '(1317 1) :data '(7))
   (list 'bignum
         :template
         [#x41 #x54 #x55 #x48 #x83 #xec #x28 #x48 #x8b #x05 nil nil
      nil nil #x4c #x8b #x20 #x8d #x47 #xfe #xa8 #x03 #x75 #x58
      #x48 #x8b #x2d nil nil nil nil #x48 #x83 #x7d #x20 #x00
      #x74 #x39 #xf3 #x0f #x7e #x45 #x08 #x66 #x48 #x0f #x6e #xcf
      #x48 #x8d #x74 #x24 #x10 #xbf #x02 #x00 #x00 #x00 #x66 #x0f
      #x6c #xc1 #x0f #x29 #x44 #x24 #x10 #x41 #xff #x94 #x24 #x88
      #x1d #x00 #x00 #x48 #x85 #xc0 #x74 #x56 #x48 #x8b #x15 nil
      nil nil nil #x48 #x8b #x12 #x80 #x3a #x00 #x75 #x39 #x48
      #x83 #xc4 #x28 #x31 #xc0 #x5d #x41 #x5c #xc3 #x0f #x1f #x80
      #x00 #x00 #x00 #x00 #x8d #x47 #xfb #xa8 #x07 #x75 #xe8 #x48
      #x89 #x7c #x24 #x08 #xbe #x02 #x00 #x00 #x00 #x41 #xff #x54
      #x24 #x08 #x48 #x8b #x7c #x24 #x08 #x84 #xc0 #x75 #x89 #xeb
      #xce #x0f #x1f #x80 #x00 #x00 #x00 #x00 #x31 #xf6 #x48 #x89
      #xc7 #x41 #xff #x54 #x24 #x38 #x84 #xc0 #x74 #xb9 #x48 #x8b
      #x45 #x20 #x48 #x83 #xc4 #x28 #x5d #x41 #x5c #xc3]
         :gots '((10 . freloc) (27 . d-reloc) (83 . symbols-with-pos))
         :imports '(945 1 7) :data '(1 4))
   (list 'car-eq-constant
         :template
         [#x8d #x47 #xfd #xa8 #x07 #x75 #x39 #x53 #x48 #x8b #x1d nil
      nil nil nil #x48 #x8b #x43 #x20 #x48 #x85 #xc0 #x74 #x1c
      #x48 #x8b #x7f #xfd #x48 #x8b #x73 #x08 #x48 #x39 #xf7 #x74
      #x11 #x48 #x8b #x05 nil nil nil nil #x48 #x8b #x00 #x80
      #x38 #x00 #x75 #x14 #x31 #xc0 #x5b #xc3 #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x31 #xc0 #xc3 #x0f #x1f #x44 #x00 #x00
      #x48 #x8b #x05 nil nil nil nil #x48 #x8b #x00 #xff #x50
      #x38 #x84 #xc0 #x74 #xdb #x48 #x8b #x43 #x20 #x5b #xc3]
         :gots '((11 . d-reloc) (40 . symbols-with-pos) (75 . freloc))
         :imports '(7) :data '(1 4))
   (list 'cons-form-constant
         :template
         [#x48 #x8b #x05 nil nil nil nil #x53 #x48 #x89 #xf7 #x31
      #xf6 #x48 #x8b #x18 #xff #x93 #xf8 #x22 #x00 #x00 #xbf #x02
      #x00 #x00 #x00 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00
      #x48 #x89 #xc6 #x48 #x8b #x05 nil nil nil nil #x48 #x8b
      #x78 #x08 #x48 #x8b #x83 #xf8 #x22 #x00 #x00 #x5b #xff #xe0]
         :gots '((3 . freloc) (42 . d-reloc))
         :imports '(1119) :data '(1))
   (list 'set-difference
         :template
         [#x41 #x57 #x41 #x56 #x49 #x89 #xf6 #x41 #x55 #x41 #x54 #x49
      #x89 #xfc #x55 #x53 #x48 #x83 #xec #x18 #x4c #x8b #x3d nil
      nil nil nil #x48 #x8b #x1d nil nil nil nil #x49 #x8b
      #x07 #x48 #x8b #x2b #x48 #x89 #x44 #x24 #x08 #x48 #x85 #xff
      #x75 #x60 #xe9 #xa0 #x00 #x00 #x00 #x66 #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x4d #x8d #x6c #x24 #xfd #x4d #x8b #x64
      #x24 #xfd #x4c #x89 #xf6 #x4c #x89 #xe7 #xff #x95 #x08 #x26
      #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x99 #x00 #x00 #x00 #x8b
      #x05 nil nil nil nil #x4d #x8b #x65 #x08 #x83 #xc0 #x01
      #x89 #x05 nil nil nil nil #xc1 #xe8 #x09 #x74 #x16 #xc7
      #x05 nil nil nil nil #x00 #x00 #x00 #x00 #x48 #x8b #x03
      #xff #x50 #x68 #x48 #x8b #x03 #xff #x50 #x70 #x4d #x85 #xe4
      #x74 #x45 #x41 #x8d #x44 #x24 #xfd #xa8 #x07 #x74 #xa5 #x48
      #x8b #x03 #x49 #x8b #x7f #x20 #x4c #x89 #xe6 #xff #x10 #x31
      #xff #x4c #x89 #xf6 #xff #x95 #x08 #x26 #x00 #x00 #x48 #x85
      #xc0 #x74 #x69 #x48 #x8b #x03 #x49 #x8b #x7f #x20 #x4c #x89
      #xe6 #xff #x10 #x8b #x05 nil nil nil nil #x83 #xc0 #x01
      #x89 #x05 nil nil nil nil #xc1 #xe8 #x09 #x75 #x39 #x48
      #x8b #x85 #xc8 #x25 #x00 #x00 #x48 #x8b #x7c #x24 #x08 #x48
      #x83 #xc4 #x18 #x5b #x5d #x41 #x5c #x41 #x5d #x41 #x5e #x41
      #x5f #xff #xe0 #x0f #x1f #x44 #x00 #x00 #x48 #x8b #x74 #x24
      #x08 #x4c #x89 #xe7 #xff #x95 #xf8 #x22 #x00 #x00 #x48 #x89
      #x44 #x24 #x08 #xe9 #x4f #xff #xff #xff #x45 #x31 #xe4 #xe9
      #x5f #xff #xff #xff #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
      #x48 #x8b #x74 #x24 #x08 #x31 #xff #xff #x95 #xf8 #x22 #x00
      #x00 #x48 #x89 #x44 #x24 #x08 #xeb #x83]
         :gots '((23 . d-reloc) (30 . freloc))
         :module-counter '((97 . 0) (110 . 0) (121 . 4) (197 . 0) (206 . 0))
         :imports '(0 13 14 1217 1119 1209) :data '(0 4))
   (list 'for-effect-constant
         :template
         [#x55 #x53 #x48 #x83 #xec #x28 #x48 #x8b #x05 nil nil nil
      nil #x48 #x8b #x2d nil nil nil nil #x48 #x89 #x7c #x24
      #x08 #x48 #x8b #x18 #x48 #x8b #x7d #x00 #xff #x93 #xb8 #x29
      #x00 #x00 #x48 #x85 #xc0 #x75 #x2d #xf3 #x0f #x7e #x45 #x10
      #x48 #x8d #x74 #x24 #x10 #xbf #x02 #x00 #x00 #x00 #x0f #x16
      #x44 #x24 #x08 #x0f #x29 #x44 #x24 #x10 #xff #x93 #x88 #x1d
      #x00 #x00 #x48 #x83 #xc4 #x28 #x5b #x5d #xc3 #x0f #x1f #x80
      #x00 #x00 #x00 #x00 #x48 #x8b #x7d #x00 #x31 #xc9 #x31 #xd2
      #x31 #xf6 #xff #x53 #x50 #x48 #x83 #xc4 #x28 #x31 #xc0 #x5b
      #x5d #xc3]
         :gots '((9 . freloc) (16 . d-reloc))
         :imports '(1335 945 10) :data '(0 2)))
  "Genuine GNU 31.1 (ABI ba35c031) native bodies admitted by exact bytes.
Every byte is fixed except the RIP-relative GOT displacements (nil); each
entry of :GOTS is (DISP32-OFFSET . KIND), the displacement being the last
four bytes of its instruction.  :IMPORTS lists the distinct freloc slots
the body calls, in the port order the dispatcher uses; :DATA lists the
`d_reloc' indices it reads.

`fixnum-range' is vendor subr.el `fixnump' (164 bytes): fixnum tag test,
else vectorlike tag test and slot 1 `helper_PSEUDOVECTOR_TYPEP_XUNTAG'
\(OBJ, 2 = PVEC_BIGNUM) answered as a C bool in %al; integers then test
d_reloc[7] (t) and make two MANY (2, argv) calls through slot 1317
`Fleq', (<= most-negative-fixnum OBJ) then the returned
\(<= OBJ most-positive-fixnum).

`bignum' is vendor subr.el `bignump' (178 bytes): the same integer
tests, then (when d_reloc[4], t, is non-nil) a MANY (2, argv) call
through slot 945 `Ffuncall' of d_reloc[1] (`fixnump') on OBJ; a nil
result returns d_reloc[4]; a non-nil one returns nil unless the
`symbols_with_pos_enabled' byte reached through
f_symbols_with_pos_enabled_reloc is set, in which case slot 7 `slow_eq'
\(RESULT, nil) decides.

`car-eq-constant' is vendor subr.el `frame-configuration-p' (95 bytes):
non-conses return nil; when d_reloc[4] (t) is non-nil, a cons whose car
word equals d_reloc[1] (the symbol `frame-configuration') returns
d_reloc[4]; any other car returns nil unless `symbols_with_pos_enabled'
is set, in which case slot 7 `slow_eq' (CAR, d_reloc[1]) decides.

`cons-form-constant' is the binary compiler-macro body GNU emits for
vendor subr.el `zerop''s (compiler-macro (lambda (_) `(= 0 ,number)))
declaration, `zerop--anon-cmacro' (60 bytes): it ignores its first
argument and builds (D_RELOC[1] 0 ARG2) -- d_reloc[1] is `=' -- through
two non-tail and one tail-JMP fixed-arity-2 call of slot 1119 `Fcons'.

`set-difference' is vendor cconv.el `cconv--set-diff' (308 bytes, S6.8):
a `dolist' over S1 (tag-checked; a non-list tail calls slot 0
`wrong_type_argument' with d_reloc[4], `listp') that calls slot 1217
`Fmemq' (X, S2) and, for a nil answer, slot 1119 `Fcons' (X, RES), RES
starting at d_reloc[0] (nil); it ends with a tail JMP through slot 1209
`Fnreverse'.  Each iteration increments the module-local 4-byte
`quitcounter' in `.bss' by direct RIP-relative access (no GOT) and every
512th one calls slot 13 `maybe_gc' then slot 14 `maybe_quit'.  Those
direct accesses are listed in :MODULE-COUNTER as (DISP32-OFFSET .
TRAILING-BYTES), TRAILING-BYTES being what follows the displacement
inside its instruction (the 4-byte immediate of `movl $0,COUNTER(%rip)');
all of them must reach one and the same address, which callers must then
authenticate as the artifact's own `quitcounter' object.

`for-effect-constant' is vendor bytecomp.el `byte-compile-constant'
\(110 bytes, S6.15): slot 1335 `Fsymbol_value' of d_reloc[0]
\(`byte-compile--for-effect'); a nil value makes a MANY (2, argv) call
through slot 945 `Ffuncall' of d_reloc[2] (`byte-compile-push-constant')
on the argument and returns its value, a non-nil one calls slot 10
`set_internal' (d_reloc[0], nil, nil, SET_INTERNAL_SET) and returns nil.")

(defun nelisp-eln-tail-code--disp32 (bytes offset)
  "Return the signed disp32 at OFFSET in BYTES."
  (let ((value (logior (aref bytes offset)
                       (ash (aref bytes (+ offset 1)) 8)
                       (ash (aref bytes (+ offset 2)) 16)
                       (ash (aref bytes (+ offset 3)) 24))))
    (if (>= value #x80000000) (- value #x100000000) value)))

(defun nelisp-eln-tail-code--match-template (bytes template)
  "Non-nil when BYTES equals TEMPLATE at every non-nil position."
  (and (= (length bytes) (length template))
       (catch 'mismatch
         (dotimes (i (length template))
           (let ((expected (aref template i)))
             (when (and expected (/= expected (aref bytes i)))
               (throw 'mismatch nil))))
         t)))

(defun nelisp-eln-tail-code-analyze-multi-import-call (bytes function-vaddr)
  "Verify BYTES as exactly one `nelisp-eln-tail-code--multi-import-shapes'
body, or return nil.  Return (:SAFE T :SHAPE NAME :IMPORTS ((:SLOT S
:GOT-VADDR G) ...) :DATA-RELOCATIONS ((:SLOT I :GOT-VADDR D) ...)
:SYMBOLS-WITH-POS-GOT X-or-nil :MODULE-COUNTER-VADDR C-or-nil :PROOF
:multi-import-call).  Every import
shares the one freloc GOT load.  As with the other verifiers here, this
proves only the instruction bytes; callers must independently
authenticate every GOT address, every freloc slot's descriptor and every
listed `d_reloc' constant's decoded identity, and any module counter
address as the artifact's own `quitcounter' object."
  (catch 'invalid
    (unless (and (stringp bytes) (integerp function-vaddr)
                 (>= function-vaddr 0))
      (throw 'invalid nil))
    (let ((shape (cl-find-if
                  (lambda (entry)
                    (nelisp-eln-tail-code--match-template
                     bytes (plist-get (cdr entry) :template)))
                  nelisp-eln-tail-code--multi-import-shapes)))
      (unless shape (throw 'invalid nil))
      (let ((freloc nil) (d-reloc nil) (swp nil) (counter nil))
        ;; Every direct module-counter access must reach the same address.
        (dolist (access (plist-get (cdr shape) :module-counter))
          (let ((vaddr (+ function-vaddr (car access) 4 (cdr access)
                          (nelisp-eln-tail-code--disp32 bytes (car access)))))
            (when (or (< vaddr 0) (and counter (/= vaddr counter)))
              (throw 'invalid nil))
            (setq counter vaddr)))
        (dolist (got (plist-get (cdr shape) :gots))
          (let ((vaddr (+ function-vaddr (car got) 4
                          (nelisp-eln-tail-code--disp32 bytes (car got)))))
            (when (< vaddr 0) (throw 'invalid nil))
            (pcase (cdr got)
              ('freloc (setq freloc vaddr))
              ('d-reloc (setq d-reloc vaddr))
              ('symbols-with-pos (setq swp vaddr))
              (_ (throw 'invalid nil)))))
        (unless (and freloc d-reloc) (throw 'invalid nil))
        (list :safe t :shape (car shape)
              :imports (mapcar (lambda (slot)
                                 (list :slot slot :got-vaddr freloc))
                               (plist-get (cdr shape) :imports))
              :data-relocations (mapcar (lambda (index)
                                          (list :slot index
                                                :got-vaddr d-reloc))
                                        (plist-get (cdr shape) :data))
              :symbols-with-pos-got swp
              :module-counter-vaddr counter
              :proof :multi-import-call)))))

(provide 'nelisp-eln-tail-code)

;;; nelisp-eln-tail-code.el ends here
