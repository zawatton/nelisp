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
   (list 'parse-body
         :template
         [#x41 #x57 #x41 #x56 #x41 #x55 #x41 #x54 #x55 #x53 #x48 #x89
      #xfb #x48 #x83 #xec #x18 #x4c #x8b #x2d nil nil nil nil
      #x4c #x8b #x35 nil nil nil nil #x49 #x8b #x6d #x00 #x4d
      #x8b #x26 #x48 #x85 #xff #x0f #x84 #x94 #x00 #x00 #x00 #x90
      #x8d #x43 #xfd #xa8 #x07 #x0f #x85 #x15 #x01 #x00 #x00 #x4c
      #x8b #x7b #xfd #x48 #x8d #x43 #xfd #x48 #x89 #x44 #x24 #x08
      #x4c #x89 #xff #xff #x95 #x00 #x2b #x00 #x00 #x48 #x85 #xc0
      #x0f #x84 #x96 #x00 #x00 #x00 #x48 #x8b #x44 #x24 #x08 #x48
      #x8b #x50 #x08 #x48 #x85 #xd2 #x0f #x84 #x74 #x01 #x00 #x00
      #x48 #x89 #xdf #x48 #x89 #x54 #x24 #x08 #xff #x95 #x50 #x2a
      #x00 #x00 #x4c #x89 #xe6 #x48 #x89 #xc7 #xff #x95 #xf8 #x22
      #x00 #x00 #x48 #x8b #x5c #x24 #x08 #x49 #x89 #xc4 #x8b #x05
      nil nil nil nil #x83 #xc0 #x01 #x89 #x05 nil nil nil
      nil #xc1 #xe8 #x09 #x74 #x8e #xc7 #x05 nil nil nil nil
      #x00 #x00 #x00 #x00 #x49 #x8b #x45 #x00 #xff #x50 #x68 #x49
      #x8b #x45 #x00 #xff #x50 #x70 #x48 #x85 #xdb #x0f #x85 #x6d
      #xff #xff #xff #x4c #x89 #xe7 #xff #x95 #xc8 #x25 #x00 #x00
      #x48 #x89 #xde #x48 #x89 #xc7 #x48 #x8b #x85 #xf8 #x22 #x00
      #x00 #x48 #x83 #xc4 #x18 #x5b #x5d #x41 #x5c #x41 #x5d #x41
      #x5e #x41 #x5f #xff #xe0 #x0f #x1f #x80 #x00 #x00 #x00 #x00
      #x4c #x89 #xff #xff #x95 #x50 #x2a #x00 #x00 #x49 #x8b #x76
      #x08 #x48 #x89 #xc7 #xff #x95 #x08 #x26 #x00 #x00 #x48 #x85
      #xc0 #x74 #xb8 #x4c #x8b #x7b #x05 #x48 #x89 #xdf #x4c #x89
      #xfb #xff #x95 #x50 #x2a #x00 #x00 #x4c #x89 #xe6 #x48 #x89
      #xc7 #xff #x95 #xf8 #x22 #x00 #x00 #x49 #x89 #xc4 #x8b #x05
      nil nil nil nil #x83 #xc0 #x01 #x89 #x05 nil nil nil
      nil #xc1 #xe8 #x09 #x0f #x84 #x78 #xff #xff #xff #xe9 #x5b
      #xff #xff #xff #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
      #x49 #x8b #x45 #x00 #x49 #x8b #x7e #x28 #x48 #x89 #xde #xff
      #x10 #x31 #xff #xff #x95 #x00 #x2b #x00 #x00 #x48 #x85 #xc0
      #x74 #x0d #x49 #x8b #x45 #x00 #x49 #x8b #x7e #x28 #x48 #x89
      #xde #xff #x10 #x31 #xff #xff #x95 #x50 #x2a #x00 #x00 #x49
      #x8b #x76 #x08 #x48 #x89 #xc7 #xff #x95 #x08 #x26 #x00 #x00
      #x48 #x85 #xc0 #x0f #x84 #x2e #xff #xff #xff #x49 #x8b #x45
      #x00 #x48 #x89 #xde #x49 #x8b #x7e #x28 #xff #x10 #x48 #x89
      #xdf #x31 #xdb #xff #x95 #x50 #x2a #x00 #x00 #x4c #x89 #xe6
      #x48 #x89 #xc7 #xff #x95 #xf8 #x22 #x00 #x00 #x49 #x89 #xc4
      #x8b #x05 nil nil nil nil #x83 #xc0 #x01 #x89 #x05 nil
      nil nil nil #xc1 #xe8 #x09 #x0f #x85 #xce #xfe #xff #xff
      #xe9 #xea #xfe #xff #xff #x0f #x1f #x80 #x00 #x00 #x00 #x00
      #x4c #x89 #xff #xff #x95 #x50 #x2a #x00 #x00 #x49 #x8b #x76
      #x08 #x48 #x89 #xc7 #xff #x95 #x08 #x26 #x00 #x00 #x48 #x85
      #xc0 #x0f #x84 #xc4 #xfe #xff #xff #x48 #x8b #x44 #x24 #x08
      #x4c #x8b #x78 #x08 #xe9 #x02 #xff #xff #xff]
         :gots '((20 . freloc) (27 . d-reloc))
         :module-counter '((144 . 0) (153 . 0) (164 . 4) (300 . 0) (309 . 0)
                           (446 . 0) (455 . 0))
         :imports '(0 13 14 1376 1354 1119 1209 1217) :data '(0 1 5))
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
         :imports '(1335 945 10) :data '(0 2))
   (list 'setq-form
         :template
         [#x41 #x55 #x41 #x54 #x55 #x48 #x89 #xfd #x53 #x48 #x83 #xec
      #x68 #x4c #x8b #x25 nil nil nil nil #x49 #x8b #x1c #x24
      #xff #x93 #x10 #x27 #x00 #x00 #x48 #x89 #xe6 #xbf #x02 #x00
      #x00 #x00 #x48 #xc7 #x44 #x24 #x08 #x0e #x00 #x00 #x00 #x48
      #x89 #x04 #x24 #xff #x93 #x40 #x29 #x00 #x00 #x4c #x8b #x2d
      nil nil nil nil #x48 #x85 #xc0 #x0f #x84 #xa7 #x00 #x00
      #x00 #x8d #x45 #xfd #xa8 #x07 #x0f #x85 #xc2 #x00 #x00 #x00
      #x48 #x8b #x75 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xfd
      #x00 #x00 #x00 #x4c #x8b #x66 #xfd #x48 #x89 #xee #xbf #x0a
      #x00 #x00 #x00 #xff #x93 #x20 #x26 #x00 #x00 #xbf #x02 #x00
      #x00 #x00 #x48 #x8d #x74 #x24 #x10 #xf3 #x41 #x0f #x7e #x45
      #x18 #x66 #x48 #x0f #x6e #xc8 #x66 #x0f #x6c #xc1 #x0f #x29
      #x44 #x24 #x10 #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b #x7d
      #x20 #xff #x93 #xb8 #x29 #x00 #x00 #x48 #x85 #xc0 #x0f #x84
      #x84 #x00 #x00 #x00 #xf3 #x41 #x0f #x7e #x45 #x28 #x66 #x49
      #x0f #x6e #xd4 #x48 #x8d #x74 #x24 #x20 #xbf #x02 #x00 #x00
      #x00 #x66 #x0f #x6c #xc2 #x0f #x29 #x44 #x24 #x20 #xff #x93
      #x88 #x1d #x00 #x00 #x49 #x8b #x7d #x20 #x31 #xc9 #x31 #xd2
      #x31 #xf6 #xff #x53 #x50 #x48 #x83 #xc4 #x68 #x31 #xc0 #x5b
      #x5d #x41 #x5c #x41 #x5d #xc3 #x66 #x0f #x1f #x44 #x00 #x00
      #xf3 #x41 #x0f #x6f #x45 #x48 #x48 #x8d #x74 #x24 #x30 #xbf
      #x02 #x00 #x00 #x00 #x0f #x29 #x44 #x24 #x30 #xff #x93 #x88
      #x1d #x00 #x00 #x8d #x45 #xfd #xa8 #x07 #x0f #x84 #x3e #xff
      #xff #xff #x48 #x85 #xed #x74 #x0d #x49 #x8b #x04 #x24 #x49
      #x8b #x7d #x78 #x48 #x89 #xee #xff #x10 #x45 #x31 #xe4 #xe9
      #x37 #xff #xff #xff #x66 #x41 #x0f #x6f #x45 #x30 #x48 #x8d
      #x74 #x24 #x40 #xbf #x03 #x00 #x00 #x00 #x48 #xc7 #x44 #x24
      #x50 #x02 #x00 #x00 #x00 #x0f #x29 #x44 #x24 #x40 #xff #x93
      #x88 #x1d #x00 #x00 #xe9 #x53 #xff #xff #xff #x0f #x1f #x80
      #x00 #x00 #x00 #x00 #x48 #x85 #xf6 #x74 #xc3 #x49 #x8b #x04
      #x24 #x49 #x8b #x7d #x78 #xff #x10 #xeb #xb7]
         :gots '((16 . freloc) (60 . d-reloc))
         :imports '(1250 1320 1220 945 1335 10 0)
         :data '(3 4 5 6 7 9 10 15))
   (list 'lambda-nth-form
         :template
         [#x41 #x54 #xbe #x02 #x00 #x00 #x00 #xbf #x0a #x00 #x00 #x00
      #x55 #x53 #x48 #x83 #xec #x20 #x48 #x8b #x05 nil nil nil
      nil #x48 #x8b #x18 #xff #x93 #x20 #x26 #x00 #x00 #x4c #x8b
      #x25 nil nil nil nil #x48 #x89 #xc5 #x49 #x8b #x7c #x24
      #x18 #xff #x93 #xb8 #x29 #x00 #x00 #xf3 #x41 #x0f #x7e #x04
      #x24 #x66 #x48 #x0f #x6e #xcd #x48 #x89 #xe6 #x48 #x89 #x44
      #x24 #x10 #xbf #x03 #x00 #x00 #x00 #x66 #x0f #x6c #xc1 #x0f
      #x29 #x04 #x24 #xff #x93 #x88 #x1d #x00 #x00 #x48 #x83 #xc4
      #x20 #x5b #x5d #x41 #x5c #xc3]
         :gots '((21 . freloc) (37 . d-reloc))
         :imports '(1220 1335 945) :data '(0 3))
   (list 'lambda-cdr-form
         :template
         [#x55 #xbe #x02 #x00 #x00 #x00 #x53 #x48 #x83 #xec #x28 #x48
      #x8b #x05 nil nil nil nil #x48 #x8b #x2d nil nil nil
      nil #x48 #x8b #x18 #x48 #x8b #xbd #xc0 #x00 #x00 #x00 #xff
      #x13 #x48 #x8b #x7d #x18 #xff #x93 #xb8 #x29 #x00 #x00 #x48
      #x8b #x55 #x20 #x48 #x89 #xe6 #x48 #xc7 #x44 #x24 #x08 #x00
      #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x10 #xbf #x03 #x00 #x00
      #x00 #x48 #x89 #x14 #x24 #xff #x93 #x88 #x1d #x00 #x00 #x48
      #x83 #xc4 #x28 #x5b #x5d #xc3]
         :gots '((14 . freloc) (21 . d-reloc))
         :imports '(0 1335 945) :data '(3 4 24))
   (list 'if-form
         :template
         [#x41 #x57 #x8d #x47 #xfd #x41 #x56 #x41 #x55 #x49 #x89 #xfd
      #x41 #x54 #x55 #x53 #x48 #x81 #xec #x58 #x01 #x00 #x00 #x4c
      #x8b #x35 nil nil nil nil #x49 #x8b #x1e #xa8 #x07 #x0f
      #x85 #x3f #x03 #x00 #x00 #x48 #x8b #x77 #x05 #x4c #x8d #x7f
      #xfd #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #x2c #x04 #x00 #x00
      #xf3 #x0f #x7e #x4e #xfd #x4c #x8b #x25 nil nil nil nil
      #xf3 #x41 #x0f #x7e #x04 #x24 #x48 #x8d #x74 #x24 #x20 #xbf
      #x02 #x00 #x00 #x00 #x66 #x0f #x6c #xc1 #x0f #x29 #x44 #x24
      #x20 #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b #x77 #x08 #x8d
      #x46 #xfd #xa8 #x07 #x0f #x85 #x7a #x03 #x00 #x00 #x48 #x8b
      #x6e #xfd #x49 #x8b #x44 #x24 #x28 #x48 #x8d #x74 #x24 #x10
      #xbf #x01 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x10 #xff #x93
      #x88 #x1d #x00 #x00 #x49 #x8b #x77 #x08 #x48 #x89 #x04 #x24
      #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xd9 #x01 #x00 #x00 #x48
      #x8b #x76 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xca #x01
      #x00 #x00 #x48 #x83 #x7e #x05 #x00 #x0f #x84 #xd1 #x01 #x00
      #x00 #x49 #x8b #x44 #x24 #x28 #x48 #x8d #x74 #x24 #x18 #xbf
      #x01 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x18 #xff #x93 #x88
      #x1d #x00 #x00 #xf3 #x41 #x0f #x6f #x4c #x24 #x38 #xbf #x03
      #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #x90 #x00 #x00 #x00 #x48
      #x89 #x84 #x24 #xa0 #x00 #x00 #x00 #x0f #x29 #x8c #x24 #x90
      #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x08 #xff #x93 #x88 #x1d
      #x00 #x00 #x66 #x49 #x0f #x6e #xd5 #x48 #x8d #x74 #x24 #x30
      #xf3 #x41 #x0f #x7e #x4c #x24 #x58 #xbf #x02 #x00 #x00 #x00
      #x66 #x0f #x6c #xca #x0f #x29 #x4c #x24 #x30 #xff #x93 #xc8
      #x22 #x00 #x00 #x66 #x48 #x0f #x6e #xdd #xbf #x03 #x00 #x00
      #x00 #xf3 #x41 #x0f #x7e #x4c #x24 #x48 #x48 #x8d #xb4 #x24
      #xb0 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #xc0 #x00 #x00 #x00
      #x66 #x0f #x6c #xcb #x0f #x29 #x8c #x24 #xb0 #x00 #x00 #x00
      #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b #x44 #x24 #x38 #xbf
      #x03 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #xd0 #x00 #x00 #x00
      #x48 #x89 #x84 #x24 #xd0 #x00 #x00 #x00 #x49 #x8b #x44 #x24
      #x60 #x48 #x89 #x84 #x24 #xd8 #x00 #x00 #x00 #x48 #x8b #x04
      #x24 #x48 #x89 #x84 #x24 #xe0 #x00 #x00 #x00 #xff #x93 #x88
      #x1d #x00 #x00 #x48 #x8d #x74 #x24 #x40 #xbf #x02 #x00 #x00
      #x00 #xf3 #x0f #x7e #x44 #x24 #x08 #xf3 #x41 #x0f #x7e #x4c
      #x24 #x68 #x66 #x0f #x6c #xc8 #x0f #x29 #x4c #x24 #x40 #xff
      #x93 #x88 #x1d #x00 #x00 #x48 #x89 #xef #x31 #xf6 #xff #x93
      #xf8 #x22 #x00 #x00 #x49 #x8b #x7c #x24 #x70 #x48 #x89 #xc6
      #xff #x93 #xf8 #x22 #x00 #x00 #x66 #x49 #x0f #x6e #xe5 #x48
      #x8d #x74 #x24 #x50 #xf3 #x41 #x0f #x7e #x44 #x24 #x78 #x48
      #x89 #xc5 #xbf #x02 #x00 #x00 #x00 #x66 #x0f #x6c #xc4 #x0f
      #x29 #x44 #x24 #x50 #xff #x93 #xc8 #x22 #x00 #x00 #x66 #x48
      #x0f #x6e #xed #xbf #x03 #x00 #x00 #x00 #xf3 #x41 #x0f #x7e
      #x44 #x24 #x48 #x48 #x8d #xb4 #x24 #xf0 #x00 #x00 #x00 #x48
      #x89 #x84 #x24 #x00 #x01 #x00 #x00 #x66 #x0f #x6c #xc5 #x0f
      #x29 #x84 #x24 #xf0 #x00 #x00 #x00 #xff #x93 #x88 #x1d #x00
      #x00 #x48 #x8d #x74 #x24 #x60 #xbf #x02 #x00 #x00 #x00 #xf3
      #x41 #x0f #x7e #x44 #x24 #x68 #x0f #x16 #x04 #x24 #x0f #x29
      #x44 #x24 #x60 #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b #x7c
      #x24 #x18 #x31 #xc9 #x31 #xd2 #x31 #xf6 #xff #x53 #x50 #x48
      #x81 #xc4 #x58 #x01 #x00 #x00 #x31 #xc0 #x5b #x5d #x41 #x5c
      #x41 #x5d #x41 #x5e #x41 #x5f #xc3 #x66 #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x48 #x85 #xf6 #x74 #x0d #x49 #x8b #x06
      #x49 #x8b #xbc #x24 #xc0 #x00 #x00 #x00 #xff #x10 #x49 #x8b
      #x7c #x24 #x18 #xff #x93 #xb8 #x29 #x00 #x00 #x48 #x85 #xc0
      #x0f #x84 #x6a #x01 #x00 #x00 #xf3 #x41 #x0f #x7e #x4c #x24
      #x40 #xf3 #x41 #x0f #x7e #x44 #x24 #x38 #x48 #x8b #x04 #x24
      #xbf #x03 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #x10 #x01 #x00
      #x00 #x66 #x0f #x6c #xc1 #x48 #x89 #x84 #x24 #x20 #x01 #x00
      #x00 #x0f #x29 #x84 #x24 #x10 #x01 #x00 #x00 #xff #x93 #x88
      #x1d #x00 #x00 #x66 #x49 #x0f #x6e #xf5 #x48 #x8d #x74 #x24
      #x70 #xf3 #x41 #x0f #x7e #x44 #x24 #x58 #xbf #x02 #x00 #x00
      #x00 #x66 #x0f #x6c #xc6 #x0f #x29 #x44 #x24 #x70 #xff #x93
      #xc8 #x22 #x00 #x00 #x66 #x48 #x0f #x6e #xfd #xbf #x03 #x00
      #x00 #x00 #xf3 #x41 #x0f #x7e #x44 #x24 #x48 #x48 #x8d #xb4
      #x24 #x30 #x01 #x00 #x00 #x48 #x89 #x84 #x24 #x40 #x01 #x00
      #x00 #x66 #x0f #x6c #xc7 #x0f #x29 #x84 #x24 #x30 #x01 #x00
      #x00 #xff #x93 #x88 #x1d #x00 #x00 #xf3 #x41 #x0f #x7e #x44
      #x24 #x68 #xbf #x02 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #x80
      #x00 #x00 #x00 #x0f #x16 #x04 #x24 #x0f #x29 #x84 #x24 #x80
      #x00 #x00 #x00 #xff #x93 #x88 #x1d #x00 #x00 #xe9 #xf3 #xfe
      #xff #xff #x66 #x0f #x1f #x44 #x00 #x00 #x4c #x8b #x25 nil
      nil nil nil #x48 #x89 #xfd #x48 #x85 #xff #x0f #x84 #xa5
      #x00 #x00 #x00 #x49 #x8b #xbc #x24 #xc0 #x00 #x00 #x00 #x4c
      #x89 #xee #x31 #xed #xff #x13 #x49 #x8b #x04 #x24 #x48 #x8d
      #x74 #x24 #x20 #xbf #x02 #x00 #x00 #x00 #x48 #xc7 #x44 #x24
      #x28 #x00 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x20 #xff #x93
      #x88 #x1d #x00 #x00 #x49 #x8b #x06 #x4c #x89 #xee #x49 #x8b
      #xbc #x24 #xc0 #x00 #x00 #x00 #xff #x10 #x49 #x8b #x44 #x24
      #x28 #x48 #x8d #x74 #x24 #x10 #xbf #x01 #x00 #x00 #x00 #x48
      #x89 #x44 #x24 #x10 #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b
      #xbc #x24 #xc0 #x00 #x00 #x00 #x4c #x89 #xee #x48 #x89 #x04
      #x24 #x49 #x8b #x06 #xff #x10 #xe9 #xa3 #xfe #xff #xff #x90
      #x48 #x85 #xf6 #x74 #x0d #x49 #x8b #x06 #x49 #x8b #xbc #x24
      #xc0 #x00 #x00 #x00 #xff #x10 #x31 #xed #xe9 #x71 #xfc #xff
      #xff #x0f #x1f #x80 #x00 #x00 #x00 #x00 #xf3 #x41 #x0f #x7e
      #x8c #x24 #x80 #x00 #x00 #x00 #xe9 #x8e #xfe #xff #xff #x90
      #x49 #x8b #x04 #x24 #x48 #x8d #x74 #x24 #x20 #xbf #x02 #x00
      #x00 #x00 #x48 #xc7 #x44 #x24 #x28 #x00 #x00 #x00 #x00 #x48
      #x89 #x44 #x24 #x20 #xff #x93 #x88 #x1d #x00 #x00 #x49 #x8b
      #x44 #x24 #x28 #x48 #x8d #x74 #x24 #x10 #xbf #x01 #x00 #x00
      #x00 #x48 #x89 #x44 #x24 #x10 #xff #x93 #x88 #x1d #x00 #x00
      #x48 #x89 #x04 #x24 #xe9 #x2d #xfe #xff #xff #x0f #x1f #x00
      #x4c #x8b #x25 nil nil nil nil #x48 #x85 #xf6 #x74 #x0a
      #x49 #x8b #xbc #x24 #xc0 #x00 #x00 #x00 #xff #x13 #x66 #x0f
      #xef #xc9 #xe9 #xc1 #xfb #xff #xff]
         :gots '((26 . freloc) (68 . d-reloc) (875 . d-reloc) (1131 . d-reloc))
         :imports '(945 1113 1119 10 1335 0)
         :data '(0 3 5 7 8 9 11 12 13 14 15 16 24))
   (list 'accumulate-forms
         :template
         [#x41 #x57 #x41 #x56 #x41 #x55 #x41 #x54 #x49 #x89 #xfc #x55
      #x53 #x48 #x89 #xfb #x48 #x83 #xec #x68 #x4c #x8b #x3d nil
      nil nil nil #x4c #x8b #x35 nil nil nil nil #x48 #x8d #x4c
      #x24 #x50 #x48 #x89 #x74 #x24 #x18 #x49 #x8b #x07 #x49 #x8b
      #x2e #x48 #x89 #x7c #x24 #x08 #x48 #x89 #x4c #x24 #x28 #x48
      #x89 #x04 #x24 #x8d #x47 #xfd #xa8 #x07 #x0f #x85 #xc7 #x01
      #x00 #x00 #x0f #x1f #x44 #x00 #x00 #x49 #x83 #x7f #x28 #x00
      #x0f #x84 #xb7 #x01 #x00 #x00 #x49 #x8d #x44 #x24 #xfd #x48
      #x83 #x7c #x24 #x18 #x00 #x4d #x8b #x6c #x24 #xfd #x48 #x89
      #x44 #x24 #x20 #x74 #x2c #x48 #x8b #x44 #x24 #x18 #x48 #x8b
      #x74 #x24 #x28 #xbf #x02 #x00 #x00 #x00 #x48 #xc7 #x44 #x24
      #x58 #x02 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x50 #xff #x95
      #x40 #x29 #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x02 #x02 #x00
      #x00 #xf3 #x41 #x0f #x7e #x47 #x08 #x66 #x49 #x0f #x6e #xcd
      #x48 #x8d #x74 #x24 #x40 #xbf #x02 #x00 #x00 #x00 #x66 #x0f
      #x6c #xc1 #x0f #x29 #x44 #x24 #x40 #xff #x95 #x88 #x1d #x00
      #x00 #x48 #x89 #x44 #x24 #x10 #x48 #x8b #x44 #x24 #x10 #x49
      #x39 #xc5 #x0f #x84 #xeb #x00 #x00 #x00 #x48 #x8b #x05 nil
      nil nil nil #x48 #x8b #x00 #x80 #x38 #x00 #x74 #x25 #xe9 #xb7
      #x00 #x00 #x00 #x0f #x1f #x80 #x00 #x00 #x00 #x00 #xc7 #x05
      nil nil nil nil #x00 #x00 #x00 #x00 #x49 #x8b #x06 #xff #x50
      #x68 #x49 #x8b #x06 #xff #x50 #x70 #x4c #x89 #xeb #x8d #x43
      #xfd #x83 #xe0 #x07 #x41 #x89 #xc5 #x4c #x39 #xe3 #x74 #x51
      #x48 #x8b #x05 nil nil nil nil #x48 #x8b #x00 #x80 #x38 #x00
      #x0f #x85 #x26 #x01 #x00 #x00 #x45 #x85 #xed #x75 #x61 #x4c
      #x8b #x6b #x05 #x48 #x89 #xdf #xff #x95 #x50 #x2a #x00 #x00
      #x48 #x8b #x34 #x24 #x48 #x89 #xc7 #xff #x95 #xf8 #x22 #x00
      #x00 #x48 #x89 #x04 #x24 #x8b #x05 nil nil nil nil #x83 #xc0
      #x01 #x89 #xc2 #xc1 #xea #x09 #x75 #x93 #x89 #x05 nil nil nil
      nil #xeb #xa1 #x0f #x1f #x00 #x49 #x83 #x7f #x28 #x00 #x0f
      #x85 #x0d #x01 #x00 #x00 #x85 #xc0 #x74 #xb8 #x49 #x8b #x06
      #x49 #x8b #x7f #x38 #x48 #x89 #xde #xff #x10 #x45 #x31 #xed
      #xeb #xab #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x48 #x85
      #xdb #x75 #xe2 #xeb #xec #x66 #x0f #x1f #x84 #x00 #x00 #x00
      #x00 #x00 #x48 #x8b #x74 #x24 #x10 #x4c #x89 #xef #xff #x55
      #x38 #x84 #xc0 #x0f #x84 #x56 #xff #xff #xff #x66 #x90 #x66
      #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x49 #x83
      #x7f #x28 #x00 #x0f #x84 #x3e #xff #xff #xff #x48 #x8b #x44
      #x24 #x20 #x48 #x8b #x40 #x08 #x48 #x89 #x44 #x24 #x08 #x8b
      #x05 nil nil nil nil #x83 #xc0 #x01 #x89 #xc1 #xc1 #xe9 #x09
      #x74 #x5f #xc7 #x05 nil nil nil nil #x00 #x00 #x00 #x00 #x49
      #x8b #x06 #xff #x50 #x68 #x49 #x8b #x06 #xff #x50 #x70 #x48
      #x8b #x44 #x24 #x08 #x49 #x89 #xc4 #x83 #xe8 #x03 #xa8 #x07
      #x0f #x84 #x3e #xfe #xff #xff #x48 #x8b #x3c #x24 #xff #x95
      #xc8 #x25 #x00 #x00 #x48 #x89 #x5c #x24 #x38 #x48 #x8d #x74
      #x24 #x30 #xbf #x02 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x30
      #xff #x95 #x60 #x25 #x00 #x00 #x48 #x83 #xc4 #x68 #x5b #x5d
      #x41 #x5c #x41 #x5d #x41 #x5e #x41 #x5f #xc3 #x0f #x1f #x00
      #x89 #x05 nil nil nil nil #xeb #xaf #x48 #x8b #x74 #x24 #x08
      #x48 #x89 #xdf #xff #x55 #x38 #x84 #xc0 #x0f #x84 #xc7 #xfe
      #xff #xff #x49 #x83 #x7f #x28 #x00 #x0f #x84 #xbc #xfe #xff
      #xff #x45 #x85 #xed #x74 #x11 #x48 #x85 #xdb #x0f #x85 #x81
      #x00 #x00 #x00 #x31 #xdb #xeb #x08 #x85 #xc0 #x75 #x79 #x48
      #x8b #x5b #x05 #x48 #x8b #x34 #x24 #x48 #x8b #x7c #x24 #x10
      #xff #x95 #xf8 #x22 #x00 #x00 #x48 #x89 #x04 #x24 #xe9 #x2b
      #xff #xff #xff #x48 #x8b #x4c #x24 #x18 #x89 #xc8 #x83 #xe8
      #x02 #xa8 #x03 #x75 #x32 #x48 #x89 #xc8 #x48 #xb9 #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #xe0 #x48 #xc1 #xf8 #x02 #x48 #x39
      #xc8 #x74 #x1c #x48 #x8d #x04 #x85 #xfe #xff #xff #xff #x4c
      #x89 #x6c #x24 #x10 #x48 #x89 #x44 #x24 #x18 #xe9 #xec #xfd
      #xff #xff #x0f #x1f #x44 #x00 #x00 #x49 #x8b #x06 #x48 #x8b
      #x7c #x24 #x18 #xff #x90 #xa0 #x28 #x00 #x00 #x4c #x89 #x6c
      #x24 #x10 #x48 #x89 #x44 #x24 #x18 #xe9 #xca #xfd #xff #xff
      #x49 #x8b #x06 #x48 #x89 #xde #x49 #x8b #x7f #x38 #x31 #xdb
      #xff #x10 #xe9 #x78 #xff #xff #xff]
         :gots '((23 . d-reloc) (30 . freloc)
                 (216 . symbols-with-pos) (282 . symbols-with-pos))
         :module-counter '((242 . 4) (335 . 0) (351 . 0) (475 . 0)
                           (491 . 4) (586 . 0))
         :imports '(1320 945 7 1354 1119 1209 1196 1300 0 13 14)
         :data '(0 1 5 7)))
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

`parse-body' is vendor macroexp.el `macroexp-parse-body' (525 bytes,
S6.5): a `while' over BODY that, per element E = (car BODY) (BODY
tag-checked; a non-list tail calls slot 0 `wrong_type_argument' with
d_reloc[5], `listp'), tests slot 1376 `Fstringp' of E (then requires a
non-nil (cdr BODY)) or else slot 1217 `Fmemq' of slot 1354 `Fcar_safe'
\(E) in d_reloc[1] (the quoted list (:documentation declare interactive
cl-declare)), and pushes each matched element on DECLS through slot 1119
`Fcons' (DECLS starting at d_reloc[0], nil); it ends with a tail JMP
through slot 1119 `Fcons' of slot 1209 `Fnreverse' (DECLS) and the
remaining BODY.  Every iteration increments the module-local `quitcounter'
by direct RIP-relative access and every 512th one calls slots 13
`maybe_gc' and 14 `maybe_quit' (see `set-difference'); the seven
accesses are listed in :MODULE-COUNTER.  Eight distinct freloc slots use
every one of the eight callback ports.

`for-effect-constant' is vendor bytecomp.el `byte-compile-constant'
\(110 bytes, S6.15): slot 1335 `Fsymbol_value' of d_reloc[0]
\(`byte-compile--for-effect'); a nil value makes a MANY (2, argv) call
through slot 945 `Ffuncall' of d_reloc[2] (`byte-compile-push-constant')
on the argument and returns its value, a non-nil one calls slot 10
`set_internal' (d_reloc[0], nil, nil, SET_INTERNAL_SET) and returns nil.

`setq-form' is vendor bytecomp.el `byte-compile-setq' (369 bytes, S6.13):
`cl-assert' of (= (length FORM) 3) -- slot 1250 `Flength', then a MANY
\(2, argv) slot 1320 `Feqlsign' against 3, a failure calling slot 945
`Ffuncall' of d_reloc[9] (`cl--assertion-failed') on d_reloc[10] (the
quoted assertion form); VAR = (nth 1 FORM) inline, list-tag checked (a
non-list calls slot 0 `wrong_type_argument' with d_reloc[15], `listp');
then `Ffuncall' of d_reloc[3] (`byte-compile-form') on slot 1220 `Fnth'
\(2, FORM); unless slot 1335 `Fsymbol_value' of d_reloc[4]
\(`byte-compile--for-effect') is non-nil, a MANY (3, argv) `Ffuncall' of
d_reloc[6] (`byte-compile-out') on d_reloc[7] (`byte-dup') and 0; then
`Ffuncall' of d_reloc[5] (`byte-compile-variable-set') on VAR, and slot 10
`set_internal' (d_reloc[4], nil, nil, SET_INTERNAL_SET); it returns nil.
`Ffuncall' is thus called with two different argument counts through its
one slot.

`lambda-nth-form' and `lambda-cdr-form' are the two native anonymous
lambdas GNU emits next to vendor bytecomp.el `byte-compile-if' (102 and
90 bytes, S6.12): with the closure variable left as the placeholder
fixnum 0 they compute (byte-compile-form (nth 2 V0) FOR-EFFECT) through
slots 1220 `Fnth', 1335 `Fsymbol_value' and 945 `Ffuncall', and
(byte-compile-body (cdr (cdr (cdr V0))) FOR-EFFECT), whose first `cdr' of
that placeholder calls slot 0 `wrong_type_argument' (`listp').  Nothing
in the admitted registration reads the `d_reloc' slots that hold them.

`if-form' is vendor bytecomp.el `byte-compile-if' itself (1159 bytes,
S6.12): five `Ffuncall' calls of `byte-compile-form', `byte-compile-make-tag',
`byte-compile-goto', `byte-compile-out-tag' and friends on inline `car'/`cdr'
walks of FORM, two slot 1113 `Fmake_closure' calls on the byte-code
prototypes d_reloc[11] and d_reloc[15] and FORM, slot 1119 `Fcons' to build
`(not CLAUSE)', a final slot 10 `set_internal' of `byte-compile--for-effect'
to nil, and slot 0 `wrong_type_argument' with d_reloc[24] (`listp') on any
non-list walk.  The closures are only ever passed on to `Ffuncall'; the body
never reads through them.")

;;; S6.6: `cconv-closure-convert' (vendor cconv.el).  Kept in its own constant
;;; (appended to the shape list below) so concurrent lanes adding shapes
;;; do not touch the same source lines.

(defconst nelisp-eln-tail-code--multi-import-shapes-cconv
  (list
   (list 'closure-convert
         :template
         [#x41 #x55 #x49 #x89 #xf5 #x31 #xf6 #x41 #x54 #x49 #x89 #xfc
      #x55 #x53 #x48 #x83 #xec #x58 #x48 #x8b #x05 nil nil nil nil
      #x48 #x8b #x2d nil nil nil nil #x48 #x8b #x18 #x48 #x8b #x7d
      #x08 #xff #x53 #x60 #x31 #xf6 #x48 #x8b #x7d #x10 #xff #x53
      #x60 #x48 #x8b #x7d #x18 #x4c #x89 #xee #xff #x53 #x60 #xf3
      #x0f #x7e #x45 #x20 #x66 #x49 #x0f #x6e #xcc #x48 #x8d #x74
      #x24 #x10 #x48 #xc7 #x44 #x24 #x20 #x00 #x00 #x00 #x00 #xbf
      #x03 #x00 #x00 #x00 #x66 #x0f #x6c #xc1 #x0f #x29 #x44 #x24
      #x10 #xff #x93 #x88 #x1d #x00 #x00 #x48 #x8b #x7d #x10 #xff
      #x93 #xb8 #x29 #x00 #x00 #x48 #x89 #xc7 #xff #x93 #xc8 #x25
      #x00 #x00 #x31 #xc9 #x31 #xd2 #x48 #x8b #x7d #x10 #x48 #x89
      #xc6 #xff #x53 #x50 #xf3 #x0f #x7e #x45 #x28 #x66 #x49 #x0f
      #x6e #xd4 #xbf #x04 #x00 #x00 #x00 #x48 #x8d #x74 #x24 #x30
      #x66 #x0f #x6c #xc2 #x0f #x29 #x44 #x24 #x30 #x66 #x0f #xef
      #xc0 #x0f #x29 #x44 #x24 #x40 #xff #x93 #x88 #x1d #x00 #x00
      #x48 #x8b #x7d #x10 #x49 #x89 #xc4 #xff #x93 #xb8 #x29 #x00
      #x00 #x48 #x85 #xc0 #x74 #x17 #xf3 #x0f #x6f #x45 #x38 #x48
      #x89 #xe6 #xbf #x02 #x00 #x00 #x00 #x0f #x29 #x04 #x24 #xff
      #x93 #x88 #x1d #x00 #x00 #xbf #x0e #x00 #x00 #x00 #xff #x53
      #x20 #x48 #x83 #xc4 #x58 #x4c #x89 #xe0 #x5b #x5d #x41 #x5c
      #x41 #x5d #xc3]
         :gots '((21 . freloc) (28 . d-reloc))
         :imports '(12 945 1335 1209 10 4)
         :data '(1 2 3 4 5 7 8)))
  "Exact multi-import shapes for vendor cconv.el bodies (S6).

`closure-convert' is `cconv-closure-convert' (245 bytes, S6.6), whose
arguments are (FORM DYNBOUND-VARS): it `specbind's (slot 12) the three
specials d_reloc[1] `cconv-var-classification' and d_reloc[2]
`cconv-freevars-alist' to nil and d_reloc[3] `cconv--dynbound-variables'
to DYNBOUND-VARS; a MANY (3, argv) slot 945 `Ffuncall' of d_reloc[4]
`cconv-analyze-form' on (FORM nil); then slot 1335 `Fsymbol_value' of
d_reloc[2], slot 1209 `Fnreverse' and slot 10 `set_internal' store the
reversed list back; a MANY (4, argv) `Ffuncall' of d_reloc[5]
`cconv-convert' on (FORM nil nil) gives the result; when slot 1335
`Fsymbol_value' of d_reloc[2] is then non-nil a MANY (2, argv)
`Ffuncall' of d_reloc[7] `cl--assertion-failed' on d_reloc[8] (the quoted
assertion form) runs; finally slot 4 `helper_unbind_n' unbinds the three
specbinds (fixnum 3) and the result is returned.  `Ffuncall' is called
with three different argument counts through its one slot.

`accumulate-forms' is vendor macroexp.el `macroexp--all-forms' (784
bytes, S6.3), the `macroexp--accumulate' loop over FORMS with an
optional SKIP: while SKIP is nil (slot 0 `Qnil') or slot 1320 `Feqlsign'
\(SKIP, 0) is true, slot 945 `Ffuncall' of d_reloc[1]
\(`macroexp--expand-all') on the current element (MANY, 2); otherwise SKIP
is decremented (inline for a fixnum, else slot 1300 `Fsub1') and the
element itself, read with slot 1354 `Fcar_safe', is kept.  Results are
consed with slot 1119 `Fcons'.  A non-list tail is rejected through slot
0 `wrong_type_argument' with d_reloc[7] (`listp'), d_reloc[5] (t) guards
the loop, and the end returns slot 1196 `Fnconc' (slot 1209 `Fnreverse'
of the accumulator, the remaining tail) as a MANY (2, argv) call.  Each
iteration bumps the module-local `quitcounter' (every 512th calls slot 13
`maybe_gc' then slot 14 `maybe_quit'), and `symbols_with_pos_enabled'
is read through f_symbols_with_pos_enabled_reloc at two sites (both must
reach the same cell), enabling slot 7 `slow_eq' comparisons only when
set (never, in NeLisp).  It is the first admitted shape with an
optional argument: arity 2, minimum 1.")

(unless (assq 'closure-convert nelisp-eln-tail-code--multi-import-shapes)
  (setq nelisp-eln-tail-code--multi-import-shapes
        (append nelisp-eln-tail-code--multi-import-shapes
                nelisp-eln-tail-code--multi-import-shapes-cconv)))

;;; S6.4: `macroexpand-1' (vendor macroexp.el).  Kept in its own constant
;;; (appended to the shape list below) so concurrent lanes adding shapes
;;; do not touch the same source lines.

(defconst nelisp-eln-tail-code--multi-import-shapes-macroexpand
  (list
   (list 'macroexpand-1
         :template
         [#x41 #x57 #x8d #x47 #xfd #x41 #x56 #x41 #x55 #x41 #x54 #x55
      #x53 #x48 #x89 #xfb #x48 #x83 #xec #x28 #xa8 #x07 #x74 #x18
      #x48 #x89 #xd8 #x48 #x83 #xc4 #x28 #x5b #x5d #x41 #x5c #x41
      #x5d #x41 #x5e #x41 #x5f #xc3 #x66 #x0f #x1f #x44 #x00 #x00
      #x48 #x8b #x2d nil nil nil nil #x48 #x83 #x7d #x38 #x00
      #x74 #xda #x4c #x8b #x35 nil nil nil nil #x4c #x8b #x6f
      #xfd #x4c #x8d #x7f #xfd #x4d #x8b #x26 #x4c #x89 #xef #x41
      #xff #x94 #x24 #xf8 #x25 #x00 #x00 #x48 #x89 #xc6 #x48 #x85
      #xc0 #x74 #x3d #x8d #x40 #xfd #xa8 #x07 #x0f #x85 #x52 #x01
      #x00 #x00 #x48 #x8b #x46 #x05 #x48 #x85 #xc0 #x74 #xa1 #x66
      #x48 #x0f #x6e #xc0 #x48 #x8d #x74 #x24 #x10 #xbf #x02 #x00
      #x00 #x00 #x41 #x0f #x16 #x47 #x08 #x0f #x29 #x44 #x24 #x10
      #x41 #xff #x94 #x24 #x90 #x1d #x00 #x00 #xeb #x81 #x66 #x0f
      #x1f #x44 #x00 #x00 #x4c #x89 #xef #x41 #xff #x94 #x24 #x10
      #x2b #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x64 #xff #xff #xff
      #x4c #x89 #xef #x41 #xff #x94 #x24 #xd8 #x29 #x00 #x00 #x48
      #x85 #xc0 #x0f #x84 #x50 #xff #xff #xff #x4c #x89 #xef #x41
      #xff #x94 #x24 #x30 #x2a #x00 #x00 #x4c #x89 #xee #x48 #x8b
      #x55 #x18 #x48 #x89 #xc7 #x41 #xff #x94 #x24 #xa0 #x1d #x00
      #x00 #x49 #x89 #xc5 #x48 #x89 #xc7 #x41 #xff #x94 #x24 #x10
      #x2b #x00 #x00 #x48 #x85 #xc0 #x74 #x48 #xf3 #x0f #x7e #x45
      #x28 #x66 #x49 #x0f #x6e #xcd #x48 #x8d #x74 #x24 #x10 #xbf
      #x02 #x00 #x00 #x00 #x66 #x0f #x6c #xc1 #x0f #x29 #x44 #x24
      #x10 #x41 #xff #x94 #x24 #x88 #x1d #x00 #x00 #x48 #x85 #xc0
      #x74 #x1e #x49 #x8b #x77 #x08 #x4c #x89 #xef #x41 #xff #x94
      #x24 #xf8 #x22 #x00 #x00 #xe9 #xe5 #xfe #xff #xff #x66 #x2e
      #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x41 #x8d #x45 #xfd
      #xa8 #x07 #x0f #x85 #xcc #xfe #xff #xff #x48 #x83 #x7d #x38
      #x00 #x0f #x84 #xc1 #xfe #xff #xff #x49 #x8b #x7d #xfd #x48
      #x8b #x75 #x18 #x4d #x8d #x75 #xfd #x48 #x39 #xf7 #x74 #x2b
      #x48 #x8b #x05 nil nil nil nil #x48 #x8b #x00 #x80 #x38
      #x00 #x0f #x84 #x9d #xfe #xff #xff #x41 #xff #x54 #x24 #x38
      #x84 #xc0 #x0f #x84 #x90 #xfe #xff #xff #x48 #x83 #x7d #x38
      #x00 #x0f #x84 #x85 #xfe #xff #xff #xf3 #x41 #x0f #x7e #x46
      #x08 #x48 #x89 #xe6 #xbf #x02 #x00 #x00 #x00 #x41 #x0f #x16
      #x47 #x08 #x0f #x29 #x04 #x24 #x41 #xff #x94 #x24 #x90 #x1d
      #x00 #x00 #xe9 #x64 #xfe #xff #xff #x66 #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x49 #x8b #x06 #x48 #x8b #x7d #x48 #xff
      #x10 #xe9 #x4a #xfe #xff #xff]
         :gots '((51 . d-reloc) (65 . freloc) (363 . symbols-with-pos))
         :imports '(0 7 945 946 948 1119 1215 1339 1350 1378)
         :data '(3 5 7 9)))
  "Exact GNU 31.1 body of vendor macroexp.el `macroexpand-1' (462 bytes,
S6.4; `(FORM &optional ENVIRONMENT)', so the body takes two Lisp arguments
and its registration is the variable-arity 1..2 `gnu-verified-subr' shape).
A non-cons FORM is returned.  Otherwise HEAD = (car FORM) goes to slot
1215 `Fassq' with ENVIRONMENT (a non-nil, non-cons answer calls slot 0
`wrong_type_argument' with d_reloc[9], `listp'); a found entry's non-nil
cdr is applied to (cdr FORM) through the MANY (2, argv) slot 946 `Fapply',
a nil cdr returns FORM.  Without an entry, a HEAD that is not a symbol
\(slot 1378 `Fsymbolp') accepted by slot 1339 `Ffboundp' returns FORM;
else DEF = slot 948 `Fautoload_do_load' of slot 1350 `Fsymbol_function'
\(HEAD), HEAD and d_reloc[3] (`macro').  A symbol DEF that a MANY (2, argv)
slot 945 `Ffuncall' of d_reloc[5] (`macrop') accepts gives (slot 1119
`Fcons' DEF (cdr FORM)); a non-cons DEF gives FORM; a cons whose car is
`eq' d_reloc[3] (the inline compare falls back to slot 7 `slow_eq' only
when `symbols_with_pos_enabled', read through
f_symbols_with_pos_enabled_reloc, is set) has its cdr applied to
\(cdr FORM) through slot 946.  Ten distinct freloc slots use ten callback
ports; d_reloc[7] (t) is compared with nil inline.")

(unless (assq 'macroexpand-1 nelisp-eln-tail-code--multi-import-shapes)
  (setq nelisp-eln-tail-code--multi-import-shapes
        (append nelisp-eln-tail-code--multi-import-shapes
                nelisp-eln-tail-code--multi-import-shapes-macroexpand)))

;;; S6.9: `byte-compile-lambda' (vendor bytecomp.el).  Kept in its own
;;; constant (appended to the shape list below).

(defconst nelisp-eln-tail-code--multi-import-shapes-lambda
  (list
   (list 'lambda-form
         :template
         [#x41 #x57 #x41 #x56 #x41 #x55 #x41 #x54 #x55 #x48 #x89 #xfd #x53
      #x48 #x81 #xec #x48 #x02 #x00 #x00 #x4c #x8b #x35 nil nil nil nil
      #x48 #x89 #x74 #x24 #x18 #x4d #x8b #x3e #x41 #xff #x97 #x50 #x2a
      #x00 #x00 #x48 #x8b #x1d nil nil nil nil #x48 #x8b #x33 #x48 #x39
      #xf0 #x74 #x67 #x48 #x8b #x15 nil nil nil nil #x48 #x8b #x12 #x80
      #x3a #x00 #x75 #x48 #x66 #x0f #x6f #x83 #x50 #x01 #x00 #x00 #x48
      #x89 #xac #x24 #x20 #x02 #x00 #x00 #xbf #x03 #x00 #x00 #x00 #x48
      #x8d #xb4 #x24 #x10 #x02 #x00 #x00 #x0f #x29 #x84 #x24 #x10 #x02
      #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00 #x45 #x31 #xed #x48
      #x81 #xc4 #x48 #x02 #x00 #x00 #x4c #x89 #xe8 #x5b #x5d #x41 #x5c
      #x41 #x5d #x41 #x5e #x41 #x5f #xc3 #x0f #x1f #x40 #x00 #x48 #x89
      #xc7 #x41 #xff #x57 #x38 #x84 #xc0 #x74 #xad #x0f #x1f #x44 #x00
      #x00 #x48 #x83 #xbb #x70 #x01 #x00 #x00 #x00 #x74 #x9e #x44 #x8d
      #x65 #xfd #x41 #x83 #xe4 #x07 #x0f #x85 #x60 #x0a #x00 #x00 #x48
      #x8b #x75 #x05 #x4c #x8d #x6d #xfd #x8d #x46 #xfd #xa8 #x07 #x0f
      #x85 #xf7 #x0a #x00 #x00 #xf3 #x0f #x7e #x4e #xfd #xf3 #x0f #x7e
      #x43 #x10 #x48 #x8d #x74 #x24 #x50 #xbf #x02 #x00 #x00 #x00 #x66
      #x0f #x6c #xc1 #x0f #x29 #x44 #x24 #x50 #x41 #xff #x97 #x88 #x1d
      #x00 #x00 #x49 #x8b #x75 #x08 #x8d #x46 #xfd #xa8 #x07 #x0f #x85
      #x92 #x04 #x00 #x00 #x48 #x8b #x46 #xfd #x48 #x89 #x44 #x24 #x20
      #xf3 #x0f #x7e #x43 #x18 #x48 #x8d #x74 #x24 #x60 #xbf #x02 #x00
      #x00 #x00 #x0f #x16 #x44 #x24 #x20 #x0f #x29 #x44 #x24 #x60 #x41
      #xff #x97 #x88 #x1d #x00 #x00 #xf3 #x0f #x7e #x43 #x20 #x48 #x8d
      #x74 #x24 #x70 #xbf #x02 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x30
      #x0f #x16 #x44 #x24 #x20 #x0f #x29 #x44 #x24 #x70 #x41 #xff #x97
      #x88 #x1d #x00 #x00 #xf3 #x0f #x7e #x43 #x18 #xbf #x02 #x00 #x00
      #x00 #x48 #x8d #xb4 #x24 #x80 #x00 #x00 #x00 #x66 #x48 #x0f #x6e
      #xd8 #x66 #x0f #x6c #xc3 #x0f #x29 #x84 #x24 #x80 #x00 #x00 #x00
      #x41 #xff #x97 #x88 #x1d #x00 #x00 #x48 #x8b #x7b #x30 #x48 #x89
      #x44 #x24 #x40 #x41 #xff #x97 #xb8 #x29 #x00 #x00 #x48 #x85 #xc0
      #x0f #x84 #xe1 #x03 #x00 #x00 #x48 #x8b #x15 nil nil nil nil #x48
      #x8b #x12 #x80 #x3a #x00 #x0f #x85 #xaf #x03 #x00 #x00 #x45 #x31
      #xed #x48 #x8b #x7b #x38 #x41 #xff #x97 #xb8 #x29 #x00 #x00 #x48
      #x8d #xb4 #x24 #x90 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00 #x4c
      #x89 #xac #x24 #x90 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #x98 #x00
      #x00 #x00 #x41 #xff #x97 #xa0 #x26 #x00 #x00 #x48 #x8b #x7b #x38
      #x48 #x89 #xc6 #x41 #xff #x57 #x60 #x45 #x85 #xe4 #x0f #x85 #xe0
      #x08 #x00 #x00 #x48 #x8b #x75 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f
      #x85 #xb1 #x09 #x00 #x00 #x48 #x8b #x6e #x05 #x8d #x45 #xfd #xa8
      #x07 #x0f #x85 #xba #x07 #x00 #x00 #x48 #x83 #x7d #x05 #x00 #x4c
      #x8d #x65 #xfd #x0f #x84 #xbf #x07 #x00 #x00 #x48 #x8b #x7d #xfd
      #x41 #xff #x97 #x00 #x2b #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #xab
      #x07 #x00 #x00 #x48 #x8b #x45 #xfd #x49 #x8b #x6c #x24 #x08 #x48
      #x89 #x44 #x24 #x10 #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
      #x48 #x8b #x7b #x40 #x48 #x89 #xee #x41 #xff #x97 #xf8 #x25 #x00
      #x00 #x48 #x8b #x7b #x30 #x48 #x89 #x44 #x24 #x08 #x48 #x8b #x43
      #x08 #x48 #x89 #x44 #x24 #x38 #x41 #xff #x97 #xb8 #x29 #x00 #x00
      #x48 #x85 #xc0 #x0f #x84 #xf0 #x00 #x00 #x00 #x48 #x83 #x7c #x24
      #x20 #x00 #x74 #x3b #xf3 #x0f #x7e #x83 #x48 #x01 #x00 #x00 #x48
      #x8b #x44 #x24 #x30 #xbf #x03 #x00 #x00 #x00 #x48 #x8d #xb4 #x24
      #x60 #x01 #x00 #x00 #x0f #x16 #x44 #x24 #x10 #x48 #x89 #x84 #x24
      #x70 #x01 #x00 #x00 #x0f #x29 #x84 #x24 #x60 #x01 #x00 #x00 #x41
      #xff #x97 #x88 #x1d #x00 #x00 #x48 #x89 #x44 #x24 #x10 #x48 #x8b
      #x44 #x24 #x40 #x48 #x85 #xc0 #x0f #x84 #x9f #x00 #x00 #x00 #x49
      #x89 #xc5 #x48 #x8d #x84 #x24 #x40 #x01 #x00 #x00 #x48 #x89 #x44
      #x24 #x28 #x4d #x89 #xec #x90 #x66 #x66 #x2e #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x41 #x8d #x44 #x24 #xfd #xa8 #x07 #x0f #x85
      #x53 #x06 #x00 #x00 #x4d #x8d #x6c #x24 #xfd #x4d #x8b #x64 #x24
      #xfd #x48 #x8b #xbb #x38 #x01 #x00 #x00 #x41 #xff #x97 #xb8 #x29
      #x00 #x00 #x48 #x89 #xc6 #x4c #x89 #xe7 #x41 #xff #x97 #xf8 #x25
      #x00 #x00 #x48 #x85 #xc0 #x74 #x35 #x48 #x8b #x03 #x66 #x49 #x0f
      #x6e #xd4 #x48 #x8b #x74 #x24 #x28 #xbf #x03 #x00 #x00 #x00 #xf3
      #x0f #x7e #x83 #x40 #x01 #x00 #x00 #x48 #x89 #x84 #x24 #x50 #x01
      #x00 #x00 #x66 #x0f #x6c #xc2 #x0f #x29 #x84 #x24 #x40 #x01 #x00
      #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00 #x4d #x8b #x65 #x08 #xe8
      #x5b #xfc #xff #xff #x4d #x85 #xe4 #x75 #x86 #x66 #x0f #x1f #x44
      #x00 #x00 #x48 #x8b #x7c #x24 #x10 #x41 #xff #x97 #x00 #x2b #x00
      #x00 #x48 #x85 #xc0 #x74 #x58 #x48 #x8b #x83 #x28 #x01 #x00 #x00
      #x48 #x8d #xb4 #x24 #xe0 #x01 #x00 #x00 #xf3 #x0f #x7e #x83 #x20
      #x01 #x00 #x00 #x48 #xc7 #x84 #x24 #xf8 #x01 #x00 #x00 #x00 #x00
      #x00 #x00 #xbf #x05 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #xf0 #x01
      #x00 #x00 #x48 #x8b #x83 #x30 #x01 #x00 #x00 #x0f #x16 #x44 #x24
      #x10 #x0f #x29 #x84 #x24 #xe0 #x01 #x00 #x00 #x48 #x89 #x84 #x24
      #x00 #x02 #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00 #x48 #x89
      #x44 #x24 #x10 #x48 #x83 #x7c #x24 #x08 #x00 #x0f #x84 #x43 #x06
      #x00 #x00 #x48 #x8b #x4c #x24 #x08 #x8d #x45 #xfd #x83 #xe0 #x07
      #x48 #x89 #x4c #x24 #x28 #x41 #x89 #xc4 #x0f #x85 #x92 #x09 #x00
      #x00 #x48 #x8b #x75 #xfd #x48 #x8b #x4c #x24 #x08 #x48 #x8d #x45
      #xfd #x48 #x39 #xce #x0f #x84 #xb9 #x09 #x00 #x00 #x48 #x8b #x05
      nil nil nil nil #x48 #x8b #x00 #x80 #x38 #x00 #x0f #x85 #xd7 #x0a
      #x00 #x00 #x8b #x44 #x24 #x08 #x83 #xe8 #x03 #xa8 #x07 #x0f #x85
      #x12 #x09 #x00 #x00 #x48 #x8b #x44 #x24 #x08 #x4c #x8d #x60 #xfd
      #x49 #x8b #x44 #x24 #x08 #x8d #x50 #xfd #x83 #xe2 #x07 #x0f #x85
      #x6f #x01 #x00 #x00 #x48 #x83 #xbb #x70 #x01 #x00 #x00 #x00 #x0f
      #x84 #x66 #x01 #x00 #x00 #x48 #x8b #x40 #x05 #xf3 #x0f #x6f #x83
      #xf8 #x00 #x00 #x00 #xbf #x03 #x00 #x00 #x00 #x48 #x8d #xb4 #x24
      #x20 #x01 #x00 #x00 #x48 #x89 #x84 #x24 #x30 #x01 #x00 #x00 #x0f
      #x29 #x84 #x24 #x20 #x01 #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00
      #x00 #x48 #x85 #xc0 #x0f #x84 #x55 #x07 #x00 #x00 #x49 #x8b #x74
      #x24 #x08 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #x88 #x07 #x00 #x00
      #x48 #x8b #x46 #x05 #x48 #x83 #xee #x03 #x48 #x89 #x44 #x24 #x38
      #x4c #x8b #x2e #xf3 #x0f #x7e #x43 #x48 #x66 #x49 #x0f #x6e #xed
      #xbf #x02 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #xe0 #x00 #x00 #x00
      #x66 #x0f #x6c #xc5 #x0f #x29 #x84 #x24 #xe0 #x00 #x00 #x00 #x41
      #xff #x97 #x88 #x1d #x00 #x00 #x48 #x89 #x6c #x24 #x28 #x48 #x89
      #x44 #x24 #x48 #x4c #x89 #xe0 #x4d #x89 #xec #x49 #x89 #xc5 #x90
      #x66 #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x4c #x89
      #xe7 #x41 #xff #x97 #x50 #x2a #x00 #x00 #x48 #x8b #xb3 #x08 #x01
      #x00 #x00 #x48 #x89 #xc7 #x41 #xff #x97 #x08 #x26 #x00 #x00 #x48
      #x85 #xc0 #x0f #x84 #x9c #x07 #x00 #x00 #x41 #x8d #x44 #x24 #xfd
      #x4c #x89 #xe5 #xa8 #x07 #x74 #x17 #xe9 #x5b #x07 #x00 #x00 #x0f
      #x1f #x00 #x48 #x83 #xbb #x70 #x01 #x00 #x00 #x00 #x74 #x15 #xe8
      #x79 #xfa #xff #xff #x48 #x8d #x45 #xfd #x48 #x8b #x68 #x08 #x8d
      #x55 #xfd #x83 #xe2 #x07 #x74 #xe1 #x4c #x8b #x20 #xe8 #x61 #xfa
      #xff #xff #xeb #x9f #x31 #xf6 #x48 #x89 #xc7 #x41 #xff #x57 #x38
      #x84 #xc0 #x0f #x84 #x40 #xfc #xff #xff #x0f #x1f #x00 #x66 #x66
      #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x48 #x83 #xbb #x70
      #x01 #x00 #x00 #x00 #x0f #x84 #x24 #xfc #xff #xff #x4c #x8b #x6c
      #x24 #x40 #xe9 #x1d #xfc #xff #xff #x0f #x1f #x84 #x00 #x00 #x00
      #x00 #x00 #x48 #x85 #xf6 #x74 #x0c #x49 #x8b #x06 #x48 #x8b #xbb
      #x80 #x01 #x00 #x00 #xff #x10 #x48 #xc7 #x44 #x24 #x20 #x00 #x00
      #x00 #x00 #xe9 #x58 #xfb #xff #xff #x48 #x85 #xc0 #x74 #x3e #xf3
      #x0f #x7e #x83 #xf0 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #xa0 #x01
      #x00 #x00 #xbf #x04 #x00 #x00 #x00 #xf3 #x0f #x7e #x8b #xe8 #x00
      #x00 #x00 #x0f #x16 #x44 #x24 #x08 #x0f #x16 #x4c #x24 #x08 #x0f
      #x29 #x8c #x24 #xa0 #x01 #x00 #x00 #x0f #x29 #x84 #x24 #xb0 #x01
      #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00 #x48 #x8b #x7b #x50
      #x48 #x89 #xee #x41 #xff #x97 #xf8 #x22 #x00 #x00 #x48 #x8b #x7b
      #x30 #x48 #x89 #xc5 #x41 #xff #x97 #xb8 #x29 #x00 #x00 #x48 #x85
      #xc0 #x74 #x29 #xf3 #x0f #x7e #x83 #xe0 #x00 #x00 #x00 #x48 #x8d
      #xb4 #x24 #xd0 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00 #x0f #x16
      #x44 #x24 #x40 #x0f #x29 #x84 #x24 #xd0 #x00 #x00 #x00 #x41 #xff
      #x97 #x88 #x1d #x00 #x00 #xf3 #x0f #x7e #x43 #x48 #x66 #x48 #x0f
      #x6e #xe5 #x48 #x8b #x13 #x48 #x89 #x84 #x24 #x30 #x02 #x00 #x00
      #x48 #x8b #x44 #x24 #x18 #x48 #x8d #xb4 #x24 #x10 #x02 #x00 #x00
      #xbf #x06 #x00 #x00 #x00 #x48 #xc7 #x84 #x24 #x20 #x02 #x00 #x00
      #x00 #x00 #x00 #x00 #x66 #x0f #x6c #xc4 #x48 #x89 #x94 #x24 #x28
      #x02 #x00 #x00 #x0f #x29 #x84 #x24 #x10 #x02 #x00 #x00 #x48 #x89
      #x84 #x24 #x38 #x02 #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00
      #x48 #x89 #xc5 #x48 #x89 #xc7 #x41 #xff #x97 #x50 #x2a #x00 #x00
      #x48 #x8b #x73 #x58 #x48 #x39 #xf0 #x0f #x84 #xe4 #x03 #x00 #x00
      #x48 #x8b #x15 nil nil nil nil #x48 #x8b #x12 #x80 #x3a #x00 #x0f
      #x85 #xc1 #x03 #x00 #x00 #x66 #x0f #x6f #x83 #xd0 #x00 #x00 #x00
      #x48 #x8d #xb4 #x24 #xc0 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00
      #x0f #x29 #x84 #x24 #xc0 #x00 #x00 #x00 #x41 #xff #x97 #x88 #x1d
      #x00 #x00 #x48 #x8b #x7b #x30 #x41 #xff #x97 #xb8 #x29 #x00 #x00
      #x48 #x85 #xc0 #x74 #x2e #xf3 #x0f #x7e #x83 #xc8 #x00 #x00 #x00
      #x48 #x8d #xb4 #x24 #xb0 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00
      #x0f #x16 #x44 #x24 #x20 #x0f #x29 #x84 #x24 #xb0 #x00 #x00 #x00
      #x41 #xff #x97 #x88 #x1d #x00 #x00 #x48 #x89 #x44 #x24 #x30 #x44
      #x8d #x65 #xfd #x41 #x83 #xe4 #x07 #x0f #x85 #x79 #x03 #x00 #x00
      #x48 #x8b #x75 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #x4a #x04
      #x00 #x00 #x4c #x8b #x6e #xfd #x48 #x8b #x7b #x70 #x41 #xff #x97
      #xb8 #x29 #x00 #x00 #x4c #x89 #xef #x48 #x89 #xc6 #x41 #xff #x97
      #x10 #x26 #x00 #x00 #x48 #x89 #xc6 #x48 #x85 #xc0 #x0f #x84 #x82
      #x02 #x00 #x00 #x8d #x40 #xfd #xa8 #x07 #x0f #x85 #xaf #x05 #x00
      #x00 #x4c #x8b #x6e #xfd #x45 #x85 #xe4 #x0f #x85 #x72 #x03 #x00
      #x00 #x48 #x8b #x75 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xe3
      #x03 #x00 #x00 #x48 #x8b #x46 #x05 #x48 #x89 #x44 #x24 #x18 #x48
      #x8b #x7c #x24 #x10 #x48 #x89 #xf8 #x48 #x0b #x44 #x24 #x28 #x0f
      #x84 #x4f #x06 #x00 #x00 #x31 #xf6 #x41 #xff #x97 #xf8 #x22 #x00
      #x00 #x48 #x83 #x7c #x24 #x38 #x00 #x48 #x89 #x44 #x24 #x10 #x0f
      #x84 #x6d #x02 #x00 #x00 #x8b #x44 #x24 #x28 #x83 #xe8 #x03 #xa8
      #x07 #x0f #x85 #xce #x05 #x00 #x00 #x48 #x8b #x44 #x24 #x28 #x48
      #x8b #x70 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xee #x05 #x00
      #x00 #x48 #x8b #x46 #xfd #x48 #x89 #x84 #x24 #xa0 #x00 #x00 #x00
      #x48 #x8b #x44 #x24 #x38 #xbf #x02 #x00 #x00 #x00 #x48 #x8d #xb4
      #x24 #xa0 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #xa8 #x00 #x00 #x00
      #x41 #xff #x97 #xe8 #x22 #x00 #x00 #x31 #xf6 #x48 #x89 #xc7 #x41
      #xff #x97 #xf8 #x22 #x00 #x00 #x48 #x89 #x44 #x24 #x08 #x48 #x8b
      #x44 #x24 #x18 #x48 #x8d #xb4 #x24 #x00 #x01 #x00 #x00 #xbf #x03
      #x00 #x00 #x00 #x48 #x89 #x84 #x24 #x00 #x01 #x00 #x00 #x48 #x8b
      #x44 #x24 #x10 #x48 #x89 #x84 #x24 #x08 #x01 #x00 #x00 #x48 #x8b
      #x44 #x24 #x08 #x48 #x89 #x84 #x24 #x10 #x01 #x00 #x00 #x41 #xff
      #x97 #xa0 #x26 #x00 #x00 #x48 #x8b #x53 #x68 #x48 #x8b #x4c #x24
      #x30 #x4c #x89 #xac #x24 #x90 #x01 #x00 #x00 #xbf #x04 #x00 #x00
      #x00 #x48 #x89 #x84 #x24 #x98 #x01 #x00 #x00 #x48 #x8d #xb4 #x24
      #x80 #x01 #x00 #x00 #x48 #x89 #x94 #x24 #x80 #x01 #x00 #x00 #x48
      #x89 #x8c #x24 #x88 #x01 #x00 #x00 #x41 #xff #x97 #x90 #x1d #x00
      #x00 #x48 #x8b #x7b #x78 #x49 #x89 #xc5 #x41 #xff #x97 #xb8 #x29
      #x00 #x00 #x48 #x85 #xc0 #x74 #x7f #x45 #x85 #xe4 #x0f #x85 #xa8
      #x04 #x00 #x00 #x48 #x8b #x75 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f
      #x85 #x19 #x05 #x00 #x00 #x48 #x8b #x6e #xfd #x48 #x8b #xbb #x90
      #x00 #x00 #x00 #x41 #xff #x97 #xb8 #x29 #x00 #x00 #x31 #xd2 #x48
      #x89 #xef #x48 #x89 #xc6 #x41 #xff #x97 #x78 #x27 #x00 #x00 #x48
      #x89 #xc7 #x48 #x89 #xc5 #x41 #xff #x97 #x80 #x2b #x00 #x00 #x48
      #x8b #xbb #xa0 #x00 #x00 #x00 #x49 #x89 #xc4 #x41 #xff #x97 #xb8
      #x29 #x00 #x00 #x4c #x89 #xe7 #x48 #x89 #xc6 #x41 #xff #x97 #x08
      #x26 #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x23 #x05 #x00 #x00 #x4c
      #x89 #xea #xbe #x06 #x00 #x00 #x00 #x48 #x89 #xef #x41 #xff #x97
      #x58 #x29 #x00 #x00 #xbf #x06 #x00 #x00 #x00 #x41 #xff #x57 #x20
      #xe9 #x3b #xf7 #xff #xff #x0f #x1f #x40 #x00 #x49 #x8b #x06 #x4c
      #x89 #xe6 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x4d #x89 #xe5 #xff
      #x10 #x48 #x8b #xbb #x38 #x01 #x00 #x00 #x41 #xff #x97 #xb8 #x29
      #x00 #x00 #x31 #xff #x48 #x89 #xc6 #x41 #xff #x97 #xf8 #x25 #x00
      #x00 #x48 #x85 #xc0 #x74 #x2f #xf3 #x0f #x7e #x83 #x40 #x01 #x00
      #x00 #x48 #x8b #x03 #xbf #x03 #x00 #x00 #x00 #x48 #x8d #xb4 #x24
      #x40 #x01 #x00 #x00 #x48 #x89 #x84 #x24 #x50 #x01 #x00 #x00 #x0f
      #x29 #x84 #x24 #x40 #x01 #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00
      #x00 #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x4c #x89
      #xee #xff #x10 #xe8 #xfc #xf5 #xff #xff #xe9 #xa7 #xf9 #xff #xff
      #x0f #x1f #x80 #x00 #x00 #x00 #x00 #x48 #x85 #xed #x74 #x0f #x49
      #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x48 #x89 #xee #xff
      #x10 #x48 #xc7 #x44 #x24 #x10 #x00 #x00 #x00 #x00 #xe9 #x5e #xf8
      #xff #xff #x66 #x0f #x1f #x44 #x00 #x00 #x48 #x8b #x7b #x70 #x41
      #xff #x97 #xb8 #x29 #x00 #x00 #x4c #x89 #xef #x48 #x89 #xc6 #x41
      #xff #x97 #xf8 #x22 #x00 #x00 #x48 #x8b #x7b #x70 #x31 #xc9 #x31
      #xd2 #x48 #x89 #xc6 #x41 #xff #x57 #x50 #xe9 #x61 #xfd #xff #xff
      #x0f #x1f #x40 #x00 #x48 #xc7 #x44 #x24 #x28 #x00 #x00 #x00 #x00
      #xe9 #xcc #xfb #xff #xff #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00
      #x00 #x00 #x48 #x83 #x7c #x24 #x08 #x00 #x0f #x84 #xe8 #xfd #xff
      #xff #x8b #x44 #x24 #x28 #x83 #xe8 #x03 #xa8 #x07 #x0f #x85 #x27
      #x04 #x00 #x00 #x48 #x8b #x44 #x24 #x28 #x48 #x8b #x70 #x05 #x8d
      #x46 #xfd #xa8 #x07 #x0f #x85 #x03 #x02 #x00 #x00 #x48 #x8b #x46
      #xfd #x48 #x89 #x44 #x24 #x38 #x48 #x8b #x7c #x24 #x38 #x31 #xf6
      #x41 #xff #x97 #xf8 #x22 #x00 #x00 #x48 #x89 #x44 #x24 #x08 #xe9
      #xa4 #xfd #xff #xff #x48 #x89 #xc7 #x41 #xff #x57 #x38 #x84 #xc0
      #x0f #x84 #x30 #xfc #xff #xff #x90 #x48 #x83 #xbb #x70 #x01 #x00
      #x00 #x00 #x0f #x85 #x45 #xfc #xff #xff #xe9 #x1c #xfc #xff #xff
      #x0f #x1f #x44 #x00 #x00 #x48 #x85 #xed #x74 #x0f #x49 #x8b #x06
      #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x48 #x89 #xee #xff #x10 #x45
      #x31 #xed #xe9 #x7e #xfc #xff #xff #x0f #x1f #x40 #x00 #x48 #x85
      #xed #x74 #x0f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00
      #x48 #x89 #xee #xff #x10 #x48 #xc7 #x44 #x24 #x10 #x00 #x00 #x00
      #x00 #x31 #xed #xe9 #x54 #xf7 #xff #xff #x0f #x1f #x40 #x00 #x48
      #x85 #xed #x74 #x0f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00
      #x00 #x48 #x89 #xee #xff #x10 #x48 #xc7 #x44 #x24 #x18 #x00 #x00
      #x00 #x00 #xe9 #x84 #xfc #xff #xff #x66 #x0f #x1f #x44 #x00 #x00
      #x48 #x85 #xed #x0f #x84 #xf6 #x02 #x00 #x00 #x49 #x8b #x06 #x48
      #x8b #xbb #x80 #x01 #x00 #x00 #x48 #x89 #xee #xff #x10 #x48 #x8b
      #x43 #x10 #x48 #x8d #x74 #x24 #x50 #xbf #x02 #x00 #x00 #x00 #x48
      #xc7 #x44 #x24 #x58 #x00 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x50
      #x41 #xff #x97 #x88 #x1d #x00 #x00 #x49 #x8b #x06 #x48 #x89 #xee
      #x48 #x8b #xbb #x80 #x01 #x00 #x00 #xff #x10 #xe9 #x3a #xfa #xff
      #xff #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x48 #x85 #xf6
      #x74 #x8f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #xff
      #x10 #xeb #x81 #x0f #x1f #x44 #x00 #x00 #x48 #x85 #xf6 #x0f #x84
      #x2b #xff #xff #xff #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00
      #x00 #xff #x10 #xe9 #x1a #xff #xff #xff #x66 #x0f #x1f #x44 #x00
      #x00 #x48 #x85 #xf6 #x0f #x84 #x2b #xff #xff #xff #x49 #x8b #x06
      #x48 #x8b #xbb #x80 #x01 #x00 #x00 #xff #x10 #xe9 #x1a #xff #xff
      #xff #x48 #x85 #xf6 #x74 #x0c #x49 #x8b #x06 #x48 #x8b #xbb #x80
      #x01 #x00 #x00 #xff #x10 #x66 #x0f #xef #xc9 #xe9 #xf4 #xf4 #xff
      #xff #xf3 #x0f #x7e #x83 #x18 #x01 #x00 #x00 #x48 #x8d #xb4 #x24
      #xc0 #x01 #x00 #x00 #xbf #x04 #x00 #x00 #x00 #xf3 #x0f #x7e #x8b
      #xe8 #x00 #x00 #x00 #x0f #x16 #x44 #x24 #x08 #x0f #x16 #x4c #x24
      #x08 #x0f #x29 #x8c #x24 #xc0 #x01 #x00 #x00 #x0f #x29 #x84 #x24
      #xd0 #x01 #x00 #x00 #x41 #xff #x97 #x88 #x1d #x00 #x00 #xe9 #x68
      #xf8 #xff #xff #x48 #x85 #xf6 #x74 #x2d #x49 #x8b #x06 #x48 #x8b
      #xbb #x80 #x01 #x00 #x00 #xff #x10 #x49 #x8b #x74 #x24 #x08 #x8d
      #x46 #xfd #xa8 #x07 #x0f #x84 #xf5 #x02 #x00 #x00 #x48 #x85 #xf6
      #x74 #x0c #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #xff
      #x10 #x48 #xc7 #x44 #x24 #x38 #x00 #x00 #x00 #x00 #x45 #x31 #xed
      #xe9 #x45 #xf8 #xff #xff #x48 #x85 #xf6 #x0f #x84 #xfd #xfd #xff
      #xff #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #xff #x10
      #xe9 #xec #xfd #xff #xff #x0f #x1f #x40 #x00 #x4d #x85 #xe4 #x74
      #x1e #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x4c #x89
      #xe6 #xff #x10 #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00
      #x4c #x89 #xe6 #xff #x10 #x45 #x31 #xe4 #xe9 #x9f #xf8 #xff #xff
      #x0f #x1f #x44 #x00 #x00 #x4c #x89 #xe8 #x4d #x89 #xe5 #x48 #x8b
      #x6c #x24 #x28 #x49 #x89 #xc4 #x4c #x89 #xef #x41 #xff #x97 #x50
      #x2a #x00 #x00 #x48 #x8b #xb3 #x10 #x01 #x00 #x00 #x48 #x39 #xf0
      #x0f #x84 #xbf #x01 #x00 #x00 #x48 #x8b #x15 nil nil nil nil #x48
      #x8b #x12 #x80 #x3a #x00 #x0f #x85 #x9d #x01 #x00 #x00 #x4d #x8b
      #x24 #x24 #x48 #x8b #x7c #x24 #x48 #x31 #xf6 #x41 #xff #x97 #xf8
      #x22 #x00 #x00 #x48 #x89 #xc6 #x4c #x89 #xe7 #x41 #xff #x97 #xf8
      #x22 #x00 #x00 #x48 #x89 #x44 #x24 #x08 #x48 #x89 #x44 #x24 #x28
      #xe9 #xd9 #xf8 #xff #xff #x0f #x1f #x80 #x00 #x00 #x00 #x00 #x49
      #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x45 #x31 #xed #xff
      #x10 #xe9 #x41 #xfa #xff #xff #x0f #x1f #x40 #x00 #x4c #x8b #x6c
      #x24 #x08 #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x4c
      #x89 #xee #xff #x10 #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00
      #x00 #x4c #x89 #xee #xff #x10 #xe9 #x92 #xf8 #xff #xff #x48 #x85
      #xed #x74 #x0f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00
      #x48 #x89 #xee #xff #x10 #x31 #xed #xe9 #x50 #xfb #xff #xff #x0f
      #x1f #x44 #x00 #x00 #x48 #x85 #xed #x74 #x0f #x49 #x8b #x06 #x48
      #x8b #xbb #x80 #x01 #x00 #x00 #x48 #x89 #xee #xff #x10 #x31 #xf6
      #xe9 #x69 #xf6 #xff #xff #x0f #x1f #x44 #x00 #x00 #x48 #x8b #x74
      #x24 #x08 #x48 #x85 #xf6 #x74 #x0c #x49 #x8b #x06 #x48 #x8b #xbb
      #x80 #x01 #x00 #x00 #xff #x10 #x31 #xc0 #xe9 #x2d #xfa #xff #xff
      #x48 #x83 #xbb #x70 #x01 #x00 #x00 #x00 #x0f #x84 #x4c #xf6 #xff
      #xff #x48 #x8b #x68 #x08 #xe9 #x43 #xf6 #xff #xff #x48 #x85 #xf6
      #x75 #xd1 #x31 #xc0 #xe9 #x0a #xfa #xff #xff #x48 #x85 #xf6 #x74
      #x8f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x31 #xed
      #xff #x10 #xe9 #xd3 #xfa #xff #xff #x48 #xc7 #x44 #x24 #x10 #x00
      #x00 #x00 #x00 #x48 #x83 #x7c #x24 #x38 #x00 #x75 #xad #x48 #xc7
      #x44 #x24 #x08 #x00 #x00 #x00 #x00 #xe9 #x0d #xfa #xff #xff #x48
      #x8b #x43 #x10 #x48 #x8d #x74 #x24 #x50 #xbf #x02 #x00 #x00 #x00
      #x48 #xc7 #x44 #x24 #x58 #x00 #x00 #x00 #x00 #x48 #x89 #x44 #x24
      #x50 #x41 #xff #x97 #x88 #x1d #x00 #x00 #xe9 #x62 #xf7 #xff #xff
      #x31 #xf6 #x48 #x89 #xef #x41 #xff #x97 #xf8 #x22 #x00 #x00 #x48
      #x8b #xbb #xb8 #x00 #x00 #x00 #x48 #x89 #xc6 #x41 #xff #x97 #xf8
      #x22 #x00 #x00 #x48 #x8b #xbb #xb0 #x00 #x00 #x00 #x48 #x89 #xc6
      #x41 #xff #x97 #xb8 #x1d #x00 #x00 #xe9 #x02 #xf2 #xff #xff #x49
      #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00 #x48 #x8b #x74 #x24
      #x08 #xff #x10 #xe9 #xe0 #xfb #xff #xff #x48 #x89 #xc7 #x41 #xff
      #x57 #x38 #x84 #xc0 #x0f #x84 #x54 #xfe #xff #xff #x48 #x83 #xbb
      #x70 #x01 #x00 #x00 #x00 #x0f #x84 #x46 #xfe #xff #xff #x48 #x8b
      #x7b #x30 #x41 #xff #x97 #xb8 #x29 #x00 #x00 #x48 #x85 #xc0 #x0f
      #x85 #x32 #xfe #xff #xff #xf3 #x0f #x7e #x43 #x18 #x48 #x8d #xb4
      #x24 #xf0 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00 #x0f #x16 #x44
      #x24 #x08 #x0f #x29 #x84 #x24 #xf0 #x00 #x00 #x00 #x41 #xff #x97
      #x88 #x1d #x00 #x00 #x48 #x89 #x44 #x24 #x08 #x48 #x89 #x44 #x24
      #x28 #xe9 #x04 #xf7 #xff #xff #x48 #x8b #x7c #x24 #x08 #x41 #xff
      #x57 #x38 #x84 #xc0 #x0f #x84 #x18 #xf5 #xff #xff #x48 #x83 #xbb
      #x70 #x01 #x00 #x00 #x00 #x0f #x84 #x0a #xf5 #xff #xff #x48 #x8d
      #x45 #xfd #x45 #x85 #xe4 #x0f #x84 #xb1 #xfe #xff #xff #x48 #x85
      #xed #x74 #x0f #x49 #x8b #x06 #x48 #x8b #xbb #x80 #x01 #x00 #x00
      #x48 #x89 #xee #xff #x10 #x31 #xed #xe9 #xe2 #xf4 #xff #xff #x31
      #xc0 #x48 #x83 #xee #x03 #x48 #x89 #x44 #x24 #x38 #xe9 #x5f #xf5
      #xff #xff]
         :gots '((23 . freloc) (45 . d-reloc) (60 . symbols-with-pos) (402 . symbols-with-pos) (1031 . symbols-with-pos) (1711 . symbols-with-pos) (3291 . symbols-with-pos))
         :imports '(1354 945 7 1335 1236 12 1376 1215 1217 1119 1218 1117 946 1263 1392 1323 4 10 951 13 14)
         :data '(0 1 2 3 4 6 7 8 9 10 11 13 14 15 18 20 22 23 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 46 48)
         :helper
         (list :back 80
               :template
               [#x8b #x05 nil nil nil nil #x83 #xc0 #x01 #x89 #xc2 #xc1 #xea #x09
      #x75 #x10 #x89 #x05 nil nil nil nil #xc3 #x66 #x0f #x1f #x84 #x00
      #x00 #x00 #x00 #x00 #x53 #x48 #x8b #x1d nil nil nil nil #xc7 #x05
      nil nil nil nil #x00 #x00 #x00 #x00 #x48 #x8b #x03 #xff #x50 #x68
      #x48 #x8b #x03 #x5b #x48 #x8b #x40 #x70 #xff #xe0 #x0f #x1f #x00
      #x66 #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00]
               :gots '((2 . counter) (18 . counter) (36 . freloc) (42 . counter4)))))
  "Exact multi-import shape for vendor bytecomp.el `byte-compile-lambda'
\(3909 bytes, S6.9); see `nelisp-eln-native-subr--multi-import-specs-lambda'.
:HELPER is GNU's local `maybe_gc_quit' (0x50 bytes including its trailing
alignment padding) that sits exactly :BACK bytes before the body: it bumps
the module-local `quitcounter' and, every 512th call, calls slots 13
`maybe_gc' (through the freloc table) and tail-jumps to slot 14
`maybe_quit'.  The body reaches it by direct `call' at fixed
displacements (fixed template bytes).")

(unless (assq 'lambda-form nelisp-eln-tail-code--multi-import-shapes)
  (setq nelisp-eln-tail-code--multi-import-shapes
        (append nelisp-eln-tail-code--multi-import-shapes
                nelisp-eln-tail-code--multi-import-shapes-lambda)))

;; S6.11 (`byte-compile-make-closure'): vendor bytecomp.el's body and the three
;; native anonymous lambdas GNU emits next to it.  Kept in its own constant,
;; appended to the shape list, so concurrent lanes adding shapes do not touch
;; the same source lines.
(defconst nelisp-eln-tail-code--multi-import-shapes-closure
  (list
   (list 'lambda-intern-format
         :template
         [#x53 #x66 #x48 #x0f #x6e #xcf #xbf #x02 #x00 #x00 #x00 #x48 #x83
      #xec #x10 #x48 #x8b #x05 nil nil nil nil #x48 #x89 #xe6 #x48 #x8b
      #x18 #x48 #x8b #x05 nil nil nil nil #xf3 #x0f #x7e #x40 #x10 #x66
      #x0f #x6c #xc1 #x0f #x29 #x04 #x24 #xff #x93 #x00 #x16 #x00 #x00
      #x31 #xf6 #x48 #x89 #xc7 #xff #x93 #x70 #x1f #x00 #x00 #x48 #x83
      #xc4 #x10 #x5b #xc3]
         :gots '((18 . freloc) (31 . d-reloc))
         :imports '(704 1006)
         :data '(2))
   (list 'lambda-aref-form
         :template
         [#x48 #x8b #x05 nil nil nil nil #x48 #x89 #xfe #xbf #x02 #x00 #x00
      #x00 #x48 #x8b #x00 #x48 #x8b #x80 #x60 #x29 #x00 #x00 #xff #xe0]
         :gots '((3 . freloc))
         :imports '(1324)
         :data '())
   (list 'lambda-cons-form
         :template
         [#x48 #x8b #x05 nil nil nil nil #x53 #x31 #xf6 #x48 #x8b #x18 #xff
      #x93 #xf8 #x22 #x00 #x00 #x48 #x89 #xc6 #x48 #x8b #x05 nil nil nil
      nil #x48 #x8b #x78 #x20 #x48 #x8b #x83 #xf8 #x22 #x00 #x00 #x5b
      #xff #xe0]
         :gots '((3 . freloc) (25 . d-reloc))
         :imports '(1119)
         :data '(4))
   (list 'make-closure-form
         :template
         [#x41 #x57 #x41 #x56 #x41 #x55 #x41 #x54 #x49 #x89 #xfc #x55 #x53
      #x48 #x81 #xec #x88 #x01 #x00 #x00 #x4c #x8b #x35 nil nil nil nil
      #x48 #x8b #x2d nil nil nil nil #x49 #x8b #x1e #x48 #x8b #x7d #x30
      #xff #x93 #xb8 #x29 #x00 #x00 #x48 #x85 #xc0 #x74 #x24 #x48 #x8b
      #x7d #x30 #x31 #xc9 #x31 #xd2 #x31 #xf6 #xff #x53 #x50 #x31 #xc0
      #x48 #x81 #xc4 #x88 #x01 #x00 #x00 #x5b #x5d #x41 #x5c #x41 #x5d
      #x41 #x5e #x41 #x5f #xc3 #x0f #x1f #x00 #x49 #x89 #xc5 #x41 #x8d
      #x44 #x24 #xfd #xa8 #x07 #x0f #x85 #x88 #x05 #x00 #x00 #x49 #x8b
      #x74 #x24 #x05 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xf8 #x05 #x00
      #x00 #x48 #x8b #x46 #xfd #x48 #x89 #x44 #x24 #x10 #x4c #x89 #xe6
      #xbf #x0a #x00 #x00 #x00 #xff #x93 #x20 #x26 #x00 #x00 #x4c #x89
      #xe6 #xbf #x0e #x00 #x00 #x00 #x49 #x89 #xc7 #xff #x93 #x20 #x26
      #x00 #x00 #x4c #x89 #xe6 #xbf #x12 #x00 #x00 #x00 #x48 #x89 #x44
      #x24 #x08 #xff #x93 #x28 #x26 #x00 #x00 #x48 #x8b #x7c #x24 #x10
      #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x8b #x7d #x58
      #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x4c #x89 #xff #x49
      #x89 #xc4 #xff #x93 #x10 #x27 #x00 #x00 #x66 #x49 #x0f #x6e #xd4
      #xbf #x03 #x00 #x00 #x00 #xf3 #x0f #x7e #x45 #x50 #x48 #x8d #xb4
      #x24 #xd0 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #xe0 #x00 #x00 #x00
      #x66 #x0f #x6c #xc2 #x0f #x29 #x84 #x24 #xd0 #x00 #x00 #x00 #xff
      #x93 #x88 #x1d #x00 #x00 #x4c #x89 #xff #x49 #x89 #xc4 #xff #x93
      #x10 #x27 #x00 #x00 #x48 #x8d #x74 #x24 #x30 #xbf #x02 #x00 #x00
      #x00 #x48 #xc7 #x44 #x24 #x38 #x02 #x00 #x00 #x00 #x48 #x89 #x44
      #x24 #x30 #xff #x93 #x30 #x29 #x00 #x00 #x48 #x0b #x44 #x24 #x08
      #x0f #x84 #x00 #x05 #x00 #x00 #x4c #x89 #xe7 #xff #x93 #xa0 #x2a
      #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x6e #x04 #x00 #x00 #xf3 #x0f
      #x7e #x45 #x70 #x48 #x8d #x74 #x24 #x40 #xbf #x02 #x00 #x00 #x00
      #x0f #x16 #x44 #x24 #x08 #x0f #x29 #x44 #x24 #x40 #xff #x93 #x88
      #x1d #x00 #x00 #x48 #x85 #xc0 #x0f #x84 #x06 #x03 #x00 #x00 #x4c
      #x89 #xff #xff #x93 #x10 #x27 #x00 #x00 #x48 #x89 #xc7 #x8d #x40
      #xfe #xa8 #x03 #x0f #x84 #xb7 #x02 #x00 #x00 #x49 #x8b #x06 #xff
      #x90 #xa0 #x28 #x00 #x00 #x48 #x8b #x95 #xc8 #x00 #x00 #x00 #x48
      #x8d #xb4 #x24 #xf0 #x00 #x00 #x00 #xbf #x03 #x00 #x00 #x00 #x48
      #xc7 #x84 #x24 #xf8 #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x48 #x89
      #x84 #x24 #x00 #x01 #x00 #x00 #x48 #x89 #x94 #x24 #xf0 #x00 #x00
      #x00 #xff #x93 #x88 #x1d #x00 #x00 #x48 #x8b #xbd #xc0 #x00 #x00
      #x00 #x48 #x89 #xc6 #xff #x93 #x58 #x25 #x00 #x00 #x66 #x49 #x0f
      #x6e #xdc #xbf #x02 #x00 #x00 #x00 #xf3 #x0f #x7e #x85 #xd8 #x00
      #x00 #x00 #x48 #x8d #xb4 #x24 #x80 #x00 #x00 #x00 #x48 #x89 #x44
      #x24 #x10 #x66 #x0f #x6c #xc3 #x0f #x29 #x84 #x24 #x80 #x00 #x00
      #x00 #xff #x93 #xc8 #x22 #x00 #x00 #x4c #x89 #xe7 #x48 #x89 #x44
      #x24 #x18 #xff #x93 #x10 #x27 #x00 #x00 #x48 #x89 #xc7 #x8d #x40
      #xfe #xa8 #x03 #x0f #x85 #x43 #x02 #x00 #x00 #x48 #xba #x00 #x00
      #x00 #x00 #x00 #x00 #x00 #xe0 #x48 #x89 #xf8 #x48 #xc1 #xf8 #x02
      #x48 #x39 #xd0 #x0f #x84 #x29 #x02 #x00 #x00 #x48 #x8d #x04 #x85
      #xfe #xff #xff #xff #x48 #x8b #x95 #xc8 #x00 #x00 #x00 #x48 #x8d
      #xb4 #x24 #x10 #x01 #x00 #x00 #xbf #x03 #x00 #x00 #x00 #x48 #xc7
      #x84 #x24 #x18 #x01 #x00 #x00 #x12 #x00 #x00 #x00 #x48 #x89 #x84
      #x24 #x20 #x01 #x00 #x00 #x48 #x89 #x94 #x24 #x10 #x01 #x00 #x00
      #xff #x93 #x88 #x1d #x00 #x00 #x48 #x8b #x7c #x24 #x18 #x48 #x89
      #xc6 #xff #x93 #x58 #x25 #x00 #x00 #x4c #x89 #xe7 #xbe #x02 #x00
      #x00 #x00 #x48 #x89 #x44 #x24 #x28 #xff #x93 #x60 #x29 #x00 #x00
      #x4c #x89 #xe7 #xbe #x06 #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x18
      #xff #x93 #x60 #x29 #x00 #x00 #x4c #x89 #xe7 #xbe #x0a #x00 #x00
      #x00 #x48 #x89 #x44 #x24 #x20 #xff #x93 #x60 #x29 #x00 #x00 #x48
      #x8b #x4c #x24 #x10 #xbf #x02 #x00 #x00 #x00 #x48 #x8d #xb4 #x24
      #x90 #x00 #x00 #x00 #x48 #x89 #x84 #x24 #x98 #x00 #x00 #x00 #x48
      #x89 #x8c #x24 #x90 #x00 #x00 #x00 #xff #x93 #x90 #x26 #x00 #x00
      #x4c #x89 #xe7 #xbe #x0e #x00 #x00 #x00 #x48 #x89 #x44 #x24 #x10
      #xff #x93 #x60 #x29 #x00 #x00 #x48 #x83 #x7c #x24 #x08 #x00 #x48
      #x8b #x54 #x24 #x28 #x49 #x89 #xc4 #x0f #x84 #x8c #x00 #x00 #x00
      #xf3 #x0f #x7e #x85 #x98 #x00 #x00 #x00 #x48 #x89 #x54 #x24 #x28
      #xbf #x02 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #xa0 #x00 #x00 #x00
      #x0f #x16 #x44 #x24 #x08 #x0f #x29 #x84 #x24 #xa0 #x00 #x00 #x00
      #xff #x93 #x88 #x1d #x00 #x00 #x48 #x8d #xb4 #x24 #x30 #x01 #x00
      #x00 #xbf #x03 #x00 #x00 #x00 #xf3 #x0f #x7e #x85 #xe8 #x00 #x00
      #x00 #x66 #x48 #x0f #x6e #xe0 #x48 #x8b #x85 #xf0 #x00 #x00 #x00
      #x66 #x0f #x6c #xc4 #x48 #x89 #x84 #x24 #x40 #x01 #x00 #x00 #x0f
      #x29 #x84 #x24 #x30 #x01 #x00 #x00 #xff #x93 #x88 #x1d #x00 #x00
      #x48 #x8b #x54 #x24 #x28 #x8d #x4a #xfd #x83 #xe1 #x07 #x0f #x85
      #x84 #x02 #x00 #x00 #x4c #x8b #x6a #x05 #x4c #x89 #xee #x48 #x89
      #xc7 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x89 #xc2 #x48 #x8b #x45
      #x78 #x48 #x89 #x94 #x24 #x78 #x01 #x00 #x00 #x48 #x8d #xb4 #x24
      #x50 #x01 #x00 #x00 #xbf #x06 #x00 #x00 #x00 #x4c #x89 #xa4 #x24
      #x70 #x01 #x00 #x00 #x48 #x89 #x84 #x24 #x50 #x01 #x00 #x00 #x48
      #x8b #x44 #x24 #x18 #x48 #x89 #x84 #x24 #x58 #x01 #x00 #x00 #x48
      #x8b #x44 #x24 #x20 #x48 #x89 #x84 #x24 #x60 #x01 #x00 #x00 #x48
      #x8b #x44 #x24 #x10 #x48 #x89 #x84 #x24 #x68 #x01 #x00 #x00 #xff
      #x93 #x90 #x1d #x00 #x00 #x4c #x89 #xfe #x48 #x89 #xc7 #xff #x93
      #xf8 #x22 #x00 #x00 #x48 #x8b #xbd #xd0 #x00 #x00 #x00 #x48 #x89
      #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x66 #x48 #x0f #x6e #xc0 #xf3
      #x0f #x7e #x4d #x68 #x48 #x8d #x74 #x24 #x70 #xbf #x02 #x00 #x00
      #x00 #x66 #x0f #x6c #xc8 #x0f #x29 #x4c #x24 #x70 #xff #x93 #x88
      #x1d #x00 #x00 #xe9 #x02 #xfc #xff #xff #x0f #x1f #x80 #x00 #x00
      #x00 #x00 #x48 #xba #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xe0 #x48
      #x89 #xf8 #x48 #xc1 #xf8 #x02 #x48 #x39 #xd0 #x0f #x84 #x2f #xfd
      #xff #xff #x48 #x8d #x04 #x85 #xfe #xff #xff #xff #xe9 #x2b #xfd
      #xff #xff #x90 #x49 #x8b #x06 #xff #x90 #xa0 #x28 #x00 #x00 #xe9
      #xd1 #xfd #xff #xff #x66 #x90 #xbe #x02 #x00 #x00 #x00 #x4c #x89
      #xe7 #xff #x93 #x60 #x29 #x00 #x00 #xbe #x06 #x00 #x00 #x00 #x4c
      #x89 #xe7 #x48 #x89 #x44 #x24 #x10 #xff #x93 #x60 #x29 #x00 #x00
      #x4c #x89 #xfe #x48 #x8b #xbd #x90 #x00 #x00 #x00 #x48 #x89 #x44
      #x24 #x18 #xff #x93 #xf8 #x22 #x00 #x00 #xbe #x0a #x00 #x00 #x00
      #x4c #x89 #xe7 #x49 #x89 #xc5 #xff #x93 #x60 #x29 #x00 #x00 #x31
      #xf6 #x48 #x89 #xc7 #xff #x93 #xf8 #x22 #x00 #x00 #x4c #x89 #xef
      #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x8b #xbd #x88
      #x00 #x00 #x00 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #xbe
      #x0e #x00 #x00 #x00 #x4c #x89 #xe7 #x49 #x89 #xc5 #xff #x93 #x60
      #x29 #x00 #x00 #x48 #x8d #x74 #x24 #x50 #xbf #x02 #x00 #x00 #x00
      #xf3 #x0f #x7e #x85 #x98 #x00 #x00 #x00 #x49 #x89 #xc6 #x0f #x16
      #x44 #x24 #x08 #x0f #x29 #x44 #x24 #x50 #xff #x93 #x88 #x1d #x00
      #x00 #x48 #x8d #x74 #x24 #x60 #xbf #x02 #x00 #x00 #x00 #x4c #x89
      #x64 #x24 #x60 #x48 #xc7 #x44 #x24 #x68 #x00 #x00 #x00 #x00 #x49
      #x89 #xc7 #xff #x93 #xa0 #x26 #x00 #x00 #xbf #x16 #x00 #x00 #x00
      #x48 #x89 #xc6 #xff #x93 #x28 #x26 #x00 #x00 #x48 #x8b #xbd #xa8
      #x00 #x00 #x00 #x48 #x89 #xc6 #xff #x93 #x58 #x25 #x00 #x00 #x4c
      #x89 #xff #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x4c #x89
      #xf7 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x4c #x89 #xef
      #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x8b #x7c #x24
      #x18 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x8b #x7c
      #x24 #x10 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x48 #x8b
      #x7d #x78 #x48 #x89 #xc6 #xff #x93 #xf8 #x22 #x00 #x00 #x66 #x48
      #x0f #x6e #xc0 #xe9 #x64 #xfe #xff #xff #x66 #x0f #x1f #x44 #x00
      #x00 #xf3 #x0f #x6f #x85 #xf8 #x00 #x00 #x00 #x48 #x8d #xb4 #x24
      #xb0 #x00 #x00 #x00 #xbf #x02 #x00 #x00 #x00 #x0f #x29 #x84 #x24
      #xb0 #x00 #x00 #x00 #xff #x93 #x88 #x1d #x00 #x00 #xe9 #x6a #xfb
      #xff #xff #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00 #x4d #x85 #xe4
      #x74 #x0f #x49 #x8b #x06 #x48 #x8b #xbd #x38 #x01 #x00 #x00 #x4c
      #x89 #xe6 #xff #x10 #x48 #xc7 #x44 #x24 #x10 #x00 #x00 #x00 #x00
      #xe9 #x6f #xfa #xff #xff #x66 #x0f #x1f #x44 #x00 #x00 #x48 #x85
      #xd2 #x0f #x84 #x77 #xfd #xff #xff #x49 #x8b #x0e #x48 #x89 #x44
      #x24 #x08 #x48 #x89 #xd6 #x48 #x8b #xbd #x38 #x01 #x00 #x00 #xff
      #x11 #x48 #x8b #x44 #x24 #x08 #xe9 #x59 #xfd #xff #xff #x90 #xf3
      #x0f #x7e #x85 #xf8 #x00 #x00 #x00 #x48 #x8d #xb4 #x24 #xc0 #x00
      #x00 #x00 #xbf #x02 #x00 #x00 #x00 #x0f #x16 #x85 #x08 #x01 #x00
      #x00 #x0f #x29 #x84 #x24 #xc0 #x00 #x00 #x00 #xff #x93 #x88 #x1d
      #x00 #x00 #xe9 #xd1 #xfa #xff #xff #x90 #x48 #x85 #xf6 #x74 #x8f
      #x49 #x8b #x06 #x48 #x8b #xbd #x38 #x01 #x00 #x00 #xff #x10 #xeb
      #x81]
         :gots '((23 . freloc) (30 . d-reloc))
         :imports '(1335 10 1220 1221 1119 1250 945 1318 1364 1300 1195 1113 1324 1234 946 1236 0)
         :data '(6 10 11 13 14 15 17 18 19 21 24 25 26 27 29 30 31 32 33 39)))
  "Exact multi-import shapes of gnu-byte-compile-make-closure.eln (S6.11); see
`nelisp-eln-native-subr--multi-import-specs-closure'.

`lambda-intern-format' (71 bytes) is GNU's `(lambda (i) (intern (format
\"V%d\" i)))' through slots 704 `Fformat' (MANY, 2) and 1006 `Fintern',
`lambda-aref-form' (27 bytes, no d_reloc read) tail-calls slot 1324 `Faref'
with the placeholder fixnum 0 as the array, and `lambda-cons-form' (43 bytes)
is `(cons \='quote (list x))' through two slot 1119 `Fcons' calls (the second
a tail call).  `make-closure-form' (1667 bytes) is vendor bytecomp.el
`byte-compile-make-closure' itself: inline `car'/`cdr' walks of FORM, slot
945 `Ffuncall' of `byte-compile-lambda', `macroexp-const-p', `number-sequence',
`eval', `byte-compile-form' and the assertion failure, three slot 1195
`Fmapcar' calls (two of them over the registered native lambdas read from
d_reloc[24] and d_reloc[21], the middle one over an `Fmake_closure' result),
slot 1234 `Fvconcat', slot 1324 `Faref' of the compiled function's slots,
slot 946 `Fapply' of `make-byte-code' with six arguments and slot 1236
`Fappend', ending in a `Ffuncall' of `byte-compile-form'.  Its inline reads
touch only FORM's own conses and the `Fmapcar' result list (`cdr' of the
optional-argument list); every other port result is only tested for nil,
passed on to a later port, or returned.")

(unless (assq 'make-closure-form nelisp-eln-tail-code--multi-import-shapes)
  (setq nelisp-eln-tail-code--multi-import-shapes
        (append nelisp-eln-tail-code--multi-import-shapes
                nelisp-eln-tail-code--multi-import-shapes-closure)))

;;; S6.14: `byte-compile-funcall' (vendor bytecomp.el).  Kept in its own constant
;;; (appended to the shape list) so concurrent lanes adding shapes do not
;;; touch the same source lines.

(defconst nelisp-eln-tail-code--multi-import-shapes-funcall
  (list
   (list 'funcall-form
         :template
         [#x41 #x55 #x8d #x47 #xfd #x41 #x54 #x55 #x53 #x48 #x83 #xec
      #x48 #x4c #x8b #x2d nil nil nil nil #x49 #x8b #x6d #x00
      #xa8 #x07 #x75 #x64 #x48 #x8b #x77 #x05 #x48 #x8d #x5f #xfd
      #x4c #x8b #x25 nil nil nil nil #x48 #x85 #xf6 #x74 #x67
      #x49 #x8b #x7c #x24 #x20 #xff #x95 #x50 #x25 #x00 #x00 #x48
      #x8b #x73 #x08 #x8d #x46 #xfd #xa8 #x07 #x0f #x85 #xa6 #x00
      #x00 #x00 #x48 #x8b #x7e #x05 #xff #x95 #x10 #x27 #x00 #x00
      #x66 #x41 #x0f #x6f #x44 #x24 #x40 #x48 #x89 #x44 #x24 #x30
      #x48 #x8d #x74 #x24 #x20 #xbf #x03 #x00 #x00 #x00 #x0f #x29
      #x44 #x24 #x20 #xff #x95 #x88 #x1d #x00 #x00 #x48 #x83 #xc4
      #x48 #x5b #x5d #x41 #x5c #x41 #x5d #xc3 #x4c #x8b #x25 nil
      nil nil nil #x48 #x85 #xff #x74 #x0b #x48 #x89 #xfe #x49
      #x8b #x7c #x24 #x70 #xff #x55 #x00 #x49 #x8b #x44 #x24 #x18
      #x48 #x8d #x74 #x24 #x08 #xbf #x01 #x00 #x00 #x00 #x48 #x89
      #x44 #x24 #x08 #xff #x95 #xf8 #x15 #x00 #x00 #x48 #x8d #x74
      #x24 #x10 #xbf #x02 #x00 #x00 #x00 #xf3 #x41 #x0f #x7e #x44
      #x24 #x08 #x66 #x48 #x0f #x6e #xc8 #x66 #x0f #x6c #xc1 #x0f
      #x29 #x44 #x24 #x10 #xff #x95 #x88 #x1d #x00 #x00 #x49 #x8b
      #x7c #x24 #x30 #xff #x95 #xb8 #x29 #x00 #x00 #x66 #x41 #x0f
      #x6f #x44 #x24 #x20 #xe9 #x6e #xff #xff #xff #x0f #x1f #x00
      #x48 #x85 #xf6 #x74 #x0b #x49 #x8b #x45 #x00 #x49 #x8b #x7c
      #x24 #x70 #xff #x10 #x31 #xff #xe9 #x47 #xff #xff #xff]
         :gots '((16 . freloc) (39 . d-reloc) (131 . d-reloc))
         :imports '(1194 1250 945 0 703 1335)
         :data '(1 3 4 5 6 8 9 14)))
  "Exact multi-import shape of gnu-byte-compile-funcall.eln (S6.14); see
`nelisp-eln-native-subr--multi-import-specs-funcall'.

`funcall-form' (263 bytes) is vendor bytecomp.el `byte-compile-funcall'
itself: FORM is tag-checked as a list (a non-list calls slot 0
`wrong_type_argument' with d_reloc[14], `listp'); when (cdr FORM) is
non-nil slot 1194 `Fmapc' runs d_reloc[4] (`byte-compile-form') over it,
slot 1250 `Flength' counts (cdr (cdr FORM)) (a non-list tail is again
rejected through slot 0), and a MANY (3, argv) slot 945 `Ffuncall' calls
d_reloc[8] (`byte-compile-out') on d_reloc[9] (`byte-call') and that
length, whose result is returned.  Otherwise slot 703 `Fformat_message'
\(MANY, 1) formats d_reloc[3] (the string \"`funcall\\=' called with no
arguments\"), a MANY (2, argv) `Ffuncall' of d_reloc[1]
\(`byte-compile-report-error') reports it, and a MANY (3, argv) `Ffuncall'
of d_reloc[4] (`byte-compile-form') on d_reloc[5] (the quoted form
`(signal \\='wrong-number-of-arguments \\='(funcall 0))') and slot 1335
`Fsymbol_value' of d_reloc[6] (`byte-compile--for-effect') gives the
result.  `Ffuncall' is thus called with two argument counts through its one
slot.  d_reloc[0], [2], [7] and [10]-[13] are never read by the body.")

(unless (assq 'funcall-form nelisp-eln-tail-code--multi-import-shapes)
  (setq nelisp-eln-tail-code--multi-import-shapes
        (append nelisp-eln-tail-code--multi-import-shapes
                nelisp-eln-tail-code--multi-import-shapes-funcall)))

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
              ;; Several loads of one kind must all reach one and the same
              ;; GOT slot.
              ('freloc (when (and freloc (/= freloc vaddr)) (throw 'invalid nil))
                       (setq freloc vaddr))
              ('d-reloc (when (and d-reloc (/= d-reloc vaddr)) (throw 'invalid nil))
                        (setq d-reloc vaddr))
              ('symbols-with-pos (when (and swp (/= swp vaddr)) (throw 'invalid nil))
                                 (setq swp vaddr))
              (_ (throw 'invalid nil)))))
        ;; A shape that declares no `d_reloc' constant (S6.11's 27-byte
        ;; `lambda-aref-form') has no d_reloc GOT load at all.
        (unless (and freloc (or d-reloc (null (plist-get (cdr shape) :data))))
          (throw 'invalid nil))
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
              :helper (plist-get (cdr shape) :helper)
              :proof :multi-import-call)))))

;; S6.9: exact local helper regions preceding a body.
(defun nelisp-eln-tail-code-analyze-helper (bytes helper function-vaddr)
  "Verify BYTES as the exact HELPER template placed :BACK bytes before the
body at FUNCTION-VADDR, or return nil.  HELPER is a shape's :HELPER plist
\(:BACK N :TEMPLATE V :GOTS ((OFFSET . KIND) ...)).  Return
\(:VADDR V :SIZE N :COUNTER C :FRELOC F): C is the one module-counter
object every `counter'/`counter4' access reaches (`counter4' marks a
`movl $imm32,disp(%rip)' whose immediate follows the displacement) and F
the one freloc GOT slot its `freloc' load reaches; any disagreement, a
missing kind or a template mismatch returns nil.  As with the body
verifiers, this proves only the instruction bytes; callers must
authenticate the counter object and the GOT slot."
  (catch 'invalid
    (let* ((template (plist-get helper :template))
           (back (plist-get helper :back))
           (vaddr (and (integerp function-vaddr) (integerp back)
                       (- function-vaddr back)))
           (counter nil) (freloc nil))
      (unless (and (stringp bytes) (vectorp template) vaddr (>= vaddr 0)
                   (= (length template) back)
                   (nelisp-eln-tail-code--match-template bytes template))
        (throw 'invalid nil))
      (dolist (got (plist-get helper :gots))
        (let* ((kind (cdr got))
               (target (+ vaddr (car got) 4 (if (eq kind 'counter4) 4 0)
                          (nelisp-eln-tail-code--disp32 bytes (car got)))))
          (when (< target 0) (throw 'invalid nil))
          (pcase (if (eq kind 'counter4) 'counter kind)
            ('counter (when (and counter (/= counter target)) (throw 'invalid nil))
                      (setq counter target))
            ('freloc (when (and freloc (/= freloc target)) (throw 'invalid nil))
                     (setq freloc target))
            (_ (throw 'invalid nil)))))
      (unless (and counter freloc) (throw 'invalid nil))
      (list :vaddr vaddr :size back :counter counter :freloc freloc))))

(provide 'nelisp-eln-tail-code)

;;; nelisp-eln-tail-code.el ends here
