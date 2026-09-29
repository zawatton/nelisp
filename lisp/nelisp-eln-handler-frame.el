;;; nelisp-eln-handler-frame.el --- Doc 210 S10 frame-local handler rule -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 210 section 4.A: NeLisp lands a native `condition-case' only inside
;; the native frame that pushed the handler (the /frame-local/ rule).  A
;; skipped frame could otherwise be a NeLisp interpreter or VM frame.  The
;; exact byte templates of `nelisp-eln-tail-code' already fix every byte of
;; an admitted handler-bearing body; this file is the second, independent
;; line: a fail-closed structural check of the bytes themselves.
;;
;; `nelisp-eln-handler-frame-check' takes the body bytes and the offsets of
;; the rel32 fields of its `call _setjmp@plt' sites (the shape's
;; :PLT-CALLS) and, for each site, proves
;;
;;  1. the push sequence: `mov $1,%esi' (CONDITION_CASE) within the bytes
;;     before an indirect call of the `push_handler' slot through the
;;     link-table register, then `lea 0x40(%rax),%rdi', `call rel32' (the
;;     PLT site itself), `test %eax,%eax' and a rel32 `je';
;;  2. the /guarded region/ (the `je' target, where `_setjmp' returned 0)
;;     decodes with a closed whitelist of instructions, contains no local
;;     `call rel32', no call through a register, no branch, and only
;;     `call *disp32(%reg)' where REG was loaded from the frame's saved
;;     link-table slot inside the region and DISP/8 is one of the region's
;;     authenticated slots; every store is to the frame (%rsp based) except
;;     the single `handlerlist = handlerlist->next' pop, which must read
;;     `current_thread_reloc' through the artifact's own GOT slot; it ends
;;     with the pop and one `jmp rel32' that stays inside the body;
;;  3. the landing pad (where `_setjmp' returned non-zero) decodes with the
;;     same whitelist, contains no call at all, performs the same pop and
;;     ends at its first branch.
;;
;; Anything else -- an unknown opcode, a runaway region, a second pop -- is
;; refused with `nelisp-eln-handler-frame-error'.

;;; Code:

(require 'cl-lib)

(define-error 'nelisp-eln-handler-frame-error
  "Native handler frame rule violated")

(defun nelisp-eln-handler-frame--fail (reason &rest detail)
  (signal 'nelisp-eln-handler-frame-error (cons reason detail)))

(defconst nelisp-eln-handler-frame--max-instructions 64
  "Bound on the instructions of one guarded region or landing pad.")

(defun nelisp-eln-handler-frame--u8 (bytes pos)
  (unless (and (integerp pos) (>= pos 0) (< pos (length bytes)))
    (nelisp-eln-handler-frame--fail 'read-out-of-range pos))
  (aref bytes pos))

(defun nelisp-eln-handler-frame--s32 (bytes pos)
  (let ((value (logior (nelisp-eln-handler-frame--u8 bytes pos)
                       (ash (nelisp-eln-handler-frame--u8 bytes (+ pos 1)) 8)
                       (ash (nelisp-eln-handler-frame--u8 bytes (+ pos 2)) 16)
                       (ash (nelisp-eln-handler-frame--u8 bytes (+ pos 3)) 24))))
    (if (>= value #x80000000) (- value #x100000000) value)))

(defun nelisp-eln-handler-frame--s8 (bytes pos)
  (let ((value (nelisp-eln-handler-frame--u8 bytes pos)))
    (if (>= value #x80) (- value #x100) value)))

(defun nelisp-eln-handler-frame--modrm (bytes pos rex)
  "Decode the ModRM operand at POS.  Return (LENGTH MOD REG RM BASE DISP RIP)
where BASE is the base register number (or nil), DISP the displacement and
RIP non-nil for a RIP-relative operand.  A SIB byte with an index register
is refused."
  (let* ((modrm (nelisp-eln-handler-frame--u8 bytes pos))
         (mod (ash modrm -6))
         (reg (logior (logand (ash modrm -3) 7) (if (/= 0 (logand rex 4)) 8 0)))
         (rm (logand modrm 7))
         (length 1) (base nil) (disp 0) (rip nil))
    (if (= mod 3)
        (setq base nil
              rm (logior rm (if (/= 0 (logand rex 1)) 8 0)))
      (if (= rm 4)
          (let* ((sib (nelisp-eln-handler-frame--u8 bytes (+ pos 1)))
                 (index (logior (logand (ash sib -3) 7)
                                (if (/= 0 (logand rex 2)) 8 0)))
                 (sbase (logand sib 7)))
            (setq length 2)
            (unless (= index 4)
              (nelisp-eln-handler-frame--fail 'indexed-operand pos))
            (when (and (= mod 0) (= sbase 5))
              (nelisp-eln-handler-frame--fail 'sib-no-base pos))
            (setq base (logior sbase (if (/= 0 (logand rex 1)) 8 0))))
        (if (and (= mod 0) (= rm 5))
            (setq rip t)
          (setq base (logior rm (if (/= 0 (logand rex 1)) 8 0)))))
      (cond ((= mod 1)
             (setq disp (nelisp-eln-handler-frame--s8 bytes (+ pos length))
                   length (1+ length)))
            ((or (= mod 2) rip)
             (setq disp (nelisp-eln-handler-frame--s32 bytes (+ pos length))
                   length (+ length 4)))))
    (list length mod reg rm base disp rip)))

(defun nelisp-eln-handler-frame--decode (bytes pos)
  "Decode the whitelisted instruction at POS of BYTES.
Return a plist (:LENGTH N :KIND K ...) where K is one of `jmp', `jcc',
`call-rel32', `call-indirect', `load', `store', `lea', `xmm-load',
`xmm-store', `xmm-to-gpr', `gpr-to-xmm', `test' (see the callers for the
extra keys).  An instruction outside the closed whitelist is refused."
  (let ((i pos) (prefix66 nil) (rex 0))
    (when (= (nelisp-eln-handler-frame--u8 bytes i) #x66)
      (setq prefix66 t i (1+ i)))
    (let ((b (nelisp-eln-handler-frame--u8 bytes i)))
      (when (= (logand b #xf0) #x40)
        (setq rex b i (1+ i))))
    (let ((op (nelisp-eln-handler-frame--u8 bytes i)))
      (cond
       ;; jmp rel32 / call rel32
       ((and (memq op '(#xe9 #xe8)) (not prefix66) (= rex 0))
        (let ((rel (nelisp-eln-handler-frame--s32 bytes (1+ i))))
          (list :length (- (+ i 5) pos)
                :kind (if (= op #xe9) 'jmp 'call-rel32)
                :target (+ i 5 rel))))
       ;; nop
       ((and (= op #x90) (not prefix66) (= rex 0))
        (list :length (- (1+ i) pos) :kind 'nop))
       ;; jcc rel8
       ((and (<= #x70 op #x7f) (not prefix66) (= rex 0))
        (list :length (- (+ i 2) pos) :kind 'jcc
              :target (+ i 2 (nelisp-eln-handler-frame--s8 bytes (1+ i)))))
       ;; two-byte opcodes
       ((= op #x0f)
        (let ((op2 (nelisp-eln-handler-frame--u8 bytes (1+ i))))
          (cond
           ((and (<= #x80 op2 #x8f) (not prefix66) (= rex 0))
            (list :length (- (+ i 6) pos) :kind 'jcc
                  :target (+ i 6 (nelisp-eln-handler-frame--s32 bytes (+ i 2)))))
           ((memq op2 '(#x10 #x28 #x6c #x6e #x7e #x11 #x29 #xd6))
            (let* ((m (nelisp-eln-handler-frame--modrm bytes (+ i 2) rex))
                   (length (- (+ i 2 (nth 0 m)) pos))
                   (memory (/= (nth 1 m) 3)))
              (list :length length
                    :kind (cond
                           ((memq op2 '(#x11 #x29 #xd6))
                            (if memory 'xmm-store 'xmm-move))
                           ((and (= op2 #x7e) memory) 'xmm-store)
                           ((and (= op2 #x7e) (not memory)) 'xmm-to-gpr)
                           ((and (= op2 #x6e) (not memory)) 'gpr-to-xmm)
                           (t (if memory 'xmm-load 'xmm-move)))
                    :reg (nth 2 m) :rm (nth 3 m) :base (nth 4 m)
                    :disp (nth 5 m) :rip (nth 6 m) :mod (nth 1 m))))
           (t (nelisp-eln-handler-frame--fail 'unsupported-instruction
                                              pos (list #x0f op2))))))
       ;; mov r64,r/m64 (8B), mov r/m64,r64 (89), lea (8D), test (85)
       ((and (memq op '(#x8b #x89 #x8d #x85)) (not prefix66))
        (let* ((m (nelisp-eln-handler-frame--modrm bytes (1+ i) rex))
               (length (- (+ i 1 (nth 0 m)) pos))
               (memory (/= (nth 1 m) 3)))
          (list :length length
                :kind (cond ((= op #x8d) 'lea)
                            ((= op #x85) 'test)
                            ((= op #x8b) (if memory 'load 'move))
                            (t (if memory 'store 'move)))
                :op op :reg (nth 2 m) :rm (nth 3 m) :base (nth 4 m)
                :disp (nth 5 m) :rip (nth 6 m) :mod (nth 1 m) :rex rex)))
       ;; call/jmp indirect (FF /2, /4)
       ((and (= op #xff) (not prefix66))
        (let* ((m (nelisp-eln-handler-frame--modrm bytes (1+ i) rex))
               (subop (logand (nth 2 m) 7))
               (length (- (+ i 1 (nth 0 m)) pos)))
          (unless (memq subop '(2 4))
            (nelisp-eln-handler-frame--fail 'unsupported-instruction pos op))
          (list :length length
                :kind (if (= subop 2) 'call-indirect 'jmp-indirect)
                :mod (nth 1 m) :rm (nth 3 m) :base (nth 4 m)
                :disp (nth 5 m) :rip (nth 6 m))))
       (t (nelisp-eln-handler-frame--fail 'unsupported-instruction pos op))))))

(defconst nelisp-eln-handler-frame--rsp 4)

(defun nelisp-eln-handler-frame--bytes-at (bytes pos list)
  "True when BYTES has the byte values LIST at POS."
  (and (>= pos 0) (<= (+ pos (length list)) (length bytes))
       (let ((k 0) (same t))
         (while (and same list)
           (unless (= (car list) (aref bytes (+ pos k)))
             (setq same nil))
           (setq list (cdr list) k (1+ k)))
         same)))

(defun nelisp-eln-handler-frame--drop-reg (alist reg)
  "Return ALIST without the entry for REG."
  (let ((out nil))
    (dolist (entry alist)
      (unless (eq (car entry) reg)
        (setq out (cons entry out))))
    (nreverse out)))

(defconst nelisp-eln-handler-frame--pop-guarded
  '(#x48 #x8b #x4a #x68  #x48 #x8b #x49 #x20  #x48 #x89 #x4a #x68)
  "mov 0x68(%rdx),%rcx; mov 0x20(%rcx),%rcx; mov %rcx,0x68(%rdx).")

(defconst nelisp-eln-handler-frame--pop-pad
  '(#x48 #x8b #x50 #x68  #x48 #x8b #x52 #x20  #x48 #x89 #x50 #x68)
  "mov 0x68(%rax),%rdx; mov 0x20(%rdx),%rdx; mov %rdx,0x68(%rax).")

(defun nelisp-eln-handler-frame--scan (bytes start kind allowed-slots
                                             function-vaddr thread-got)
  "Decode and check one region of BYTES starting at START.
KIND is `guarded' (ends at its `jmp rel32') or `pad' (ends at its first
branch, no call allowed).  Return the plist (:START S :END E :POP P)."
  (let ((pos start) (count 0) (table-regs nil) (thread-state nil)
        (pops 0) (done nil) (pop-at nil)
        (pop-pattern (if (eq kind 'guarded)
                         nelisp-eln-handler-frame--pop-guarded
                       nelisp-eln-handler-frame--pop-pad)))
    (while (not done)
      (when (> (setq count (1+ count))
               nelisp-eln-handler-frame--max-instructions)
        (nelisp-eln-handler-frame--fail 'runaway-region kind start))
      (let* ((ins (nelisp-eln-handler-frame--decode bytes pos))
             (length (plist-get ins :length))
             (k (plist-get ins :kind))
             (base (plist-get ins :base))
             (reg (plist-get ins :reg))
             (next (+ pos length)))
        (pcase k
          ('call-rel32
           (nelisp-eln-handler-frame--fail 'local-call-in-region kind pos))
          ('jmp-indirect
           (nelisp-eln-handler-frame--fail 'indirect-jump-in-region kind pos))
          ('call-indirect
           (unless (eq kind 'guarded)
             (nelisp-eln-handler-frame--fail 'call-in-landing-pad pos))
           (let ((disp (plist-get ins :disp)))
             (unless (and (= (plist-get ins :mod) 2) base
                          (memq base table-regs)
                          (= (% disp 8) 0) (>= disp 0)
                          (memq (/ disp 8) allowed-slots))
               (nelisp-eln-handler-frame--fail
                'unauthenticated-indirect-call pos disp base))))
          ((or 'jmp 'jcc)
           (cond
            ((and (eq kind 'guarded) (eq k 'jmp))
             (let ((target (plist-get ins :target)))
               (unless (and (>= target 0) (< target (length bytes)))
                 (nelisp-eln-handler-frame--fail 'exit-outside-body pos target))
               (setq done t)))
            ((eq kind 'guarded)
             (nelisp-eln-handler-frame--fail 'branch-in-guarded-region pos))
            (t (setq done t))))
          ((or 'store 'xmm-store)
           ;; a store is to the frame, or the pop triple's own store
           (cond
            ((and (eq base nelisp-eln-handler-frame--rsp)
                  (not (plist-get ins :rip)))
             nil)
            ((and (eq k 'store)
                  (nelisp-eln-handler-frame--bytes-at
                   bytes (- next 12) pop-pattern))
             (setq pops (1+ pops) pop-at (- next 12)))
            (t (nelisp-eln-handler-frame--fail 'store-outside-frame pos))))
          (_ nil))
        ;; register tracking
        (pcase k
          ('load
           (cond
            ;; mov (%rsp),%reg: the saved link-table pointer
            ((and (eq base nelisp-eln-handler-frame--rsp) (= (plist-get ins :disp) 0)
                  (not (plist-get ins :rip)))
             (setq table-regs (cons reg (delq reg table-regs)))
             (setq thread-state (nelisp-eln-handler-frame--drop-reg thread-state reg)))
            ;; mov GOT(%rip),%reg reaching current_thread_reloc
            ((plist-get ins :rip)
             (setq table-regs (delq reg table-regs))
             (setq thread-state (nelisp-eln-handler-frame--drop-reg thread-state reg))
             (when (eql (+ function-vaddr next (plist-get ins :disp)) thread-got)
               (push (cons reg 1) thread-state)))
            ;; mov (%r),%r2 through the thread chain
            ((and base (= (plist-get ins :disp) 0)
                  (assq base thread-state)
                  (< (cdr (assq base thread-state)) 3))
             (let ((level (1+ (cdr (assq base thread-state)))))
               (setq table-regs (delq reg table-regs))
               (setq thread-state (nelisp-eln-handler-frame--drop-reg thread-state reg))
               (push (cons reg level) thread-state)))
            (t (setq table-regs (delq reg table-regs))
               (setq thread-state (nelisp-eln-handler-frame--drop-reg thread-state reg)))))
          ((or 'lea 'move)
           (let ((dest (if (eq k 'lea) reg (if (= (plist-get ins :op) #x8b) reg
                                              (plist-get ins :rm)))))
             (setq table-regs (delq dest table-regs))
             (setq thread-state (nelisp-eln-handler-frame--drop-reg thread-state dest))))
          ('xmm-to-gpr
           (setq table-regs (delq (plist-get ins :rm) table-regs))
           (setq thread-state (nelisp-eln-handler-frame--drop-reg
                            thread-state (plist-get ins :rm))))
          (_ nil))
        ;; the pop must go through the real thread pointer (level 3 = the
        ;; `struct thread_state *' after GOT -> cell -> &current_thread ->
        ;; thread)
        (when (and pop-at (eql (+ pop-at 12) next)
                   (let ((pop-base (if (eq kind 'guarded) 2 0)))
                     ;; %rdx (2) for the guarded pop, %rax (0) for the pad
                     (not (eql (cdr (assq pop-base thread-state)) 3))))
          (nelisp-eln-handler-frame--fail 'pop-not-through-current-thread pos))
        (setq pos next)))
    (unless (= pops 1)
      (nelisp-eln-handler-frame--fail 'pop-count kind start pops))
    (list :start start :end pos :pop pop-at)))

(defun nelisp-eln-handler-frame-check (bytes plt-offsets allowed-slots
                                             function-vaddr thread-got
                                             &optional push-slot)
  "Prove the frame-local rule for every `_setjmp' site of BYTES.
PLT-OFFSETS lists the offsets of the rel32 fields of the body's
`call _setjmp@plt' sites, ALLOWED-SLOTS the link-table slots a guarded
region may call, FUNCTION-VADDR the body's virtual address and THREAD-GOT
the virtual address of the GOT slot of `current_thread_reloc'.  PUSH-SLOT
defaults to 2 (`push_handler').  Return a list of plists, one per site:
\(:PLT-CALL P :PUSH-CALL C :GUARDED (S . E) :PAD (S . E)).  Signal
`nelisp-eln-handler-frame-error' on any violation."
  (let ((push-slot (or push-slot 2)) (regions nil))
    (unless (and (stringp bytes) plt-offsets (listp allowed-slots)
                 (integerp function-vaddr) (integerp thread-got))
      (nelisp-eln-handler-frame--fail 'bad-arguments))
    (dolist (p plt-offsets)
      (unless (and (integerp p) (>= p 12) (< (+ p 4) (length bytes))
                   (= (aref bytes (1- p)) #xe8))
        (nelisp-eln-handler-frame--fail 'plt-site-not-a-call p))
      (let* ((push-call (- p 5 3))
             (test-at (+ p 4)))
        ;; push_handler (tag, CONDITION_CASE); lea 0x40(%rax),%rdi
        (unless (nelisp-eln-handler-frame--bytes-at bytes (- p 5)
                                                    '(#x48 #x8d #x78 #x40))
          (nelisp-eln-handler-frame--fail 'no-jmp-buffer-address p))
        (unless (and (= (aref bytes push-call) #xff)
                     (memq (aref bytes (1+ push-call)) '(#x50 #x53 #x55))
                     (= (aref bytes (+ push-call 2)) (* 8 push-slot)))
          (nelisp-eln-handler-frame--fail 'no-push-handler-call p))
        (unless (let ((k (max 0 (- push-call 24))) (found nil))
                  (while (and (not found) (< k (- push-call 4)))
                    (when (nelisp-eln-handler-frame--bytes-at
                           bytes k '(#xbe #x01 #x00 #x00 #x00))
                      (setq found t))
                    (setq k (1+ k)))
                  found)
          (nelisp-eln-handler-frame--fail 'handler-type-not-condition-case p))
        (unless (nelisp-eln-handler-frame--bytes-at bytes test-at '(#x85 #xc0 #x0f #x84))
          (nelisp-eln-handler-frame--fail 'no-setjmp-branch p))
        (let* ((rel (nelisp-eln-handler-frame--s32 bytes (+ test-at 4)))
               (guarded-start (+ test-at 4 4 rel))
               (pad-start (+ test-at 8)))
          (unless (and (>= guarded-start 0) (< guarded-start (length bytes)))
            (nelisp-eln-handler-frame--fail 'guarded-outside-body p))
          (let ((guarded (nelisp-eln-handler-frame--scan
                          bytes guarded-start 'guarded allowed-slots
                          function-vaddr thread-got))
                (pad (nelisp-eln-handler-frame--scan
                      bytes pad-start 'pad allowed-slots
                      function-vaddr thread-got)))
            (push (list :plt-call p :push-call push-call
                        :guarded (cons (plist-get guarded :start)
                                       (plist-get guarded :end))
                        :pad (cons (plist-get pad :start)
                                   (plist-get pad :end)))
                  regions)))))
    (nreverse regions)))

(provide 'nelisp-eln-handler-frame)

;;; nelisp-eln-handler-frame.el ends here
