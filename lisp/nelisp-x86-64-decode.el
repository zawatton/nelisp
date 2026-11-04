;;; nelisp-x86-64-decode.el --- x86-64 instruction length and control-flow decoder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 The NeLisp authors.

;;; Commentary:

;; A table-driven x86-64 decoder that answers exactly the questions the
;; rooted prelink closure asks of capstone: how long is the instruction at a
;; given offset, is it a call or a jump, is its target an immediate, and if so
;; where does the immediate live and what absolute address does it name.
;;
;; The decoder never guesses.  Any opcode that is not in its tables, any
;; reserved ModRM.reg extension, and any instruction that would extend past
;; END signals `nelisp-x86-64-decode-error'.
;;
;; The tables are vectors indexed by the opcode byte so that decoding stays
;; cheap even when this file is loaded as interpreted source.  An entry packs
;;   bit 0      the opcode takes a ModRM byte
;;   bits 1-4   the immediate kind (see the `imm' constants below)
;;   bits 5-7   the branch class (see the `br' constants below)
;;
;; `nelisp-x86-64-decode-insn' returns a vector:
;;   [LENGTH MNEMONIC CONTROL OPERAND IMM-OFFSET IMM-SIZE TARGET ADDRESS]
;; CONTROL is nil, `call' or `jump' (capstone CS_GRP_CALL / CS_GRP_JUMP; ret is
;; neither).  OPERAND is nil, `imm' or `indirect'.  MNEMONIC is capstone's
;; text for every call/jump form and the coarse label "insn" for the rest.

;;; Code:

(define-error 'nelisp-x86-64-decode-error "x86-64 decode error")

(defun nelisp-x86-64-decode--fail (message pos)
  "Signal a decode error with MESSAGE for the instruction at index POS."
  (signal 'nelisp-x86-64-decode-error (list message pos)))

;; Immediate kinds.
;;  0 none, 1 imm8, 2 imm16, 3 immz (2 with 66, else 4), 4 movabs-capable
;;  (8 with REX.W, 2 with 66, else 4), 5 enter (imm16+imm8), 6 rel8, 7 rel32,
;;  8 moffs (8, or 4 with 67), 9 F6 group (imm8 for /0 /1), 10 F7 group
;;  (immz for /0 /1).
;; Branch classes.
;;  0 none, 1 call rel, 2 jmp rel, 3 jcc rel, 4 loop family, 5 FF group.

(defconst nelisp-x86-64-decode--prefix-table
  (let ((table (make-vector 256 nil)))
    (dolist (byte '(#x26 #x2E #x36 #x3E #x64 #x65 #xF0 #xF2 #xF3))
      (aset table byte 3))
    (aset table #x66 1)
    (aset table #x67 2)
    table)
  "Legacy prefix classification: 1 operand size, 2 address size, 3 other.")

(defconst nelisp-x86-64-decode--map1
  (let ((table (make-vector 256 nil)))
    (let ((put (lambda (lo hi modrm imm br)
                 (let ((i lo)
                       (entry (+ modrm (ash imm 1) (ash br 5))))
                   (while (<= i hi)
                     (aset table i entry)
                     (setq i (1+ i)))))))
      (let ((base 0))
        (while (< base 64)
          (funcall put base (+ base 3) 1 0 0)
          (funcall put (+ base 4) (+ base 4) 0 1 0)
          (funcall put (+ base 5) (+ base 5) 0 3 0)
          (setq base (+ base 8))))
      (funcall put #x50 #x5F 0 0 0)
      (funcall put #x63 #x63 1 0 0)
      (funcall put #x68 #x68 0 3 0)
      (funcall put #x69 #x69 1 3 0)
      (funcall put #x6A #x6A 0 1 0)
      (funcall put #x6B #x6B 1 1 0)
      (funcall put #x6C #x6F 0 0 0)
      (funcall put #x70 #x7F 0 6 3)
      (funcall put #x80 #x80 1 1 0)
      (funcall put #x81 #x81 1 3 0)
      (funcall put #x83 #x83 1 1 0)
      (funcall put #x84 #x8F 1 0 0)
      (funcall put #x90 #x99 0 0 0)
      (funcall put #x9B #x9F 0 0 0)
      (funcall put #xA0 #xA3 0 8 0)
      (funcall put #xA4 #xA7 0 0 0)
      (funcall put #xA8 #xA8 0 1 0)
      (funcall put #xA9 #xA9 0 3 0)
      (funcall put #xAA #xAF 0 0 0)
      (funcall put #xB0 #xB7 0 1 0)
      (funcall put #xB8 #xBF 0 4 0)
      (funcall put #xC0 #xC1 1 1 0)
      (funcall put #xC2 #xC2 0 2 0)
      (funcall put #xC3 #xC3 0 0 0)
      (funcall put #xC6 #xC6 1 1 0)
      (funcall put #xC7 #xC7 1 3 0)
      (funcall put #xC8 #xC8 0 5 0)
      (funcall put #xC9 #xC9 0 0 0)
      (funcall put #xCA #xCA 0 2 0)
      (funcall put #xCB #xCC 0 0 0)
      (funcall put #xCD #xCD 0 1 0)
      (funcall put #xCF #xCF 0 0 0)
      (funcall put #xD0 #xD3 1 0 0)
      (funcall put #xD7 #xD7 0 0 0)
      (funcall put #xD8 #xDF 1 0 0)
      (funcall put #xE0 #xE3 0 6 4)
      (funcall put #xE4 #xE7 0 1 0)
      (funcall put #xE8 #xE8 0 7 1)
      (funcall put #xE9 #xE9 0 7 2)
      (funcall put #xEB #xEB 0 6 2)
      (funcall put #xEC #xEF 0 0 0)
      (funcall put #xF1 #xF1 0 0 0)
      (funcall put #xF4 #xF5 0 0 0)
      (funcall put #xF6 #xF6 1 9 0)
      (funcall put #xF7 #xF7 1 10 0)
      (funcall put #xF8 #xFD 0 0 0)
      (funcall put #xFE #xFE 1 0 0)
      (funcall put #xFF #xFF 1 0 5))
    table)
  "One-byte opcode map.")

(defconst nelisp-x86-64-decode--map2
  (let ((table (make-vector 256 nil)))
    (let ((put (lambda (lo hi modrm imm br)
                 (let ((i lo)
                       (entry (+ modrm (ash imm 1) (ash br 5))))
                   (while (<= i hi)
                     (aset table i entry)
                     (setq i (1+ i)))))))
      (funcall put #x00 #x03 1 0 0)
      (funcall put #x05 #x05 0 0 0)
      (funcall put #x0B #x0B 0 0 0)
      (funcall put #x0D #x0D 1 0 0)
      (funcall put #x10 #x17 1 0 0)
      (funcall put #x18 #x1F 1 0 0)
      (funcall put #x28 #x2F 1 0 0)
      (funcall put #x31 #x31 0 0 0)
      (funcall put #x40 #x6F 1 0 0)
      (funcall put #x70 #x73 1 1 0)
      (funcall put #x74 #x76 1 0 0)
      (funcall put #x77 #x77 0 0 0)
      (funcall put #x7C #x7F 1 0 0)
      (funcall put #x80 #x8F 0 7 3)
      (funcall put #x90 #x9F 1 0 0)
      (funcall put #xA0 #xA2 0 0 0)
      (funcall put #xA3 #xA3 1 0 0)
      (funcall put #xA4 #xA4 1 1 0)
      (funcall put #xA5 #xA5 1 0 0)
      (funcall put #xA8 #xA9 0 0 0)
      (funcall put #xAB #xAB 1 0 0)
      (funcall put #xAC #xAC 1 1 0)
      (funcall put #xAD #xAF 1 0 0)
      (funcall put #xB0 #xB1 1 0 0)
      (funcall put #xB3 #xB3 1 0 0)
      (funcall put #xB6 #xB8 1 0 0)
      (funcall put #xBA #xBA 1 1 0)
      (funcall put #xBB #xC1 1 0 0)
      (funcall put #xC2 #xC2 1 1 0)
      (funcall put #xC3 #xC3 1 0 0)
      (funcall put #xC4 #xC6 1 1 0)
      (funcall put #xC7 #xC7 1 0 0)
      (funcall put #xC8 #xCF 0 0 0)
      (funcall put #xD0 #xFE 1 0 0))
    table)
  "Two-byte (0F xx) opcode map.")

(defconst nelisp-x86-64-decode--group1
  (let ((table (make-vector 256 nil)))
    ;; Allowed ModRM.reg values as a bit mask; nil means every value.
    (aset table #x8F #x01)
    (aset table #xC6 #x01)
    (aset table #xC7 #x01)
    (aset table #xFE #x03)
    (aset table #xFF #x7F)
    table)
  "Per-opcode ModRM.reg masks for the one-byte map.")

(defconst nelisp-x86-64-decode--group2
  (let ((table (make-vector 256 nil)))
    (aset table #xBA #xF0)
    table)
  "Per-opcode ModRM.reg masks for the two-byte map.")

(defconst nelisp-x86-64-decode--cc-names
  ["jo" "jno" "jb" "jae" "je" "jne" "jbe" "ja"
   "js" "jns" "jp" "jnp" "jl" "jge" "jle" "jg"]
  "Capstone mnemonics of the conditional jumps, indexed by condition code.")

(defconst nelisp-x86-64-decode--loop-names
  ["loopne" "loope" "loop" "jrcxz"]
  "Capstone mnemonics of E0-E3, indexed by opcode minus E0.")

(defmacro nelisp-x86-64-decode--byte (index)
  "Read the byte at INDEX of BYTES, failing when INDEX is not below END."
  `(if (< ,index end)
       (aref bytes ,index)
     (nelisp-x86-64-decode--fail "Truncated instruction" pos)))

(defun nelisp-x86-64-decode-insn (bytes pos end address)
  "Decode the instruction of unibyte string BYTES that starts at index POS.
No byte at or past index END is read.  ADDRESS is the virtual address of POS.
Return [LENGTH MNEMONIC CONTROL OPERAND IMM-OFFSET IMM-SIZE TARGET ADDRESS].
Signal `nelisp-x86-64-decode-error' for unsupported or truncated input."
  (let ((p pos) (b 0) (opsize nil) (adsize nil) (rex 0) (entry nil)
        (mask nil) (modrm 0) (kind 0) (br 0) (opcode 0)
        (isize 0) (prefixes 0) (class nil))
    (setq b (nelisp-x86-64-decode--byte p))
    (while (setq class (aref nelisp-x86-64-decode--prefix-table b))
      (cond ((eq class 1) (setq opsize t))
            ((eq class 2) (setq adsize t)))
      (setq prefixes (1+ prefixes))
      (if (> prefixes 4)
          (nelisp-x86-64-decode--fail "Too many prefixes" pos))
      (setq p (1+ p))
      (setq b (nelisp-x86-64-decode--byte p)))
    (when (= (logand b #xF0) #x40)
      (setq rex b)
      (setq p (1+ p))
      (setq b (nelisp-x86-64-decode--byte p))
      (if (aref nelisp-x86-64-decode--prefix-table b)
          (nelisp-x86-64-decode--fail "Prefix after REX" pos)))
    (setq p (1+ p))
    (setq opcode b)
    (cond
     ((= b #x0F)
      (setq opcode (nelisp-x86-64-decode--byte p))
      (setq p (1+ p))
      (cond
       ((or (= opcode #x38) (= opcode #x3A))
        ;; Three-byte maps: ModRM always, imm8 only for 0F 3A.
        (setq class (nelisp-x86-64-decode--byte p))
        (setq p (1+ p))
        (setq entry (if (= opcode #x38) 1 3))
        (setq opcode (+ 256 opcode)))
       (t
        (setq entry (aref nelisp-x86-64-decode--map2 opcode))
        (setq mask (aref nelisp-x86-64-decode--group2 opcode)))))
     (t
      (setq entry (aref nelisp-x86-64-decode--map1 b))
      (setq mask (aref nelisp-x86-64-decode--group1 b))))
    (unless entry
      (nelisp-x86-64-decode--fail "Unsupported opcode" pos))
    (setq kind (logand (ash entry -1) 15))
    (setq br (ash entry -5))
    (when (= (logand entry 1) 1)
      (setq modrm (nelisp-x86-64-decode--byte p))
      (setq p (1+ p))
      (if (and mask (not (/= 0 (logand mask (ash 1 (logand (ash modrm -3) 7))))))
          (nelisp-x86-64-decode--fail "Reserved ModRM.reg" pos))
      (let ((mod (ash modrm -6))
            (rm (logand modrm 7)))
        (cond
         ((= mod 3))
         (t
          (if (= rm 4)
              (let ((sib (nelisp-x86-64-decode--byte p)))
                (setq p (1+ p))
                (if (and (= mod 0) (= (logand sib 7) 5))
                    (setq p (+ p 4))))
            (if (and (= mod 0) (= rm 5))
                (setq p (+ p 4))))
          (cond ((= mod 1) (setq p (1+ p)))
                ((= mod 2) (setq p (+ p 4)))))))
      (when (> kind 8)
        (let ((reg (logand (ash modrm -3) 7)))
          (setq kind (cond ((> reg 1) 0)
                           ((= kind 9) 1)
                           (t 3))))))
    (setq isize
          (cond ((= kind 0) 0)
                ((= kind 1) 1)
                ((= kind 2) 2)
                ((= kind 3) (cond ((/= 0 (logand rex 8)) 4) (opsize 2) (t 4)))
                ((= kind 4) (cond ((/= 0 (logand rex 8)) 8) (opsize 2) (t 4)))
                ((= kind 5) 3)
                ((= kind 6) 1)
                ((= kind 7) 4)
                (t (if adsize 4 8))))
    (setq p (+ p isize))
    (if (> p end)
        (nelisp-x86-64-decode--fail "Truncated instruction" pos))
    (let ((length (- p pos)))
      (cond
       ((= br 0)
        (vector length "insn" nil nil nil nil nil address))
       ((= br 5)
        (let ((reg (logand (ash modrm -3) 7)))
          (cond
           ((or (< reg 2) (> reg 5))
            (vector length "insn" nil nil nil nil nil address))
           (t
            (if (or opsize (and (or (= reg 3) (= reg 5)) (= (ash modrm -6) 3)))
                (nelisp-x86-64-decode--fail "Unsupported indirect branch" pos))
            (vector length
                    (aref ["call" "lcall" "jmp" "ljmp"] (- reg 2))
                    (if (< reg 4) 'call 'jump)
                    'indirect nil nil nil address)))))
       (t
        (if opsize
            (nelisp-x86-64-decode--fail "Unsupported branch prefix" pos))
        (let* ((last (1- p))
               (disp (if (= isize 1)
                         (let ((v (aref bytes last)))
                           (if (> v 127) (- v 256) v))
                       (let ((v (+ (aref bytes (- last 3))
                                   (ash (aref bytes (- last 2)) 8)
                                   (ash (aref bytes (- last 1)) 16)
                                   (ash (aref bytes last) 24))))
                         (if (> v #x7FFFFFFF) (- v #x100000000) v)))))
          (vector length
                  (cond ((= br 1) "call")
                        ((= br 2) "jmp")
                        ((= br 3) (aref nelisp-x86-64-decode--cc-names
                                        (logand opcode 15)))
                        (t (aref nelisp-x86-64-decode--loop-names
                                 (- opcode #xE0))))
                  (if (= br 1) 'call 'jump)
                  'imm (- length isize) isize
                  (+ address length disp) address)))))))

(defun nelisp-x86-64-decode-range (bytes start end base)
  "Decode every instruction of BYTES in [START, END) and return them in order.
BASE is the virtual address of index 0.  The instructions must tile the range
exactly; otherwise `nelisp-x86-64-decode-error' is signalled."
  (let ((pos start) (result nil) (insn nil))
    (while (< pos end)
      (setq insn (nelisp-x86-64-decode-insn bytes pos end (+ base pos)))
      (setq result (cons insn result))
      (setq pos (+ pos (aref insn 0))))
    (nreverse result)))

(defsubst nelisp-x86-64-decode-insn-length (insn) "Length of INSN." (aref insn 0))
(defsubst nelisp-x86-64-decode-insn-mnemonic (insn) "Mnemonic of INSN." (aref insn 1))
(defsubst nelisp-x86-64-decode-insn-control (insn) "Control class of INSN." (aref insn 2))
(defsubst nelisp-x86-64-decode-insn-operand (insn) "Branch operand kind of INSN." (aref insn 3))
(defsubst nelisp-x86-64-decode-insn-imm-offset (insn) "Immediate offset of INSN." (aref insn 4))
(defsubst nelisp-x86-64-decode-insn-imm-size (insn) "Immediate size of INSN." (aref insn 5))
(defsubst nelisp-x86-64-decode-insn-target (insn) "Absolute branch target of INSN." (aref insn 6))
(defsubst nelisp-x86-64-decode-insn-address (insn) "Address of INSN." (aref insn 7))

(provide 'nelisp-x86-64-decode)
;;; nelisp-x86-64-decode.el ends here
