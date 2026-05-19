;;; nelisp-cc-sexp-eq.el --- Phase 47 nl_sexp_eq elisp .o  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Moves the `#[no_mangle] extern "C" fn nl_sexp_eq' body from
;; `build-tool/src/eval/special_forms.rs' into a Phase 47 elisp `.o'.
;;
;; Contract: `(nl_sexp_eq A B)' — both `*const Sexp' — returns i64:
;;   1 if the two Sexp values are `eq', 0 otherwise.
;;
;; Mirrors `sexp_eq(a: &Sexp, b: &Sexp) -> bool' logic, arm by arm:
;;
;;   tag 0  (Nil)       — same-tag → 1.
;;   tag 1  (T)         — same-tag → 1.
;;   tag 2  (Int)       — compare `sexp-int-unwrap' (i64 at offset 8).
;;   tag 3  (Float)     — compare bits via `sexp-int-unwrap' (same layout).
;;   tag 4  (Symbol)    — `symbol-eq' grammar op (inline name compare).
;;   tag 5  (Str)       — `str-eq' grammar op (byte-payload equality).
;;   tags 6,7,8,9,10,12 (MutStr/Cons/Vector/CharTable/BoolVector/Record)
;;                      — identity via `(= (sexp-payload-ptr a)
;;                                        (sexp-payload-ptr b))'.
;;   tag mismatch / tag 11 (Cell) / anything else → 0.
;;
;; Tag constants (from `build-tool/src/eval/sexp.rs'):
;;   0=Nil 1=T 2=Int 3=Float 4=Symbol 5=Str 6=MutStr 7=Cons
;;   8=Vector 9=CharTable 10=BoolVector 11=Cell 12=Record
;;
;; Arity 2 (even) — body-entry rsp ≡ 0 mod 16.  All sub-calls use
;; inline grammar ops (`symbol-eq', `str-eq', `sexp-int-unwrap',
;; `sexp-payload-ptr') with no PLT round-trip.
;;
;; The defun name `nl_sexp_eq' is the exported symbol; existing callers
;; that use `extern-call nl_sexp_eq' resolve to this .o without change.

;;; Code:

(defconst nelisp-cc-sexp-eq--source
  '(defun nl_sexp_eq (a b)
     ;; a, b: *const Sexp (rdi / rsi).
     ;; Returns i64: 1 = eq, 0 = not eq.
     ;;
     ;; Step 1: tag-byte equality fast-reject.
     (if (= (sexp-tag a) (sexp-tag b))
         ;; Tags match — dispatch on tag value.
         (if (= (sexp-tag a) 2)
             ;; Int: compare i64 payload at offset 8.
             (if (= (sexp-int-unwrap a) (sexp-int-unwrap b)) 1 0)
           (if (= (sexp-tag a) 4)
               ;; Symbol: inline name compare via symbol-eq.
               (symbol-eq a b)
             (if (= (sexp-tag a) 5)
                 ;; Str: byte-payload equality via str-eq.
                 (str-eq a b)
               (if (= (sexp-tag a) 3)
                   ;; Float: compare raw bits (same layout as Int at offset 8).
                   (if (= (sexp-int-unwrap a) (sexp-int-unwrap b)) 1 0)
                 (if (= (sexp-tag a) 0)
                     ;; Nil: same-tag → eq.
                     1
                   (if (= (sexp-tag a) 1)
                       ;; T: same-tag → eq.
                       1
                     ;; MutStr(6) / Cons(7) / Vector(8) / CharTable(9)
                     ;; BoolVector(10) / Record(12): identity = same box ptr.
                     ;; Cell(11) and any unknown tag: same test (ptr or 0).
                     (if (= (sexp-payload-ptr a) (sexp-payload-ptr b))
                         1
                       0)))))))
       ;; Tags differ → not eq.
       0))
  "Phase 47 source for `nl_sexp_eq'.

Replaces the Rust `#[no_mangle] extern \"C\" fn nl_sexp_eq' in
`build-tool/src/eval/special_forms.rs'.  Tag dispatch with inline
grammar ops; no heap allocation; no PLT calls beyond the inline
string-eq-core shared helper used by `symbol-eq' and `str-eq'.

The defun name `nl_sexp_eq' is the exported symbol; callers using
`extern-call nl_sexp_eq' resolve unchanged.")

(provide 'nelisp-cc-sexp-eq)

;;; nelisp-cc-sexp-eq.el ends here
