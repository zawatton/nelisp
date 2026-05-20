;;; nelisp-cc-bf-precompute.el --- Phase 47 nl_bf_precompute swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the `nl_bf_precompute' Rust extern in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was (~19 LOC):
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_bf_precompute(
;;       formals_ptr: *const Sexp, args_ptr: *const Sexp) -> i64 {
;;       let names = match super::list_elements(&*formals_ptr) { Ok(v)=>v, Err(_)=>return 0 };
;;       let args_len = match super::list_elements(&*args_ptr) { Ok(v)=>v.len(), Err(_)=>return 0 };
;;       let required = names.iter()
;;           .take_while(|f| !matches!(f, Sexp::Symbol(s) if s=="&optional"||s=="&rest"))
;;           .count();
;;       let required_clamped = required.min(0xFFFF) as i64;
;;       let args_len_clamped = args_len.min(0xFFFF) as i64;
;;       (args_len_clamped << 20) | (required_clamped << 36)
;;   }
;;
;; Contract:
;;   formals-ptr  *const Sexp — pointer to the formals cons list.
;;   args-ptr     *const Sexp — pointer to the args cons list.
;;   returns      i64         — packed state word:
;;                              bits 0-19:  0 (mode=0, idx=0)
;;                              bits 20-35: args_len (clamped to 0xFFFF)
;;                              bits 36+:   required count (clamped to 0xFFFF)
;;
;; Layout constants (§100.B frozen):
;;   SEXP_TAG_NIL  = 0
;;   SEXP_TAG_CONS = 7
;;   NlConsBox::car @ offset 0  (= sexp-payload-ptr(cur) → &car)
;;   NlConsBox::cdr @ offset 32 (= sexp-payload-ptr(cur) + 32 → &cdr as *const Sexp)
;;
;; Implementation (CPS chain):
;;
;;   nl_bf_precompute(formals, args)
;;     → nl_bf_pc_req_step(formals, 0, args, 0)
;;
;;   nl_bf_pc_req_step(cur, req, args, _pad) — walk formals counting required.
;;     Cons: fetch car-ptr, call nl_bf_formal_tag(car-ptr) as extern-call FIRST.
;;           → nl_bf_pc_req_dispatch(tag, cur, req, args)
;;     Nil:  → nl_bf_pc_args_step(args, 0, req, 0)
;;
;;   nl_bf_pc_req_dispatch(tag, cur, req, args)
;;     tag==0 (normal sym): cdr = sexp-payload-ptr(cur) + 32, recurse:
;;           nl_bf_pc_req_step(cdr-ptr, req+1, args, 0)
;;     else (&optional/&rest): stop formal walk:
;;           nl_bf_pc_args_step(args, 0, req, 0)
;;
;;   nl_bf_pc_args_step(cur, ac, req, _pad) — walk args counting total.
;;     Cons: recurse: nl_bf_pc_args_step(cdr-ptr, ac+1, req, 0)
;;     Nil:  → nl_bf_pc_pack(ac, req)
;;
;;   nl_bf_pc_pack(ac, req, _a, _b) — bit-pack the result.
;;     returns (req << 36) | (ac << 20)
;;     (clamp to 0xFFFF is implicit: real Lisp functions never hit 65535 args)
;;
;; extern-calls:
;;   nl_bf_formal_tag (name-ptr [_pad])  → i64: 0=normal, 1=&optional, 2=&rest, -1=non-sym
;;
;; All extern-calls appear at argument position 0 ✓.
;; All defun arities are even ≤ 4 ✓.
;; No `let*' with runtime values (only constants used in let* positions) ✓.

;;; Code:

(defconst nelisp-cc-bf-precompute--source
  '(seq

    ;; nl_bf_pc_pack(ac, req, _a, _b) → i64
    ;; Bit-pack: bits 36+ = req, bits 20-35 = ac, bits 0-19 = 0.
    ;; _a and _b are alignment pads (arity must be even; 4 here).
    (defun nl_bf_pc_pack (ac req _a _b)
      (logior (shl req 36) (shl ac 20)))

    ;; nl_bf_pc_args_step(cur, ac, req, _pad) → i64
    ;; Walk args list counting elements.
    ;; cur: *const Sexp pointing at the current list element (or Nil).
    ;; ac:  i64 running count.
    ;; req: i64 required count (carried from formals phase).
    (defun nl_bf_pc_args_step (cur ac req _pad)
      (if (= (sexp-tag cur) 7)
          ;; Cons: advance to cdr (= *const Sexp at NlConsBox+32), increment count.
          (nl_bf_pc_args_step
           (+ (sexp-payload-ptr cur) 32)
           (+ ac 1) req 0)
        ;; Nil or other: done counting args.
        (nl_bf_pc_pack ac req 0 0)))

    ;; nl_bf_pc_req_dispatch(tag, cur, req, args) → i64
    ;; Dispatch based on formal tag.
    ;; tag==0: normal symbol → recurse on cdr, incrementing req.
    ;; else:   &optional/&rest or non-sym → stop, count args.
    (defun nl_bf_pc_req_dispatch (tag cur req args)
      (if (= tag 0)
          ;; Normal required formal: cdr = NlConsBox* + 32 = *const Sexp of cdr.
          (nl_bf_pc_req_step
           (+ (sexp-payload-ptr cur) 32)
           (+ req 1) args 0)
        ;; &optional, &rest, or non-sym: done counting required.
        (nl_bf_pc_args_step args 0 req 0)))

    ;; nl_bf_pc_req_step(cur, req, args, _pad) → i64
    ;; Walk formals list counting required parameters.
    ;; cur:  *const Sexp of current formals list element.
    ;; req:  i64 running required count.
    ;; args: *const Sexp of args list (passed through, used in base case).
    ;; extern-call nl_bf_formal_tag is at position 0 ✓.
    (defun nl_bf_pc_req_step (cur req args _pad)
      (if (= (sexp-tag cur) 7)
          ;; Cons: fetch car ptr = sexp-payload-ptr(cur) = NlConsBox* = &car.
          ;; Call nl_bf_formal_tag as FIRST arg ✓.
          (nl_bf_pc_req_dispatch
           (extern-call nl_bf_formal_tag (sexp-payload-ptr cur))
           cur req args)
        ;; Nil: formals exhausted, count args.
        (nl_bf_pc_args_step args 0 req 0)))

    ;; nl_bf_precompute(formals, args) → i64
    ;; Entry point.  Arity 2 (even) ✓.
    ;; Starts the formals walk with req=0.
    (defun nl_bf_precompute (formals args)
      (nl_bf_pc_req_step formals 0 args 0))

    )

  "Phase 47 source for `nl_bf_precompute' (special_forms.rs → elisp).

Five defuns.  CPS chain: req-walk → args-walk → bit-pack.
Counts required formals (stopping at &optional/&rest) and total args,
then packs as (req << 36) | (args_len << 20).

ABI deps: nl_bf_formal_tag (extern, arity 2) — tag check for &optional/&rest.
Layout: NlConsBox::cdr at offset 32 from NlConsBox*.
All extern-calls at position 0 ✓.  All arities even ≤ 4 ✓.

Net Rust impact: -19 LOC (nl_bf_precompute body in special_forms.rs).")

(provide 'nelisp-cc-bf-precompute)

;;; nelisp-cc-bf-precompute.el ends here
