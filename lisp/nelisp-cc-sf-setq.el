;;; nelisp-cc-sf-setq.el --- AOT nl_sf_setq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the `sf_setq' Rust body in
;; `build-tool/src/eval/special_forms.rs'.  The Rust body was:
;;
;;   fn sf_setq(args: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;       let parts = args_vec(args)?;
;;       if parts.len() % 2 != 0 { return Err(...); }
;;       let mut last = Sexp::Nil;
;;       while let Some(name_form) = iter.next() {
;;           let value_form = iter.next().unwrap();
;;           let name = match name_form { Symbol(s) => s, ... };
;;           let val = eval(&value_form, env)?;
;;           env.set_value(&name, val.clone())?;
;;           last = val;
;;       }
;;       Ok(last)
;;   }
;;
;; `args' is the raw arg list: (SYM1 VAL1 SYM2 VAL2 ...).
;; Pairs: car=symbol-ptr (not eval'd), car(cdr)=value-form (eval'd).
;; Result slot `out' receives each value; last pair's value remains.
;;
;; ABI:
;;   nl_cons_car_ptr: *const Sexp → i64
;;   nl_cons_cdr_ptr: *const Sexp → i64
;;   nelisp_eval_call: (*const Sexp, *mut c_void, *mut Sexp) → i64
;;   nl_env_set_value: (*mut c_void, *const Sexp, *const Sexp) → i64
;;
;; Alignment: every extern-call is argument 0 at its call site.
;; CPS chain: each function receives exactly one extern-call result
;; as its first register argument, then does tag-checks or delegates.
;;
;; perf/setq-pair-cdr-raw-word (segment G2): `nl_sf_setq_evaled' used to
;; fetch the next-pair-or-Nil cdr via the materialising `nl_cons_cdr_ptr'
;; (lisp/nelisp-cc-jit-cons-cdr-ptr.el), which allocates a fresh 32-byte
;; box whenever that cdr is an immediate WORD -- which it always is for
;; the common one-pair `setq' (the Nil list terminator).  `nl_sf_setq_
;; cdr_word' reads the raw 8-byte tagged WORD directly instead, and
;; `nl_sf_setq_rest' / `nl_sf_setq_set_rc' / `nl_sf_setq_step' now thread
;; that WORD (classified via `nl_val_tag', not `sexp-tag') instead of a
;; materialised pointer.  See the commentary on `nl_sf_setq_cdr_word'
;; below for why this is behavior-preserving.
;;
;; Structure (9 defuns):
;;   nl_sf_setq_cdr_word (val-cdr) — raw cdr WORD, no allocation
;;   nl_sf_setq_set_rc (set-rc rest env out) — after set_value
;;   nl_sf_setq_rest   (rest sym env out) — after cdr-word(val-cdr)
;;   nl_sf_setq_evaled (eval-rc sym val-cdr env out _pad6) — after nelisp_eval_call, arity 6
;;   nl_sf_setq_val    (val-form sym val-cdr env out _pad6) — after car(val-cdr), arity 6
;;   nl_sf_setq_sym    (sym val-cdr env out) — after car(args-pair)
;;   nl_sf_setq_pair   (val-cdr args-pair env out) — after cdr(args-pair)
;;   nl_sf_setq_step   (cdr env out _pad) — recursive entry, cdr is a raw WORD
;;   nl_sf_setq        (args env out _pad) — public entry, arity 4 (even)

;;; Code:

(defconst nelisp-cc-sf-setq--source
  '(seq

    ;; perf/setq-pair-cdr-raw-word: `nl_sf_setq_evaled' fetches
    ;; `nl_cons_cdr_ptr(val-cdr)' purely to learn "is there another
    ;; (SYM VAL) pair, or is this the end of the arg list" -- the
    ;; overwhelmingly common case for `setq' (one pair) makes this the
    ;; Nil list terminator, an IMMEDIATE WORD, so `nl_cons_cdr_ptr'
    ;; (lisp/nelisp-cc-jit-cons-cdr-ptr.el) pays `(alloc-bytes 32 8)' +
    ;; `nl_val_load' on every `setq' just to materialise a Nil box that
    ;; `nl_sf_setq_step' immediately reduces to a single `sexp-tag == 0'
    ;; test and discards.  When more pairs follow, the materialised
    ;; pointer is used only to re-derive the SAME address a moment
    ;; later via `nl_cons_car_ptr'/`nl_cons_cdr_ptr' in
    ;; `nl_sf_setq_pair' -- it is never stored past this walk.
    ;;
    ;; Fix (mirrors A1/A2's raw-word pattern, ab4a72484 / 441aa152c):
    ;; read the cdr's raw 8-byte tagged WORD directly (same box_ptr ->
    ;; NlConsBox -> cdr-word addressing `nl_cons_cdr_ptr' itself uses)
    ;; and thread the WORD -- not a materialised view -- through
    ;; `nl_sf_setq_rest' / `nl_sf_setq_set_rc' / `nl_sf_setq_step'.
    ;; This is safe because a tagged WORD and "the pointer
    ;; `nl_cons_cdr_ptr' would have returned" are THE SAME VALUE in the
    ;; non-immediate case (low bit 0 already IS the 8-aligned `*const
    ;; Sexp' the accessor's own pointer branch returns unchanged), so
    ;; every downstream use that used to dereference a materialised
    ;; pointer keeps working once its `sexp-tag' call is replaced by
    ;; `nl_val_tag' (scripts/nelisp-standalone-build.el, Doc 146 §3.0),
    ;; which classifies a raw WORD the identical way `sexp-tag' would
    ;; classify the corresponding materialised view (for a pointer WORD
    ;; it dereferences the same tag byte; for an immediate WORD it
    ;; decodes the tag from the bits) for every value this slot can
    ;; hold.  VAL-CDR must already be a validated Cons view (tag 7):
    ;; `nl_sf_setq_sym' just called `nl_cons_car_ptr(val-cdr)' on the
    ;; same pointer to fetch VAL-FORM, which already requires it.
    ;; Guard mirrors `nl_cons_cdr_ptr' exactly (returns the same 0
    ;; sentinel for a non-Cons argument, rather than blindly
    ;; dereferencing) -- belt-and-suspenders: `nl_sf_setq_sym' already
    ;; calls `nl_cons_car_ptr(val-cdr)' moments earlier, which would
    ;; itself have faulted on a non-Cons val-cdr before this is ever
    ;; reached, but this keeps the two accessors' contracts identical
    ;; for every VAL-CDR value, not just the ones that survive that
    ;; earlier call.
    (defun nl_sf_setq_cdr_word (val-cdr)
      (if (= (sexp-tag val-cdr) 7)
          (ptr-read-u64 (ptr-read-u64 val-cdr 8) 8)
        0))

    ;; After nl_env_set_value: check rc, then recurse on rest.
    ;; REST is now a raw tagged WORD (perf/setq-pair-cdr-raw-word above),
    ;; not a materialised pointer; `nl_sf_setq_step' classifies it via
    ;; `nl_val_tag' accordingly.
    ;; Arity 4 (even).
    (defun nl_sf_setq_set_rc (set-rc rest env out)
      (if (= set-rc 0)
          (nl_sf_setq_step rest env out 0)
        1))

    ;; rest = nl_sf_setq_cdr_word(val-cdr) already fetched as first arg
    ;; (a raw WORD, perf/setq-pair-cdr-raw-word above -- no allocation).
    ;; Set the evaluated value via nl_env_set_value (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_setq_rest (rest sym env out)
      (nl_sf_setq_set_rc
       (extern-call nl_env_set_value env sym out)
       rest env out))

    ;; After nelisp_eval_call: check rc; if Ok, fetch rest as a raw WORD
    ;; (perf/setq-pair-cdr-raw-word above -- replaces the materialising
    ;; nl_cons_cdr_ptr call, which used to be sequenced after eval only
    ;; so its (formerly materialised, now raw-word) result would not
    ;; need to survive across eval's GC; a raw WORD carries no such
    ;; concern -- an immediate is a self-contained value and a pointer
    ;; WORD is exactly the address the box already lives at, unaffected
    ;; by whether it is read before or after this eval).  val-cdr is the
    ;; real CONS box and is already kept alive by the form.
    ;; Arity 6 (even): _pad6 makes arity even → no prologue sub rsp.
    (defun nl_sf_setq_evaled (eval-rc sym val-cdr env out _pad6)
      (if (= eval-rc 0)
          (nl_sf_setq_rest
           (nl_sf_setq_cdr_word val-cdr)
           sym env out)
        1))

    ;; val-form = nl_cons_car_ptr(val-cdr) already fetched as first arg.
    ;; Now eval val-form via nelisp_eval_call (extern-call FIRST ✓), carrying
    ;; the rooted val-cdr rather than a possibly materialised rest view.
    ;; Arity 6 (even): _pad6 makes arity even.
    (defun nl_sf_setq_val (val-form sym val-cdr env out _pad6)
      (nl_sf_setq_evaled
       (extern-call nelisp_eval_call val-form env out)
       sym val-cdr env out 0))

    ;; sym = nl_cons_car_ptr(args-pair) already fetched as first arg.
    ;; val-cdr = cdr(args-pair) already available.
    ;; Fetch val-form = nl_cons_car_ptr(val-cdr) (extern-call FIRST ✓).
    ;; nl_sf_setq_val is now arity 6; pass 0 as _pad6.
    ;; Arity 4 (even).
    (defun nl_sf_setq_sym (sym val-cdr env out)
      (nl_sf_setq_val
       (extern-call nl_cons_car_ptr val-cdr)
       sym val-cdr env out 0))

    ;; val-cdr = nl_cons_cdr_ptr(args-pair) already fetched as first arg.
    ;; Fetch sym = nl_cons_car_ptr(args-pair) (extern-call FIRST ✓).
    ;; Arity 4 (even).
    (defun nl_sf_setq_pair (val-cdr args-pair env out)
      (nl_sf_setq_sym
       (extern-call nl_cons_car_ptr args-pair)
       val-cdr env out))

    ;; Recursive step: cdr is the remaining (SYM VAL ...) cons list, now a
    ;; raw tagged WORD (perf/setq-pair-cdr-raw-word above) rather than a
    ;; materialised pointer -- `nl_val_tag' classifies it the same way
    ;; `sexp-tag' classified the old materialised view (see that
    ;; commentary).  If Nil → done, no allocation at all on this, the
    ;; overwhelmingly common, path.  Else CDR is already the exact
    ;; pointer a materialising call would have returned (a pointer WORD
    ;; IS that `*const Sexp'), so it is used unchanged both as the next
    ;; ARGS-PAIR and as the argument to `nl_cons_cdr_ptr' below.
    ;; Arity 4 (even).
    (defun nl_sf_setq_step (cdr env out _pad)
      (if (= (nl_val_tag cdr) 0)
          0
        (nl_sf_setq_pair
         (extern-call nl_cons_cdr_ptr cdr)
         cdr env out)))

    ;; Public entry: nl_sf_setq(args, env, out, _pad) → i64
    ;; Empty args → 0; else start first pair: cdr(args) FIRST ✓.
    ;; Arity 4 (even): no prologue sub rsp → no double-sub misalignment.
    (defun nl_sf_setq (args env out _pad)
      (if (= (sexp-tag args) 0)
          0
        (nl_sf_setq_pair
         (extern-call nl_cons_cdr_ptr args)
         args env out))))

  "AOT source for `nl_sf_setq' (eval/special_forms.rs sf_setq → elisp).

Eight defuns (seq form).  CPS chain with one extern-call per step.

Entry chain:
  nl_sf_setq → (cdr FIRST) → nl_sf_setq_pair
  → (car FIRST) → nl_sf_setq_sym
  → (car(val-cdr) FIRST) → nl_sf_setq_val
  → (nelisp_eval_call FIRST) → nl_sf_setq_evaled
  → (cdr(val-cdr) FIRST) → nl_sf_setq_rest
  → (nl_env_set_value FIRST) → nl_sf_setq_set_rc
  → nl_sf_setq_step (recurse)

Each extern-call is argument 0 at its call site → rsp 0 mod 16 ✓.")

(provide 'nelisp-cc-sf-setq)

;;; nelisp-cc-sf-setq.el ends here
