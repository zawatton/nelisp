;;; nelisp-cc-bind-formals.el --- Phase 47 nl_bind_formals_impl swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 parallel implementation of `bind_formals_impl' in
;; `build-tool/src/eval/special_forms.rs' (~70 LOC Rust).
;;
;; The original Rust body:
;;   fn bind_formals_impl(formals, args, env):
;;       names = list_elements(formals)?
;;       required = count of names before &optional / &rest
;;       mode = Required, idx = 0, saw_rest = false, consumed_rest = false
;;       for each formal Symbol name:
;;           "&optional" → mode = Optional (error if mode was Rest)
;;           "&rest"     → mode = Rest, saw_rest = true (error if saw_rest)
;;           Required    → bind args[idx]?, idx += 1 (error if idx >= len)
;;           Optional    → bind args[idx] or Nil; idx += 1 if available
;;           Rest        → bind list(args[idx..]), consumed_rest = true
;;                         (error if consumed_rest already set)
;;       idx < args.len() && !consumed_rest → WrongNumberOfArguments
;;       saw_rest && !consumed_rest        → WrongType (dangling &rest)
;;
;; ABI externs (build-tool/src/eval/special_forms.rs):
;;   nl_bf_precompute(formals, args) -> i64
;;     Returns initial packed state word:
;;       bits  0-1:  mode  (0=Required, 1=Optional, 2=Rest)  — all 0 init
;;       bit   2:    saw-rest                                 — 0 init
;;       bit   3:    consumed-rest                            — 0 init
;;       bits  4-19: idx        (16 bits)                     — 0 init
;;       bits 20-35: args-len   (16 bits, cached)
;;       bits 36+:   required   (16 bits, cached for arity errors)
;;     Result has bits 0-19 = 0 and bits 20+ set from counts.
;;   nl_bf_formal_tag(name_ptr) -> i64
;;     0=regular symbol, 1=&optional, 2=&rest, -1=non-Symbol.
;;   nl_bf_args_nth_ptr(args, idx) -> *const Sexp as i64
;;     Returns ptr to args[idx], or 0 if idx >= len.
;;   nl_bf_bind_sym(env, name_ptr, val_ptr) -> i64
;;     env.bind_local(name, *val_ptr).  Returns 0.
;;   nl_bf_bind_optional(env, name_ptr, args_ptr, idx) -> i64
;;     Optional bind: args[idx] or Nil.  Returns new idx.
;;   nl_bf_bind_rest(env, name_ptr, args_ptr, idx) -> i64
;;     Rest bind: list(args[idx..]).  Returns 0.
;;   nl_bf_err_arity(env, required, got) -> i64
;;     Stashes WrongNumberOfArguments, returns 1.
;;   nl_bf_err_type(env, name_ptr) -> i64
;;     Stashes WrongType, returns 1.
;;   nl_bf_err_dangling_rest(env) -> i64
;;     Stashes WrongType for bare &rest without following symbol, returns 1.
;;   nl_cons_car_ptr / nl_cons_cdr_ptr — standard cons walkers.
;;
;; NOTE on Phase 47 shift syntax: `ash' is NOT a value-expr in Phase 47.
;; Use `(sar x n)' for arithmetic right shift (= shift right by positive n)
;; and `(shl x n)' for left shift.  Negative shift amounts not allowed.
;;
;; State bit extraction:
;;   mode          = logand state 3
;;   saw-rest      = logand (sar state 2) 1
;;   consumed-rest = logand (sar state 3) 1
;;   idx           = logand (sar state 4) 65535
;;   args-len      = logand (sar state 20) 65535
;;   required      = sar state 36
;;
;; State update helpers (pure arithmetic):
;;   Set mode to M (M=1 or M=2):
;;     new-state = logior (logand state (- 0 4)) M
;;   Set saw-rest (bit 2) and mode to 2:
;;     new-state = logior (logand state (- 0 16)) 6
;;   Set consumed-rest (bit 3):
;;     new-state = logior state 8
;;   Update idx to new-idx:
;;     new-state = logior (logand state (- 0 1048576))  ; keep bits 20+
;;                        (logand state 15)              ; keep bits 0-3
;;                        (shl new-idx 4)               ; new idx
;;
;; CPS chain (17 defuns, seq form):
;;   nl_bf_ret1             (v _p1 _p2 _p3 _p4 _p5)               arity 6
;;   nl_bf_end_dangling     (env state _p2 _p3 _p4 _p5)            arity 6
;;   nl_bf_end_over_args    (env state _p2 _p3 _p4 _p5)            arity 6
;;   nl_bf_end              (env state _p2 _p3 _p4 _p5)            arity 6
;;   nl_bf_optional         (new-idx cdr-f args env state _pad)    arity 6
;;   nl_bf_after_required   (rc cdr-f args env new-state _pad)     arity 6
;;   nl_bf_arity_err2       (env required got _p3 _p4 _p5)         arity 6
;;   nl_bf_required         (arg-ptr name-ptr cdr-f args env state) arity 6
;;   nl_bf_rest             (rc cdr-f args env state _pad)         arity 6
;;   nl_bf_tag              (tag name-ptr cdr-f args env state)    arity 6
;;   nl_bf_dispatch         (cdr-f name-ptr args env state _p6)    arity 6
;;   nl_bf_get_cdr          (name-ptr formals args env state _p6)  arity 6
;;   nl_bf_loop             (formals args env state _p5 _p6)       arity 6
;;   nl_bf_start            (state formals args env)               arity 4
;;   nl_bind_formals_impl   (formals args env _pad)                arity 4

;;; Code:

(defconst nelisp-cc-bind-formals--source
  '(seq

    ;; Identity wrapper: returns first argument unchanged.
    ;; Used by error trampolines where extern-call result IS the return value.
    ;; Arity 6 (even).
    (defun nl_bf_ret1 (v _p1 _p2 _p3 _p4 _p5)
      v)

    ;; End-of-formals: dangling &rest (saw-rest=1, consumed-rest=0).
    ;; Arity 6 (even).
    (defun nl_bf_end_dangling (env state _p2 _p3 _p4 _p5)
      (nl_bf_ret1
       (extern-call nl_bf_err_dangling_rest env)
       0 0 0 0 0))

    ;; End-of-formals: too many args (idx < args-len, consumed-rest=0).
    ;; expected = idx (how many we accepted), got = args-len.
    ;; Arity 6 (even).
    (defun nl_bf_end_over_args (env state _p2 _p3 _p4 _p5)
      (nl_bf_ret1
       (extern-call nl_bf_err_arity env
                    (logand (sar state 4) 65535)    ; idx
                    (logand (sar state 20) 65535))  ; args-len
       0 0 0 0 0))

    ;; End-of-formals check.
    ;; consumed-rest=0 AND idx < args-len → too many args.
    ;; saw-rest=1 AND consumed-rest=0 → dangling &rest.
    ;; Otherwise: success → 0.
    ;; Arity 6 (even).
    (defun nl_bf_end (env state _p2 _p3 _p4 _p5)
      (if (= (logand (sar state 3) 1) 0)
          ;; !consumed-rest: check for over-applied args
          (if (< (logand (sar state 4) 65535)
                 (logand (sar state 20) 65535))
              ;; idx < args-len: too many args
              (nl_bf_end_over_args env state 0 0 0 0)
            ;; idx >= args-len: check dangling &rest
            (if (= (logand (sar state 2) 1) 1)
                (nl_bf_end_dangling env state 0 0 0 0)
              0))
        ;; consumed-rest=1: no over-args possible, success
        0))

    ;; Optional mode: nl_bf_bind_optional bound arg (or Nil) and returned
    ;; new-idx.  Update idx in state and continue loop.
    ;; Arity 6 (even).
    (defun nl_bf_optional (new-idx cdr-formals args env state _pad)
      (nl_bf_loop cdr-formals args env
                  (logior (logior (logand state (- 0 1048576))  ; keep bits 20+
                                  (logand state 15))            ; keep bits 0-3
                          (shl new-idx 4))                       ; new idx
                  0 0))

    ;; After nl_bf_bind_sym in Required mode (rc always 0).
    ;; Continue loop with updated state.
    ;; Arity 6 (even).
    (defun nl_bf_after_required (rc cdr-formals args env new-state _pad)
      (nl_bf_loop cdr-formals args env new-state 0 0))

    ;; Required mode arity error trampoline.
    ;; Calls nl_bf_err_arity as FIRST arg and returns its result (1).
    ;; Arity 6 (even).
    (defun nl_bf_arity_err2 (env required got _p3 _p4 _p5)
      (nl_bf_ret1
       (extern-call nl_bf_err_arity env required got)
       0 0 0 0 0))

    ;; Required mode: arg-ptr from nl_bf_args_nth_ptr.
    ;; If 0 → not enough args → arity error.
    ;; Else → bind *arg-ptr to name, increment idx, continue.
    ;; Arity 6 (even).
    (defun nl_bf_required (arg-ptr name-ptr cdr-formals args env state)
      (if (= arg-ptr 0)
          ;; Not enough required args.
          (nl_bf_arity_err2 env
                            (sar state 36)                  ; required count
                            (logand (sar state 20) 65535)   ; args-len (= got)
                            0 0 0)
        ;; Bind *arg-ptr to name; advance idx.
        (nl_bf_after_required
         (extern-call nl_bf_bind_sym env name-ptr arg-ptr)
         cdr-formals args env
         (logior (logior (logand state (- 0 1048576))               ; keep bits 20+
                         (logand state 15))                         ; keep bits 0-3
                 (shl (+ (logand (sar state 4) 65535) 1) 4))       ; idx + 1
         0)))

    ;; Rest mode: nl_bf_bind_rest built and bound list(args[idx..]).
    ;; rc = 0 always.  Set consumed-rest (bit 3), continue loop.
    ;; Arity 6 (even).
    (defun nl_bf_rest (rc cdr-formals args env state _pad)
      (nl_bf_loop cdr-formals args env (logior state 8) 0 0))

    ;; Dispatch on formal tag.
    ;; tag = -1: non-Symbol formal → WrongType error.
    ;; tag =  1: &optional marker → switch mode to 1 (error if mode=2).
    ;; tag =  2: &rest marker → switch mode to 2, set saw-rest (error if dup).
    ;; tag =  0: regular symbol → dispatch on current mode.
    ;;   mode 0 (Required) → nl_bf_required (arg-ptr fetch FIRST).
    ;;   mode 1 (Optional) → nl_bf_optional (bind_optional FIRST).
    ;;   mode 2 (Rest)     → nl_bf_rest (bind_rest FIRST) if !consumed-rest.
    ;;                        WrongType error if consumed-rest already set.
    ;; Arity 6 (even).
    (defun nl_bf_tag (tag name-ptr cdr-formals args env state)
      (if (= tag -1)
          ;; Non-Symbol formal.
          (nl_bf_ret1
           (extern-call nl_bf_err_type env name-ptr)
           0 0 0 0 0)
        (if (= tag 1)
            ;; &optional marker.
            (if (= (logand state 3) 2)
                ;; &optional after &rest: error.
                (nl_bf_ret1
                 (extern-call nl_bf_err_type env name-ptr)
                 0 0 0 0 0)
              ;; Switch to Optional mode (1): clear bits 0-1, set 1.
              ;; (- 0 4) = -4 = keeps bits 2+ intact.
              (nl_bf_loop cdr-formals args env
                          (logior (logand state (- 0 4)) 1)
                          0 0))
          (if (= tag 2)
              ;; &rest marker.
              (if (= (logand (sar state 2) 1) 1)
                  ;; Double &rest: error.
                  (nl_bf_ret1
                   (extern-call nl_bf_err_type env name-ptr)
                   0 0 0 0 0)
                ;; Switch to Rest mode (2) and set saw-rest (bit 2).
                ;; Value 6 = 0b110 = mode:2, saw-rest:1.
                ;; (- 0 16) = -16 keeps bits 4+ (idx, args-len, required).
                (nl_bf_loop cdr-formals args env
                            (logior (logand state (- 0 16)) 6)
                            0 0))
            ;; tag = 0: regular binding symbol.  Dispatch on mode.
            (if (= (logand state 3) 0)
                ;; Required mode: fetch arg ptr as FIRST.
                (nl_bf_required
                 (extern-call nl_bf_args_nth_ptr args (logand (sar state 4) 65535))
                 name-ptr cdr-formals args env state)
              (if (= (logand state 3) 1)
                  ;; Optional mode: bind_optional handles Nil, returns new-idx.
                  (nl_bf_optional
                   (extern-call nl_bf_bind_optional env name-ptr args
                                (logand (sar state 4) 65535))
                   cdr-formals args env state 0)
                ;; Rest mode (mode = 2).
                (if (= (logand (sar state 3) 1) 1)
                    ;; consumed-rest already set: extra sym after &rest → error.
                    (nl_bf_ret1
                     (extern-call nl_bf_err_type env name-ptr)
                     0 0 0 0 0)
                  ;; Bind list(args[idx..]) to name atomically.
                  (nl_bf_rest
                   (extern-call nl_bf_bind_rest env name-ptr args
                                (logand (sar state 4) 65535))
                   cdr-formals args env state 0))))))))

    ;; cdr-formals fetched as FIRST; now classify the formal symbol.
    ;; Arity 6 (even).
    (defun nl_bf_dispatch (cdr-formals name-ptr args env state _p6)
      (nl_bf_tag
       (extern-call nl_bf_formal_tag name-ptr)
       name-ptr cdr-formals args env state))

    ;; car-ptr (= name-ptr) fetched as FIRST; now fetch cdr of formals.
    ;; Arity 6 (even).
    (defun nl_bf_get_cdr (name-ptr formals args env state _p6)
      (nl_bf_dispatch
       (extern-call nl_cons_cdr_ptr formals)
       name-ptr args env state 0))

    ;; Main loop entry.
    ;; formals: remaining formals cons list.
    ;; Nil (sexp-tag 0) → end of loop.
    ;; Cons (sexp-tag 7) → get car (name-ptr) as FIRST, advance.
    ;; Arity 6 (even).
    (defun nl_bf_loop (formals args env state _p5 _p6)
      (if (= (sexp-tag formals) 0)
          (nl_bf_end env state 0 0 0 0)
        (nl_bf_get_cdr
         (extern-call nl_cons_car_ptr formals)
         formals args env state 0)))

    ;; After nl_bf_precompute: state = initial packed state word.
    ;; idx=0, mode=0 (Required), flags=0, args-len and required set.
    ;; Enter the main loop.
    ;; Arity 4 (even).
    (defun nl_bf_start (state formals args env)
      (nl_bf_loop formals args env state 0 0))

    ;; Public entry: nl_bind_formals_impl(formals, args, env, _pad) -> i64
    ;; formals: *const Sexp — cons list of formal parameter symbols.
    ;; args:    *const Sexp — cons list of evaluated argument values.
    ;; env:     *mut c_void — &mut Env.
    ;; _pad:    unused (keeps arity even = 4).
    ;; Returns: 0=Ok (all formals bound), 1=Err (error stashed in env).
    ;; Arity 4 (even).
    (defun nl_bind_formals_impl (formals args env _pad)
      (nl_bf_start
       (extern-call nl_bf_precompute formals args)
       formals args env)))

  "Phase 47 source for `nl_bind_formals_impl' (Stage 1 parallel
implementation of `bind_formals_impl' in special_forms.rs, ~70 LOC).

15 defuns (seq form) — CPS chain for the Required/Optional/Rest
formals-binding state machine with full arity/type error signaling.

State word (packed i64):
  bits  0-1:  mode (0=Required, 1=Optional, 2=Rest)
  bit   2:    saw-rest
  bit   3:    consumed-rest
  bits  4-19: idx (current binding index, 16 bits)
  bits 20-35: args-len (16 bits, from nl_bf_precompute)
  bits 36+:   required (16 bits, for arity error messages)

Phase 47 shift ops: `sar' (signed right shift) / `shl' (left shift).
`ash' is not a Phase 47 value-expr; all shifts are translated:
  (ash x -n) → (sar x n), (ash x n) → (shl x n) for n > 0.

New ABI externs in build-tool/src/eval/special_forms.rs:
  nl_bf_precompute   (formals, args)        -> i64  initial state word
  nl_bf_formal_tag   (name_ptr)             -> i64  0/1/2/-1
  nl_bf_args_nth_ptr (args, idx)            -> i64  ptr or 0
  nl_bf_bind_sym     (env, name, val)       -> i64  bind_local (0)
  nl_bf_bind_optional(env, name, args, idx) -> i64  optional bind, new idx
  nl_bf_bind_rest    (env, name, args, idx) -> i64  rest bind (0)
  nl_bf_err_arity    (env, required, got)   -> i64  stash+1
  nl_bf_err_type     (env, name_ptr)        -> i64  stash+1
  nl_bf_err_dangling_rest (env)             -> i64  stash+1
  nl_cons_car_ptr / nl_cons_cdr_ptr         standard cons walkers

All defuns have even arity ≤ 6 → body-entry rsp ≡ 0 mod 16 ✓.
Every extern-call is argument 0 at its call site ✓.")

(provide 'nelisp-cc-bind-formals)

;;; nelisp-cc-bind-formals.el ends here
