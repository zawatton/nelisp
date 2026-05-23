;;; nelisp-cc-env-shim-op.el --- Doc 86 §86.4 env-shim OP dispatcher Phase 47  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 86 §86.4 — Phase 47 reimplementation of the
;; `nelisp--env-globals-op' read/clear/predicate dispatch arms.
;; Replaces 7 of the 10 macro arms in
;; `build-tool/src/eval/env_shim.rs::bi_globals_op', letting the Rust
;; body shrink to a thin arg-check + Phase-47 call + error-mapping.
;;
;; Handled OPs (returns i64 result code + writes Sexp into result-slot):
;;   get-value     → 0 = unbound variable, 1 = hit (value in result-slot)
;;   get-function  → -1 = unbound function, 1 = hit (fn in result-slot)
;;   is-bound      → 1 (writes T or Nil to result-slot)
;;   is-fbound     → 1 (writes T or Nil to result-slot)
;;   is-constant   → 1 (writes T or Nil to result-slot)
;;   clear-value   → 1 (clears slot 0, clones sym to result-slot)
;;   clear-function→ 1 (clears slot 1, clones sym to result-slot)
;;   anything else → -2 (set-value / set-function / set-constant handled
;;                       by Rust; capture-lexical also handled by Rust)
;;
;; Signature of `nelisp_env_shim_op':
;;   op-ptr:          *const Sexp — the OP symbol (e.g. Symbol("get-value"))
;;   mirror-ptr:      *const Sexp — env globals record
;;   sym-ptr:         *const Sexp — the symbol name Sexp
;;   unbound-ptr:     *const Sexp — env unbound_marker
;;   result-slot:     *mut Sexp   — output slot (caller-owned, pre-filled Nil)
;;   vec-scratch:     *mut Sexp   — unused scratch slot (alignment pad)
;;   Returns: i64 as above.
;;
;; Arity conventions:
;;   All defuns are even-arity (6 or 4) so body-entry rsp ≡ 0 mod 16,
;;   matching the alignment invariant for `extern-call' / PLT call sites.
;;   Odd-arity shims use _pad parameters.
;;
;; ABI deps:
;;   nelisp_mirror_lookup_entry  — hit/miss check (returns 0 = miss)
;;   nelisp_mirror_lookup_value  — copies value to result-slot (hit path)
;;   nelisp_mirror_lookup_function — copies function cell to result-slot
;;   nelisp_mirror_is_bound      — predicate (returns 0 or 1)
;;   nelisp_mirror_is_fbound     — predicate (returns 0 or 1)
;;   nelisp_mirror_is_constant   — predicate (returns 0 or 1)
;;   nelisp_mirror_clear_value   — sets slot 0 to unbound_marker
;;   nelisp_mirror_clear_function — sets slot 1 to unbound_marker
;;   nl_sexp_clone_into          — refcount-aware Sexp copy
;;   sexp-write-nil, sexp-write-t — write Nil/T tag into slot
;;   symbol-name-eq              — inline tag+len+byte compare (G1 op)

;;; Code:

(defconst nelisp-cc-env-shim-op--source
  '(seq

    ;; ---- CPS helpers -----------------------------------------------

    ;; nelisp_env_shim_check_lookup
    ;;
    ;; Called after nelisp_mirror_lookup_entry to check entry existence.
    ;; entry-ptr: result of nelisp_mirror_lookup_entry (0 = miss, else hit).
    ;; mirror-ptr / sym-ptr / unbound-ptr: forwarded args (unused now;
    ;;   kept for arity stability with the dispatcher call sites).
    ;; result-slot: *mut Sexp — written on hit.
    ;; miss-code: i64 to return on miss (0 for get-value, -1 for get-function).
    ;;
    ;; R11a CSE-hoist: previous shape called `nelisp_mirror_lookup_value'
    ;; on the hit branch (= a wrapper that re-hashes via `lookup_entry'
    ;; + reads slot 0 via record-slot-ref).  We already hold the entry
    ;; pointer from the dispatcher's outer `lookup_entry' call, so call
    ;; `record-slot-ref' on entry-ptr slot 0 directly — bypassing the
    ;; wrapper and its redundant FNV-1a hash + bucket walk.  2 hashes →
    ;; 1 per get-value/get-function call; semantics identical
    ;; (= `record-slot-ref' uses the same `nl_sexp_clone_into').
    ;;
    ;; Arity 6 (even) — body-entry rsp ≡ 0 mod 16.  `record-slot-ref' is
    ;; a 3-arg helper whose extern-call invocation is arg 0 of `and' ✓.
    (defun nelisp_env_shim_check_lookup
        (entry-ptr mirror-ptr sym-ptr result-slot miss-code _pad)
      (if (= entry-ptr 0)
          miss-code
        (and (record-slot-ref entry-ptr 0 result-slot) 1)))

    ;; nelisp_env_shim_check_lookup_fn
    ;;
    ;; Same as nelisp_env_shim_check_lookup but for the function cell
    ;; (= slot 1 instead of slot 0).  Same R11a hoist rationale.
    ;; Arity 6 (even).
    (defun nelisp_env_shim_check_lookup_fn
        (entry-ptr mirror-ptr sym-ptr result-slot miss-code _pad)
      (if (= entry-ptr 0)
          miss-code
        (and (record-slot-ref entry-ptr 1 result-slot) 1)))

    ;; nelisp_env_shim_write_bool
    ;;
    ;; Writes T or Nil to result-slot based on bool-val (0=Nil, else T).
    ;; Returns 1 always.
    ;; Arity 4 (even).
    (defun nelisp_env_shim_write_bool (bool-val result-slot _pad _pad2)
      (if (= bool-val 0)
          (and (sexp-write-nil result-slot) 1)
        (and (sexp-write-t result-slot) 1)))

    ;; nelisp_env_shim_clear_after
    ;;
    ;; After mirror_clear_*, clone sym-ptr into result-slot (return sym).
    ;; _clear-rv: unused return value of the clear call.
    ;; Returns 1.
    ;; Arity 4 (even).
    (defun nelisp_env_shim_clear_after (_clear-rv sym-ptr result-slot _pad)
      (and (extern-call nl_sexp_clone_into sym-ptr result-slot) 1))

    ;; ---- Main dispatcher -------------------------------------------

    ;; nelisp_env_shim_op
    ;;
    ;; Dispatch on op-ptr symbol name.  Handles the 7 read/clear/pred
    ;; arms.  Returns -2 for any OP not handled here (= set-value /
    ;; set-function / set-constant / capture-lexical — handled by Rust).
    ;;
    ;; Parameter layout:
    ;;   op-ptr      (arg 0 / rdi): *const Sexp — OP symbol
    ;;   mirror-ptr  (arg 1 / rsi): *const Sexp — env globals record
    ;;   sym-ptr     (arg 2 / rdx): *const Sexp — symbol name
    ;;   unbound-ptr (arg 3 / rcx): *const Sexp — unbound_marker
    ;;   result-slot (arg 4 / r8):  *mut Sexp   — output slot
    ;;   vec-scratch (arg 5 / r9):  *mut Sexp   — unused pad (arity alignment)
    ;;
    ;; Arity 6 (even) — body-entry rsp ≡ 0 mod 16.  All sub-call sites
    ;; use CPS helpers that forward lookup results through arg 0, keeping
    ;; the stack state well-defined before each PLT call.
    ;;
    ;; `symbol-name-eq' is inline (no PLT call) so `(if (= (symbol-name-eq
    ;; op-ptr "...") 1) ...)' is safe even inside a chain of `if' arms.
    (defun nelisp_env_shim_op
        (op-ptr mirror-ptr sym-ptr unbound-ptr result-slot vec-scratch)

      ;; get-value: lookup value cell; 0=unbound-var, 1=hit.
      (if (= (symbol-name-eq op-ptr "get-value") 1)
          (nelisp_env_shim_check_lookup
           (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
           mirror-ptr sym-ptr result-slot 0 0)

        ;; get-function: lookup function cell; -1=unbound-fn, 1=hit.
        (if (= (symbol-name-eq op-ptr "get-function") 1)
            (nelisp_env_shim_check_lookup_fn
             (extern-call nelisp_mirror_lookup_entry mirror-ptr sym-ptr)
             mirror-ptr sym-ptr result-slot -1 0)

          ;; is-bound: mirror_is_bound → write T/Nil.
          (if (= (symbol-name-eq op-ptr "is-bound") 1)
              (nelisp_env_shim_write_bool
               (extern-call nelisp_mirror_is_bound mirror-ptr sym-ptr unbound-ptr)
               result-slot 0 0)

            ;; is-fbound: mirror_is_fbound → write T/Nil.
            (if (= (symbol-name-eq op-ptr "is-fbound") 1)
                (nelisp_env_shim_write_bool
                 (extern-call nelisp_mirror_is_fbound mirror-ptr sym-ptr unbound-ptr)
                 result-slot 0 0)

              ;; is-constant: mirror_is_constant → write T/Nil.
              (if (= (symbol-name-eq op-ptr "is-constant") 1)
                  (nelisp_env_shim_write_bool
                   (extern-call nelisp_mirror_is_constant mirror-ptr sym-ptr)
                   result-slot 0 0)

                ;; clear-value: clear slot 0, return sym.
                (if (= (symbol-name-eq op-ptr "clear-value") 1)
                    (nelisp_env_shim_clear_after
                     (extern-call nelisp_mirror_clear_value mirror-ptr sym-ptr unbound-ptr)
                     sym-ptr result-slot 0)

                  ;; clear-function: clear slot 1, return sym.
                  (if (= (symbol-name-eq op-ptr "clear-function") 1)
                      (nelisp_env_shim_clear_after
                       (extern-call nelisp_mirror_clear_function mirror-ptr sym-ptr unbound-ptr)
                       sym-ptr result-slot 0)

                    ;; Anything else (set-*, capture-lexical): Rust handles.
                    -2))))))))
    )
  "Phase 47 source for Doc 86 §86.4 `nelisp_env_shim_op'.

CPS-dispatch over 7 of the 10 `nelisp--env-globals-op' arms.
The 3 remaining arms (set-value / set-function / set-constant)
stay Rust-side because they require the `_or_insert' auto-vivify
path which needs a scratch vector that the Rust safe wrapper builds.
`capture-lexical' stays Rust because it calls `Env::capture_lexical'
directly.

Return code contract:
  1  = success (result Sexp written to result-slot)
  0  = unbound variable (get-value miss)
  -1 = unbound function (get-function miss)
  -2 = OP not handled here (Rust fallback)")

(provide 'nelisp-cc-env-shim-op)

;;; nelisp-cc-env-shim-op.el ends here
