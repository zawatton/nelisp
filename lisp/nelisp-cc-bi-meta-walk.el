;;; nelisp-cc-bi-meta-walk.el --- Wave A26 manifest-vector iteration kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A26 (Phase 47 self-application — final wave, manifest walker
;; Phase 47 native).  Replaces the elisp `while' / `aref' iteration of
;; `compile-elisp-objects-meta--walk' with a Phase 47-compiled
;; tail-recursive walker that batch-evaluates the per-entry
;; `nelisp_meta_should_rebuild' decision and packs the result into a
;; single i64 bitmask returned to the elisp driver via the standalone
;; NeLisp interpreter's `nl-jit-call-out-2' bridge.
;;
;; Architectural milestone.  The chain
;;
;;   elisp driver
;;     -> nelisp_meta_walk          (= A26 — this file)
;;       -> nelisp_meta_should_rebuild  (= A25.2 — meta-driver)
;;         -> nelisp_bi_syscall_stat_mtime (= A25.1-min)
;;           -> libc stat(2)        (= OS edge)
;;
;; is now Phase 47 native end-to-end up to the libc syscall edge.  No
;; elisp interpreter step participates in the per-entry up-to-date
;; check; the elisp driver only walks the returned bitmask to dispatch
;; the emit step (which itself is too heavy to Phase 47-compile in this
;; PoC scope — it lives one layer up in `nelisp-phase47-compile-to-
;; object').
;;
;; Bridge contract.  The standalone NeLisp interpreter's
;; `nl-jit-call-out-2' primitive resolves the kernel via
;; `dlsym(RTLD_DEFAULT, "nelisp_meta_walk")', casts to
;; `extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64', and
;; surfaces the slot contents as the call's return.  The walker writes
;; `Sexp::Int(bitmask)' into the result slot; elisp can then read it
;; via `(let ((bm (nl-jit-call-out-2 "nelisp_meta_walk" SRCS OUTS)))
;; ... iterate bits ...)'.
;;
;; Bitmask encoding.  Bit i of the returned i64 is set iff
;; `nelisp_meta_should_rebuild' decided the i-th entry needs rebuilding
;; (= 1 in its result slot).  PoC subset = 5 entries, so we only need
;; the low 5 bits.  i64 holds 64 entries; larger manifests would split
;; into multiple walker dispatches or switch to a vector-based return
;; (= out of scope for A26).
;;
;; Input vectors.  Two parallel vectors instead of a single
;; pair-vector: `srcs[i]' = `Sexp::Str' of the source path, `outs[i]'
;; = `Sexp::Str' of the output path.  This avoids `(* 2 i)' index
;; arithmetic in the walker (= Phase 47 supports `*' but the simpler
;; layout makes the iteration easier to read in the cross-`.o' debug
;; output produced by the assembler).  Vector lengths MUST match;
;; mismatches are caller-detected on the elisp side and never reach
;; the walker.
;;
;; Substrate composition:
;;
;;   §111.C  `vector-len' / `vector-ref-ptr'  — vector probe + element
;;            pointer extraction (cross-`.o' Phase 47 ops).
;;   §125.A  `alloc-bytes' / `dealloc-bytes'  — single 32-byte scratch
;;            slot for the per-entry `nelisp_meta_should_rebuild'
;;            result.  Allocated once outside the loop, reused on
;;            every iteration (= 0 bytes/iter overhead).
;;   §100.A  `extern-call' to A25.2
;;            `nelisp_meta_should_rebuild'  — cross-`.o' Phase 47
;;            dispatch into the should-rebuild kernel.
;;   §100    `sexp-int-unwrap' / `sexp-int-make' — read decision i64 +
;;            write final bitmask Sexp::Int.
;;   §100.D  `shl'                       — left-shift decision bit
;;            into bitmask position i.
;;   §100    `logior'                    — accumulate bit into mask.
;;
;; Function contract:
;;   srcs:        *const Sexp — `Sexp::Vec' of N `Sexp::Str' source paths.
;;   outs:        *const Sexp — `Sexp::Vec' of N `Sexp::Str' output paths.
;;                MUST be the same length as `srcs' (caller-enforced).
;;   result-slot: *mut Sexp  — caller-owned 32-byte slot.  Receives
;;                `Sexp::Int(bitmask)' where bit i = 1 iff entry i
;;                needs rebuilding.
;;   returns:     TRAMPOLINE_OK (= 0) in rax — the bridge reads the
;;                bitmask from the result slot.
;;
;; Linux-x86_64 only — same arch gate as the A25.2 / A25.1-min
;; parents.  Composes only existing Phase 47 grammar (= no new opcode,
;; no Rust extern added).

;;; Code:

(defconst nelisp-cc-bi-meta-walk--source
  '(seq
    ;; 3-arg side-effect sequencer — `(val _e1 _e2) -> val'.  Used by
    ;; the public entry to thread the `dealloc-bytes' cleanup of the
    ;; per-call scratch slot behind the result-slot write.  Odd arity
    ;; (3) — Phase 47 emits the rsp alignment pad via the `--needs-
    ;; align' branch around the outer caller's dispatch into this
    ;; entry.
    (defun nelisp_meta_walk_seq3 (val _e1 _e2) val)

    ;; 2-arg discard-and-return — `(_x ret) -> ret'.  Used by the
    ;; public entry to swap the outer driver's slot-pointer return for
    ;; TRAMPOLINE_OK (= 0) so the elisp interpreter's
    ;; `nl-jit-call-out-2' bridge sees the success rc and surfaces the
    ;; bitmask via the caller-owned RESULT-SLOT.  Mirror of A25.6's
    ;; `nelisp_meta_zero2' wrapper that the same trick used for the
    ;; should-rebuild kernel.  Even arity (2) — no rsp pad needed.
    (defun nelisp_meta_walk_zero2 (_x ret) ret)

    ;; 2-arg post-effect read — `(_ignored scratch) -> sexp-int-unwrap
    ;; scratch'.  Used by the walker iter to discard the extern-call's
    ;; return value (= 0 = TRAMPOLINE_OK from `nelisp_meta_should_
    ;; rebuild's outer `nelisp_meta_zero2' wrapper) and read the actual
    ;; 0/1 rebuild decision out of the scratch slot the kernel wrote
    ;; via `sexp-int-make'.  Even arity (2) — no rsp pad needed.
    ;;
    ;; This sequencer is REQUIRED because `nelisp_meta_should_rebuild's
    ;; public outer entry was deliberately built (A25.6) to return
    ;; TRAMPOLINE_OK (= 0) instead of the slot pointer, so the elisp
    ;; interpreter's `nl-jit-call-out-2' bridge could read the decision
    ;; out of the caller-owned result slot.  That same return-shape
    ;; means `(sexp-int-unwrap (extern-call nelisp_meta_should_rebuild
    ;; ...))' would dereference NULL and segfault — the walker has to
    ;; pull the decision out of the scratch slot directly instead.
    (defun nelisp_meta_walk_read_decision (_ignored scratch)
      (sexp-int-unwrap scratch))

    ;; 6-arg tail-recursive walker — for each i in [0, n) accumulates
    ;; `((should_rebuild srcs[i] outs[i]) << i) | acc' into `acc' and
    ;; recurses on `i+1'.  Returns the final accumulator (= the
    ;; bitmask).
    ;;
    ;; Even arity (6) — SysV AMD64 marshals all args via rdi / rsi /
    ;; rdx / rcx / r8 / r9 (= no stack spill).  The recursive call
    ;; reuses the same arg layout so each iteration is a single `call'
    ;; instruction plus 4 register moves (i / acc are the only args
    ;; that change per iteration; n / srcs / outs / scratch stay put).
    ;;
    ;; `(vector-ref-ptr SRCS I)' returns the `*const Sexp' inside the
    ;; vector's heap storage at index I; the bytes there are the i-th
    ;; entry's `Sexp::Str' record.  Passing that pointer to
    ;; `nelisp_meta_should_rebuild' satisfies the kernel's
    ;; `*const Sexp -> Sexp::Str' contract — the kernel's
    ;; `nelisp_cstr_from_sexp' indirection reads tag + payload from the
    ;; same 32-byte slot.  Refcount note: `vector-ref-ptr' does NOT
    ;; clone, so the kernel must not free the Sexp::Str (it doesn't —
    ;; the kernel only reads the bytes via `str-len' + `ptr-read-u8').
    ;;
    ;; Decision read: the extern-call returns 0 (= TRAMPOLINE_OK from
    ;; the A25.6 `nelisp_meta_zero2' wrapper), so we use the
    ;; `nelisp_meta_walk_read_decision' sequencer to discard that
    ;; return and read the actual 0/1 decision out of the scratch
    ;; slot the kernel wrote.
    (defun nelisp_meta_walk_iter (i n srcs outs scratch acc)
      (if (= i n)
          acc
        (nelisp_meta_walk_iter
         (+ i 1)
         n
         srcs
         outs
         scratch
         (logior
          acc
          (shl
           (nelisp_meta_walk_read_decision
            (extern-call nelisp_meta_should_rebuild
                         (vector-ref-ptr srcs i)
                         (vector-ref-ptr outs i)
                         scratch)
            scratch)
           i)))))

    ;; 4-arg outer driver — writes `Sexp::Int(bitmask)' into the
    ;; caller's result slot, then frees the per-call scratch slot.
    ;; The walker dispatches into `nelisp_meta_walk_iter' with `i = 0',
    ;; `n = (vector-len srcs)', and `acc = 0' (= empty bitmask).
    ;;
    ;; Even arity (4) — SysV AMD64 marshals (srcs, outs, scratch,
    ;; result-slot) in (rdi, rsi, rdx, rcx).  The `seq3' wrapper
    ;; sequences the dealloc-bytes effect behind the result write.
    (defun nelisp_meta_walk_outer (srcs outs scratch result-slot)
      (nelisp_meta_walk_seq3
       (sexp-int-make
        result-slot
        (nelisp_meta_walk_iter
         0 (vector-len srcs) srcs outs scratch 0))
       (dealloc-bytes scratch 32 8)
       0))

    ;; Public 3-arg entry — allocates a single 32-byte scratch slot
    ;; for the per-iteration `nelisp_meta_should_rebuild' result and
    ;; dispatches into the 4-arg outer driver.  Odd arity (3) — Phase
    ;; 47 emits the rsp alignment pad via the `--needs-align' branch.
    ;;
    ;; The 32-byte size matches `sizeof::<Sexp>()' (= same as the
    ;; A25.2 `nelisp_meta_should_rebuild' contract).  Align 8 matches
    ;; the slot's alignment requirement.  The scratch slot is reused
    ;; for every iteration (= 0 alloc/iter overhead); the inner
    ;; `nelisp_meta_should_rebuild' writes a fresh `Sexp::Int(0|1)' on
    ;; each call so the read after `sexp-int-unwrap' always sees the
    ;; current decision.
    ;;
    ;; Return value (= rax) is TRAMPOLINE_OK (= 0).  The
    ;; `nl-jit-call-out-2' bridge in the standalone NeLisp interpreter
    ;; casts the symbol's return value to `i64' and treats anything
    ;; other than 0 as TRAMPOLINE_ERR; the actual bitmask is
    ;; communicated via the caller-owned RESULT-SLOT (= `Sexp::Int
    ;; (bitmask)') which the bridge surfaces as the call's return.
    (defun nelisp_meta_walk (srcs outs result-slot)
      (nelisp_meta_walk_zero2
       (nelisp_meta_walk_outer
        srcs
        outs
        (alloc-bytes 32 8)
        result-slot)
       0)))
  "Phase 47 source for the Wave A26 manifest walker
`(nelisp_meta_walk SRCS OUTS RESULT-SLOT)'.

Six-entry `(seq DEFUN ...)' manifest:

- `nelisp_meta_walk_seq3 (val _e1 _e2) -> val' — 3-arg side-effect
  sequencer.  Threads `dealloc-bytes' cleanup behind the result-slot
  write.

- `nelisp_meta_walk_zero2 (_x ret) -> ret' — 2-arg discard-and-return.
  Swaps the outer driver's slot-pointer return for TRAMPOLINE_OK at
  the public entry (= the elisp `nl-jit-call-out-2' bridge convention).

- `nelisp_meta_walk_read_decision (_ignored scratch) -> i64' — 2-arg
  post-effect read.  Discards the extern-call return (= 0) and reads
  the 0/1 decision out of the scratch slot the kernel wrote.

- `nelisp_meta_walk_iter (i n srcs outs scratch acc) -> bitmask' —
  6-arg tail-recursive walker.  Per iteration: call A25.2
  `nelisp_meta_should_rebuild' with `(vector-ref-ptr srcs i)' +
  `(vector-ref-ptr outs i)' + the shared scratch slot, unwrap the
  result `Sexp::Int', left-shift into bit position i, OR into the
  accumulator, recurse on `i + 1'.  Stops when `i = n'.

- `nelisp_meta_walk_outer (srcs outs scratch result-slot) -> 0' —
  4-arg outer driver.  Computes the bitmask via the iter chain,
  writes `Sexp::Int(bitmask)' into the result slot, frees the
  scratch slot, returns TRAMPOLINE_OK.

- `nelisp_meta_walk (srcs outs result-slot) -> 0' — public 3-arg
  entry.  Allocates a single 32-byte scratch slot (reused across
  iterations) and dispatches into the outer driver.  Returns
  TRAMPOLINE_OK so the `nl-jit-call-out-2' bridge can surface the
  bitmask via the caller-owned RESULT-SLOT.

Composes only existing Phase 47 grammar — no new opcode:

- §111.C `vector-len' / `vector-ref-ptr' — vector probe + element
  pointer extraction (cross-`.o' Phase 47 ops).
- §125.A `alloc-bytes' / `dealloc-bytes' — single 32-byte scratch
  slot, reused per iteration.
- §100.A `extern-call' to A25.2 `nelisp_meta_should_rebuild' —
  cross-`.o' Phase 47 dispatch into the should-rebuild kernel.
- §100 `sexp-int-unwrap' / `sexp-int-make' — read decision i64 +
  write final bitmask Sexp::Int.
- §100.D `shl' — left-shift decision bit into mask position i.
- §100 `logior' — accumulate bit into bitmask.

Bitmask semantics: bit i = 1 iff entry i needs rebuilding (= the
kernel's 0/1 decision shape, lifted into a flat i64).  Supports up
to 64 entries in a single walker call.  Larger manifests (= the
production 208-entry walk) would split into multiple walker
dispatches or switch to a vector-based return, both out of scope
for A26 (= which targets the existing 5-entry PoC subset).

Refcount note: `vector-ref-ptr' returns a borrowed `*const Sexp'
inside the vector's heap storage.  The kernel
`nelisp_meta_should_rebuild' only reads bytes from that pointer
(via `nelisp_cstr_from_sexp' -> `str-len' + `ptr-read-u8') and
never frees it.  The vector itself stays owned by the elisp caller
across the bridge call.

Linux-x86_64 only — same arch gate as the A25.2 / A25.1-min
parents.")

(provide 'nelisp-cc-bi-meta-walk)

;;; nelisp-cc-bi-meta-walk.el ends here
