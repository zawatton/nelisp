;;; nelisp-cc-bi-meta-dispatch-loop.el --- Wave A30 per-entry dispatch loop AOT kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A30 (AOT self-application — per-entry dispatch loop).
;; Collapses the 212-iter elisp dispatch loop in
;; `compile-elisp-objects-meta--walk' (= the final cached-path
;; bottleneck after A26-A29 walker chain) into a single AOT
;; native call.
;;
;; Background.  After Wave A27 production cutover, the walker dispatch
;; into `nelisp_meta_walk' is AOT native (= 4 chunks × 64 entries,
;; ceil(212/64)=4 bridge calls), but the elisp dispatch loop that
;; iterates entries [0, 212) to do per-entry "emit or skip" decisions
;; still runs in the standalone NeLisp elisp interpreter.  Each iter
;; performs ~6 trivial ops (= aref / logand / cond / push) but the
;; interpreter dispatch overhead per op accumulates: 212 iter × ~6 ops
;; × ~300ms/op ≈ 6m30s observed on Debian VM cached-path runs.
;;
;; Wave A30 swaps this elisp loop for a AOT kernel
;; `nelisp_meta_dispatch_loop' that walks the per-chunk dirty + arch-
;; skip bitmask vectors, computes per-chunk `(dirty AND NOT arch-skip)'
;; emit-needed bitmasks + per-chunk `(NOT arch-skip)' accept-bitmasks,
;; and writes both into a caller-provided result vector while
;; accumulating the popcount of all accept bits into a result slot.
;; The elisp wrapper then iterates only the 4 chunk slots (= 4 elisp
;; iters) to dispatch the emit step on each chunk's set bits, instead
;; of iterating all 212 entries.
;;
;; Bitmask-only kernel (= no Sexp::Str cloning).  The kernel operates
;; on i64 bitmask values via the existing `vector-ref-ptr' +
;; `sexp-int-unwrap' read path and `vector-slot-set' + `sexp-int-make'
;; write path.  No string / cons / heap-allocated payload crosses the
;; bridge, so the kernel composes only the already-validated AOT
;; grammar from §111.C + §100.D + §125.A.
;;
;; Bridge contract.  The standalone NeLisp interpreter's
;; `nl-jit-call-out-2' bridge resolves `nelisp_meta_dispatch_loop' via
;; `dlsym(RTLD_DEFAULT, ...)' and casts to
;; `extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64'.
;; The bridge limits arity to 2 Sexp args + 1 result slot, so the
;; caller packs the 3 chunk vectors into a 2-vector input shape:
;;
;;   ARG1 = inputs-vec  = `[dirty-chunks-vec arch-skip-chunks-vec]'
;;   ARG2 = emit-chunks-vec  (preallocated output, written via
;;                            vector-slot-set per chunk)
;;   RESULT-SLOT = popcount-i64 (writeback)
;;
;; The kernel:
;;
;;   1. Reads `dirty-chunks   = inputs-vec[0]'  via vector-ref-ptr.
;;   2. Reads `arch-skip-chunks = inputs-vec[1]'  via vector-ref-ptr.
;;   3. Iterates chunks [0, (vector-len dirty-chunks)) tail-recursively.
;;   4. Per chunk c:
;;      - dirty[c]     = (sexp-int-unwrap (vector-ref-ptr dirty c))
;;      - arch-skip[c] = (sexp-int-unwrap (vector-ref-ptr arch-skip c))
;;      - emit[c]      = dirty[c] AND (arch-skip[c] XOR -1)  → written
;;                       into emit-chunks-vec[c] via vector-slot-set.
;;      - accept[c]    = arch-skip[c] XOR -1
;;      - popcount-acc += popcount(accept[c])
;;   5. Writes Sexp::Int(popcount-acc) into result-slot.
;;
;; Caller MUST pre-OR spillover bits (= bit positions ≥ chunk-len in
;; the final possibly-short chunk) into arch-skip-chunks[final-chunk]
;; so `(arch-skip XOR -1)' is already accept-masked.  This keeps the
;; arity at 2 input vecs (no separate valid-bits arg) and concentrates
;; the chunk-len math on the elisp side where the production
;; manifest's odd tail-length (212 mod 64 = 20) is known once at
;; construction time.
;;
;; Returns TRAMPOLINE_OK (= 0) so the bridge surfaces the popcount via
;; the caller-owned RESULT-SLOT (= Sexp::Int).  The per-chunk
;; emit-bitmasks land in the caller-provided emit-chunks-vec via
;; `vector-slot-set' (refcount-safe overwrite).
;;
;; Substrate composition:
;;
;;   §111.C  `vector-len' / `vector-ref-ptr' / `vector-slot-set' —
;;            vector probe + element pointer extraction + refcount-aware
;;            slot write.
;;   §100    `sexp-int-unwrap' / `sexp-int-make' — read i64 mask payload
;;            + materialise final-mask Sexp::Int in scratch slot for
;;            vector-slot-set.
;;   §100.D  `logand' / `logxor' — final-mask = dirty AND (arch-skip
;;            XOR -1); accept = (arch-skip XOR -1).
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — 32-byte scratch slot for
;;            materialising the final-mask Sexp::Int before
;;            `vector-slot-set' copies it into the chunk slot.
;;            Allocated per-chunk (= 4 alloc+dealloc total for the
;;            production manifest, negligible vs. 212 elisp ops).
;;
;; Popcount.  Implemented as a tail-recursive bit-strip walker (= Brian
;; Kernighan's algorithm: `n & (n-1)' clears the lowest set bit).  Each
;; iter strips one bit and recurses with count+1; stops when n=0.
;;
;; Linux-x86_64 only — same arch gate as the A25.2 / A26 parents.
;; Composes only existing AOT grammar (no new opcode, no Rust
;; extern added).  Net Rust LOC delta = 0.

;;; Code:

(defconst nelisp-cc-bi-meta-dispatch-loop--source
  '(seq
    ;; 3-arg side-effect sequencer — `(val _e1 _e2) -> val'.  Used by
    ;; the inner step to thread `dealloc-bytes' cleanup behind the
    ;; vector-slot-set write.  Odd arity (3) — AOT emits rsp pad.
    (defun nelisp_meta_dispatch_loop_seq3 (val _e1 _e2) val)

    ;; 2-arg discard-and-return — `(_x ret) -> ret'.  Used by the
    ;; public entry to swap the inner driver's slot-pointer return
    ;; for TRAMPOLINE_OK (= 0) so the elisp interpreter's
    ;; `nl-jit-call-out-2' bridge sees the success rc and surfaces the
    ;; popcount via the caller-owned RESULT-SLOT.  Even arity (2).
    (defun nelisp_meta_dispatch_loop_zero2 (_x ret) ret)

    ;; 2-arg tail-recursive popcount — Brian Kernighan's bit-strip
    ;; algorithm.  Per iter: `n & (n-1)' clears the lowest set bit;
    ;; recurse with count+1 until n=0.  Returns final count.  Even
    ;; arity (2).
    (defun nelisp_meta_dispatch_loop_popcount (n acc)
      (if (= n 0)
          acc
        (nelisp_meta_dispatch_loop_popcount
         (logand n (- n 1))
         (+ acc 1))))

    ;; 4-arg inner emit-step writer — writes the precomputed
    ;; final-mask Sexp::Int into a scratch slot, then
    ;; `vector-slot-set' the scratch into emit-vec[c].  Frees scratch
    ;; via the seq3 wrapper.  Returns 0.  Even arity (4).
    (defun nelisp_meta_dispatch_loop_emit_step_inner
        (c emit-vec scratch final-mask)
      (nelisp_meta_dispatch_loop_seq3
       (vector-slot-set emit-vec c (sexp-int-make scratch final-mask))
       (dealloc-bytes scratch 32 8)
       0))

    ;; 5-arg emit-step driver — allocates a fresh 32-byte scratch slot
    ;; per chunk, computes final-mask = dirty AND (arch-skip XOR -1)
    ;; from the per-chunk i64 values, and dispatches into the inner
    ;; writer.  Returns 0 so it composes additively into the iter
    ;; walker's seq3 sequencer.  Odd arity (5) — AOT emits rsp pad.
    (defun nelisp_meta_dispatch_loop_emit_step
        (c dirty-vec arch-skip-vec emit-vec dirty-val)
      (nelisp_meta_dispatch_loop_emit_step_inner
       c
       emit-vec
       (alloc-bytes 32 8)
       (logand
        dirty-val
        (logxor
         (sexp-int-unwrap (vector-ref-ptr arch-skip-vec c))
         -1))))

    ;; 6-arg tail-recursive per-chunk walker.  Reads dirty[c] +
    ;; arch-skip[c] once per iter; computes:
    ;;
    ;;   chunk-popcount = popcount(arch-skip[c] XOR -1)
    ;;   new-acc        = acc + chunk-popcount
    ;;   emit-step side-effect: writes (dirty[c] AND (arch-skip[c] XOR
    ;;                          -1)) Sexp::Int into emit-vec[c].
    ;;
    ;; The seq3 wrapper threads the emit-step side-effect behind the
    ;; new-acc value so the recursive call sees the popcount-updated
    ;; accumulator while vector-slot-set has already fired.  Returns
    ;; final accumulator when c = total.  Even arity (6) — register-only
    ;; under SysV AMD64.
    (defun nelisp_meta_dispatch_loop_iter
        (c total dirty-vec arch-skip-vec emit-vec acc)
      (if (= c total)
          acc
        (nelisp_meta_dispatch_loop_iter
         (+ c 1)
         total
         dirty-vec
         arch-skip-vec
         emit-vec
         (nelisp_meta_dispatch_loop_seq3
          ;; Value-form: new-acc = old-acc + popcount(arch-skip XOR -1).
          ;; Caller pre-OR'd spillover bits into arch-skip[final-chunk]
          ;; so the XOR -1 already accept-masks the chunk.
          (+ acc
             (nelisp_meta_dispatch_loop_popcount
              (logxor
               (sexp-int-unwrap (vector-ref-ptr arch-skip-vec c))
               -1)
              0))
          ;; Side-effect: write final-mask Sexp::Int into emit-vec[c].
          (nelisp_meta_dispatch_loop_emit_step
           c
           dirty-vec
           arch-skip-vec
           emit-vec
           (sexp-int-unwrap (vector-ref-ptr dirty-vec c)))
          ;; Pad — keeps seq3 arity at 3.  Discarded.
          0))))

    ;; 4-arg outer driver — given the input chunk vecs already
    ;; unpacked from the bridge's 2-Sexp arg shape and the
    ;; caller-owned RESULT-SLOT, sets up the iter walker with acc=0
    ;; and writes Sexp::Int(popcount) into RESULT-SLOT.  Even arity (4).
    (defun nelisp_meta_dispatch_loop_outer
        (dirty-vec arch-skip-vec emit-vec result-slot)
      (sexp-int-make
       result-slot
       (nelisp_meta_dispatch_loop_iter
        0
        (vector-len dirty-vec)
        dirty-vec
        arch-skip-vec
        emit-vec
        0)))

    ;; Public 3-arg entry — `(inputs-vec emit-vec result-slot)'.
    ;; INPUTS-VEC is a 2-element Sexp::Vec packed by the elisp caller:
    ;;
    ;;   inputs-vec[0] = dirty-chunks-vec      (Sexp::Vec of i64 chunks)
    ;;   inputs-vec[1] = arch-skip-chunks-vec  (Sexp::Vec of i64 chunks)
    ;;
    ;; EMIT-VEC is preallocated by the caller to length = chunk-count;
    ;; written via vector-slot-set with final-mask Sexp::Int per chunk.
    ;;
    ;; RESULT-SLOT is the bridge's implicit 3rd ptr; receives
    ;; Sexp::Int(popcount-of-accepted-entries) which the
    ;; nl-jit-call-out-2 bridge surfaces as the call's return.
    ;;
    ;; Returns TRAMPOLINE_OK (= 0) via the zero2 wrapper.  Odd arity (3)
    ;; — AOT emits the rsp alignment pad via `--needs-align'.
    (defun nelisp_meta_dispatch_loop (inputs-vec emit-vec result-slot)
      (nelisp_meta_dispatch_loop_zero2
       (nelisp_meta_dispatch_loop_outer
        (vector-ref-ptr inputs-vec 0)
        (vector-ref-ptr inputs-vec 1)
        emit-vec
        result-slot)
       0)))
  "AOT source for the Wave A30 per-entry dispatch loop kernel
`(nelisp_meta_dispatch_loop INPUTS-VEC EMIT-VEC RESULT-SLOT)'.

Eight-entry `(seq DEFUN ...)' manifest:

- `nelisp_meta_dispatch_loop_seq3 (val _e1 _e2) -> val' — 3-arg
  side-effect sequencer; threads dealloc-bytes cleanup behind the
  vector-slot-set write, and the emit-step side-effect behind the
  popcount-updated accumulator return.

- `nelisp_meta_dispatch_loop_zero2 (_x ret) -> ret' — 2-arg
  discard-and-return; swaps the outer driver's slot-pointer return
  for TRAMPOLINE_OK at the public entry (= elisp `nl-jit-call-out-2'
  bridge convention).

- `nelisp_meta_dispatch_loop_popcount (n acc) -> popcount(n) + acc'
  — 2-arg tail-recursive bit-strip popcount (Brian Kernighan's
  algorithm).

- `nelisp_meta_dispatch_loop_emit_step_inner (c emit scratch final)
  -> 0' — 4-arg inner; materialises Sexp::Int(final) into scratch,
  `vector-slot-set's into emit[c], frees scratch via seq3.

- `nelisp_meta_dispatch_loop_emit_step (c dirty arch-skip emit
  dirty-val) -> 0' — 5-arg driver; allocates scratch slot, computes
  final-mask = dirty-val AND (arch-skip XOR -1), dispatches into
  the inner writer.

- `nelisp_meta_dispatch_loop_iter (c total dirty arch-skip emit acc)
  -> final-acc' — 6-arg tail-recursive per-chunk walker; per iter
  computes (dirty AND NOT arch-skip), writes into emit[c], accumulates
  popcount of (NOT arch-skip) into acc.

- `nelisp_meta_dispatch_loop_outer (dirty arch-skip emit result-slot)
  -> result-slot' — 4-arg outer driver; runs the walker with acc=0
  and writes Sexp::Int(popcount) into result-slot.

- `nelisp_meta_dispatch_loop (inputs-vec emit-vec result-slot) -> 0'
  — public 3-arg entry; unpacks the 2 input chunk vecs from
  inputs-vec[0/1] via `vector-ref-ptr', dispatches into the outer
  driver, then swaps the slot-pointer return for TRAMPOLINE_OK so
  the elisp `nl-jit-call-out-2' bridge surfaces the popcount via the
  caller-owned RESULT-SLOT.

Bridge contract (= `nl-jit-call-out-2' shape, see jit.rs):

  fn nelisp_meta_dispatch_loop(
      inputs_vec: *const Sexp,  // Sexp::Vec[dirty-chunks, arch-skip]
      emit_vec:   *const Sexp,  // Sexp::Vec (preallocated, writeback)
      result:     *mut Sexp,    // Sexp::Int(popcount) writeback
  ) -> i64                       // TRAMPOLINE_OK (= 0)

Caller-side preparation (elisp wrapper):

  1. Build dirty-chunks + arch-skip-chunks i64 vectors as before.
  2. Pre-OR spillover bits into arch-skip-chunks[final-chunk] so
     `(NOT arch-skip)' clears bit positions ≥ chunk-len in the
     final possibly-short chunk.
  3. Wrap each i64 chunk value in Sexp::Int and pack into Sexp::Vec.
  4. inputs-vec = (vector dirty-chunks-vec arch-skip-chunks-vec).
  5. emit-vec   = (make-vector chunk-count (sexp::int 0)).
  6. result     = (sexp::nil) placeholder.
  7. Call (nl-jit-call-out-2 \"nelisp_meta_dispatch_loop\"
                              inputs-vec emit-vec).
  8. Returned Sexp::Int = accept popcount = `(length acc)' the
     pre-A30 dispatch loop would have produced.
  9. emit-vec slots hold per-chunk Sexp::Int(final-mask); the
     wrapper iterates 4 chunks (not 212 entries) to extract set bits
     and dispatch `nelisp-aot-compile-to-object'.

Composes only existing AOT grammar — no new opcode:

- §111.C `vector-len' / `vector-ref-ptr' / `vector-slot-set' — vector
  probe + element pointer extraction + refcount-aware slot write.
- §125.A `alloc-bytes' / `dealloc-bytes' — 32-byte scratch slot for
  Sexp::Int materialisation before vector-slot-set.
- §100 `sexp-int-unwrap' / `sexp-int-make' — read/write i64 chunk
  bitmask payloads.
- §100.D `logand' / `logxor' — final-mask = dirty AND (arch-skip
  XOR -1); accept popcount via (arch-skip XOR -1).

Refcount note: `vector-ref-ptr' returns a borrowed `*const Sexp'
inside the vector's heap storage; `sexp-int-unwrap' reads the i64
payload without bumping refcounts.  `vector-slot-set' invokes
`nl_vector_set_slot' which is refcount-aware (= drops the slot's
prior value via Vec::index_mut assignment, clones the new value via
nl_sexp_clone_into).  Sexp::Int has no boxed payload so the clone
is a flat byte copy; refcount overhead is zero.

Linux-x86_64 only — same arch gate as the A25.2 / A26 parents.")

(provide 'nelisp-cc-bi-meta-dispatch-loop)

;;; nelisp-cc-bi-meta-dispatch-loop.el ends here
