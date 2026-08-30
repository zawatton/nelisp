;;; nelisp-cc-rootstack.el --- Doc 152 §11.37 Stage 2: dynamic root stack  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 152 §11.37 (B+E handle-based root API) Stage 2 — a dynamic root
;; stack so that, in Stage 3, every eval transient box can be parked in a
;; REGISTERED root slot (instead of an unenumerable in-flight C-stack /
;; arena-scratch pointer).  This is the foundation that lets a future
;; mid-form / safepoint GC be SOUND: the marker no longer has to "guess"
;; the live in-flight roots — they are all on this stack.
;;
;; Stage 2 installed this API dormant; Doc 152 Stage 3 activates it from
;; the evaluator.  A reserved entry is mutable 32-byte Sexp storage: eval
;; writes the value into the entry, and the marker revisits that same entry
;; after any allocation/safepoint.  The address of the entry is the handle;
;; callers must not copy its value into unregistered scratch and keep that
;; scratch across a safepoint.
;;
;; Main-thread storage is driver-owned BSS, not arena memory:
;;   (data-addr nl_rootstack_top)   = next free entry (0 = uninitialised)
;;   (data-addr nl_rootstack_region)= 131072 fixed 32-byte entries
;; BSS is outside sweep/compaction, so handles stay stable.  Each entry is
;; marked via `nl_gc_mark_slot' exactly like ctx/result/out.
;;
;; Doc 199 Tier 3b extends the same API to Tier-3a workers.  A registered
;; worker uses env+120 as its private top and [env+4096, top) as its private
;; reserve.  `nl_thread_registry' is a fixed driver-owned BSS table:
;;   +0 count, +8 reserved, +16.. 64 entries of {env, published-top} (16B).
;; Reserve/release publish the new top with a SeqCst CAS store.  The marker
;; takes a SeqCst snapshot and reuses `nl_gc_mark_rootstack_walk'; collection
;; remains stop-the-world at the Tier-3b barrier, not concurrent.
;;
;; API (consumed by Stage 3):
;;   nl_root_mark ENV        -> current top (a release marker; LIFO)
;;   nl_root_reserve ENV     -> reserve one zeroed 32B slot, return addr
;;   nl_root_release ENV M   -> restore top to marker M (pop the frame)
;;
;; Tier 3b park protocol (`nl_thread_parallel_ctx'):
;;   +24 request flag, +32 current parked count, +40 missed-collect count,
;;   +48 last satisfied parked count, +56 successful parked collects.
;; A registered worker increments +32 exactly once per contiguous request,
;; spins until +24 clears, then decrements +32 before returning to eval.  The
;; collector waits for +32 >= the registry count with a bounded spin.  Timeout
;; clears the request, increments +40, and MUST NOT collect.
;;
;; A registered worker is also forbidden to mutate the shared globals mirror.
;; Both the main EvalCtx and the private worker EvalCtx put globals_record at
;; env+0 and frames_record at env+32.  Mirror mutation helpers receive the
;; ADDRESS of globals_record, so their mirror-ptr is numerically the EvalCtx
;; address and can be looked up directly in this same registry.  The guard's
;; count-zero path is deliberately one BSS load + compare; mirror reads never
;; call it.  A refusal raises `nelisp-worker-mirror-mutation' with the attempted
;; global name as its sole data element.

;;; Code:

(defconst nelisp-cc-rootstack--source
  '(seq
    ;; Region = FIXED bss array (data-addr nl_rootstack_region); top = bss slot.
    ;; Do NOT mmap (os_alloc_chunk perturbs the arena chunk-growth VA layout ->
    ;; freelist corruption on the next collect, Doc 152 §11.30-33 class).
    ;; top == 0 means uninitialised (bss zero-fill); after init top >= region
    ;; addr (non-zero), so the zero-check is a reliable "not yet armed" gate.
    (defun nl_rootstack_init ()
      (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0)
          (ptr-write-u64 (data-addr nl_rootstack_top) 0 (data-addr nl_rootstack_region))
        0))
    ;; Fixed worker registry.  ADD is parent-only during the spawn phase, and
    ;; publishes count only after the entry is complete.  CLEAR is likewise
    ;; parent-only after every worker has joined.  Runtime values stay in
    ;; helper arguments: cc-unit locals cannot carry them across calls.
    (defun nl_thread_registry_entry (i)
      (+ (data-addr nl_thread_registry) (+ 16 (* i 16))))
    (defun nl_thread_registry_add_at (env i)
      (if (>= i 64) (- 0 1)
        (seq
         (ptr-write-u64 (nl_thread_registry_entry i) 0 env)
         (ptr-write-u64 (nl_thread_registry_entry i) 8
                        (ptr-read-u64 env 120))
         (ptr-write-u64 (data-addr nl_thread_registry) 0 (+ i 1))
         i)))
    ;; Intern the refusal condition on the parent before publishing any worker.
    ;; Otherwise the first refusal would make `nl_alloc_symbol' insert the
    ;; condition name into the shared intern table from the worker itself.
    ;; Runtime allocation results are threaded through helper arguments: the
    ;; cc-unit has no general across-call local other than `status'.
    (defun nl_thread_mirror_condition_init_at (tag-buf tag-slot)
      (seq
       (ptr-write-u64 tag-buf 0 8587643705457665390)
       (ptr-write-u64 (+ tag-buf 8) 0 7596778115794956911)
       (ptr-write-u64 (+ tag-buf 16) 0 8391733522635649650)
       (ptr-write-u64 (+ tag-buf 24) 0 474315584609)
       (extern-call nl_alloc_symbol tag-buf 29 tag-slot)))
    (defun nl_thread_mirror_condition_init ()
      (nl_thread_mirror_condition_init_at
       (alloc-bytes 32 1) (alloc-bytes 32 8)))
    (defun nl_thread_registry_add (env)
      (seq
       (nl_thread_mirror_condition_init)
       (nl_thread_registry_add_at
        env (ptr-read-u64 (data-addr nl_thread_registry) 0))))
    (defun nl_thread_registry_clear ()
      (ptr-write-u64 (data-addr nl_thread_registry) 0 0))
    (defun nl_thread_registry_find_from (env i count)
      (if (>= i count) 0
        (if (= (ptr-read-u64 (nl_thread_registry_entry i) 0) env)
            (nl_thread_registry_entry i)
          (nl_thread_registry_find_from env (+ i 1) count))))
    (defun nl_thread_registry_find (env)
      (nl_thread_registry_find_from
       env 0 (ptr-read-u64 (data-addr nl_thread_registry) 0)))
    ;; Catchable mirror-write refusal.  TAG/VAL/flag use the established M6
    ;; signal stash.  Build VAL as (NAME), so the uncaught diagnostic names the
    ;; attempted global and a condition-case handler can inspect `(cadr err)'.
    (defun nl_thread_mirror_mutation_signal_at
        (name-ptr tag-buf nil-slot _pad)
      (seq
       (ptr-write-u64 tag-buf 0 8587643705457665390)
       (ptr-write-u64 (+ tag-buf 8) 0 7596778115794956911)
       (ptr-write-u64 (+ tag-buf 16) 0 8391733522635649650)
       (ptr-write-u64 (+ tag-buf 24) 0 474315584609)
       (extern-call nl_alloc_symbol tag-buf 29 268435480)
       (ptr-write-u64 nil-slot 0 0)
       (ptr-write-u64 (+ nil-slot 8) 0 0)
       (ptr-write-u64 (+ nil-slot 16) 0 0)
       (ptr-write-u64 (+ nil-slot 24) 0 0)
       (extern-call nelisp_cons_construct name-ptr nil-slot 268435512)
       (ptr-write-u64 268435472 0 1)
       (atomic-fetch-add 268435544 1)
       1))
    (defun nl_thread_mirror_mutation_signal (name-ptr)
      (nl_thread_mirror_mutation_signal_at
       name-ptr (alloc-bytes 32 1) (alloc-bytes 32 8) 0))
    (defun nl_thread_mirror_mutation_guard_registered (entry name-ptr)
      (if (= entry 0) 0
        (nl_thread_mirror_mutation_signal name-ptr)))
    (defun nl_thread_mirror_mutation_guard (mirror-ptr name-ptr)
      ;; Outside a parallel section count is zero: one load + compare, no scan.
      (if (= (ptr-read-u64 (data-addr nl_thread_registry) 0) 0)
          0
        (nl_thread_mirror_mutation_guard_registered
         (nl_thread_registry_find mirror-ptr) name-ptr)))
    ;; The AOT DSL has no separate atomic-store operation.  A successful
    ;; SeqCst compare-exchange is the required atomic publication store; the
    ;; fetch-add by zero supplies its SeqCst expected value / marker load.
    (defun nl_thread_registry_store_top (entry top)
      (if (= (atomic-compare-exchange
              (+ entry 8) (atomic-fetch-add (+ entry 8) 0) top)
             1)
          top
        (nl_thread_registry_store_top entry top)))
    ;; Tier 3b stop-the-world park barrier.  The spin loops deliberately use
    ;; the already-proven `nl_thread_join_impl' shape: an aligned atomic/load
    ;; predicate and an empty body.  The collector-side loop is bounded; its
    ;; sole across-call-style local is `status', and every runtime value is
    ;; threaded through helper arguments.
    (defun nl_thread_park_request_store (value)
      (if (= (atomic-compare-exchange
              (+ (data-addr nl_thread_parallel_ctx) 24)
              (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 24) 0)
              value)
             1)
          value
        (nl_thread_park_request_store value)))
    (defun nl_thread_park_missed ()
      (nl_seq2
       (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 40) 1)
       0))
    (defun nl_thread_park_wait_bounded (expected)
      (let ((status 16777216))
        (seq
         (while (if (< (atomic-fetch-add
                        (+ (data-addr nl_thread_parallel_ctx) 32) 0)
                       expected)
                    (> status 0)
                  0)
           (setq status (- status 1)))
         (if (>= (atomic-fetch-add
                  (+ (data-addr nl_thread_parallel_ctx) 32) 0)
                 expected)
             1
           0))))
    (defun nl_thread_park_request_wait (expected)
      (if (= (nl_thread_park_wait_bounded expected) 1)
          (nl_seq2
           (ptr-write-u64 (data-addr nl_thread_parallel_ctx) 48 expected)
           1)
        (nl_seq2
         (nl_thread_park_request_store 0)
         (nl_thread_park_missed))))
    (defun nl_thread_park_request_claim (expected)
      (if (= (atomic-fetch-add
              (+ (data-addr nl_thread_parallel_ctx) 32) 0)
             0)
          (if (= (atomic-compare-exchange
                  (+ (data-addr nl_thread_parallel_ctx) 24) 0 1)
                 1)
              (nl_thread_park_request_wait expected)
            (nl_thread_park_missed))
        (nl_thread_park_missed)))
    (defun nl_thread_park_request_begin ()
      (nl_thread_park_request_claim
       (ptr-read-u64 (data-addr nl_thread_registry) 0)))
    (defun nl_thread_park_request_end ()
      (nl_thread_park_request_store 0))
    (defun nl_thread_park_collect_succeeded ()
      (nl_seq2
       (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 56) 1)
       0))
    (defun nl_thread_park_safepoint_at (entry)
      (if (= entry 0) 0
        (if (= (atomic-fetch-add
                (+ (data-addr nl_thread_parallel_ctx) 24) 0)
               1)
            (seq
             (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 32) 1)
             (while (= (atomic-fetch-add
                        (+ (data-addr nl_thread_parallel_ctx) 24) 0)
                       1)
               0)
             (atomic-fetch-add (+ (data-addr nl_thread_parallel_ctx) 32)
                               (- 0 1))
             1)
          0)))
    (defun nl_thread_park_safepoint (env)
      (nl_thread_park_safepoint_at (nl_thread_registry_find env)))
    (defun nl_root_mark_at (env entry)
      (if (= entry 0)
          (ptr-read-u64 (data-addr nl_rootstack_top) 0)
        (ptr-read-u64 env 120)))
    (defun nl_root_mark (env)
      (nl_root_mark_at env (nl_thread_registry_find env)))
    (defun nl_root_depth ()
      (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0) 0
        (sar (- (ptr-read-u64 (data-addr nl_rootstack_top) 0)
                (data-addr nl_rootstack_region))
             5)))
    ;; Reserve one 32-byte slot at top, zero it, bump top, return slot addr.
    ;; END of the fixed region: 131072 entries * 32 bytes.  The bump below had
    ;; no bound at all, so a deep enough recursion walked `top' straight out of
    ;; `nl_rootstack_region' and kept writing into whatever bss follows it --
    ;; a silent out-of-bounds write that surfaces as a SIGSEGV somewhere else
    ;; entirely.  Doc 152 Stage 3 rooting costs root-depth 3N+6 per non-tail
    ;; recursion, so the ceiling is reachable, and any future arm that roots
    ;; one more slot per call lowers the recursion depth at which it is hit.
    ;; Answering 0 here lets `nl_root_reserve_at' fall back to an ordinary
    ;; scratch: that slot is then unrooted, which is exactly what every caller
    ;; had before Stage 3, rather than memory corruption.
    (defun nl_rootstack_end ()
      (+ (data-addr nl_rootstack_region) 4194304))
    (defun nl_root_reserve_slot (slot)
      (if (= slot 0) 0
        (if (> (+ slot 32) (nl_rootstack_end)) 0
          (seq (ptr-write-u64 slot 0 0)
               (ptr-write-u64 (+ slot 8) 0 0)
               (ptr-write-u64 (+ slot 16) 0 0)
               (ptr-write-u64 (+ slot 24) 0 0)
               (ptr-write-u64 (data-addr nl_rootstack_top) 0 (+ slot 32))
               slot))))
    (defun nl_root_reserve_private (env entry slot)
      (if (= slot 0) 0
        (seq
         (ptr-write-u64 slot 0 0)
         (ptr-write-u64 (+ slot 8) 0 0)
         (ptr-write-u64 (+ slot 16) 0 0)
         (ptr-write-u64 (+ slot 24) 0 0)
         (ptr-write-u64 env 120 (+ slot 32))
         (nl_thread_registry_store_top entry (+ slot 32))
         slot)))
    ;; A full region answers an ordinary scratch slot instead of 0: every
    ;; caller writes through the returned pointer immediately, so 0 would be a
    ;; null store.  The fallback slot is not a GC root -- the caller is back to
    ;; pre-Stage-3 behaviour for that one value -- which is a bounded loss of
    ;; precision at a recursion depth that already had none.
    (defun nl_root_reserve_or_scratch (slot)
      (if (= slot 0) (alloc-bytes 32 8) slot))
    (defun nl_root_reserve_at (env entry)
      (if (= entry 0)
          (seq
           (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0)
               (nl_rootstack_init) 0)
           (nl_root_reserve_or_scratch
            (nl_root_reserve_slot
             (ptr-read-u64 (data-addr nl_rootstack_top) 0))))
        (nl_root_reserve_or_scratch
         (nl_root_reserve_private env entry (ptr-read-u64 env 120)))))
    (defun nl_root_reserve (env)
      (nl_root_reserve_at env (nl_thread_registry_find env)))
    (defun nl_root_release_at (env entry marker)
      (if (= entry 0)
          (ptr-write-u64 (data-addr nl_rootstack_top) 0 marker)
        (seq
         (ptr-write-u64 env 120 marker)
         (nl_thread_registry_store_top entry marker)
         0)))
    (defun nl_root_release (env marker)
      (nl_root_release_at env (nl_thread_registry_find env) marker))
    ;; GC: walk [region, top) in 32-byte steps, mark each slot like a root.
    (defun nl_gc_mark_rootstack_walk (p end)
      (if (>= p end) 0
          (seq (extern-call nl_gc_mark_slot p)
               (nl_gc_mark_rootstack_walk (+ p 32) end))))
    (defun nl_gc_mark_rootstack ()
      (if (= (ptr-read-u64 (data-addr nl_rootstack_top) 0) 0) 0
          (nl_gc_mark_rootstack_walk (data-addr nl_rootstack_region)
                                     (ptr-read-u64 (data-addr nl_rootstack_top) 0))))
    ;; Tier 3b: marker-side enumeration of every published private reserve.
    ;; The barrier has stopped workers before this runs; the atomic top load is
    ;; still paired with reserve/release publication so the API is explicit.
    (defun nl_gc_mark_thread_roots_one (env top)
      (if (= env 0) 0
        (nl_seq2
         ;; The mmap-resident EvalCtx is not itself in the private reserve.
         ;; Mark its mirror/frame-stack/unbound slots exactly like a recorded
         ;; evaluator frame before walking transient handles; otherwise a
         ;; parked `let' can resume through a swept frame backing vector.
         (extern-call nl_gc_mark_recorded_env env)
         (nl_gc_mark_rootstack_walk (+ env 4096) top))))
    (defun nl_gc_mark_thread_roots_from (i count)
      (if (>= i count) 0
        (seq
         (nl_gc_mark_thread_roots_one
          (ptr-read-u64 (nl_thread_registry_entry i) 0)
          (atomic-fetch-add (+ (nl_thread_registry_entry i) 8) 0))
         (nl_gc_mark_thread_roots_from (+ i 1) count))))
    (defun nl_gc_mark_thread_roots ()
      (nl_gc_mark_thread_roots_from
       0 (ptr-read-u64 (data-addr nl_thread_registry) 0))))
  "AOT source for the Doc 152 §11.37 Stage 2 dynamic root stack.

Lazy-inits on the first main-thread `nl_root_reserve'.  Stage-3 evaluator callers use
the returned mutable entry directly as their eval/function result slot and
restore a saved `nl_root_mark' on every status path.  Tier-3b workers select
their registered private reserve by EvalCtx address.  See the Commentary for
the storage layout and soundness rationale.")

(provide 'nelisp-cc-rootstack)

;;; nelisp-cc-rootstack.el ends here
