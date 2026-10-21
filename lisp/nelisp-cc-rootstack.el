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
;;   +0 managed count, +8 legacy chain, +16.. 64 managed entries (16B).
;; Reserve/release publish the new top with a SeqCst CAS store.  The marker
;; takes a SeqCst snapshot and reuses `nl_gc_mark_rootstack_walk'; collection
;; remains stop-the-world at the Tier-3b barrier, not concurrent.
;;
;; API (consumed by Stage 3):
;;   nl_root_mark ENV        -> current top (a release marker; LIFO)
;;   nl_root_reserve ENV     -> reserve one zeroed 32B slot, return addr
;;   nl_root_reserve_checked ENV -> reserve one registered slot, or 0 at capacity
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
      (if (= (heap-read-u64 (data-addr nl_rootstack_top) 0) 0)
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
                        (heap-read-u64 env 120))
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
        env (heap-read-u64 (data-addr nl_thread_registry) 0))))
    (defun nl_thread_registry_clear ()
      (ptr-write-u64 (data-addr nl_thread_registry) 0 0))
    (defun nl_thread_registry_find_from (env i count)
      (if (or (>= i count) (>= i 64)) 0
        (if (= (heap-read-u64 (nl_thread_registry_entry i) 0) env)
            (nl_thread_registry_entry i)
          (nl_thread_registry_find_from env (+ i 1) count))))
    ;; Callers test the registry count inline first: with no worker thread
    ;; registered (the interpreter's normal state) every lookup answers 0, and
    ;; root reservation alone made 21 of these calls per interpreted defun
    ;; call.
    (defun nl_thread_registry_find (env)
      (nl_thread_registry_find_from
       env 0 (heap-read-u64 (data-addr nl_thread_registry) 0)))
    ;; GNU thread.h:103-143 gives each thread its activation/catch state;
    ;; thread.c:711 marks those owners separately. Legacy form workers have
    ;; the same root-bank layout, but not managed-worker mutation/FFI policy.
    ;; +168 is an aligned previous-owner address with bit0 = still running.
    ;; The parent publishes initialized roots before clone, and only END
    ;; removes owners after terminal acknowledgment and the managed joins.
    (defun nl_thread_legacy_head ()
      (atomic-fetch-add (+ (data-addr nl_thread_registry) 8) 0))
    (defun nl_thread_legacy_find (env)
      (let* ((node (nl_thread_legacy_head)) (found 0))
        (seq
         (while (and (/= node 0) (= found 0))
           (if (= node env) (setq found node)
             (setq node (logand (atomic-fetch-add (+ node 168) 0) -2))))
         found)))
    (defun nl_thread_legacy_p (env)
      (if (= env 0) 0 (if (= (nl_thread_legacy_find env) 0) 0 1)))
    ;; 0 = main roots, 1 = legacy private roots, otherwise managed entry.
    ;; Membership is established before dereferencing an arbitrary ENV.
    (defun nl_root_owner (env)
      (let* ((entry (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
                        0 (nl_thread_registry_find env))))
        (if (/= entry 0) entry
          (if (= (nl_thread_legacy_p env) 1) 1 0))))
    (defun nl_thread_legacy_publish (env)
      (let* ((previous (nl_thread_legacy_head)) (published 0))
        (seq
         (while (= published 0)
           (seq
            (ptr-write-u64 env 168 (+ previous 1))
            (if (= (atomic-compare-exchange
                    (+ (data-addr nl_thread_registry) 8) previous env) 1)
                (setq published 1)
              (setq previous (nl_thread_legacy_head))))) env)))
    (defun nl_thread_legacy_complete (env)
      ;; This is the child's final access to its context/root mapping.
      (seq (atomic-fetch-add (+ env 168) -1) 0))
    (defun nl_thread_legacy_finish_all ()
      (let* ((head (nl_thread_legacy_head)) (node 0) (detached 0))
        (seq
         (while (= detached 0)
           (seq
            (setq node head)
            (while (/= node 0)
              (seq
               (while (= (logand (atomic-fetch-add (+ node 168) 0) 1) 1) 0)
               (setq node (logand (atomic-fetch-add (+ node 168) 0) -2))))
            (if (= (atomic-compare-exchange
                    (+ (data-addr nl_thread_registry) 8) head 0) 1)
                (setq detached 1)
              (setq head (nl_thread_legacy_head)))))
         ;; No legacy task can use a root view after its terminal store; END
         ;; also requires the existing managed joins before reaching here.
         (while (/= head 0)
           (let* ((previous (logand (atomic-fetch-add (+ head 168) 0) -2)))
             (seq (nl_os_free_chunk head 1052672) (setq head previous)))) 0)))
    (defun nl_root_legacy_store_top (env top)
      (if (= (atomic-compare-exchange
              (+ env 120) (atomic-fetch-add (+ env 120) 0) top) 1)
          top (nl_root_legacy_store_top env top)))
    (defun nl_root_private_slot_valid (env slot)
      (if (and (>= slot (+ env 4096))
               (<= slot (- (+ env 1052672) 32))
               (= (logand (- slot (+ env 4096)) 31) 0)) 1 0))
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
      (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
          0
        (nl_thread_mirror_mutation_guard_registered
         (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0) 0 (nl_thread_registry_find mirror-ptr)) name-ptr)))
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
       (heap-read-u64 (data-addr nl_thread_registry) 0)))
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
      (nl_thread_park_safepoint_at (if (= (heap-read-u64 (data-addr nl_thread_registry) 0) 0) 0 (nl_thread_registry_find env))))
    (defun nl_root_mark_at (env entry)
      (if (= entry 0)
          (heap-read-u64 (data-addr nl_rootstack_top) 0)
        (heap-read-u64 env 120)))
    (defun nl_root_mark (env)
      (nl_root_mark_at env (nl_root_owner env)))
    (defun nl_root_depth ()
      (if (= (heap-read-u64 (data-addr nl_rootstack_top) 0) 0) 0
        (sar (- (heap-read-u64 (data-addr nl_rootstack_top) 0)
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
    ;; Mirror lookup accepts a Sexp view only while its 32-byte slot is a
    ;; live root. Main-thread roots occupy the global BSS range; registered
    ;; workers use their private env+4096 arena and publish the active top in
    ;; the registry. Keep this check bounded and require a complete aligned
    ;; slot below the current top so freed, future, or interior addresses do
    ;; not become trusted lookup keys.
    (defun nl_rootstack_slot_active_worker (slot i count)
      (if (>= i count) 0
        (let* ((entry (nl_thread_registry_entry i))
               (env (heap-read-u64 entry 0))
               (top (atomic-fetch-add (+ entry 8) 0))
               (base (+ env 4096))
               (end (+ env 1052672)))
          (if (and (/= env 0)
                   (>= top base) (<= top end)
                   (= (logand (- top base) 31) 0)
                   (>= slot base) (<= slot (- top 32))
                   (= (logand (- slot base) 31) 0))
              1
            (nl_rootstack_slot_active_worker slot (+ i 1) count)))))
    (defun nl_rootstack_slot_active_legacy (slot)
      (let* ((node (nl_thread_legacy_head)) (active 0) (top 0))
        (seq
         (while (and (/= node 0) (= active 0))
           (seq
            (setq top (atomic-fetch-add (+ node 120) 0))
            (if (and (>= top (+ node 4096)) (<= top (+ node 1052672))
                     (= (logand (- top (+ node 4096)) 31) 0)
                     (= (nl_root_private_slot_valid node slot) 1)
                     (<= slot (- top 32)))
                (setq active 1)
              (setq node (logand (atomic-fetch-add (+ node 168) 0) -2)))))
         active)))
    (defun nl_rootstack_slot_active (slot)
      (let* ((base (data-addr nl_rootstack_region))
             (end (nl_rootstack_end))
             (top (heap-read-u64 (data-addr nl_rootstack_top) 0)))
        (if (and (/= top 0)
                 (>= top base) (<= top end)
                 (= (logand (- top base) 31) 0)
                 (>= slot base) (<= slot (- top 32))
                 (= (logand (- slot base) 31) 0))
            1
          (if (= (nl_rootstack_slot_active_worker
                  slot 0 (heap-read-u64 (data-addr nl_thread_registry) 0)) 1) 1
            (nl_rootstack_slot_active_legacy slot)))))
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
      (if (= (nl_root_private_slot_valid env slot) 0) 0
        (seq
         (ptr-write-u64 slot 0 0)
         (ptr-write-u64 (+ slot 8) 0 0)
         (ptr-write-u64 (+ slot 16) 0 0)
         (ptr-write-u64 (+ slot 24) 0 0)
         (if (= entry 1) (nl_root_legacy_store_top env (+ slot 32))
           (seq (ptr-write-u64 env 120 (+ slot 32))
                (nl_thread_registry_store_top entry (+ slot 32))))
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
           (if (= (heap-read-u64 (data-addr nl_rootstack_top) 0) 0)
               (nl_rootstack_init) 0)
           (nl_root_reserve_or_scratch
            (nl_root_reserve_slot
             (heap-read-u64 (data-addr nl_rootstack_top) 0))))
        (nl_root_reserve_or_scratch
         (nl_root_reserve_private env entry (heap-read-u64 env 120)))))
    (defun nl_root_reserve (env)
      (nl_root_reserve_at env (nl_root_owner env)))
    ;; The native loader cannot use `nl_root_reserve': its capacity fallback
    ;; is deliberately unrooted scratch for legacy callers.  Object arguments
    ;; must remain visible to a moving collection, so this variant reserves
    ;; only from a validated registered region and answers 0 on exhaustion.
    (defun nl_root_reserve_checked (env)
      (let* ((entry (nl_root_owner env)))
        (if (= entry 0)
            (seq
             (if (= (heap-read-u64 (data-addr nl_rootstack_top) 0) 0)
                 (nl_rootstack_init) 0)
             (let* ((slot (heap-read-u64 (data-addr nl_rootstack_top) 0))
                    (base (data-addr nl_rootstack_region)))
               (if (if (< slot base) 1
                     (if (> (+ slot 32) (nl_rootstack_end)) 1
                       (if (/= (logand (- slot base) 31) 0) 1 0)))
                   0
                 (nl_root_reserve_slot slot))))
          (let* ((slot (heap-read-u64 env 120))
                 (base (+ env 4096))
                 (end (+ env 1052672)))
            (if (if (< slot base) 1
                  (if (> (+ slot 32) end) 1
                    (if (/= (logand (- slot base) 31) 0) 1 0)))
                0
              (nl_root_reserve_private env entry slot))))))
    (defun nl_root_release_at (env entry marker)
      (if (= entry 0)
          (ptr-write-u64 (data-addr nl_rootstack_top) 0 marker)
        (if (= entry 1)
            (if (and (>= marker (+ env 4096))
                     (<= marker (atomic-fetch-add (+ env 120) 0))
                     (= (logand (- marker (+ env 4096)) 31) 0))
                (seq (nl_root_legacy_store_top env marker) 0) 0)
          (seq (ptr-write-u64 env 120 marker)
               (nl_thread_registry_store_top entry marker) 0))))
    (defun nl_root_release (env marker)
      (nl_root_release_at env (nl_root_owner env) marker))
    ;; Native loader roots live in a separate driver-owned BSS region.  The
    ;; evaluator restores its ordinary root top after every interpreted call,
    ;; so a loader frame must never borrow that stack.  Pinning is single-owner:
    ;; calls from registered workers, and any caller with a different env,
    ;; fail closed instead of sharing mutable roots.  Doc 207: the owning env
    ;; may nest frames strictly LIFO (a native activation calling back into
    ;; Lisp that enters another native activation).  A nested frame starts
    ;; with a link slot holding the enclosing marker; the enclosing frame's
    ;; slots stay below it, still scanned as roots, but outside the inner
    ;; frame's active range.
    (defun nl_root_pin_begin_nested (env)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (end (+ base 524288))
             (top (atomic-fetch-add (+ control 24) 0))
             (prev (heap-read-u64 control 16)))
        (if (if (/= (atomic-fetch-add control 0) 1) 1
              (if (/= (heap-read-u64 control 8) env) 1
                (if (/= (heap-read-u64 control 48) 1) 1
                (if (< top base) 1
                  (if (> (+ top 64) end) 1
                    (if (/= (logand (- top base) 31) 0) 1 0))))))
            0
          (seq
           ;; Fixnum-tagged link: the collector treats it as an integer.
           (ptr-write-u64 top 0 2)
           (ptr-write-u64 (+ top 8) 0 prev)
           (ptr-write-u64 (+ top 16) 0 1234567890123456789)
           (ptr-write-u64 (+ top 24) 0 (+ top 32))
           (ptr-write-u64 (+ control 24) 0 (+ top 32))
           (ptr-write-u64 (+ control 16) 0 (+ top 32))
           (+ top 32)))))
    (defun nl_root_pin_begin (env)
        (if (/= (heap-read-u64 (data-addr nl_thread_registry) 0) 0)
            0
        (if (= (heap-read-u64 (data-addr nl_root_pin_control) 48) 0)
            (if (= (atomic-compare-exchange
                    (data-addr nl_root_pin_control) 0 1)
                   1)
                (seq
                 (ptr-write-u64 (data-addr nl_root_pin_control) 8 env)
                 (if (= (heap-read-u64 (data-addr nl_root_pin_control) 24) 0)
                     (ptr-write-u64 (data-addr nl_root_pin_control) 24
                                    (data-addr nl_root_pin_region))
                   0)
                 (let ((marker
                        (heap-read-u64 (data-addr nl_root_pin_control) 24)))
                   (ptr-write-u64 (data-addr nl_root_pin_control) 16 marker)
                   (ptr-write-u64 (data-addr nl_root_pin_control) 48 1)
                   marker))
              0)
          (if (= (heap-read-u64 (data-addr nl_root_pin_control) 48) 1)
              (nl_root_pin_begin_nested env)
            0))))
    (defun nl_root_pin_reserve (env marker)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (end (+ base 524288))
             (top (atomic-fetch-add (+ control 24) 0)))
        (if (if (/= (atomic-fetch-add control 0) 1) 1
              (if (/= (heap-read-u64 control 8) env) 1
              (if (/= (heap-read-u64 control 48) 1) 1
               (if (/= (heap-read-u64 control 16) marker) 1
                  (if (if (< top base) 1
                        (if (> (+ top 32) end) 1
                          (if (/= (logand (- top base) 31) 0) 1 0)))
                      1 0)))))
            0
          (seq
           (ptr-write-u64 top 0 0)
           (ptr-write-u64 (+ top 8) 0 0)
           (ptr-write-u64 (+ top 16) 0 0)
           (ptr-write-u64 (+ top 24) 0 0)
           (if (= (atomic-compare-exchange (+ control 24) top (+ top 32)) 1)
               top
             0)))))
    ;; Native artifact name slots are valid Sexp views while their pin frame
    ;; is active, even though the driver-owned pin region is outside the GC
    ;; arena.  Keep acceptance bounded to an aligned, fully reserved slot.
    (defun nl_root_pin_slot_active (slot)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (end (+ base 524288))
             (top (atomic-fetch-add (+ control 24) 0))
             (marker (heap-read-u64 control 16)))
        (if (and (= (atomic-fetch-add control 0) 1)
                 (> (heap-read-u64 control 8) 0)
                 (>= top base)
                 (<= top end)
                 (= (logand (- top base) 31) 0)
                 (>= marker base)
                 (<= marker top)
                 (<= marker end)
                 (= (logand (- marker base) 31) 0)
                 (>= slot marker)
                 (>= slot base)
                 (<= slot (- top 32))
                 (= (logand (- slot base) 31) 0))
            1
          0)))
    (defun nl_root_pin_end_valid (base top marker)
        (if (< marker base) 0
          (if (> marker top) 0
            (if (> top (+ base 524288)) 0
              (if (/= (logand (- marker base) 31) 0) 0 1)))))
    (defun nl_root_pin_nested_link_p (base marker)
      (if (< marker (+ base 32)) 0
        (let ((link (- marker 32)))
          (if (and (= (heap-read-u64 link 0) 2)
                   (= (heap-read-u64 link 16) 1234567890123456789)
                   (= (heap-read-u64 link 24) marker))
              1 0))))
    (defun nl_root_pin_end_nested (control marker)
      (let ((link (- marker 32)))
        (seq
         (ptr-write-u64 (+ control 16) 0 (heap-read-u64 link 8))
         (ptr-write-u64 (+ control 24) 0 link)
         (ptr-write-u64 link 0 0)
         (ptr-write-u64 (+ link 8) 0 0)
         (ptr-write-u64 (+ link 16) 0 0)
         (ptr-write-u64 (+ link 24) 0 0)
         1)))
    (defun nl_root_pin_end (env marker)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (top (atomic-fetch-add (+ control 24) 0)))
        (if (= (atomic-fetch-add control 0) 1)
            (if (= (heap-read-u64 control 48) 1)
                (if (= (heap-read-u64 control 8) env)
                    (if (= (heap-read-u64 control 16) marker)
                        (if (= (nl_root_pin_end_valid base top marker) 1)
                            (if (= (nl_root_pin_nested_link_p base marker) 1)
                                (nl_root_pin_end_nested control marker)
                              (seq
                               (ptr-write-u64 (+ control 24) 0 marker)
                               (ptr-write-u64 (+ control 8) 0 0)
                               (ptr-write-u64 (+ control 16) 0 0)
                               (ptr-write-u64 (+ control 48) 0 0)
                               (if (= (atomic-compare-exchange control 1 0) 1)
                                   1 0)))
                          0)
                      0)
                  0)
              0)
          0)))
    ;; V2 tickets are process-local identities, independent of reusable root
    ;; addresses.  Control offsets +32/+40/+48 hold current/next/mode; nested
    ;; links store the enclosing ticket in their +16 word.  The monotonic
    ;; counter fails closed before signed-i64 wrap and is never serialized.
    ;; The standalone linker reserves a 64-byte control prefix before the
    ;; pin-root region so these fields cannot overlap the first root slot.
    (defun nl_root_pin_v2_next_token (control)
      (let* ((next (heap-read-u64 control 40))
             (token (if (= next 0) 1 next)))
        ;; Tickets are positive signed-i64 values in the ptr-call ABI; stop at
        ;; INT64_MAX instead of allowing an emitter or host to wrap the u64.
        (if (>= token 9223372036854775807) 0 token)))
    (defun nl_root_pin_v2_commit_token (control token)
      (seq
       (ptr-write-u64 (+ control 32) 0 token)
       (ptr-write-u64 (+ control 40) 0 (+ token 1))
       token))
    (defun nl_root_pin_begin_v2 (env)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (mode (heap-read-u64 control 48))
             (token (nl_root_pin_v2_next_token control)))
        (if (or (= token 0)
                (/= (heap-read-u64 (data-addr nl_thread_registry) 0) 0))
            0
          (if (= mode 0)
              (if (= (atomic-compare-exchange control 0 1) 1)
                  (seq
                   (ptr-write-u64 (+ control 8) 0 env)
                   (if (= (heap-read-u64 control 24) 0)
                       (ptr-write-u64 (+ control 24) 0 base)
                     0)
                   (ptr-write-u64 (+ control 16) 0
                                  (heap-read-u64 control 24))
                   (ptr-write-u64 (+ control 48) 0 2)
                   (nl_root_pin_v2_commit_token control token))
                0)
            (if (= mode 2)
                (let* ((top (atomic-fetch-add (+ control 24) 0))
                       (prev-marker (heap-read-u64 control 16))
                       (prev-token (heap-read-u64 control 32))
                       (end (+ base 524288)))
                  (if (if (/= (atomic-fetch-add control 0) 1) 1
                        (if (/= (heap-read-u64 control 8) env) 1
                          (if (< top base) 1
                            (if (> (+ top 64) end) 1
                              (if (/= (logand (- top base) 31) 0) 1 0)))))
                      0
                    (seq
                     (ptr-write-u64 top 0 2)
                     (ptr-write-u64 (+ top 8) 0 prev-marker)
                     (ptr-write-u64 (+ top 16) 0 prev-token)
                     (ptr-write-u64 (+ top 24) 0 (+ top 32))
                     (ptr-write-u64 (+ control 24) 0 (+ top 32))
                     (ptr-write-u64 (+ control 16) 0 (+ top 32))
                     (nl_root_pin_v2_commit_token control token))))
              0)))))
    (defun nl_root_pin_reserve_v2 (env token)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (end (+ base 524288))
             (top (atomic-fetch-add (+ control 24) 0)))
        (if (if (/= (atomic-fetch-add control 0) 1) 1
              (if (/= (heap-read-u64 control 48) 2) 1
                (if (/= (heap-read-u64 control 8) env) 1
                  (if (/= (heap-read-u64 control 32) token) 1
                    (if (if (< top base) 1
                          (if (> (+ top 32) end) 1
                            (if (/= (logand (- top base) 31) 0) 1 0)))
                        1 0)))))
            0
          (seq
           (ptr-write-u64 top 0 0)
           (ptr-write-u64 (+ top 8) 0 0)
           (ptr-write-u64 (+ top 16) 0 0)
           (ptr-write-u64 (+ top 24) 0 0)
           (if (= (atomic-compare-exchange (+ control 24) top (+ top 32)) 1)
               top
             0)))))
    ;; Clause 1 root storage. Reserve and publish one contiguous, zeroed bank
    ;; with the same ownership, ticket, alignment and capacity checks as the
    ;; single-slot API. No allocation/evaluator/GC call occurs before publish.
    (defun nl_root_pin_reserve_many_v2 (env token count)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (end (+ base 524288))
             (top (atomic-fetch-add (+ control 24) 0))
             (next (+ top (* count 32))) (i 0))
        (if (or (< count 1) (> count 255)
                (/= (atomic-fetch-add control 0) 1)
                (/= (heap-read-u64 control 48) 2)
                (/= (heap-read-u64 control 8) env)
                (/= (heap-read-u64 control 32) token)
                (< top base) (> next end)
                (/= (logand (- top base) 31) 0))
            0
          (seq
           (while (< i count)
             (let ((slot (+ top (* i 32))))
               (ptr-write-u64 slot 0 0)
               (ptr-write-u64 slot 8 0)
               (ptr-write-u64 slot 16 0)
               (ptr-write-u64 slot 24 0))
             (setq i (+ i 1)))
           (if (= (atomic-compare-exchange (+ control 24) top next) 1) top 0)))))
    (defun nl_root_pin_end_v2 (env token)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (top (atomic-fetch-add (+ control 24) 0))
             (marker (heap-read-u64 control 16)))
        (if (= (atomic-fetch-add control 0) 1)
          (if (= (heap-read-u64 control 48) 2)
           (if (= (heap-read-u64 control 8) env)
            (if (= (heap-read-u64 control 32) token)
             (if (= (nl_root_pin_end_valid base top marker) 1)
            (if (> marker base)
                (let ((link (- marker 32)))
                  (if (and (= (heap-read-u64 link 0) 2)
                           (= (heap-read-u64 link 24) marker)
                           (> (heap-read-u64 link 16) 0))
                      (seq
                       (ptr-write-u64 (+ control 16) 0
                                      (heap-read-u64 link 8))
                       (ptr-write-u64 (+ control 24) 0 link)
                       (ptr-write-u64 (+ control 32) 0
                                      (heap-read-u64 link 16))
                       (ptr-write-u64 link 0 0)
                       (ptr-write-u64 (+ link 8) 0 0)
                       (ptr-write-u64 (+ link 16) 0 0)
                       (ptr-write-u64 (+ link 24) 0 0)
                       1)
                    0))
              (seq
               (ptr-write-u64 (+ control 24) 0 marker)
               (ptr-write-u64 (+ control 8) 0 0)
               (ptr-write-u64 (+ control 16) 0 0)
               (ptr-write-u64 (+ control 32) 0 0)
               (ptr-write-u64 (+ control 48) 0 0)
               (if (= (atomic-compare-exchange control 1 0) 1) 1 0)))
             0)
            0)
           0)
          0)
          0)))
    ;; Return an index into only the active frame's reserved slots.  The
    ;; caller supplies no address, so every pointer is derived from the
    ;; runtime-owned frame marker after validating its ticket and link.
    (defun nl_root_pin_slot_v2 (env token index)
      (let* ((control (data-addr nl_root_pin_control))
             (base (data-addr nl_root_pin_region))
             (region-end (if (> base 9223372036854251519)
                             0 (+ base 524288)))
             (top (atomic-fetch-add (+ control 24) 0))
             (marker (heap-read-u64 control 16))
             (safe-marker
              (if (if (= region-end 0) 1
                    (if (< marker base) 1 (> marker region-end)))
                  base marker))
             (safe-top
              (if (if (= region-end 0) 1
                    (if (< top base) 1 (> top region-end)))
                  base top))
             (safe-index
              (if (if (< index 0) 1
                    (if (>= index 16384)
                        1 (> safe-marker 9223372036854251519)))
                  0 index))
             (slot (+ safe-marker (* safe-index 32)))
             (slot-end (if (> slot 9223372036854775775)
                           0 (+ slot 32)))
             (link (if (> safe-marker base) (- safe-marker 32) base))
             (parent-marker (heap-read-u64 link 8))
             (safe-parent-marker
              (if (if (< parent-marker base) 1 (> parent-marker link))
                  base parent-marker))
             (parent-token (heap-read-u64 link 16))
             (frame-valid
              (if (= safe-marker base) 1
                (if (and (> safe-marker base)
                         (>= link base)
                         (= (heap-read-u64 link 0) 2)
                         (= (heap-read-u64 link 24) safe-marker)
                         (>= parent-marker base)
                         (<= parent-marker link)
                         (= (logand (- safe-parent-marker base) 31) 0)
                         (> parent-token 0)
                         (< parent-token token))
                    1 0))))
        (if (if (or (/= (atomic-fetch-add control 0) 1)
                    (/= (heap-read-u64 control 48) 2)
                    (/= (heap-read-u64 control 8) env)
                    (<= token 0)
                    (/= (heap-read-u64 control 32) token)
                    (= region-end 0)
                    (< marker base) (> marker top) (> top region-end)
                    (/= (logand (- marker base) 31) 0)
                    (/= (logand (- safe-top base) 31) 0)
                    (= frame-valid 0)
                    (< index 0) (>= index 16384)
                    (= slot-end 0) (< slot marker) (> slot-end top))
                1 0)
            0
          slot)))
    ;; Return status: 0 success, 1 unsupported input tag, 2 malformed request.
    ;; The caller supplies only root-slot indices; both addresses are resolved
    ;; and authenticated before either slot is dereferenced.  No object pointer
    ;; is kept across the CAR accessor call, which may materialise an immediate.
    ;; Doc 215: store value WORD W as a 32-byte Sexp in root slot OUTPUT.
    ;; A pointer WORD is the child's own box (copied, as the old view copy
    ;; did); an immediate is written as its storage form (tag 0 nil, 1 t,
    ;; 2 fixnum) without allocating.  Returns 0 (success status).
    (defun nl_native_store_word_v2 (output w)
      (if (or (= (logand w 1) 1) (= (logand w 7) 6))
          (seq (nl_sexp_clone_into w output) 0)
        (seq (ptr-write-u64 output 0 (heap-read-u64 w 0))
             (ptr-write-u64 output 8 (heap-read-u64 w 8))
             (ptr-write-u64 output 16 (heap-read-u64 w 16))
             (ptr-write-u64 output 24 (heap-read-u64 w 24)) 0)))

    (defun nl_native_car_v2 (env token input-index output-index)
      (let* ((input (nl_root_pin_slot_v2 env token input-index))
             (output (nl_root_pin_slot_v2 env token output-index)))
        (if (or (= input 0) (= output 0)) 2
          (let ((tag (heap-sexp-tag-word input)))
            (if (= tag 0)
                (seq
                 (ptr-write-u64 output 0 0)
                 (ptr-write-u64 output 8 0)
                 (ptr-write-u64 output 16 0)
                 (ptr-write-u64 output 24 0)
                 0)
              (if (= tag 7)
                  ;; Doc 215: read the car WORD and store it into the pinned
                  ;; OUTPUT root slot directly (no materialised view box).
                  (let ((car-word (nl_cons_car_word input)))
                    (if (= car-word 0) 2
                      (nl_native_store_word_v2 output car-word)))
                1))))))
    ;; Return status: 0 success, 2 malformed request.  Resolve every input
    ;; and output slot before reading values or calling the allocating
    ;; constructor.  The constructor receives only addresses inside this live
    ;; frame, so its allocations can collect without losing either input.
    (defun nl_native_cons_v2 (env token car-index cdr-index output-index)
      (let* ((car-slot (nl_root_pin_slot_v2 env token car-index))
             (cdr-slot (nl_root_pin_slot_v2 env token cdr-index))
             (output-slot (nl_root_pin_slot_v2 env token output-index)))
        (if (or (= car-slot 0) (= cdr-slot 0) (= output-slot 0))
            2
          (seq (extern-call nelisp_cons_construct
                            car-slot cdr-slot output-slot)
               0))))
    ;; Return status: 0 success, 1 wrong type, 2 malformed request.
    ;; The result of nl_cons_cdr_ptr is copied immediately into the already
    ;; authenticated output root; the input/output roots remain live during
    ;; any materialization allocation inside that accessor.
    (defun nl_native_cdr_v2 (env token input-index output-index)
      (let* ((input (nl_root_pin_slot_v2 env token input-index))
             (output (nl_root_pin_slot_v2 env token output-index)))
        (if (or (= input 0) (= output 0)) 2
          (let ((tag (heap-sexp-tag-word input)))
            (if (= tag 0)
                (seq
                 (ptr-write-u64 output 0 0)
                 (ptr-write-u64 output 8 0)
                 (ptr-write-u64 output 16 0)
                 (ptr-write-u64 output 24 0)
                 0)
              (if (= tag 7)
                  ;; Doc 215: read the cdr WORD and store it into the pinned
                  ;; OUTPUT root slot directly (no materialised view box).
                  (let ((cdr-word (nl_cons_cdr_word input)))
                    (if (= cdr-word 0) 2
                      (nl_native_store_word_v2 output cdr-word)))
                1))))))
    ;; GC: walk [region, top) in 32-byte steps, mark each slot like a root.
    (defun nl_gc_mark_rootstack_walk (p end)
      (if (>= p end) 0
          (seq (extern-call nl_gc_mark_slot p)
               (nl_gc_mark_rootstack_walk (+ p 32) end))))
    ;; The non-local exit stash is a root too.  TAG @268435480 and VAL
    ;; @268435512 keep the last thrown or signalled tag and value after the
    ;; exit completes (the flag @268435472 is cleared, the slots are not),
    ;; and `nl_n3_notify', `unwind-protect' and the bytecode handlers save
    ;; and restore them on every pass, cloning whatever they hold.  Unrooted,
    ;; a thrown cons dies once its catcher drops it and those clones then
    ;; bump the refcount word of whatever reuses the block.  Once cold-image
    ;; holes became collectable, the (FORM . END) cons that
    ;; `emacs-load--native-read-one' throws was freed this way; ~19k
    ;; increments in one collection interval rewrote the neighbouring block
    ;; header, and the global hash tables (special variables, symbol plists)
    ;; broke.
    (defun nl_gc_mark_rootstack ()
      (seq
       (if (= (heap-read-u64 (data-addr nl_rootstack_top) 0) 0) 0
         (nl_gc_mark_rootstack_walk (data-addr nl_rootstack_region)
                                    (heap-read-u64 (data-addr nl_rootstack_top) 0)))
       (nl_gc_mark_pinned_roots)
       (extern-call nl_gc_mark_slot 268435480)
       (extern-call nl_gc_mark_slot 268435512)))
    (defun nl_gc_mark_pinned_roots ()
      (let* ((base (data-addr nl_root_pin_region))
             (top (atomic-fetch-add
                   (+ (data-addr nl_root_pin_control) 24) 0)))
        (if (= (atomic-fetch-add (data-addr nl_root_pin_control) 0) 0)
            0
          (if (if (< top base) 1
                (if (> top (+ base 524288)) 1
                  (if (/= (logand (- top base) 31) 0) 1 0)))
              0
            (nl_gc_mark_rootstack_walk base top)))))
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
          (heap-read-u64 (nl_thread_registry_entry i) 0)
          (atomic-fetch-add (+ (nl_thread_registry_entry i) 8) 0))
         (nl_gc_mark_thread_roots_from (+ i 1) count))))
    (defun nl_gc_mark_thread_roots ()
      (nl_gc_mark_thread_roots_from
       0 (heap-read-u64 (data-addr nl_thread_registry) 0))))
  "AOT source for the Doc 152 §11.37 Stage 2 dynamic root stack.

Lazy-inits on the first main-thread `nl_root_reserve'.
Stage-3 evaluator callers use
the returned mutable entry directly as their eval/function result slot and
restore a saved `nl_root_mark' on every status path.  Tier-3b workers select
their registered private reserve by EvalCtx address.  See the Commentary for
the storage layout and soundness rationale.")

(provide 'nelisp-cc-rootstack)

;;; nelisp-cc-rootstack.el ends here
