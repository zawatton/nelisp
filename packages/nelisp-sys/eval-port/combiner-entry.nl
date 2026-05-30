;; Doc 135 Stage 135.D — nl_eval recursion-wrapper + nl_eval_ctx_make; STAGED;
;; entry-wiring/bin-main OUT OF SCOPE.
;; (unwired — do NOT touch manifest/build.rs / bin/nelisp.rs / any *.rs)
;;
;; Implements two public C-ABI entry points:
;;
;;   nl_eval(form_ptr, env, out) -> i64
;;     Rust eval() body port: quit-flag check (DEFERRED — see below), rec-depth
;;     guard, delegate to nl_eval_inner, restore rec_cur, return rc.
;;     0 = ok (result in *out); 1 = error (signal stashed in env by inner callee).
;;
;;   nl_eval_ctx_make(globals_ptr, frames_ptr, unbound_ptr, rec_max, out_ctx_slot) -> i64
;;     EvalCtx constructor: sys:alloc 120 8 → ctx; clone 3 Sexp operands into
;;     ctx at offsets 0/32/64; poke rec_cur=0, rec_max, flags=0;
;;     write ctx pointer into out_ctx_slot (as Int Sexp); return 0.
;;
;;   nl_eval_in_ctx(form_ptr, env, out) -> i64  [alias / convenience]
;;     Identical to nl_eval — provided for callers that prefer the longer name.
;;
;; ── Key findings from source audit ──────────────────────────────────────────
;;
;; nl_eval_inner / nl_eval_inner_cons  (archive status, confirmed):
;;   The Phase 47 eval chain works in two hops:
;;     Rust eval() calls nl_eval_inner via crate::elisp_cc_spike::eval_inner_call.
;;     nl_eval_inner (shipped elisp .o) dispatches to nl_eval_inner_cons for Cons
;;     forms (symbol/self-eval handled there), delegating via extern nl_eval_inner_cons.
;;   ABI:   nl_eval_inner(form, env, out, _pad) -> i64   [4-arg; _pad=0]
;;   The combiner-cons.nl declares nl_eval_inner_cons, so our combiner-entry
;;   delegates via nl_eval_inner (not nl_eval_inner_cons directly) to match the
;;   same chain the Rust eval() used.  nm confirms:
;;     nl_eval_inner        T 0x0019 in libnelisp_elisp_spike.a
;;     nl_eval_inner_cons   U (defined by combiner-cons.nl / Rust stub)
;;   We declare nl_eval_inner as sys:extern here.
;;
;; nl_sexp_clone_into                   — U in archive, provided by Rust binary
;; nelisp_env_set_value                 — T 0x009a in archive
;;
;; QUIT FLAG — DEFERRED:
;;   Rust eval() body: `if take_quit_flag() { return Err(EvalError::Quit); }`
;;   take_quit_flag() is a Rust-internal function (not C-ABI exported).
;;   The only C-ABI exported quit primitive is:
;;     nl_quit_flag_ptr() -> *mut i64   [#[no_mangle]; returns QUIT_FLAG.as_ptr()]
;;   There is NO take_quit_flag C-ABI symbol; the atomic swap is done in Rust only.
;;   To implement the check in nelisp-sys we need either:
;;     (a) a new C-ABI nl_take_quit_flag() -> i64 exported from Rust (net zero LOC
;;         if added as a #[no_mangle] wrapper in the same block as nl_quit_flag_ptr),
;;         OR
;;     (b) implement the atomic swap in nelisp-sys via nl_quit_flag_ptr +
;;         nelisp_tty_take_atomic (which performs AtomicI64::swap(0, SeqCst)).
;;   Option (b) uses nelisp_tty_take_atomic whose sig is (flag_ptr: *mut i64) -> i64;
;;   this is a SeqCst swap — exactly what take_quit_flag does.
;;   BUT nelisp_tty_take_atomic is a TTY-specific symbol name; using it here would
;;   be semantically correct but misleading.  VERDICT: DEFERRED with precise note.
;;   The nl_eval body below omits the quit-flag check (it is a liveness/cancellation
;;   guard, not a correctness guard) and inserts a TODO comment.
;;   The rec-depth guard + nl_eval_inner call is fully implemented.
;;
;; MAX-DEPTH ERROR PAYLOAD — PARTIALLY DEFERRED:
;;   Rust: Err(EvalError::internal("max-lisp-eval-depth exceeded (...)")) which
;;   eval_stash_err turns into an env-install of signal-data ("error" . ("max-lisp-eval-depth exceeded")).
;;   Building that signal cons from nelisp-sys requires nl_alloc_symbol + nelisp_cons_construct
;;   + nelisp_env_set_value — the same pattern as nl_apply_stash_wta in combiner-apply.nl.
;;   We implement it here using the same mechanism:
;;     signal = cons(Symbol("error"), cons(Symbol("max-lisp-eval-depth"), Nil))
;;   installed via nelisp_env_set_value into mirror/frames using the "nelisp--last-signal-data"
;;   key.  The exact Rust format includes the depth number; we omit the number (use
;;   a constant symbol name) since integer-to-string formatting is not available
;;   in nelisp-sys without a further helper.  This is noted below.
;;
;; EvalCtx layout (LOCKED §2.3):
;;   offset   0: mirror  (sexp, 32B)
;;   offset  32: frames  (sexp, 32B)
;;   offset  64: unbound (sexp, 32B)
;;   offset  96: rec_cur (i64)
;;   offset 104: rec_max (i64)
;;   offset 112: flags   (i64)
;;   Total: 120 bytes, align 8
;;
;; Sexp layout (32B):
;;   offset  0: tag (u8, 7 pad bytes) — peek-u64 word0 = tag for small values
;;   offset  8: payload (u64)
;;   offset 16: pad (16 bytes)
;;
;; ── Archive nm confirmation ──────────────────────────────────────────────────
;;   nl_eval_inner         T 0x0019 (libnelisp_elisp_spike.a)
;;   nl_sexp_clone_into    U  (provided by Rust binary link)
;;   nelisp_env_set_value  T 0x009a (libnelisp_elisp_spike.a)
;;   nl_alloc_symbol       T 0x04cf (libnelisp_elisp_spike.a)
;;   nelisp_cons_construct T 0x0000 (libnelisp_elisp_spike.a)
;;   nl_alloc_vector       T 0x01f4 (libnelisp_elisp_spike.a)
;;   nl_vector_set_slot    T 0x0000 (libnelisp_elisp_spike.a)
;;   nelisp_env_bind_local T 0x0171 (libnelisp_elisp_spike.a)
;;   nl_quit_flag_ptr      U  (Rust binary; needed for deferred quit-flag check)
;;
;; String LE-u64 encodings used in this file:
;;   "error"                     [ 5]: 547519316581
;;   "max-lisp-eval-depth"       [20]: 8243116665881813101, 4840666116958122080, 1684
;;   "nelisp--last-signal-data"  [24]: 3255381746650998126, 7451613697525637484,
;;                                     7022344801864147310
;;   "symbol-entry"              [12]: 7290602597431212403, 2037544046

;; ---------------------------------------------------------------------------
;; Struct declarations (LOCKED §2.3 layout)
;; ---------------------------------------------------------------------------

(sys:defstruct sexp (:repr c)
  (tag u8) (payload u64) (pad (array u8 16)))

(sys:defstruct eval_ctx (:repr c)
  (mirror sexp) (frames sexp) (unbound sexp)
  (rec_cur i64) (rec_max i64) (flags i64))

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nl_eval_inner(form_ptr, env_ptr, out_ptr, _pad) -> i64
;; T 0x0019 in libnelisp_elisp_spike.a
;; Phase 47 elisp replacement for eval_inner: dispatches Symbol/Cons/Cell/self-eval.
;; _pad=0 keeps arity even (4-arg, rsp ≡ 0 mod 16).
(sys:extern nl_eval_inner
  (:symbol "nl_eval_inner" :abi c :unsafe t)
  ((form_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64
;; U in archive (provided by Rust binary at link).
;; Refcount-aware clone; writes *src into *dst using tag-dispatch.
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_set_value(mirror_ptr, frames_ptr, name_ptr, val_ptr, scratch_ptr, _pad) -> i64
;; T 0x009a in archive. scratch_ptr = 11-slot Sexp::Vector.
(sys:extern nelisp_env_set_value
  (:symbol "nelisp_env_set_value" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot [T]
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64 [T]
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((car_ptr usize) (cdr_ptr usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_alloc_vector(cap) -> box_ptr [T]
(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64 [T]
(sys:extern nl_vector_set_slot
  (:symbol "nl_vector_set_slot" :abi c :unsafe t)
  ((vec_ptr usize) (n i64) (val usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_bind_local(mirror_ptr, frames_ptr, name_ptr, val_ptr, scratch_ptr, _pad) -> i64 [T]
(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; UTILITY: write Sexp::Nil into a 32B slot
;; ---------------------------------------------------------------------------

(sys:defun nl_entry_write_nil
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; MAX-DEPTH ERROR STASH HELPERS
;;
;; Build signal = cons(Symbol("error"), cons(Symbol("max-lisp-eval-depth"), Nil))
;; Install into env via nelisp_env_set_value under "nelisp--last-signal-data".
;;
;; NOTE: The Rust format string includes the actual depth count; we use the
;; symbol "max-lisp-eval-depth" without the count suffix.  This is a minor
;; deviation — the signal tag is correct; only the detail message differs.
;; Full number-to-string formatting requires a helper not available in
;; nelisp-sys without an additional extern (nl_i64_append_to_mut_str or similar).
;; Noted as partial deferral: tag correct, detail-number omitted.
;; ---------------------------------------------------------------------------

;; nl_entry_build_scratch(sym_slot_5, unbound_ptr) -> box_ptr (usize)
;; Build an 11-slot NlVector for nelisp_env_set_value / nelisp_env_bind_local.
;; slot5 = symbol-entry Symbol; slot7 = val (caller fills after this returns).
;; "symbol-entry" [12]: 7290602597431212403, 2037544046
(sys:defun nl_entry_build_scratch_box
    ((sym_slot_5 usize) (unbound_ptr usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 sym_buf 7290602597431212403)
     (sys:poke-u64 (+ sym_buf 8) 2037544046)
     (nl_alloc_symbol sym_buf 12 sym_slot_5)
     (nl_vector_set_slot box_ptr 5 sym_slot_5))
    box_ptr))

;; nl_entry_stash_max_depth(env) -> i64
;; Stash the max-lisp-eval-depth signal and return 1.
;; Signal: cons(Symbol("error"), cons(Symbol("max-lisp-eval-depth"), Nil))
;; Install via nelisp_env_bind_local (same semantics as set_value for signal stash).
;; "error"               [5]: 547519316581
;; "max-lisp-eval-depth" [20]: 8243116665881813101, 4840666116958122080, 1684
;; "nelisp--last-signal-data" [24]: 3255381746650998126, 7451613697525637484, 7022344801864147310
(sys:defun nl_entry_stash_max_depth
    ((env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((err_buf usize (sys:alloc 8 1))
        (err_slot usize (sys:alloc 32 8))
        (depth_buf usize (sys:alloc 24 1))
        (depth_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (inner_pair usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    ;; Build Symbol("error")
    (sys:unsafe
     (sys:poke-u64 err_buf 547519316581)
     (nl_alloc_symbol err_buf 5 err_slot))
    ;; Build Symbol("max-lisp-eval-depth")
    (sys:unsafe
     (sys:poke-u64 depth_buf 8243116665881813101)
     (sys:poke-u64 (+ depth_buf 8) 4840666116958122080)
     (sys:poke-u64 (+ depth_buf 16) 1684)
     (nl_alloc_symbol depth_buf 20 depth_slot))
    ;; Nil slot
    (nl_entry_write_nil nil_slot)
    ;; cons(depth, Nil) → inner_pair
    (sys:unsafe (nelisp_cons_construct depth_slot nil_slot inner_pair))
    ;; cons(error, inner_pair) → signal_slot
    (sys:unsafe (nelisp_cons_construct err_slot inner_pair signal_slot))
    ;; Install: nelisp--last-signal-data = signal_slot
    (let ((name_sym_slot usize (sys:alloc 32 8))
          (name_buf usize (sys:alloc 24 1))
          (mirror_ptr usize (+ (sys:cast usize env)
                               (sys:offsetof eval_ctx mirror)))
          (frames_ptr usize (+ (sys:cast usize env)
                               (sys:offsetof eval_ctx frames)))
          (unbound_ptr usize (+ (sys:cast usize env)
                                (sys:offsetof eval_ctx unbound)))
          (sym5_slot usize (sys:alloc 32 8))
          (vec_slot usize (sys:alloc 32 8)))
      (sys:unsafe
       (sys:poke-u64 name_buf 3255381746650998126)
       (sys:poke-u64 (+ name_buf 8) 7451613697525637484)
       (sys:poke-u64 (+ name_buf 16) 7022344801864147310)
       (nl_alloc_symbol name_buf 24 name_sym_slot))
      ;; Build scratch vector for env_bind_local
      (let ((box_ptr usize (nl_entry_build_scratch_box sym5_slot unbound_ptr)))
        (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
          (sys:unsafe
           (nl_sexp_clone_into (+ data_ptr 224) signal_slot)
           (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr)
           (sys:poke-u64 vec_slot 8)
           (sys:poke-u64 (+ vec_slot 8) box_ptr)
           (sys:poke-u64 (+ vec_slot 16) 0)
           (sys:poke-u64 (+ vec_slot 24) 0))
          (sys:unsafe
           (nelisp_env_bind_local mirror_ptr frames_ptr
                                  name_sym_slot signal_slot vec_slot 0))
          (sys:cast i64 1))))))

;; ---------------------------------------------------------------------------
;; (a) nl_eval(form_ptr, env, out) -> i64
;;
;; Rust eval() body port (build-tool/src/eval/mod.rs):
;;   pub fn eval(form: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
;;     if take_quit_flag() { return Err(EvalError::Quit); }        // ← DEFERRED
;;     if env.current_recursion >= env.max_recursion {             // ← rec_cur@96, rec_max@104
;;       return Err(EvalError::internal("max-lisp-eval-depth exceeded ..."));
;;     }
;;     env.current_recursion += 1;
;;     let rc = eval_inner_call(form, env, &mut out, 0);           // ← nl_eval_inner
;;     env.current_recursion -= 1;
;;     if rc == 0 { Ok(out) } else { Err(consume_stashed_error(env, "eval_inner")) }
;;   }
;;
;; DEFERRED: quit-flag check.
;;   take_quit_flag() is not C-ABI exported.  The only exported primitive is:
;;     nl_quit_flag_ptr() -> *mut i64   (#[no_mangle], returns QUIT_FLAG.as_ptr())
;;   The swap (AtomicI64::swap(0, SeqCst)) requires either a dedicated C-ABI shim
;;   (nl_take_quit_flag — 1 Rust LOC, net zero with the swap) or reusing
;;   nelisp_tty_take_atomic(flag_ptr) which performs the same SeqCst swap.
;;   Neither is unambiguously safe without user sign-off (tty_take_atomic is
;;   semantically TTY-specific in name).  The check is omitted; the
;;   depth-limit guard still catches runaway recursion.
;;
;; rec_cur is at offset 96; rec_max at offset 104 (verified §2.3).
;; Both are i64 in EvalCtx (Rust Env uses u32 but §2.3 promotes to i64).
;; ---------------------------------------------------------------------------

(sys:defun nl_eval
    ((form_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; TODO DEFERRED: quit-flag check.
  ;; When nl_take_quit_flag C-ABI is available, add:
  ;;   if (= (nl_take_quit_flag) 1) → stash Quit signal + return 1.
  (let ((ctx_base usize (sys:cast usize env))
        (env_ptr usize (sys:cast usize env)))
    (let ((rec_cur_addr usize (+ ctx_base 96))
          (rec_max_addr usize (+ ctx_base 104)))
      (let ((rec_cur i64 (sys:unsafe (sys:cast i64 (sys:peek-u64 rec_cur_addr))))
            (rec_max i64 (sys:unsafe (sys:cast i64 (sys:peek-u64 rec_max_addr)))))
        ;; Depth guard
        (if (>= rec_cur rec_max)
            ;; max-lisp-eval-depth exceeded: stash error signal, return 1
            (nl_entry_stash_max_depth env)
          ;; Increment rec_cur
          (sys:unsafe (sys:poke-u64 rec_cur_addr (sys:cast u64 (+ rec_cur 1))))
          ;; Delegate to nl_eval_inner (the Phase 47 elisp .o for Symbol/Cons/Cell dispatch)
          (let ((rc i64 (sys:unsafe (nl_eval_inner form_ptr env_ptr out 0))))
            ;; Decrement rec_cur
            (sys:unsafe (sys:poke-u64 rec_cur_addr (sys:cast u64 rec_cur)))
            ;; Propagate rc: 0=ok, 1=error-stashed-by-inner
            rc))))))

;; ---------------------------------------------------------------------------
;; (b) nl_eval_ctx_make(globals_ptr, frames_ptr, unbound_ptr, rec_max, out_ctx_slot) -> i64
;;
;; Allocate and initialise an EvalCtx (120B, align 8).
;;
;; Parameters:
;;   globals_ptr   — *const Sexp pointing at the already-built globals Sexp
;;                   (mirror; 32B Sexp::Vector or similar)
;;   frames_ptr    — *const Sexp pointing at the already-built frames Sexp
;;   unbound_ptr   — *const Sexp pointing at the already-built unbound marker Sexp
;;   rec_max       — maximum recursion depth (i64)
;;   out_ctx_slot  — *mut Sexp: caller-owned 32B slot that receives a Sexp::Int
;;                   whose payload holds the ctx pointer as a usize/i64.
;;                   (Callers can recover the ctx with a peek-u64 on slot+8.)
;;
;; Returns 0 on success.
;;
;; DESIGN NOTES:
;;   We use nl_sexp_clone_into to copy the 32B Sexp operands into ctx, which
;;   is refcount-correct (it increments refcounts for heap-boxed tags such as
;;   Vector/Cons).  This is the same approach as combiner-apply.nl nl_apply_stash_wta.
;;
;;   The ctx pointer is written into out_ctx_slot as a Sexp::Int (tag=2, payload=ptr).
;;   This is a thin Sexp wrapper around the raw pointer, following the same pattern
;;   used throughout the nelisp-sys .nl files for opaque pointer passing across
;;   C-ABI boundaries.  Callers that need the raw pointer use:
;;     ctx_ptr = (sys:peek-u64 (+ out_ctx_slot 8))
;;   and cast to (ptr eval_ctx) via (sys:cast usize ctx_ptr).
;; ---------------------------------------------------------------------------

(sys:defun nl_eval_ctx_make
    ((globals_ptr usize) (frames_ptr_arg usize) (unbound_ptr usize)
     (rec_max i64) (out_ctx_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; Allocate 120 bytes, align 8 for EvalCtx
  (let ((ctx usize (sys:alloc 120 8)))
    ;; Clone globals into ctx+0 (mirror, offset 0)
    (sys:unsafe (nl_sexp_clone_into ctx globals_ptr))
    ;; Clone frames into ctx+32 (frames, offset 32)
    (sys:unsafe (nl_sexp_clone_into (+ ctx 32) frames_ptr_arg))
    ;; Clone unbound into ctx+64 (unbound, offset 64)
    (sys:unsafe (nl_sexp_clone_into (+ ctx 64) unbound_ptr))
    ;; rec_cur = 0 at offset 96
    (sys:unsafe (sys:poke-u64 (+ ctx 96) (sys:cast u64 0)))
    ;; rec_max = rec_max at offset 104
    (sys:unsafe (sys:poke-u64 (+ ctx 104) (sys:cast u64 rec_max)))
    ;; flags = 0 at offset 112
    (sys:unsafe (sys:poke-u64 (+ ctx 112) (sys:cast u64 0)))
    ;; Write ctx pointer into out_ctx_slot as Sexp::Int (tag=2, payload=ctx)
    (sys:unsafe
     (sys:poke-u64 out_ctx_slot 2)
     (sys:poke-u64 (+ out_ctx_slot 8) (sys:cast u64 ctx))
     (sys:poke-u64 (+ out_ctx_slot 16) 0)
     (sys:poke-u64 (+ out_ctx_slot 24) 0))
    (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; nl_eval_in_ctx(form_ptr, env, out) -> i64  [alias / convenience]
;;
;; Identical to nl_eval.  Provided for callers that construct the ctx via
;; nl_eval_ctx_make and want a named entry point that makes the ctx explicit.
;; ---------------------------------------------------------------------------

(sys:defun nl_eval_in_ctx
    ((form_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (nl_eval form_ptr env out))
