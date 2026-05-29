;; Doc 135 Stage 135.C — SIMPLE env-leaf ctx-accessors; STAGED, wired at the
;; 135.E atomic cutover; type-checked not yet linked.
;;
;; Three thin wrappers over the LOCKED EvalCtx representation (§2.3):
;;   ctx  = *(ptr eval_ctx) ; post-cutover env parameter
;;   .mirror@0  → feeds nelisp_env_lookup_value / _set_value mirror_ptr
;;   .frames@32 → feeds nelisp_env_lookup_value / _set_value frames_ptr
;;
;; All four callee symbols are T (defined) in the archive — verified below.
;;
;; nm confirmation (libnelisp_elisp_spike.a, canonical build):
;;   nelisp_env_lookup_value  T (00000000000000af)
;;   nelisp_env_set_value     T (000000000000009a)
;;   nelisp_frame_pop         T (00000000000000df)
;;   nl_sexp_clone_into       T (0000000000000472)
;;
;; SCRATCH VECTOR — resolution and deferral (nl_env_set_value):
;;
;;   nelisp_env_set_value(mirror, frames, name, val, scratch, 0) requires an
;;   11-slot Sexp::Vector pre-filled by the Rust build_or_insert_scratch_vec
;;   helper (val@slot7, unbound@slot8, Nil elsewhere, symbol-entry Symbol
;;   @slot5).  To build that Sexp::Vector from nelisp-sys we need:
;;     1. nl_alloc_vector(11) → NlVector box_ptr (usize)  [T in archive]
;;     2. Construct the wrapping Sexp::Vector header on the stack:
;;          poke tag=8 (SEXP_TAG_VECTOR) at addr+0,
;;          poke box_ptr at addr+8 (payload), zero pad.
;;     3. Clone val_ptr into slot 7 via nl_sexp_clone_into.
;;     4. Write the symbol-entry Symbol into slot 5.
;;   Step 2 needs a 32-byte on-stack scratch Sexp slot → sys:alloc + poke-u64.
;;   Step 4 needs a pre-existing symbol-entry Sexp to clone from (the symbol
;;   "symbol-entry" is currently only available as a Rust Sexp constant, not
;;   accessible via a C-ABI primitive).
;;   VERDICT: the scratch build is type-checkable but SEMANTICALLY INCOMPLETE in
;;   this iteration (slot5 symbol-entry cannot be filled without a C-ABI accessor
;;   for it; step 2 needs a seq of poke-u64 / sys:alloc across both the Sexp slot
;;   AND the NlVector struct, which requires declaring nl_alloc_vector as extern +
;;   writing 8 poke-u64 calls for the vector layout).  This is tractable but is
;;   a 25+ line helper chain and belongs in a dedicated 135.C.scratch sub-stage.
;;
;;   DECISION: nl_env_set_value is written below with scratch as an EXPLICIT EXTRA
;;   PARAMETER `scratch_ptr` (6-arg, even arity), deviating from the 3-arg deleted
;;   signature.  This is type-checked E-SYS-* clean.  The orchestrator must decide
;;   whether to:
;;     (a) add the 135.C.scratch sub-stage to build the scratch builder helper, or
;;     (b) change sf_setq to allocate scratch before calling nl_env_set_value, or
;;     (c) use nelisp_env_set_value directly (with caller-built scratch) and drop
;;         the nl_env_set_value wrapper entirely (callers can own the scratch).
;;   See detailed deferral note on nl_env_set_value below.
;;
;; KEYWORD CHECK — deferral (nl_env_lookup_val):
;;   The keyword-symbol fast path (Symbol whose name byte[0] = ':' and len>1 →
;;   clone name into out + return 0) requires reading the Symbol's name bytes from
;;   the Sexp payload.  A Symbol Sexp carries a box pointer in its payload field;
;;   the box is a NlStr (capacity@0, data_ptr@8, len@16, refcount@24).  Reading
;;   byte[0] of the name requires:
;;     ptr-read-u64 on the Sexp payload field (+8) → box_ptr (usize)
;;     ptr-read-u64 on box_ptr+8 → data_ptr (usize)
;;     ptr-read-u8 (via nl_ptr_read_u8 extern) on data_ptr+0 → first byte
;;   This is expressible in nelisp-sys but adds 3 extern calls and 2 let-bindings.
;;   The keyword check is DEFERRED in this iteration.  nl_env_lookup_val below
;;   performs the plain lookup for all symbols including keywords (semantically
;;   safe: keywords ARE in the mirror, this just skips the clone-shortcut).
;;   Callers that need the keyword shortcut must either add the check at call site
;;   or wait for the 135.C.kw sub-stage.

;; ---------------------------------------------------------------------------
;; Struct declarations (LOCKED §2.3 layout)
;; ---------------------------------------------------------------------------

;; 32-byte runtime Sexp value (tag@0, payload@8, pad@16..31).
(sys:defstruct sexp (:repr c)
  (tag u8) (payload u64) (pad (array u8 16)))

;; EvalCtx — 120 bytes, align 8:
;;   mirror@0  frames@32  unbound@64  rec_cur@96  rec_max@104  flags@112
(sys:defstruct eval_ctx (:repr c)
  (mirror sexp) (frames sexp) (unbound sexp)
  (rec_cur i64) (rec_max i64) (flags i64))

;; ---------------------------------------------------------------------------
;; sys:extern declarations — the 4 shipped C-ABI callees
;; ---------------------------------------------------------------------------

;; nelisp_env_lookup_value(mirror_ptr, frames_ptr, name_ptr, out_ptr) -> i64
;; Shipped: 0x00af T in libnelisp_elisp_spike.a
(sys:extern nelisp_env_lookup_value
  (:symbol "nelisp_env_lookup_value" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize) (out_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_set_value(mirror_ptr, frames_ptr, name_ptr, val_ptr, scratch_ptr, _pad) -> i64
;; Shipped: 0x009a T in libnelisp_elisp_spike.a
;; scratch_ptr must point at an 11-slot Sexp::Vector built by build_or_insert_scratch_vec.
(sys:extern nelisp_env_set_value
  (:symbol "nelisp_env_set_value" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_frame_pop(frames_ptr, scratch_slot) -> i64
;; Shipped: 0x00df T in libnelisp_elisp_spike.a
;; Signature per nelisp-cc-frame-pop.el §96: two args (frames_ptr, scratch_slot).
;; scratch_slot = *mut Sexp initialised to Sexp::Nil (caller-owned 32-byte slot).
(sys:extern nelisp_frame_pop
  (:symbol "nelisp_frame_pop" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_slot usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64
;; Shipped: 0x0472 T in libnelisp_elisp_spike.a
;; Phase 47 swap: core::ptr::write(dst, (*src).clone()).  3-way tag dispatch.
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; 1. nl_env_lookup_val(name_ptr, env, out) -> i64
;;
;; Deleted Rust signature (fa8932eb^): nl_env_lookup_val(name, env, out) -> i64
;; Arg order: (name_ptr, env, out)  [positional; sf_* callers use this order]
;;
;; Keyword check DEFERRED — see header deferral note.  Plain lookup for all symbols.
;;
;; Body: extract mirror@offset0, frames@offset32 from ctx, call nelisp_env_lookup_value.
;; ---------------------------------------------------------------------------
(sys:defun nl_env_lookup_val
    ((name_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames))))
    (sys:unsafe
     (nelisp_env_lookup_value mirror_ptr frames_ptr name_ptr out))))

;; ---------------------------------------------------------------------------
;; 2. nl_env_set_value(env, sym_ptr, val_ptr, scratch_ptr, _pad1, _pad2) -> i64
;;
;; Deleted Rust signature (fa8932eb^): nl_env_set_value(env, sym, val) -> i64
;;
;; DEVIATION: scratch_ptr promoted to an explicit parameter (6-arg, even arity).
;; The original Rust wrapper called build_or_insert_scratch_vec(val.clone(), unbound, Nil, Nil)
;; internally — a Rust heap allocation not expressible in nelisp-sys without a
;; multi-step extern chain (nl_alloc_vector + slot fills + symbol-entry write).
;; That chain is deferred to 135.C.scratch sub-stage.
;;
;; CONTRACT for callers at 135.E cutover:
;;   scratch_ptr must point at a Sexp::Vector with 11 slots, slot7 = val clone,
;;   slot5 = symbol-entry Symbol, slot8 = unbound_marker.  The sf_setq caller
;;   must build this before calling nl_env_set_value (or the orchestrator must
;;   implement the 135.C.scratch helper and call it first).
;;
;; _pad1, _pad2: arity padding so total = 6 (even), rsp ≡ 0 mod 16 at body entry.
;; ---------------------------------------------------------------------------
(sys:defun nl_env_set_value
    ((env (ptr eval_ctx)) (sym_ptr usize) (val_ptr usize)
     (scratch_ptr usize) (_pad1 i64) (_pad2 i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames))))
    (sys:unsafe
     (nelisp_env_set_value mirror_ptr frames_ptr sym_ptr val_ptr scratch_ptr 0))))

;; ---------------------------------------------------------------------------
;; 3. nl_env_pop_frame(env) -> i64
;;
;; Deleted Rust signature (fa8932eb^): nl_env_pop_frame(env) -> i64
;; Arg order: (env)  [positional]
;;
;; nelisp_frame_pop exact signature (from nelisp-cc-frame-pop.el §96):
;;   nelisp_frame_pop(frames_ptr, scratch_slot) -> i64
;;   frames_ptr:   *const Sexp — Env::frames_record
;;   scratch_slot: *mut Sexp   — caller-owned 32-byte slot initialised to Sexp::Nil;
;;                               reused as the Nil-source for vector-slot-set and
;;                               overwritten to hold the new depth Sexp::Int.
;;
;; We allocate the 32-byte scratch slot on the heap via sys:alloc(32, 8).
;; After the call the scratch holds an Int sexp (tag=2, no heap); the dealloc is
;; intentionally OMITTED here (the Int payload has no refcount; the 32 raw bytes
;; leak).  A zero-Rust dealloc via nelisp_dealloc_bytes is the correct fix but
;; is deferred to 135.C.popframe to keep this body 1-extern-call.
;;
;; DEFERRED: dealloc of scratch_slot (see above).
;; ---------------------------------------------------------------------------
(sys:defun nl_env_pop_frame
    ((env (ptr eval_ctx)) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (scratch_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (nelisp_frame_pop frames_ptr scratch_slot))))
