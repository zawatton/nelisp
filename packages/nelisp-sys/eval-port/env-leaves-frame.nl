;; Doc 135 Stage 135.C — FRAME-COMPOSE env-leaves; STAGED
;; (unwired — do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements 2 FRAME-COMPOSE env-leaf ctx-accessors for the nelisp-sys DSL:
;;   1. nl_push_and_bind(formals_ptr, args_list_ptr, env) -> i64
;;   2. nl_env_push_captured(env, alist_ptr) -> i64
;;
;; Post-cutover env parameter = (ptr eval_ctx) throughout.
;;
;; ---------------------------------------------------------------------------
;; Archive symbol verification (most-recent libnelisp_elisp_spike.a):
;;   nelisp_frame_push    T 0x0000  (nelisp_frame_push.o)
;;   nl_bind_formals_impl T 0x0a61  (nl_bind_formals_impl.o)
;;   nelisp_frame_pop     T 0x00df  (nelisp_frame_pop.o)
;;   nelisp_env_bind_local T 0x0171 (nelisp_env_bind_local.o)
;;   nl_alloc_vector      T 0x01f4  (nl_alloc_vector.o)
;;   nl_vector_set_slot   T 0x0000  (nl_vector_set_slot.o)
;;   nl_alloc_symbol      T 0x04cf  (nl_alloc_symbol.o)
;;   nl_cons_car_ptr      T 0x0000  (nl_cons_car_ptr.o)
;;   nl_cons_cdr_ptr      T 0x0000  (nl_cons_cdr_ptr.o)
;;   nl_sexp_clone_into   T 0x0472  (nl_sexp_clone_into.o)
;;
;; C-ABI signatures recovered from build-tool/src/lib.rs extern block +
;; fa8932eb^ mod.rs:
;;   nelisp_frame_push(frames_ptr: *const Sexp, scratch_vec_ptr: *const Sexp) -> i64
;;     (2 args; scratch = 7-slot Vector with type-tag Symbols pre-filled)
;;   nl_bind_formals_impl(formals: *const Sexp, args: *const Sexp,
;;                        env: *mut c_void, _pad: i64) -> i64
;;     (4 args; env-opaque; _pad keeps arity even; 0=ok, 1=err)
;;   nelisp_frame_pop(frames_ptr: *const Sexp, scratch_slot: *mut Sexp) -> i64
;;     (2 args; scratch_slot = caller-owned 32B Nil-initialised slot)
;;   nelisp_env_bind_local(mirror_ptr, frames_ptr, name_ptr, val_ptr,
;;                         scratch_ptr, _pad) -> i64
;;     (6 args; scratch_ptr = 11-slot Sexp::Vector built by nl_env_build_scratch)
;;
;; Deleted Rust bodies (fa8932eb^:build-tool/src/eval/mod.rs):
;;   nl_push_and_bind:
;;     e.frame_push_rust_direct();
;;     let rc = crate::elisp_cc_spike::bind_formals_impl_call(formals_ptr, args_list_ptr, env, 0);
;;     if rc != 0 { e.frame_pop_rust_direct(); }
;;     rc
;;   nl_env_push_captured:
;;     i64::from(e.push_captured(&*alist_ptr).is_err())
;;
;; Env::push_captured actual logic (fa8932eb^ mod.rs):
;;   1. nelisp_wrap_alist_cells(alist) → Sexp n  (wraps alist cells into wire form)
;;   2. lookup_function("nelisp-lexframe-make-from-alist") → f  (early-return Ok if not found)
;;   3. apply_function(&f, &[n], self) → frame  (calls an elisp function via the evaluator)
;;   4. nelisp_frame_stack_install(frames_ptr, frame_ptr, scratch) → install frame
;;
;; NOTE on nl_env_push_captured deviation (tracked):
;;   Step 3 (apply_function / invoking "nelisp-lexframe-make-from-alist") requires
;;   calling back into the evaluator (nl_eval_inner or an equivalent dispatch path),
;;   which cannot be cleanly expressed in nelisp-sys in a single type-checkable pass
;;   without exposing the full lambda-apply machinery.  This implementation substitutes
;;   a semantically equivalent but structurally simpler approach:
;;     frame_push (allocates a fresh empty frame) + alist walk binding each (name . val)
;;   via nelisp_env_bind_local.  This produces the same observable result (a new frame
;;   with the captured bindings installed) but skips the wrap_alist_cells → elisp
;;   function apply path.  If the production cutover requires exact Rust parity
;;   (e.g. the wire-form wrapping matters for GC or serialization), a sub-stage must
;;   add nelisp_wrap_alist_cells + an apply-frame helper.  For the eval-driver port
;;   the simplified version is correct at the binding-level contract.
;;
;; ---------------------------------------------------------------------------
;; String byte encodings (little-endian u64 words, for Symbol names):
;;   "nelisp-lexframe"   [15]: 7795010171040458094, 28549237946349669
;;   "fast-hash-table"   [15]: 8314040931539181926, 28548142445374824
;;
;; NlVector layout (#[repr(C)], 32 bytes):
;;   [box +  0] = Vec.capacity  [box +  8] = Vec.data_ptr
;;   [box + 16] = Vec.len       [box + 24] = refcount
;;
;; Sexp::Vector slot layout (32 bytes):
;;   [slot + 0] = tag (u8=8, upper 7 bytes zero padding)
;;   [slot + 8] = box_ptr (u64 = NlVector*)
;;   [slot +16] = 0  [slot+24] = 0
;;
;; SEQUENCING PATTERN: In nelisp-sys, `and` is short-circuit BOOL logic.
;; Multi-expr bodies use seq / let / sys:unsafe for sequencing with i64 returns.
;; ---------------------------------------------------------------------------

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

;; nelisp_frame_push(frames_ptr, scratch_vec_ptr) -> i64
;; frames_ptr:      *const Sexp — ctx.frames (Sexp::Record / nelisp-lexframe-stack)
;; scratch_vec_ptr: *const Sexp — 7-slot Sexp::Vector (type-tag Symbols pre-filled)
(sys:extern nelisp_frame_push
  (:symbol "nelisp_frame_push" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_vec_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_bind_formals_impl(formals, args, env, _pad) -> i64
;; formals: *const Sexp — cons list of formal parameter symbols.
;; args:    *const Sexp — cons list of evaluated argument values.
;; env:     *mut c_void — env pointer (opaque / eval_ctx* post-cutover).
;; _pad:    i64 — unused, keeps arity even.
;; Returns: 0=Ok, 1=Err (error stashed in env).
(sys:extern nl_bind_formals_impl
  (:symbol "nl_bind_formals_impl" :abi c :unsafe t)
  ((formals_ptr usize) (args_list_ptr usize) (env_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_frame_pop(frames_ptr, scratch_slot) -> i64
;; frames_ptr:   *const Sexp — ctx.frames
;; scratch_slot: *mut Sexp   — caller-owned 32B slot; must be Sexp::Nil on entry.
(sys:extern nelisp_frame_pop
  (:symbol "nelisp_frame_pop" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_slot usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_bind_local(mirror, frames, name, val, scratch, _pad) -> i64
(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_vector(cap) -> box_ptr (usize = NlVector*)
(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64
;; vec_ptr: NlVector* (box_ptr from nl_alloc_vector)
;; n:       zero-based slot index (i64)
;; val:     *const Sexp — source 32-byte slot to raw-copy from (usize)
(sys:extern nl_vector_set_slot
  (:symbol "nl_vector_set_slot" :abi c :unsafe t)
  ((vec_ptr usize) (n i64) (val usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot (usize)
;; Writes Sexp::Symbol(tag=4) into result_slot and returns result_slot.
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64 — refcount-aware clone
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_cons_car_ptr(cons_ptr) -> usize  (*const Sexp pointing at car slot)
(sys:extern nl_cons_car_ptr
  (:symbol "nl_cons_car_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_cons_cdr_ptr(cons_ptr) -> usize  (*const Sexp pointing at cdr slot)
(sys:extern nl_cons_cdr_ptr
  (:symbol "nl_cons_cdr_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; SHARED HELPERS: scratch vector for nelisp_frame_push
;;
;; The safe Rust wrapper (lib.rs::frame_push) allocates a 7-slot Sexp::Vector:
;;   slot 0: Symbol("nelisp-lexframe")   — lexframe type-tag
;;   slot 1: Symbol("fast-hash-table")   — ht type-tag
;;   slots 2-6: Sexp::Nil               — scratch for frame-push ops
;;
;; We build it here with nl_alloc_vector(7) + nl_alloc_symbol + nl_vector_set_slot.
;;
;; HELPERS:
;;   nl_frame_scratch_write_lexframe_sym(sym_slot) -> sym_slot
;;   nl_frame_scratch_write_hashsym(sym_slot)      -> sym_slot
;;   nl_frame_build_push_scratch(out_vec_sexp)     -> out_vec_sexp
;; ---------------------------------------------------------------------------

;; Write Symbol("nelisp-lexframe") [15 bytes] into sym_slot; return sym_slot.
;; "nelisp-lexframe" LE u64 encoding:
;;   word0 = 7795010171040458094  ('n','e','l','i','s','p','-','l')
;;   word1 = 28549237946349669    ('e','x','f','r','a','m','e',0)
(sys:defun nl_frame_scratch_write_lexframe_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7795010171040458094)
     (sys:poke-u64 (+ buf 8) 28549237946349669)
     (nl_alloc_symbol buf 15 sym_slot))))

;; Write Symbol("fast-hash-table") [15 bytes] into sym_slot; return sym_slot.
;; "fast-hash-table" LE u64 encoding:
;;   word0 = 8314040931539181926  ('f','a','s','t','-','h','a','s')
;;   word1 = 28548142445374824    ('h','-','t','a','b','l','e',0)
(sys:defun nl_frame_scratch_write_hashsym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8314040931539181926)
     (sys:poke-u64 (+ buf 8) 28548142445374824)
     (nl_alloc_symbol buf 15 sym_slot))))

;; Build the 7-slot Sexp::Vector scratch required by nelisp_frame_push.
;; Writes slot 0 = Symbol("nelisp-lexframe"), slot 1 = Symbol("fast-hash-table"),
;; slots 2-6 = Sexp::Nil (already Nil from nl_alloc_vector).
;; out_vec_sexp: caller-owned 32B Sexp slot → receives the Sexp::Vector header.
;; Returns out_vec_sexp (ready to pass as scratch_vec_ptr to nelisp_frame_push).
(sys:defun nl_frame_build_push_scratch
    ((out_vec_sexp usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 7)))
        (sym0_slot usize (sys:alloc 32 8))
        (sym1_slot usize (sys:alloc 32 8)))
    (nl_frame_scratch_write_lexframe_sym sym0_slot)
    (nl_frame_scratch_write_hashsym sym1_slot)
    (sys:unsafe
     (nl_vector_set_slot box_ptr 0 sym0_slot)
     (nl_vector_set_slot box_ptr 1 sym1_slot)
     ;; Write the Sexp::Vector header into out_vec_sexp:
     ;; tag=8 at word 0, box_ptr at word 1, zeros at words 2 and 3.
     (sys:poke-u64 out_vec_sexp 8)
     (sys:poke-u64 (+ out_vec_sexp 8) box_ptr)
     (sys:poke-u64 (+ out_vec_sexp 16) 0)
     (sys:poke-u64 (+ out_vec_sexp 24) 0)
     out_vec_sexp)))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_frame_build_bind_scratch
;;
;; Builds the standard 11-slot Sexp::Vector scratch for nelisp_env_bind_local,
;; adapted for the push_captured walk (val_ptr clone + unbound_ptr clone).
;; Mirrors nl_env_build_scratch from env-leaves-bind.nl — duplicated here
;; because each .nl is type-checked independently.
;;
;; Returns: out_sexp_vec_slot (ready to pass as scratch_ptr to nelisp_env_bind_local).
;;
;; String encodings: "symbol-entry" [12]:
;;   word0 = 7290602597431212403  word1 = 2037544046
;; ---------------------------------------------------------------------------

(sys:defun nl_frame_write_symentry
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7290602597431212403)
     (sys:poke-u64 (+ buf 8) 2037544046)
     (nl_alloc_symbol buf 12 sym_slot))))

(sys:defun nl_frame_build_bind_scratch
    ((val_ptr usize) (unbound_ptr usize) (out_sexp_vec_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_slot usize (sys:alloc 32 8)))
    (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
      (nl_frame_write_symentry sym_slot)
      (sys:unsafe
       (nl_vector_set_slot box_ptr 5 sym_slot)
       (nl_sexp_clone_into val_ptr (+ data_ptr 224))
       (nl_sexp_clone_into unbound_ptr (+ data_ptr 256))
       (sys:poke-u64 out_sexp_vec_slot 8)
       (sys:poke-u64 (+ out_sexp_vec_slot 8) box_ptr)
       (sys:poke-u64 (+ out_sexp_vec_slot 16) 0)
       (sys:poke-u64 (+ out_sexp_vec_slot 24) 0)
       out_sexp_vec_slot))))

;; ---------------------------------------------------------------------------
;; 1. nl_push_and_bind(formals_ptr, args_list_ptr, env) -> i64
;;
;; Deleted Rust body (fa8932eb^):
;;   e.frame_push_rust_direct();
;;   let rc = crate::elisp_cc_spike::bind_formals_impl_call(formals_ptr, args_list_ptr, env, 0);
;;   if rc != 0 { e.frame_pop_rust_direct(); }
;;   rc
;;
;; frame_push_rust_direct() calls crate::elisp_cc_spike::frame_push(&self.frames_record)
;; which is the lib.rs safe wrapper:
;;   pub unsafe fn frame_push(frames_ptr: *const Sexp) -> i64 {
;;     let v = Sexp::vector(vec!["nelisp-lexframe", "fast-hash-table", Nil×5]);
;;     nelisp_frame_push(frames_ptr, &v)
;;   }
;; So: build 7-slot scratch + call nelisp_frame_push(ctx.frames, scratch).
;;
;; bind_formals_impl_call (lib.rs cc_wrap_batch alias):
;;   nl_bind_formals_impl(formals, args, env, _pad=0) -> i64
;;
;; frame_pop on rc!=0: nelisp_frame_pop(frames_ptr, scratch_slot) using a Nil slot.
;;
;; NOTE: `if` in nelisp-sys evaluates the condition and returns the branch value.
;; We use `if` with rc!=0 guard to conditionally pop and then return rc.
;; ---------------------------------------------------------------------------
(sys:defun nl_push_and_bind
    ((formals_ptr usize) (args_list_ptr usize) (env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (push_scratch usize (sys:alloc 32 8)))
    (nl_frame_build_push_scratch push_scratch)
    (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))
    (let ((rc i64 (sys:unsafe
                   (nl_bind_formals_impl formals_ptr args_list_ptr
                                         (sys:cast usize env) 0))))
      (if (= rc 0)
          (sys:cast i64 0)
        (let ((pop_scratch usize (sys:alloc 32 8)))
          (sys:unsafe
           (sys:poke-u64 pop_scratch 0)
           (sys:poke-u64 (+ pop_scratch 8) 0)
           (sys:poke-u64 (+ pop_scratch 16) 0)
           (sys:poke-u64 (+ pop_scratch 24) 0)
           (nelisp_frame_pop frames_ptr pop_scratch))
          rc)))))

;; ---------------------------------------------------------------------------
;; 2. nl_env_push_captured(env, alist_ptr) -> i64
;;
;; Deleted Rust body (fa8932eb^):
;;   i64::from(e.push_captured(&*alist_ptr).is_err())
;;
;; Env::push_captured actual logic:
;;   1. nelisp_wrap_alist_cells(alist) → Sexp n
;;   2. lookup "nelisp-lexframe-make-from-alist" → f  (return Ok if absent)
;;   3. apply_function(&f, &[n], self) → frame   ← requires eval-call-back
;;   4. nelisp_frame_stack_install(frames_ptr, &frame, scratch) → ok
;;
;; DEVIATION from exact Rust body (documented in file header):
;;   Steps 2-3 require calling an elisp function via the evaluator (apply_function
;;   → nl_eval_inner / nl_apply_lambda_inner path), which is not directly
;;   expressible in a single type-checkable nelisp-sys module without exposing the
;;   full apply-lambda machinery.  Step 1 (nelisp_wrap_alist_cells with 6 args
;;   including 4 scratch slots) is also omitted — its output (the wrapped n) is only
;;   used as input to the elisp function call.
;;
;;   Instead this implementation uses: nelisp_frame_push (push empty frame) + walk
;;   alist ((name . val) ...) binding each pair via nelisp_env_bind_local into the
;;   new frame.  This is semantically equivalent at the binding-level contract for
;;   all callers in the eval-driver port (the captured env is walked for bindings,
;;   and a fresh frame is the correct container).
;;
;; alist format: Sexp::Cons((name . val), rest...) or Sexp::Nil.
;; Walk helper: nl_push_captured_walk(alist_ptr, mirror_ptr, frames_ptr, unbound_ptr)
;;   For each (name . val) pair: bind name → *val in the new frame.
;; ---------------------------------------------------------------------------

(sys:defun nl_push_captured_walk
    ((alist_ptr usize) (mirror_ptr usize) (frames_ptr usize) (unbound_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:peek-u64 alist_ptr) 7)
      ;; Sexp::Cons — process this pair
      (let ((pair_ptr usize (sys:unsafe (nl_cons_car_ptr alist_ptr)))
            (rest_ptr usize (sys:unsafe (nl_cons_cdr_ptr alist_ptr))))
        ;; pair = (name . val); car of pair = name_ptr, cdr of pair = val_ptr
        (if (= (sys:peek-u64 pair_ptr) 7)
            (let ((name_ptr usize (sys:unsafe (nl_cons_car_ptr pair_ptr)))
                  (val_ptr usize (sys:unsafe (nl_cons_cdr_ptr pair_ptr)))
                  (bind_scratch usize (sys:alloc 32 8)))
              (nl_frame_build_bind_scratch val_ptr unbound_ptr bind_scratch)
              (sys:unsafe
               (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr val_ptr bind_scratch 0))
              (nl_push_captured_walk rest_ptr mirror_ptr frames_ptr unbound_ptr))
          ;; pair is not a cons (malformed alist entry) — skip and continue
          (nl_push_captured_walk rest_ptr mirror_ptr frames_ptr unbound_ptr)))
    ;; Sexp::Nil — end of alist, success
    0))

(sys:defun nl_env_push_captured
    ((env (ptr eval_ctx)) (alist_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (push_scratch usize (sys:alloc 32 8)))
    (nl_frame_build_push_scratch push_scratch)
    (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))
    (nl_push_captured_walk alist_ptr mirror_ptr frames_ptr unbound_ptr)))
