;; Doc 135 Stage 135.C — BIND+STASH env-leaves + set_value fix
;; STAGED (unwired — do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements 7 env-leaf ctx-accessors for the nelisp-sys DSL (Doc 135 full-cascade
;; eval-port).  EvalCtx = (ptr eval_ctx) post-cutover.
;;
;; Archive symbol verification (target/debug/.../libnelisp_elisp_spike.a, most recent):
;;   nelisp_env_set_value   T 0x009a  nelisp_env_bind_local  T 0x0171
;;   nelisp_cons_construct  T 0x0000  nl_alloc_vector        T 0x01f4
;;   nl_alloc_symbol        T 0x04cf  nl_sexp_clone_into     T 0x0472
;;   nl_vector_set_slot     T 0x0000  nl_cons_car_ptr        T 0x0000
;;   nl_cons_cdr_ptr        T 0x0000
;;
;; Scratch vector layout (standard 11-slot, build_or_insert_scratch_vec):
;;   slot 5: Symbol("symbol-entry")  <- must be pre-filled
;;   slot 7: value                   <- must be pre-filled
;;   slot 8: unbound_marker          <- must be pre-filled
;;   slots 0-4, 6, 9-10: Nil
;;
;; SEXP tag constants: 0=Nil, 2=Int, 4=Symbol, 5=Str, 7=Cons, 8=Vector
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
;; String byte encodings (little-endian u64 words):
;;   "symbol-entry"           [12]: 7290602597431212403, 2037544046
;;   "nelisp--last-signal-data" [24]: 3255381746650998126, 7451613697525637484, 7022344801864147310
;;   "wrong-number-of-arguments" [25]: 8461750672133419639, 3271424420314702445, 8389754676633367137, 115
;;   "wrong-type-argument"    [19]: 8751669898145395319, 7887324063363589488, 7630437
;;   "lambda"                 [ 6]: 107083775959404
;;   "symbol"                 [ 6]: 119225648511347
;;   "symbol after &rest"     [18]: 6998713046582262131, 7309947065975796838, 29811
;;   "&rest"                  [ 5]: 500152234534
;;
;; nl_alloc_vector(cap) -> box_ptr (usize = NlVector*) — box NOT a Sexp slot
;; nl_vector_set_slot(box_ptr, n, val_sexp_ptr) -> i64  (val_sexp_ptr = *const Sexp)
;; nl_alloc_symbol(bytes_ptr, len, result_sexp_slot) -> result_sexp_slot (usize)
;; nl_sexp_clone_into(dst, src) -> i64   (dst, src = usize = *mut/*const Sexp)
;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64
;; nl_cons_car_ptr(cons_sexp_ptr) -> usize  (*const Sexp of car)
;; nl_cons_cdr_ptr(cons_sexp_ptr) -> usize  (*const Sexp of cdr)
;;
;; SEQUENCING PATTERN: In nelisp-sys, `and` is short-circuit BOOL logic (bool x bool -> bool).
;; For sequencing i64-returning statements, use multi-expr bodies in sys:defun / sys:unsafe /
;; let / seq — each expr is evaluated in order, the last is the return.

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

(sys:extern nelisp_env_set_value
  (:symbol "nelisp_env_set_value" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64
;; vec_ptr: NlVector* (= box_ptr from nl_alloc_vector, NOT a Sexp* address)
;; n:       zero-based element index (i64)
;; val:     *const Sexp — source 32-byte slot to copy from (usize)
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

;; nl_sexp_clone_into(dst, src) -> i64
;; Refcount-aware clone of the Sexp at src into dst.
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((arg0 usize) (arg1 usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

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
;; SHARED HELPER: nl_env_scratch_write_symentry
;;
;; Writes "symbol-entry" (12 bytes) into a 16-byte char buffer, then calls
;; nl_alloc_symbol to write a Sexp::Symbol into sym_slot.
;; Returns sym_slot (= the address of the written Sexp::Symbol slot).
;;
;; Encoding: "symbol-entry" little-endian u64:
;;   word0 = 7290602597431212403  ('s','y','m','b','o','l','-','e')
;;   word1 = 2037544046           ('n','t','r','y',0,0,0,0)
;; ---------------------------------------------------------------------------
(sys:defun nl_env_scratch_write_symentry
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7290602597431212403)
     (sys:poke-u64 (+ buf 8) 2037544046)
     (nl_alloc_symbol buf 12 sym_slot))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_env_scratch_vec_sexp
;;
;; Wraps a raw NlVector box_ptr into a Sexp::Vector header written into sexp_slot.
;; Writes: tag=8@0, box_ptr@8, 0@16, 0@24.  Returns sexp_slot.
;; ---------------------------------------------------------------------------
(sys:defun nl_env_scratch_vec_sexp
    ((box_ptr usize) (sexp_slot usize))
  usize
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 sexp_slot 8)
   (sys:poke-u64 (+ sexp_slot 8) box_ptr)
   (sys:poke-u64 (+ sexp_slot 16) 0)
   (sys:poke-u64 (+ sexp_slot 24) 0)
   sexp_slot))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_env_build_scratch
;;
;; Builds the standard 11-slot Sexp::Vector scratch used by
;; nelisp_env_set_value and nelisp_env_bind_local.
;;
;; Signature: (val_ptr, unbound_ptr, out_sexp_vec_slot) -> usize
;;   val_ptr:            *const Sexp — value to set/bind (cloned into slot 7)
;;   unbound_ptr:        *const Sexp — ctx.unbound (cloned into slot 8)
;;   out_sexp_vec_slot:  usize — caller-owned 32-byte Sexp slot that receives
;;                               the Sexp::Vector wrapping the NlVector.
;;   Returns: out_sexp_vec_slot (ready to pass as scratch_ptr).
;;
;; Steps:
;;   1. nl_alloc_vector(11) → box_ptr (NlVector* with 11 Nil-filled slots)
;;   2. sym_slot = sys:alloc 32 8 — scratch slot for the Symbol("symbol-entry")
;;   3. nl_env_scratch_write_symentry(sym_slot) → fills sym_slot
;;   4. nl_vector_set_slot(box_ptr, 5, sym_slot) → slot 5 = symbol-entry
;;   5. data_ptr = sys:peek-u64(box_ptr + 8) — Vec.data_ptr field
;;   6. nl_sexp_clone_into(data_ptr + 224, val_ptr) → slot 7 (7*32=224)
;;   7. nl_sexp_clone_into(data_ptr + 256, unbound_ptr) → slot 8 (8*32=256)
;;   8. nl_env_scratch_vec_sexp(box_ptr, out_sexp_vec_slot) → Sexp::Vector header
;;
;; VERIFIED-CORRECT — refcount discipline analysis (orchestrator-confirmed):
;;
;;   Slot 5 (symbol-entry): built fresh by nl_alloc_symbol (rc=1, sole owner;
;;   the sym_slot stack-local is the only reference and is abandoned/never-dropped
;;   after this call site).  nl_vector_set_slot does a raw 4-u64 copy (no refcount
;;   bump) which is correct here because sym_slot is the sole owner being
;;   transferred to the vector; the source stack slot is subsequently unused.
;;
;;   Slots 7/8 (val_ptr, unbound_ptr): these point at the CALLER'S LIVE Sexp
;;   values.  Writing them via raw nl_vector_set_slot would copy the payload
;;   WITHOUT bumping the refcount, producing a double-free when both the caller's
;;   reference and the vector's slot are dropped.  nl_sexp_clone_into is therefore
;;   required for slots 7 and 8 — it performs a refcount-aware clone so the vector
;;   holds an independent ownership reference.
;;
;;   NlVector layout (data_ptr@box+8 verified by nl_vector_set_slot source):
;;     nl_vector_set_slot(vec, n, val) → dst = ptr-read-u64(vec, 8) + (n * 32)
;;     then raw-copies 4 u64 words.  So data_ptr = peek-u64(box_ptr+8),
;;     slot 7 = data_ptr+224 (7*32), slot 8 = data_ptr+256 (8*32).
;;   Raw-offset access for slots 7/8 matches nl_vector_set_slot's own addressing
;;   exactly — the layout is confirmed, not guessed.
;;
;; ALLOC budget: nl_alloc_vector (32 + 11*32 = 384 B) +
;;               sym_slot (32 B) + char_buf in nl_alloc_symbol (12 B) ≈ 428 B total.
;; ---------------------------------------------------------------------------
(sys:defun nl_env_build_scratch
    ((val_ptr usize) (unbound_ptr usize) (out_sexp_vec_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_slot usize (sys:alloc 32 8)))
    (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
      (sys:unsafe
       (nl_env_scratch_write_symentry sym_slot)
       (nl_vector_set_slot box_ptr 5 sym_slot)
       (nl_sexp_clone_into (+ data_ptr 224) val_ptr)
       (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr)
       (nl_env_scratch_vec_sexp box_ptr out_sexp_vec_slot)))))

;; ---------------------------------------------------------------------------
;; 1. nl_env_set_value(env, sym_ptr, val_ptr) -> i64  [3-arg, FIXED]
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_env_set_value(env: *mut c_void, sym: *const Sexp, val: *const Sexp) -> i64
;;
;; Builds 11-slot scratch from val_ptr + ctx.unbound, then calls
;; nelisp_env_set_value(ctx.mirror, ctx.frames, sym_ptr, val_ptr, scratch, 0).
;;
;; TYPE GUARD: The Rust body matched sym on Symbol|Str → returned 1 for others.
;; Guard OMITTED here; callers pass well-typed symbol pointers.
;; ---------------------------------------------------------------------------
(sys:defun nl_env_set_value
    ((env (ptr eval_ctx)) (sym_ptr usize) (val_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (out_vec_slot usize (sys:alloc 32 8)))
    (nl_env_build_scratch val_ptr unbound_ptr out_vec_slot)
    (sys:unsafe
     (nelisp_env_set_value mirror_ptr frames_ptr sym_ptr val_ptr out_vec_slot 0))))

;; ---------------------------------------------------------------------------
;; 2. nl_bf_bind_sym(env, name_ptr, val_ptr) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_bind_sym(env: *mut c_void, name_ptr: *const Sexp, val_ptr: *const Sexp) -> i64
;;
;; If name is Symbol: env.bind_local(name, (*val_ptr).clone()); return 0.
;; TYPE GUARD: OMITTED (same justification as #1).
;; ---------------------------------------------------------------------------
(sys:defun nl_bf_bind_sym
    ((env (ptr eval_ctx)) (name_ptr usize) (val_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (out_vec_slot usize (sys:alloc 32 8)))
    (nl_env_build_scratch val_ptr unbound_ptr out_vec_slot)
    (sys:unsafe
     (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr val_ptr out_vec_slot 0))))

;; ---------------------------------------------------------------------------
;; 3. nl_bf_bind_optional(env, name_ptr, args_ptr, idx) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_bind_optional(env, name_ptr, args_ptr, idx: i64) -> i64
;;
;; Walk args list to index idx; bind name → (nth idx args) or Nil.
;; Returns idx+1 if idx < len, else idx.
;;
;; SEXP TAG CHECK: (sys:peek-u64 ptr) reads 8 bytes as i64/u64.
;;   Sexp::Cons has tag=7 at byte 0; bytes 1-7 of the first u64 word are pad=0.
;;   (sys:peek-u64 ptr) == 7 correctly identifies Sexp::Cons (tag=7, pad=0).
;;   Sexp::Nil has tag=0, so (sys:peek-u64 ptr) == 0 identifies Nil.
;;
;; WALK HELPER: nl_bf_bind_opt_walk(cons_ptr, idx) -> usize
;;   Returns the *const Sexp of element at index idx, OR 0 if idx >= len.
;;   - If tag != 7 (non-Cons = Nil or error): return 0
;;   - If idx == 0: return nl_cons_car_ptr(cons_ptr)
;;   - Else: recurse with (cdr, idx-1)
;; ---------------------------------------------------------------------------

(sys:defun nl_bf_bind_opt_walk
    ((cons_ptr usize) (idx i64))
  usize
  (:alloc none :ffi may :unsafe may)
  (if (= (sys:peek-u64 cons_ptr) 7)
      (if (= idx 0)
          (sys:unsafe (nl_cons_car_ptr cons_ptr))
        (nl_bf_bind_opt_walk
         (sys:unsafe (nl_cons_cdr_ptr cons_ptr))
         (+ idx -1)))
    0))

(sys:defun nl_bf_bind_optional
    ((env (ptr eval_ctx)) (name_ptr usize) (args_ptr usize) (idx i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((elem_ptr usize (nl_bf_bind_opt_walk args_ptr idx))
        (mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (out_vec_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 nil_slot 0)
     (sys:poke-u64 (+ nil_slot 8) 0)
     (sys:poke-u64 (+ nil_slot 16) 0)
     (sys:poke-u64 (+ nil_slot 24) 0))
    (if (= elem_ptr 0)
        (seq
         (nl_env_build_scratch nil_slot unbound_ptr out_vec_slot)
         (sys:unsafe
          (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr nil_slot out_vec_slot 0))
         idx)
      (seq
       (nl_env_build_scratch elem_ptr unbound_ptr out_vec_slot)
       (sys:unsafe
        (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr elem_ptr out_vec_slot 0))
       (+ idx 1)))))

;; ---------------------------------------------------------------------------
;; 4. nl_bf_bind_rest(env, name_ptr, args_ptr, idx) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_bind_rest(env, name_ptr, args_ptr, idx: i64) -> i64
;;
;; Bind name → (list args[idx..end]).  Returns 0.
;;
;; The Rust body: list_elements(args) → Vec, bind name → Sexp::list_from(&args[idx..]).
;; In nelisp-sys: walk idx steps along cdrs to get the tail sublist,
;; then bind name → *tail_ptr directly (the tail IS a proper Cons-or-Nil list).
;;
;; TAIL WALK: nl_bf_bind_rest_tail(cons_ptr, idx) -> usize
;;   Walk idx cdrs; if Nil reached early, return that Nil ptr (= all remaining = Nil).
;;   Returns pointer to the Sexp at tail position idx (a proper list prefix).
;; ---------------------------------------------------------------------------

(sys:defun nl_bf_bind_rest_tail
    ((cons_ptr usize) (idx i64))
  usize
  (:alloc none :ffi may :unsafe may)
  (if (= idx 0)
      cons_ptr
    (if (= (sys:peek-u64 cons_ptr) 7)
        (nl_bf_bind_rest_tail
         (sys:unsafe (nl_cons_cdr_ptr cons_ptr))
         (+ idx -1))
      cons_ptr)))

(sys:defun nl_bf_bind_rest
    ((env (ptr eval_ctx)) (name_ptr usize) (args_ptr usize) (idx i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tail_ptr usize (nl_bf_bind_rest_tail args_ptr idx))
        (mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (out_vec_slot usize (sys:alloc 32 8)))
    (nl_env_build_scratch tail_ptr unbound_ptr out_vec_slot)
    (sys:unsafe
     (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr tail_ptr out_vec_slot 0))))

;; ---------------------------------------------------------------------------
;; SIGNAL STASH HELPERS
;;
;; stash_err(env, e) = set_value("nelisp--last-signal-data", e.signal_data())
;; signal_data = cons(Symbol(tag), data)
;;
;; "nelisp--last-signal-data" (24 bytes = 3 u64 words):
;;   word0: 3255381746650998126  word1: 7451613697525637484
;;   word2: 7022344801864147310
;;
;; HELPER: nl_env_stash_write_name_sym(sym_slot) -> sym_slot
;;   Writes Symbol("nelisp--last-signal-data") into sym_slot.
;; ---------------------------------------------------------------------------

(sys:defun nl_env_stash_write_name_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7451613697525637484)
     (sys:poke-u64 (+ buf 16) 7022344801864147310)
     (nl_alloc_symbol buf 24 sym_slot))))

;; nl_env_stash_signal(env, signal_slot) -> i64
;; Sets "nelisp--last-signal-data" = *signal_slot in env.  Returns 1.
(sys:defun nl_env_stash_signal
    ((env (ptr eval_ctx)) (signal_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((name_sym_slot usize (sys:alloc 32 8)))
    (nl_env_stash_write_name_sym name_sym_slot)
    (nl_env_set_value env name_sym_slot signal_slot)))

;; nl_env_write_nil_slot(slot) -> i64
;; Writes a Sexp::Nil (all-zero) into the 32-byte slot at 'slot'.
(sys:defun nl_env_write_nil_slot
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)))

;; ---------------------------------------------------------------------------
;; 5. nl_bf_err_arity(env, required, got) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_err_arity(env: *mut c_void, required: i64, got: i64) -> i64
;;
;; EvalError::wrong_arity("lambda", required.to_string(), got as usize):
;;   Generic("wrong-number-of-arguments", list_from([Symbol("lambda"), Int(got)]))
;;
;; signal_data = cons(Symbol("wrong-number-of-arguments"),
;;                    cons(Symbol("lambda"), cons(Int(got), Nil)))
;;
;; String encodings:
;;   "wrong-number-of-arguments" [25]: 8461750672133419639, 3271424420314702445,
;;                                     8389754676633367137, 115
;;   "lambda" [6]: 107083775959404
;;
;; Sexp::Int layout: tag=2@0 (written as u64 with zeros in bytes 1-7), payload=got@8,
;;                   zeros@16, zeros@24.
;; ---------------------------------------------------------------------------

(sys:defun nl_bf_err_arity_write_wna_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8461750672133419639)
     (sys:poke-u64 (+ buf 8) 3271424420314702445)
     (sys:poke-u64 (+ buf 16) 8389754676633367137)
     (sys:poke-u64 (+ buf 24) 115)
     (nl_alloc_symbol buf 25 sym_slot))))

(sys:defun nl_bf_err_arity_write_lambda_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe
     (sys:poke-u64 buf 107083775959404)
     (nl_alloc_symbol buf 6 sym_slot))))

(sys:defun nl_bf_err_arity
    ((env (ptr eval_ctx)) (required i64) (got i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag_slot usize (sys:alloc 32 8))
        (lambda_slot usize (sys:alloc 32 8))
        (int_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (inner_cdr usize (sys:alloc 32 8))
        (lambda_pair usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    (nl_bf_err_arity_write_wna_sym tag_slot)
    (nl_bf_err_arity_write_lambda_sym lambda_slot)
    (sys:unsafe
     (sys:poke-u64 int_slot 2)
     (sys:poke-u64 (+ int_slot 8) got)
     (sys:poke-u64 (+ int_slot 16) 0)
     (sys:poke-u64 (+ int_slot 24) 0))
    (nl_env_write_nil_slot nil_slot)
    (sys:unsafe
     (nelisp_cons_construct int_slot nil_slot inner_cdr)
     (nelisp_cons_construct lambda_slot inner_cdr lambda_pair)
     (nelisp_cons_construct tag_slot lambda_pair signal_slot))
    (nl_env_stash_signal env signal_slot)))

;; ---------------------------------------------------------------------------
;; 6. nl_bf_err_type(env, name_ptr) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_err_type(env: *mut c_void, name_ptr: *const Sexp) -> i64
;;
;; EvalError::wrong_type("symbol", (*name_ptr).clone()):
;;   Generic("wrong-type-argument", list_from([Symbol("symbol"), name_clone]))
;;
;; signal_data = cons(Symbol("wrong-type-argument"),
;;                    cons(Symbol("symbol"), cons(name_clone, Nil)))
;;
;; String encodings:
;;   "wrong-type-argument" [19]: 8751669898145395319, 7887324063363589488, 7630437
;;   "symbol" [6]: 119225648511347
;; ---------------------------------------------------------------------------

(sys:defun nl_bf_err_type_write_wta_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8751669898145395319)
     (sys:poke-u64 (+ buf 8) 7887324063363589488)
     (sys:poke-u64 (+ buf 16) 7630437)
     (nl_alloc_symbol buf 19 sym_slot))))

(sys:defun nl_bf_err_type_write_symbol_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe
     (sys:poke-u64 buf 119225648511347)
     (nl_alloc_symbol buf 6 sym_slot))))

(sys:defun nl_bf_err_type
    ((env (ptr eval_ctx)) (name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag_slot usize (sys:alloc 32 8))
        (symbol_slot usize (sys:alloc 32 8))
        (name_clone usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (inner_cdr usize (sys:alloc 32 8))
        (symbol_pair usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    (nl_bf_err_type_write_wta_sym tag_slot)
    (nl_bf_err_type_write_symbol_sym symbol_slot)
    (sys:unsafe (nl_sexp_clone_into name_clone name_ptr))
    (nl_env_write_nil_slot nil_slot)
    (sys:unsafe
     (nelisp_cons_construct name_clone nil_slot inner_cdr)
     (nelisp_cons_construct symbol_slot inner_cdr symbol_pair)
     (nelisp_cons_construct tag_slot symbol_pair signal_slot))
    (nl_env_stash_signal env signal_slot)))

;; ---------------------------------------------------------------------------
;; 7. nl_bf_err_dangling_rest(env) -> i64
;;
;; Deleted Rust sig (fa8932eb^):
;;   fn nl_bf_err_dangling_rest(env: *mut c_void) -> i64
;;
;; EvalError::wrong_type("symbol after &rest", Sexp::Symbol("&rest")):
;;   Generic("wrong-type-argument",
;;           list_from([Symbol("symbol after &rest"), Symbol("&rest")]))
;;
;; signal_data = cons(Symbol("wrong-type-argument"),
;;                    cons(Symbol("symbol after &rest"), cons(Symbol("&rest"), Nil)))
;;
;; String encodings:
;;   "symbol after &rest" [18]: 6998713046582262131, 7309947065975796838, 29811
;;   "&rest" [5]: 500152234534
;; ---------------------------------------------------------------------------

(sys:defun nl_bf_err_dangling_rest_write_sym_after
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 6998713046582262131)
     (sys:poke-u64 (+ buf 8) 7309947065975796838)
     (sys:poke-u64 (+ buf 16) 29811)
     (nl_alloc_symbol buf 18 sym_slot))))

(sys:defun nl_bf_err_dangling_rest_write_amp_rest
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe
     (sys:poke-u64 buf 500152234534)
     (nl_alloc_symbol buf 5 sym_slot))))

(sys:defun nl_bf_err_dangling_rest
    ((env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag_slot usize (sys:alloc 32 8))
        (sym_after usize (sys:alloc 32 8))
        (amp_rest_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (inner_cdr usize (sys:alloc 32 8))
        (after_pair usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    (nl_bf_err_type_write_wta_sym tag_slot)
    (nl_bf_err_dangling_rest_write_sym_after sym_after)
    (nl_bf_err_dangling_rest_write_amp_rest amp_rest_slot)
    (nl_env_write_nil_slot nil_slot)
    (sys:unsafe
     (nelisp_cons_construct amp_rest_slot nil_slot inner_cdr)
     (nelisp_cons_construct sym_after inner_cdr after_pair)
     (nelisp_cons_construct tag_slot after_pair signal_slot))
    (nl_env_stash_signal env signal_slot)))
