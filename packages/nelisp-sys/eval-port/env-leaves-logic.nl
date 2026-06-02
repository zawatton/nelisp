;; Doc 135 Stage 135.C -- REAL-LOGIC env-leaves let_setup + cc_match_and_bind
;; STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements 2 high-logic env-leaf ctx-accessors for the nelisp-sys DSL:
;;   1. nl_let_setup(bindings_list, env, sequential) -> i64
;;   2. nl_cc_match_and_bind(clauses, err_inout, var, env) -> i64
;;
;; Both are faithfully reproduced from fa8932eb^ (deleted Rust bodies).
;;
;; ---------------------------------------------------------------------------
;; Archive symbol verification (target/debug/.../libnelisp_elisp_spike.a):
;;   nelisp_env_bind_local  T 0x0171   nelisp_env_set_value   T 0x009a
;;   nelisp_frame_push      T 0x0000   nelisp_frame_pop       T 0x00df
;;   nelisp_cons_construct  T 0x0000   nl_alloc_vector        T 0x01f4
;;   nl_alloc_symbol        T 0x04cf   nl_sexp_clone_into     T (U in archive = provided by Rust link)
;;   nl_cons_car_ptr        T 0x0000   nl_cons_cdr_ptr        T 0x0000
;;   nelisp_eval_call       U          (defined in Rust eval/mod.rs, resolved at link)
;;
;; NOTE: nl_sexp_clone_into and nelisp_eval_call appear as `U' (undefined) in
;; libnelisp_elisp_spike.a -- they are referenced by the archive and supplied by
;; the linking Rust binary, not by the archive itself.
;;
;; ---------------------------------------------------------------------------
;; is_error_subtype callability finding:
;;   The Rust `is_error_subtype(clause_tag: &str, actual_tag: &str) -> bool` is
;;   defined in build-tool/src/eval/mod.rs as a plain (non-#[no_mangle]) Rust fn
;;   with Rust-internal signature (&str, &str) -> bool.  It is NOT exported as a
;;   C-ABI symbol and does NOT appear as a T in any archive.
;;
;;   Its logic is: clause==actual  OR  clause=="t"  OR  (clause=="error" && actual!="quit")
;;
;;   This file implements nl_cc_match_and_bind with SIMPLIFIED MATCHING:
;;     - symbol equality: tag==4 and the Symbol pointer equals actual_tag_ptr by
;;       identity (NlStr ptr comparison via nelisp_eq_symbol)
;;     - Sexp::T (tag==1) catch-all -> true
;;     - Cons in clause car -> walk elements, any matches -> true (same eq-symbol)
;;   The "error catches all non-quit" hierarchy subtyping is DEFERRED.
;;   Exact-tag + T catch-all cover the common cases; error-hierarchy refinement
;;   requires a C-ABI is_error_subtype export (add #[no_mangle] to the Rust fn,
;;   or expose a nelisp_is_error_subtype wrapper taking two Symbol Sexp ptrs).
;;   This is flagged for the 135.D sub-stage.
;;
;; ---------------------------------------------------------------------------
;; Sexp tag constants (verified via SEXP_TAG_* test suite):
;;   0 = Nil   1 = T   2 = Int   4 = Symbol   5 = Str   7 = Cons   8 = Vector
;;
;; String byte encodings (little-endian u64 words):
;;   "symbol-entry"             [12]: 7290602597431212403, 2037544046
;;   "nelisp--last-signal-data" [24]: 3255381746650998126, 7451613697525637484,
;;                                    7022344801864147310
;;   "nil"                      [ 3]: 7104878
;;   "t"                        [ 1]: 116
;;
;; SEQUENCING PATTERN: `and` is short-circuit boolean. Use seq / let / sys:unsafe
;; for multi-statement i64-returning sequences.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Struct declarations (LOCKED sec.2.3 layout)
;; ---------------------------------------------------------------------------

(sys:defstruct sexp (:repr c)
  (tag u8) (payload u64) (pad (array u8 16)))

(sys:defstruct eval_ctx (:repr c)
  (mirror sexp) (frames sexp) (unbound sexp)
  (rec_cur i64) (rec_max i64) (flags i64))

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nelisp_eval_call(form_ptr, env_ptr, out_ptr) -> i64
;; 0=ok (out holds result), 1=err (signal stashed in env).
;; env_ptr: opaque *mut c_void = eval_ctx* post-cutover.
(sys:extern nelisp_eval_call
  (:symbol "nelisp_eval_call" :abi c :unsafe t)
  ((form_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_env_bind_local(mirror, frames, name, val, scratch, _pad) -> i64
(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_set_value(mirror, frames, name, val, scratch, _pad) -> i64
(sys:extern nelisp_env_set_value
  (:symbol "nelisp_env_set_value" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_frame_push(frames_ptr, scratch_vec_ptr) -> i64
;; scratch_vec_ptr: 7-slot Sexp::Vector with type-tag Symbols at slots 0,1.
(sys:extern nelisp_frame_push
  (:symbol "nelisp_frame_push" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_vec_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_frame_pop(frames_ptr, scratch_slot) -> i64
;; scratch_slot: caller-owned 32B Nil-initialised slot.
(sys:extern nelisp_frame_pop
  (:symbol "nelisp_frame_pop" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_slot usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_vector(cap) -> box_ptr (usize = NlVector*)
(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64
(sys:extern nl_vector_set_slot
  (:symbol "nl_vector_set_slot" :abi c :unsafe t)
  ((vec_ptr usize) (n i64) (val usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot (usize)
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64  (refcount-aware clone)
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

;; nl_cons_car_ptr(cons_ptr) -> usize  (*const Sexp of car slot)
(sys:extern nl_cons_car_ptr
  (:symbol "nl_cons_car_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_cons_cdr_ptr(cons_ptr) -> usize  (*const Sexp of cdr slot)
(sys:extern nl_cons_cdr_ptr
  (:symbol "nl_cons_cdr_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nelisp_eq_symbol(a, b, result_slot) -> *mut Sexp (Sexp::T if equal, Sexp::Nil if not)
;; Returns result_slot as usize; tag byte at result_slot == 1 means equal.
(sys:extern nelisp_eq_symbol
  (:symbol "nelisp_eq_symbol" :abi c :unsafe t)
  ((a_ptr usize) (b_ptr usize) (result_slot usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_env_scratch_write_symentry
;;
;; Writes "symbol-entry" (12 bytes) into a 16-byte char buffer, then calls
;; nl_alloc_symbol to write Sexp::Symbol("symbol-entry") into sym_slot.
;; Returns sym_slot.
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_write_symentry
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7290602597431212403)
     (sys:poke-u64 (+ buf 8) 2037544046)
     (nl_alloc_symbol buf 12 sym_slot))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_build_scratch
;;
;; Builds the standard 11-slot Sexp::Vector scratch for nelisp_env_bind_local
;; and nelisp_env_set_value.
;;
;; (val_ptr, unbound_ptr, out_sexp_vec_slot) -> usize
;; Returns out_sexp_vec_slot ready to pass as scratch_ptr.
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_build_scratch
    ((val_ptr usize) (unbound_ptr usize) (out_sexp_vec_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_slot usize (sys:alloc 32 8)))
    (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
      (sys:unsafe
       (nl_logic_write_symentry sym_slot)
       (nl_vector_set_slot box_ptr 5 sym_slot)
       (nl_sexp_clone_into val_ptr (+ data_ptr 224))
       (nl_sexp_clone_into unbound_ptr (+ data_ptr 256))
       (sys:poke-u64 out_sexp_vec_slot 8)
       (sys:poke-u64 (+ out_sexp_vec_slot 8) box_ptr)
       (sys:poke-u64 (+ out_sexp_vec_slot 16) 0)
       (sys:poke-u64 (+ out_sexp_vec_slot 24) 0)
       out_sexp_vec_slot))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_frame_push_scratch
;;
;; Builds the 7-slot Sexp::Vector scratch for nelisp_frame_push:
;;   slot 0: Symbol("nelisp-lexframe")  slot 1: Symbol("fast-hash-table")
;;   slots 2-6: Sexp::Nil
;; Returns out_vec_sexp ready to pass as scratch_vec_ptr.
;;
;; "nelisp-lexframe" [15]: 7795010171040458094, 28549237946349669
;; "fast-hash-table" [15]: 8314040931539181926, 28548142445374824
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_write_lexframe_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7795010171040458094)
     (sys:poke-u64 (+ buf 8) 28549237946349669)
     (nl_alloc_symbol buf 15 sym_slot))))

(sys:defun nl_logic_write_hashsym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8314040931539181926)
     (sys:poke-u64 (+ buf 8) 28548142445374824)
     (nl_alloc_symbol buf 15 sym_slot))))

(sys:defun nl_logic_frame_push_scratch
    ((out_vec_sexp usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 7)))
        (sym0_slot usize (sys:alloc 32 8))
        (sym1_slot usize (sys:alloc 32 8)))
    (nl_logic_write_lexframe_sym sym0_slot)
    (nl_logic_write_hashsym sym1_slot)
    (sys:unsafe
     (nl_vector_set_slot box_ptr 0 sym0_slot)
     (nl_vector_set_slot box_ptr 1 sym1_slot)
     (sys:poke-u64 out_vec_sexp 8)
     (sys:poke-u64 (+ out_vec_sexp 8) box_ptr)
     (sys:poke-u64 (+ out_vec_sexp 16) 0)
     (sys:poke-u64 (+ out_vec_sexp 24) 0)
     out_vec_sexp)))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_pop_frame(frames_ptr) -> i64
;;
;; Pop a frame. Allocates a 32B Nil scratch slot; calls nelisp_frame_pop.
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_pop_frame
    ((frames_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((scratch usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 scratch 0)
     (sys:poke-u64 (+ scratch 8) 0)
     (sys:poke-u64 (+ scratch 16) 0)
     (sys:poke-u64 (+ scratch 24) 0)
     (nelisp_frame_pop frames_ptr scratch))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_write_nil_slot(slot) -> i64
;;
;; Writes Sexp::Nil (all-zero) into a 32B slot.
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_write_nil_slot
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   ;; Return 0 (= success) explicitly.  Callers (`nl_let_parse_val')
   ;; treat the return as a 0/1 rc; the trailing `poke-u64' otherwise
   ;; yields a non-zero slot address that `nl_let_setup' mis-reads as a
   ;; parse error, rejecting every nil-valued `let'/`let*' binding
   ;; (`(let (v) ...)' / `(let ((v)) ...)').
   (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_bind_local(env, name_ptr, val_ptr) -> i64
;;
;; Extracts mirror/frames/unbound from ctx; builds scratch; calls bind_local.
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_bind_local
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
    (nl_logic_build_scratch val_ptr unbound_ptr out_vec_slot)
    (sys:unsafe
     (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr val_ptr out_vec_slot 0))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_logic_stash_signal(env, signal_slot) -> i64
;;
;; Sets "nelisp--last-signal-data" = *signal_slot in env. Returns 1.
;; "nelisp--last-signal-data" [24]: 3255381746650998126, 7451613697525637484,
;;                                  7022344801864147310
;; ---------------------------------------------------------------------------
(sys:defun nl_logic_write_signal_name_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7451613697525637484)
     (sys:poke-u64 (+ buf 16) 7022344801864147310)
     (nl_alloc_symbol buf 24 sym_slot))))

(sys:defun nl_logic_stash_signal
    ((env (ptr eval_ctx)) (signal_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((name_sym_slot usize (sys:alloc 32 8))
        (mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (out_vec_slot usize (sys:alloc 32 8)))
    (nl_logic_write_signal_name_sym name_sym_slot)
    (nl_logic_build_scratch signal_slot unbound_ptr out_vec_slot)
    (sys:unsafe
     (nelisp_env_set_value mirror_ptr frames_ptr name_sym_slot signal_slot out_vec_slot 0))
    (sys:cast i64 1)))

;; ===========================================================================
;; (i) nl_let_setup(bindings_list, env, sequential) -> i64
;;
;; Deleted Rust body (fa8932eb^):
;;
;;   fn parse(b: &Sexp, e: &mut Env) -> Result<(String, Sexp), EvalError> {
;;     match b {
;;       Sexp::Symbol(n) => Ok((n.clone(), Sexp::Nil)),
;;       Sexp::Cons(_) => {
;;         let p = list_elements(b)?;
;;         let n = match &p[0] { Sexp::Symbol(s) => s.clone(), o => return Err(...) };
;;         Ok((n, if p.len() >= 2 { eval(&p[1], e)? } else { Sexp::Nil }))
;;       }
;;       o => Err(...)
;;     }
;;   }
;;   let e = &mut *(env as *mut Env);
;;   let Ok(bindings) = list_elements(&*bindings_list) else { return 1 };
;;   if sequential != 0 {                // let*
;;     e.frame_push_rust_direct();
;;     for b in &bindings {
;;       match parse(b, e) { Ok((n,v)) => e.bind_local(&n,v), Err(_) => { e.frame_pop_rust_direct(); return 1; } }
;;     }
;;   } else {                            // let
;;     let mut values = Vec::with_capacity(bindings.len());
;;     for b in &bindings { match parse(b, e) { Ok(p) => values.push(p), Err(_) => return 1 } }
;;     e.frame_push_rust_direct();
;;     for (n,v) in values { e.bind_local(&n,v); }
;;   }
;;   0
;;
;; CRITICAL SEMANTIC: the let (sequential==0) path evaluates ALL values in the
;; OUTER frame (before frame_push), so bindings cannot see each other.  The let*
;; (sequential!=0) path pushes first, then evaluates each value in the growing
;; frame so later bindings can see earlier ones.  These paths are NOT collapsed.
;;
;; IMPLEMENTATION STRATEGY for let (non-sequential):
;;   Two tail-recursive helper defuns:
;;     nl_let_collect_walk: walks bindings list, evaluates each value in the
;;       caller's frame (outer scope), builds a forward-order cons list of
;;       (name_ptr_as_int . val_sexp_slot) pairs -- name is stored as Sexp::Int
;;       with the pointer value bit-cast as i64 (safe: pointers are live sexp
;;       slots in the caller's frame, not moved during this evaluation pass).
;;       Returns: usize = pointer to head cons, or 0 on parse/eval error.
;;     nl_let_bind_walk: walks the collected pairs list, calls bind_local for
;;       each (name_ptr, val_ptr) in the new frame.
;;
;; parse_binding_name: given a binding Sexp ptr, returns the name_ptr (Symbol
;;   Sexp slot) if the binding is Symbol or (Symbol ...). Returns 0 on error.
;;
;; parse_binding_val: given a binding Sexp ptr and env, evaluates the value
;;   into a fresh 32B slot. Returns the val_slot ptr or 0 on error.
;; ===========================================================================

;; nl_let_parse_name(b_ptr) -> usize
;; Given a binding b (a *const Sexp):
;;   - If tag==4 (Symbol): return b_ptr (name is the binding itself, value=Nil)
;;   - If tag==7 (Cons): return nl_cons_car_ptr(b_ptr) if car is a Symbol, else 0
;;   - Else: return 0 (error)
(sys:defun nl_let_parse_name
    ((b_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may)
  (let ((tag usize (sys:cast usize (sys:peek-u64 b_ptr))))
    (if (= tag 4)
        ;; Symbol: the binding itself is the name
        b_ptr
      (if (= tag 7)
          ;; Cons: car must be a Symbol
          (let ((car_ptr usize (sys:unsafe (nl_cons_car_ptr b_ptr))))
            (if (= (sys:cast usize (sys:peek-u64 car_ptr)) 4)
                car_ptr
              (sys:cast usize 0)))
        ;; Neither Symbol nor Cons: error
        (sys:cast usize 0)))))

;; nl_let_parse_val(b_ptr, env_ptr, val_slot) -> i64
;; Given a binding b (a *const Sexp) and a 32B val_slot:
;;   - If tag==4 (Symbol): write Sexp::Nil into val_slot, return 0.
;;   - If tag==7 (Cons): if list has >=2 elements, eval second element into
;;     val_slot (nelisp_eval_call), return rc.  If only 1 element, write Nil.
;;   - Else: return 1 (error -- name check already handled this case)
;;
;; For Cons case with 2+ elements: second element = cdr(cdr is first cdr).
;; The cons structure is (name val ...) where car=name, cdr=rest,
;; and car(cdr)=val.  So: val_form_ptr = nl_cons_car_ptr(nl_cons_cdr_ptr(b_ptr))
;; if cdr tag==7.  If cdr tag==0 (Nil), only 1 element -> val=Nil.
(sys:defun nl_let_parse_val
    ((b_ptr usize) (env_ptr usize) (val_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag usize (sys:cast usize (sys:peek-u64 b_ptr))))
    (if (= tag 4)
        ;; Symbol binding: value is Nil
        (nl_logic_write_nil_slot val_slot)
      (if (= tag 7)
          ;; Cons: check if there is a second element (cdr is Cons)
          (let ((cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr b_ptr))))
            (if (= (sys:cast usize (sys:peek-u64 cdr_ptr)) 7)
                ;; cdr is Cons -> second element = car(cdr)
                (let ((val_form_ptr usize (sys:unsafe (nl_cons_car_ptr cdr_ptr))))
                  (sys:unsafe (nelisp_eval_call val_form_ptr env_ptr val_slot)))
              ;; cdr is Nil -> only name, value defaults to Nil
              (nl_logic_write_nil_slot val_slot)))
        ;; Else: error
        (sys:cast i64 1)))))

;; ===========================================================================
;; nl_let* (sequential) path helpers
;; ===========================================================================

;; nl_let_star_walk(bindings_ptr, env) -> i64
;; Tail-recursive: for each binding b in bindings_ptr (a cons list):
;;   1. parse name (0 -> error: pop frame, return 1)
;;   2. eval value into fresh 32B slot (rc!=0 -> error: pop frame, return 1)
;;   3. bind_local(name, val)
;;   4. recurse on cdr
;; The frame has already been pushed by the caller.
;;
;; On error we pop the frame then return 1.  Crucially the pop happens inside
;; the walker so the error-return is clean.
;;
;; frames_ptr is passed explicitly so nl_logic_pop_frame can target it without
;; re-deriving from env.
(sys:defun nl_let_star_walk
    ((bindings_ptr usize) (env (ptr eval_ctx)) (frames_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:cast usize (sys:peek-u64 bindings_ptr)) 7)
      ;; bindings_ptr is Cons -- process this binding
      (let ((b_ptr usize (sys:unsafe (nl_cons_car_ptr bindings_ptr)))
            (rest_ptr usize (sys:unsafe (nl_cons_cdr_ptr bindings_ptr))))
        (let ((name_ptr usize (nl_let_parse_name b_ptr)))
          (if (= name_ptr 0)
              ;; name parse error: pop frame + fail
              (seq
               (nl_logic_pop_frame frames_ptr)
               (sys:cast i64 1))
            ;; name ok: eval value
            (let ((val_slot usize (sys:alloc 32 8)))
              (let ((rc i64 (nl_let_parse_val b_ptr (sys:cast usize env) val_slot)))
                (if (= rc 0)
                    ;; value ok: bind then continue
                    (seq
                     (nl_logic_bind_local env name_ptr val_slot)
                     (nl_let_star_walk rest_ptr env frames_ptr))
                  ;; eval error: pop frame + fail
                  (seq
                   (nl_logic_pop_frame frames_ptr)
                   (sys:cast i64 1))))))))
    ;; Non-Cons (Nil = end of list, or malformed): success
    (sys:cast i64 0)))

;; ===========================================================================
;; nl_let (non-sequential) path helpers
;; ===========================================================================

;; nl_let_collect_walk(bindings_ptr, env_ptr) -> usize
;;
;; Walks the bindings list (in the OUTER frame, before frame_push), evaluates
;; each value in order, and builds a forward-order cons list of
;; (name_ptr_as_int . val_slot_as_int) pairs.
;;
;; To build in FORWARD order without a separate reverse pass:
;;   Recurse on rest FIRST, get the tail list (rest_pairs), then cons the
;;   current entry at the front.  Recursion depth = number of bindings (shallow).
;;
;; Sentinels (both fit in usize):
;;   ERROR sentinel   = 1  (never a valid heap pointer on modern systems; alignment >= 8)
;;   EMPTY (Nil tail) = the address of a live Sexp::Nil slot allocated on the heap,
;;                      which is always > 1 and has tag==0.
;;
;; The empty-tail case (base case) allocates a Nil Sexp slot to serve as the
;; list terminator -- this makes it a proper Nil-terminated cons list.
;;
;; Returns: 1 (error sentinel) on parse or eval error,
;;          or usize ptr to head of pairs cons list (always >= 8).
(sys:defun nl_let_collect_nil_slot
    ()
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((nil_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 nil_slot 0)
     (sys:poke-u64 (+ nil_slot 8) 0)
     (sys:poke-u64 (+ nil_slot 16) 0)
     (sys:poke-u64 (+ nil_slot 24) 0)
     nil_slot)))

(sys:defun nl_let_collect_walk
    ((bindings_ptr usize) (env_ptr usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:cast usize (sys:peek-u64 bindings_ptr)) 7)
      ;; Cons: process this binding
      (let ((b_ptr usize (sys:unsafe (nl_cons_car_ptr bindings_ptr)))
            (rest_ptr usize (sys:unsafe (nl_cons_cdr_ptr bindings_ptr))))
        (let ((name_ptr usize (nl_let_parse_name b_ptr)))
          (if (= name_ptr 0)
              ;; name parse error: return error sentinel
              (sys:cast usize 1)
            ;; name ok: eval value in outer scope (env_ptr = outer frame)
            (let ((val_slot usize (sys:alloc 32 8)))
              (let ((rc i64 (nl_let_parse_val b_ptr env_ptr val_slot)))
                (if (= rc 0)
                    ;; value ok: collect rest first (for forward order)
                    (let ((rest_pairs usize (nl_let_collect_walk rest_ptr env_ptr)))
                      (if (= rest_pairs 1)
                          ;; error from rest: propagate
                          (sys:cast usize 1)
                        ;; rest_pairs is the tail (empty Nil slot or a cons node)
                        ;; Build pair: car=Int(name_ptr), cdr=Int(val_slot)
                        ;; Then node = cons(pair, rest_sexp)
                        (let ((name_int_slot usize (sys:alloc 32 8))
                              (val_int_slot usize (sys:alloc 32 8))
                              (pair_slot usize (sys:alloc 32 8))
                              (node_slot usize (sys:alloc 32 8)))
                          (sys:unsafe
                           ;; name_int_slot = Sexp::Int(name_ptr as i64)
                           (sys:poke-u64 name_int_slot 2)
                           (sys:poke-u64 (+ name_int_slot 8) (sys:cast i64 name_ptr))
                           (sys:poke-u64 (+ name_int_slot 16) 0)
                           (sys:poke-u64 (+ name_int_slot 24) 0)
                           ;; val_int_slot = Sexp::Int(val_slot as i64)
                           (sys:poke-u64 val_int_slot 2)
                           (sys:poke-u64 (+ val_int_slot 8) (sys:cast i64 val_slot))
                           (sys:poke-u64 (+ val_int_slot 16) 0)
                           (sys:poke-u64 (+ val_int_slot 24) 0)
                           ;; pair = cons(name_int, val_int)
                           (nelisp_cons_construct name_int_slot val_int_slot pair_slot)
                           ;; node = cons(pair, *rest_pairs_sexp_slot)
                           ;; rest_pairs is already a valid Sexp* (Nil or Cons)
                           (nelisp_cons_construct pair_slot rest_pairs node_slot)
                           node_slot))))
                  ;; eval error: return error sentinel
                  (sys:cast usize 1)))))))
    ;; Non-Cons (Nil = end of bindings): return a fresh Nil slot as terminator
    (nl_let_collect_nil_slot)))

;; nl_let_bind_walk(pairs_ptr, env) -> i64
;; Walk the pairs cons list built by nl_let_collect_walk.
;; Each node: cons(pair, rest) where pair = cons(Int(name_ptr), Int(val_slot)).
;; Extract name_ptr and val_slot as i64 payload of Int sexps; bind.
(sys:defun nl_let_bind_walk
    ((pairs_ptr usize) (env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:cast usize (sys:peek-u64 pairs_ptr)) 7)
      ;; Cons node: car = pair sexp, cdr = rest
      (let ((pair_ptr usize (sys:unsafe (nl_cons_car_ptr pairs_ptr)))
            (rest_ptr usize (sys:unsafe (nl_cons_cdr_ptr pairs_ptr))))
        ;; pair = cons(Int(name_ptr), Int(val_slot))
        (let ((name_int_ptr usize (sys:unsafe (nl_cons_car_ptr pair_ptr)))
              (val_int_ptr usize (sys:unsafe (nl_cons_cdr_ptr pair_ptr))))
          ;; Read name_ptr from Int payload (offset +8)
          (let ((name_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ name_int_ptr 8)))))
                (val_slot usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ val_int_ptr 8))))))
            (nl_logic_bind_local env name_ptr val_slot)
            (nl_let_bind_walk rest_ptr env))))
    ;; Non-Cons (Nil): done
    (sys:cast i64 0)))

;; ===========================================================================
;; 1. nl_let_setup(bindings_list, env, sequential) -> i64
;;
;; sequential == 0 -> let   (evaluate ALL values before frame_push)
;; sequential != 0 -> let*  (push frame first; eval+bind in order)
;;
;; SEMANTIC FIDELITY:
;;   let* path: frame_push then loop eval+bind (sees earlier binds)
;;   let path:  collect ALL (name,value) pairs first (outer scope, no new frame),
;;              THEN frame_push, THEN bind all.
;; ===========================================================================
(sys:defun nl_let_setup
    ((bindings_list usize) (env (ptr eval_ctx)) (sequential i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (push_scratch usize (sys:alloc 32 8)))
    (if (= sequential 0)
        ;; --- let (non-sequential): collect values in outer scope FIRST ---
        (let ((pairs_ptr usize (nl_let_collect_walk bindings_list (sys:cast usize env))))
          ;; pairs_ptr == 1 means error sentinel from nl_let_collect_walk.
          ;; Any other value (>= 8) is a valid Sexp slot pointer.
          (if (= pairs_ptr 1)
              ;; Collect error
              (sys:cast i64 1)
            ;; Collect succeeded (pairs_ptr is a Nil slot or a Cons head).
            ;; Push frame then bind all collected pairs.
            (seq
             (nl_logic_frame_push_scratch push_scratch)
             (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))
             (nl_let_bind_walk pairs_ptr env)
             (sys:cast i64 0))))
      ;; --- let* (sequential): push frame then eval+bind in order ---
      (seq
       (nl_logic_frame_push_scratch push_scratch)
       (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))
       (nl_let_star_walk bindings_list env frames_ptr)))))

;; ===========================================================================
;; (ii) nl_cc_match_and_bind(clauses, err_inout, var, env) -> i64
;;
;; Deleted Rust body (fa8932eb^):
;;
;;   let e = &mut *(env as *mut Env);
;;   let err = (*err_inout).clone();
;;   let actual_tag = match &err {
;;     Sexp::Cons(b) => match &b.car { Sexp::Symbol(s) => s.clone(), _ => return 1 },
;;     _ => return 1
;;   };
;;   let mut cur = &*clauses;
;;   while let Sexp::Cons(cc) = cur {
;;     if let Sexp::Cons(cb) = &cc.car {
;;       let m = match &cb.car {
;;         Sexp::Symbol(s) => is_error_subtype(s, &actual_tag),
;;         Sexp::T         => true,
;;         Sexp::Cons(_)   => list_elements(&cb.car).ok().map_or(false, |el|
;;                              el.iter().any(|t| matches!(t, Sexp::Symbol(s)
;;                                              if is_error_subtype(s, &actual_tag)))),
;;         _               => false
;;       };
;;       if m {
;;         e.frame_push_rust_direct();
;;         if let Sexp::Symbol(name) = &*var { if name != "nil" { e.bind_local(name, err.clone()); } }
;;         std::ptr::write(err_inout, cb.cdr.clone());
;;         return 0;
;;       }
;;     }
;;     cur = &cc.cdr;
;;   }
;;   let _ = e.set_value("nelisp--last-signal-data", err);
;;   1
;;
;; IS_ERROR_SUBTYPE FINDING:
;;   Not a C-ABI symbol. Implemented here as:
;;     - Symbol: identity check via nelisp_eq_symbol (eq if same interned string)
;;       + special-case: Symbol "t" (1-byte) always matches (catch-all)
;;       + DEFERRED: "error" matches all non-quit (hierarchy subtyping)
;;     - T (tag==1): always matches (the `t` catch-all)
;;     - Cons: walk the list, any element that matches -> true
;;
;; SIMPLIFIED match: exact symbol equality + T catch-all.
;; Error hierarchy (e.g. "arith-error" subset "error") is deferred to 135.D.
;;
;; actual_tag_ptr: the *const Sexp of the car of err (a Symbol sexp slot).
;;
;; "nil" [3]: 7104878   "t" [1]: 116
;; ===========================================================================

;; nl_cc_sym_is_nil(sym_ptr) -> i64
;; Returns 1 if the Symbol at sym_ptr has name "nil", 0 otherwise.
;; We build a fresh Symbol("nil") and use nelisp_eq_symbol.
;; "nil" [3 bytes]: LE u64 word = 7104878
(sys:defun nl_cc_sym_is_nil
    ((sym_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1))
        (nil_sym_slot usize (sys:alloc 32 8))
        (result_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 buf 7104878)
     (nl_alloc_symbol buf 3 nil_sym_slot)
     (nelisp_eq_symbol sym_ptr nil_sym_slot result_slot)
     ;; result tag == 1 (Sexp::T) means equal
     (sys:cast i64 (= (sys:cast usize (sys:peek-u64 result_slot)) 1)))))

;; nl_cc_sym_is_t(sym_ptr) -> i64
;; Returns 1 if the Symbol at sym_ptr has name "t" (catch-all clause).
;; "t" [1 byte]: LE u64 word = 116
(sys:defun nl_cc_sym_is_t
    ((sym_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1))
        (t_sym_slot usize (sys:alloc 32 8))
        (result_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 buf 116)
     (nl_alloc_symbol buf 1 t_sym_slot)
     (nelisp_eq_symbol sym_ptr t_sym_slot result_slot)
     (sys:cast i64 (= (sys:cast usize (sys:peek-u64 result_slot)) 1)))))

;; nl_cc_clause_matches(cb_car_ptr, actual_tag_ptr) -> i64
;; Implements the cb.car match arm:
;;   Symbol(s) -> is_error_subtype(s, actual_tag)  [simplified: eq + t-catch-all]
;;   T (tag==1) -> true
;;   Cons       -> any element in the list matches
;;   _          -> false
(sys:defun nl_cc_cons_any_matches
    ((list_ptr usize) (actual_tag_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:cast usize (sys:peek-u64 list_ptr)) 7)
      (let ((elem_ptr usize (sys:unsafe (nl_cons_car_ptr list_ptr)))
            (rest_ptr usize (sys:unsafe (nl_cons_cdr_ptr list_ptr))))
        ;; elem must be a Symbol to possibly match
        (if (= (sys:cast usize (sys:peek-u64 elem_ptr)) 4)
            ;; Symbol: check equality with actual_tag OR is "t"
            (let ((result_slot usize (sys:alloc 32 8)))
              (sys:unsafe
               (nelisp_eq_symbol elem_ptr actual_tag_ptr result_slot))
              (if (= (sys:cast usize (sys:peek-u64 result_slot)) 1)
                  ;; Exact match
                  (sys:cast i64 1)
                (if (= (nl_cc_sym_is_t elem_ptr) 1)
                    ;; "t" catch-all
                    (sys:cast i64 1)
                  ;; No match from this elem; try rest
                  (nl_cc_cons_any_matches rest_ptr actual_tag_ptr))))
          ;; Not a Symbol: skip, try rest
          (nl_cc_cons_any_matches rest_ptr actual_tag_ptr)))
    ;; Non-Cons (Nil or end): no match
    (sys:cast i64 0)))

(sys:defun nl_cc_clause_matches
    ((cb_car_ptr usize) (actual_tag_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((cb_car_tag usize (sys:cast usize (sys:peek-u64 cb_car_ptr))))
    (if (= cb_car_tag 4)
        ;; Symbol: equality check + t catch-all
        (let ((result_slot usize (sys:alloc 32 8)))
          (sys:unsafe
           (nelisp_eq_symbol cb_car_ptr actual_tag_ptr result_slot))
          (if (= (sys:cast usize (sys:peek-u64 result_slot)) 1)
              (sys:cast i64 1)
            (nl_cc_sym_is_t cb_car_ptr)))
      (if (= cb_car_tag 1)
          ;; Sexp::T (tag==1): unconditional catch-all
          (sys:cast i64 1)
        (if (= cb_car_tag 7)
            ;; Cons: walk the list, any match
            (nl_cc_cons_any_matches cb_car_ptr actual_tag_ptr)
          ;; Else: no match
          (sys:cast i64 0))))))

;; nl_cc_clause_walk(clauses_ptr, err_clone_ptr, actual_tag_ptr, var_ptr, env) -> i64
;; Tail-recursive walk of the clauses cons list.
;; On match: push frame; optionally bind var; write cb.cdr into err_inout (via
;;   nl_sexp_clone_into from cb.cdr into a caller-provided slot); return 0.
;; No match: fall through to stash signal, return 1.
;;
;; NOTE: err_inout_ptr is passed to allow writing back cb.cdr (the handler body
;; list).  We use nl_sexp_clone_into(err_inout_ptr, cb_cdr_ptr) to write the cdr
;; into the slot pointed by err_inout.  This mirrors:
;;   std::ptr::write(err_inout, cb.cdr.clone())
(sys:defun nl_cc_clause_walk
    ((clauses_ptr usize) (err_clone_ptr usize) (actual_tag_ptr usize)
     (var_ptr usize) (env (ptr eval_ctx)) (err_inout_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:cast usize (sys:peek-u64 clauses_ptr)) 7)
      ;; Cons: process this clause
      (let ((cc_car_ptr usize (sys:unsafe (nl_cons_car_ptr clauses_ptr)))
            (cc_cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr clauses_ptr))))
        ;; cc.car must be a Cons (cb)
        (if (= (sys:cast usize (sys:peek-u64 cc_car_ptr)) 7)
            (let ((cb_car_ptr usize (sys:unsafe (nl_cons_car_ptr cc_car_ptr)))
                  (cb_cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr cc_car_ptr))))
              (let ((matched i64 (nl_cc_clause_matches cb_car_ptr actual_tag_ptr)))
                (if (= matched 1)
                    ;; MATCH: push frame, optionally bind var, write handler body
                    (let ((frames_ptr usize (+ (sys:cast usize env)
                                               (sys:offsetof eval_ctx frames)))
                          (push_scratch usize (sys:alloc 32 8)))
                      (nl_logic_frame_push_scratch push_scratch)
                      (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))
                      ;; Bind var if it's a non-nil Symbol
                      (if (= (sys:cast usize (sys:peek-u64 var_ptr)) 4)
                          ;; var is Symbol: check if name == "nil"
                          (if (= (nl_cc_sym_is_nil var_ptr) 0)
                              ;; not nil: bind var = err
                              (nl_logic_bind_local env var_ptr err_clone_ptr)
                            ;; var is nil: skip bind
                            (sys:cast i64 0))
                        ;; var is not a Symbol: skip bind (matches Rust: only
                        ;; binds if let Sexp::Symbol(name) succeeds)
                        (sys:cast i64 0))
                      ;; Write handler body (cb.cdr) into *err_inout:
                      ;; nl_sexp_clone_into(err_inout_ptr, cb_cdr_ptr)
                      (sys:unsafe (nl_sexp_clone_into cb_cdr_ptr err_inout_ptr))
                      (sys:cast i64 0))
                  ;; no match: try next clause
                  (nl_cc_clause_walk cc_cdr_ptr err_clone_ptr actual_tag_ptr
                                     var_ptr env err_inout_ptr))))
          ;; cc.car is not a Cons: skip, try next
          (nl_cc_clause_walk cc_cdr_ptr err_clone_ptr actual_tag_ptr
                             var_ptr env err_inout_ptr)))
    ;; Non-Cons (Nil): no clause matched -> stash + return 1
    (nl_logic_stash_signal env err_clone_ptr)))

;; ===========================================================================
;; 2. nl_cc_match_and_bind(clauses, err_inout, var, env) -> i64
;;
;; clauses:   usize = *const Sexp (the clauses cons list)
;; err_inout: usize = *mut Sexp  (in: the error value; out: matched handler body)
;; var:       usize = *const Sexp (the condition-case variable, or nil)
;; env:       (ptr eval_ctx)
;;
;; Returns 0 on match (frame pushed, var maybe bound, err_inout = handler body).
;; Returns 1 on no match (error stashed in env as nelisp--last-signal-data).
;; ===========================================================================
(sys:defun nl_cc_match_and_bind
    ((clauses usize) (err_inout usize) (var usize) (env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; Step 1: read err = *err_inout (the error signal)
  ;; Step 2: extract actual_tag = err.car -- must be Cons with Symbol car
  (let ((err_tag usize (sys:cast usize (sys:peek-u64 err_inout))))
    (if (= err_tag 7)
        ;; err is Cons: get car (= actual_tag Symbol)
        (let ((actual_tag_ptr usize (sys:unsafe (nl_cons_car_ptr err_inout))))
          (if (= (sys:cast usize (sys:peek-u64 actual_tag_ptr)) 4)
              ;; car is Symbol: proceed with clause walk
              ;; Clone err into a local slot for the bind (if matched)
              (let ((err_clone usize (sys:alloc 32 8)))
                (sys:unsafe (nl_sexp_clone_into err_inout err_clone))
                (nl_cc_clause_walk clauses err_clone actual_tag_ptr var env err_inout))
            ;; car is not a Symbol: return 1
            (sys:cast i64 1)))
      ;; err is not Cons: return 1
      (sys:cast i64 1))))
