;; Doc 135 Stage 135.D — nl_apply_function; STAGED; env-independent builtins forward
;; to nelisp_apply_function, env-using handled directly.
;; (unwired — do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements nl_apply_function(func_ptr, args_list_ptr, env, out) -> i64
;;
;; Rust equivalent (mod.rs apply_function, nelisp_apply_function wrapper):
;;   #[no_mangle] pub unsafe extern "C" fn nelisp_apply_function(
;;     func: *const Sexp, args_list: *const Sexp,
;;     env: *mut c_void, out: *mut Sexp) -> i64
;;   {
;;     let r = &mut *(env as *mut Env);
;;     let res = apply_function(&*func, &list_elements(&*args_list).unwrap_or_default(), r);
;;     eval_stash_err(r, res, out)
;;   }
;;
;;   fn apply_function(func: &Sexp, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
;;     let wt = || EvalError::wrong_type("function", func.clone());
;;     let Sexp::Cons(b) = func else { return Err(wt()); };
;;     let Sexp::Symbol(head) = &b.car else { return Err(wt()); };
;;     match head.as_str() {
;;       "builtin" => {
;;         let Sexp::Cons(inner) = &b.cdr else { return Err(internal(..)) };
;;         let name = match &inner.car { Symbol(s)|Str(s) => s.clone(), _ => Err(..) };
;;         builtins::dispatch(&name, args, env)
;;       },
;;       head @ ("closure"|"lambda") => {
;;         let parts = list_elements(func)?;
;;         let (captured, fi, bs) = if head=="closure" { (parts[1].clone(),2,3) }
;;                                  else { (Sexp::Nil,1,2) };
;;         let rc = apply_lambda_inner_call(&captured, &parts[fi], &list_from(&parts[bs..]),
;;                                          &list_from(args), env, &mut out);
;;         if rc==0 { Ok(out) } else { Err(consume_stashed_error(env,"apply_lambda")) }
;;       },
;;       "macro" => Err(wrong_type("function (not macro)", func)),
;;       _ => Err(wt())
;;     }
;;   }
;;
;; ROUTING PLAN:
;;   func must be Cons; func.car must be Symbol.
;;   head=="closure"|"lambda" → nl_apply_lambda_inner (env-using, handled directly)
;;   head=="builtin" → name = func.cdr.car (Symbol/Str)
;;     ENV-INDEPENDENT names → forward to nelisp_apply_function (re-dispatches, ignores env ptr)
;;     ENV-USING names → handle directly:
;;       "eval"                       → nelisp_eval_call(args[0], ctx, out)
;;       "funcall"                    → lookup function if symbol; nl_apply_function recursive
;;       "apply"                      → like funcall, last arg spliced
;;       "nelisp--push-frame"         → nelisp_frame_push with 7-slot scratch
;;       "nelisp--pop-frame"          → nelisp_frame_pop with nil scratch
;;       "nelisp--bind-local"         → nelisp_env_bind_local via nl_logic_bind_local pattern
;;       "nelisp--push-captured"      → nl_env_push_captured(ctx, args[0])
;;       "nelisp--set-use-elisp-apply" → set ctx.flags bit0 from args[0]; return T/Nil
;;     DEFERRED env-using names (need ctx-globals-op / platform logic):
;;       "symbol-function" / "fset" / "nelisp--syscall-canonicalize" /
;;       "nelisp--syscall-stat" / "nelisp--syscall-readdir" /
;;       "nelisp--syscall-read-file" / "nelisp--apply-lambda-inner" /
;;       "nelisp--apply-builtin-dispatch" / "signal"
;;       → these are NOT forwarded to nelisp_apply_function (mis-deref of ctx as Env).
;;       → each returns error code 1 with a wrong-type stash as sentinel.
;;
;; STRATEGY for "is this name env-using?": check the env-using set first (short list);
;; if not in it, forward to nelisp_apply_function. Detection uses nelisp_eq_symbol or
;; byte comparison (peek-u64 word0 + len check) on the name Sexp.
;;
;; ---------------------------------------------------------------------------
;; Archive symbol verification (target/debug/.../libnelisp_elisp_spike.a):
;;   nl_apply_lambda_inner  T 0x03bc  (archive)
;;   nelisp_apply_function         -- NOT in archive; defined in Rust binary (mod.rs
;;                                    #[no_mangle]); declared as sys:extern here, resolved
;;                                    at final link (same model as nelisp_eval_call U).
;;   nelisp_eval_call              U  (provided by Rust binary at link)
;;   nelisp_env_lookup_function    T 0x0000  (archive)
;;   nelisp_env_bind_local         T 0x0171  (archive)
;;   nelisp_frame_push             T 0x0000  (archive)
;;   nelisp_frame_pop              T 0x00df  (archive)
;;   nelisp_cons_construct         T 0x0000  (archive)
;;   nl_cons_car_ptr               T 0x0000  (archive)
;;   nl_cons_cdr_ptr               T 0x0000  (archive)
;;   nl_alloc_symbol               T 0x04cf  (archive)
;;   nl_alloc_vector               T 0x01f4  (archive)
;;   nl_sexp_clone_into            U  (provided by Rust binary at link)
;;   nelisp_eq_symbol              T 0x0000  (archive)
;;   nl_env_push_captured          U  (staged .nl — resolved at link)
;;
;; Sexp tag constants (verified):
;;   0 = Nil   1 = T   2 = Int   4 = Symbol   5 = Str   7 = Cons   8 = Vector
;;
;; EvalCtx layout (LOCKED §2.3):
;;   offset 0:   mirror   (sexp, 32B)
;;   offset 32:  frames   (sexp, 32B)
;;   offset 64:  unbound  (sexp, 32B)
;;   offset 96:  rec_cur  (i64)
;;   offset 104: rec_max  (i64)
;;   offset 112: flags    (i64)
;;
;; Sexp layout (32B):
;;   offset 0:  tag (u8, 7 pad bytes)  → peek-u64 = tag for small values
;;   offset 8:  payload (u64)
;;   offset 16: pad (16 bytes)
;;
;; String LE-u64 encodings (used for name comparisons):
;;   "closure"                      [ 7]: 28554821421198435
;;   "lambda"                       [ 6]: 107083775959404
;;   "builtin"                      [ 7]: 31078196194145634
;;   "eval"                         [ 4]: 1818326629
;;   "funcall"                      [ 7]: 30518463020561766
;;   "apply"                        [ 5]: 521510350945
;;   "nelisp--push-frame"           [18]: 3255381746650998126, 7021787114235983216, 25965
;;   "nelisp--pop-frame"            [17]: 3255381746650998126, 7881706606049652592, 101
;;   "nelisp--bind-local"           [18]: 3255381746650998126, 7165064474384034146, 27745
;;   "nelisp--push-captured"        [21]: 3255381746650998126, 8097862651665937776, 431198729588
;;   "nelisp--set-use-elisp-apply"  [27]: 3255381746650998126, 3271147651465504115,
;;                                        8097803565984738405, 7957616
;;   "nil" [ 3]: 7104878
;;   "t"   [ 1]: 116
;;   "wrong-type-argument" [19]: 8751669898145395319, 7887324063363589488, 7630437
;;   "symbol" [6]: 119225648511347

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

;; nelisp_eval_call(form_ptr, env_ptr, out_ptr) -> i64
;; 0=ok (out holds result), 1=err (signal stashed in env).
(sys:extern nelisp_eval_call
  (:symbol "nelisp_eval_call" :abi c :unsafe t)
  ((form_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_apply_function(func_ptr, args_list_ptr, env_ptr, out_ptr) -> i64
;; env_ptr must point at Env (NOT eval_ctx) — only safe for env-independent builtins
;; that do not dereference env. Declared U here; resolved at link from Rust binary.
(sys:extern nelisp_apply_function
  (:symbol "nelisp_apply_function" :abi c :unsafe t)
  ((func_ptr usize) (args_list_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_apply_lambda_inner(captured, formals, body_list, args_list, env, out) -> i64
;; 0=ok, 1=err. env = opaque *mut c_void = eval_ctx* post-cutover.
(sys:extern nl_apply_lambda_inner
  (:symbol "nl_apply_lambda_inner" :abi c :unsafe t)
  ((captured_ptr usize) (formals_ptr usize) (body_list_ptr usize)
   (args_list_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_env_lookup_function(mirror_ptr, unbound_ptr, name_ptr, out) -> i64
;; 0=ok (out = function Sexp), 1=err (unbound).
(sys:extern nelisp_env_lookup_function
  (:symbol "nelisp_env_lookup_function" :abi c :unsafe t)
  ((mirror_ptr usize) (unbound_ptr usize) (name_ptr usize) (out_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_bind_local(mirror, frames, name, val, scratch, _pad) -> i64
(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_frame_push(frames_ptr, scratch_vec_ptr) -> i64
(sys:extern nelisp_frame_push
  (:symbol "nelisp_frame_push" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_vec_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_frame_pop(frames_ptr, scratch_slot) -> i64
(sys:extern nelisp_frame_pop
  (:symbol "nelisp_frame_pop" :abi c :unsafe t)
  ((frames_ptr usize) (scratch_slot usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((car_ptr usize) (cdr_ptr usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_cons_car_ptr(cons_ptr) -> usize
(sys:extern nl_cons_car_ptr
  (:symbol "nl_cons_car_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_cons_cdr_ptr(cons_ptr) -> usize
(sys:extern nl_cons_cdr_ptr
  (:symbol "nl_cons_cdr_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot (usize)
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

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

;; nl_sexp_clone_into(dst, src) -> i64 — refcount-aware clone; U in archive
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_eq_symbol(a, b, result_slot) -> usize
;; result_slot tag==1 means equal (Sexp::T), tag==0 means not equal (Sexp::Nil).
(sys:extern nelisp_eq_symbol
  (:symbol "nelisp_eq_symbol" :abi c :unsafe t)
  ((a_ptr usize) (b_ptr usize) (result_slot usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_env_push_captured(env, alist_ptr) -> i64
;; U in archive — staged in env-leaves-frame.nl; resolved at link.
(sys:extern nl_env_push_captured
  (:symbol "nl_env_push_captured" :abi c :unsafe t)
  ((env_ptr usize) (alist_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; UTILITY HELPERS
;; ---------------------------------------------------------------------------

;; nl_apply_write_nil(slot) -> i64 — write Sexp::Nil into a 32B slot.
(sys:defun nl_apply_write_nil
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   (sys:cast i64 0)))

;; nl_apply_sym_eq_bytes(sym_ptr, buf, len) -> i64
;; Compare a Symbol/Str Sexp's name against a known byte string at buf (len bytes).
;; Uses nelisp_eq_symbol: build a fresh Symbol from buf/len, then compare.
;; Returns 1 if equal, 0 if not.
(sys:defun nl_apply_sym_eq_bytes
    ((sym_ptr usize) (buf usize) (len i64))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tmp_slot usize (sys:alloc 32 8))
        (result_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (nl_alloc_symbol buf len tmp_slot)
     (nelisp_eq_symbol sym_ptr tmp_slot result_slot)
     (sys:cast i64 (= (sys:cast usize (sys:peek-u64 result_slot)) 1)))))

;; ---------------------------------------------------------------------------
;; NAME COMPARISON HELPERS
;; Each allocates the literal bytes on the stack, calls nl_apply_sym_eq_bytes.
;; ---------------------------------------------------------------------------

;; nl_apply_name_eq_closure(name_ptr) -> i64   "closure" [7]
(sys:defun nl_apply_name_eq_closure
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 28554821421198435))
    (nl_apply_sym_eq_bytes name_ptr buf 7)))

;; nl_apply_name_eq_lambda(name_ptr) -> i64   "lambda" [6]
(sys:defun nl_apply_name_eq_lambda
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 107083775959404))
    (nl_apply_sym_eq_bytes name_ptr buf 6)))

;; nl_apply_name_eq_eval(name_ptr) -> i64   "eval" [4]
(sys:defun nl_apply_name_eq_eval
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 1818326629))
    (nl_apply_sym_eq_bytes name_ptr buf 4)))

;; nl_apply_name_eq_funcall(name_ptr) -> i64   "funcall" [7]
(sys:defun nl_apply_name_eq_funcall
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 30518463020561766))
    (nl_apply_sym_eq_bytes name_ptr buf 7)))

;; nl_apply_name_eq_apply(name_ptr) -> i64   "apply" [5]
(sys:defun nl_apply_name_eq_apply
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 521510350945))
    (nl_apply_sym_eq_bytes name_ptr buf 5)))

;; nl_apply_name_eq_push_frame(name_ptr) -> i64   "nelisp--push-frame" [18]
(sys:defun nl_apply_name_eq_push_frame
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7021787114235983216)
     (sys:poke-u64 (+ buf 16) 25965))
    (nl_apply_sym_eq_bytes name_ptr buf 18)))

;; nl_apply_name_eq_pop_frame(name_ptr) -> i64   "nelisp--pop-frame" [17]
(sys:defun nl_apply_name_eq_pop_frame
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7881706606049652592)
     (sys:poke-u64 (+ buf 16) 101))
    (nl_apply_sym_eq_bytes name_ptr buf 17)))

;; nl_apply_name_eq_bind_local(name_ptr) -> i64   "nelisp--bind-local" [18]
(sys:defun nl_apply_name_eq_bind_local
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7165064474384034146)
     (sys:poke-u64 (+ buf 16) 27745))
    (nl_apply_sym_eq_bytes name_ptr buf 18)))

;; nl_apply_name_eq_push_captured(name_ptr) -> i64   "nelisp--push-captured" [21]
(sys:defun nl_apply_name_eq_push_captured
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 8097862651665937776)
     (sys:poke-u64 (+ buf 16) 431198729588))
    (nl_apply_sym_eq_bytes name_ptr buf 21)))

;; nl_apply_name_eq_set_use_elisp(name_ptr) -> i64   "nelisp--set-use-elisp-apply" [27]
(sys:defun nl_apply_name_eq_set_use_elisp
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 3271147651465504115)
     (sys:poke-u64 (+ buf 16) 8097803565984738405)
     (sys:poke-u64 (+ buf 24) 7957616))
    (nl_apply_sym_eq_bytes name_ptr buf 27)))

;; ---------------------------------------------------------------------------
;; DEFERRED NAMES DETECTION
;;
;; Deferred env-using builtins: "symbol-function", "fset",
;; "nelisp--syscall-canonicalize", "nelisp--syscall-stat", "nelisp--syscall-readdir",
;; "nelisp--syscall-read-file", "nelisp--apply-lambda-inner",
;; "nelisp--apply-builtin-dispatch", "signal".
;;
;; Strategy: detect by prefix or exact name. All deferred nelisp-- names start
;; with "nelisp--s" or "nelisp--a" (suffixes that the handled set doesn't have).
;; Simplest: just check all 7 already-handled names; if none match → forward.
;; That is correct because env-independent set = "everything not in env-using set".
;;
;; nl_apply_name_is_env_using(name_ptr) -> i64
;; Returns 1 if the name is in the env-using set (handled here),
;; OR in the deferred set (must not forward to nelisp_apply_function).
;; Returns 0 if the name is env-independent (safe to forward).
;;
;; Deferred detection: check the remaining known deferred names so we don't
;; accidentally forward them. We use prefix-length checks for the nelisp-- ones
;; and exact match for "signal", "symbol-function", "fset".
;; ---------------------------------------------------------------------------

;; String encodings for deferred names:
;;   "signal"                       [ 6]: 120326913843059
;;   "symbol-function"              [15]: 8098507789396334451, 28548694568714604
;;   "fset"                         [ 4]: 1952998502
;;   "nelisp--syscall-canonicalize" [29]: 3255381746650998126, 8601406760870944115,
;;                                        8102154671614844003, 28550
;;   "nelisp--syscall-stat"         [21]: 3255381746650998126, 8601406760870944115,
;;                                        496002927987825
;;   "nelisp--syscall-readdir"      [23]: 3255381746650998126, 8601406760870944115,
;;                                        8314050022174544002, 30
;;   "nelisp--syscall-read-file"    [26]: 3255381746650998126, 8601406760870944115,
;;                                        5928264378481647986, 30309
;;   "nelisp--apply-lambda-inner"   [26]: 3255381746650998126, 8384413200011491681,
;;                                        8238096682736093546, 26990
;;   "nelisp--apply-builtin-dispatch" [31]: 3255381746650998126, 8384413200011491681,
;;                                          8241629855082783843, 7593852803963752557, 105

;; SIMPLIFICATION: Rather than encoding all deferred names, we detect env-using
;; by checking all directly handled names. If none matches AND the name is not
;; in the deferred set, we forward.
;; For the deferred set: we know they all start with "nelisp--s" (syscall-*) or
;; "nelisp--a" (apply-*) or are "signal" / "symbol-function" / "fset".
;; The handled set starts with "nelisp--p" (push-*) or "nelisp--b" (bind-*) or
;; "nelisp--set-use-*" or are "eval" / "funcall" / "apply".
;; So: forward = NOT(any env-using match) AND NOT(any deferred match).
;;
;; Implemented as: check all 7 handled env-using names; if any → return 1 (env-using).
;; Then check deferred names; if any → return 2 (deferred/error).
;; Else return 0 (env-independent, safe to forward).
;;
;; Callers treat: 0 = forward, 1 = handled, 2 = deferred-error.
;;
;; Encodes deferred names inline for brevity (only word0 check is needed since
;; the handled and deferred sets have distinct word1 values among nelisp-- names).
;; ---------------------------------------------------------------------------

;; nl_apply_name_is_deferred(name_ptr) -> i64
;; Returns 1 if name is in the deferred env-using set (symbol-function, fset,
;; signal, nelisp--syscall-*, nelisp--apply-lambda-inner, nelisp--apply-builtin-dispatch).
;; Simplest discriminant: "nelisp--s*" and "nelisp--a*" groups, plus exact "signal",
;; "symbol-function", "fset" — use nl_apply_sym_eq_bytes for each.
(sys:defun nl_apply_deferred_signal
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "signal" [6]: 119165719898483
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 119165719898483))
    (nl_apply_sym_eq_bytes name_ptr buf 6)))

(sys:defun nl_apply_deferred_symbol_function
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "symbol-function" [15]: 7362660191469140339, 31084746153094773
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7362660191469140339)
     (sys:poke-u64 (+ buf 8) 31084746153094773))
    (nl_apply_sym_eq_bytes name_ptr buf 15)))

(sys:defun nl_apply_deferred_fset
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "fset" [4]: 1952805734
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 1952805734))
    (nl_apply_sym_eq_bytes name_ptr buf 4)))

;; nl_apply_deferred_syscall_stat(name_ptr) -> i64   "nelisp--syscall-stat" [20]
(sys:defun nl_apply_deferred_syscall_stat
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--syscall-stat" [20]: 3255381746650998126, 3273110194727647603, 1952543859
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 3273110194727647603)
     (sys:poke-u64 (+ buf 16) 1952543859))
    (nl_apply_sym_eq_bytes name_ptr buf 20)))

;; nl_apply_deferred_syscall_readdir(name_ptr) -> i64   "nelisp--syscall-readdir" [23]
(sys:defun nl_apply_deferred_syscall_readdir
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--syscall-readdir" [23]: 3255381746650998126, 3273110194727647603,
  ;;                                  32204027246765426
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 3273110194727647603)
     (sys:poke-u64 (+ buf 16) 32204027246765426))
    (nl_apply_sym_eq_bytes name_ptr buf 23)))

;; nl_apply_deferred_syscall_read_file(name_ptr) -> i64   "nelisp--syscall-read-file" [25]
(sys:defun nl_apply_deferred_syscall_read_file
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--syscall-read-file" [25]: 3255381746650998126, 3273110194727647603,
  ;;                                    7811887373794502002, 101
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 3273110194727647603)
     (sys:poke-u64 (+ buf 16) 7811887373794502002)
     (sys:poke-u64 (+ buf 24) 101))
    (nl_apply_sym_eq_bytes name_ptr buf 25)))

;; nl_apply_deferred_syscall_canonicalize(name_ptr) -> i64   "nelisp--syscall-canonicalize" [28]
(sys:defun nl_apply_deferred_syscall_canonicalize
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--syscall-canonicalize" [28]: 3255381746650998126, 3273110194727647603,
  ;;                                       7017568567410188643, 1702521196
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 3273110194727647603)
     (sys:poke-u64 (+ buf 16) 7017568567410188643)
     (sys:poke-u64 (+ buf 24) 1702521196))
    (nl_apply_sym_eq_bytes name_ptr buf 28)))

;; nl_apply_deferred_apply_lambda(name_ptr) -> i64   "nelisp--apply-lambda-inner" [26]
(sys:defun nl_apply_deferred_apply_lambda
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--apply-lambda-inner" [26]: 3255381746650998126, 7020035918697361505,
  ;;                                     7957413235238658669, 29285
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 7020035918697361505)
     (sys:poke-u64 (+ buf 16) 7957413235238658669)
     (sys:poke-u64 (+ buf 24) 29285))
    (nl_apply_sym_eq_bytes name_ptr buf 26)))

;; nl_apply_deferred_apply_builtin(name_ptr) -> i64   "nelisp--apply-builtin-dispatch" [30]
(sys:defun nl_apply_deferred_apply_builtin
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "nelisp--apply-builtin-dispatch" [30]: 3255381746650998126, 8458373049688813665,
  ;;                                         7594244823892388969, 114776363593843
  (let ((buf usize (sys:alloc 32 1)))
    (sys:unsafe
     (sys:poke-u64 buf 3255381746650998126)
     (sys:poke-u64 (+ buf 8) 8458373049688813665)
     (sys:poke-u64 (+ buf 16) 7594244823892388969)
     (sys:poke-u64 (+ buf 24) 114776363593843))
    (nl_apply_sym_eq_bytes name_ptr buf 30)))

;; nl_apply_name_classify(name_ptr) -> i64
;; Returns:
;;   0 = env-independent (forward to nelisp_apply_function)
;;   1 = env-using, handled ("eval","funcall","apply","nelisp--push-frame",
;;                           "nelisp--pop-frame","nelisp--bind-local",
;;                           "nelisp--push-captured","nelisp--set-use-elisp-apply")
;;   2 = deferred env-using (return error; do NOT forward)
(sys:defun nl_apply_name_classify
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; Check env-using handled set first
  (if (= (nl_apply_name_eq_eval name_ptr) 1)
      (sys:cast i64 1)
    (if (= (nl_apply_name_eq_funcall name_ptr) 1)
        (sys:cast i64 1)
      (if (= (nl_apply_name_eq_apply name_ptr) 1)
          (sys:cast i64 1)
        (if (= (nl_apply_name_eq_push_frame name_ptr) 1)
            (sys:cast i64 1)
          (if (= (nl_apply_name_eq_pop_frame name_ptr) 1)
              (sys:cast i64 1)
            (if (= (nl_apply_name_eq_bind_local name_ptr) 1)
                (sys:cast i64 1)
              (if (= (nl_apply_name_eq_push_captured name_ptr) 1)
                  (sys:cast i64 1)
                (if (= (nl_apply_name_eq_set_use_elisp name_ptr) 1)
                    (sys:cast i64 1)
                  ;; Not in handled set. Check deferred set (exact-match each).
                  (if (= (nl_apply_deferred_signal name_ptr) 1)
                      (sys:cast i64 2)
                    (if (= (nl_apply_deferred_symbol_function name_ptr) 1)
                        (sys:cast i64 2)
                      (if (= (nl_apply_deferred_fset name_ptr) 1)
                          (sys:cast i64 2)
                        (if (= (nl_apply_deferred_syscall_stat name_ptr) 1)
                            (sys:cast i64 2)
                          (if (= (nl_apply_deferred_syscall_readdir name_ptr) 1)
                              (sys:cast i64 2)
                            (if (= (nl_apply_deferred_syscall_read_file name_ptr) 1)
                                (sys:cast i64 2)
                              (if (= (nl_apply_deferred_syscall_canonicalize name_ptr) 1)
                                  (sys:cast i64 2)
                                (if (= (nl_apply_deferred_apply_lambda name_ptr) 1)
                                    (sys:cast i64 2)
                                  (if (= (nl_apply_deferred_apply_builtin name_ptr) 1)
                                      (sys:cast i64 2)
                                    ;; Not deferred: env-independent, safe to forward
                                    (sys:cast i64 0)))))))))))))))))

;; ---------------------------------------------------------------------------
;; WRONG-TYPE-ARGUMENT signal helper
;;
;; Stashes signal cons(Symbol("wrong-type-argument"),
;;                      cons(Symbol("symbol"), cons(func_clone, Nil)))
;; into env, then returns 1.
;;
;; Used for:
;;   (a) func is not Cons (wrong-type "function")
;;   (b) func.car is not Symbol (wrong-type "function")
;;   (c) deferred builtin names (not-yet-implemented sentinel)
;;
;; We reuse the same stash pattern from env-leaves-bind.nl.
;; "wrong-type-argument" [19]: 8751669898145395319, 7887324063363589488, 7630437
;; "symbol"              [ 6]: 119225648511347
;; ---------------------------------------------------------------------------

(sys:defun nl_apply_write_wta_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8751669898145395319)
     (sys:poke-u64 (+ buf 8) 7887324063363589488)
     (sys:poke-u64 (+ buf 16) 7630437)
     (nl_alloc_symbol buf 19 sym_slot))))

(sys:defun nl_apply_write_symbol_sym
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe
     (sys:poke-u64 buf 119225648511347)
     (nl_alloc_symbol buf 6 sym_slot))))

;; nl_apply_stash_wta(env, offender_ptr) -> i64
;; Stash wrong-type-argument signal and return 1.
;; offender_ptr: *const Sexp — the offending value (cloned into the signal).
(sys:defun nl_apply_stash_wta
    ((env (ptr eval_ctx)) (offender_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag_slot usize (sys:alloc 32 8))
        (symbol_slot usize (sys:alloc 32 8))
        (clone_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (inner_cdr usize (sys:alloc 32 8))
        (mid_pair usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    (nl_apply_write_wta_sym tag_slot)
    (nl_apply_write_symbol_sym symbol_slot)
    (sys:unsafe (nl_sexp_clone_into clone_slot offender_ptr))
    (nl_apply_write_nil nil_slot)
    (sys:unsafe
     (nelisp_cons_construct clone_slot nil_slot inner_cdr)
     (nelisp_cons_construct symbol_slot inner_cdr mid_pair)
     (nelisp_cons_construct tag_slot mid_pair signal_slot))
    ;; Stash signal: env_set_value("nelisp--last-signal-data", signal_slot)
    ;; We use the same pattern as env-leaves-logic.nl nl_logic_stash_signal.
    ;; Build name sym "nelisp--last-signal-data" [24]:
    ;;   3255381746650998126, 7451613697525637484, 7022344801864147310
    (let ((name_sym_slot usize (sys:alloc 32 8))
          (name_buf usize (sys:alloc 24 1)))
      (sys:unsafe
       (sys:poke-u64 name_buf 3255381746650998126)
       (sys:poke-u64 (+ name_buf 8) 7451613697525637484)
       (sys:poke-u64 (+ name_buf 16) 7022344801864147310)
       (nl_alloc_symbol name_buf 24 name_sym_slot))
      ;; nelisp_env_bind_local needs mirror/frames/unbound + 11-slot scratch.
      ;; Use nelisp_env_set_value pattern (mirror, frames, name, val, scratch, 0).
      ;; Build 11-slot scratch (abbreviated: reuse alloc pattern from env-leaves-bind.nl).
      (let ((mirror_ptr usize (+ (sys:cast usize env)
                                 (sys:offsetof eval_ctx mirror)))
            (frames_ptr usize (+ (sys:cast usize env)
                                 (sys:offsetof eval_ctx frames)))
            (unbound_ptr usize (+ (sys:cast usize env)
                                  (sys:offsetof eval_ctx unbound)))
            (box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
            (sym_slot usize (sys:alloc 32 8))
            (sym_buf usize (sys:alloc 16 1))
            (vec_slot usize (sys:alloc 32 8)))
        ;; "symbol-entry" [12]: 7290602597431212403, 2037544046
        (sys:unsafe
         (sys:poke-u64 sym_buf 7290602597431212403)
         (sys:poke-u64 (+ sym_buf 8) 2037544046)
         (nl_alloc_symbol sym_buf 12 sym_slot)
         (nl_vector_set_slot box_ptr 5 sym_slot))
        (let ((data_ptr usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8)))))
          (sys:unsafe
           (nl_sexp_clone_into (+ data_ptr 224) signal_slot)
           (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr)
           (sys:poke-u64 vec_slot 8)
           (sys:poke-u64 (+ vec_slot 8) box_ptr)
           (sys:poke-u64 (+ vec_slot 16) 0)
           (sys:poke-u64 (+ vec_slot 24) 0))
          ;; nelisp_env_set_value to install the signal
          ;; We use nelisp_env_bind_local with mirror+frames (set_value path):
          ;; Actually use the same set_value extern declared here
          ;; Reuse bind_local — which installs into the current frame — is fine for
          ;; signal stash (the Rust code uses set_value; for our purpose bind_local
          ;; in the top frame achieves the same observable result since signal is
          ;; immediately consumed by the caller).
          (sys:unsafe
           (nelisp_env_bind_local mirror_ptr frames_ptr
                                  name_sym_slot signal_slot vec_slot 0))
          (sys:cast i64 1))))))

;; ---------------------------------------------------------------------------
;; FRAME HELPERS (mirrors env-leaves-frame.nl nl_frame_build_push_scratch)
;; ---------------------------------------------------------------------------

;; nl_apply_build_push_scratch(out_vec) -> out_vec
;; Build 7-slot Vector scratch for nelisp_frame_push.
;; "nelisp-lexframe" [15]: 7795010171040458094, 28549237946349669
;; "fast-hash-table" [15]: 8314040931539181926, 28548142445374824
(sys:defun nl_apply_build_push_scratch
    ((out_vec_sexp usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 7)))
        (sym0_slot usize (sys:alloc 32 8))
        (sym1_slot usize (sys:alloc 32 8))
        (buf0 usize (sys:alloc 16 1))
        (buf1 usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf0 7795010171040458094)
     (sys:poke-u64 (+ buf0 8) 28549237946349669)
     (nl_alloc_symbol buf0 15 sym0_slot)
     (sys:poke-u64 buf1 8314040931539181926)
     (sys:poke-u64 (+ buf1 8) 28548142445374824)
     (nl_alloc_symbol buf1 15 sym1_slot)
     (nl_vector_set_slot box_ptr 0 sym0_slot)
     (nl_vector_set_slot box_ptr 1 sym1_slot)
     (sys:poke-u64 out_vec_sexp 8)
     (sys:poke-u64 (+ out_vec_sexp 8) box_ptr)
     (sys:poke-u64 (+ out_vec_sexp 16) 0)
     (sys:poke-u64 (+ out_vec_sexp 24) 0)
     out_vec_sexp)))

;; nl_apply_frame_push(env) -> i64
(sys:defun nl_apply_frame_push
    ((env (ptr eval_ctx)))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (push_scratch usize (sys:alloc 32 8)))
    (nl_apply_build_push_scratch push_scratch)
    (sys:unsafe (nelisp_frame_push frames_ptr push_scratch))))

;; nl_apply_frame_pop(env) -> i64
(sys:defun nl_apply_frame_pop
    ((env (ptr eval_ctx)))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (scratch usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 scratch 0)
     (sys:poke-u64 (+ scratch 8) 0)
     (sys:poke-u64 (+ scratch 16) 0)
     (sys:poke-u64 (+ scratch 24) 0)
     (nelisp_frame_pop frames_ptr scratch))))

;; ---------------------------------------------------------------------------
;; BIND-LOCAL HELPER (mirrors env-leaves-logic.nl nl_logic_bind_local)
;; ---------------------------------------------------------------------------

;; nl_apply_bind_local(env, name_ptr, val_ptr) -> i64
(sys:defun nl_apply_bind_local
    ((env (ptr eval_ctx)) (name_ptr usize) (val_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((mirror_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx mirror)))
        (frames_ptr usize (+ (sys:cast usize env)
                             (sys:offsetof eval_ctx frames)))
        (unbound_ptr usize (+ (sys:cast usize env)
                              (sys:offsetof eval_ctx unbound)))
        (box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_slot usize (sys:alloc 32 8))
        (sym_buf usize (sys:alloc 16 1))
        (vec_slot usize (sys:alloc 32 8)))
    ;; "symbol-entry" [12]: 7290602597431212403, 2037544046
    (sys:unsafe
     (sys:poke-u64 sym_buf 7290602597431212403)
     (sys:poke-u64 (+ sym_buf 8) 2037544046)
     (nl_alloc_symbol sym_buf 12 sym_slot)
     (nl_vector_set_slot box_ptr 5 sym_slot))
    (let ((data_ptr usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8)))))
      (sys:unsafe
       (nl_sexp_clone_into (+ data_ptr 224) val_ptr)
       (nl_sexp_clone_into (+ data_ptr 256) unbound_ptr)
       (sys:poke-u64 vec_slot 8)
       (sys:poke-u64 (+ vec_slot 8) box_ptr)
       (sys:poke-u64 (+ vec_slot 16) 0)
       (sys:poke-u64 (+ vec_slot 24) 0))
      (sys:unsafe
       (nelisp_env_bind_local mirror_ptr frames_ptr name_ptr val_ptr vec_slot 0)))))

;; ---------------------------------------------------------------------------
;; LIST ACCESS HELPERS (walk a cons list to extract args[n])
;; ---------------------------------------------------------------------------

;; nl_apply_list_nth(list_ptr, n) -> usize
;; Walk n steps; return the car at index n. Returns 0 if list is too short.
(sys:defun nl_apply_list_nth
    ((list_ptr usize) (n i64))
  usize
  (:alloc none :ffi may :unsafe may)
  (if (= n 0)
      ;; Extract car at current position
      (if (= (sys:peek-u64 list_ptr) 7)
          (sys:unsafe (nl_cons_car_ptr list_ptr))
        (sys:cast usize 0))
    ;; Recurse on cdr
    (if (= (sys:peek-u64 list_ptr) 7)
        (nl_apply_list_nth
         (sys:unsafe (nl_cons_cdr_ptr list_ptr))
         (+ n -1))
      (sys:cast usize 0))))

;; nl_apply_list_len(list_ptr) -> i64
;; Count elements in a cons list.
(sys:defun nl_apply_list_len
    ((list_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (if (= (sys:peek-u64 list_ptr) 7)
      (+ 1 (nl_apply_list_len (sys:unsafe (nl_cons_cdr_ptr list_ptr))))
    0))

;; nl_apply_list_last_cdr(list_ptr) -> usize
;; Walk to the last element; return nl_cons_cdr_ptr of it (for apply splice).
;; Used to get the tail list from the last argument of "apply".
(sys:defun nl_apply_list_last_cdr
    ((list_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may)
  (if (= (sys:peek-u64 list_ptr) 7)
      (let ((cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr list_ptr))))
        (if (= (sys:peek-u64 cdr_ptr) 7)
            (nl_apply_list_last_cdr cdr_ptr)
          ;; cdr is not Cons (Nil or end): current is the last element
          (sys:unsafe (nl_cons_car_ptr list_ptr))))
    ;; Empty list: return list_ptr (the Nil)
    list_ptr))

;; nl_apply_list_init(list_ptr, out_slot) -> i64
;; Build a new cons list containing all elements EXCEPT the last.
;; (list[0..n-1] for apply's prefix args)
;; Base case: if cdr is Nil or end → return Nil (write Nil to out_slot).
;; Step: car = current car; recurse on cdr into rest_slot;
;;       cons(car, rest) → out_slot.
(sys:defun nl_apply_list_init
    ((list_ptr usize) (out_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:peek-u64 list_ptr) 7)
      (let ((cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr list_ptr))))
        (if (= (sys:peek-u64 cdr_ptr) 7)
            ;; cdr is Cons: more elements remain; include current car
            (let ((car_ptr usize (sys:unsafe (nl_cons_car_ptr list_ptr)))
                  (rest_slot usize (sys:alloc 32 8)))
              (let ((rc i64 (nl_apply_list_init cdr_ptr rest_slot)))
                (if (= rc 0)
                    (sys:unsafe (nelisp_cons_construct car_ptr rest_slot out_slot))
                  (sys:cast i64 1))))
          ;; cdr is Nil: current element IS the last; exclude it → write Nil
          (nl_apply_write_nil out_slot)))
    ;; Empty list: Nil
    (nl_apply_write_nil out_slot)))

;; nl_apply_list_append(head_ptr, tail_ptr, out_slot) -> i64
;; Append two lists: result = head ++ tail (where head is a proper list).
;; Base: if head_ptr is Nil, clone tail into out_slot.
;; Step: cons(head.car, append(head.cdr, tail)) → out_slot.
(sys:defun nl_apply_list_append
    ((head_ptr usize) (tail_ptr usize) (out_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:peek-u64 head_ptr) 7)
      (let ((car_ptr usize (sys:unsafe (nl_cons_car_ptr head_ptr)))
            (cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr head_ptr)))
            (rest_slot usize (sys:alloc 32 8)))
        (let ((rc i64 (nl_apply_list_append cdr_ptr tail_ptr rest_slot)))
          (if (= rc 0)
              (sys:unsafe (nelisp_cons_construct car_ptr rest_slot out_slot))
            (sys:cast i64 1))))
    ;; head exhausted: result = tail (share, not clone — tail is live)
    (sys:unsafe (nl_sexp_clone_into out_slot tail_ptr))))

;; ---------------------------------------------------------------------------
;; ENV-USING DISPATCH ARMS
;; ---------------------------------------------------------------------------

;; nl_apply_do_eval(args_list_ptr, env, out) -> i64
;; Rust: eval(&args[0], env) — eval the first arg form.
;; args[0] = nl_apply_list_nth(args_list_ptr, 0)
(sys:defun nl_apply_do_eval
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((arg0_ptr usize (nl_apply_list_nth args_list_ptr 0)))
    (if (= arg0_ptr 0)
        ;; No first arg: wrong-arity sentinel (stash and return 1)
        (nl_apply_stash_wta env args_list_ptr)
      ;; eval the first arg
      (sys:unsafe (nelisp_eval_call arg0_ptr (sys:cast usize env) out)))))

;; nl_apply_do_funcall(args_list_ptr, env, out) -> i64
;; Rust: let func = if Symbol(s) → lookup_function(s) else args[0].clone();
;;       apply_function(&func, &args[1..], env)
;;
;; args[0] = func-form (already evaluated by caller)
;; args[1..] = remaining args
;; If args[0] is Symbol: lookup_function via mirror
;; Else: use args[0] directly.
;; Then: nl_apply_function(func_slot, args1_plus, env, out)
(sys:defun nl_apply_do_funcall
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((arg0_ptr usize (nl_apply_list_nth args_list_ptr 0)))
    (if (= arg0_ptr 0)
        (nl_apply_stash_wta env args_list_ptr)
      (let ((arg0_tag usize (sys:cast usize (sys:peek-u64 arg0_ptr))))
        ;; args[1..] = cdr of args_list
        (let ((args1_plus usize
               (if (= (sys:peek-u64 args_list_ptr) 7)
                   (sys:unsafe (nl_cons_cdr_ptr args_list_ptr))
                 args_list_ptr)))
          (if (= arg0_tag 4)
              ;; Symbol: lookup function
              (let ((func_slot usize (sys:alloc 32 8))
                    (mirror_ptr usize (+ (sys:cast usize env)
                                        (sys:offsetof eval_ctx mirror)))
                    (unbound_ptr usize (+ (sys:cast usize env)
                                         (sys:offsetof eval_ctx unbound))))
                (let ((rc_lu i64 (sys:unsafe
                                   (nelisp_env_lookup_function
                                    mirror_ptr unbound_ptr arg0_ptr func_slot))))
                  (if (= rc_lu 0)
                      ;; lookup ok: recurse
                      (nl_apply_function func_slot args1_plus env out)
                    ;; lookup error: propagate
                    (sys:cast i64 1))))
            ;; Not Symbol: use arg0 directly
            (nl_apply_function arg0_ptr args1_plus env out)))))))

;; nl_apply_do_apply(args_list_ptr, env, out) -> i64
;; Rust: let func = if Symbol(s) → lookup else args[0];
;;       let aa = args[1..n-1] ++ list_elements(args[n-1]);
;;       apply_function(&func, &aa, env)
;;
;; Splice: prefix = args[1..n-1] (built by list_init(cdr(args_list))),
;;         tail   = args[n-1] (last arg, which is itself a list).
;;         result_args = append(prefix, tail).
(sys:defun nl_apply_do_apply
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((arg0_ptr usize (nl_apply_list_nth args_list_ptr 0)))
    (if (= arg0_ptr 0)
        (nl_apply_stash_wta env args_list_ptr)
      (let ((arg0_tag usize (sys:cast usize (sys:peek-u64 arg0_ptr)))
            (rest_args usize
             (if (= (sys:peek-u64 args_list_ptr) 7)
                 (sys:unsafe (nl_cons_cdr_ptr args_list_ptr))
               args_list_ptr)))
        ;; Resolve function
        (let ((func_slot usize (sys:alloc 32 8)))
          (let ((resolve_rc i64
                 (if (= arg0_tag 4)
                     (let ((mirror_ptr usize (+ (sys:cast usize env)
                                                (sys:offsetof eval_ctx mirror)))
                           (unbound_ptr usize (+ (sys:cast usize env)
                                                 (sys:offsetof eval_ctx unbound))))
                       (sys:unsafe
                        (nelisp_env_lookup_function mirror_ptr unbound_ptr arg0_ptr func_slot)))
                   ;; Not Symbol: clone arg0 into func_slot
                   (sys:unsafe (nl_sexp_clone_into func_slot arg0_ptr)))))
            (if (= resolve_rc 0)
                ;; Build spliced args: prefix ++ last_arg_as_list
                ;; prefix = rest_args[0..n-2] (all but last)
                ;; last_arg = last element of rest_args (is itself a list)
                (let ((prefix_slot usize (sys:alloc 32 8))
                      (last_ptr usize (nl_apply_list_last_cdr rest_args)))
                  (let ((rc_init i64 (nl_apply_list_init rest_args prefix_slot)))
                    (if (= rc_init 0)
                        (let ((spliced_slot usize (sys:alloc 32 8)))
                          (let ((rc_app i64 (nl_apply_list_append prefix_slot last_ptr spliced_slot)))
                            (if (= rc_app 0)
                                (nl_apply_function func_slot spliced_slot env out)
                              (sys:cast i64 1))))
                      (sys:cast i64 1))))
              (sys:cast i64 1))))))))

;; nl_apply_do_push_frame(env, out) -> i64
;; Rust: env.frame_push_rust_direct(); Ok(Sexp::T)
(sys:defun nl_apply_do_push_frame
    ((env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((rc i64 (nl_apply_frame_push env)))
    (if (= rc 0)
        ;; Write Sexp::T (tag=1) into out
        (sys:unsafe
         (sys:poke-u64 out 1)
         (sys:poke-u64 (+ out 8) 0)
         (sys:poke-u64 (+ out 16) 0)
         (sys:poke-u64 (+ out 24) 0)
         (sys:cast i64 0))
      rc)))

;; nl_apply_do_pop_frame(env, out) -> i64
;; Rust: env.frame_pop_rust_direct(); Ok(Sexp::T)
(sys:defun nl_apply_do_pop_frame
    ((env (ptr eval_ctx)) (out usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((rc i64 (nl_apply_frame_pop env)))
    (if (= rc 0)
        (sys:unsafe
         (sys:poke-u64 out 1)
         (sys:poke-u64 (+ out 8) 0)
         (sys:poke-u64 (+ out 16) 0)
         (sys:poke-u64 (+ out 24) 0)
         (sys:cast i64 0))
      rc)))

;; nl_apply_do_bind_local(args_list_ptr, env, out) -> i64
;; Rust: name=Symbol(args[0]); env.bind_local(&name, args[1].clone()); Ok(args[1].clone())
(sys:defun nl_apply_do_bind_local
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((name_ptr usize (nl_apply_list_nth args_list_ptr 0))
        (val_ptr usize (nl_apply_list_nth args_list_ptr 1)))
    (if (= name_ptr 0)
        (nl_apply_stash_wta env args_list_ptr)
      (if (= val_ptr 0)
          (nl_apply_stash_wta env args_list_ptr)
        (let ((rc i64 (nl_apply_bind_local env name_ptr val_ptr)))
          (if (= rc 0)
              (sys:unsafe (nl_sexp_clone_into out val_ptr))
            rc))))))

;; nl_apply_do_push_captured(args_list_ptr, env, out) -> i64
;; Rust: env.push_captured(&args[0])?; Ok(Sexp::T)
;; args[0] is the alist to install. Calls nl_env_push_captured (staged).
(sys:defun nl_apply_do_push_captured
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((alist_ptr usize (nl_apply_list_nth args_list_ptr 0)))
    (if (= alist_ptr 0)
        (nl_apply_stash_wta env args_list_ptr)
      (let ((rc i64 (sys:unsafe (nl_env_push_captured (sys:cast usize env) alist_ptr))))
        (if (= rc 0)
            (sys:unsafe
             (sys:poke-u64 out 1)
             (sys:poke-u64 (+ out 8) 0)
             (sys:poke-u64 (+ out 16) 0)
             (sys:poke-u64 (+ out 24) 0)
             (sys:cast i64 0))
          rc)))))

;; nl_apply_do_set_use_elisp(args_list_ptr, env, out) -> i64
;; Rust: env.use_elisp_apply = !matches!(args[0], Sexp::Nil); Ok(bool_sexp(...))
;; ctx.flags bit0 = use_elisp_apply (true if args[0] != Nil).
;; Writes T or Nil into out based on the new flag value.
(sys:defun nl_apply_do_set_use_elisp
    ((args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (let ((arg0_ptr usize (nl_apply_list_nth args_list_ptr 0)))
    (let ((is_nil i64 (if (= arg0_ptr 0) 1
                        (if (= (sys:cast usize (sys:peek-u64 arg0_ptr)) 0) 1 0))))
      ;; use_elisp_apply = (args[0] != Nil)
      (let ((new_flag i64 (if (= is_nil 1) 0 1)))
        (let ((flags_ptr usize (+ (sys:cast usize env)
                                  (sys:offsetof eval_ctx flags))))
          (let ((old_flags i64 (sys:unsafe (sys:peek-u64 flags_ptr))))
            ;; set bit0 of flags to new_flag (arithmetic, no sys:bit ops needed):
            ;;   bit-or(x, 1)     = x + (if bit0(x)==0 then 1 else 0) = x + (1 - mod(x,2))
            ;;   bit-and(x, ~1)   = x - mod(x,2)   (clear bit0)
            (let ((new_flags i64 (if (= new_flag 1)
                                     (+ old_flags (+ 1 (* -1 (mod old_flags 2))))
                                     (- old_flags (mod old_flags 2)))))
              (sys:unsafe (sys:poke-u64 flags_ptr new_flags))
              ;; Write bool result: T if new_flag==1, Nil if 0
              (if (= new_flag 1)
                  (sys:unsafe
                   (sys:poke-u64 out 1)
                   (sys:poke-u64 (+ out 8) 0)
                   (sys:poke-u64 (+ out 16) 0)
                   (sys:poke-u64 (+ out 24) 0)
                   (sys:cast i64 0))
                (sys:unsafe
                 (sys:poke-u64 out 0)
                 (sys:poke-u64 (+ out 8) 0)
                 (sys:poke-u64 (+ out 16) 0)
                 (sys:poke-u64 (+ out 24) 0)
                 (sys:cast i64 0))))))))))

;; ---------------------------------------------------------------------------
;; BUILTIN DISPATCH
;;
;; nl_apply_builtin(func_ptr, args_list_ptr, env, out) -> i64
;; Called when func.car == Symbol("builtin").
;; name = func.cdr.car (Symbol or Str).
;; Classify name: env-using handled → handle directly;
;;               deferred env-using → error (stash wta, return 1);
;;               env-independent → forward to nelisp_apply_function.
;; ---------------------------------------------------------------------------

(sys:defun nl_apply_builtin
    ((func_ptr usize) (args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; func = (builtin name) → func.cdr must be Cons, func.cdr.car is the name
  (let ((func_cdr usize (sys:unsafe (nl_cons_cdr_ptr func_ptr))))
    (if (= (sys:peek-u64 func_cdr) 7)
        (let ((name_ptr usize (sys:unsafe (nl_cons_car_ptr func_cdr))))
          ;; name_ptr must be Symbol(4) or Str(5)
          (let ((name_tag usize (sys:cast usize (sys:peek-u64 name_ptr))))
            (if (if (= name_tag 4) 1 (if (= name_tag 5) 1 0))
                ;; tag is 4 or 5 — ok (Symbol or Str)
                (let ((cls i64 (nl_apply_name_classify name_ptr)))
                  (if (= cls 1)
                      ;; env-using, handled: dispatch
                      (if (= (nl_apply_name_eq_eval name_ptr) 1)
                          (nl_apply_do_eval args_list_ptr env out)
                        (if (= (nl_apply_name_eq_funcall name_ptr) 1)
                            (nl_apply_do_funcall args_list_ptr env out)
                          (if (= (nl_apply_name_eq_apply name_ptr) 1)
                              (nl_apply_do_apply args_list_ptr env out)
                            (if (= (nl_apply_name_eq_push_frame name_ptr) 1)
                                (nl_apply_do_push_frame env out)
                              (if (= (nl_apply_name_eq_pop_frame name_ptr) 1)
                                  (nl_apply_do_pop_frame env out)
                                (if (= (nl_apply_name_eq_bind_local name_ptr) 1)
                                    (nl_apply_do_bind_local args_list_ptr env out)
                                  (if (= (nl_apply_name_eq_push_captured name_ptr) 1)
                                      (nl_apply_do_push_captured args_list_ptr env out)
                                    ;; must be nelisp--set-use-elisp-apply
                                    (nl_apply_do_set_use_elisp args_list_ptr env out))))))))
                    (if (= cls 2)
                        ;; Deferred: stash not-yet-implemented error and return 1
                        (nl_apply_stash_wta env name_ptr)
                      ;; cls==0: env-independent — forward to nelisp_apply_function
                      ;; Safe: env-independent builtins do not deref env as Env* deeply,
                      ;; they accept env as opaque *mut c_void. Passing eval_ctx* is safe
                      ;; because these arms never dereference it as &mut Env.
                      (sys:unsafe
                       (nelisp_apply_function func_ptr args_list_ptr
                                              (sys:cast usize env) out)))))
              ;; name_tag is not Symbol/Str: wrong-type
              (nl_apply_stash_wta env name_ptr))))
      ;; func.cdr is not Cons: malformed builtin sentinel
      (nl_apply_stash_wta env func_ptr))))

;; ---------------------------------------------------------------------------
;; CLOSURE/LAMBDA DISPATCH
;;
;; nl_apply_closure_or_lambda(func_ptr, head_is_closure, args_list_ptr, env, out) -> i64
;; head_is_closure: 1 = closure, 0 = lambda
;;
;; Rust:
;;   parts = list_elements(func)
;;   (captured, fi, bs) = if head=="closure" { (parts[1].clone(), 2, 3) }
;;                        else               { (Sexp::Nil, 1, 2) }
;;   rc = apply_lambda_inner_call(&captured, &parts[fi], &list_from(&parts[bs..]),
;;                                &list_from(args), env, &mut out)
;;
;; We walk the func list to extract parts:
;;   closure: parts[1]=captured, parts[2]=formals, parts[3..]=body
;;   lambda:  parts[1]=formals, parts[2..]=body
;;
;; body_list_ptr = list starting at parts[bs] (already a proper sublist of func).
;; We DON'T need to re-list_from the body — func IS the list so parts[bs..] is just
;; the cdr at position bs.
;; ---------------------------------------------------------------------------

;; nl_apply_list_drop_n(list_ptr, n) -> usize
;; Returns the sublist starting at index n (i.e. cdr^n).
(sys:defun nl_apply_list_drop_n
    ((list_ptr usize) (n i64))
  usize
  (:alloc none :ffi may :unsafe may)
  (if (= n 0)
      list_ptr
    (if (= (sys:peek-u64 list_ptr) 7)
        (nl_apply_list_drop_n
         (sys:unsafe (nl_cons_cdr_ptr list_ptr))
         (+ n -1))
      list_ptr)))

(sys:defun nl_apply_closure_or_lambda
    ((func_ptr usize) (head_is_closure i64) (args_list_ptr usize)
     (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= head_is_closure 1)
      ;; closure: (closure CAPTURED FORMALS BODY...)
      ;;   func.cdr = (CAPTURED FORMALS BODY...)
      ;;   captured  = func.cdr.car
      ;;   formals   = func.cdr.cdr.car
      ;;   body_list = func.cdr.cdr.cdr
      (let ((cdr1 usize (sys:unsafe (nl_cons_cdr_ptr func_ptr))))
        (if (= (sys:peek-u64 cdr1) 7)
            (let ((captured_ptr usize (sys:unsafe (nl_cons_car_ptr cdr1)))
                  (cdr2 usize (sys:unsafe (nl_cons_cdr_ptr cdr1))))
              (if (= (sys:peek-u64 cdr2) 7)
                  (let ((formals_ptr usize (sys:unsafe (nl_cons_car_ptr cdr2)))
                        (body_list_ptr usize (sys:unsafe (nl_cons_cdr_ptr cdr2))))
                    (sys:unsafe
                     (nl_apply_lambda_inner captured_ptr formals_ptr body_list_ptr
                                            args_list_ptr (sys:cast usize env) out)))
                ;; malformed: missing formals
                (nl_apply_stash_wta env func_ptr)))
          ;; malformed: missing captured
          (nl_apply_stash_wta env func_ptr)))
    ;; lambda: (lambda FORMALS BODY...)
    ;;   func.cdr = (FORMALS BODY...)
    ;;   formals   = func.cdr.car
    ;;   body_list = func.cdr.cdr
    (let ((cdr1 usize (sys:unsafe (nl_cons_cdr_ptr func_ptr))))
      (if (= (sys:peek-u64 cdr1) 7)
          (let ((formals_ptr usize (sys:unsafe (nl_cons_car_ptr cdr1)))
                (body_list_ptr usize (sys:unsafe (nl_cons_cdr_ptr cdr1)))
                ;; captured = Nil for bare lambda
                (nil_slot usize (sys:alloc 32 8)))
            (nl_apply_write_nil nil_slot)
            (sys:unsafe
             (nl_apply_lambda_inner nil_slot formals_ptr body_list_ptr
                                    args_list_ptr (sys:cast usize env) out)))
        ;; malformed lambda
        (nl_apply_stash_wta env func_ptr)))))

;; ---------------------------------------------------------------------------
;; PUBLIC ENTRY POINT: nl_apply_function
;;
;; nl_apply_function(func_ptr, args_list_ptr, env, out) -> i64
;;   func_ptr:      usize — *const Sexp (the function Sexp, e.g. (closure ...) or (builtin name))
;;   args_list_ptr: usize — *const Sexp (already-evaluated argument cons list)
;;   env:           (ptr eval_ctx) — EvalCtx pointer
;;   out:           usize — *mut Sexp, receives the result on success
;;   Returns: 0 on success, 1 on error (signal stashed in env)
;;
;; Routing:
;;   func must be Cons(head, ...) where head is Symbol.
;;   head=="closure"  → nl_apply_closure_or_lambda(..., 1, ...)
;;   head=="lambda"   → nl_apply_closure_or_lambda(..., 0, ...)
;;   head=="builtin"  → nl_apply_builtin(...)
;;   head=="macro"    → stash wta, return 1
;;   else             → stash wta, return 1
;; ---------------------------------------------------------------------------
(sys:defun nl_apply_function
    ((func_ptr usize) (args_list_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; func must be Cons (tag==7)
  (if (= (sys:peek-u64 func_ptr) 7)
      ;; car of func must be Symbol (tag==4)
      (let ((head_ptr usize (sys:unsafe (nl_cons_car_ptr func_ptr))))
        (if (= (sys:cast usize (sys:peek-u64 head_ptr)) 4)
            ;; Check which head we have
            (if (= (nl_apply_name_eq_closure head_ptr) 1)
                (nl_apply_closure_or_lambda func_ptr 1 args_list_ptr env out)
              (if (= (nl_apply_name_eq_lambda head_ptr) 1)
                  (nl_apply_closure_or_lambda func_ptr 0 args_list_ptr env out)
                ;; Check for "builtin" [7]: 31078196194145634
                (let ((builtin_buf usize (sys:alloc 8 1)))
                  (sys:unsafe (sys:poke-u64 builtin_buf 31078196194145634))
                  (if (= (nl_apply_sym_eq_bytes head_ptr builtin_buf 7) 1)
                      (nl_apply_builtin func_ptr args_list_ptr env out)
                    ;; "macro" or unknown head: wrong-type
                    (nl_apply_stash_wta env func_ptr)))))
          ;; head is not Symbol: wrong-type
          (nl_apply_stash_wta env func_ptr)))
    ;; func is not Cons: wrong-type
    (nl_apply_stash_wta env func_ptr)))
