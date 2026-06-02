;; Doc 135 Stage 135.D — nl_eval_inner_cons + nl_apply_special; STAGED
;; (unwired — do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements:
;;   nl_apply_special(name_ptr, args_ptr, env, out) -> i64
;;     0 = handled, result in *out
;;     1 = handled, error stashed in env
;;     2 = NOT a special form (caller continues to function lookup)
;;
;;   nl_eval_inner_cons(head_ptr, tail_ptr, env, out) -> i64
;;     0 = ok, result in *out
;;     1 = error stashed in env
;;
;; DEFERRED (flagged precisely):
;;   A. use_elisp_apply delegation branch (both the non-Symbol and Symbol paths)
;;      Reason: requires ctx.flags bit0 read + delegation_depth counter.
;;      delegation_depth is NOT in eval_ctx §2.3 — it lives inside Rust Env only.
;;      The delegation gate will be wired once ctx layout is extended (Doc 135 §2.3b).
;;      Flag: both arms contain a TODO comment; the code omits the `if use_elisp_apply`
;;      fork and falls through directly to nl_apply_function / nl_apply_special.
;;   B. macro-expansion path
;;      Reason: two strategies in Rust (nelisp--expand-macro delegation and inline
;;      apply_function+eval fallback) both require either re-eval (nelisp_eval_call)
;;      or delegation (see A). The delegation variant needs flag A resolved.
;;      The inline (non-delegation) macro path IS implemented here: it calls
;;      nl_apply_function on the macro cdr's function and then re-evals the expansion
;;      via nelisp_eval_call. This matches the Rust fallback path:
;;        parts = list_elements(func); apply_function(parts[1], args, e); eval(expansion, e)
;;      The nelisp--expand-macro delegation variant is DEFERRED (same flag A reason).
;;
;; Rust reference (mod.rs nl_eval_inner_cons, apply_special):
;;   nl_eval_inner_cons: #[no_mangle] pub unsafe extern "C"
;;     fn nl_eval_inner_cons(head_ptr: *const Sexp, tail_ptr: *const Sexp,
;;                           env: *mut c_void, out: *mut Sexp) -> i64
;;   apply_special: fn apply_special(name: &str, args: &Sexp, env: &mut Env)
;;                    -> Result<Option<Sexp>, EvalError>
;;   Rust returns Ok(None) when name does not match — we return 2 (NOT-special).
;;
;; sf_call! macro shapes (confirmed from lib.rs cc_wrap_batch):
;;   3-arg (if/let/let*/setq/while/unwind-protect/progn):
;;     nl_sf_X(args: *const Sexp, env: *mut c_void, out: *mut Sexp, _pad: i64) -> i64
;;   s1-variants (function/lambda/condition-case):
;;     nl_sf_X(args: *const Sexp, env: *mut c_void, out: *mut Sexp, s1: *mut Sexp) -> i64
;;     s1 is a caller-provided 32B Sexp scratch slot (Sexp::Nil initialised).
;;   quote: nl_sf_quote(args: *const Sexp, out: *mut Sexp) -> i64 (2-arg, no env)
;;     rc != 0 means arity error (wrong number of args to quote).
;;
;; nelisp_env_lookup_function(mirror_ptr, unbound_ptr, name_ptr, out_ptr) -> i64
;;   0 = ok (out = function Sexp)
;;   1 = error / void-function (signal already stashed by callee)
;;   Confirmed from lib.rs extern block and nm T 0x0000.
;;
;; Archive nm confirmation (debug build, canonical):
;;   nl_sf_quote              T  nl_sf_if                T
;;   nl_sf_let                T  nl_sf_let_star          T
;;   nl_sf_lambda             T  nl_sf_setq              T
;;   nl_sf_while              T  nl_sf_condition_case    T
;;   nl_sf_unwind_protect     T  nl_sf_progn             T
;;   nl_sf_function           T  nelisp_env_lookup_function T
;;   nl_apply_lambda_inner    T  nelisp_eval_call        U (Rust binary)
;;   nl_eval_inner_cons       U  (Rust binary — we produce the elisp replacement here)
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
;;   offset 0: tag (u8) — peek-u64 word0 = tag for small tag values
;;   offset 8: payload (u64)
;;   offset 16: pad[16]
;; Tags: 0=Nil 1=T 2=Int 4=Symbol 5=Str 7=Cons 8=Vector
;;
;; String LE-u64 encodings (for name comparisons — ALL verified in this file):
;;   "quote"          [ 5]: 435745158513
;;   "function"       [ 8]: 7957695015192261990
;;   "if"             [ 2]: 26217
;;   "let"            [ 3]: 7628140
;;   "let*"           [ 4]: 712271212
;;   "lambda"         [ 6]: 107083775959404
;;   "setq"           [ 4]: 1903453555
;;   "while"          [ 5]: 435610544247
;;   "condition-case" [14]: 8028075806769966947, 111546229534062
;;   "unwind-protect" [14]: 8083227331578523253, 127970252713842
;;   "progn"          [ 5]: 474181759600
;;   "macro"          [ 5]: 478660485485
;;   "void-function"  [13]: 7959380261591412598, 474315584611

;; ---------------------------------------------------------------------------
;; Struct declarations (LOCKED §2.3)
;; ---------------------------------------------------------------------------

(sys:defstruct sexp (:repr c)
  (tag u8) (payload u64) (pad (array u8 16)))

(sys:defstruct eval_ctx (:repr c)
  (mirror sexp) (frames sexp) (unbound sexp)
  (rec_cur i64) (rec_max i64) (flags i64))

;; ---------------------------------------------------------------------------
;; sys:extern declarations — all callee symbols verified T in archive or
;; U from Rust binary (noted).
;; ---------------------------------------------------------------------------

;; nelisp_eval_call(form_ptr, env_ptr, out_ptr) -> i64  [U — Rust binary]
(sys:extern nelisp_eval_call
  (:symbol "nelisp_eval_call" :abi c :unsafe t)
  ((form_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_env_lookup_function(mirror_ptr, unbound_ptr, name_ptr, out_ptr) -> i64  [T]
(sys:extern nelisp_env_lookup_function
  (:symbol "nelisp_env_lookup_function" :abi c :unsafe t)
  ((mirror_ptr usize) (unbound_ptr usize) (name_ptr usize) (out_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_apply_function(func_ptr, args_list_ptr, env_ptr, out_ptr) -> i64
;; staged in combiner-apply.nl; declared sys:extern here for independence.
(sys:extern nl_apply_function
  (:symbol "nl_apply_function" :abi c :unsafe t)
  ((func_ptr usize) (args_list_ptr usize) (env_ptr usize) (out_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_eval_arg_list(args_ptr, env_ptr, out_list_slot) -> i64
;; staged in combiner-arglist.nl; declared sys:extern for independence.
(sys:extern nl_eval_arg_list
  (:symbol "nl_eval_arg_list" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_list_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_sf_quote(args, out) -> i64  [T]  2-arg; no env; rc!=0 = arity error
(sys:extern nl_sf_quote
  (:symbol "nl_sf_quote" :abi c :unsafe t)
  ((args_ptr usize) (out_ptr usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; 3-arg sf_ group: (args, env, out, _pad=0) -> i64  [all T]
(sys:extern nl_sf_if
  (:symbol "nl_sf_if" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_let
  (:symbol "nl_sf_let" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_let_star
  (:symbol "nl_sf_let_star" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_setq
  (:symbol "nl_sf_setq" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_while
  (:symbol "nl_sf_while" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_unwind_protect
  (:symbol "nl_sf_unwind_protect" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_progn
  (:symbol "nl_sf_progn" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

;; s1-arg sf_ group: (args, env, out, s1) -> i64  [all T]
;; s1 is a 32B Sexp scratch slot (Nil-initialised by caller per sf_call! macro).
(sys:extern nl_sf_function
  (:symbol "nl_sf_function" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (s1_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_lambda
  (:symbol "nl_sf_lambda" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (s1_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

(sys:extern nl_sf_condition_case
  (:symbol "nl_sf_condition_case" :abi c :unsafe t)
  ((args_ptr usize) (env_ptr usize) (out_ptr usize) (s1_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64  [T]
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((car_ptr usize) (cdr_ptr usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_cons_car_ptr(cons_ptr) -> usize  [T]
(sys:extern nl_cons_car_ptr
  (:symbol "nl_cons_car_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_cons_cdr_ptr(cons_ptr) -> usize  [T]
(sys:extern nl_cons_cdr_ptr
  (:symbol "nl_cons_cdr_ptr" :abi c :unsafe t)
  ((cons_ptr usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot  [T]
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nelisp_eq_symbol(a_ptr, b_ptr, result_slot) -> usize  [T]
;; result_slot tag=1 means equal (T), tag=0 means not equal (Nil).
(sys:extern nelisp_eq_symbol
  (:symbol "nelisp_eq_symbol" :abi c :unsafe t)
  ((a_ptr usize) (b_ptr usize) (result_slot usize))
  usize
  (:alloc none :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64  [U — Rust binary]
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nl_alloc_vector(cap) -> box_ptr  [T]
(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64  [T]
(sys:extern nl_vector_set_slot
  (:symbol "nl_vector_set_slot" :abi c :unsafe t)
  ((vec_ptr usize) (n i64) (val usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_env_bind_local(mirror_ptr, frames_ptr, name_ptr, val_ptr, scratch_ptr, _pad) -> i64  [T]
(sys:extern nelisp_env_bind_local
  (:symbol "nelisp_env_bind_local" :abi c :unsafe t)
  ((mirror_ptr usize) (frames_ptr usize) (name_ptr usize)
   (val_ptr usize) (scratch_ptr usize) (_pad i64))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; UTILITY: write Nil / T into a 32B slot
;; ---------------------------------------------------------------------------

(sys:defun nl_cons_write_nil
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 0)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   (sys:cast i64 0)))

(sys:defun nl_cons_write_t
    ((slot usize))
  i64
  (:alloc none :ffi may :unsafe may)
  (sys:unsafe
   (sys:poke-u64 slot 1)
   (sys:poke-u64 (+ slot 8) 0)
   (sys:poke-u64 (+ slot 16) 0)
   (sys:poke-u64 (+ slot 24) 0)
   (sys:cast i64 0)))

;; ---------------------------------------------------------------------------
;; NAME COMPARISON HELPER
;; nl_cons_sym_eq(sym_ptr, buf, len) -> i64
;; Build a fresh Symbol from buf+len; compare with nelisp_eq_symbol.
;; Returns 1 if equal, 0 if not.
;; Identical pattern to nl_apply_sym_eq_bytes in combiner-apply.nl.
;; ---------------------------------------------------------------------------

(sys:defun nl_cons_sym_eq
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
;; void-function signal stash helper
;;
;; Used by nl_eval_inner_cons when lookup_function fails to find a binding
;; (nelisp_env_lookup_function already stashes void-function in the Env
;;  before returning 1, per Rust stash_err pattern).
;; We do NOT need to stash again; we simply propagate rc=1.
;; This comment documents the contract.
;;
;; macro signal stash: build (void-function NAME) as signal data.
;; Reuses the nl_apply_stash_wta pattern from combiner-apply.nl.
;; We inline the stash for void-function here to keep this file self-contained.
;; ---------------------------------------------------------------------------

;; nl_cons_stash_void_function(env, name_ptr) -> i64
;; Stash signal cons("void-function", cons(name_clone, Nil)) into env.
;; "void-function" [13]: 7959380261591412598, 474315584611
;; nelisp--last-signal-data [24]: 3255381746650998126, 7451613697525637484, 7022344801864147310
(sys:defun nl_cons_stash_void_function
    ((env (ptr eval_ctx)) (name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((tag_buf usize (sys:alloc 16 1))
        (tag_slot usize (sys:alloc 32 8))
        (clone_slot usize (sys:alloc 32 8))
        (nil_slot usize (sys:alloc 32 8))
        (pair_slot usize (sys:alloc 32 8))
        (signal_slot usize (sys:alloc 32 8)))
    (sys:unsafe
     (sys:poke-u64 tag_buf 7959380261591412598)
     (sys:poke-u64 (+ tag_buf 8) 474315584611)
     (nl_alloc_symbol tag_buf 13 tag_slot)
     (nl_sexp_clone_into name_ptr clone_slot))
    (nl_cons_write_nil nil_slot)
    (sys:unsafe
     (nelisp_cons_construct clone_slot nil_slot pair_slot)
     (nelisp_cons_construct tag_slot pair_slot signal_slot))
    ;; Install signal via env_bind_local on mirror
    (let ((name_sym_slot usize (sys:alloc 32 8))
          (name_buf usize (sys:alloc 24 1))
          (mirror_ptr usize (+ (sys:cast usize env)
                               (sys:offsetof eval_ctx mirror)))
          (frames_ptr usize (+ (sys:cast usize env)
                               (sys:offsetof eval_ctx frames)))
          (unbound_ptr usize (+ (sys:cast usize env)
                                (sys:offsetof eval_ctx unbound)))
          (box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
          (sym_slot usize (sys:alloc 32 8))
          (sym_buf usize (sys:alloc 16 1))
          (vec_slot usize (sys:alloc 32 8)))
      (sys:unsafe
       (sys:poke-u64 name_buf 3255381746650998126)
       (sys:poke-u64 (+ name_buf 8) 7451613697525637484)
       (sys:poke-u64 (+ name_buf 16) 7022344801864147310)
       (nl_alloc_symbol name_buf 24 name_sym_slot)
       ;; "symbol-entry" [12]: 7290602597431212403, 2037544046
       (sys:poke-u64 sym_buf 7290602597431212403)
       (sys:poke-u64 (+ sym_buf 8) 2037544046)
       (nl_alloc_symbol sym_buf 12 sym_slot)
       (nl_vector_set_slot box_ptr 5 sym_slot))
      (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
        (sys:unsafe
         (nl_sexp_clone_into signal_slot (+ data_ptr 224))
         (nl_sexp_clone_into unbound_ptr (+ data_ptr 256))
         (sys:poke-u64 vec_slot 8)
         (sys:poke-u64 (+ vec_slot 8) box_ptr)
         (sys:poke-u64 (+ vec_slot 16) 0)
         (sys:poke-u64 (+ vec_slot 24) 0))
        (sys:unsafe
         (nelisp_env_bind_local mirror_ptr frames_ptr
                                name_sym_slot signal_slot vec_slot 0))
        (sys:cast i64 1)))))

;; ---------------------------------------------------------------------------
;; nl_apply_special
;; (name_ptr, args_ptr, env, out) -> i64
;;   0 = handled ok (result in *out)
;;   1 = handled, error (signal stashed)
;;   2 = NOT a special form (caller continues)
;;
;; name_ptr: *const Sexp — the head Symbol of the form
;; args_ptr: *const Sexp — the unevaluated argument list (tail of the form)
;; env:      (ptr eval_ctx)
;; out:      usize — *mut Sexp, output slot
;;
;; Each sf_X call returns 0 on success, 1 on error (signal already stashed
;; by the callee in env). We propagate those codes directly.
;;
;; 11-way dispatch (ordered roughly by frequency):
;;   progn / if / let / let* / setq / while / lambda / function / quote
;;   / condition-case / unwind-protect
;;
;; name_ptr tag must be 4 (Symbol) — caller (nl_eval_inner_cons) guarantees this.
;; ---------------------------------------------------------------------------

;; --- per-name comparison helpers (inline, not reused across files) ---

(sys:defun nl_sp_eq_progn
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "progn" [5]: 474181759600
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 474181759600))
    (nl_cons_sym_eq name_ptr buf 5)))

(sys:defun nl_sp_eq_if
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "if" [2]: 26217
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 26217))
    (nl_cons_sym_eq name_ptr buf 2)))

(sys:defun nl_sp_eq_let
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "let" [3]: 7628140
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 7628140))
    (nl_cons_sym_eq name_ptr buf 3)))

(sys:defun nl_sp_eq_let_star
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "let*" [4]: 712271212
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 712271212))
    (nl_cons_sym_eq name_ptr buf 4)))

(sys:defun nl_sp_eq_setq
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "setq" [4]: 1903453555
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 1903453555))
    (nl_cons_sym_eq name_ptr buf 4)))

(sys:defun nl_sp_eq_while
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "while" [5]: 435610544247
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 435610544247))
    (nl_cons_sym_eq name_ptr buf 5)))

(sys:defun nl_sp_eq_lambda
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "lambda" [6]: 107083775959404
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 107083775959404))
    (nl_cons_sym_eq name_ptr buf 6)))

(sys:defun nl_sp_eq_function
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "function" [8]: 7957695015192261990
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 7957695015192261990))
    (nl_cons_sym_eq name_ptr buf 8)))

(sys:defun nl_sp_eq_quote
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "quote" [5]: 435745158513
  (let ((buf usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 buf 435745158513))
    (nl_cons_sym_eq name_ptr buf 5)))

(sys:defun nl_sp_eq_condition_case
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "condition-case" [14]: 8028075806769966947, 111546229534062
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8028075806769966947)
     (sys:poke-u64 (+ buf 8) 111546229534062))
    (nl_cons_sym_eq name_ptr buf 14)))

(sys:defun nl_sp_eq_unwind_protect
    ((name_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; "unwind-protect" [14]: 8083227331578523253, 127970252713842
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 8083227331578523253)
     (sys:poke-u64 (+ buf 8) 127970252713842))
    (nl_cons_sym_eq name_ptr buf 14)))

;; --- nl_apply_special dispatch body ---

(sys:defun nl_apply_special
    ((name_ptr usize) (args_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((env_ptr usize (sys:cast usize env)))
    ;; "progn" (most frequent in body forms)
    (if (= (nl_sp_eq_progn name_ptr) 1)
        (sys:unsafe (nl_sf_progn args_ptr env_ptr out 0))
      ;; "if"
      (if (= (nl_sp_eq_if name_ptr) 1)
          (sys:unsafe (nl_sf_if args_ptr env_ptr out 0))
        ;; "let"
        (if (= (nl_sp_eq_let name_ptr) 1)
            (sys:unsafe (nl_sf_let args_ptr env_ptr out 0))
          ;; "let*"
          (if (= (nl_sp_eq_let_star name_ptr) 1)
              (sys:unsafe (nl_sf_let_star args_ptr env_ptr out 0))
            ;; "setq"
            (if (= (nl_sp_eq_setq name_ptr) 1)
                (sys:unsafe (nl_sf_setq args_ptr env_ptr out 0))
              ;; "while"
              (if (= (nl_sp_eq_while name_ptr) 1)
                  (sys:unsafe (nl_sf_while args_ptr env_ptr out 0))
                ;; "lambda" — s1 variant (Nil-initialised scratch slot)
                (if (= (nl_sp_eq_lambda name_ptr) 1)
                    (let ((s1 usize (sys:alloc 32 8)))
                      (nl_cons_write_nil s1)
                      (sys:unsafe (nl_sf_lambda args_ptr env_ptr out s1)))
                  ;; "function" — s1 variant
                  (if (= (nl_sp_eq_function name_ptr) 1)
                      (let ((s1 usize (sys:alloc 32 8)))
                        (nl_cons_write_nil s1)
                        (sys:unsafe (nl_sf_function args_ptr env_ptr out s1)))
                    ;; "quote" — 2-arg, no env; rc!=0 = arity error → return 1
                    (if (= (nl_sp_eq_quote name_ptr) 1)
                        (let ((rc_q i64 (sys:unsafe (nl_sf_quote args_ptr out))))
                          ;; rc_q 0=ok, nonzero=arity error (already stashed by nl_sf_quote)
                          rc_q)
                      ;; "condition-case" — s1 variant
                      (if (= (nl_sp_eq_condition_case name_ptr) 1)
                          (let ((s1 usize (sys:alloc 32 8)))
                            (nl_cons_write_nil s1)
                            (sys:unsafe
                             (nl_sf_condition_case args_ptr env_ptr out s1)))
                        ;; "unwind-protect"
                        (if (= (nl_sp_eq_unwind_protect name_ptr) 1)
                            (sys:unsafe (nl_sf_unwind_protect args_ptr env_ptr out 0))
                          ;; NOT a special form — return 2
                          (sys:cast i64 2))))))))))))))

;; ---------------------------------------------------------------------------
;; nl_eval_inner_cons (public C-ABI entry point)
;; (head_ptr, tail_ptr, env, out) -> i64
;;   head_ptr: *const Sexp — the car of the form being applied (head of the call)
;;   tail_ptr: *const Sexp — the cdr of the form (unevaluated argument list)
;;   env:      (ptr eval_ctx) — EvalCtx*
;;   out:      usize — *mut Sexp
;;   Returns:  0 = ok, result in *out
;;             1 = error, signal stashed in env
;;
;; Implementation plan (mirrors mod.rs nl_eval_inner_cons):
;;
;;  [A] head is NOT Symbol (tag != 4):
;;      1. eval head → func  (nelisp_eval_call)
;;      2. eval arg list     (nl_eval_arg_list)
;;      3. TODO DEFERRED A: use_elisp_apply delegation check
;;         (requires ctx.flags bit0 + delegation_depth counter not in §2.3 layout)
;;      4. nl_apply_function(func, args, env, out)
;;
;;  [B] head IS Symbol (name):
;;      1. sp = nl_apply_special(name, tail, env, out)
;;         sp==0 → return 0
;;         sp==1 → return 1
;;         sp==2 → not special, continue
;;      2. nelisp_env_lookup_function(mirror, unbound, name, func_slot) → rc_lu
;;         rc_lu != 0 → void-function: env_lookup_function already stashed signal → return 1
;;      3. Check if func is (macro . _):
;;         func.car tag==7 (Cons head) AND func.car is Symbol "macro"
;;         [macro path — PARTIALLY IMPLEMENTED: inline apply+eval fallback]
;;         [delegation variant DEFERRED — same reason as A]
;;      4. nl_eval_arg_list(tail, env, args_slot)
;;      5. TODO DEFERRED A: use_elisp_apply delegation for non-macro
;;      6. nl_apply_function(func, args, env, out)
;; ---------------------------------------------------------------------------

;; nl_cons_is_macro(func_ptr) -> i64
;; Returns 1 if func is Cons whose car is Symbol("macro"), 0 otherwise.
;; "macro" [5]: 478660485485
(sys:defun nl_cons_is_macro
    ((func_ptr usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:peek-u64 func_ptr) 7)
      (let ((car_ptr usize (sys:unsafe (nl_cons_car_ptr func_ptr))))
        (if (= (sys:cast usize (sys:peek-u64 car_ptr)) 4)
            (let ((buf usize (sys:alloc 8 1)))
              (sys:unsafe (sys:poke-u64 buf 478660485485))
              (nl_cons_sym_eq car_ptr buf 5))
          (sys:cast i64 0)))
    (sys:cast i64 0)))

;; nl_cons_macro_apply_eval(func_ptr, tail_ptr, env, out) -> i64
;; Inline macro expansion (non-delegation path):
;;   parts = list_elements(func) — func is (macro CLOSURE-OR-LAMBDA FORMALS BODY...)
;;   macrofn = func.cdr.car   (the actual function object, parts[1])
;;   args_unevaled = tail_ptr (the raw unevaluated argument list)
;;   expansion = nl_apply_function(macrofn, args_unevaled, env, exp_slot)
;;   result    = nelisp_eval_call(expansion, env, out)
;;
;; DEFERRED: nelisp--expand-macro delegation variant (requires delegation_depth counter).
;; TODO DEFERRED B: wire the delegation path here when §2.3b extends ctx layout.
(sys:defun nl_cons_macro_apply_eval
    ((func_ptr usize) (tail_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; macrofn = func.cdr.car
  ;; func = (macro . REST), REST = func.cdr
  (let ((func_cdr usize (sys:unsafe (nl_cons_cdr_ptr func_ptr))))
    (if (= (sys:peek-u64 func_cdr) 7)
        (let ((macrofn_ptr usize (sys:unsafe (nl_cons_car_ptr func_cdr)))
              (exp_slot usize (sys:alloc 32 8))
              (env_ptr usize (sys:cast usize env)))
          ;; Apply macrofn to unevaluated args (tail)
          (let ((rc_mac i64 (sys:unsafe
                              (nl_apply_function macrofn_ptr tail_ptr env_ptr exp_slot))))
            (if (= rc_mac 0)
                ;; Re-eval the expansion
                (sys:unsafe (nelisp_eval_call exp_slot env_ptr out))
              (sys:cast i64 1))))
      ;; Malformed macro (no cdr): propagate error
      ;; nelisp_env_lookup_function already set up func; signal as void-function
      (nl_cons_stash_void_function env func_ptr))))

(sys:defun nl_eval_inner_cons
    ((head_ptr usize) (tail_ptr usize) (env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((env_ptr usize (sys:cast usize env))
        (head_tag usize (sys:cast usize (sys:peek-u64 head_ptr))))
    (if (= head_tag 4)
        ;; --- [B] head is Symbol ---
        (let ((sp i64 (nl_apply_special head_ptr tail_ptr env out)))
          (if (= sp 0)
              ;; special form handled ok
              (sys:cast i64 0)
            (if (= sp 1)
                ;; special form error
                (sys:cast i64 1)
              ;; sp==2 → not special; look up as function
              (let ((func_slot usize (sys:alloc 32 8))
                    (mirror_ptr usize (+ (sys:cast usize env)
                                         (sys:offsetof eval_ctx mirror)))
                    (unbound_ptr usize (+ (sys:cast usize env)
                                          (sys:offsetof eval_ctx unbound))))
                (let ((rc_lu i64 (sys:unsafe
                                   (nelisp_env_lookup_function
                                    mirror_ptr unbound_ptr head_ptr func_slot))))
                  (if (= rc_lu 0)
                      ;; lookup ok — check for macro
                      (if (= (nl_cons_is_macro func_slot) 1)
                          ;; macro: inline apply+eval (delegation variant DEFERRED — TODO B)
                          (nl_cons_macro_apply_eval func_slot tail_ptr env out)
                        ;; regular function: eval args, then apply
                        ;; TODO DEFERRED A: use_elisp_apply delegation check here
                        (let ((args_slot usize (sys:alloc 32 8)))
                          (let ((rc_args i64
                                 (sys:unsafe
                                  (nl_eval_arg_list tail_ptr env_ptr args_slot))))
                            (if (= rc_args 0)
                                (sys:unsafe
                                 (nl_apply_function func_slot args_slot env_ptr out))
                              (sys:cast i64 1)))))
                    ;; lookup failed: void-function signal already stashed by callee
                    (sys:cast i64 1)))))))
      ;; --- [A] head is NOT Symbol (any other tag) ---
      ;; eval the head expression to get the function object
      (let ((func_slot usize (sys:alloc 32 8)))
        (let ((rc_eval i64 (sys:unsafe (nelisp_eval_call head_ptr env_ptr func_slot))))
          (if (= rc_eval 0)
              ;; eval args
              ;; TODO DEFERRED A: use_elisp_apply delegation check here
              (let ((args_slot usize (sys:alloc 32 8)))
                (let ((rc_args i64
                       (sys:unsafe
                        (nl_eval_arg_list tail_ptr env_ptr args_slot))))
                  (if (= rc_args 0)
                      (sys:unsafe
                       (nl_apply_function func_slot args_slot env_ptr out))
                    (sys:cast i64 1))))
            (sys:cast i64 1)))))))
