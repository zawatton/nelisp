;; Doc 135 Stage 135.D — eval_arg_list combiner helper; STAGED
;; (unwired — do NOT touch manifest/build.rs)
;; Type-checked not linked.
;;
;; Implements nl_eval_arg_list(args_ptr, env, out_list_slot) -> i64
;;
;; Rust equivalent (fa8932eb^ eval/mod.rs walk_proper_list + eval_arg_list):
;;   fn eval_arg_list(args: &Sexp, env: &mut Env) -> Result<Vec<Sexp>, EvalError> {
;;     walk_proper_list(args, |car| eval(car, env))
;;   }
;;   // Then apply_function consumes the Vec<Sexp> as a slice.
;;
;; OUTPUT REPRESENTATION CHOICE:
;;   A fresh Sexp cons-LIST (left-to-right order) is the natural representation
;;   because:
;;     (a) nl_eval_inner_cons / apply_function already consume args as list_elements
;;         → a cons-list avoids any extra allocation step at the consumer.
;;     (b) Tail-recursive forward accumulation (recurse-on-rest-first, then cons
;;         current-front) produces the correct left-to-right order without a
;;         reverse pass.
;;     (c) nelisp_cons_construct is already T in the archive, cost = 1 call/arg.
;;
;; SIGNATURE:
;;   nl_eval_arg_list(args_ptr usize, env (ptr eval_ctx), out_list_slot usize) -> i64
;;     args_ptr:      *const Sexp — the unevaluated argument cons-list (or Nil)
;;     env:           (ptr eval_ctx) — EvalCtx (§2.3 layout) used for sub-evals
;;     out_list_slot: usize — caller-owned 32B Sexp slot; receives the evaluated list
;;     Returns:       0 = ok (out_list_slot = cons-list of evaluated args, left-to-right)
;;                    1 = error from a sub-eval (signal already stashed in env)
;;
;; IMPLEMENTATION:
;;   nl_eval_arg_list_walk(cur_ptr, env_ptr, acc_slot) -> i64
;;     Tail-recursive walk of the input cons-list (cur_ptr).
;;     Strategy: recurse on cdr FIRST so that cons-ing at the front naturally
;;     produces the forward-order result without a reverse pass.
;;       Base case (cur_ptr tag != 7, i.e. Nil): write Sexp::Nil into acc_slot; return 0.
;;       Step:
;;         1. car_ptr = nl_cons_car_ptr(cur_ptr)
;;         2. cdr_ptr = nl_cons_cdr_ptr(cur_ptr)
;;         3. Allocate a 32B eval_slot; nelisp_eval_call(car_ptr, env_ptr, eval_slot) -> rc
;;            If rc != 0: propagate immediately (sub-eval error).
;;         4. Recurse on cdr: nl_eval_arg_list_walk(cdr_ptr, env_ptr, rest_slot)
;;            If rc != 0: propagate.
;;         5. nelisp_cons_construct(eval_slot, rest_slot, acc_slot) — builds (eval . rest)
;;         Return 0.
;;
;;   nl_eval_arg_list builds the result directly into out_list_slot.
;;
;; Archive symbol verification (libnelisp_elisp_spike.a, canonical build):
;;   nelisp_eval_call   U (resolved by Rust link)
;;   nl_cons_car_ptr    T 0x0000
;;   nl_cons_cdr_ptr    T 0x0000
;;   nelisp_cons_construct T 0x0000
;;   nl_sexp_clone_into T 0x0472
;;
;; SEXP tag constants:
;;   0 = Nil   2 = Int   4 = Symbol   7 = Cons
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
;;   offset 0:  tag (u8, 7 pad bytes in upper word → full u64 word = tag for Nil/Cons/Symbol)
;;   offset 8:  payload (u64)
;;   offset 16: pad (16 bytes)

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
;; env_ptr: opaque *mut c_void = eval_ctx* post-cutover.
(sys:extern nelisp_eval_call
  (:symbol "nelisp_eval_call" :abi c :unsafe t)
  ((form_ptr usize) (env_ptr usize) (out_ptr usize))
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

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64
;; Writes a fresh Sexp::Cons(car, cdr) into result_slot. Returns 0.
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((car_ptr usize) (cdr_ptr usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_write_nil_slot(slot) -> i64
;;
;; Writes Sexp::Nil (all-zero 32B) into slot. Returns 0.
;; Used for the base-case list terminator.
;; ---------------------------------------------------------------------------
(sys:defun nl_write_nil_slot
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
;; nl_eval_arg_list_walk(cur_ptr, env_ptr, acc_slot) -> i64
;;
;; Tail-recursive cons-list walker.
;;   cur_ptr:  *const Sexp — current position in the unevaluated arg list
;;   env_ptr:  usize       — eval_ctx* cast to usize (for nelisp_eval_call)
;;   acc_slot: usize       — 32B output slot; written with the result Sexp
;;
;; Base case (cur_ptr tag != 7): write Nil into acc_slot; return 0.
;; Step:
;;   1. car_ptr = nl_cons_car_ptr(cur_ptr)
;;   2. cdr_ptr = nl_cons_cdr_ptr(cur_ptr)
;;   3. Allocate eval_slot (32B); nelisp_eval_call(car_ptr, env_ptr, eval_slot)
;;      → rc; if rc != 0, return 1 immediately.
;;   4. Allocate rest_slot (32B); recurse nl_eval_arg_list_walk(cdr_ptr, env_ptr, rest_slot)
;;      → rc; if rc != 0, return 1 immediately.
;;   5. nelisp_cons_construct(eval_slot, rest_slot, acc_slot)
;;      → acc_slot = Sexp::Cons(evaluated_car, rest_list)
;;   Return 0.
;;
;; ORDER: step 3 (evaluate car) happens BEFORE step 4 (recurse on cdr), which
;; correctly evaluates arguments left-to-right. The recursion on cdr runs after
;; car is fully evaluated, so the cons-ing in step 5 puts the already-evaluated
;; car at the head of the rest-list produced by the recursive call — final result
;; is in original left-to-right order.
;; ---------------------------------------------------------------------------
(sys:defun nl_eval_arg_list_walk
    ((cur_ptr usize) (env_ptr usize) (acc_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (if (= (sys:peek-u64 cur_ptr) 7)
      ;; Sexp::Cons — process this element
      (let ((car_ptr usize (sys:unsafe (nl_cons_car_ptr cur_ptr)))
            (cdr_ptr usize (sys:unsafe (nl_cons_cdr_ptr cur_ptr)))
            (eval_slot usize (sys:alloc 32 8))
            (rest_slot usize (sys:alloc 32 8)))
        (let ((rc_eval i64 (sys:unsafe
                            (nelisp_eval_call car_ptr env_ptr eval_slot))))
          (if (= rc_eval 0)
              ;; car evaluated ok — recurse on cdr
              (let ((rc_rest i64 (nl_eval_arg_list_walk cdr_ptr env_ptr rest_slot)))
                (if (= rc_rest 0)
                    ;; rest built ok — cons(eval_slot, rest_slot) → acc_slot
                    (sys:unsafe
                     (nelisp_cons_construct eval_slot rest_slot acc_slot))
                  ;; recursive error: propagate
                  (sys:cast i64 1)))
            ;; sub-eval error: propagate immediately
            (sys:cast i64 1))))
    ;; Non-Cons (Nil or improper tail): write Nil into acc_slot; return 0.
    (nl_write_nil_slot acc_slot)))

;; ---------------------------------------------------------------------------
;; nl_eval_arg_list(args_ptr, env, out_list_slot) -> i64
;;
;; Public entry point. Thin wrapper that casts env to usize for env_ptr
;; (nelisp_eval_call takes opaque *mut c_void = usize post-cutover).
;;
;; args_ptr:      *const Sexp — unevaluated argument list (cons or Nil)
;; env:           (ptr eval_ctx) — EvalCtx pointer
;; out_list_slot: usize — caller-owned 32B slot; receives the evaluated list
;;
;; Returns 0 on success; 1 on sub-eval error (signal stashed in env).
;; ---------------------------------------------------------------------------
(sys:defun nl_eval_arg_list
    ((args_ptr usize) (env (ptr eval_ctx)) (out_list_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((env_ptr usize (sys:cast usize env)))
    (nl_eval_arg_list_walk args_ptr env_ptr out_list_slot)))
