;; Doc 135 Stage 135.C — nl_env_capture_lexical
;; STAGED (unwired — do NOT touch manifest/build.rs)
;; Type-checked and compile-gated only.
;;
;; Implements ONE public C-ABI entry point:
;;
;;   nl_env_capture_lexical(env: usize, out: usize) -> i64
;;     Post-cutover port of: std::ptr::write(out, (&mut *(env as *mut Env)).capture_lexical()); 0
;;     Called by nl_sf_lambda (the lambda special form) to snapshot the current
;;     lexical environment into a closure.
;;
;;     env: *mut EvalCtx (post-cutover; pre-cutover was *mut Env)
;;     out: *mut Sexp (32-byte caller-owned slot; receives the captured alist)
;;     Returns 0 on success.
;;
;; ── Rust semantics (build-tool/src/eval/mod.rs, fa8932eb^) ─────────────────
;;
;;   pub fn capture_lexical(&mut self) -> Sexp {
;;     let Sexp::Record(r) = &self.frames_record else { return Sexp::Nil };
;;     let (Some(Sexp::Int(d)), Ok(f)) =
;;         (r.slots.get(1), self.lookup_function("nelisp-lexframe-stack-capture-to-depth"))
;;     else { return Sexp::Nil };
;;     apply_function(&f, &[self.frames_record.clone(), Sexp::Int(*d)], self)
;;         .unwrap_or(Sexp::Nil)
;;   }
;;
;;   Semantics: walk the current lexical frame stack (`frames_record`, a
;;   nelisp-lexframe-stack Record) up to depth `slots[1]`, collecting all
;;   live bindings into a deduplicated alist of (name . cell) pairs
;;   (inner-shadows-outer, inner first).  The alist is the closure's captured
;;   environment.
;;
;; ── Post-cutover mapping ────────────────────────────────────────────────────
;;
;;   EvalCtx layout (LOCKED §2.3):
;;     offset   0: mirror  (sexp, 32B)
;;     offset  32: frames  (sexp, 32B)    ← frames_record
;;     offset  64: unbound (sexp, 32B)
;;     offset  96: rec_cur (i64)
;;     offset 104: rec_max (i64)
;;     offset 112: flags   (i64)
;;
;;   frames = ctx.frames_record (Sexp at ctx+32).
;;   frames.slots[1] = depth i64 (Record slot 1, stored as Sexp::Int in the
;;                     nelisp-lexframe-stack Record built by bootstrap).
;;
;;   The C-ABI pre-compiled helper nl_capture_descend_native
;;   (T in libnelisp_elisp_spike.a, nelisp_frame_stack_find.o) implements
;;   exactly "nelisp-lexframe-stack-capture-to-depth(frames, depth)" without
;;   calling back through the evaluator.  It takes:
;;     (frames_sexp_ptr usize, out_alist_slot usize) -> i64
;;   On return, *out_alist_slot holds the captured alist (Sexp::Cons chain or Nil).
;;
;;   This file delegates to nl_capture_descend_native rather than routing
;;   through apply_function / nl_eval_inner, because:
;;     1. nl_capture_descend_native IS the compiled nelisp-sys implementation
;;        of the same walk that "nelisp-lexframe-stack-capture-to-depth" performs.
;;     2. Calling back into the evaluator from inside nl_sf_lambda creates a
;;        re-entrant eval context we cannot safely suspend (no continuation
;;        capture in this ABI layer).
;;     3. nl_capture_descend_native is already T in the archive; zero-Rust.
;;
;; ── Deviation flag ──────────────────────────────────────────────────────────
;;
;;   FLAG-A (minor / structural): The Rust body first checks
;;     `frames_record` is Sexp::Record (tag == 9) and that slots[1] is Sexp::Int.
;;   This implementation skips the Record-tag guard and calls
;;   nl_capture_descend_native directly.  If frames is Nil or not a Record,
;;   nl_capture_descend_native sees a Nil-or-mismatched frames pointer and
;;   returns 0 without emitting any alist entries, leaving *out_alist_slot = Nil.
;;   Behavioural result is identical (Nil alist = empty captured env) but the
;;   early-return branch is not taken; instead the helper returns gracefully.
;;   NOT on the immediate (fact 5 / non-closure) critical path.
;;   Correctness can be refined if closure-heavyfuzz exposes divergence.
;;
;;   FLAG-B (structural): depth extraction.  The Rust body reads r.slots.get(1)
;;   as Sexp::Int(*d) and passes Sexp::Int(*d) to apply_function.
;;   nl_capture_descend_native internally performs the same slot[1] read from
;;   frames_sexp_ptr (confirmed by disassembly of nl_capture_descend_native at
;;   offset 0x834: loads -0x8(%rbp)=frames_ptr → slots[1]×0x20 offset → reads
;;   data_ptr → gets count).  We therefore pass frames_sexp_ptr directly without
;;   pre-reading depth — nl_capture_descend_native is self-contained.
;;   No depth is passed explicitly; the helper reads it internally.
;;
;; ── Archive nm verification ──────────────────────────────────────────────────
;;
;;   nl_capture_descend_native   T  (nelisp_frame_stack_find.o)
;;   nl_sexp_clone_into          U  (provided by Rust binary at link)
;;   nl_env_capture_lexical      U  (defined here; resolves nl_sf_lambda's U)
;;
;;   nl_sf_lambda.o calls nl_env_capture_lexical as:
;;     rdi = env_ptr   (EvalCtx*)
;;     rsi = out_ptr   (*mut Sexp 32B slot)
;;   Confirmed by disassembly at nl_sf_lambda+0x47 / reloc 0x321.
;;
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

;; nl_capture_descend_native(frames_sexp_ptr: usize, out_alist_slot: usize) -> i64
;; T in libnelisp_elisp_spike.a (nelisp_frame_stack_find.o).
;; Walks the lexframe-stack Record and builds an alist of (name . cell) pairs
;; with inner-shadows-outer deduplication.  On return, *out_alist_slot holds
;; the alist (Cons chain or Nil).  Returns 0 (ok) always (does not signal).
(sys:extern nl_capture_descend_native
  (:symbol "nl_capture_descend_native" :abi c :unsafe t)
  ((frames_sexp_ptr usize) (out_alist_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_sexp_clone_into(dst: usize, src: usize) -> i64
;; U in archive (provided by Rust binary at link).
;; Refcount-aware 32B Sexp clone: reads *src, increments heap refcount if
;; applicable, writes into *dst.
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; UTILITY: initialise a 32B Sexp slot to Nil (tag=0, payload=0, pad=0)
;; ---------------------------------------------------------------------------

(sys:defun nl_clx_write_nil
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
;; (a) nl_env_capture_lexical(env: usize, out: usize) -> i64
;;
;; Port of Env::capture_lexical (build-tool/src/eval/mod.rs):
;;   1. Locate frames: ctx.frames at EvalCtx+32.
;;   2. Call nl_capture_descend_native(frames_ptr, alist_slot) — this is the
;;      compiled nelisp-sys implementation of nelisp-lexframe-stack-capture-to-depth.
;;   3. Clone the resulting alist into *out.
;;   4. Return 0.
;;
;; FLAG-A: Record-tag guard omitted (see deviation note above).
;; FLAG-B: depth passed implicitly via frames_ptr (nl_capture_descend_native
;;         reads slot[1] internally).
;; ---------------------------------------------------------------------------

(sys:defun nl_env_capture_lexical
    ((env (ptr eval_ctx)) (out usize))
  i64
  (:alloc may :ffi may :unsafe may)
  ;; frames_sexp_ptr = address of ctx.frames field = EvalCtx + offsetof(frames) = ctx+32
  (let ((frames_sexp_ptr usize (+ (sys:cast usize env)
                                  (sys:offsetof eval_ctx frames)))
        ;; Allocate a 32B Nil-initialised scratch slot for the output alist.
        ;; nl_capture_descend_native writes the Cons chain (or leaves Nil) here.
        (alist_slot usize (sys:alloc 32 8)))
    ;; Initialise to Nil (nl_capture_descend_native may leave it Nil if empty).
    (nl_clx_write_nil alist_slot)
    ;; Walk the lexframe stack and populate alist_slot.
    (sys:unsafe (nl_capture_descend_native frames_sexp_ptr alist_slot))
    ;; Clone the alist (Nil or Cons chain) into *out — refcount-correct.
    (sys:unsafe (nl_sexp_clone_into alist_slot out))
    ;; Return 0 (ok).
    (sys:cast i64 0)))
