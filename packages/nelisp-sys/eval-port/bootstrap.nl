;; Doc 135 Stage 135.D -- nl_install_builtins (60-builtin bootstrap)
;; STAGED (unwired -- do NOT touch manifest/build.rs)
;; Type-checked, not linked.
;;
;; Registers all 60 builtins into the mirror's function-cells, mirroring
;; the Rust install_builtins in build-tool/src/eval/mod.rs.
;;
;; For each NAME the sequence is:
;;   1. alloc bytes buf, poke UTF-8 LE words, nl_alloc_symbol -> Sexp::Symbol(NAME)
;;   2. cons(Symbol("builtin"), cons(Symbol(NAME), Nil)) -> sentinel
;;   3. build 11-slot function-variant scratch vector (slot5=symbol-entry,
;;      slot7=unbound, slot8=sentinel) -- matches Rust mirror_set_function_or_insert
;;   4. nelisp_mirror_set_function_or_insert(mirror_ptr, name_sym, scratch_vec, 0)
;;
;; Scratch vector layout (function variant, build_or_insert_scratch_vec):
;;   slot 0-4: Nil   slot 5: Symbol("symbol-entry")   slot 6: Nil
;;   slot 7: unbound_ptr (clone)    slot 8: sentinel (= (builtin NAME), clone)
;;   slot 9: Nil (plist)            slot 10: Nil (constant)
;;
;; Shared helper: nl_install_one(mirror_ptr, unbound_ptr, name_bytes_ptr,
;;                               name_len, builtin_sym_slot) -> i64
;;   Builds the sentinel for one NAME and calls set_function_or_insert.
;;
;; Exported entry point:
;;   nl_install_builtins(mirror_ptr: usize, unbound_ptr: usize) -> i64
;;   Calls nl_install_one 60 times in order.
;;
;; Sig change vs initial spec: unbound_ptr is added as second parameter
;; (matches env.unbound_marker; avoids a fresh Sexp::Nil which is wrong for unbound).
;;
;; nm verification (nelisp-build-tool-2761033c43e4fcda/out/libnelisp_elisp_spike.a):
;;   nl_alloc_symbol                      T 0x04cf
;;   nl_alloc_vector                      T 0x01f4
;;   nl_vector_set_slot                   T 0x0000
;;   nelisp_cons_construct                T 0x0000
;;   nelisp_mirror_set_function_or_insert T 0x01e5
;;
;; String byte encodings used here (little-endian u64 words):
;;   "builtin"      [7]:  31078196194145634
;;   "symbol-entry" [12]: 7290602597431212403, 2037544046
;;
;; See also: env-leaves-bind.nl (nl_env_build_scratch pattern reference).
;; NO *.rs files edited. Not committed. Not wired.

;; ---------------------------------------------------------------------------
;; Struct declarations (LOCKED section 2.3 layout)
;; ---------------------------------------------------------------------------

(sys:defstruct sexp (:repr c)
  (tag u8) (payload u64) (pad (array u8 16)))

;; ---------------------------------------------------------------------------
;; sys:extern declarations
;; ---------------------------------------------------------------------------

;; nl_alloc_symbol(bytes_ptr, len, result_slot) -> result_slot (usize)
;; Writes Sexp::Symbol(tag=4) into result_slot and returns result_slot.
(sys:extern nl_alloc_symbol
  (:symbol "nl_alloc_symbol" :abi c :unsafe t)
  ((bytes_ptr usize) (len i64) (result_slot usize))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_alloc_vector(cap) -> box_ptr (usize = NlVector*)
;; Returns a raw NlVector* (NOT a Sexp slot); all slots initialised to Nil.
(sys:extern nl_alloc_vector
  (:symbol "nl_alloc_vector" :abi c :unsafe t)
  ((capacity i64))
  usize
  (:alloc may :ffi may :unsafe may))

;; nl_vector_set_slot(vec_ptr, n, val) -> i64
;; vec_ptr: NlVector* (box_ptr from nl_alloc_vector), NOT a Sexp* address.
;; Copies 4 u64 words raw (no refcount bump). Use only when transferring ownership.
(sys:extern nl_vector_set_slot
  (:symbol "nl_vector_set_slot" :abi c :unsafe t)
  ((vec_ptr usize) (n i64) (val usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; nelisp_cons_construct(car_ptr, cdr_ptr, result_slot) -> i64
(sys:extern nelisp_cons_construct
  (:symbol "nelisp_cons_construct" :abi c :unsafe t)
  ((arg0 usize) (arg1 usize) (result_slot usize))
  i64
  (:alloc may :ffi may :unsafe may))

;; nelisp_mirror_set_function_or_insert(mirror_ptr, sym_ptr, scratch_vec_ptr, _pad) -> i64
;; scratch_vec_ptr: 11-slot Sexp::Vector with slot5=Symbol("symbol-entry"),
;;   slot7=unbound_marker, slot8=function_sexp.
(sys:extern nelisp_mirror_set_function_or_insert
  (:symbol "nelisp_mirror_set_function_or_insert" :abi c :unsafe t)
  ((mirror_ptr usize) (sym_ptr usize) (scratch_vec_ptr usize) (_pad i64))
  i64
  (:alloc may :ffi may :unsafe may))

;; nl_sexp_clone_into(dst, src) -> i64 (refcount-aware clone of *const Sexp into *mut Sexp)
(sys:extern nl_sexp_clone_into
  (:symbol "nl_sexp_clone_into" :abi c :unsafe t)
  ((dst usize) (src usize))
  i64
  (:alloc none :ffi may :unsafe may))

;; ---------------------------------------------------------------------------
;; HELPER: nl_bootstrap_write_symentry(sym_slot) -> sym_slot
;; Writes Symbol("symbol-entry") into sym_slot.
;; Encoding: "symbol-entry" [12] LE u64: 7290602597431212403, 2037544046
;; ---------------------------------------------------------------------------
(sys:defun nl_bootstrap_write_symentry
    ((sym_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((buf usize (sys:alloc 16 1)))
    (sys:unsafe
     (sys:poke-u64 buf 7290602597431212403)
     (sys:poke-u64 (+ buf 8) 2037544046)
     (nl_alloc_symbol buf 12 sym_slot))))

;; ---------------------------------------------------------------------------
;; HELPER: nl_bootstrap_scratch_vec_sexp(box_ptr, sexp_slot) -> sexp_slot
;; Wraps a raw NlVector box_ptr into a Sexp::Vector header written into sexp_slot.
;; Writes: tag=8@0, box_ptr@8, 0@16, 0@24.  Returns sexp_slot.
;; ---------------------------------------------------------------------------
(sys:defun nl_bootstrap_scratch_vec_sexp
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
;; HELPER: nl_bootstrap_build_fn_scratch(unbound_ptr, sentinel_ptr, out_slot) -> usize
;;
;; Builds the 11-slot function-variant scratch vector matching Rust:
;;   build_or_insert_scratch_vec(unbound, sentinel, Nil, Nil)
;;
;; Layout: slot5=Symbol("symbol-entry"), slot7=unbound(clone), slot8=sentinel(clone).
;; Slots 0-4, 6, 9-10 remain Nil (nl_alloc_vector initialises all to Nil).
;;
;; Slot 5 uses nl_vector_set_slot (raw copy; sym_slot is sole owner => transfer ok).
;; Slots 7/8 use nl_sexp_clone_into (refcount-aware) because caller still holds refs.
;;
;; NlVector data_ptr = peek-u64(box_ptr+8).  Slot N at data_ptr + N*32.
;; ---------------------------------------------------------------------------
(sys:defun nl_bootstrap_build_fn_scratch
    ((unbound_ptr usize) (sentinel_ptr usize) (out_slot usize))
  usize
  (:alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 11)))
        (sym_slot usize (sys:alloc 32 8)))
    (let ((data_ptr usize (sys:cast usize (sys:unsafe (sys:peek-u64 (+ box_ptr 8))))))
      (sys:unsafe
       (nl_bootstrap_write_symentry sym_slot)
       (nl_vector_set_slot box_ptr 5 sym_slot)
       (nl_sexp_clone_into unbound_ptr (+ data_ptr 224))
       (nl_sexp_clone_into sentinel_ptr (+ data_ptr 256))
       (nl_bootstrap_scratch_vec_sexp box_ptr out_slot)))))

;; ---------------------------------------------------------------------------
;; SHARED HELPER: nl_install_one
;;   (mirror_ptr unbound_ptr name_bytes_ptr name_len builtin_sym_slot) -> i64
;;
;; Installs one builtin into the mirror's function-cell:
;;   1. nl_alloc_symbol(name_bytes_ptr, name_len, name_slot) => Sexp::Symbol(NAME)
;;   2. inner = nelisp_cons_construct(name_slot, nil_slot, inner_slot)
;;      sent  = nelisp_cons_construct(builtin_sym_slot, inner_slot, sent_slot)
;;              => Sexp::Cons(builtin, Cons(NAME, Nil)) = (builtin NAME)
;;   3. nl_bootstrap_build_fn_scratch(unbound_ptr, sent_slot, scratch_slot)
;;   4. nelisp_mirror_set_function_or_insert(mirror_ptr, name_slot, scratch_slot, 0)
;;
;; The sentinel shape (builtin NAME) matches apply_function's check:
;;   func.car == "builtin" && func.cdr.car == NAME
;;
;; builtin_sym_slot is passed in (built once by nl_install_builtins) to avoid
;; 60 redundant allocations.  It is cloned into the sentinel cons cell.
;; ---------------------------------------------------------------------------
(sys:defun nl_install_one
    ((mirror_ptr usize) (unbound_ptr usize)
     (name_bytes_ptr usize) (name_len i64)
     (builtin_sym_slot usize))
  i64
  (:alloc may :ffi may :unsafe may)
  (let ((name_slot    usize (sys:alloc 32 8))
        (nil_slot     usize (sys:alloc 32 8))
        (inner_slot   usize (sys:alloc 32 8))
        (sent_slot    usize (sys:alloc 32 8))
        (scratch_slot usize (sys:alloc 32 8)))
    ;; Initialise nil_slot to Sexp::Nil (all zeros)
    (sys:unsafe
     (sys:poke-u64 nil_slot 0)
     (sys:poke-u64 (+ nil_slot 8) 0)
     (sys:poke-u64 (+ nil_slot 16) 0)
     (sys:poke-u64 (+ nil_slot 24) 0))
    ;; Step 1: build Symbol(NAME)
    (sys:unsafe (nl_alloc_symbol name_bytes_ptr name_len name_slot))
    ;; Step 2: sentinel = cons(builtin_sym, cons(name_sym, nil))
    (sys:unsafe
     (nelisp_cons_construct name_slot nil_slot inner_slot)
     (nelisp_cons_construct builtin_sym_slot inner_slot sent_slot))
    ;; Step 3: build function-variant scratch vector
    (nl_bootstrap_build_fn_scratch unbound_ptr sent_slot scratch_slot)
    ;; Step 4: register into mirror
    (sys:unsafe
     (nelisp_mirror_set_function_or_insert mirror_ptr name_slot scratch_slot 0))))

;; ---------------------------------------------------------------------------
;; ENTRY POINT: nl_install_builtins(mirror_ptr, unbound_ptr) -> i64
;;
;; Builds Symbol("builtin") once, then calls nl_install_one 60 times
;; in the order matching Rust install_builtins.  Returns 0.
;;
;; Signature: (mirror_ptr usize, unbound_ptr usize) -> i64
;;   mirror_ptr:   *const Sexp pointing at the Env mirror field.
;;   unbound_ptr:  *const Sexp pointing at the unbound_marker Sexp.
;;
;; "builtin" [7] LE u64: 31078196194145634
;; ---------------------------------------------------------------------------
(sys:defun nl_install_builtins
    ((mirror_ptr usize) (unbound_ptr usize))
  i64
  (:export "nl_install_builtins" :abi c :alloc may :ffi may :unsafe may)
  ;; Build Symbol("builtin") once -- shared (cloned) across all 60 sentinels.
  (let ((builtin_sym_slot usize (sys:alloc 32 8))
        (builtin_buf      usize (sys:alloc 8 1)))
    (sys:unsafe (sys:poke-u64 builtin_buf 31078196194145634))
    (sys:unsafe (nl_alloc_symbol builtin_buf 7 builtin_sym_slot))
    ;; Install all 60 builtins in Rust install_builtins order.
    ;; Each let allocates a name buffer, pokes UTF-8 LE words, calls nl_install_one.
    ;; Results are discarded (nelisp_mirror_set_function_or_insert is infallible).
    (seq
     ;; 0/60: "fset" [4] (Doc 135: was missing from the 60-builtin list -- the
     ;; first stdlib form `(fset 'cons ...)' could not resolve fset, so the
     ;; mirror lookup missed and crashed instead of dispatching to nl_apply_do_fset)
     (let ((bfset usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ bfset 0) 1952805734))
       (nl_install_one mirror_ptr unbound_ptr bfset 4 builtin_sym_slot))
     ;; 1/60: "vector" [6]
     (let ((b0 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b0 0) 125823019607414))
       (nl_install_one mirror_ptr unbound_ptr b0 6 builtin_sym_slot))
     ;; 2/60: "make-vector" [11]
     (let ((b1 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b1 0) 7162260719749783917))
       (sys:unsafe (sys:poke-u64 (+ b1 8) 7499636))
       (nl_install_one mirror_ptr unbound_ptr b1 11 builtin_sym_slot))
     ;; 3/60: "nelisp--length-cons-cc" [22]
     (let ((b2 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b2 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b2 8) 7146483032867628396))
       (sys:unsafe (sys:poke-u64 (+ b2 16) 109277615451759))
       (nl_install_one mirror_ptr unbound_ptr b2 22 builtin_sym_slot))
     ;; 4/60: "nelisp--recordp-cc" [18]
     (let ((b3 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b3 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b3 8) 3274227371756184946))
       (sys:unsafe (sys:poke-u64 (+ b3 16) 25443))
       (nl_install_one mirror_ptr unbound_ptr b3 18 builtin_sym_slot))
     ;; 5/60: "string-bytes" [12]
     (let ((b4 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b4 0) 7074424313582089331))
       (sys:unsafe (sys:poke-u64 (+ b4 8) 1936028793))
       (nl_install_one mirror_ptr unbound_ptr b4 12 builtin_sym_slot))
     ;; 6/60: "nl-jit-call-format-float" [24]
     (let ((b5 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b5 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b5 8) 7886488382118587489))
       (sys:unsafe (sys:poke-u64 (+ b5 16) 8386106492501980257))
       (nl_install_one mirror_ptr unbound_ptr b5 24 builtin_sym_slot))
     ;; 7/60: "truncate" [8]
     (let ((b6 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b6 0) 7310575174812004980))
       (nl_install_one mirror_ptr unbound_ptr b6 8 builtin_sym_slot))
     ;; 8/60: "nelisp--syscall-canonicalize" [28]
     (let ((b7 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b7 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b7 8) 3273110194727647603))
       (sys:unsafe (sys:poke-u64 (+ b7 16) 7017568567410188643))
       (sys:unsafe (sys:poke-u64 (+ b7 24) 1702521196))
       (nl_install_one mirror_ptr unbound_ptr b7 28 builtin_sym_slot))
     ;; 9/60: "nelisp--syscall-stat" [20]
     (let ((b8 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b8 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b8 8) 3273110194727647603))
       (sys:unsafe (sys:poke-u64 (+ b8 16) 1952543859))
       (nl_install_one mirror_ptr unbound_ptr b8 20 builtin_sym_slot))
     ;; 10/60: "nelisp--syscall-readdir" [23]
     (let ((b9 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b9 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b9 8) 3273110194727647603))
       (sys:unsafe (sys:poke-u64 (+ b9 16) 32204027246765426))
       (nl_install_one mirror_ptr unbound_ptr b9 23 builtin_sym_slot))
     ;; 11/60: "nelisp--syscall-read-file" [25]
     (let ((b10 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b10 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b10 8) 3273110194727647603))
       (sys:unsafe (sys:poke-u64 (+ b10 16) 7811887373794502002))
       (sys:unsafe (sys:poke-u64 (+ b10 24) 101))
       (nl_install_one mirror_ptr unbound_ptr b10 25 builtin_sym_slot))
     ;; 12/60: "nelisp--syscall" [15]
     (let ((b11 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b11 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b11 8) 30518463020890483))
       (nl_install_one mirror_ptr unbound_ptr b11 15 builtin_sym_slot))
     ;; 13/60: "symbol-function" [15]
     (let ((b12 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b12 0) 7362660191469140339))
       (sys:unsafe (sys:poke-u64 (+ b12 8) 31084746153094773))
       (nl_install_one mirror_ptr unbound_ptr b12 15 builtin_sym_slot))
     ;; 14/60: "fset" [4]
     (let ((b13 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b13 0) 1952805734))
       (nl_install_one mirror_ptr unbound_ptr b13 4 builtin_sym_slot))
     ;; 15/60: "nelisp--push-frame" [18]
     (let ((b14 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b14 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b14 8) 7021787114235983216))
       (sys:unsafe (sys:poke-u64 (+ b14 16) 25965))
       (nl_install_one mirror_ptr unbound_ptr b14 18 builtin_sym_slot))
     ;; 16/60: "nelisp--pop-frame" [17]
     (let ((b15 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b15 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b15 8) 7881706606049652592))
       (sys:unsafe (sys:poke-u64 (+ b15 16) 101))
       (nl_install_one mirror_ptr unbound_ptr b15 17 builtin_sym_slot))
     ;; 17/60: "nelisp--push-captured" [21]
     (let ((b16 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b16 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b16 8) 8097862651665937776))
       (sys:unsafe (sys:poke-u64 (+ b16 16) 431198729588))
       (nl_install_one mirror_ptr unbound_ptr b16 21 builtin_sym_slot))
     ;; 18/60: "nelisp--bind-local" [18]
     (let ((b17 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b17 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b17 8) 7165064474384034146))
       (sys:unsafe (sys:poke-u64 (+ b17 16) 27745))
       (nl_install_one mirror_ptr unbound_ptr b17 18 builtin_sym_slot))
     ;; 19/60: "nelisp--apply-builtin-dispatch" [30]
     (let ((b18 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b18 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b18 8) 8458373049688813665))
       (sys:unsafe (sys:poke-u64 (+ b18 16) 7594244823892388969))
       (sys:unsafe (sys:poke-u64 (+ b18 24) 114776363593843))
       (nl_install_one mirror_ptr unbound_ptr b18 30 builtin_sym_slot))
     ;; 20/60: "nelisp--set-use-elisp-apply" [27]
     (let ((b19 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b19 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b19 8) 3271147651465504115))
       (sys:unsafe (sys:poke-u64 (+ b19 16) 8097803565984738405))
       (sys:unsafe (sys:poke-u64 (+ b19 24) 7957616))
       (nl_install_one mirror_ptr unbound_ptr b19 27 builtin_sym_slot))
     ;; 21/60: "nelisp--apply-lambda-inner" [26]
     (let ((b20 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b20 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b20 8) 7020035918697361505))
       (sys:unsafe (sys:poke-u64 (+ b20 16) 7957413235238658669))
       (sys:unsafe (sys:poke-u64 (+ b20 24) 29285))
       (nl_install_one mirror_ptr unbound_ptr b20 26 builtin_sym_slot))
     ;; 22/60: "funcall" [7]
     (let ((b21 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b21 0) 30518463020561766))
       (nl_install_one mirror_ptr unbound_ptr b21 7 builtin_sym_slot))
     ;; 23/60: "apply" [5]
     (let ((b22 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b22 0) 521510350945))
       (nl_install_one mirror_ptr unbound_ptr b22 5 builtin_sym_slot))
     ;; 24/60: "eval" [4]
     (let ((b23 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b23 0) 1818326629))
       (nl_install_one mirror_ptr unbound_ptr b23 4 builtin_sym_slot))
     ;; 25/60: "signal" [6]
     (let ((b24 usize (sys:alloc 8 1)))
       (sys:unsafe (sys:poke-u64 (+ b24 0) 119165719898483))
       (nl_install_one mirror_ptr unbound_ptr b24 6 builtin_sym_slot))
     ;; 26/60: "nelisp--write-stdout-bytes" [26]
     (let ((b25 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b25 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b25 8) 8391100444489380471))
       (sys:unsafe (sys:poke-u64 (+ b25 16) 8392847327948533604))
       (sys:unsafe (sys:poke-u64 (+ b25 24) 29541))
       (nl_install_one mirror_ptr unbound_ptr b25 26 builtin_sym_slot))
     ;; 27/60: "nelisp--write-stderr-line" [25]
     (let ((b26 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b26 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b26 8) 8391100444489380471))
       (sys:unsafe (sys:poke-u64 (+ b26 16) 7956009159176119652))
       (sys:unsafe (sys:poke-u64 (+ b26 24) 101))
       (nl_install_one mirror_ptr unbound_ptr b26 25 builtin_sym_slot))
     ;; 28/60: "read-stdin-bytes" [16]
     (let ((b27 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b27 0) 7238537139886056818))
       (sys:unsafe (sys:poke-u64 (+ b27 8) 8315180351696498281))
       (nl_install_one mirror_ptr unbound_ptr b27 16 builtin_sym_slot))
     ;; 29/60: "nelisp--f64-trunc" [17]
     (let ((b28 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b28 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b28 8) 7959393759747389030))
       (sys:unsafe (sys:poke-u64 (+ b28 16) 99))
       (nl_install_one mirror_ptr unbound_ptr b28 17 builtin_sym_slot))
     ;; 30/60: "nl-write-file" [13]
     (let ((b29 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b29 0) 7310584035475811438))
       (sys:unsafe (sys:poke-u64 (+ b29 8) 435610543661))
       (nl_install_one mirror_ptr unbound_ptr b29 13 builtin_sym_slot))
     ;; 31/60: "nl-make-directory" [17]
     (let ((b30 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b30 0) 3271138770542226542))
       (sys:unsafe (sys:poke-u64 (+ b30 8) 8245937412991248740))
       (sys:unsafe (sys:poke-u64 (+ b30 16) 121))
       (nl_install_one mirror_ptr unbound_ptr b30 17 builtin_sym_slot))
     ;; 32/60: "terminal-raw-mode-enter" [23]
     (let ((b31 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b31 0) 7809644627923985780))
       (sys:unsafe (sys:poke-u64 (+ b31 8) 7237123168251507245))
       (sys:unsafe (sys:poke-u64 (+ b31 16) 32199698087751013))
       (nl_install_one mirror_ptr unbound_ptr b31 23 builtin_sym_slot))
     ;; 33/60: "terminal-raw-mode-leave" [23]
     (let ((b32 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b32 0) 7809644627923985780))
       (sys:unsafe (sys:poke-u64 (+ b32 8) 7237123168251507245))
       (sys:unsafe (sys:poke-u64 (+ b32 16) 28559133333269861))
       (nl_install_one mirror_ptr unbound_ptr b32 23 builtin_sym_slot))
     ;; 34/60: "read-stdin-byte-available" [25]
     (let ((b33 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b33 0) 7238537139886056818))
       (sys:unsafe (sys:poke-u64 (+ b33 8) 3271148769041542761))
       (sys:unsafe (sys:poke-u64 (+ b33 16) 7809911822066218593))
       (sys:unsafe (sys:poke-u64 (+ b33 24) 101))
       (nl_install_one mirror_ptr unbound_ptr b33 25 builtin_sym_slot))
     ;; 35/60: "_termios-saved-p" [16]
     (let ((b34 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b34 0) 8317982955568198751))
       (sys:unsafe (sys:poke-u64 (+ b34 8) 8083227293140480813))
       (nl_install_one mirror_ptr unbound_ptr b34 16 builtin_sym_slot))
     ;; 36/60: "_raw-mode-hooks-installed-p" [27]
     (let ((b35 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b35 0) 7237123168251507295))
       (sys:unsafe (sys:poke-u64 (+ b35 8) 3275079480383122789))
       (sys:unsafe (sys:poke-u64 (+ b35 16) 7308335461136821865))
       (sys:unsafe (sys:poke-u64 (+ b35 24) 7351652))
       (nl_install_one mirror_ptr unbound_ptr b35 27 builtin_sym_slot))
     ;; 37/60: "set-quit-flag" [13]
     (let ((b36 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b36 0) 8388364909908616563))
       (sys:unsafe (sys:poke-u64 (+ b36 8) 444016125485))
       (nl_install_one mirror_ptr unbound_ptr b36 13 builtin_sym_slot))
     ;; 38/60: "clear-quit-flag" [15]
     (let ((b37 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b37 0) 8462595144089431139))
       (sys:unsafe (sys:poke-u64 (+ b37 8) 29099040799814761))
       (nl_install_one mirror_ptr unbound_ptr b37 15 builtin_sym_slot))
     ;; 39/60: "quit-flag-pending-p" [19]
     (let ((b38 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b38 0) 7020098264576390513))
       (sys:unsafe (sys:poke-u64 (+ b38 8) 7956000642037722471))
       (sys:unsafe (sys:poke-u64 (+ b38 16) 7351655))
       (nl_install_one mirror_ptr unbound_ptr b38 19 builtin_sym_slot))
     ;; 40/60: "install-sigint-handler" [22]
     (let ((b39 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b39 0) 3273110195012857449))
       (sys:unsafe (sys:poke-u64 (+ b39 8) 7506784171460094323))
       (sys:unsafe (sys:poke-u64 (+ b39 16) 125779935784545))
       (nl_install_one mirror_ptr unbound_ptr b39 22 builtin_sym_slot))
     ;; 41/60: "_sigint-handler-installed-p" [27]
     (let ((b40 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b40 0) 3275364227990778719))
       (sys:unsafe (sys:poke-u64 (+ b40 8) 3274791395267600744))
       (sys:unsafe (sys:poke-u64 (+ b40 16) 7308335461136821865))
       (sys:unsafe (sys:poke-u64 (+ b40 24) 7351652))
       (nl_install_one mirror_ptr unbound_ptr b40 27 builtin_sym_slot))
     ;; 42/60: "install-winsize-handler" [23]
     (let ((b41 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b41 0) 3273110195012857449))
       (sys:unsafe (sys:poke-u64 (+ b41 8) 3271155297681303927))
       (sys:unsafe (sys:poke-u64 (+ b41 16) 32199663560843624))
       (nl_install_one mirror_ptr unbound_ptr b41 23 builtin_sym_slot))
     ;; 43/60: "_winsize-handler-installed-p" [28]
     (let ((b42 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b42 0) 7312272889483982687))
       (sys:unsafe (sys:poke-u64 (+ b42 8) 8243113871575967789))
       (sys:unsafe (sys:poke-u64 (+ b42 16) 7812726606361684269))
       (sys:unsafe (sys:poke-u64 (+ b42 24) 1882023013))
       (nl_install_one mirror_ptr unbound_ptr b42 28 builtin_sym_slot))
     ;; 44/60: "terminal-take-winsize-changed" [29]
     (let ((b43 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b43 0) 7809644627923985780))
       (sys:unsafe (sys:poke-u64 (+ b43 8) 7599592809827497005))
       (sys:unsafe (sys:poke-u64 (+ b43 16) 7521905716507538286))
       (sys:unsafe (sys:poke-u64 (+ b43 24) 431198006881))
       (nl_install_one mirror_ptr unbound_ptr b43 29 builtin_sym_slot))
     ;; 45/60: "terminal-current-winsize" [24]
     (let ((b44 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b44 0) 7809644627923985780))
       (sys:unsafe (sys:poke-u64 (+ b44 8) 8389754698058785581))
       (sys:unsafe (sys:poke-u64 (+ b44 16) 7312272889483982637))
       (nl_install_one mirror_ptr unbound_ptr b44 24 builtin_sym_slot))
     ;; 46/60: "install-jobctrl-handlers" [24]
     (let ((b45 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b45 0) 3273110195012857449))
       (sys:unsafe (sys:poke-u64 (+ b45 8) 3273116873400676202))
       (sys:unsafe (sys:poke-u64 (+ b45 16) 8318822977922556264))
       (nl_install_one mirror_ptr unbound_ptr b45 24 builtin_sym_slot))
     ;; 47/60: "_jobctrl-handlers-installed-p" [29]
     (let ((b46 usize (sys:alloc 32 1)))
       (sys:unsafe (sys:poke-u64 (+ b46 0) 7814436273643285087))
       (sys:unsafe (sys:poke-u64 (+ b46 8) 8243113871575967789))
       (sys:unsafe (sys:poke-u64 (+ b46 16) 7809651267959598451))
       (sys:unsafe (sys:poke-u64 (+ b46 24) 481797891436))
       (nl_install_one mirror_ptr unbound_ptr b46 29 builtin_sym_slot))
     ;; 48/60: "terminal-take-sigcont" [21]
     (let ((b47 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b47 0) 7809644627923985780))
       (sys:unsafe (sys:poke-u64 (+ b47 8) 7598466909920654381))
       (sys:unsafe (sys:poke-u64 (+ b47 16) 500069000039))
       (nl_install_one mirror_ptr unbound_ptr b47 21 builtin_sym_slot))
     ;; 49/60: "nl-jit-call-i64-i64" [19]
     (let ((b48 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b48 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b48 8) 3257288255857257569))
       (sys:unsafe (sys:poke-u64 (+ b48 16) 3421801))
       (nl_install_one mirror_ptr unbound_ptr b48 19 builtin_sym_slot))
     ;; 50/60: "nl-jit-call-ptr-ptr" [19]
     (let ((b49 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b49 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b49 8) 3274807904199011425))
       (sys:unsafe (sys:poke-u64 (+ b49 16) 7500912))
       (nl_install_one mirror_ptr unbound_ptr b49 19 builtin_sym_slot))
     ;; 51/60: "nl-jit-call-syscall" [19]
     (let ((b50 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b50 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b50 8) 7166204967666871393))
       (sys:unsafe (sys:poke-u64 (+ b50 16) 7105633))
       (nl_install_one mirror_ptr unbound_ptr b50 19 builtin_sym_slot))
     ;; 52/60: "nl-jit-call-out-1" [17]
     (let ((b51 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b51 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b51 8) 3275371949369093217))
       (sys:unsafe (sys:poke-u64 (+ b51 16) 49))
       (nl_install_one mirror_ptr unbound_ptr b51 17 builtin_sym_slot))
     ;; 53/60: "nl-jit-call-out-2" [17]
     (let ((b52 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b52 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b52 8) 3275371949369093217))
       (sys:unsafe (sys:poke-u64 (+ b52 16) 50))
       (nl_install_one mirror_ptr unbound_ptr b52 17 builtin_sym_slot))
     ;; 54/60: "nl-jit-call-out-1i" [18]
     (let ((b53 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b53 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b53 8) 3275371949369093217))
       (sys:unsafe (sys:poke-u64 (+ b53 16) 26929))
       (nl_install_one mirror_ptr unbound_ptr b53 18 builtin_sym_slot))
     ;; 55/60: "nl-jit-call-out-2i" [18]
     (let ((b54 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b54 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b54 8) 3275371949369093217))
       (sys:unsafe (sys:poke-u64 (+ b54 16) 26930))
       (nl_install_one mirror_ptr unbound_ptr b54 18 builtin_sym_slot))
     ;; 56/60: "nl-jit-call-float-float" [23]
     (let ((b55 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b55 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b55 8) 7020949530198436961))
       (sys:unsafe (sys:poke-u64 (+ b55 16) 32758228486335860))
       (nl_install_one mirror_ptr unbound_ptr b55 23 builtin_sym_slot))
     ;; 57/60: "nl-jit-call-float-cmp" [21]
     (let ((b56 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b56 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b56 8) 7020949530198436961))
       (sys:unsafe (sys:poke-u64 (+ b56 16) 482871553396))
       (nl_install_one mirror_ptr unbound_ptr b56 21 builtin_sym_slot))
     ;; 58/60: "nl-jit-call-float-unary" [23]
     (let ((b57 usize (sys:alloc 24 1)))
       (sys:unsafe (sys:poke-u64 (+ b57 0) 7146496179808595054))
       (sys:unsafe (sys:poke-u64 (+ b57 8) 7020949530198436961))
       (sys:unsafe (sys:poke-u64 (+ b57 16) 34184234972556660))
       (nl_install_one mirror_ptr unbound_ptr b57 23 builtin_sym_slot))
     ;; 59/60: "nl-fact-i64" [11]
     (let ((b58 usize (sys:alloc 16 1)))
       (sys:unsafe (sys:poke-u64 (+ b58 0) 3275352098982423662))
       (sys:unsafe (sys:poke-u64 (+ b58 8) 3421801))
       (nl_install_one mirror_ptr unbound_ptr b58 11 builtin_sym_slot))
     ;; 60/60: "nelisp--read-all-from-string-native" [35]
     (let ((b59 usize (sys:alloc 40 1)))
       (sys:unsafe (sys:poke-u64 (+ b59 0) 3255381746650998126))
       (sys:unsafe (sys:poke-u64 (+ b59 8) 7812726301166495090))
       (sys:unsafe (sys:poke-u64 (+ b59 16) 8391100478765819437))
       (sys:unsafe (sys:poke-u64 (+ b59 24) 8386105122428447090))
       (sys:unsafe (sys:poke-u64 (+ b59 32) 6649449))
       (nl_install_one mirror_ptr unbound_ptr b59 35 builtin_sym_slot))
     (sys:cast i64 0))))

;; End of Doc 135 Stage 135.D -- nl_install_builtins

;; --- Doc 135 Stage 135.E: nl_bootstrap_make_mirror (nelisp-sys, raw leaked scratch) ---
(sys:extern nelisp_env_install_empty_globals_frames
  (:symbol "nelisp_env_install_empty_globals_frames" :abi c :unsafe t)
  ((globals_out usize) (frames_out usize) (scratch_ptr usize) (_pad i64))
  i64 (:alloc may :ffi may :unsafe may))

(sys:defun nl_bootstrap_make_mirror
    ((globals_out usize) (frames_out usize) (unbound_out usize))
  i64
  (:export "nl_bootstrap_make_mirror" :abi c :alloc may :ffi may :unsafe may)
  (let ((box_ptr usize (sys:unsafe (nl_alloc_vector 6)))
        (s0 usize (sys:alloc 32 8)) (s1 usize (sys:alloc 32 8)) (s2 usize (sys:alloc 32 8))
        (scr usize (sys:alloc 32 8))
        (buf0 usize (sys:alloc 16 1)) (buf1 usize (sys:alloc 16 1))
        (buf2 usize (sys:alloc 24 1)) (ubuf usize (sys:alloc 24 1)))
    (sys:unsafe
     (sys:poke-u64 buf0 7290607012774962542) (sys:poke-u64 (+ buf0 8) 30318)
     (nl_alloc_symbol buf0 10 s0) (nl_vector_set_slot box_ptr 0 s0)
     (sys:poke-u64 buf1 8314040931539181926) (sys:poke-u64 (+ buf1 8) 28548142445374824)
     (nl_alloc_symbol buf1 15 s1) (nl_vector_set_slot box_ptr 1 s1)
     (sys:poke-u64 buf2 7795010171040458094) (sys:poke-u64 (+ buf2 8) 3271140969653106789)
     (sys:poke-u64 (+ buf2 16) 461228831859)
     (nl_alloc_symbol buf2 21 s2) (nl_vector_set_slot box_ptr 2 s2)
     (nl_bootstrap_scratch_vec_sexp box_ptr scr)
     (nelisp_env_install_empty_globals_frames globals_out frames_out scr 0)
     (sys:poke-u64 ubuf 3255381746650998126) (sys:poke-u64 (+ ubuf 8) 3270860680036773493)
     (sys:poke-u64 (+ ubuf 16) 125779919921517)
     (nl_alloc_symbol ubuf 22 unbound_out)
     (nl_install_builtins globals_out unbound_out)
     (sys:cast i64 0))))
