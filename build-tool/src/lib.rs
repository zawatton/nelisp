//! NeLisp build-time tool — reader + minimal evaluator + JIT bridge.
//! x86_64-linux only (Phase 47 ELF emit produces this crate's `.o' deps).

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!(
    "Doc 114: nelisp-build-tool requires x86_64-linux \
     (Phase 47 emit is x86_64-linux only).  Build via Docker / Linux VM."
);

pub mod eval;
pub(crate) mod jit;
pub mod reader;

/// Rust home for elisp `.o' symbols (Phase 47 emit) — prevents DCE.
pub mod elisp_cc_spike {
    use crate::eval::sexp::Sexp;

    /// `cc_wrap!(FOO: nelisp_FOO, (args) -> ret)` →
    /// `pub unsafe fn FOO(args) -> ret { nelisp_FOO(args) }`.
    macro_rules! cc_wrap {
        ($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty) => {
            #[allow(clippy::missing_safety_doc)]
            pub unsafe fn $name($($arg: $aty),*) -> $ret { $extern($($arg),*) }
        };
    }

    // Sexp is #[repr(C, u8)] (sexp_abi_assert.rs); elisp .o touches
    // only offset 0 (tag) + 8 (payload).  improper_ctypes allow:
    // variants embed String but we never pass by value.
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        // Doc 99 §99.C — recursive i64 factorial from
        // `lisp/nelisp-cc-fact-i64.el'.  N must satisfy 0 ≤ N ≤ 20
        // (= the fixnum-safe range for i64); the elisp body itself
        // doesn't range-check, so callers in safe Rust must clamp.
        fn nelisp_fact_i64(n: i64) -> i64;
        // Doc 100 §100.C — `(truncate INT)' Int arm.
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.B — `(length CONS)' proper-list walk.
        fn nelisp_length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.C — `(eq SYMBOL SYMBOL)'.
        fn nelisp_eq_symbol(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 101 §101.D — `(cons A B)' constructor.
        fn nelisp_cons_construct(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.2 — `(string-bytes STR)' byte-length.
        fn nelisp_bi_string_bytes(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.B — `(recordp X)' predicate.
        fn nelisp_recordp(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.C — `(aref VECTOR IDX)' Vector arm.
        fn nelisp_aref_vector(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.1 — `(make-vector N INIT)' alloc + fill.
        fn nelisp_bi_make_vector(
            n_ptr: *const Sexp,
            init_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> i64;
        // Doc 117 §117.B — quit-flag atomic ops (CAS via §122.E).
        fn nelisp_bi_set_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_clear_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_quit_flag_pending_p(flag_ptr: *const i64) -> i64;
        // Doc 117 §117.B / Doc 122 §122.H — I/O syscall sweep.
        fn nelisp_bi_write_stderr_line(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_write_stdout_bytes(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_read_stdin_bytes(buf_ptr: *mut u8, limit: i64) -> i64;
        // Doc 111 §111.D — Cell ops (nelisp-cc-cell-ops.el).
        fn nelisp_cell_value(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_set_value(arg0: *const Sexp, val_ptr: *const Sexp);
        fn nelisp_cell_make(val_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_null_p(arg0: *const Sexp) -> i64;
        // Doc 122 §122.A — sexp-write-str/symbol grammar ops.
        fn nelisp_sexp_write_str(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        fn nelisp_sexp_write_symbol(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        // Doc 122 §122.G — sexp-write-float (both params f64-class).
        fn nelisp_sexp_write_float(
            slot_bits: f64,
            val: f64,
        ) -> *mut Sexp;
        // Doc 122 §122.B — Mutable string builder grammar ops.
        fn nelisp_mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp;
        fn nelisp_mut_str_push_byte(ptr: *mut Sexp, byte: i64) -> i64;
        fn nelisp_mut_str_push_codepoint(ptr: *mut Sexp, cp: i64) -> i64;
        fn nelisp_mut_str_len(ptr: *const Sexp) -> i64;
        fn nelisp_mut_str_finalize(
            ptr: *const Sexp,
            slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 122 §122.D — UTF-8 helper grammar ops.
        fn nelisp_str_char_count(ptr: *const Sexp) -> i64;
        fn nelisp_str_codepoint_at(
            ptr: *const Sexp,
            idx: i64,
            cp_slot: *mut i64,
            width_slot: *mut i64,
        ) -> i64;
        fn nelisp_str_is_alphanumeric_at(
            ptr: *const Sexp,
            idx: i64,
        ) -> i64;
        // Doc 122 §122.E — Atomic + raw memory primitive Phase 47
        // grammar ops compiled from `lisp/nelisp-cc-atomic-raw-mem.el'.
        // Substrate gate for Doc 123-128 (= refcount elisp化,
        // nl*.rs Clone/Drop elisp化, alloc / dealloc handlers).
        fn nelisp_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64;
        fn nelisp_atomic_compare_exchange(
            ptr: *mut i64,
            expected: i64,
            new_val: i64,
        ) -> i64;
        fn nelisp_ptr_read_u64(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_ptr_read_u8(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) -> i64;
        // Doc 122 §122.J — width-{2,4} raw-mem ops.
        // Doc 125 §125.A — alloc/dealloc primitives.
        fn nelisp_alloc_bytes(size: i64, align: i64) -> *mut u8;
        fn nelisp_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) -> i64;
        // Doc 122 §122.I — CString from/drop (§125.A backed).
        fn nelisp_cstr_from_sexp(str_ptr: *const Sexp) -> *mut u8;
        fn nelisp_cstr_drop(buf_ptr: *mut u8, size: i64) -> i64;
        // Doc 117 §117.D.gaps.3 — file-I/O sweep via §122.I.
        fn nelisp_bi_syscall_stat(path_ptr: *const Sexp, statbuf: *mut u8) -> i64;
        fn nelisp_bi_syscall_canonicalize(
            path_ptr: *const Sexp,
            result_buf: *mut u8,
        ) -> i64;
        fn nelisp_bi_nl_write_file(
            path_ptr: *const Sexp,
            content_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_bi_nl_make_directory(path_ptr: *const Sexp) -> i64;
        fn nelisp_bi_syscall_read_file(
            path_ptr: *const Sexp,
            buf_ptr: *mut u8,
            read_size: i64,
        ) -> i64;
        // Doc 122 §122.C — Extended extern-call (f64 + varargs) probes.
        fn nelisp_libm_sqrt(x: f64) -> f64;
        fn nelisp_libm_sin(x: f64) -> f64;
        fn nelisp_libm_cos(x: f64) -> f64;
        // Doc 123 §123.A-D — refcount + GC walk kernels (live via tests/).
        fn nelisp_rc_inc(box_ptr: *mut i64) -> i64;
        fn nelisp_rc_dec(box_ptr: *mut i64) -> i64;
        fn nelisp_rc_strong_count(box_ptr: *const u8) -> i64;
        fn nelisp_rc_kind(sexp_ptr: *const u8) -> i64;
        fn nelisp_rc_payload_ptr(sexp_ptr: *const u8) -> i64;
        fn nelisp_gc_walk_children(
            sexp_ptr: *const Sexp,
            result_slot: *mut Sexp,
            tail_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 124 §124.A-F — NlBox Clone kernels (refcount-inc + ptr passthrough).
        fn nelisp_nlconsbox_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_clone(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.G-L+ — NlBox Drop kernels (refcount-dec + cond dealloc).
        fn nelisp_nlconsbox_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlboolvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlchartable_drop(box_ptr: *mut i64) -> i64;
        // Doc 111 §111.E #1 — mirror_lookup_entry (returns *Sexp or 0).
        fn nelisp_mirror_lookup_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E #2-6 — Group A read helpers (compose on #1).
        fn nelisp_mirror_lookup_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_lookup_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_is_bound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_fbound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E Group B (#7-#12) — write helpers (compose on #1).
        fn nelisp_mirror_set_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            flag_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_install_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            value_ptr: *const Sexp,
            function_ptr: *const Sexp,
            plist_ptr: *const Sexp,
            constant_ptr: *const Sexp,
        ) -> i64;
        // Doc 119 §119.A — auto-vivify fold (11-slot scratch_vec for prepend + entry).
        fn nelisp_mirror_set_value_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_set_function_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_set_constant_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_install_entry_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        // Doc 111 §111.E #19-26 Group E / Doc 115 — frame stack ops (pure elisp).
        fn nelisp_frame_stack_depth(frames_ptr: *const Sexp) -> i64;
        fn nelisp_frame_stack_ensure_capacity(
            frames_ptr: *const Sexp,
            needed: i64,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_push(
            frames_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_frame_pop(
            frames_ptr: *const Sexp,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_bind(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
            cell_ptr: *const Sexp,
            scratch_pair_slot: *mut Sexp,
            scratch_outer_slot: *mut Sexp,
            scratch_count_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_stack_find(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_wrap_alist_cells(
            alist_ptr: *const Sexp,
            result_slot: *mut Sexp,
            work_slot: *mut Sexp,
            name_slot: *mut Sexp,
            cell_slot: *mut Sexp,
            inner_slot: *mut Sexp,
        ) -> i64;
        // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a (high 32 bits = 0).
        fn nelisp_fnv1a(str_ptr: *const Sexp) -> i64;
        // Doc 116 §116.A — Reader lexer (returns kind 0=EOF/1..11=delim/
        // 20=Int/21=Float/22=Str/23=Sym/24=Char/25=Radix/-1=Error).
        fn nelisp_reader_lex_one(
            str_ptr: *const Sexp,
            cursor: i64,
            payload_slot: *mut Sexp,
            cursor_out_slot: *mut Sexp,
            scratch_mutstr_slot: *mut Sexp,
        ) -> i64;
        // Doc 116 §116.B — Reader parser (consumes §116.A tokens; rc 1=OK / else error).
        fn nelisp_reader_parse_one(
            str_ptr: *const Sexp,
            cursor_slot: *mut Sexp,
            result_slot: *mut Sexp,
            slot_pool: *const Sexp,
            depth: i64,
        ) -> i64;
        // Doc 100 §100.D — 12 nl_jit_arith_* trampoline swaps.
        pub fn nelisp_jit_add2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_sub2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_mul2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_eq2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_lt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_gt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_le2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ge2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logior2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logand2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logxor2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ash(n: i64, c: i64) -> i64;
        // Doc 110 §110 — float arith/cmp/eq-eps + math (= 12 trampolines).
        pub fn nl_jit_float_add(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_sub(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_mul(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_div(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_lt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_gt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_le(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_ge(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_float(x: f64) -> f64;
        pub fn nl_jit_float_exp(x: f64) -> f64;
        pub fn nl_jit_float_log(x: f64) -> f64;
        // Doc 120 §120.A — predicate_eq + ref_eq (sxhash/type_of stay Rust).
        pub fn nelisp_jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64;
        pub fn nelisp_jit_ref_eq(
            a: *const Sexp,
            b: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.B — record_{type,len,ref,set} (alloc stays Rust).
        pub fn nelisp_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_ref(
            arg: *const Sexp,
            idx: i64,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_record_set(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.D — length / aref / aset / elt trampoline swaps.
        pub fn nelisp_jit_length(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aset(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
    }

    /// Doc 99 §99.B probe — call the elisp-compiled function and return
    /// its result.  Used by `tests/elisp_cc_spike_probe.rs' to prove the
    /// build chain (elisp source → Phase 47 compile → ET_REL .o → ar
    /// static archive → cargo link) terminates in a callable symbol.
    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }

    /// Doc 99 §99.C — i64 factorial implemented in elisp.  Wraps the
    /// `nelisp_fact_i64' extern.  The caller is responsible for the
    /// 0..=20 range invariant; the Rust shim in `eval::builtins'
    /// enforces it before calling.
    pub fn fact_i64(n: i64) -> i64 {
        unsafe { nelisp_fact_i64(n) }
    }

    // Doc 100 §100.C — `(truncate INT)' Int arm, elisp-compiled.
    cc_wrap!(truncate_int: nelisp_truncate_int, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.B — `(length CONS)' proper-list walk, elisp-compiled.
    cc_wrap!(length_cons: nelisp_length_cons, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.C — `(eq SYMBOL SYMBOL)' via elisp-compiled Symbol/Str ops.
    cc_wrap!(eq_symbol: nelisp_eq_symbol, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.D — `(cons A B)' via elisp-compiled Cons construction ops.
    cc_wrap!(cons_construct: nelisp_cons_construct, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 117 §117.A.2 — `(string-bytes STR)' byte-length via `str-len' + `sexp-int-make'.
    cc_wrap!(bi_string_bytes: nelisp_bi_string_bytes, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 111 §111.B — `(recordp X)' via elisp-compiled Record ops.
    cc_wrap!(recordp: nelisp_recordp, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 111 §111.C — `(aref VECTOR IDX)' via elisp-compiled Vector read ops.
    // Caller-provided `slot' must be initialized to `Sexp::Nil' (bit-shape
    // Copy) so the refcount-aware `nl_sexp_clone_into' helper's `ptr::write'
    // does not Drop arbitrary bytes.
    cc_wrap!(aref_vector: nelisp_aref_vector, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 117 §117.A.1 — `(make-vector N INIT)' via `vector-make' (§115.1) + fill loop.
    cc_wrap!(bi_make_vector: nelisp_bi_make_vector, (n_ptr: *const Sexp, init_ptr: *const Sexp, slot: *mut Sexp) -> i64);

    // Doc 117 §117.B — QUIT_FLAG atomic ops (set / clear / pending-p) via §122.E CAS.
    cc_wrap!(bi_set_quit_flag: nelisp_bi_set_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_clear_quit_flag: nelisp_bi_clear_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_quit_flag_pending_p: nelisp_bi_quit_flag_pending_p, (flag_ptr: *const i64) -> i64);

    // Doc 117 §117.B / Doc 122 §122.H — I/O syscall sweep (stderr/stdout/stdin).
    // Single libc `write' / `read' per call; non-string args become 0-length no-ops.
    cc_wrap!(bi_write_stderr_line: nelisp_bi_write_stderr_line, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_write_stdout_bytes: nelisp_bi_write_stdout_bytes, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_read_stdin_bytes: nelisp_bi_read_stdin_bytes, (buf_ptr: *mut u8, limit: i64) -> i64);

    // Doc 111 §111.D — Cell ops (value / set-value / make / null-p) via Phase 47 grammar.
    cc_wrap!(cell_value: nelisp_cell_value, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_set_value: nelisp_cell_set_value, (arg0: *const Sexp, val_ptr: *const Sexp) -> ());
    cc_wrap!(cell_make: nelisp_cell_make, (val_ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_null_p: nelisp_cell_null_p, (arg0: *const Sexp) -> i64);

    // Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' Phase 47 grammar ops.
    cc_wrap!(sexp_write_str: nelisp_sexp_write_str, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);
    cc_wrap!(sexp_write_symbol: nelisp_sexp_write_symbol, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);

    /// Doc 122 §122.G — `(sexp-write-float SLOT VAL)' Phase 47 grammar
    /// op.  Phase 47 MVP forbids mixed-class defun params, so slot +
    /// val are both f64-class; slot pointer arrives in xmm0 as a
    /// bit-cast f64 and the elisp emit code MOVQ's it back into rdi
    /// before calling the Rust extern.  Caller bit-casts slot via
    /// `(slot as u64).to_bits()' before invocation.
    ///
    /// # Safety
    /// - `slot' (after bit-cast) must be non-null, aligned, writable
    ///   for one Sexp slot.  Pre-init to `Sexp::Nil'.
    /// - `val' is any f64 (NaN / ±Inf / -0.0 all valid).
    pub unsafe fn sexp_write_float_via_grammar(
        slot: *mut Sexp,
        val: f64,
    ) -> *mut Sexp {
        let slot_bits = f64::from_bits(slot as u64);
        nelisp_sexp_write_float(slot_bits, val)
    }

    /// Doc 122 §122.G — direct Rust-extern entry to
    /// [`nl_sexp_write_float`] (= probe bypasses elisp grammar).
    ///
    /// # Safety
    /// Same as [`sexp_write_float_via_grammar`].
    pub unsafe fn sexp_write_float_extern(slot: *mut Sexp, val: f64) -> *mut Sexp {
        crate::eval::nlstr::nl_sexp_write_float(slot, val)
    }

    /// Doc 122 §122.G — `nl_str_to_float(bytes, len, slot) → i64'.
    /// Parses UTF-8 as f64 via `str::parse::<f64>()'; writes
    /// `Sexp::Float(parsed)' into `*slot' (return 1) or `Sexp::Nil'
    /// (return 0) on failure.
    ///
    /// # Safety
    /// - `bytes_ptr' non-null + valid UTF-8 for `len' bytes (or len=0).
    /// - `slot' non-null + writable; pre-init to Nil.
    pub unsafe fn str_to_float(
        bytes_ptr: *const u8,
        len: i64,
        slot: *mut Sexp,
    ) -> i64 {
        crate::eval::nlstr::nl_str_to_float(bytes_ptr, len, slot)
    }

    // Doc 122 §122.B — MutStr Phase 47 grammar ops (make-empty / push / len / finalize).
    cc_wrap!(mut_str_make_empty: nelisp_mut_str_make_empty, (slot: *mut Sexp, cap: i64) -> *mut Sexp);
    cc_wrap!(mut_str_push_byte: nelisp_mut_str_push_byte, (ptr: *mut Sexp, byte: i64) -> i64);
    cc_wrap!(mut_str_push_codepoint: nelisp_mut_str_push_codepoint, (ptr: *mut Sexp, cp: i64) -> i64);
    cc_wrap!(mut_str_len: nelisp_mut_str_len, (ptr: *const Sexp) -> i64);
    cc_wrap!(mut_str_finalize: nelisp_mut_str_finalize, (ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 122 §122.D — Str Phase 47 grammar ops (char-count / codepoint-at / alphanumeric-at).
    cc_wrap!(str_char_count: nelisp_str_char_count, (ptr: *const Sexp) -> i64);
    cc_wrap!(str_codepoint_at: nelisp_str_codepoint_at, (ptr: *const Sexp, idx: i64, cp_slot: *mut i64, width_slot: *mut i64) -> i64);
    cc_wrap!(str_is_alphanumeric_at: nelisp_str_is_alphanumeric_at, (ptr: *const Sexp, idx: i64) -> i64);

    // Doc 122 §122.E atomic + raw-mem grammar wrappers — collapsed
    // via §127.A `cc_wrap!'.  All SeqCst ordering except as noted
    // in the elisp `.el' source headers.
    cc_wrap!(atomic_fetch_add: nelisp_atomic_fetch_add, (ptr: *mut i64, delta: i64) -> i64);
    cc_wrap!(atomic_compare_exchange: nelisp_atomic_compare_exchange, (ptr: *mut i64, expected: i64, new_val: i64) -> i64);
    cc_wrap!(ptr_read_u64: nelisp_ptr_read_u64, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u64: nelisp_ptr_write_u64, (ptr: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(ptr_read_u8: nelisp_ptr_read_u8, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u8: nelisp_ptr_write_u8, (ptr: *mut u8, offset: i64, val: i64) -> i64);

    // Doc 125 §125.A — generic byte-level alloc / dealloc; Doc 122 §122.I — cstr {from-sexp, drop}.
    cc_wrap!(alloc_bytes: nelisp_alloc_bytes, (size: i64, align: i64) -> *mut u8);
    cc_wrap!(dealloc_bytes: nelisp_dealloc_bytes, (ptr: *mut u8, size: i64, align: i64) -> i64);
    cc_wrap!(cstr_from_sexp: nelisp_cstr_from_sexp, (str_ptr: *const Sexp) -> *mut u8);
    cc_wrap!(cstr_drop: nelisp_cstr_drop, (buf_ptr: *mut u8, size: i64) -> i64);

    // Doc 117 §117.D.gaps.3 — file-I/O syscall body sweeps.  Each
    // wrapper trivially dispatches to its matching `nelisp_*' extern;
    // the safety preconditions are documented per-function (= the
    // shared invariant is that the input Sexp pointer must be valid
    // and point at a string-y variant, and the Rust-owned result
    // buffer must be appropriately sized).
    cc_wrap!(
        bi_syscall_stat: nelisp_bi_syscall_stat,
        (path_ptr: *const Sexp, statbuf: *mut u8) -> i64
    );
    cc_wrap!(
        bi_syscall_canonicalize: nelisp_bi_syscall_canonicalize,
        (path_ptr: *const Sexp, result_buf: *mut u8) -> i64
    );
    cc_wrap!(
        bi_nl_write_file: nelisp_bi_nl_write_file,
        (path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64
    );
    // Doc 117 §117.D.gaps.3 (cont.) — second file-I/O sweep batch.
    // See the extern decl block above for the per-fn contract.
    cc_wrap!(
        bi_nl_make_directory: nelisp_bi_nl_make_directory,
        (path_ptr: *const Sexp) -> i64
    );
    cc_wrap!(
        bi_syscall_read_file: nelisp_bi_syscall_read_file,
        (path_ptr: *const Sexp, buf_ptr: *mut u8, read_size: i64) -> i64
    );

    // Doc 122 §122.C libm probes + Doc 123 §123.A-C rc primitive
    // wrappers — collapsed via §127.A `cc_wrap!'.
    cc_wrap!(libm_sqrt: nelisp_libm_sqrt, (x: f64) -> f64);
    cc_wrap!(libm_sin: nelisp_libm_sin, (x: f64) -> f64);
    cc_wrap!(libm_cos: nelisp_libm_cos, (x: f64) -> f64);
    cc_wrap!(rc_inc: nelisp_rc_inc, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_dec: nelisp_rc_dec, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_strong_count: nelisp_rc_strong_count, (box_ptr: *const u8) -> i64);
    cc_wrap!(rc_kind: nelisp_rc_kind, (sexp_ptr: *const u8) -> i64);

    // Doc 123 §123.D — pure-elisp `nelisp_rc_payload_ptr' kernel.
    // Reads inner NlBox* payload pointer at offset 8 (= `SEXP_PAYLOAD_OFFSET').
    // Tag-dispatch is caller's responsibility — this wrapper performs the raw
    // load unconditionally.
    cc_wrap!(rc_payload_ptr: nelisp_rc_payload_ptr, (sexp_ptr: *const u8) -> i64);

    // Doc 123 §123.D — pure-elisp `nelisp_gc_walk_children' kernel.
    // Materializes the 2-list `Sexp::Cons((car . (cdr . nil)))' in
    // `result_slot' using `tail_slot' as scratch.  Cons-only — caller must
    // filter non-Cons inputs.  Returns `result_slot' for caller ergonomics.
    cc_wrap!(gc_walk_children: nelisp_gc_walk_children, (sexp_ptr: *const Sexp, result_slot: *mut Sexp, tail_slot: *mut Sexp) -> *mut Sexp);

    // Doc 124 §124.A — pure-elisp `nelisp_nlconsbox_clone' kernel.
    // Bumps refcount at NlConsBox offset 64 via §122.E `atomic-fetch-add'
    // (delta=+1), then returns `box_ptr' unchanged.
    cc_wrap!(nlconsbox_clone: nelisp_nlconsbox_clone, (box_ptr: *mut i64) -> i64);

    // Doc 127 §127.A — Doc 124 NlBox Clone/Drop kernel wrappers.
    // The 9 wrappers below are trivially-identical dispatches to
    // their `nelisp_nl{type}_{clone,drop}' extern counterparts; the
    // per-type layout literals (REFCOUNT_OFFSET / SIZE / ALIGN) live
    // in the elisp .o source headers (lisp/nelisp-cc-nl*-{clone,drop}.el)
    // and the §124.A-K design doc, not duplicated in Rust docstrings.
    cc_wrap!(nlconsbox_drop: nelisp_nlconsbox_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_drop: nelisp_nlvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_drop: nelisp_nlcell_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_drop: nelisp_nlrecord_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_drop: nelisp_nlstr_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_clone: nelisp_nlvector_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_clone: nelisp_nlcell_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_clone: nelisp_nlrecord_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_clone: nelisp_nlstr_clone, (box_ptr: *mut i64) -> i64);
    // Doc 124 §124.L+ — Drop kernels for the remaining 2 NlBox types
    // (NlBoolVector + NlCharTable).  Per-type layout literals (SIZE /
    // REFCOUNT_OFFSET / ALIGN) live in `lisp/nelisp-cc-nl{boolvector,
    // chartable}-drop.el' headers, not duplicated here.
    cc_wrap!(nlboolvector_drop: nelisp_nlboolvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlchartable_drop: nelisp_nlchartable_drop, (box_ptr: *mut i64) -> i64);

    /// Doc 111 §111.E #1 — Phase 47 `mirror_lookup_entry' probe wrapper.
    /// Walks the env-mirror fast-hash-table for `sym_ptr' (Symbol/Str)
    /// and returns the raw `*const Sexp' of the matching symbol-entry
    /// Record, or 0 on miss / empty mirror.  The returned pointer is
    /// NOT refcount-bumped — it borrows the bucket's `(KEY . ENTRY)'
    /// cons slot, so callers must clone (`nl_sexp_clone_into') before
    /// storing it past mirror mutation.  Used by
    /// `tests/elisp_cc_mirror_lookup_entry_probe.rs' for the §111.E #1
    /// gate; production still routes through `Env::mirror_lookup_entry'.
    ///
    /// # Safety
    /// - `mirror_ptr' must point at a `Sexp::Record(_)' built by
    ///   `Env::install_empty_mirror_rust_direct' (or `mirror_install_entry'
    ///   descendants).  See extern decl preconditions.
    /// - `sym_ptr' must point at `Sexp::Symbol(_)' / `Sexp::Str(_)'.
    /// - Returned pointer valid only while `*mirror_ptr' bucket chain
    ///   is unchanged.
    pub unsafe fn mirror_lookup_entry(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
    ) -> *const Sexp {
        nelisp_mirror_lookup_entry(mirror_ptr, sym_ptr) as *const Sexp
    }

    // Doc 111 §111.E #2-12 — Phase 47 mirror_* probe wrappers, collapsed
    // via §127.A `cc_wrap!'.
    cc_wrap!(mirror_lookup_value: nelisp_mirror_lookup_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_lookup_function: nelisp_mirror_lookup_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_is_bound: nelisp_mirror_is_bound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_fbound: nelisp_mirror_is_fbound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_constant: nelisp_mirror_is_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_value: nelisp_mirror_set_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, val_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_function: nelisp_mirror_set_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, val_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_value: nelisp_mirror_clear_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_function: nelisp_mirror_clear_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_constant: nelisp_mirror_set_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, flag_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_install_entry: nelisp_mirror_install_entry, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp) -> i64);

    // ---- Doc 119 §119.A auto-vivify fold ---------------------------
    // Each wrapper builds an 11-slot scratch Sexp::Vector, calls the
    // Phase 47 helper (= dispatches hit/miss + writes final state into
    // the mirror), and drops the scratch on return (= refcount balance).
    // Callers in `eval/env_helpers.rs' don't need to know slot layout;
    // see `lisp/nelisp-cc-mirror-set-value-or-insert.el' commentary.

    /// Doc 119 §119.A — build the 11-slot scratch `Sexp::Vector' used
    /// by the four `_or_insert' wrappers.  Slots 0..4 are pre-filled
    /// with `Sexp::Nil' (= caller-owned scratches for `bucket_prepend');
    /// slot 5 holds the `symbol-entry' tag symbol; slot 6 starts as
    /// `Sexp::Nil' (= filled by `alloc_entry' on miss).  Slots 7..10
    /// hold the caller-supplied value / function / plist / constant
    /// Sexps (= refcount-bumped via Vec clone).
    fn build_or_insert_scratch_vec(
        value: Sexp, function: Sexp, plist: Sexp, constant: Sexp,
    ) -> Sexp {
        Sexp::vector(vec![
            Sexp::Nil,                                  // 0: Nil source
            Sexp::Nil,                                  // 1: inner-pair scratch
            Sexp::Nil,                                  // 2: outer-cell scratch
            Sexp::Nil,                                  // 3: count int scratch
            Sexp::Nil,                                  // 4: KEY str scratch
            Sexp::Symbol("symbol-entry".into()),        // 5: type tag
            Sexp::Nil,                                  // 6: entry result
            value,                                      // 7: value cell
            function,                                   // 8: function cell
            plist,                                      // 9: plist
            constant,                                   // 10: constant flag
        ])
    }

    // Doc 119 §119.A — `mirror_set_value' miss-path: VALUE in slot 0, UNBOUND in slot 1.
    pub unsafe fn mirror_set_value_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        value_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*value_ptr).clone(),
            (*unbound_ptr).clone(),
            Sexp::Nil,
            Sexp::Nil,
        );
        nelisp_mirror_set_value_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    // Doc 119 §119.A — `mirror_set_function' miss-path: UNBOUND in slot 0, FUNC in slot 1.
    pub unsafe fn mirror_set_function_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        func_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*unbound_ptr).clone(),
            (*func_ptr).clone(),
            Sexp::Nil,
            Sexp::Nil,
        );
        nelisp_mirror_set_function_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    // Doc 119 §119.A — `mirror_set_constant' miss-path: UNBOUND in slots 0/1, FLAG in slot 3.
    pub unsafe fn mirror_set_constant_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        flag_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*unbound_ptr).clone(),
            (*unbound_ptr).clone(),
            Sexp::Nil,
            (*flag_ptr).clone(),
        );
        nelisp_mirror_set_constant_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    // Doc 119 §119.A — `mirror_install_entry' miss-path: all 4 slot Sexps from caller.
    pub unsafe fn mirror_install_entry_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        value_ptr: *const Sexp,
        function_ptr: *const Sexp,
        plist_ptr: *const Sexp,
        constant_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*value_ptr).clone(),
            (*function_ptr).clone(),
            (*plist_ptr).clone(),
            (*constant_ptr).clone(),
        );
        nelisp_mirror_install_entry_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    // ---- Doc 111 §111.E Group E (env_lexframe.rs) probes -----------

    cc_wrap!(frame_stack_depth: nelisp_frame_stack_depth, (frames_ptr: *const Sexp) -> i64);

    // Doc 115 §115.1 — caller-owned scratch slot kept off the public API.
    pub unsafe fn frame_stack_ensure_capacity(frames_ptr: *const Sexp, needed: i64) -> i64 {
        let mut scratch = Sexp::Nil;
        nelisp_frame_stack_ensure_capacity(frames_ptr, needed, &mut scratch as *mut Sexp)
    }

    // Doc 115 §115.3 — 7-slot scratch vector (2 tag syms + 5 Nil scratches) + odd-arity pad.
    pub unsafe fn frame_push(frames_ptr: *const Sexp) -> i64 {
        let scratch_vec = Sexp::vector(vec![
            Sexp::Symbol("nelisp-lexframe".into()),
            Sexp::Symbol("fast-hash-table".into()),
            Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil,
        ]);
        nelisp_frame_push(frames_ptr, &scratch_vec as *const Sexp, 0)
    }

    // Doc 115 §115.2 — single Nil scratch slot kept off the public API.
    pub unsafe fn frame_pop(frames_ptr: *const Sexp) -> i64 {
        let mut scratch = Sexp::Nil;
        nelisp_frame_pop(frames_ptr, &mut scratch as *mut Sexp)
    }

    // Doc 115 §115.5 — 3 caller-owned scratch Nil slots kept off the public API.
    pub unsafe fn frame_bind(
        frames_ptr: *const Sexp, name_ptr: *const Sexp, cell_ptr: *const Sexp,
    ) -> i64 {
        let mut scratch_pair = Sexp::Nil;
        let mut scratch_outer = Sexp::Nil;
        let mut scratch_count = Sexp::Nil;
        nelisp_frame_bind(frames_ptr, name_ptr, cell_ptr,
            &mut scratch_pair as *mut Sexp,
            &mut scratch_outer as *mut Sexp,
            &mut scratch_count as *mut Sexp)
    }

    // Doc 115 §115.6 — i64-encoded cell-slot ptr → *const Sexp widen (0 = miss).
    pub unsafe fn frame_stack_find(frames_ptr: *const Sexp, name_ptr: *const Sexp) -> *const Sexp {
        nelisp_frame_stack_find(frames_ptr, name_ptr) as *const Sexp
    }

    // Doc 115 §115.4 — 4 caller-owned Nil scratch slots; mem-forget on return
    // to avoid double-decrement (Phase 47 cons-make / cell-make don't bump nested box refs).
    pub unsafe fn wrap_alist_cells(alist_ptr: *const Sexp, result_slot: *mut Sexp) -> i64 {
        let mut work_slot = Sexp::Nil;
        let mut name_slot = Sexp::Nil;
        let mut cell_slot = Sexp::Nil;
        let mut inner_slot = Sexp::Nil;
        let rc = nelisp_wrap_alist_cells(alist_ptr, result_slot,
            &mut work_slot as *mut Sexp, &mut name_slot as *mut Sexp,
            &mut cell_slot as *mut Sexp, &mut inner_slot as *mut Sexp);
        core::ptr::write(&mut work_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut name_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut cell_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut inner_slot as *mut Sexp, Sexp::Nil);
        rc
    }

    // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash (Str/Symbol arms).
    cc_wrap!(fnv1a: nelisp_fnv1a, (str_ptr: *const Sexp) -> i64);

    // Doc 116 §116.A / §116.B — pure-elisp Reader lexer + parser.
    cc_wrap!(reader_lex_one: nelisp_reader_lex_one, (str_ptr: *const Sexp, cursor: i64, payload_slot: *mut Sexp, cursor_out_slot: *mut Sexp, scratch_mutstr_slot: *mut Sexp) -> i64);
    cc_wrap!(reader_parse_one: nelisp_reader_parse_one, (str_ptr: *const Sexp, cursor_slot: *mut Sexp, result_slot: *mut Sexp, slot_pool: *const Sexp, depth: i64) -> i64);

    // Doc 100 §100.D Stage 1 — 12 jit/arith trampoline probes (pin link in test-bin).
    pub fn jit_add2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_add2(a, b) } }
    pub fn jit_sub2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_sub2(a, b) } }
    pub fn jit_mul2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_mul2(a, b) } }
    pub fn jit_eq2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_eq2(a, b) } }
    pub fn jit_lt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_lt2(a, b) } }
    pub fn jit_gt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_gt2(a, b) } }
    pub fn jit_le2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_le2(a, b) } }
    pub fn jit_ge2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_ge2(a, b) } }
    pub fn jit_logior2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logior2(a, b) } }
    pub fn jit_logand2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logand2(a, b) } }
    pub fn jit_logxor2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logxor2(a, b) } }
    pub fn jit_ash(n: i64, c: i64) -> i64 { unsafe { nelisp_jit_ash(n, c) } }

    // Doc 110 §110.E.2.a — 4 jit/float trampoline probes (add / sub / mul / div).
    pub fn jit_float_add(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_add(a, b) } }
    pub fn jit_float_sub(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_sub(a, b) } }
    pub fn jit_float_mul(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_mul(a, b) } }
    pub fn jit_float_div(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_div(a, b) } }

    // Doc 110 §110.C.2.a — 4 jit/float ordered-cmp trampolines (NaN → 0).
    pub fn jit_float_lt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_lt(a, b) } }
    pub fn jit_float_gt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_gt(a, b) } }
    pub fn jit_float_le(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_le(a, b) } }
    pub fn jit_float_ge(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_ge(a, b) } }

    // Doc 110 §110.C.2.b — EQ-EPS probe (|a-b| < 1e-15 ∧ ordered).
    pub fn jit_float_eq_eps(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_eq_eps(a, b) } }

    // Doc 110 §110.F — 3 jit/math trampolines (float identity / exp / log via libm).
    pub fn jit_float_float(x: f64) -> f64 { unsafe { nl_jit_float_float(x) } }
    pub fn jit_float_exp(x: f64) -> f64 { unsafe { nl_jit_float_exp(x) } }
    pub fn jit_float_log(x: f64) -> f64 { unsafe { nl_jit_float_log(x) } }

    // Doc 120 §120.A — jit_predicate_eq / jit_ref_eq Phase 47 probes.
    cc_wrap!(jit_predicate_eq: nelisp_jit_predicate_eq, (a: *const Sexp, b: *const Sexp) -> i64);
    cc_wrap!(jit_ref_eq: nelisp_jit_ref_eq, (a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64);

    // Doc 120 §120.B — jit_record_* (type / len / ref / set) Phase 47 probes.
    cc_wrap!(jit_record_type: nelisp_jit_record_type, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_len: nelisp_jit_record_len, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_ref: nelisp_jit_record_ref, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_set: nelisp_jit_record_set, (arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp) -> i64);

    // Doc 120 §120.D — jit_length / jit_aref / jit_aset / jit_elt Phase 47 probes.
    cc_wrap!(jit_length: nelisp_jit_length, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_aref: nelisp_jit_aref, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
    cc_wrap!(jit_aset: nelisp_jit_aset, (arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_elt: nelisp_jit_elt, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
}
