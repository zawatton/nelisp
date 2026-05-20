//! NeLisp build-time tool: reader + eval + JIT bridge.
//! x86_64-linux only.

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!("Doc 114: nelisp-build-tool requires x86_64-linux. Build via Docker / Linux VM.");

pub mod eval;
pub(crate) mod jit;
pub mod reader;

/// Rust home for elisp `.o` symbols; keeps linked elisp objects alive.
pub mod elisp_cc_spike {
    use crate::eval::sexp::Sexp;

    macro_rules! cc_wrap {
        ($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty) => {
            #[allow(clippy::missing_safety_doc)]
            pub unsafe fn $name($($arg: $aty),*) -> $ret { $extern($($arg),*) }
        };
    }

    macro_rules! jit_wrap {
        ($($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty);* $(;)?) => {
            $(pub fn $name($($arg: $aty),*) -> $ret { unsafe { $extern($($arg),*) } })*
        };
    }

    // Sexp is #[repr(C, u8)]; elisp touches tag + payload only.
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        fn nelisp_fact_i64(n: i64) -> i64;
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_truncate_float(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_eq_symbol(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_cons_construct(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_bi_string_bytes(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_recordp(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_aref_vector(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_bi_make_vector(
            n_ptr: *const Sexp,
            init_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_bi_nl_fact_i64(arg_ptr: *const Sexp, result_slot: *mut Sexp) -> i64;
        fn nelisp_bi_set_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_clear_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_quit_flag_pending_p(flag_ptr: *const i64) -> i64;
        fn nelisp_bi_write_stderr_line(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_write_stdout_bytes(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_read_stdin_bytes(buf_ptr: *mut u8, limit: i64) -> i64;
        fn nelisp_bi_signal_dispatch(
            tag_ptr: *const Sexp,
            quit_ptr: *const Sexp,
            arith_ptr: *const Sexp,
            wrong_type_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_cell_value(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_set_value(arg0: *const Sexp, val_ptr: *const Sexp);
        fn nelisp_cell_make(val_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_null_p(arg0: *const Sexp) -> i64;
        fn nelisp_sexp_write_str(slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp;
        fn nelisp_sexp_write_symbol(slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp;
        fn nelisp_sexp_write_float(slot_bits: f64, val: f64) -> *mut Sexp;
        fn nelisp_mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp;
        fn nelisp_mut_str_push_byte(ptr: *mut Sexp, byte: i64) -> i64;
        fn nelisp_mut_str_push_codepoint(ptr: *mut Sexp, cp: i64) -> i64;
        fn nelisp_mut_str_len(ptr: *const Sexp) -> i64;
        fn nelisp_mut_str_finalize(ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_str_char_count(ptr: *const Sexp) -> i64;
        fn nelisp_str_codepoint_at(
            ptr: *const Sexp,
            idx: i64,
            cp_slot: *mut i64,
            width_slot: *mut i64,
        ) -> i64;
        fn nelisp_str_is_alphanumeric_at(ptr: *const Sexp, idx: i64) -> i64;
        fn nelisp_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64;
        fn nelisp_atomic_compare_exchange(ptr: *mut i64, expected: i64, new_val: i64) -> i64;
        fn nelisp_ptr_read_u64(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_ptr_read_u8(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_alloc_bytes(size: i64, align: i64) -> *mut u8;
        fn nelisp_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) -> i64;
        fn nelisp_cstr_from_sexp(str_ptr: *const Sexp) -> *mut u8;
        fn nelisp_cstr_drop(buf_ptr: *mut u8, size: i64) -> i64;
        fn nelisp_bi_syscall_stat(path_ptr: *const Sexp, statbuf: *mut u8) -> i64;
        fn nelisp_bi_syscall_canonicalize(path_ptr: *const Sexp, result_buf: *mut u8) -> i64;
        fn nelisp_bi_nl_write_file(path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64;
        fn nelisp_bi_nl_make_directory(path_ptr: *const Sexp) -> i64;
        fn nelisp_bi_syscall_read_file(
            path_ptr: *const Sexp,
            buf_ptr: *mut u8,
            read_size: i64,
        ) -> i64;
        fn nelisp_bi_syscall_resolve_nr(sym_ptr: *const Sexp) -> i64;
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
        // nl_alloc_consbox / nl_alloc_cell / nl_alloc_vector / nl_alloc_record
        // — exported by their respective elisp `.o' files; declared here so
        // the static archive is pulled into the link and other elisp `.o' PLT
        // refs to these symbols resolve correctly.  Types cast to *mut u8 to
        // avoid importing the concrete types in this module.
        fn nl_alloc_consbox() -> *mut u8;
        fn nl_alloc_cell(initial: *const u8) -> *mut u8;
        fn nl_alloc_vector(capacity: i64) -> *mut u8;
        fn nl_alloc_record(type_tag_ptr: *const u8, slot_count: i64) -> *mut u8;
        fn nelisp_nlconsbox_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlconsbox_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlboolvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlchartable_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_mirror_lookup_entry(mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64;
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
        fn nelisp_mirror_is_constant(mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64;
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
        fn nelisp_frame_stack_ensure_capacity(
            frames_ptr: *const Sexp,
            needed: i64,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_fnv1a(str_ptr: *const Sexp) -> i64;
        fn nelisp_reader_lex_one(
            str_ptr: *const Sexp,
            cursor: i64,
            payload_slot: *mut Sexp,
            cursor_out_slot: *mut Sexp,
            scratch_mutstr_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_reader_parse_one(
            str_ptr: *const Sexp,
            cursor_slot: *mut Sexp,
            result_slot: *mut Sexp,
            slot_pool: *const Sexp,
            depth: i64,
        ) -> i64;
        fn nl_sf_quote(args: *const Sexp, out: *mut Sexp) -> i64;
        fn nl_sf_if(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_setq(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_progn(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_while(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_let(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_let_star(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_sf_lambda(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            s1: *mut Sexp,
        ) -> i64;
        fn nl_sf_function(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            s1: *mut Sexp,
        ) -> i64;
        fn nl_sf_condition_case(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            s1: *mut Sexp,
        ) -> i64;
        fn nl_sf_unwind_protect(
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
        fn nl_apply_lambda_inner(
            captured: *const Sexp,
            formals: *const Sexp,
            body_list: *const Sexp,
            args_list: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
        ) -> i64;
        // Phase 47 — nl_bind_formals_impl: pure-elisp bind_formals_impl
        // (Stage 1 parallel implementation in nelisp-cc-bind-formals.el).
        fn nl_bind_formals_impl(
            formals: *const Sexp,
            args: *const Sexp,
            env: *mut std::ffi::c_void,
            _pad: i64,
        ) -> i64;
        // Phase 47 — nl_eval_inner: sexp-tag dispatch; cons path → nl_eval_inner_cons.
        fn nl_eval_inner(
            form: *const Sexp,
            env: *mut std::ffi::c_void,
            out: *mut Sexp,
            _pad: i64,
        ) -> i64;
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
        pub fn nelisp_jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64;
        pub fn nelisp_jit_ref_eq(a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_ref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_set(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_length(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aset(arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp)
            -> i64;
        pub fn nelisp_jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        // Doc 122 §122.J — sexp.rs formatter chain elisp化.
        fn nelisp_fmt_sexp(s: *const Sexp, slot: *mut Sexp) -> i64;
        // Doc 86 §86.4 — 7-arm OP dispatcher (get/is/clear); returns i64
        // result code (1=hit, 0=unbound-var, -1=unbound-fn, -2=Rust handles).
        fn nelisp_env_shim_op(
            op_ptr: *const Sexp,
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
            result_slot: *mut Sexp,
            vec_scratch: *mut Sexp,
        ) -> i64;
        fn nl_bi_f64_trunc_impl(mode: *const Sexp, num: *const Sexp, den: *const Sexp, out: *mut Sexp) -> i64; // Doc 118
        fn nelisp_wrap_alist_cells(alist_ptr: *const Sexp, result_slot: *mut Sexp, work_slot: *mut Sexp, name_slot: *mut Sexp, cell_slot: *mut Sexp, inner_slot: *mut Sexp) -> i64; // Doc 115 §115.4
    }

    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }

    pub fn fact_i64(n: i64) -> i64 {
        unsafe { nelisp_fact_i64(n) }
    }

    cc_wrap!(truncate_int: nelisp_truncate_int, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(truncate_float: nelisp_truncate_float, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(length_cons: nelisp_length_cons, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(eq_symbol: nelisp_eq_symbol, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cons_construct: nelisp_cons_construct, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(bi_string_bytes: nelisp_bi_string_bytes, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(recordp: nelisp_recordp, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // `slot` must start as `Sexp::Nil`.
    cc_wrap!(aref_vector: nelisp_aref_vector, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(bi_make_vector: nelisp_bi_make_vector, (n_ptr: *const Sexp, init_ptr: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(bi_nl_fact_i64: nelisp_bi_nl_fact_i64, (arg_ptr: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(bi_set_quit_flag: nelisp_bi_set_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_clear_quit_flag: nelisp_bi_clear_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_quit_flag_pending_p: nelisp_bi_quit_flag_pending_p, (flag_ptr: *const i64) -> i64);
    cc_wrap!(bi_write_stderr_line: nelisp_bi_write_stderr_line, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_write_stdout_bytes: nelisp_bi_write_stdout_bytes, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_read_stdin_bytes: nelisp_bi_read_stdin_bytes, (buf_ptr: *mut u8, limit: i64) -> i64);
    cc_wrap!(bi_signal_dispatch: nelisp_bi_signal_dispatch, (tag_ptr: *const Sexp, quit_ptr: *const Sexp, arith_ptr: *const Sexp, wrong_type_ptr: *const Sexp) -> i64);
    cc_wrap!(cell_value: nelisp_cell_value, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_set_value: nelisp_cell_set_value, (arg0: *const Sexp, val_ptr: *const Sexp) -> ());
    cc_wrap!(cell_make: nelisp_cell_make, (val_ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_null_p: nelisp_cell_null_p, (arg0: *const Sexp) -> i64);
    cc_wrap!(sexp_write_str: nelisp_sexp_write_str, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);
    cc_wrap!(sexp_write_symbol: nelisp_sexp_write_symbol, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);

    /// Grammar path for `sexp-write-float`; caller bit-casts `slot` through `f64`.
    pub unsafe fn sexp_write_float_via_grammar(slot: *mut Sexp, val: f64) -> *mut Sexp {
        let slot_bits = f64::from_bits(slot as u64);
        nelisp_sexp_write_float(slot_bits, val)
    }

    /// Direct Rust entry for `nl_sexp_write_float`.
    pub unsafe fn sexp_write_float_extern(slot: *mut Sexp, val: f64) -> *mut Sexp {
        crate::eval::nlstr::nl_sexp_write_float(slot, val)
    }

    /// Thin wrapper over `nl_str_to_float`.
    pub unsafe fn str_to_float(bytes_ptr: *const u8, len: i64, slot: *mut Sexp) -> i64 {
        crate::eval::nlstr::nl_str_to_float(bytes_ptr, len, slot)
    }

    cc_wrap!(mut_str_make_empty: nelisp_mut_str_make_empty, (slot: *mut Sexp, cap: i64) -> *mut Sexp);
    cc_wrap!(mut_str_push_byte: nelisp_mut_str_push_byte, (ptr: *mut Sexp, byte: i64) -> i64);
    cc_wrap!(mut_str_push_codepoint: nelisp_mut_str_push_codepoint, (ptr: *mut Sexp, cp: i64) -> i64);
    cc_wrap!(mut_str_len: nelisp_mut_str_len, (ptr: *const Sexp) -> i64);
    cc_wrap!(mut_str_finalize: nelisp_mut_str_finalize, (ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(str_char_count: nelisp_str_char_count, (ptr: *const Sexp) -> i64);
    cc_wrap!(str_codepoint_at: nelisp_str_codepoint_at, (ptr: *const Sexp, idx: i64, cp_slot: *mut i64, width_slot: *mut i64) -> i64);
    cc_wrap!(str_is_alphanumeric_at: nelisp_str_is_alphanumeric_at, (ptr: *const Sexp, idx: i64) -> i64);
    cc_wrap!(atomic_fetch_add: nelisp_atomic_fetch_add, (ptr: *mut i64, delta: i64) -> i64);
    cc_wrap!(atomic_compare_exchange: nelisp_atomic_compare_exchange, (ptr: *mut i64, expected: i64, new_val: i64) -> i64);
    cc_wrap!(ptr_read_u64: nelisp_ptr_read_u64, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u64: nelisp_ptr_write_u64, (ptr: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(ptr_read_u8: nelisp_ptr_read_u8, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u8: nelisp_ptr_write_u8, (ptr: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(alloc_bytes: nelisp_alloc_bytes, (size: i64, align: i64) -> *mut u8);
    cc_wrap!(dealloc_bytes: nelisp_dealloc_bytes, (ptr: *mut u8, size: i64, align: i64) -> i64);
    cc_wrap!(cstr_from_sexp: nelisp_cstr_from_sexp, (str_ptr: *const Sexp) -> *mut u8);
    cc_wrap!(cstr_drop: nelisp_cstr_drop, (buf_ptr: *mut u8, size: i64) -> i64);

    cc_wrap!(bi_syscall_stat: nelisp_bi_syscall_stat, (path_ptr: *const Sexp, statbuf: *mut u8) -> i64);
    cc_wrap!(bi_syscall_canonicalize: nelisp_bi_syscall_canonicalize, (path_ptr: *const Sexp, result_buf: *mut u8) -> i64);
    cc_wrap!(bi_nl_write_file: nelisp_bi_nl_write_file, (path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_nl_make_directory: nelisp_bi_nl_make_directory, (path_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_syscall_read_file: nelisp_bi_syscall_read_file, (path_ptr: *const Sexp, buf_ptr: *mut u8, read_size: i64) -> i64);
    cc_wrap!(bi_syscall_resolve_nr: nelisp_bi_syscall_resolve_nr, (sym_ptr: *const Sexp) -> i64);
    cc_wrap!(rc_inc: nelisp_rc_inc, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_dec: nelisp_rc_dec, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_strong_count: nelisp_rc_strong_count, (box_ptr: *const u8) -> i64);
    cc_wrap!(rc_kind: nelisp_rc_kind, (sexp_ptr: *const u8) -> i64);
    cc_wrap!(rc_payload_ptr: nelisp_rc_payload_ptr, (sexp_ptr: *const u8) -> i64);
    cc_wrap!(gc_walk_children: nelisp_gc_walk_children, (sexp_ptr: *const Sexp, result_slot: *mut Sexp, tail_slot: *mut Sexp) -> *mut Sexp);
    /// Pin `nl_alloc_consbox' symbol from the elisp `.o' archive so
    /// other elisp `.o' PLT references to it are resolved at link time.
    pub unsafe fn nl_alloc_consbox_raw() -> *mut u8 { nl_alloc_consbox() }
    /// Pin `nl_alloc_cell', `nl_alloc_vector', `nl_alloc_record' symbols
    /// from their elisp `.o' archives (same rationale as nl_alloc_consbox_raw).
    pub unsafe fn nl_alloc_cell_raw(initial: *const u8) -> *mut u8 { nl_alloc_cell(initial) }
    pub unsafe fn nl_alloc_vector_raw(capacity: i64) -> *mut u8 { nl_alloc_vector(capacity) }
    pub unsafe fn nl_alloc_record_raw(type_tag_ptr: *const u8, slot_count: i64) -> *mut u8 { nl_alloc_record(type_tag_ptr, slot_count) }
    cc_wrap!(nlconsbox_clone: nelisp_nlconsbox_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlconsbox_drop: nelisp_nlconsbox_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_drop: nelisp_nlvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_drop: nelisp_nlcell_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_drop: nelisp_nlrecord_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_drop: nelisp_nlstr_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_clone: nelisp_nlvector_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_clone: nelisp_nlcell_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_clone: nelisp_nlrecord_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_clone: nelisp_nlstr_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlboolvector_drop: nelisp_nlboolvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlchartable_drop: nelisp_nlchartable_drop, (box_ptr: *mut i64) -> i64);

    /// Raw mirror entry lookup; borrowed pointer, 0 on miss.
    pub unsafe fn mirror_lookup_entry(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
    ) -> *const Sexp {
        nelisp_mirror_lookup_entry(mirror_ptr, sym_ptr) as *const Sexp
    }

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
    cc_wrap!(env_shim_op: nelisp_env_shim_op, (op_ptr: *const Sexp, mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp, result_slot: *mut Sexp, vec_scratch: *mut Sexp) -> i64);

    /// Scratch vector for the four `_or_insert` wrappers.
    fn build_or_insert_scratch_vec(
        value: Sexp,
        function: Sexp,
        plist: Sexp,
        constant: Sexp,
    ) -> Sexp {
        Sexp::vector(vec![
            Sexp::Nil,                           // 0: Nil source
            Sexp::Nil,                           // 1: inner-pair scratch
            Sexp::Nil,                           // 2: outer-cell scratch
            Sexp::Nil,                           // 3: count int scratch
            Sexp::Nil,                           // 4: KEY str scratch
            Sexp::Symbol("symbol-entry".into()), // 5: type tag
            Sexp::Nil,                           // 6: entry result
            value,                               // 7: value cell
            function,                            // 8: function cell
            plist,                               // 9: plist
            constant,                            // 10: constant flag
        ])
    }

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
        nelisp_mirror_set_value_or_insert(mirror_ptr, sym_ptr, &scratch as *const Sexp, 0)
    }

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
        nelisp_mirror_set_function_or_insert(mirror_ptr, sym_ptr, &scratch as *const Sexp, 0)
    }

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
        nelisp_mirror_set_constant_or_insert(mirror_ptr, sym_ptr, &scratch as *const Sexp, 0)
    }

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
        nelisp_mirror_install_entry_or_insert(mirror_ptr, sym_ptr, &scratch as *const Sexp, 0)
    }

    pub unsafe fn frame_stack_ensure_capacity(frames_ptr: *const Sexp, needed: i64) -> i64 {
        let mut scratch = Sexp::Nil;
        nelisp_frame_stack_ensure_capacity(frames_ptr, needed, &mut scratch as *mut Sexp)
    }

    pub unsafe fn wrap_alist_cells(alist_ptr: *const Sexp, result_slot: *mut Sexp) -> i64 {
        let (mut w, mut n, mut c, mut i) = (Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil);
        let rc = nelisp_wrap_alist_cells(alist_ptr, result_slot, &mut w, &mut n, &mut c, &mut i);
        // Prevent double-free: cons-make raw-copies without Rc::clone; zero
        // scratch slots via ptr::write (= Nil-fill without calling Drop).
        for s in [&mut w, &mut n, &mut c, &mut i] { core::ptr::write(s, Sexp::Nil); }
        rc
    }

    cc_wrap!(fnv1a: nelisp_fnv1a, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(reader_lex_one: nelisp_reader_lex_one, (str_ptr: *const Sexp, cursor: i64, payload_slot: *mut Sexp, cursor_out_slot: *mut Sexp, scratch_mutstr_slot: *mut Sexp) -> i64);
    cc_wrap!(reader_parse_one: nelisp_reader_parse_one, (str_ptr: *const Sexp, cursor_slot: *mut Sexp, result_slot: *mut Sexp, slot_pool: *const Sexp, depth: i64) -> i64);

    jit_wrap!(
        jit_add2: nelisp_jit_add2, (a: i64, b: i64) -> i64;
        jit_sub2: nelisp_jit_sub2, (a: i64, b: i64) -> i64;
        jit_mul2: nelisp_jit_mul2, (a: i64, b: i64) -> i64;
        jit_eq2: nelisp_jit_eq2, (a: i64, b: i64) -> i64;
        jit_lt2: nelisp_jit_lt2, (a: i64, b: i64) -> i64;
        jit_gt2: nelisp_jit_gt2, (a: i64, b: i64) -> i64;
        jit_le2: nelisp_jit_le2, (a: i64, b: i64) -> i64;
        jit_ge2: nelisp_jit_ge2, (a: i64, b: i64) -> i64;
        jit_logior2: nelisp_jit_logior2, (a: i64, b: i64) -> i64;
        jit_logand2: nelisp_jit_logand2, (a: i64, b: i64) -> i64;
        jit_logxor2: nelisp_jit_logxor2, (a: i64, b: i64) -> i64;
        jit_ash: nelisp_jit_ash, (n: i64, c: i64) -> i64;
        jit_float_add: nl_jit_float_add, (a: f64, b: f64) -> f64;
        jit_float_sub: nl_jit_float_sub, (a: f64, b: f64) -> f64;
        jit_float_mul: nl_jit_float_mul, (a: f64, b: f64) -> f64;
        jit_float_div: nl_jit_float_div, (a: f64, b: f64) -> f64;
        jit_float_lt: nl_jit_float_lt, (a: f64, b: f64) -> i64;
        jit_float_gt: nl_jit_float_gt, (a: f64, b: f64) -> i64;
        jit_float_le: nl_jit_float_le, (a: f64, b: f64) -> i64;
        jit_float_ge: nl_jit_float_ge, (a: f64, b: f64) -> i64;
        jit_float_eq_eps: nl_jit_float_eq_eps, (a: f64, b: f64) -> i64;
        jit_float_float: nl_jit_float_float, (x: f64) -> f64;
        jit_float_exp: nl_jit_float_exp, (x: f64) -> f64;
        jit_float_log: nl_jit_float_log, (x: f64) -> f64
    );

    cc_wrap!(sf_quote_call: nl_sf_quote, (args: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(sf_if_call: nl_sf_if, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_setq_call: nl_sf_setq, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_progn_call: nl_sf_progn, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_while_call: nl_sf_while, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_let_call: nl_sf_let, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_let_star_call: nl_sf_let_star, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(sf_lambda_call: nl_sf_lambda, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64);
    cc_wrap!(sf_function_call: nl_sf_function, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64);
    cc_wrap!(sf_condition_case_call: nl_sf_condition_case, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64);
    cc_wrap!(sf_unwind_protect_call: nl_sf_unwind_protect, (args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(apply_lambda_inner_call: nl_apply_lambda_inner,
        (captured: *const Sexp, formals: *const Sexp, body_list: *const Sexp,
         args_list: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp) -> i64);
    cc_wrap!(bind_formals_impl_call: nl_bind_formals_impl,
        (formals: *const Sexp, args: *const Sexp,
         env: *mut std::ffi::c_void, _pad: i64) -> i64);
    cc_wrap!(eval_inner_call: nl_eval_inner,
        (form: *const Sexp, env: *mut std::ffi::c_void,
         out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(fmt_sexp_call: nelisp_fmt_sexp, (s: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(jit_predicate_eq: nelisp_jit_predicate_eq, (a: *const Sexp, b: *const Sexp) -> i64);
    cc_wrap!(jit_ref_eq: nelisp_jit_ref_eq, (a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_type: nelisp_jit_record_type, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_len: nelisp_jit_record_len, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_ref: nelisp_jit_record_ref, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
    cc_wrap!(jit_record_set: nelisp_jit_record_set, (arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_length: nelisp_jit_length, (arg: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_aref: nelisp_jit_aref, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
    cc_wrap!(jit_aset: nelisp_jit_aset, (arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(jit_elt: nelisp_jit_elt, (arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64);
    cc_wrap!(f64_trunc_impl: nl_bi_f64_trunc_impl, (mode: *const Sexp, num: *const Sexp, den: *const Sexp, out: *mut Sexp) -> i64);
}
