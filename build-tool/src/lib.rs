#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!("Doc 114: nelisp-build-tool requires x86_64-linux. Build via Docker / Linux VM.");
pub mod eval;
pub(crate) mod jit;
pub mod reader;
pub mod elisp_cc_spike {
    use crate::eval::sexp::Sexp;
    macro_rules! cc_wrap {
        ($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty) => {
            #[allow(clippy::missing_safety_doc)]
            pub unsafe fn $name($($arg: $aty),*) -> $ret { $extern($($arg),*) }
        };
    }
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        fn nelisp_fact_i64(n: i64) -> i64;
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_truncate_float(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_eq_symbol(arg0: *const Sexp, arg1: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_bi_string_bytes(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_recordp(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_bi_make_vector(n_ptr: *const Sexp, init_ptr: *const Sexp, result_slot: *mut Sexp) -> i64;
        fn nelisp_bi_nl_fact_i64(arg_ptr: *const Sexp, result_slot: *mut Sexp) -> i64;
        fn nelisp_bi_set_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_clear_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_quit_flag_pending_p(flag_ptr: *const i64) -> i64;
        fn nelisp_bi_write_stderr_line(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_write_stdout_bytes(str_ptr: *const Sexp) -> i64;
        fn nelisp_bi_read_stdin_bytes(buf_ptr: *mut u8, limit: i64) -> i64;
        fn nelisp_bi_signal_dispatch(tag_ptr: *const Sexp, quit_ptr: *const Sexp, arith_ptr: *const Sexp, wrong_type_ptr: *const Sexp) -> i64;
        fn nelisp_mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp;
        fn nelisp_bi_syscall_stat(path_ptr: *const Sexp, statbuf: *mut u8) -> i64;
        fn nelisp_bi_syscall_canonicalize(path_ptr: *const Sexp, result_buf: *mut u8) -> i64;
        fn nelisp_bi_nl_write_file(path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64;
        fn nelisp_bi_nl_make_directory(path_ptr: *const Sexp) -> i64;
        fn nelisp_bi_syscall_read_file(path_ptr: *const Sexp, buf_ptr: *mut u8, read_size: i64) -> i64;
        fn nelisp_bi_syscall_resolve_nr(sym_ptr: *const Sexp) -> i64;
        fn nl_alloc_consbox() -> *mut u8;
        fn nl_alloc_cell(initial: *const u8) -> *mut u8;
        fn nl_alloc_vector(capacity: i64) -> *mut u8;
        fn nl_alloc_record(type_tag_ptr: *const u8, slot_count: i64) -> *mut u8;
        fn nelisp_nlconsbox_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlboolvector_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlconsbox_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlboolvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlchartable_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlchartable_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_mirror_lookup_entry(mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_lookup_value(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_mirror_lookup_function(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_mirror_is_bound(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_is_fbound(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_is_constant(mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_clear_value(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_clear_function(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_set_constant(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, flag_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_install_entry(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp) -> i64;
        fn nelisp_mirror_set_value_or_insert(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_vec_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_mirror_set_function_or_insert(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_vec_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_mirror_set_constant_or_insert(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_vec_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_mirror_install_entry_or_insert(mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_vec_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_frame_stack_ensure_capacity(frames_ptr: *const Sexp, needed: i64, scratch_slot: *mut Sexp) -> i64;
        fn nelisp_frame_stack_install(frames_ptr: *const Sexp, frame_ptr: *const Sexp, scratch_slot: *mut Sexp) -> i64;
        fn nelisp_fnv1a(str_ptr: *const Sexp) -> i64;
        fn nelisp_reader_lex_one(str_ptr: *const Sexp, cursor: i64, payload_slot: *mut Sexp, cursor_out_slot: *mut Sexp, scratch_mutstr_slot: *mut Sexp) -> i64;
        fn nelisp_reader_parse_one(str_ptr: *const Sexp, cursor_slot: *mut Sexp, result_slot: *mut Sexp, slot_pool: *const Sexp, depth: i64) -> i64;
        fn nl_sf_quote(args: *const Sexp, out: *mut Sexp) -> i64;
        fn nl_sf_if(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_setq(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_progn(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_while(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_let(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_let_star(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_sf_lambda(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64;
        fn nl_sf_function(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64;
        fn nl_sf_condition_case(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, s1: *mut Sexp) -> i64;
        fn nl_sf_unwind_protect(args: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nl_apply_lambda_inner(captured: *const Sexp, formals: *const Sexp, body_list: *const Sexp, args_list: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp) -> i64;
        fn nl_bind_formals_impl(formals: *const Sexp, args: *const Sexp, env: *mut std::ffi::c_void, _pad: i64) -> i64;
        fn nl_eval_inner(form: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64;
        fn nelisp_fmt_sexp(s: *const Sexp, slot: *mut Sexp) -> i64;
        fn nelisp_env_shim_op(op_ptr: *const Sexp, mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp, result_slot: *mut Sexp, vec_scratch: *mut Sexp) -> i64;
        fn nl_bi_f64_trunc_impl(mode: *const Sexp, num: *const Sexp, den: *const Sexp, out: *mut Sexp) -> i64;
        fn nelisp_wrap_alist_cells(alist_ptr: *const Sexp, result_slot: *mut Sexp, work_slot: *mut Sexp, name_slot: *mut Sexp, cell_slot: *mut Sexp, inner_slot: *mut Sexp) -> i64;
        fn nelisp_env_lookup_value(mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, out: *mut Sexp) -> i64;
        fn nelisp_env_set_value(mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, val_ptr: *const Sexp, scratch_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_env_lookup_function(mirror_ptr: *const Sexp, unbound_ptr: *const Sexp, name_ptr: *const Sexp, out: *mut Sexp) -> i64;
        fn nelisp_env_shim_set_op(op_ptr: *const Sexp, mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_ptr: *const Sexp, result_slot: *mut Sexp, _pad: i64) -> i64;
        fn nelisp_env_bind_local(mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, val_ptr: *const Sexp, scratch_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_frame_bind(frames_ptr: *const Sexp, name_ptr: *const Sexp, cell_ptr: *const Sexp, scratch_pair: *mut Sexp, scratch_outer: *mut Sexp, scratch_count: *mut Sexp) -> i64;
        fn nelisp_frame_stack_find(frames_ptr: *const Sexp, name_ptr: *const Sexp) -> i64;
        fn nelisp_frame_push(frames_ptr: *const Sexp, scratch_vec_ptr: *const Sexp) -> i64;
        fn nelisp_env_install_empty_globals_frames(globals_out: *mut Sexp, frames_out: *mut Sexp, scratch_ptr: *const Sexp, _pad: i64) -> i64;
        fn nelisp_tty_raw_enter(statbuf: *mut u8) -> i64;
        fn nelisp_tty_raw_leave(saved_buf: *const u8) -> i64;
        fn nelisp_tty_stdin_byte_avail(pfd_buf: *mut u8, timeout_ms: i64) -> i64;
        fn nelisp_tty_winsize_current(ws_buf: *mut u8) -> i64;
        fn nelisp_tty_take_atomic(flag_ptr: *mut i64) -> i64;
    }
    pub fn probe() -> i64 { unsafe { nelisp_spike_noop() } }
    pub fn fact_i64(n: i64) -> i64 { unsafe { nelisp_fact_i64(n) } }
    cc_wrap!(truncate_int: nelisp_truncate_int, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(truncate_float: nelisp_truncate_float, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(length_cons: nelisp_length_cons, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(eq_symbol: nelisp_eq_symbol, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(bi_string_bytes: nelisp_bi_string_bytes, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(recordp: nelisp_recordp, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(bi_make_vector: nelisp_bi_make_vector, (n_ptr: *const Sexp, init_ptr: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(bi_nl_fact_i64: nelisp_bi_nl_fact_i64, (arg_ptr: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(bi_set_quit_flag: nelisp_bi_set_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_clear_quit_flag: nelisp_bi_clear_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_quit_flag_pending_p: nelisp_bi_quit_flag_pending_p, (flag_ptr: *const i64) -> i64);
    cc_wrap!(bi_write_stderr_line: nelisp_bi_write_stderr_line, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_write_stdout_bytes: nelisp_bi_write_stdout_bytes, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_read_stdin_bytes: nelisp_bi_read_stdin_bytes, (buf_ptr: *mut u8, limit: i64) -> i64);
    cc_wrap!(bi_signal_dispatch: nelisp_bi_signal_dispatch, (tag_ptr: *const Sexp, quit_ptr: *const Sexp, arith_ptr: *const Sexp, wrong_type_ptr: *const Sexp) -> i64);
    cc_wrap!(mut_str_make_empty: nelisp_mut_str_make_empty, (slot: *mut Sexp, cap: i64) -> *mut Sexp);
    cc_wrap!(bi_syscall_stat: nelisp_bi_syscall_stat, (path_ptr: *const Sexp, statbuf: *mut u8) -> i64);
    cc_wrap!(bi_syscall_canonicalize: nelisp_bi_syscall_canonicalize, (path_ptr: *const Sexp, result_buf: *mut u8) -> i64);
    cc_wrap!(bi_nl_write_file: nelisp_bi_nl_write_file, (path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_nl_make_directory: nelisp_bi_nl_make_directory, (path_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_syscall_read_file: nelisp_bi_syscall_read_file, (path_ptr: *const Sexp, buf_ptr: *mut u8, read_size: i64) -> i64);
    cc_wrap!(bi_syscall_resolve_nr: nelisp_bi_syscall_resolve_nr, (sym_ptr: *const Sexp) -> i64);
    cc_wrap!(tty_raw_enter: nelisp_tty_raw_enter, (statbuf: *mut u8) -> i64);
    cc_wrap!(tty_raw_leave: nelisp_tty_raw_leave, (saved_buf: *const u8) -> i64);
    cc_wrap!(tty_stdin_byte_avail: nelisp_tty_stdin_byte_avail, (pfd_buf: *mut u8, timeout_ms: i64) -> i64);
    cc_wrap!(tty_winsize_current: nelisp_tty_winsize_current, (ws_buf: *mut u8) -> i64);
    cc_wrap!(tty_take_atomic: nelisp_tty_take_atomic, (flag_ptr: *mut i64) -> i64);
    pub unsafe fn nl_alloc_consbox_raw() -> *mut u8 { nl_alloc_consbox() }
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
    cc_wrap!(nlboolvector_clone: nelisp_nlboolvector_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlboolvector_drop: nelisp_nlboolvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlchartable_drop: nelisp_nlchartable_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlchartable_clone: nelisp_nlchartable_clone, (box_ptr: *mut i64) -> i64);
    pub unsafe fn mirror_lookup_entry(mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> *const Sexp { nelisp_mirror_lookup_entry(mirror_ptr, sym_ptr) as *const Sexp }
    cc_wrap!(mirror_lookup_value: nelisp_mirror_lookup_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_lookup_function: nelisp_mirror_lookup_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_is_bound: nelisp_mirror_is_bound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_fbound: nelisp_mirror_is_fbound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_constant: nelisp_mirror_is_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_value: nelisp_mirror_clear_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_function: nelisp_mirror_clear_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_constant: nelisp_mirror_set_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, flag_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_install_entry: nelisp_mirror_install_entry, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp) -> i64);
    cc_wrap!(env_shim_op: nelisp_env_shim_op, (op_ptr: *const Sexp, mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp, result_slot: *mut Sexp, vec_scratch: *mut Sexp) -> i64);
    pub fn build_or_insert_scratch_vec(value: Sexp, function: Sexp, plist: Sexp, constant: Sexp) -> Sexp { Sexp::vector(vec![Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Symbol("symbol-entry".into()), Sexp::Nil, value, function, plist, constant]) }
    macro_rules! or_insert_wrap {
        ($pub_name:ident: $ext:ident($mirror:ident, $sym:ident $(, $a:ident: $at:ty)*) => $v:expr, $f:expr, $p:expr, $c:expr) => {
            pub unsafe fn $pub_name($mirror: *const Sexp, $sym: *const Sexp $(, $a: $at)*) -> i64 { let s = build_or_insert_scratch_vec($v, $f, $p, $c); $ext($mirror, $sym, &s as *const Sexp, 0) }
        };
    }
    or_insert_wrap!(mirror_set_value_or_insert: nelisp_mirror_set_value_or_insert(m, s, value_ptr: *const Sexp, unbound_ptr: *const Sexp) => (*value_ptr).clone(), (*unbound_ptr).clone(), Sexp::Nil, Sexp::Nil);
    or_insert_wrap!(mirror_set_function_or_insert: nelisp_mirror_set_function_or_insert(m, s, func_ptr: *const Sexp, unbound_ptr: *const Sexp) => (*unbound_ptr).clone(), (*func_ptr).clone(), Sexp::Nil, Sexp::Nil);
    or_insert_wrap!(mirror_set_constant_or_insert: nelisp_mirror_set_constant_or_insert(m, s, flag_ptr: *const Sexp, unbound_ptr: *const Sexp) => (*unbound_ptr).clone(), (*unbound_ptr).clone(), Sexp::Nil, (*flag_ptr).clone());
    or_insert_wrap!(mirror_install_entry_or_insert: nelisp_mirror_install_entry_or_insert(m, s, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp) => (*value_ptr).clone(), (*function_ptr).clone(), (*plist_ptr).clone(), (*constant_ptr).clone());
    pub unsafe fn frame_stack_ensure_capacity(frames_ptr: *const Sexp, needed: i64) -> i64 { let mut scratch = Sexp::Nil; nelisp_frame_stack_ensure_capacity(frames_ptr, needed, &mut scratch as *mut Sexp) }
    pub unsafe fn frame_stack_install(frames_ptr: *const Sexp, frame_ptr: *const Sexp) -> i64 { let mut s = Sexp::Nil; nelisp_frame_stack_install(frames_ptr, frame_ptr, &mut s) }
    pub unsafe fn wrap_alist_cells(alist_ptr: *const Sexp, result_slot: *mut Sexp) -> i64 { let (mut a, mut b, mut c, mut d) = (Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil);
        let rc = nelisp_wrap_alist_cells(alist_ptr, result_slot, &mut a, &mut b, &mut c, &mut d); for s in [&mut a, &mut b, &mut c, &mut d] { core::ptr::write(s, Sexp::Nil); } rc }
    cc_wrap!(fnv1a: nelisp_fnv1a, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(reader_lex_one: nelisp_reader_lex_one, (str_ptr: *const Sexp, cursor: i64, payload_slot: *mut Sexp, cursor_out_slot: *mut Sexp, scratch_mutstr_slot: *mut Sexp) -> i64);
    cc_wrap!(reader_parse_one: nelisp_reader_parse_one, (str_ptr: *const Sexp, cursor_slot: *mut Sexp, result_slot: *mut Sexp, slot_pool: *const Sexp, depth: i64) -> i64);
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
    cc_wrap!(apply_lambda_inner_call: nl_apply_lambda_inner, (captured: *const Sexp, formals: *const Sexp, body_list: *const Sexp, args_list: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp) -> i64);
    cc_wrap!(bind_formals_impl_call: nl_bind_formals_impl, (formals: *const Sexp, args: *const Sexp, env: *mut std::ffi::c_void, _pad: i64) -> i64);
    cc_wrap!(eval_inner_call: nl_eval_inner, (form: *const Sexp, env: *mut std::ffi::c_void, out: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(fmt_sexp_call: nelisp_fmt_sexp, (s: *const Sexp, slot: *mut Sexp) -> i64);
    cc_wrap!(f64_trunc_impl: nl_bi_f64_trunc_impl, (mode: *const Sexp, num: *const Sexp, den: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(env_lookup_value: nelisp_env_lookup_value, (mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(env_set_value: nelisp_env_set_value, (mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, val_ptr: *const Sexp, scratch_ptr: *const Sexp, _pad: i64) -> i64);
    cc_wrap!(env_lookup_function: nelisp_env_lookup_function, (mirror_ptr: *const Sexp, unbound_ptr: *const Sexp, name_ptr: *const Sexp, out: *mut Sexp) -> i64);
    cc_wrap!(env_shim_set_op: nelisp_env_shim_set_op, (op_ptr: *const Sexp, mirror_ptr: *const Sexp, sym_ptr: *const Sexp, scratch_ptr: *const Sexp, result_slot: *mut Sexp, _pad: i64) -> i64);
    cc_wrap!(env_bind_local: nelisp_env_bind_local, (mirror_ptr: *const Sexp, frames_ptr: *const Sexp, name_ptr: *const Sexp, val_ptr: *const Sexp, scratch_ptr: *const Sexp, _pad: i64) -> i64);
    pub unsafe fn frame_bind(frames_ptr: *const Sexp, name_ptr: *const Sexp, cell_ptr: *const Sexp) -> i64 { let (mut a, mut b, mut c) = (Sexp::Nil, Sexp::Nil, Sexp::Nil); nelisp_frame_bind(frames_ptr, name_ptr, cell_ptr, &mut a, &mut b, &mut c) }
    pub unsafe fn frame_stack_find_raw(frames_ptr: *const Sexp, name_ptr: *const Sexp) -> *const Sexp { nelisp_frame_stack_find(frames_ptr, name_ptr) as *const Sexp }
    #[inline] fn sym(s: &str) -> Sexp { Sexp::Symbol(s.into()) }
    pub unsafe fn frame_push(frames_ptr: *const Sexp) -> i64 { let v = Sexp::vector(vec![sym("nelisp-lexframe"), sym("fast-hash-table"), Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil, Sexp::Nil]); nelisp_frame_push(frames_ptr, &v) }
    pub unsafe fn env_install_empty_globals_frames(globals_out: *mut Sexp, frames_out: *mut Sexp) -> i64 { let v = Sexp::vector(vec![sym("nelisp-env"), sym("fast-hash-table"), sym("nelisp-lexframe-stack"), Sexp::Nil, Sexp::Nil, Sexp::Nil]); nelisp_env_install_empty_globals_frames(globals_out, frames_out, &v, 0) }
}
