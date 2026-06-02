use std::process::ExitCode; use nelisp_build_tool::eval::sexp::Sexp; use nelisp_build_tool::eval::Env;
fn main() -> ExitCode {
    let mut env = Env::new_global();
    let _ = env.set_value("nelisp--cli-version", Sexp::Str(env!("CARGO_PKG_VERSION").to_string()));
    let mut ctx_slot = Sexp::Nil;
    unsafe { nelisp_build_tool::elisp_cc_spike::eval_ctx_make(&env.globals_record as *const Sexp, &env.frames_record as *const Sexp, &env.unbound_marker as *const Sexp, 1024, &mut ctx_slot as *mut Sexp); }
    let ctx = match ctx_slot { Sexp::Int(p) => p as usize as *mut std::ffi::c_void, _ => { eprintln!("nelisp: ctx build failed"); return ExitCode::from(2); } };
    let argv = Sexp::list_from(&std::env::args().map(Sexp::Str).collect::<Vec<Sexp>>());
    let args_list = Sexp::list_from(&[argv]);
    let f = match env.lookup_function("nelisp-cli-main") { Ok(f) => f, Err(_) => { eprintln!("nelisp: bootstrap failed: nelisp-cli-main not loaded"); return ExitCode::from(2); } };
    let mut out = Sexp::Nil;
    let rc = unsafe { nelisp_build_tool::elisp_cc_spike::apply_function_ctx(&f as *const Sexp, &args_list as *const Sexp, ctx, &mut out as *mut Sexp) };
    if rc == 0 { match out { Sexp::Int(code) => ExitCode::from((code as u8).min(255)), _ => ExitCode::SUCCESS } } else { eprintln!("nelisp: eval error"); ExitCode::from(1) }
}
