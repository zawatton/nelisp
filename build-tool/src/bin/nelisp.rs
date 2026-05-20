use std::process::ExitCode;

use nelisp_build_tool::eval::sexp::Sexp;
use nelisp_build_tool::eval::{apply_function, Env};

fn main() -> ExitCode {
    let mut env = Env::new_global();
    let _ = env.set_value("nelisp--cli-version", Sexp::Str(env!("CARGO_PKG_VERSION").to_string()));
    let argv = Sexp::list_from(&std::env::args().map(Sexp::Str).collect::<Vec<Sexp>>());
    let cli_main = match env.lookup_function("nelisp-cli-main") {
        Ok(f) => f,
        Err(_) => {
            eprintln!("nelisp: bootstrap failed: nelisp-cli-main not loaded");
            return ExitCode::from(2);
        }
    };
    match apply_function(&cli_main, &[argv], &mut env) {
        Ok(Sexp::Int(code)) => ExitCode::from((code as u8).min(255)),
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("nelisp: {}", e);
            ExitCode::from(1)
        }
    }
}
