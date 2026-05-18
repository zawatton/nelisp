//! End-user `nelisp` CLI — minimal bootstrap stub (Doc 128).
//!
//! All CLI parsing + sub-command dispatch lives in
//! `lisp/nelisp-cli.el::nelisp-cli-main'.  This binary's only job is
//! to bring up the global eval environment, hand the (Rust-owned)
//! argv across the boundary as a Lisp list of strings, and translate
//! the elisp integer exit code into a `std::process::ExitCode'.
//!
//! Pre-Doc-128 this file was ~209 LOC of `Command' enum + match +
//! per-subcommand helpers; the move keeps the CLI surface (= every
//! flag and behaviour) identical while shrinking the Rust side to a
//! pure transport layer.

use std::process::ExitCode;

use nelisp_build_tool::eval::sexp::Sexp;
use nelisp_build_tool::eval::{apply_function, Env};

fn main() -> ExitCode {
    let mut env = Env::new_global();
    // Seed `nelisp--cli-version' for `nelisp-cli-main' (= the elisp
    // side has no `CARGO_PKG_VERSION' equivalent).
    let _ = env.set_value(
        "nelisp--cli-version",
        Sexp::Str(env!("CARGO_PKG_VERSION").to_string()),
    );
    let argv = Sexp::list_from(
        &std::env::args()
            .map(Sexp::Str)
            .collect::<Vec<Sexp>>(),
    );
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
