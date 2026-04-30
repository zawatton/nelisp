//! End-user `nelisp` CLI — plain Elisp launcher.
//!
//! Phase 8.x (post-v1.0 doc-driven): the standalone tarball needs a
//! Lisp-y entry point that does not assume any of anvil's conventions
//! so the README can document `nelisp ...` independently of any
//! downstream consumer.  This binary is intentionally thin — it
//! delegates everything to the existing `eval_str' / `eval_str_all'
//! Rust API.
//!
//! Stage 9a-9g course correction (2026-04-30, Sweeps 1-4): the
//! mint-* command handlers (`mint-int-as-code', `mint-int-via-
//! emitted-load', `mint-int-plus-imm', `mint-chain', `mint-from-
//! ast') were removed — their logic was Elisp-misplaced into Rust.
//! Replacement: `lisp/doc47-mint.el' driven via `emacs --batch'.
//!
//! Sweep 5 (2026-04-30): the surviving Stage 6/7a/8 mint
//! handlers (`mint-list-from-source', `mint-string-from-source',
//! `mint-symbol-from-source', `mint-eval-result', `mint-eval-file')
//! were also removed for the same reason — their lowering /
//! orchestration is Elisp-portable.  The `lisp/doc47-mint.el'
//! Stage-9a-style precompute path subsumes their use cases (= the
//! Elisp side computes the boot-exit value at build time and emits
//! a constant-return code segment).
//!
//! Surviving CLI surface — REPL-style only:
//!   --version                   print version + exit
//!   eval EXPR                   evaluate EXPR via the Doc 44 §3.2
//!                               minimal evaluator + print the result
//!   -l FILE                     load + eval FILE (every form, last
//!                               value printed)
//!   -                           read from stdin and eval
//!   compile-image SRC IMG       Doc 47 §3.1 phase 4 walking skeleton
//!                               — read SRC (Elisp source), encode as
//!                               IMG (NELIMG\\0\\x01 sexp image)
//!   eval-image IMG              decode IMG and evaluate, print the
//!                               last value
//!
//! The `eval/' + `reader/' + `bridge/' Rust modules are retained as
//! the boot-interpreter substrate (= principle (d) of memory
//! `feedback_rust_core_minimization_principle'); they are needed
//! both by the REPL surfaces here AND by the `anvil-runtime'
//! consumer crate that minted images depend on at boot.
//!
//! Sweep 6 (2026-04-30): the older `nelisp_runtime::image::*' native-
//! code-in-image subsystem (~2,420 LOC of NlImage v1 + reloc + heap +
//! boot) was deleted.  The new `image' module under this crate is a
//! 314-LOC walking skeleton that operates on real `Sexp' values and
//! reuses the existing `eval/' + `reader/' modules — no second object
//! model, no native code emit — proving Doc 47 §3.1 phase 4 in pure
//! interpreter terms.

use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::process::ExitCode;

use nelisp_build_tool::eval::{eval_str, eval_str_all};
use nelisp_build_tool::image;
use nelisp_build_tool::reader::fmt_sexp;

const USAGE: &str = "usage: nelisp --version
       nelisp eval EXPR                 # evaluate EXPR and print the result
       nelisp -l FILE                   # load FILE and print the last result
       nelisp -                         # read from stdin and print the last result
       nelisp compile-image SRC IMG     # Doc 47 phase 4 walking skeleton
       nelisp eval-image IMG            # decode IMG and evaluate, print result

Note: Doc 47 image mint commands (mint-list-from-source, mint-eval-*,
mint-int-*, mint-chain, mint-from-ast) were removed in the 2026-04-30
Elisp-pivot rollback.  The 2,420-LOC `nelisp_runtime::image' native-
code subsystem was likewise retired in Sweep 6 (2026-04-30) in favour
of the simpler walking-skeleton image format above (`compile-image' /
`eval-image'), which serializes real `Sexp' values and lets the
existing `eval/' module evaluate them — no second object model.";

#[derive(Debug)]
enum Command {
    Version,
    Eval(String),
    LoadFile(String),
    LoadStdin,
    CompileImage { src: String, image: String },
    EvalImage(String),
}

fn parse_args<I, S>(args: I) -> Result<Command, String>
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let argv: Vec<String> = args.into_iter().map(|s| s.as_ref().to_string()).collect();
    match argv.as_slice() {
        [_, flag] if flag == "--version" || flag == "-V" => Ok(Command::Version),
        [_, mode, expr] if mode == "eval" => Ok(Command::Eval(expr.clone())),
        [_, flag, path] if flag == "-l" || flag == "--load" => Ok(Command::LoadFile(path.clone())),
        [_, flag] if flag == "-" => Ok(Command::LoadStdin),
        [_, mode, src, image] if mode == "compile-image" => Ok(Command::CompileImage {
            src: src.clone(),
            image: image.clone(),
        }),
        [_, mode, image] if mode == "eval-image" => Ok(Command::EvalImage(image.clone())),
        _ => Err(USAGE.to_string()),
    }
}

fn main() -> ExitCode {
    let command = match parse_args(std::env::args()) {
        Ok(c) => c,
        Err(usage) => {
            eprintln!("{}", usage);
            return ExitCode::from(2);
        }
    };

    match command {
        Command::Version => {
            println!("nelisp {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
        Command::Eval(expr) => run_eval(&expr),
        Command::LoadFile(path) => match fs::read_to_string(Path::new(&path)) {
            Ok(s) => run_eval_all(&s),
            Err(e) => {
                eprintln!("nelisp: cannot read {}: {}", path, e);
                ExitCode::from(1)
            }
        },
        Command::LoadStdin => {
            let mut buf = String::new();
            if let Err(e) = io::stdin().read_to_string(&mut buf) {
                eprintln!("nelisp: stdin read error: {}", e);
                return ExitCode::from(1);
            }
            run_eval_all(&buf)
        }
        Command::CompileImage { src, image } => run_compile_image(&src, &image),
        Command::EvalImage(image) => run_eval_image(&image),
    }
}

fn run_compile_image(src_path: &str, image_path: &str) -> ExitCode {
    let source = match fs::read_to_string(Path::new(src_path)) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: cannot read {}: {}", src_path, e);
            return ExitCode::from(1);
        }
    };
    let bytes = match image::compile_elisp_to_image(&source) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("nelisp: compile-image: {}", e);
            return ExitCode::from(7);
        }
    };
    if let Err(e) = fs::write(Path::new(image_path), bytes) {
        eprintln!("nelisp: cannot write {}: {}", image_path, e);
        return ExitCode::from(1);
    }
    ExitCode::SUCCESS
}

fn run_eval_image(image_path: &str) -> ExitCode {
    let bytes = match fs::read(Path::new(image_path)) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("nelisp: cannot read {}: {}", image_path, e);
            return ExitCode::from(1);
        }
    };
    match image::eval_image(&bytes) {
        Ok(value) => {
            println!("{}", fmt_sexp(&value));
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: eval-image: {}", e);
            ExitCode::from(7)
        }
    }
}

fn run_eval(input: &str) -> ExitCode {
    match eval_str(input) {
        Ok(value) => {
            println!("{}", fmt_sexp(&value));
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            ExitCode::from(1)
        }
    }
}

fn run_eval_all(input: &str) -> ExitCode {
    match eval_str_all(input) {
        Ok(value) => {
            println!("{}", fmt_sexp(&value));
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            ExitCode::from(1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_args, Command};

    #[test]
    fn parses_version() {
        assert!(matches!(parse_args(["nelisp", "--version"]).unwrap(), Command::Version));
        assert!(matches!(parse_args(["nelisp", "-V"]).unwrap(), Command::Version));
    }

    #[test]
    fn parses_eval() {
        match parse_args(["nelisp", "eval", "(+ 1 2)"]).unwrap() {
            Command::Eval(s) => assert_eq!(s, "(+ 1 2)"),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_load_file() {
        match parse_args(["nelisp", "-l", "foo.el"]).unwrap() {
            Command::LoadFile(s) => assert_eq!(s, "foo.el"),
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_stdin() {
        assert!(matches!(parse_args(["nelisp", "-"]).unwrap(), Command::LoadStdin));
    }

    #[test]
    fn rejects_unknown() {
        assert!(parse_args(["nelisp", "frobnicate"]).is_err());
        assert!(parse_args(["nelisp", "mint-list-from-source", "(1)", "/tmp/x.bin"]).is_err());
        assert!(parse_args(["nelisp", "mint-eval-file", "boot.el", "/tmp/b.bin"]).is_err());
    }

    #[test]
    fn parses_compile_image() {
        match parse_args(["nelisp", "compile-image", "boot.el", "/tmp/img.bin"]).unwrap() {
            Command::CompileImage { src, image } => {
                assert_eq!(src, "boot.el");
                assert_eq!(image, "/tmp/img.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_eval_image() {
        match parse_args(["nelisp", "eval-image", "/tmp/img.bin"]).unwrap() {
            Command::EvalImage(p) => assert_eq!(p, "/tmp/img.bin"),
            other => panic!("unexpected: {:?}", other),
        }
    }
}
