//! End-user `nelisp` CLI — plain Elisp launcher.
//!
//! Thin driver over the `eval_str' / `eval_str_all' Rust API plus
//! the walking-skeleton `image' module.  The binary intentionally
//! holds no policy: every CLI subcommand below maps to one library
//! call so the README can document `nelisp ...` independently of
//! any downstream consumer (anvil-runtime, elisp-lsp, etc.).
//!
//! Surviving CLI surface — REPL-style only:
//!   --version, -V               print version + exit
//!   eval EXPR                   evaluate EXPR and print the result
//!   -l, --load FILE             load + eval FILE, print the last value
//!   exec FILE                   load + eval FILE silently (no final-
//!                               value print — for stdio servers)
//!   -                           read from stdin and eval
//!   eval-image IMG              decode IMG (NELIMG\\0\\x01 sexp
//!                               image) and evaluate, print the last
//!                               value
//!
//! Architecture invariants (post Doc 28 §3.6 cluster takeover +
//! §3.7.b Cranelift全削除):
//! - The `eval/' + `reader/' + `bridge/' + `image/' Rust modules are
//!   the boot-interpreter substrate.  All hot paths beyond the boot
//!   interpreter are emitted by the elisp-side `nelisp-cc' native
//!   compiler; this binary never references the JIT directly.
//! - `compile-image' is feature-gated behind `image-baker' and lives
//!   in the dev-tooling `nelisp-baker' binary (= `make bake-images'
//!   / `cargo run --bin nelisp-baker --features image-baker').  The
//!   production `nelisp' binary deliberately does not parse it.

use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::process::ExitCode;

use nelisp_build_tool::eval::{eval_str, eval_str_all};
use nelisp_build_tool::image;
use nelisp_build_tool::eval::sexp::fmt_sexp;

const USAGE: &str = "usage: nelisp --version
       nelisp eval EXPR                 # evaluate EXPR and print the result
       nelisp -l FILE                   # load FILE and print the last result
       nelisp exec FILE                 # load FILE silently (no final-value print)
       nelisp -                         # read from stdin and print the last result
       nelisp eval-image IMG            # decode IMG and evaluate, print result

Note: `compile-image SRC IMG' lives in the dev-tooling `nelisp-baker'
binary behind the `image-baker' feature (= `make bake-images' /
`cargo run --bin nelisp-baker --features image-baker').";

#[derive(Debug)]
enum Command {
    Version,
    Eval(String),
    LoadFile(String),
    /// Like LoadFile but suppresses the final-value `println!`.  Required
    /// for stdio servers (e.g. elisp-lsp) where any byte after the last
    /// reply corrupts the wire.  Errors still go to stderr and set exit 1.
    ExecFile(String),
    LoadStdin,
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
        [_, mode, path] if mode == "exec" => Ok(Command::ExecFile(path.clone())),
        [_, flag] if flag == "-" => Ok(Command::LoadStdin),
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
        Command::ExecFile(path) => match fs::read_to_string(Path::new(&path)) {
            Ok(s) => exec_eval_all(&s),
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
        Command::EvalImage(image) => run_eval_image(&image),
    }
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

/// Same as `run_eval_all' but suppresses the final-value print.
/// Used by `nelisp exec FILE' for stdio-protocol servers (elisp-lsp,
/// future REPLs over stdio) where any byte after the last protocol
/// reply corrupts the wire.  Errors still flow to stderr / exit 1.
fn exec_eval_all(input: &str) -> ExitCode {
    match eval_str_all(input) {
        Ok(_) => ExitCode::SUCCESS,
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
    fn parses_exec_file() {
        match parse_args(["nelisp", "exec", "server.el"]).unwrap() {
            Command::ExecFile(s) => assert_eq!(s, "server.el"),
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
    fn rejects_compile_image() {
        // `compile-image' lives in the dev-tooling `nelisp-baker' bin
        // under the `image-baker' feature; the production `nelisp'
        // binary deliberately does not parse it.
        assert!(parse_args(["nelisp", "compile-image", "boot.el", "/tmp/img.bin"]).is_err());
    }

    #[test]
    fn parses_eval_image() {
        match parse_args(["nelisp", "eval-image", "/tmp/img.bin"]).unwrap() {
            Command::EvalImage(p) => assert_eq!(p, "/tmp/img.bin"),
            other => panic!("unexpected: {:?}", other),
        }
    }
}
