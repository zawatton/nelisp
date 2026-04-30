//! End-user `nelisp` CLI — plain Elisp launcher.
//!
//! Phase 8.x (post-v1.0 doc-driven): the standalone tarball needs a
//! Lisp-y entry point that does not assume any of anvil's conventions
//! so the README can document `nelisp ...` independently of any
//! downstream consumer.  This binary is intentionally thin — it
//! delegates everything to the existing `eval_str` / `eval_str_all`
//! Rust API.

use std::fs;
use std::io::{self, Read};
use std::path::Path;
use std::process::ExitCode;

use nelisp_build_tool::eval::{eval_str, eval_str_all};
use nelisp_build_tool::reader::fmt_sexp;

const USAGE: &str = "usage: nelisp --version
       nelisp eval EXPR        # evaluate EXPR and print the result
       nelisp -l FILE          # load FILE and print the last result
       nelisp -                 # read from stdin and print the last result";

#[derive(Debug)]
enum Command {
    Version,
    Eval(String),
    LoadFile(String),
    LoadStdin,
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
    }
}
