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
use nelisp_build_tool::image_lowering::lower_to_heap;
use nelisp_build_tool::reader::{fmt_sexp, read_str, Sexp};
use nelisp_runtime::image::{
    write_image_with_heap_code_and_relocs, HAS_NATIVE_LIST_LENGTH,
    HAS_NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC, HAS_NATIVE_LOAD_HEAP_INT_UNTAG,
    HAS_NATIVE_LOAD_HEAP_STRING_LEN, HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN, NATIVE_LIST_LENGTH,
    NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC, NATIVE_LOAD_HEAP_INT_UNTAG, NATIVE_LOAD_HEAP_STRING_LEN,
    NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
};

const USAGE: &str = "usage: nelisp --version
       nelisp eval EXPR        # evaluate EXPR and print the result
       nelisp -l FILE          # load FILE and print the last result
       nelisp -                 # read from stdin and print the last result
       nelisp mint-list-from-source SRC OUT    # Stage 6d: read SRC, lower to image at OUT
       nelisp mint-string-from-source SRC OUT  # Stage 6e: SRC must read as a string literal
       nelisp mint-symbol-from-source SRC OUT  # Stage 6e: SRC must read as a symbol
       nelisp mint-eval-result SRC OUT         # Stage 7a: evaluate SRC, lower result, auto-pick asset";

#[derive(Debug)]
enum Command {
    Version,
    Eval(String),
    LoadFile(String),
    LoadStdin,
    /// Doc 47 Stage 6d — read SRC with the build-tool reader, lower
    /// the resulting Sexp to a NlImage v1 heap + reloc table, write
    /// the image at OUT alongside the canned `NATIVE_LIST_LENGTH`
    /// asset.  Boot exits with the list length.
    MintListFromSource { src: String, out: String },
    /// Doc 47 Stage 6e — same lowering pipeline; SRC must read as a
    /// string literal.  Image bundles `NATIVE_LOAD_HEAP_STRING_LEN'
    /// so boot exits with the byte length.
    MintStringFromSource { src: String, out: String },
    /// Doc 47 Stage 6e — SRC must read as a symbol; image bundles
    /// `NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN' so boot exits with the
    /// symbol name's byte length.
    MintSymbolFromSource { src: String, out: String },
    /// Doc 47 Stage 7a — read + *evaluate* SRC via the build-tool
    /// minimal interpreter, lower the resulting `Sexp' value to a
    /// NlImage v1 heap + reloc table, and pick the appropriate
    /// per-shape native asset (Int → load_heap_int_untag, Cons/Nil
    /// → list_length, Str → load_heap_string_len, Symbol →
    /// load_heap_symbol_name_len) so the seed boots with no
    /// evaluator linked in.  This is the first surface where
    /// build-tool's evaluator output flows into a Doc 47-spec
    /// binary image.
    MintEvalResult { src: String, out: String },
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
        [_, mode, src, out] if mode == "mint-list-from-source" => {
            Ok(Command::MintListFromSource {
                src: src.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, out] if mode == "mint-string-from-source" => {
            Ok(Command::MintStringFromSource {
                src: src.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, out] if mode == "mint-symbol-from-source" => {
            Ok(Command::MintSymbolFromSource {
                src: src.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, out] if mode == "mint-eval-result" => {
            Ok(Command::MintEvalResult {
                src: src.clone(),
                out: out.clone(),
            })
        }
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
        Command::MintListFromSource { src, out } => {
            run_mint_with_asset(
                &src,
                &out,
                "list-from-source",
                NATIVE_LIST_LENGTH,
                HAS_NATIVE_LIST_LENGTH,
            )
        }
        Command::MintStringFromSource { src, out } => {
            run_mint_with_asset(
                &src,
                &out,
                "string-from-source",
                NATIVE_LOAD_HEAP_STRING_LEN,
                HAS_NATIVE_LOAD_HEAP_STRING_LEN,
            )
        }
        Command::MintSymbolFromSource { src, out } => {
            run_mint_with_asset(
                &src,
                &out,
                "symbol-from-source",
                NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
                HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
            )
        }
        Command::MintEvalResult { src, out } => run_mint_eval_result(&src, &out),
    }
}

/// Pick the matching seed-side native asset for a Sexp result.  Each
/// asset assumes a specific heap shape that the lowering produces:
///
///   `Sexp::Int(_)`     → 8-byte heap with `tag_int(n)` immediate
///   `Sexp::Nil`        → 8-byte heap with NL_VALUE_TAG_NIL immediate
///   `Sexp::Cons(_, _)` → head ptr + cell chain terminated by NIL
///   `Sexp::Str(_)`     → head ptr + length-prefixed string struct
///   `Sexp::Symbol(_)`  → head ptr + symbol struct + name string
///
/// Returns `(asset_bytes, has_asset_flag, human_label)' or a
/// String error pinpointing which Sexp variant Stage 7a does not
/// know how to boot yet.
fn pick_asset_for_eval_result(
    result: &Sexp,
) -> Result<(&'static [u8], bool, &'static str), String> {
    match result {
        // Sexp::T joins Sexp::Int on NATIVE_LOAD_HEAP_INT_UNTAG: T is
        // encoded as `(1 << 3) | NIL_TAG' = 11, so `sar 3' produces
        // 1 (= boolean true exit code).  NIL → list_length still
        // returns 0 (boolean false exit code).
        Sexp::Int(_) | Sexp::T => Ok((
            NATIVE_LOAD_HEAP_INT_UNTAG,
            HAS_NATIVE_LOAD_HEAP_INT_UNTAG,
            "int-or-t",
        )),
        Sexp::Nil | Sexp::Cons(_, _) => {
            Ok((NATIVE_LIST_LENGTH, HAS_NATIVE_LIST_LENGTH, "list"))
        }
        Sexp::Str(_) => Ok((
            NATIVE_LOAD_HEAP_STRING_LEN,
            HAS_NATIVE_LOAD_HEAP_STRING_LEN,
            "string",
        )),
        Sexp::Symbol(_) => Ok((
            NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
            HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
            "symbol",
        )),
        Sexp::Float(_) => Ok((
            NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC,
            HAS_NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC,
            "float-trunc",
        )),
        Sexp::Vector(_) => Err("Stage 7b-2 does not yet boot a Vector result".into()),
    }
}

fn run_mint_eval_result(src: &str, out: &str) -> ExitCode {
    let result = match eval_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            return ExitCode::from(2);
        }
    };
    let (asset, has_asset, label) = match pick_asset_for_eval_result(&result) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("nelisp: {}", e);
            return ExitCode::from(3);
        }
    };
    if !has_asset {
        eprintln!(
            "nelisp: mint-eval-result: {} asset unavailable on this arch",
            label
        );
        return ExitCode::from(14);
    }
    let (heap, relocs) = match lower_to_heap(&result) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: lowering failed for eval result: {}", e);
            return ExitCode::from(3);
        }
    };
    match write_image_with_heap_code_and_relocs(out, asset, &heap, &relocs) {
        Ok(()) => {
            println!(
                "minted eval-result NlImage at {} (eval={}, asset={}, heap_size={}, reloc_count={})",
                out,
                fmt_sexp(&result),
                label,
                heap.len(),
                relocs.len()
            );
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: image write error: {}", e);
            ExitCode::from(4)
        }
    }
}

fn run_mint_with_asset(
    src: &str,
    out: &str,
    label: &str,
    asset: &[u8],
    has_asset: bool,
) -> ExitCode {
    if !has_asset {
        eprintln!("nelisp: mint-{}: native asset unavailable on this arch", label);
        return ExitCode::from(14);
    }
    let sexp = match read_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: read error: {}", e);
            return ExitCode::from(2);
        }
    };
    let (heap, relocs) = match lower_to_heap(&sexp) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: {}", e);
            return ExitCode::from(3);
        }
    };
    match write_image_with_heap_code_and_relocs(out, asset, &heap, &relocs) {
        Ok(()) => {
            println!(
                "minted {} NlImage at {} (heap_size={} reloc_count={})",
                label,
                out,
                heap.len(),
                relocs.len()
            );
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: image write error: {}", e);
            ExitCode::from(4)
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

    #[test]
    fn parses_mint_list_from_source() {
        match parse_args(["nelisp", "mint-list-from-source", "(1 2 3)", "/tmp/x.bin"]).unwrap() {
            Command::MintListFromSource { src, out } => {
                assert_eq!(src, "(1 2 3)");
                assert_eq!(out, "/tmp/x.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_string_and_symbol() {
        match parse_args(["nelisp", "mint-string-from-source", "\"hi\"", "/tmp/s.bin"]).unwrap() {
            Command::MintStringFromSource { src, out } => {
                assert_eq!(src, "\"hi\"");
                assert_eq!(out, "/tmp/s.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
        match parse_args(["nelisp", "mint-symbol-from-source", "foo", "/tmp/y.bin"]).unwrap() {
            Command::MintSymbolFromSource { src, out } => {
                assert_eq!(src, "foo");
                assert_eq!(out, "/tmp/y.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_eval_result() {
        match parse_args(["nelisp", "mint-eval-result", "(+ 1 2)", "/tmp/r.bin"]).unwrap() {
            Command::MintEvalResult { src, out } => {
                assert_eq!(src, "(+ 1 2)");
                assert_eq!(out, "/tmp/r.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn pick_asset_classifies_each_supported_shape() {
        use super::pick_asset_for_eval_result;
        use nelisp_build_tool::reader::Sexp;

        assert_eq!(
            pick_asset_for_eval_result(&Sexp::Int(7)).unwrap().2,
            "int-or-t"
        );
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::T).unwrap().2,
            "int-or-t",
            "Stage 7b-1 wires T through the same INT_UNTAG asset"
        );
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::Nil).unwrap().2,
            "list"
        );
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::cons(Sexp::Int(1), Sexp::Nil)).unwrap().2,
            "list"
        );
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::Str("hi".into())).unwrap().2,
            "string"
        );
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::Symbol("foo".into())).unwrap().2,
            "symbol"
        );
    }

    #[test]
    fn pick_asset_includes_float() {
        use super::pick_asset_for_eval_result;
        use nelisp_build_tool::reader::Sexp;
        assert_eq!(
            pick_asset_for_eval_result(&Sexp::Float(3.14)).unwrap().2,
            "float-trunc"
        );
    }

    #[test]
    fn pick_asset_rejects_vector_with_clear_message() {
        use super::pick_asset_for_eval_result;
        use nelisp_build_tool::reader::Sexp;

        let v = Sexp::Vector(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        let err = pick_asset_for_eval_result(&v).unwrap_err();
        assert!(
            err.contains("Vector"),
            "error {:?} should mention Vector",
            err
        );
    }
}
