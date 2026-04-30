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

use nelisp_build_tool::eval::{eval_str, eval_str_all, eval_str_all_at_path};
use nelisp_build_tool::image_lowering::lower_to_heap;
use nelisp_build_tool::native_emit::{
    compose, emit_int_plus_imm, emit_load_heap_int_untag, emit_return_i32, ChainOp,
    HAS_CHAIN_OP_COMPOSE, HAS_EMIT_INT_PLUS_IMM, HAS_EMIT_LOAD_HEAP_INT_UNTAG,
    HAS_EMIT_RETURN_I32,
};
use nelisp_build_tool::reader::{fmt_sexp, read_str, Sexp};
use nelisp_runtime::image::{
    write_image_with_heap_code_and_relocs, write_image_with_native_entry, HAS_NATIVE_LIST_LENGTH,
    HAS_NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC, HAS_NATIVE_LOAD_HEAP_INT_UNTAG,
    HAS_NATIVE_LOAD_HEAP_STRING_LEN, HAS_NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
    HAS_NATIVE_LOAD_HEAP_VECTOR_LEN, NATIVE_LIST_LENGTH, NATIVE_LOAD_HEAP_FLOAT_INT_TRUNC,
    NATIVE_LOAD_HEAP_INT_UNTAG, NATIVE_LOAD_HEAP_STRING_LEN, NATIVE_LOAD_HEAP_SYMBOL_NAME_LEN,
    NATIVE_LOAD_HEAP_VECTOR_LEN,
};

const USAGE: &str = "usage: nelisp --version
       nelisp eval EXPR        # evaluate EXPR and print the result
       nelisp -l FILE          # load FILE and print the last result
       nelisp -                 # read from stdin and print the last result
       nelisp mint-list-from-source SRC OUT    # Stage 6d: read SRC, lower to image at OUT
       nelisp mint-string-from-source SRC OUT  # Stage 6e: SRC must read as a string literal
       nelisp mint-symbol-from-source SRC OUT  # Stage 6e: SRC must read as a symbol
       nelisp mint-eval-result SRC OUT         # Stage 7a: evaluate SRC, lower result, auto-pick asset
       nelisp mint-eval-file SRC-FILE OUT      # Stage 8: read FILE as a sequence of forms, evaluate, lower last value
       nelisp mint-int-as-code SRC OUT         # Stage 9a: evaluate SRC (must be Int), emit native return-i32 code
       nelisp mint-int-via-emitted-load SRC OUT # Stage 9b: evaluate SRC, lower Int to heap, emit load-heap asm (vs pre-baked asset)
       nelisp mint-int-plus-imm SRC IMM OUT     # Stage 9c: chain load+add asm; boot exits (eval-of-SRC) + IMM
       nelisp mint-chain SRC OPS OUT            # Stage 9d: SRC -> heap; OPS = ChainOp spec ('add:5 mul:2 neg' etc)";

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
    /// Doc 47 Stage 8 — like `mint-eval-result' but reads SRC-FILE
    /// as a *sequence* of top-level forms (= `progn` semantics) and
    /// lowers the *last* form's value.  Used to bake real `.el'
    /// fixtures end-to-end.  Fails with the evaluator's normal
    /// error if any form throws.
    MintEvalFile { src_file: String, out: String },
    /// Doc 47 Stage 9a — evaluate SRC (must reduce to an integer
    /// value), then emit a native return-i32 function body via
    /// `native_emit::emit_return_i32' and write an image whose code
    /// segment IS that body.  No heap, no relocs — the entire
    /// program lives in the code segment as a few bytes of asm.
    /// First step toward Doc 47 §3.1 phase 7 (Phase 7 native arena).
    MintIntAsCode { src: String, out: String },
    /// Doc 47 Stage 9b — evaluate SRC (must reduce to an integer),
    /// lower the value to a tagged-int heap word (= Stage 7a path),
    /// and write an image whose code segment is the
    /// `emit_load_heap_int_untag' bytes the build-tool itself
    /// produces, *not* the runtime's pre-baked
    /// `NATIVE_LOAD_HEAP_INT_UNTAG' constant.  Both paths must produce
    /// byte-identical code segments and exit with the same int — that
    /// equivalence is the walking-skeleton parity gate for Stage 9c
    /// (closure body compilation).
    MintIntViaEmittedLoad { src: String, out: String },
    /// Doc 47 Stage 9c — chain composition demo.  Evaluates SRC to an
    /// integer (becomes the heap word the boot reads), parses IMM as
    /// a literal i32 (gets baked into the asm immediate), then emits
    /// `emit_load_heap_int_untag_head' + `emit_add_rax_imm32(IMM)' +
    /// `emit_ret' chained into a single function body.  Boot exits
    /// with `eval(SRC) + IMM' — the smallest closure-body shape that
    /// proves the build-tool composes asm building blocks rather than
    /// just emitting one canned shape.
    MintIntPlusImm {
        src: String,
        imm: String,
        out: String,
    },
    /// Doc 47 Stage 9d — generic chain composer.  Evaluates SRC to an
    /// integer (heap word), parses OPS as a whitespace-separated spec
    /// of `ChainOp' values, and emits the composed function body via
    /// [`native_emit::compose`].  Op spec grammar:
    ///   add:N | sub:N | mul:N    (N = signed decimal i32)
    ///   neg
    /// `LoadHeapHead' is implicit at the start; `Ret' is implicit at
    /// the end.  The driver appends them so the spec only describes
    /// the transformation chain.
    MintChain {
        src: String,
        ops: String,
        out: String,
    },
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
        [_, mode, src_file, out] if mode == "mint-eval-file" => {
            Ok(Command::MintEvalFile {
                src_file: src_file.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, out] if mode == "mint-int-as-code" => {
            Ok(Command::MintIntAsCode {
                src: src.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, out] if mode == "mint-int-via-emitted-load" => {
            Ok(Command::MintIntViaEmittedLoad {
                src: src.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, imm, out] if mode == "mint-int-plus-imm" => {
            Ok(Command::MintIntPlusImm {
                src: src.clone(),
                imm: imm.clone(),
                out: out.clone(),
            })
        }
        [_, mode, src, ops, out] if mode == "mint-chain" => {
            Ok(Command::MintChain {
                src: src.clone(),
                ops: ops.clone(),
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
        Command::MintEvalFile { src_file, out } => {
            match fs::read_to_string(Path::new(&src_file)) {
                Ok(s) => run_mint_eval_all(&s, &out, &src_file),
                Err(e) => {
                    eprintln!("nelisp: cannot read {}: {}", src_file, e);
                    ExitCode::from(1)
                }
            }
        }
        Command::MintIntAsCode { src, out } => run_mint_int_as_code(&src, &out),
        Command::MintIntViaEmittedLoad { src, out } => {
            run_mint_int_via_emitted_load(&src, &out)
        }
        Command::MintIntPlusImm { src, imm, out } => {
            run_mint_int_plus_imm(&src, &imm, &out)
        }
        Command::MintChain { src, ops, out } => run_mint_chain(&src, &ops, &out),
    }
}

fn run_mint_int_as_code(src: &str, out: &str) -> ExitCode {
    if !HAS_EMIT_RETURN_I32 {
        eprintln!("nelisp: mint-int-as-code: native_emit unavailable on this arch");
        return ExitCode::from(14);
    }
    let result = match eval_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            return ExitCode::from(2);
        }
    };
    let n = match result {
        Sexp::Int(n) => n,
        other => {
            eprintln!(
                "nelisp: mint-int-as-code: expression evaluated to {:?}, want Int",
                fmt_sexp(&other)
            );
            return ExitCode::from(3);
        }
    };
    // i64 → i32 truncation; users picking values outside i32 range
    // see the modular wrap (matches the seed's exit-code semantics).
    let value = n as i32;
    let code = emit_return_i32(value);
    match write_image_with_native_entry(out, &code) {
        Ok(()) => {
            println!(
                "minted int-as-code NlImage at {} (eval={}, value_i32={}, code_size={})",
                out,
                n,
                value,
                code.len()
            );
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("nelisp: image write error: {}", e);
            ExitCode::from(4)
        }
    }
}

fn run_mint_int_via_emitted_load(src: &str, out: &str) -> ExitCode {
    if !HAS_EMIT_LOAD_HEAP_INT_UNTAG {
        eprintln!(
            "nelisp: mint-int-via-emitted-load: native_emit unavailable on this arch"
        );
        return ExitCode::from(14);
    }
    let result = match eval_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            return ExitCode::from(2);
        }
    };
    if !matches!(result, Sexp::Int(_)) {
        eprintln!(
            "nelisp: mint-int-via-emitted-load: expression evaluated to {:?}, want Int",
            fmt_sexp(&result)
        );
        return ExitCode::from(3);
    }
    // Lower the int the same way Stage 7a does — produces an 8-byte
    // heap word + zero relocs (the int's tagged form is written
    // directly into slot 0 with no pointer fixup).
    let (heap, relocs) = match lower_to_heap(&result) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: lowering failed: {}", e);
            return ExitCode::from(3);
        }
    };
    // Emit the load-heap-int-untag function body from the build-tool.
    // The bytes must be byte-identical with the runtime's pre-baked
    // NATIVE_LOAD_HEAP_INT_UNTAG (asserted by unit test in
    // `native_emit::tests'); we *also* assert the equality here at
    // mint time so a regression at runtime/build-tool boundary is
    // caught before the image is written rather than at boot.
    let code = emit_load_heap_int_untag();
    if HAS_NATIVE_LOAD_HEAP_INT_UNTAG && code.as_slice() != NATIVE_LOAD_HEAP_INT_UNTAG {
        eprintln!(
            "nelisp: mint-int-via-emitted-load: emitted code differs from \
             runtime asset (emitted={} bytes, asset={} bytes)",
            code.len(),
            NATIVE_LOAD_HEAP_INT_UNTAG.len()
        );
        return ExitCode::from(5);
    }
    match write_image_with_heap_code_and_relocs(out, &code, &heap, &relocs) {
        Ok(()) => {
            println!(
                "minted int-via-emitted-load NlImage at {} (eval={}, code_size={}, heap_size={}, reloc_count={})",
                out,
                fmt_sexp(&result),
                code.len(),
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

fn run_mint_int_plus_imm(src: &str, imm_str: &str, out: &str) -> ExitCode {
    if !HAS_EMIT_INT_PLUS_IMM {
        eprintln!("nelisp: mint-int-plus-imm: native_emit unavailable on this arch");
        return ExitCode::from(14);
    }
    // Parse IMM as a signed integer.  Accepts decimal with optional
    // leading minus; outside the i32 range is a hard error since the
    // x86_64 `add rax, imm32' encoding sign-extends a 32-bit value.
    let imm: i32 = match imm_str.parse::<i64>() {
        Ok(n) if n >= i32::MIN as i64 && n <= i32::MAX as i64 => n as i32,
        Ok(n) => {
            eprintln!(
                "nelisp: mint-int-plus-imm: IMM {} out of i32 range",
                n
            );
            return ExitCode::from(2);
        }
        Err(e) => {
            eprintln!("nelisp: mint-int-plus-imm: cannot parse IMM {:?}: {}", imm_str, e);
            return ExitCode::from(2);
        }
    };
    // Evaluate SRC; result must reduce to an integer.
    let result = match eval_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            return ExitCode::from(2);
        }
    };
    let heap_int = match result {
        Sexp::Int(n) => n,
        other => {
            eprintln!(
                "nelisp: mint-int-plus-imm: SRC evaluated to {:?}, want Int",
                fmt_sexp(&other)
            );
            return ExitCode::from(3);
        }
    };
    // Lower the integer to an 8-byte tagged-int heap word.  The asm
    // chain expects argv[0] to point at this word.
    let (heap, relocs) = match lower_to_heap(&Sexp::Int(heap_int)) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: lowering failed: {}", e);
            return ExitCode::from(3);
        }
    };
    // Compose the chain: head + add(imm) + ret.
    let code = emit_int_plus_imm(imm);
    match write_image_with_heap_code_and_relocs(out, &code, &heap, &relocs) {
        Ok(()) => {
            // i64 + i32 → i64; downstream exit-code semantics will
            // truncate to i32 when boot returns it.
            let predicted = (heap_int as i64).wrapping_add(imm as i64);
            println!(
                "minted int-plus-imm NlImage at {} (heap_int={}, imm={}, predicted={}, code_size={}, heap_size={}, reloc_count={})",
                out,
                heap_int,
                imm,
                predicted,
                code.len(),
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

/// Parse a Stage 9d op spec like "add:5 mul:2 neg" into a `ChainOp'
/// vector wrapped between `LoadHeapHead' and `Ret'.  Returns an error
/// string with the offending token on bad input.
fn parse_chain_ops(spec: &str) -> Result<Vec<ChainOp>, String> {
    let mut ops = vec![ChainOp::LoadHeapHead];
    for token in spec.split_whitespace() {
        let lower = token.to_ascii_lowercase();
        let parsed = if let Some(rest) = lower.strip_prefix("add:") {
            ChainOp::AddImm(rest.parse::<i32>().map_err(|e| {
                format!("bad imm in {:?}: {}", token, e)
            })?)
        } else if let Some(rest) = lower.strip_prefix("sub:") {
            ChainOp::SubImm(rest.parse::<i32>().map_err(|e| {
                format!("bad imm in {:?}: {}", token, e)
            })?)
        } else if let Some(rest) = lower.strip_prefix("mul:") {
            ChainOp::MulImm(rest.parse::<i32>().map_err(|e| {
                format!("bad imm in {:?}: {}", token, e)
            })?)
        } else if lower == "neg" {
            ChainOp::Neg
        } else {
            return Err(format!("unknown op {:?}", token));
        };
        ops.push(parsed);
    }
    ops.push(ChainOp::Ret);
    Ok(ops)
}

/// Symbolically apply a [`ChainOp`] sequence to an i64 accumulator
/// (matches the asm semantics: load → arithmetic → ret).  Returns
/// the value the boot is expected to exit with — used in the driver
/// log line so the user sees the predicted result alongside the
/// actual asm bytes.
fn predict_chain_value(heap_int: i64, ops: &[ChainOp]) -> i64 {
    let mut acc: i64 = 0;
    for op in ops {
        match op {
            ChainOp::LoadHeapHead => acc = heap_int,
            ChainOp::AddImm(n) => acc = acc.wrapping_add(*n as i64),
            ChainOp::SubImm(n) => acc = acc.wrapping_sub(*n as i64),
            ChainOp::MulImm(n) => acc = acc.wrapping_mul(*n as i64),
            ChainOp::Neg => acc = acc.wrapping_neg(),
            ChainOp::Ret => break,
        }
    }
    acc
}

fn run_mint_chain(src: &str, ops_spec: &str, out: &str) -> ExitCode {
    if !HAS_CHAIN_OP_COMPOSE {
        eprintln!("nelisp: mint-chain: native_emit unavailable on this arch");
        return ExitCode::from(14);
    }
    let ops = match parse_chain_ops(ops_spec) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("nelisp: mint-chain: {}", e);
            return ExitCode::from(2);
        }
    };
    let result = match eval_str(src) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            return ExitCode::from(2);
        }
    };
    let heap_int = match result {
        Sexp::Int(n) => n,
        other => {
            eprintln!(
                "nelisp: mint-chain: SRC evaluated to {:?}, want Int",
                fmt_sexp(&other)
            );
            return ExitCode::from(3);
        }
    };
    let (heap, relocs) = match lower_to_heap(&Sexp::Int(heap_int)) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: lowering failed: {}", e);
            return ExitCode::from(3);
        }
    };
    let code = compose(&ops);
    let predicted = predict_chain_value(heap_int, &ops);
    match write_image_with_heap_code_and_relocs(out, &code, &heap, &relocs) {
        Ok(()) => {
            println!(
                "minted chain NlImage at {} (heap_int={}, ops={:?}, predicted={}, code_size={}, heap_size={}, reloc_count={})",
                out,
                heap_int,
                ops_spec,
                predicted,
                code.len(),
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
        Sexp::Vector(_) => Ok((
            NATIVE_LOAD_HEAP_VECTOR_LEN,
            HAS_NATIVE_LOAD_HEAP_VECTOR_LEN,
            "vector-len",
        )),
    }
}

fn run_mint_eval_all(src: &str, out: &str, label_for_log: &str) -> ExitCode {
    // Doc 47 Stage 8b — when label_for_log is the source file path
    // (= `mint-eval-file' callers), seed `default-directory' /
    // `load-path' so `(require 'sibling)' resolves siblings.
    let result = if label_for_log.is_empty() {
        eval_str_all(src)
    } else {
        eval_str_all_at_path(src, label_for_log)
    };
    match result {
        Ok(value) => mint_eval_result(value, out, &format!("eval-file({})", label_for_log)),
        Err(e) => {
            eprintln!("nelisp: eval error: {}", e);
            ExitCode::from(2)
        }
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
    mint_eval_result(result, out, "eval-result")
}

/// Shared finisher: take an evaluator result, pick the matching
/// native asset, lower the value to (heap, relocs), and write the
/// image at OUT.  Used by both `mint-eval-result' (single-form
/// inline source) and `mint-eval-file' (whole-file `progn').
fn mint_eval_result(result: Sexp, out: &str, mint_label: &str) -> ExitCode {
    let (asset, has_asset, asset_label) = match pick_asset_for_eval_result(&result) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("nelisp: {}", e);
            return ExitCode::from(3);
        }
    };
    if !has_asset {
        eprintln!(
            "nelisp: {}: {} asset unavailable on this arch",
            mint_label, asset_label
        );
        return ExitCode::from(14);
    }
    let (heap, relocs) = match lower_to_heap(&result) {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("nelisp: lowering failed for {}: {}", mint_label, e);
            return ExitCode::from(3);
        }
    };
    match write_image_with_heap_code_and_relocs(out, asset, &heap, &relocs) {
        Ok(()) => {
            println!(
                "minted {} NlImage at {} (eval={}, asset={}, heap_size={}, reloc_count={})",
                mint_label,
                out,
                fmt_sexp(&result),
                asset_label,
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
    use nelisp_build_tool::native_emit::ChainOp;

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
    fn parses_mint_eval_file() {
        match parse_args(["nelisp", "mint-eval-file", "boot.el", "/tmp/b.bin"]).unwrap() {
            Command::MintEvalFile { src_file, out } => {
                assert_eq!(src_file, "boot.el");
                assert_eq!(out, "/tmp/b.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_int_as_code() {
        match parse_args(["nelisp", "mint-int-as-code", "(+ 1 2)", "/tmp/c.bin"]).unwrap() {
            Command::MintIntAsCode { src, out } => {
                assert_eq!(src, "(+ 1 2)");
                assert_eq!(out, "/tmp/c.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_int_via_emitted_load() {
        match parse_args(["nelisp", "mint-int-via-emitted-load", "(+ 1 2)", "/tmp/d.bin"])
            .unwrap()
        {
            Command::MintIntViaEmittedLoad { src, out } => {
                assert_eq!(src, "(+ 1 2)");
                assert_eq!(out, "/tmp/d.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_int_plus_imm() {
        match parse_args(["nelisp", "mint-int-plus-imm", "(+ 1 2)", "10", "/tmp/e.bin"])
            .unwrap()
        {
            Command::MintIntPlusImm { src, imm, out } => {
                assert_eq!(src, "(+ 1 2)");
                assert_eq!(imm, "10");
                assert_eq!(out, "/tmp/e.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parses_mint_chain() {
        match parse_args([
            "nelisp",
            "mint-chain",
            "5",
            "add:3 mul:2",
            "/tmp/f.bin",
        ])
        .unwrap()
        {
            Command::MintChain { src, ops, out } => {
                assert_eq!(src, "5");
                assert_eq!(ops, "add:3 mul:2");
                assert_eq!(out, "/tmp/f.bin");
            }
            other => panic!("unexpected: {:?}", other),
        }
    }

    #[test]
    fn parse_chain_ops_basic() {
        use super::parse_chain_ops;
        let ops = parse_chain_ops("add:5 sub:3 mul:2 neg").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::AddImm(5),
                ChainOp::SubImm(3),
                ChainOp::MulImm(2),
                ChainOp::Neg,
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn parse_chain_ops_empty_yields_load_then_ret() {
        use super::parse_chain_ops;
        let ops = parse_chain_ops("").unwrap();
        assert_eq!(ops, vec![ChainOp::LoadHeapHead, ChainOp::Ret]);
    }

    #[test]
    fn parse_chain_ops_rejects_unknown() {
        use super::parse_chain_ops;
        assert!(parse_chain_ops("add:5 wibble").is_err());
        assert!(parse_chain_ops("add:not-a-number").is_err());
    }

    #[test]
    fn predict_chain_value_arithmetic() {
        use super::predict_chain_value;
        // Heap = 5, chain: add 3 -> 8, mul 2 -> 16, neg -> -16.
        let val = predict_chain_value(
            5,
            &[
                ChainOp::LoadHeapHead,
                ChainOp::AddImm(3),
                ChainOp::MulImm(2),
                ChainOp::Neg,
                ChainOp::Ret,
            ],
        );
        assert_eq!(val, -16);
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
    fn pick_asset_includes_vector() {
        use super::pick_asset_for_eval_result;
        use nelisp_build_tool::reader::Sexp;
        let v = Sexp::Vector(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        assert_eq!(pick_asset_for_eval_result(&v).unwrap().2, "vector-len");
    }
}
