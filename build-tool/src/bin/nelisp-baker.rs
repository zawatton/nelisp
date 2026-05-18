//! Doc 95 §95.e cross-implementation byte-identity verifier.
//!
//! Originally (Doc 72 Stage 7.7.a / Doc 98 §98.2-§98.3) this bin
//! also drove `make bake-images' to emit `lisp/*.image' frozen-heap
//! files consumed by `Env::new_global' via `decode_v3_into'.  Doc 126
//! retired that production path (= eval-boot loads `.el' sources
//! directly through `reader::read_all + eval'); the bake loop +
//! `STDLIB_FILES' mirror were deleted alongside the on-disk
//! `.image' artifacts in 126.C.
//!
//! What survives in this binary is the §95.e gate: decode each
//! elisp-produced NELIMG v3 fixture (= emitted by
//! `lisp/nelisp-sexp-dsl.el's encoder), re-encode via the Rust
//! `image::encode_v3_with_fallback', and assert byte-identity.
//! Failure = drift between the elisp encoder and the Rust encoder.
//!
//! CLI:
//!
//!     nelisp-baker --verify-elisp-fixtures FIXTURE [FIXTURE...]
//!
//! Driven by `make verify-elisp-fixtures' (Makefile target) which
//! runs the elisp fixture emitter first and pipes the result paths
//! here.

use nelisp_build_tool::eval::Env;
use nelisp_build_tool::image;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args = env::args().skip(1);
    let mut verify_fixtures: Vec<PathBuf> = Vec::new();
    let mut in_verify_mode = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--verify-elisp-fixtures" => {
                in_verify_mode = true;
                // Remaining args are fixture paths.
                for path in args.by_ref() {
                    verify_fixtures.push(PathBuf::from(path));
                }
            }
            "--help" | "-h" => {
                println!(
                    "usage: nelisp-baker --verify-elisp-fixtures FIXTURE [FIXTURE...]"
                );
                return ExitCode::SUCCESS;
            }
            other => {
                eprintln!("nelisp-baker: unknown argument {}", other);
                return ExitCode::from(2);
            }
        }
    }
    if !in_verify_mode {
        eprintln!(
            "nelisp-baker: --verify-elisp-fixtures FIXTURE [FIXTURE...] is the \
             only supported mode (Doc 126 retired the .image bake loop)."
        );
        return ExitCode::from(2);
    }
    if verify_fixtures.is_empty() {
        eprintln!(
            "nelisp-baker: --verify-elisp-fixtures needs >=1 fixture path"
        );
        return ExitCode::from(2);
    }
    // Doc 95 §95.e cross-impl byte-identity gate: decode each elisp-
    // produced fixture and assert the Rust encoder yields identical
    // bytes for the same fallback-form payload.
    let mut mismatches = Vec::new();
    for fixture in &verify_fixtures {
        match verify_elisp_fixture(fixture) {
            Ok(()) => {
                println!("verified {} (byte-identical)", fixture.display());
            }
            Err(msg) => {
                eprintln!("nelisp-baker: {} : {}", fixture.display(), msg);
                mismatches.push(fixture.clone());
            }
        }
    }
    if mismatches.is_empty() {
        ExitCode::SUCCESS
    } else {
        ExitCode::from(4)
    }
}

/// Doc 95 §95.e: decode an elisp-produced NELIMG v3 fixture, re-emit
/// via the Rust encoder, and confirm byte-identity.  Returns Err with
/// a diagnostic string on any mismatch / decode error.
fn verify_elisp_fixture(path: &std::path::Path) -> Result<(), String> {
    let bytes = fs::read(path).map_err(|e| format!("read failed: {}", e))?;
    let mut env = Env::empty();
    let fallback_forms = image::decode_v3_into(&mut env, &bytes)
        .map_err(|e| format!("decode failed: {}", e))?;
    // Re-emit via Rust encoder using the same fallback-form payload
    // and an empty env (= matches the envelope-only subset produced
    // by `compile_elisp_to_image').
    let reemit = image::encode_v3_with_fallback(&Env::empty(), &fallback_forms)
        .map_err(|e| format!("re-encode failed: {}", e))?;
    if reemit == bytes {
        Ok(())
    } else {
        // Find first diverging byte for a more actionable report.
        let first_diff = bytes
            .iter()
            .zip(reemit.iter())
            .position(|(a, b)| a != b);
        Err(format!(
            "byte-mismatch (on-disk-len={} reemit-len={} first-diff={:?})",
            bytes.len(),
            reemit.len(),
            first_diff
        ))
    }
}
