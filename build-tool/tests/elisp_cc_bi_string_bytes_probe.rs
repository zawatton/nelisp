#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

//! Doc 117 §117.A.2 — probe test for the `bi_string_bytes' elisp swap.
//!
//! Exercises the Phase 47 `.o' compiled from
//! `lisp/nelisp-cc-bi-string-bytes.el' through the safe wrapper
//! `nelisp_build_tool::elisp_cc_spike::bi_string_bytes'.  Each case
//! checks both the byte-count payload (`Sexp::Int(_)`) and the
//! ergonomic return value (= the caller-provided slot pointer).

use nelisp_build_tool::eval::sexp::Sexp;

fn run_string_bytes(input: Sexp) -> Sexp {
    let mut slot: Sexp = Sexp::Nil;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_string_bytes(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        )
    };
    assert_eq!(
        returned,
        &mut slot as *mut Sexp,
        "bi_string_bytes must return the caller-provided slot pointer",
    );
    slot
}

#[test]
fn bi_string_bytes_empty_is_zero() {
    assert_eq!(run_string_bytes(Sexp::Str(String::new())), Sexp::Int(0));
}

#[test]
fn bi_string_bytes_single_ascii_byte_is_one() {
    assert_eq!(run_string_bytes(Sexp::Str("x".into())), Sexp::Int(1));
}

#[test]
fn bi_string_bytes_ascii_word_matches_byte_len() {
    let s = "hello";
    assert_eq!(
        run_string_bytes(Sexp::Str(s.into())),
        Sexp::Int(s.len() as i64)
    );
}

#[test]
fn bi_string_bytes_multibyte_utf8_counts_bytes_not_chars() {
    // Japanese "藤澤" is 6 UTF-8 bytes (2 codepoints × 3 bytes each) —
    // the elisp body must return the byte count, not the char count.
    let s = "藤澤";
    assert_eq!(s.chars().count(), 2);
    assert_eq!(s.len(), 6);
    assert_eq!(
        run_string_bytes(Sexp::Str(s.into())),
        Sexp::Int(s.len() as i64)
    );
}
