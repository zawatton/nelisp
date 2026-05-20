#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn fnv1a_bytes(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for b in s.bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

fn run_fnv1a(s: &str) -> u32 {
    let sym = Sexp::Symbol(s.into());
    let raw = unsafe { nelisp_build_tool::elisp_cc_spike::fnv1a(&sym as *const Sexp) };
    // High 32 bits must be zero (= the `(logand h #xFFFFFFFF)' mask).
    assert_eq!(
        raw as u64 >> 32,
        0,
        "nelisp_fnv1a returned i64 with non-zero high 32 bits ({:#x}) for {:?}",
        raw,
        s,
    );
    raw as u32
}

#[test]
fn fnv1a_empty_returns_offset_basis() {
    // Reference vector: FNV-1a hash of "" is the OFFSET_BASIS itself.
    assert_eq!(run_fnv1a(""), 0x811C9DC5);
}

#[test]
fn fnv1a_single_ascii_byte_matches_reference() {
    // Reference vector: FNV-1a("a") = 0xE40C292C (= classic published
    // value from the algorithm's original spec).
    assert_eq!(run_fnv1a("a"), 0xE40C292C);
}

#[test]
fn fnv1a_short_word_matches_reference() {
    // Reference vector: FNV-1a("foobar") = 0xBF9CF968 (= same value
    // the deleted Rust `mirror_fnv1a_matches_elisp_hash_loop' test
    // asserted, preserved as a regression anchor).
    assert_eq!(run_fnv1a("foobar"), 0xBF9CF968);
}

#[test]
fn fnv1a_hello_matches_byte_oracle() {
    let s = "hello";
    assert_eq!(run_fnv1a(s), fnv1a_bytes(s));
}

#[test]
fn fnv1a_string_with_underscores_matches_byte_oracle() {
    // Representative elisp identifier shape (= `defvar' name pattern).
    let s = "nelisp--unbound-marker";
    assert_eq!(run_fnv1a(s), fnv1a_bytes(s));
}

#[test]
fn fnv1a_multibyte_utf8_iterates_bytes() {
    // The elisp body uses `str-byte-at' (= raw byte iteration), so the
    // multi-byte UTF-8 path matches `fnv1a_bytes', not the codepoint-
    // iteration impl that lived in the deleted Rust `mirror_fnv1a'.
    // Env mirror keys are always ASCII so this divergence is benign;
    // the probe asserts the byte-iteration behaviour explicitly.
    let s = "藤澤";
    assert_eq!(s.len(), 6); // 2 codepoints x 3 bytes
    assert_eq!(run_fnv1a(s), fnv1a_bytes(s));
}

#[test]
fn fnv1a_long_string_matches_byte_oracle() {
    // 100-char ASCII string exercises the tail-recursive inner loop
    // well past any short-input fast path Phase 47 might emit.
    let s: String = std::iter::repeat('x').take(100).collect();
    assert_eq!(run_fnv1a(&s), fnv1a_bytes(&s));
}

#[test]
fn fnv1a_accepts_str_arm_in_addition_to_symbol() {
    // The helper's `str-len' / `str-byte-at' ops read the shared
    // 24-byte `String' header layout used by both Sexp::Str and
    // Sexp::Symbol; the elisp body itself doesn't dispatch on tag.
    // Confirm by feeding a `Sexp::Str' and asserting the same hash.
    let s = "hello";
    let str_arm = Sexp::Str(s.into());
    let raw = unsafe { nelisp_build_tool::elisp_cc_spike::fnv1a(&str_arm as *const Sexp) };
    assert_eq!(raw as u32, fnv1a_bytes(s));
    assert_eq!(raw as u64 >> 32, 0);
}
