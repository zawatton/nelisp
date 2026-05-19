//! Doc 122 §122.D probes — direct calls into the three Phase 47-
//! compiled UTF-8 helper grammar ops (= `str-char-count' /
//! `str-codepoint-at' / `str-is-alphanumeric-at').  Pattern mirrors
//! `elisp_cc_sexp_write_str_probe.rs' (§122.A) and
//! `elisp_cc_mut_str_probe.rs' (§122.B).
//!
//! Verifies:
//!
//!   - Empty string char count = 0.
//!   - ASCII-only char count == byte count.
//!   - Multi-byte UTF-8 char count != byte count (`"藤澤"` = 6
//!     bytes / 2 chars).
//!   - Codepoint-at edge cases (start / mid-stream / past end /
//!     mid-codepoint byte index).
//!   - Alphanumeric ASCII + non-ASCII alphanumeric (Greek alpha
//!     `"α"`, fullwidth digit `"１"`) + non-alnum punctuation +
//!     whitespace.
//!
//! Each test allocates a `Sexp::Str` Rust-side, takes its raw
//! pointer, and invokes the elisp-compiled extern via the safe
//! `elisp_cc_spike` wrapper.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

// ---- helpers ----

fn char_count_of(text: &str) -> i64 {
    let s = Sexp::Str(text.to_string());
    let ptr = &s as *const Sexp;
    unsafe { nelisp_build_tool::elisp_cc_spike::str_char_count(ptr) }
}

fn codepoint_at(text: &str, idx: i64) -> Option<(i64, i64)> {
    let s = Sexp::Str(text.to_string());
    let ptr = &s as *const Sexp;
    let mut cp: i64 = -1;
    let mut width: i64 = -1;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::str_codepoint_at(
            ptr,
            idx,
            &mut cp as *mut i64,
            &mut width as *mut i64,
        )
    };
    if rc == 1 {
        Some((cp, width))
    } else {
        None
    }
}

fn is_alphanumeric_at(text: &str, idx: i64) -> i64 {
    let s = Sexp::Str(text.to_string());
    let ptr = &s as *const Sexp;
    unsafe { nelisp_build_tool::elisp_cc_spike::str_is_alphanumeric_at(ptr, idx) }
}

// ---- str-char-count cases ----

#[test]
fn char_count_empty_string_is_zero() {
    assert_eq!(char_count_of(""), 0);
}

#[test]
fn char_count_ascii_matches_byte_count() {
    // For pure ASCII the codepoint count equals the byte count.
    let text = "hello";
    assert_eq!(char_count_of(text), 5);
    assert_eq!(char_count_of(text) as usize, text.as_bytes().len());
}

#[test]
fn char_count_multibyte_japanese_diverges_from_byte_count() {
    // CJK example from the task spec: "藤澤" = 2 codepoints / 6
    // UTF-8 bytes.  The op MUST return 2, not 6.
    let text = "藤澤";
    assert_eq!(text.as_bytes().len(), 6, "test data shape sanity");
    assert_eq!(char_count_of(text), 2);
}

#[test]
fn char_count_mixed_ascii_cjk_emoji() {
    // Cover 1- / 3- / 4-byte UTF-8 sequences in one input.
    //   'A'   = 1 byte  / 1 codepoint
    //   '日'  = 3 bytes / 1 codepoint
    //   '🦀'  = 4 bytes / 1 codepoint
    // Total = 8 bytes / 3 codepoints.
    let text = "A日🦀";
    assert_eq!(text.as_bytes().len(), 8);
    assert_eq!(char_count_of(text), 3);
}

#[test]
fn char_count_greek_alphabet_two_byte_path() {
    // Greek alpha "α" = U+03B1 (2 bytes).  "αβγ" exercises the
    // 2-byte UTF-8 codepoint width that ASCII / CJK didn't cover.
    let text = "αβγ";
    assert_eq!(text.as_bytes().len(), 6, "Greek lowercase = 2 bytes each");
    assert_eq!(char_count_of(text), 3);
}

// ---- str-codepoint-at cases ----

#[test]
fn codepoint_at_start_of_ascii_string() {
    let text = "hello";
    let (cp, width) = codepoint_at(text, 0).expect("decode at 0 must succeed");
    assert_eq!(cp, b'h' as i64);
    assert_eq!(width, 1);
}

#[test]
fn codepoint_at_mid_ascii_string() {
    // Position 2 lands on 'l'.
    let text = "hello";
    let (cp, width) = codepoint_at(text, 2).expect("decode at 2 must succeed");
    assert_eq!(cp, b'l' as i64);
    assert_eq!(width, 1);
}

#[test]
fn codepoint_at_past_end_returns_zero() {
    // `idx == len' must return 0 (out of range) — by definition the
    // empty suffix has no codepoint.
    let text = "hi";
    assert_eq!(codepoint_at(text, text.as_bytes().len() as i64), None);
    // `idx > len' also returns 0.
    assert_eq!(codepoint_at(text, 100), None);
}

#[test]
fn codepoint_at_mid_multibyte_codepoint_returns_zero() {
    // "藤" is U+85E4 = 3 bytes E8 97 A4.  Byte idx 0 = boundary,
    // 1 / 2 are interior bytes -> decode must fail.
    let text = "藤澤";
    let (cp0, w0) = codepoint_at(text, 0).expect("byte 0 is boundary");
    assert_eq!(cp0, 0x85E4);
    assert_eq!(w0, 3);
    assert_eq!(codepoint_at(text, 1), None, "byte 1 inside U+85E4");
    assert_eq!(codepoint_at(text, 2), None, "byte 2 inside U+85E4");
    // Byte 3 is the start of "澤" = U+6FA4 (3 bytes).
    let (cp3, w3) = codepoint_at(text, 3).expect("byte 3 is next boundary");
    assert_eq!(cp3, 0x6FA4);
    assert_eq!(w3, 3);
}

#[test]
fn codepoint_at_four_byte_emoji() {
    // "🦀" = U+1F980 = 4 bytes F0 9F A6 80.
    let text = "🦀";
    let (cp, width) = codepoint_at(text, 0).expect("emoji must decode");
    assert_eq!(cp, 0x1F980);
    assert_eq!(width, 4);
}

#[test]
fn codepoint_at_negative_idx_returns_zero() {
    let text = "abc";
    assert_eq!(codepoint_at(text, -1), None, "negative idx must fail");
}

// ---- str-is-alphanumeric-at cases ----

#[test]
fn is_alphanumeric_at_ascii_letter() {
    let text = "hello";
    for i in 0..text.as_bytes().len() {
        assert_eq!(
            is_alphanumeric_at(text, i as i64),
            1,
            "all bytes of {:?} must be alphanumeric",
            text
        );
    }
}

#[test]
fn is_alphanumeric_at_ascii_digit() {
    let text = "0123456789";
    for i in 0..text.as_bytes().len() {
        assert_eq!(is_alphanumeric_at(text, i as i64), 1, "digit at {}", i);
    }
}

#[test]
fn is_alphanumeric_at_non_ascii_alphabetic() {
    // Greek alpha "α" = U+03B1 (2 bytes E2 B1 -- actually CE B1).
    // Multi-byte slow path must consult `char::is_alphanumeric'.
    let text = "α";
    assert_eq!(text.as_bytes().len(), 2);
    assert_eq!(
        is_alphanumeric_at(text, 0),
        1,
        "Greek alpha must be alphanumeric on the multi-byte path"
    );
    // Interior byte returns 0 (= the ASCII fast path's `b' is not
    // ASCII alphanumeric, then boundary check fails).
    assert_eq!(is_alphanumeric_at(text, 1), 0);
}

#[test]
fn is_alphanumeric_at_punctuation_and_whitespace() {
    // Pure ASCII non-alnum characters must return 0.
    for ch in [' ', '.', ',', '!', '?', '-', '+', '\t', '\n'].iter() {
        let s = ch.to_string();
        assert_eq!(
            is_alphanumeric_at(&s, 0),
            0,
            "{:?} must not be alphanumeric",
            ch
        );
    }
}

#[test]
fn is_alphanumeric_at_mixed_string() {
    // "abc def" -- a/b/c/d/e/f alphanumeric, space at idx 3 not.
    let text = "abc def";
    assert_eq!(is_alphanumeric_at(text, 0), 1);
    assert_eq!(is_alphanumeric_at(text, 1), 1);
    assert_eq!(is_alphanumeric_at(text, 2), 1);
    assert_eq!(is_alphanumeric_at(text, 3), 0, "space at idx 3");
    assert_eq!(is_alphanumeric_at(text, 4), 1);
    assert_eq!(is_alphanumeric_at(text, 5), 1);
    assert_eq!(is_alphanumeric_at(text, 6), 1);
    // Past end must return 0.
    assert_eq!(is_alphanumeric_at(text, 7), 0);
    assert_eq!(is_alphanumeric_at(text, 100), 0);
}

#[test]
fn is_alphanumeric_at_fullwidth_digit_unicode_path() {
    // Fullwidth digit "１" (U+FF11) is alphanumeric under Unicode
    // but its UTF-8 bytes (EF BC 91) are not ASCII digits, so the
    // multi-byte slow path must trigger.
    let text = "１";
    assert_eq!(text.as_bytes().len(), 3);
    assert_eq!(
        is_alphanumeric_at(text, 0),
        1,
        "fullwidth digit U+FF11 must be alphanumeric via slow path"
    );
}
