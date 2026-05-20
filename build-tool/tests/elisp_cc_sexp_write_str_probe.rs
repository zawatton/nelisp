#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

// ---- helpers ----

fn write_str_into_slot(bytes: &[u8]) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::sexp_write_str(
            slot_ptr,
            bytes.as_ptr(),
            bytes.len() as i64,
        )
    };
    assert_eq!(
        returned, slot_ptr,
        "sexp-write-str must return the caller-provided slot pointer"
    );
    slot
}

fn write_symbol_into_slot(bytes: &[u8]) -> Sexp {
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::sexp_write_symbol(
            slot_ptr,
            bytes.as_ptr(),
            bytes.len() as i64,
        )
    };
    assert_eq!(
        returned, slot_ptr,
        "sexp-write-symbol must return the caller-provided slot pointer"
    );
    slot
}

// ---- Case 1: empty string allocation ----

#[test]
fn sexp_write_str_empty_string_yields_str_variant_with_zero_len() {
    // `len == 0' is the edge case: the elisp emit + Rust extern must
    // tolerate a possibly-null/dangling bytes_ptr because
    // `slice::from_raw_parts(ptr, 0)' is defined regardless of ptr.
    // We pass `b""'s pointer (= non-null, but the slice is empty) to
    // mirror what a real call from the Reader lexer would do.
    let bytes: &[u8] = b"";
    let s = write_str_into_slot(bytes);
    match s {
        Sexp::Str(ref text) => {
            assert_eq!(text.len(), 0, "empty input must yield empty String");
            assert_eq!(text, "", "empty Str variant must contain empty string");
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 2: single-byte ASCII ("A") ----

#[test]
fn sexp_write_str_single_byte_ascii_round_trips() {
    let bytes: &[u8] = b"A";
    let s = write_str_into_slot(bytes);
    match s {
        Sexp::Str(ref text) => {
            assert_eq!(text.len(), 1);
            assert_eq!(text.as_bytes(), b"A");
            assert_eq!(text, "A");
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 3: multi-byte UTF-8 ----

#[test]
fn sexp_write_str_multibyte_utf8_round_trips() {
    // CJK "日本語" = 3 codepoints × 3 bytes = 9 bytes; a 4-byte
    // emoji "🦀" appended for the 4-byte UTF-8 lead-byte case.
    // Together that's 13 bytes.  The elisp extern must do a byte-
    // accurate copy without any codepoint reinterpretation.
    let text = "日本語🦀";
    let bytes = text.as_bytes();
    assert_eq!(bytes.len(), 13, "test data shape sanity");
    let s = write_str_into_slot(bytes);
    match s {
        Sexp::Str(ref got) => {
            assert_eq!(got.as_bytes().len(), 13);
            assert_eq!(got, text);
        }
        other => panic!("expected Sexp::Str, got {:?}", other),
    }
}

// ---- Case 4: Symbol allocation ----

#[test]
fn sexp_write_symbol_yields_symbol_variant() {
    // Symbol path mirrors Str but emits `Sexp::Symbol(_)'.  Pick a
    // typical type-of symbol name to mirror the
    // `nl_jit_symbol_name' / `nl_jit_type_of' use case (= 4
    // trampolines unblocked by this op).
    let bytes: &[u8] = b"cons";
    let s = write_symbol_into_slot(bytes);
    match s {
        Sexp::Symbol(ref name) => {
            assert_eq!(name, "cons");
            assert_eq!(name.as_bytes(), b"cons");
        }
        other => panic!("expected Sexp::Symbol, got {:?}", other),
    }
}

// ---- Case 5: round-trip with String content equality ----

#[test]
fn sexp_write_str_round_trip_content_matches_input() {
    // Round-trip gate: a freshly-allocated `Sexp::Str' wrapping the
    // input bytes must structurally equal a Rust-side
    // `Sexp::Str(input.to_string())'.  This is the "compiler agrees
    // with the Rust extern on the Sexp::Str layout" check that the
    // 4 downstream trampolines (`nl_jit_intern' /
    // `nl_jit_symbol_name' / `nl_jit_type_of' / `nl_jit_make_symbol')
    // rely on for their value-equality assertions.
    let input = "round-trip-probe";
    let bytes = input.as_bytes();
    let got = write_str_into_slot(bytes);
    let expected = Sexp::Str(input.to_string());
    assert_eq!(
        got, expected,
        "round-trip Str must equal a fresh Rust-side Sexp::Str"
    );
}

// ---- Case 6: Symbol round-trip ----

#[test]
fn sexp_write_symbol_round_trip_content_matches_input() {
    // Symbol round-trip parallels Case 5.
    let input = "integer";
    let bytes = input.as_bytes();
    let got = write_symbol_into_slot(bytes);
    let expected = Sexp::Symbol(input.to_string());
    assert_eq!(
        got, expected,
        "round-trip Symbol must equal Rust-side Sexp::Symbol"
    );
}
