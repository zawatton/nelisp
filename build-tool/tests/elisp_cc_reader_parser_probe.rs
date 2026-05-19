//! Doc 116 §116.B probes — direct calls into the Phase 47-compiled
//! pure-elisp Reader parser (`nelisp_reader_parse_one').  Pattern
//! mirrors `elisp_cc_reader_lexer_probe.rs' for the §116.A lexer.
//!
//! Each test allocates a `Sexp::Str' source, sets up the slot pool
//! Vector (= MutStr scratch slot, payload slot, const-Nil source slot,
//! per-depth working slots), allocates a `Sexp::Int(0)' cursor slot
//! + a `Sexp::Nil' result slot, then calls `reader_parse_one' and
//! asserts the produced Sexp value matches the expected shape.
//!
//! Scope (= §116.B MVP):
//!   - Atom leaves: Nil / T / Int / Symbol / Str / Char / RadixInt /
//!     Float (= Doc 122 §122.G unlock).
//!   - Proper list construction `(a b c)' + empty list `()'.
//!   - Dotted-pair construction `(a . b)'.
//!   - Quote-family desugar `'x' / `` `x `` / `,x' / `,@x' / `#'x'.
//!   - Nested lists `((a b) c)' / `(1 (2 3))'.
//!
//! Out of scope (= §116.C top-level + §116.D file deletion):
//!   vectors `[..]', records `#s(..)', byte-code `#[..]', the public
//!   `(read-str S)' / `(read-all S)' helpers.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::elisp_cc_spike;
use nelisp_build_tool::eval::sexp::Sexp;

/// Per-test slot-pool capacity (= covers nesting depth ~32 with
/// 4 working slots per depth + 3 fixed-position slots).
const POOL_SIZE: usize = 3 + 4 * 32;

/// Parse one top-level form from SOURCE.  Returns the parsed Sexp
/// (= `*result_slot' after the call).  Panics if the parser reports
/// a non-success status.
/// Returns the parsed Sexp.  Leaks `pool_slot' (= via `mem::forget')
/// because cons-make's MVP refcount semantics (= raw 32-byte payload
/// copy without refcount bump on inner boxes) means `result_slot'
/// shares heap boxes with `pool_slot[*]'; dropping pool first would
/// trigger use-after-free on the shared boxes (= the test would
/// observe garbage Sexp values or hit a SIGSEGV during the drop
/// chain).  The leaked memory is reclaimed by the test process exit.
/// See `nelisp-phase47-compiler--emit-cons-make' for the constraint;
/// a future §116.C top-level wrapper will either deep-clone the
/// parser output before returning to the caller OR refcount-bump
/// the shared boxes in `cons-make' itself (= Doc 123 cluster).
fn parse_one(source: &str) -> Sexp {
    let src = Sexp::Str(source.to_string());
    let mut cursor_slot = Sexp::Int(0);
    let mut result_slot = Sexp::Nil;
    let mut slots = vec![Sexp::Nil; POOL_SIZE];
    slots[0] = Sexp::mut_str(String::with_capacity(64));
    let pool_slot = Sexp::vector(slots);
    let status = unsafe {
        elisp_cc_spike::reader_parse_one(
            &src as *const Sexp,
            &mut cursor_slot as *mut Sexp,
            &mut result_slot as *mut Sexp,
            &pool_slot as *const Sexp,
            0,
        )
    };
    assert_eq!(status, 1, "parser returned non-success status");
    // Leak pool_slot to keep shared boxes alive (= cons-make MVP
    // refcount discipline; see `assert_parses_to' comment).
    std::mem::forget(pool_slot);
    result_slot
}

// ---------- atoms ----------

#[test]
fn parse_nil_atom() {
    assert_eq!(parse_one("nil"), Sexp::Nil);
}

#[test]
fn parse_t_atom() {
    assert_eq!(parse_one("t"), Sexp::T);
}

#[test]
fn parse_int_atom() {
    assert_eq!(parse_one("42"), Sexp::Int(42));
}

#[test]
fn parse_negative_int_atom() {
    assert_eq!(parse_one("-7"), Sexp::Int(-7));
}

#[test]
fn parse_positive_int_atom() {
    assert_eq!(parse_one("+3"), Sexp::Int(3));
}

#[test]
fn parse_zero_int() {
    assert_eq!(parse_one("0"), Sexp::Int(0));
}

#[test]
fn parse_symbol_atom() {
    assert_eq!(parse_one("foo"), Sexp::Symbol("foo".to_string()));
}

#[test]
fn parse_symbol_with_dashes() {
    assert_eq!(parse_one("foo-bar"), Sexp::Symbol("foo-bar".to_string()));
}

#[test]
fn parse_string_atom() {
    assert_eq!(parse_one("\"hello\""), Sexp::Str("hello".to_string()));
}

#[test]
fn parse_empty_string_atom() {
    assert_eq!(parse_one("\"\""), Sexp::Str("".to_string()));
}

// ---------- empty list + simple lists ----------

#[test]
fn parse_empty_list() {
    assert_eq!(parse_one("()"), Sexp::Nil);
}

#[test]
fn parse_one_element_list() {
    let got = parse_one("(1)");
    assert_eq!(got, Sexp::list_from(&[Sexp::Int(1)]));
}

#[test]
fn parse_three_element_list() {
    let got = parse_one("(1 2 3)");
    assert_eq!(
        got,
        Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
    );
}

#[test]
fn parse_four_element_list() {
    let got = parse_one("(1 2 3 4)");
    assert_eq!(
        got,
        Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3), Sexp::Int(4),])
    );
}

#[test]
fn parse_two_element_list() {
    let got = parse_one("(1 2)");
    assert_eq!(got, Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]));
}

#[test]
fn parse_one_then_inner() {
    let got = parse_one("(1 (2))");
    assert_eq!(
        got,
        Sexp::list_from(&[Sexp::Int(1), Sexp::list_from(&[Sexp::Int(2)]),])
    );
}

#[test]
fn parse_mixed_atom_list() {
    let got = parse_one("(foo 42 bar)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("foo".to_string()),
            Sexp::Int(42),
            Sexp::Symbol("bar".to_string()),
        ])
    );
}

// ---------- dotted pair ----------

#[test]
fn parse_dotted_pair() {
    let got = parse_one("(a . b)");
    assert_eq!(
        got,
        Sexp::cons(Sexp::Symbol("a".to_string()), Sexp::Symbol("b".to_string()),)
    );
}

// ---------- nested lists ----------

#[test]
fn parse_nested_list_left() {
    // ((a b) c)
    let got = parse_one("((a b) c)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::list_from(&[Sexp::Symbol("a".to_string()), Sexp::Symbol("b".to_string()),]),
            Sexp::Symbol("c".to_string()),
        ])
    );
}

#[test]
fn parse_nested_list_right() {
    // (a (b c))
    let got = parse_one("(a (b c))");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("a".to_string()),
            Sexp::list_from(&[Sexp::Symbol("b".to_string()), Sexp::Symbol("c".to_string()),]),
        ])
    );
}

#[test]
fn parse_deeply_nested() {
    // ((((1))))
    let got = parse_one("((((1))))");
    let mut expected = Sexp::list_from(&[Sexp::Int(1)]);
    for _ in 0..3 {
        expected = Sexp::list_from(&[expected]);
    }
    assert_eq!(got, expected);
}

// ---------- quote family ----------

#[test]
fn parse_quote_symbol() {
    // 'x -> (quote x)
    let got = parse_one("'x");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("quote".to_string()),
            Sexp::Symbol("x".to_string()),
        ])
    );
}

#[test]
fn parse_quote_int() {
    // '7 -> (quote 7)
    let got = parse_one("'7");
    assert_eq!(
        got,
        Sexp::list_from(&[Sexp::Symbol("quote".to_string()), Sexp::Int(7),])
    );
}

#[test]
fn parse_quote_list() {
    // '(a b) -> (quote (a b))
    let got = parse_one("'(a b)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("quote".to_string()),
            Sexp::list_from(&[Sexp::Symbol("a".to_string()), Sexp::Symbol("b".to_string()),]),
        ])
    );
}

#[test]
fn parse_backquote_atom() {
    let got = parse_one("`x");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("backquote".to_string()),
            Sexp::Symbol("x".to_string()),
        ])
    );
}

#[test]
fn parse_comma_atom() {
    let got = parse_one(",foo");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("comma".to_string()),
            Sexp::Symbol("foo".to_string()),
        ])
    );
}

#[test]
fn parse_comma_at_atom() {
    let got = parse_one(",@xs");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("comma-at".to_string()),
            Sexp::Symbol("xs".to_string()),
        ])
    );
}

#[test]
fn parse_function_quote_atom() {
    let got = parse_one("#'foo");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("function".to_string()),
            Sexp::Symbol("foo".to_string()),
        ])
    );
}

// ---------- whitespace handling ----------

#[test]
fn parse_leading_whitespace() {
    assert_eq!(parse_one("  \n  42"), Sexp::Int(42));
}

#[test]
fn parse_comments_between_atoms() {
    let got = parse_one("(1 ; comment\n 2)");
    assert_eq!(got, Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]));
}

// ---------- two-level quote ----------

#[test]
fn parse_quote_quote() {
    // ''x -> (quote (quote x))
    let got = parse_one("''x");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("quote".to_string()),
            Sexp::list_from(&[
                Sexp::Symbol("quote".to_string()),
                Sexp::Symbol("x".to_string()),
            ]),
        ])
    );
}

// ---------- Doc 116 §116.B+ char literals (kind 24) ----------

#[test]
fn parse_char_literal_plain_ascii() {
    // `?a' -> Sexp::Int(97).
    assert_eq!(parse_one("?a"), Sexp::Int(97));
}

#[test]
fn parse_char_literal_named_escape_newline() {
    // `?\n' -> 10.
    assert_eq!(parse_one("?\\n"), Sexp::Int(10));
}

#[test]
fn parse_char_literal_named_escape_tab() {
    assert_eq!(parse_one("?\\t"), Sexp::Int(9));
}

#[test]
fn parse_char_literal_named_escape_space() {
    assert_eq!(parse_one("?\\s"), Sexp::Int(32));
}

#[test]
fn parse_char_literal_hex() {
    // `?\xff' -> 255.
    assert_eq!(parse_one("?\\xff"), Sexp::Int(255));
}

#[test]
fn parse_char_literal_control() {
    // `?\C-a' -> 1.
    assert_eq!(parse_one("?\\C-a"), Sexp::Int(1));
}

#[test]
fn parse_char_literal_paren_escape() {
    // `?\(' -> 40 (= literal `(').
    assert_eq!(parse_one("?\\("), Sexp::Int(40));
}

// ---------- Doc 116 §116.B+ radix integers (kind 25) ----------

#[test]
fn parse_radix_int_hex() {
    assert_eq!(parse_one("#x10"), Sexp::Int(16));
}

#[test]
fn parse_radix_int_octal() {
    assert_eq!(parse_one("#o17"), Sexp::Int(15));
}

#[test]
fn parse_radix_int_binary() {
    assert_eq!(parse_one("#b1010"), Sexp::Int(10));
}

#[test]
fn parse_radix_int_hex_negative() {
    assert_eq!(parse_one("#x-ff"), Sexp::Int(-255));
}

#[test]
fn parse_radix_int_upper_x() {
    assert_eq!(parse_one("#X10"), Sexp::Int(16));
}

// ---------- Doc 116 §116.B+ bare-sign symbols ----------

#[test]
fn parse_bare_plus_as_symbol() {
    // `(+ x y)' — the `+' parses as the symbol `+'.
    let got = parse_one("(+ x y)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("+".to_string()),
            Sexp::Symbol("x".to_string()),
            Sexp::Symbol("y".to_string()),
        ])
    );
}

#[test]
fn parse_bare_minus_as_symbol() {
    let got = parse_one("(- a b)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("-".to_string()),
            Sexp::Symbol("a".to_string()),
            Sexp::Symbol("b".to_string()),
        ])
    );
}

#[test]
fn parse_signed_int_still_works() {
    // Regression: `+42' must still parse as Int(42).
    assert_eq!(parse_one("+42"), Sexp::Int(42));
}

#[test]
fn parse_negative_int_in_list() {
    let got = parse_one("(- 1 -2)");
    assert_eq!(
        got,
        Sexp::list_from(&[Sexp::Symbol("-".to_string()), Sexp::Int(1), Sexp::Int(-2),])
    );
}

// ---------- Doc 122 §122.G — Float kind 21 ----------

#[test]
fn parse_simple_float() {
    // Basic decimal — covers the `digits . digits' path.
    assert_eq!(parse_one("1.5"), Sexp::Float(1.5));
}

#[test]
fn parse_float_with_exponent() {
    // Exponent path — covers the `digits e digits' form.
    assert_eq!(parse_one("1e3"), Sexp::Float(1000.0));
}

#[test]
fn parse_negative_float() {
    // Leading sign + decimal.
    assert_eq!(parse_one("-0.5"), Sexp::Float(-0.5));
}

#[test]
fn parse_float_zero() {
    // `0.0' must parse to Sexp::Float(0.0), not Sexp::Int(0).
    assert_eq!(parse_one("0.0"), Sexp::Float(0.0));
}

#[test]
fn parse_negative_float_with_exponent() {
    // Combined sign + decimal + signed exponent.
    assert_eq!(parse_one("-1.5e-2"), Sexp::Float(-0.015));
}

#[test]
fn parse_float_in_list() {
    // Float inside a list — exercises the per-depth recursion path
    // for kind 21 just like Int / Sym / Str.
    let got = parse_one("(+ 1.5 2.5)");
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("+".to_string()),
            Sexp::Float(1.5),
            Sexp::Float(2.5),
        ])
    );
}
