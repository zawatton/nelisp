//! Doc 116 §116.A probes — direct calls into the Phase 47-compiled
//! pure-elisp Reader lexer (`nelisp_reader_lex_one').  Pattern mirrors
//! `elisp_cc_mut_str_probe.rs' / `elisp_cc_utf8_probe.rs'.
//!
//! Each test allocates a `Sexp::Str' source, a `Sexp::Nil' payload
//! slot, a `Sexp::Nil' cursor-out slot, and a fresh `Sexp::MutStr'
//! scratch slot via `mut_str_make_empty', then calls
//! `reader_lex_one' once and inspects the returned kind code plus
//! the side-effect outputs.  Covers all 16+ token kinds the §116.A
//! scope discipline targets:
//!
//!   - Single-byte punctuation: ( ) [ ] ' ` , .
//!   - Two-byte tokens: `,@', `#''.
//!   - Three-byte token: `#s('.
//!   - Atom kinds: Int / Float / Sym.
//!   - String literals with `\\n \\t \\\\ \\\"' escapes.
//!   - Whitespace + comment skip + EOF.
//!   - Error path: lone `#X' (unknown sharpsign form).

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::elisp_cc_spike;
use nelisp_build_tool::eval::sexp::Sexp;

// ---------- helpers ----------

/// Lex one token from SOURCE starting at CURSOR.  Returns
/// (kind, next-cursor, payload-as-string-if-any).
fn lex_one_at(source: &str, cursor: i64) -> (i64, i64, Option<String>) {
    let src = Sexp::Str(source.to_string());
    let mut payload_slot = Sexp::Nil;
    let mut cursor_out_slot = Sexp::Nil;
    let mut scratch_slot = Sexp::Nil;
    unsafe {
        elisp_cc_spike::mut_str_make_empty(&mut scratch_slot as *mut Sexp, 64);
    }
    let kind = unsafe {
        elisp_cc_spike::reader_lex_one(
            &src as *const Sexp,
            cursor,
            &mut payload_slot as *mut Sexp,
            &mut cursor_out_slot as *mut Sexp,
            &mut scratch_slot as *mut Sexp,
        )
    };
    let next_cursor = match cursor_out_slot {
        Sexp::Int(n) => n,
        ref other => {
            panic!(
                "cursor_out_slot must be Sexp::Int after lex; got {:?}",
                other
            )
        }
    };
    let payload = match payload_slot {
        Sexp::Str(ref text) => Some(text.clone()),
        Sexp::Nil => None,
        ref other => {
            panic!(
                "payload_slot must be Sexp::Str or Sexp::Nil after lex; got {:?}",
                other
            )
        }
    };
    (kind, next_cursor, payload)
}

/// Convenience wrapper: lex one token from byte 0.
fn lex_one(source: &str) -> (i64, i64, Option<String>) {
    lex_one_at(source, 0)
}

// ---------- single-byte punctuation tokens ----------

#[test]
fn lex_lparen() {
    let (kind, next, payload) = lex_one("(");
    assert_eq!(kind, 1, "`(' must lex as LParen (kind 1)");
    assert_eq!(next, 1);
    assert!(payload.is_none());
}

#[test]
fn lex_rparen() {
    let (kind, next, payload) = lex_one(")");
    assert_eq!(kind, 2);
    assert_eq!(next, 1);
    assert!(payload.is_none());
}

#[test]
fn lex_lbracket_and_rbracket() {
    let (kind, next, _) = lex_one("[");
    assert_eq!(kind, 3, "`[' kind");
    assert_eq!(next, 1);
    let (kind, next, _) = lex_one("]");
    assert_eq!(kind, 4, "`]' kind");
    assert_eq!(next, 1);
}

#[test]
fn lex_quote() {
    let (kind, next, _) = lex_one("'foo");
    assert_eq!(kind, 5, "`'' kind");
    assert_eq!(next, 1, "quote advances 1 byte");
}

#[test]
fn lex_backquote() {
    let (kind, next, _) = lex_one("`x");
    assert_eq!(kind, 6, "`\\`' kind");
    assert_eq!(next, 1);
}

// ---------- comma + comma-at ----------

#[test]
fn lex_comma_followed_by_symbol() {
    let (kind, next, _) = lex_one(",foo");
    assert_eq!(kind, 7, "`,' kind");
    assert_eq!(next, 1);
}

#[test]
fn lex_comma_at() {
    let (kind, next, _) = lex_one(",@xs");
    assert_eq!(kind, 8, "`,@' kind");
    assert_eq!(next, 2, "comma-at advances 2 bytes");
}

// ---------- sharpsign forms ----------

#[test]
fn lex_function_quote() {
    let (kind, next, _) = lex_one("#'fn-ref");
    assert_eq!(kind, 9, "`#\\'' kind");
    assert_eq!(next, 2, "function-quote advances 2 bytes");
}

#[test]
fn lex_sharps_paren() {
    let (kind, next, _) = lex_one("#s(my-record)");
    assert_eq!(kind, 11, "`#s(' kind");
    assert_eq!(next, 3, "sharps-paren advances 3 bytes");
}

#[test]
fn lex_unknown_sharpsign_errors() {
    let (kind, _, _) = lex_one("#z");
    assert_eq!(kind, -1, "`#z' is an unknown sharpsign form");
}

// ---------- numbers ----------

#[test]
fn lex_simple_int() {
    let (kind, next, payload) = lex_one("42");
    assert_eq!(kind, 20, "Int kind");
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("42"));
}

#[test]
fn lex_negative_int() {
    let (kind, next, payload) = lex_one("-7");
    assert_eq!(kind, 20);
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("-7"));
}

#[test]
fn lex_positive_int() {
    let (kind, next, payload) = lex_one("+3");
    assert_eq!(kind, 20);
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("+3"));
}

#[test]
fn lex_int_then_paren_terminator() {
    // `42)' -> Int(42); RParen left for next call.
    let (kind, next, payload) = lex_one("42)");
    assert_eq!(kind, 20);
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("42"));
}

#[test]
fn lex_float_with_dot() {
    let (kind, next, payload) = lex_one("3.14");
    assert_eq!(kind, 21, "Float kind");
    assert_eq!(next, 4);
    assert_eq!(payload.as_deref(), Some("3.14"));
}

#[test]
fn lex_float_leading_dot() {
    let (kind, next, payload) = lex_one(".5");
    assert_eq!(kind, 21);
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some(".5"));
}

#[test]
fn lex_float_trailing_dot() {
    let (kind, next, payload) = lex_one("1.");
    assert_eq!(kind, 21);
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("1."));
}

#[test]
fn lex_float_with_exponent() {
    let (kind, next, payload) = lex_one("1e3");
    assert_eq!(kind, 21);
    assert_eq!(next, 3);
    assert_eq!(payload.as_deref(), Some("1e3"));
}

#[test]
fn lex_float_negative_with_exponent() {
    let (kind, next, payload) = lex_one("-1.5e-2");
    assert_eq!(kind, 21);
    assert_eq!(next, 7);
    assert_eq!(payload.as_deref(), Some("-1.5e-2"));
}

// ---------- dot token (lone `.') ----------

#[test]
fn lex_dot_token_lone() {
    // Surround the dot with whitespace so the lexer sees a lone `.'.
    let (kind, next, payload) = lex_one_at("(a . b)", 3);
    assert_eq!(kind, 10, "lone `.' is the dot token (kind 10)");
    assert_eq!(next, 4);
    assert!(payload.is_none(), "dot token has no payload");
}

// ---------- symbols ----------

#[test]
fn lex_plain_symbol() {
    let (kind, next, payload) = lex_one("foo");
    assert_eq!(kind, 23, "Sym kind");
    assert_eq!(next, 3);
    assert_eq!(payload.as_deref(), Some("foo"));
}

#[test]
fn lex_symbol_with_punctuation() {
    let (kind, next, payload) = lex_one("foo-bar?");
    assert_eq!(kind, 23);
    assert_eq!(next, 8);
    assert_eq!(payload.as_deref(), Some("foo-bar?"));
}

#[test]
fn lex_symbol_star() {
    let (kind, next, payload) = lex_one("let*");
    assert_eq!(kind, 23);
    assert_eq!(next, 4);
    assert_eq!(payload.as_deref(), Some("let*"));
}

#[test]
fn lex_symbol_ns_colon_slash() {
    let (kind, next, payload) = lex_one("my:ns/name");
    assert_eq!(kind, 23);
    assert_eq!(next, 10);
    assert_eq!(payload.as_deref(), Some("my:ns/name"));
}

// ---------- strings ----------

#[test]
fn lex_empty_string() {
    let (kind, next, payload) = lex_one("\"\"");
    assert_eq!(kind, 22, "Str kind");
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some(""));
}

#[test]
fn lex_simple_string() {
    let (kind, next, payload) = lex_one("\"hello\"");
    assert_eq!(kind, 22);
    assert_eq!(next, 7);
    assert_eq!(payload.as_deref(), Some("hello"));
}

#[test]
fn lex_string_escape_newline() {
    let (kind, next, payload) = lex_one("\"a\\nb\"");
    assert_eq!(kind, 22);
    assert_eq!(next, 6);
    assert_eq!(payload.as_deref(), Some("a\nb"));
}

#[test]
fn lex_string_escape_tab() {
    let (kind, next, payload) = lex_one("\"x\\ty\"");
    assert_eq!(kind, 22);
    assert_eq!(next, 6);
    assert_eq!(payload.as_deref(), Some("x\ty"));
}

#[test]
fn lex_string_escape_backslash() {
    let (kind, next, payload) = lex_one("\"a\\\\b\"");
    assert_eq!(kind, 22);
    assert_eq!(next, 6);
    assert_eq!(payload.as_deref(), Some("a\\b"));
}

#[test]
fn lex_string_escape_double_quote() {
    let (kind, next, payload) = lex_one("\"a\\\"b\"");
    assert_eq!(kind, 22);
    assert_eq!(next, 6);
    assert_eq!(payload.as_deref(), Some("a\"b"));
}

#[test]
fn lex_unterminated_string_returns_error() {
    let (kind, _, _) = lex_one("\"oops");
    assert_eq!(kind, -1, "unterminated string must lex as -1");
}

// ---------- whitespace + comments ----------

#[test]
fn lex_skips_leading_whitespace_to_int() {
    let (kind, next, payload) = lex_one("  \t\n  42");
    assert_eq!(kind, 20);
    assert_eq!(next, 8);
    assert_eq!(payload.as_deref(), Some("42"));
}

#[test]
fn lex_skips_line_comment() {
    let (kind, next, payload) = lex_one("; comment\nfoo");
    assert_eq!(kind, 23);
    assert_eq!(next, 13);
    assert_eq!(payload.as_deref(), Some("foo"));
}

#[test]
fn lex_skips_only_whitespace_then_eof() {
    let (kind, _, _) = lex_one("   \t\n  ");
    assert_eq!(kind, 0, "whitespace-only input lexes as EOF");
}

#[test]
fn lex_skips_only_comment_then_eof() {
    let (kind, _, _) = lex_one("; nothing else");
    assert_eq!(kind, 0, "comment-only input lexes as EOF");
}

#[test]
fn lex_empty_source_is_eof() {
    let (kind, next, payload) = lex_one("");
    assert_eq!(kind, 0);
    assert_eq!(next, 0);
    assert!(payload.is_none());
}

// ---------- iteration: cursor threading across multiple tokens ----------

#[test]
fn lex_can_iterate_a_simple_form() {
    // Walk "(foo 42)" by hand, asserting each successive token.
    let src = "(foo 42)";
    let (k, c, p) = lex_one_at(src, 0);
    assert_eq!(k, 1, "first token = LParen");
    assert_eq!(c, 1);
    assert!(p.is_none());

    let (k, c, p) = lex_one_at(src, c);
    assert_eq!(k, 23, "second token = Sym(foo)");
    assert_eq!(c, 4);
    assert_eq!(p.as_deref(), Some("foo"));

    let (k, c, p) = lex_one_at(src, c);
    assert_eq!(k, 20, "third token = Int(42)");
    assert_eq!(c, 7);
    assert_eq!(p.as_deref(), Some("42"));

    let (k, c, p) = lex_one_at(src, c);
    assert_eq!(k, 2, "fourth token = RParen");
    assert_eq!(c, 8);
    assert!(p.is_none());

    let (k, _, _) = lex_one_at(src, c);
    assert_eq!(k, 0, "fifth token = EOF");
}

#[test]
fn lex_can_iterate_a_dotted_pair() {
    let src = "(a . b)";
    let mut cursor = 0;
    let mut kinds: Vec<i64> = Vec::new();
    let mut texts: Vec<Option<String>> = Vec::new();
    loop {
        let (k, c, p) = lex_one_at(src, cursor);
        if k == 0 {
            kinds.push(k);
            break;
        }
        kinds.push(k);
        texts.push(p);
        cursor = c;
    }
    // Expected stream: LParen, Sym(a), Dot, Sym(b), RParen, EOF.
    assert_eq!(kinds, vec![1, 23, 10, 23, 2, 0]);
    assert_eq!(texts[0], None);
    assert_eq!(texts[1].as_deref(), Some("a"));
    assert_eq!(texts[2], None, "dot token has no payload");
    assert_eq!(texts[3].as_deref(), Some("b"));
    assert_eq!(texts[4], None);
}

#[test]
fn lex_can_iterate_quasiquote_form() {
    let src = "`(a ,b ,@c)";
    let mut cursor = 0;
    let mut kinds: Vec<i64> = Vec::new();
    loop {
        let (k, c, _) = lex_one_at(src, cursor);
        kinds.push(k);
        if k == 0 || k < 0 {
            break;
        }
        cursor = c;
    }
    // Backquote, LParen, Sym, Comma, Sym, CommaAt, Sym, RParen, EOF.
    assert_eq!(kinds, vec![6, 1, 23, 7, 23, 8, 23, 2, 0]);
}

#[test]
fn lex_function_quote_then_symbol() {
    let src = "#'my-fn";
    let (k, c, _) = lex_one_at(src, 0);
    assert_eq!(k, 9);
    assert_eq!(c, 2);
    let (k, c, p) = lex_one_at(src, c);
    assert_eq!(k, 23);
    assert_eq!(c, 7);
    assert_eq!(p.as_deref(), Some("my-fn"));
}

// ---------- Doc 116 §116.B+ char literals (kind 24) ----------

#[test]
fn lex_char_literal_plain_ascii() {
    let (kind, next, payload) = lex_one("?a");
    assert_eq!(kind, 24, "`?a' lexes as Char (kind 24)");
    assert_eq!(next, 2);
    assert_eq!(payload.as_deref(), Some("a"));
}

#[test]
fn lex_char_literal_escape_newline() {
    let (kind, next, payload) = lex_one("?\\n");
    assert_eq!(kind, 24);
    assert_eq!(next, 3);
    assert_eq!(payload.as_deref(), Some("\\n"));
}

#[test]
fn lex_char_literal_ctrl() {
    let (kind, next, payload) = lex_one("?\\C-a");
    assert_eq!(kind, 24);
    assert_eq!(next, 5);
    assert_eq!(payload.as_deref(), Some("\\C-a"));
}

#[test]
fn lex_char_literal_hex() {
    let (kind, next, payload) = lex_one("?\\xff");
    assert_eq!(kind, 24);
    assert_eq!(next, 5);
    assert_eq!(payload.as_deref(), Some("\\xff"));
}

#[test]
fn lex_char_literal_paren_escape() {
    // Doc 51 Phase 3-A''-1 — `?\(' must read as kind 24 with body `\('
    // so the parser-side decoder produces 40 (= literal `(').
    let (kind, next, payload) = lex_one("?\\(");
    assert_eq!(kind, 24);
    assert_eq!(next, 3);
    assert_eq!(payload.as_deref(), Some("\\("));
}

#[test]
fn lex_bare_question_is_symbol() {
    // Bare `?' followed by whitespace is the symbol named `?'.  This
    // matches the Rust legacy dispatcher.
    let (kind, _next, payload) = lex_one("? ");
    assert_eq!(kind, 23, "bare `?' is Sym");
    assert_eq!(payload.as_deref(), Some("?"));
}

// ---------- Doc 116 §116.B+ radix integers (kind 25) ----------

#[test]
fn lex_radix_int_hex() {
    let (kind, next, payload) = lex_one("#x10");
    assert_eq!(kind, 25, "`#x10' lexes as RadixInt");
    assert_eq!(next, 4);
    // Payload = base marker `x' + digits `10'.
    assert_eq!(payload.as_deref(), Some("x10"));
}

#[test]
fn lex_radix_int_octal() {
    let (kind, next, payload) = lex_one("#o17");
    assert_eq!(kind, 25);
    assert_eq!(next, 4);
    assert_eq!(payload.as_deref(), Some("o17"));
}

#[test]
fn lex_radix_int_binary() {
    let (kind, next, payload) = lex_one("#b1010");
    assert_eq!(kind, 25);
    assert_eq!(next, 6);
    assert_eq!(payload.as_deref(), Some("b1010"));
}

#[test]
fn lex_radix_int_hex_signed() {
    let (kind, next, payload) = lex_one("#x-ff");
    assert_eq!(kind, 25);
    assert_eq!(next, 5);
    assert_eq!(payload.as_deref(), Some("x-ff"));
}

#[test]
fn lex_radix_int_upper_x() {
    // `#X10' should normalise to lowercase `x' marker in payload.
    let (kind, _next, payload) = lex_one("#X10");
    assert_eq!(kind, 25);
    assert_eq!(payload.as_deref(), Some("x10"));
}

// ---------- Doc 116 §116.B+ bare-sign symbols ----------

#[test]
fn lex_bare_plus_is_symbol() {
    // `(+ x y)' — the `+' must lex as Sym, not Int.  Standalone test
    // assertion for the bare-sign fix.
    let (kind, next, payload) = lex_one("+");
    assert_eq!(kind, 23, "lone `+' is Sym");
    assert_eq!(next, 1);
    assert_eq!(payload.as_deref(), Some("+"));
}

#[test]
fn lex_bare_minus_is_symbol() {
    let (kind, _next, payload) = lex_one("-");
    assert_eq!(kind, 23, "lone `-' is Sym");
    assert_eq!(payload.as_deref(), Some("-"));
}

#[test]
fn lex_bare_dot_followed_by_terminator_is_dot_token() {
    // Lone `.' is the dot token (kind 10), not a symbol.  The
    // dispatch arm in `nelisp_reader_lex_atom_finalize' handles this.
    // Surround with space so it terminates as a lone `.'.
    let (kind, next, _payload) = lex_one(" . ");
    assert_eq!(kind, 10, "lone `.' is the Dot token");
    assert_eq!(next, 2);
}

#[test]
fn lex_sign_then_symbol_is_symbol() {
    // `+foo' has no digits = Sym, not Int.  The saw-digit bit fix.
    let (kind, _next, payload) = lex_one("+foo");
    assert_eq!(kind, 23, "`+foo' is Sym");
    assert_eq!(payload.as_deref(), Some("+foo"));
}

#[test]
fn lex_signed_int_still_int() {
    // Regression: `+42' must still lex as Int.
    let (kind, _next, payload) = lex_one("+42");
    assert_eq!(kind, 20, "`+42' is Int");
    assert_eq!(payload.as_deref(), Some("+42"));
}
