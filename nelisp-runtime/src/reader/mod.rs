//! Phase 8.0.1 (= Doc 44 §3.2 LOCKED) — Rust-side minimal Elisp
//! reader / sexp parser.
//!
//! Public surface:
//!   - [`read_str`]  — parse exactly one form, error on trailing junk
//!   - [`read_all`]  — parse every top-level form (`.el` file shape)
//!   - [`fmt_sexp`]  — debug pretty-printer (lossy, NOT prin1)
//!   - [`Sexp`]      — value enum
//!   - [`ReadError`] — failure type with explicit
//!     `NotYetImplemented` for remaining deferred features
//!
//! Layer rule (= prompt constraint): the reader must NOT depend on
//! the evaluator.  Phase 7.5.4.2 is responsible for taking `Sexp` and
//! producing `LispObject`; this module stops at the syntactic value.
//!
//! Remaining deferred per Doc 44 §3.2:
//!   - meta char literal modifiers (`?\M-a`, multi-modifier variants)
//!   - `#[...]` byte-code literal
//!   - `#s(...)` structure literal
//!   - multibyte / non-ASCII string content (currently passed through
//!     as raw bytes; Phase 7.4 NeLisp coding will own real decoding)

pub mod error;
pub mod lexer;
pub mod parser;
pub mod sexp;

pub use error::{ReadError, SourcePos};
pub use sexp::{fmt_sexp, Sexp};

/// Parse exactly one top-level form from `input`.  Trailing
/// non-whitespace tokens (after one form is read) are an error — use
/// [`read_all`] if you want every form.
pub fn read_str(input: &str) -> Result<Sexp, ReadError> {
    let tokens = lexer::tokenize(input)?;
    let (form, consumed) = parser::parse_one(&tokens)?;
    if consumed < tokens.len() {
        let next = &tokens[consumed];
        return Err(ReadError::parse(
            format!("trailing token after first form: {:?}", next.token),
            next.pos,
        ));
    }
    Ok(form)
}

/// Parse every top-level form in `input`.  Returns an empty vector
/// if the input is empty (or whitespace + comments only).
pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    let tokens = lexer::tokenize(input)?;
    parser::parse_all(&tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Doc 44 §3.2 ERT smoke #1: trivial atoms.
    #[test]
    fn smoke_atoms() {
        assert_eq!(read_str("123").unwrap(), Sexp::Int(123));
        assert_eq!(read_str("3.14").unwrap(), Sexp::Float(3.14));
        assert_eq!(read_str("foo").unwrap(), Sexp::Symbol("foo".into()));
        assert_eq!(read_str("nil").unwrap(), Sexp::Nil);
        assert_eq!(read_str("t").unwrap(), Sexp::T);
    }

    /// String escapes per prompt scope.
    #[test]
    fn smoke_string_escapes() {
        assert_eq!(read_str("\"hello\\n\"").unwrap(), Sexp::Str("hello\n".into()));
        assert_eq!(read_str("\"a\\tb\"").unwrap(), Sexp::Str("a\tb".into()));
        assert_eq!(read_str("\"\\\\\"").unwrap(), Sexp::Str("\\".into()));
        assert_eq!(read_str("\"\\\"\"").unwrap(), Sexp::Str("\"".into()));
    }

    /// Symbols allow alnum + - _ . :
    #[test]
    fn smoke_symbol_punctuation() {
        assert_eq!(
            read_str("foo-bar").unwrap(),
            Sexp::Symbol("foo-bar".into())
        );
        assert_eq!(
            read_str("ns:name").unwrap(),
            Sexp::Symbol("ns:name".into())
        );
        assert_eq!(
            read_str("file.ext").unwrap(),
            Sexp::Symbol("file.ext".into())
        );
        assert_eq!(read_str("a_b").unwrap(), Sexp::Symbol("a_b".into()));
    }

    /// `(a b c)` → cons chain.
    #[test]
    fn smoke_proper_list() {
        let got = read_str("(a b c)").unwrap();
        let expected = Sexp::list_from(&[
            Sexp::Symbol("a".into()),
            Sexp::Symbol("b".into()),
            Sexp::Symbol("c".into()),
        ]);
        assert_eq!(got, expected);
    }

    /// `(a . b)` → dotted pair.
    #[test]
    fn smoke_dotted_pair() {
        let got = read_str("(a . b)").unwrap();
        let expected = Sexp::Cons(
            Box::new(Sexp::Symbol("a".into())),
            Box::new(Sexp::Symbol("b".into())),
        );
        assert_eq!(got, expected);
    }

    /// `[a b c]` → vector literal.
    #[test]
    fn smoke_vector() {
        let got = read_str("[1 2 3]").unwrap();
        let expected = Sexp::Vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        assert_eq!(got, expected);
    }

    /// `'x` → `(quote x)`.
    #[test]
    fn smoke_quote() {
        let got = read_str("'x").unwrap();
        assert_eq!(got, Sexp::quote(Sexp::Symbol("x".into())));
    }

    /// Backquote / unquote / splice desugar to tagged lists.
    #[test]
    fn smoke_backquote_family() {
        let got = read_str("`(a ,b ,@c)").unwrap();
        assert_eq!(
            got,
            Sexp::backquote(Sexp::list_from(&[
                Sexp::Symbol("a".into()),
                Sexp::comma(Sexp::Symbol("b".into())),
                Sexp::comma_at(Sexp::Symbol("c".into())),
            ]))
        );
    }

    /// Character literals read as integer codepoints.
    #[test]
    fn smoke_char_literals() {
        assert_eq!(read_str("?a").unwrap(), Sexp::Int(97));
        assert_eq!(read_str("?\\xff").unwrap(), Sexp::Int(255));
        assert_eq!(read_str("?\\C-a").unwrap(), Sexp::Int(1));
    }

    /// `#'foo` → `(function foo)`.
    #[test]
    fn smoke_function_quote() {
        let got = read_str("#'foo").unwrap();
        assert_eq!(got, Sexp::function(Sexp::Symbol("foo".into())));
    }

    /// Backslash-newline in strings is a line continuation.
    #[test]
    fn smoke_string_backslash_newline() {
        assert_eq!(
            read_str("\"hi\\\nthere\"").unwrap(),
            Sexp::Str("hithere".into())
        );
    }

    /// Comment skip — the `;` line is gone, the int after stands.
    #[test]
    fn smoke_comment_skip() {
        let got = read_str("; comment\n42").unwrap();
        assert_eq!(got, Sexp::Int(42));
    }

    /// Unbalanced paren is a hard error (not silent).
    #[test]
    fn smoke_unbalanced_paren_errors() {
        assert!(read_str("(a b").is_err());
        assert!(read_str("a)").is_err());
    }

    /// Each deferred feature returns NotYetImplemented (= explicit).
    #[test]
    fn deferred_features_explicit() {
        for src in &["?\\M-a", "#[1 2]", "#s(x)"] {
            match read_str(src) {
                Err(ReadError::NotYetImplemented { .. }) => (),
                Err(other) => panic!("expected NotYetImplemented for {:?}, got {:?}", src, other),
                Ok(v) => panic!("expected NotYetImplemented for {:?}, got Ok({:?})", src, v),
            }
        }
    }

    /// Doc 44 §3.2 ERT smoke #6 — five-deep nesting.
    #[test]
    fn smoke_deep_nest() {
        let got = read_str("(((((1)))))").unwrap();
        let mut expected = Sexp::list_from(&[Sexp::Int(1)]);
        for _ in 0..4 {
            expected = Sexp::list_from(&[expected]);
        }
        assert_eq!(got, expected);
    }

    /// Doc 44 §3.2 mentions hex/oct/bin radix integers.
    #[test]
    fn smoke_radix_integers() {
        assert_eq!(read_str("#x10").unwrap(), Sexp::Int(16));
        assert_eq!(read_str("#o17").unwrap(), Sexp::Int(15));
        assert_eq!(read_str("#b1010").unwrap(), Sexp::Int(10));
    }

    /// Float w/ exponent.
    #[test]
    fn smoke_float_exponent() {
        assert_eq!(read_str("1e3").unwrap(), Sexp::Float(1000.0));
        assert_eq!(read_str("-1.5e-2").unwrap(), Sexp::Float(-0.015));
    }

    /// `read_all` consumes multiple forms.
    #[test]
    fn read_all_multi_form() {
        let got = read_all("1 2 3 ; tail comment\n").unwrap();
        assert_eq!(got, vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
    }

    /// `read_str` rejects trailing forms.
    #[test]
    fn read_str_rejects_trailing() {
        assert!(read_str("1 2").is_err());
    }

    /// `read_all` on empty / whitespace-only returns empty Vec.
    #[test]
    fn read_all_empty_inputs() {
        assert_eq!(read_all("").unwrap(), Vec::<Sexp>::new());
        assert_eq!(read_all("   ; only comment\n").unwrap(), Vec::<Sexp>::new());
    }

    /// `fmt_sexp` round-trip on a non-trivial form.
    #[test]
    fn fmt_sexp_roundtrip_shape() {
        let src = "(let ((x 1) (y 2)) (+ x y))";
        let parsed = read_str(src).unwrap();
        let printed = fmt_sexp(&parsed);
        // Exact string match — printer must produce the canonical
        // form Elisp's prin1 would (modulo whitespace normalisation).
        assert_eq!(printed, "(let ((x 1) (y 2)) (+ x y))");
        // And the printed form re-reads to an equivalent value.
        assert_eq!(read_str(&printed).unwrap(), parsed);
    }

    #[test]
    fn fmt_sexp_roundtrip_new_reader_forms() {
        let src = "(a `(b ,c ,@d) ?x #'foo \"hi\\\nthere\")";
        let parsed = read_str(src).unwrap();
        let printed = fmt_sexp(&parsed);
        assert_eq!(printed, "(a `(b ,c ,@d) 120 #'foo \"hithere\")");
        assert_eq!(read_str(&printed).unwrap(), parsed);
    }
}
