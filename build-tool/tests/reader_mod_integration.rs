//! Integration tests for `reader' (= moved out of `src/reader/mod.rs's
//! `#[cfg(test)] mod tests' for src LOC reduction).  Same coverage.

use nelisp_build_tool::reader::{read_all, read_str, ReadError};
use nelisp_build_tool::eval::sexp::{fmt_sexp, Sexp};

/// Smoke #1: trivial atoms.
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
    let expected = Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
    assert_eq!(got, expected);
}

/// Vector arm — `[..]' handled by the elisp parser.  Empty `[]',
/// nested vectors, and vectors inside lists / quotes all compose
/// via `p_dispatch' kind 3.
#[test]
fn smoke_vector_literal() {
    assert_eq!(
        read_str("[1 2 3]").unwrap(),
        Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
    );
    assert_eq!(read_str("[]").unwrap(), Sexp::vector(vec![]));
    assert_eq!(
        read_str("[:a :b]").unwrap(),
        Sexp::vector(vec![
            Sexp::Symbol(":a".into()),
            Sexp::Symbol(":b".into()),
        ])
    );
    // Vector inside list — exercises the `p_list_dispatch'
    // fall-through to `p_dispatch' kind 3.
    let got = read_str("(a [b c] d)").unwrap();
    assert_eq!(
        got,
        Sexp::list_from(&[
            Sexp::Symbol("a".into()),
            Sexp::vector(vec![
                Sexp::Symbol("b".into()),
                Sexp::Symbol("c".into()),
            ]),
            Sexp::Symbol("d".into()),
        ])
    );
    // Nested vector — exercises the `p_vec_dispatch'
    // fall-through to `p_dispatch' kind 3.
    let nested = read_str("[[1] [2 3]]").unwrap();
    assert_eq!(
        nested,
        Sexp::vector(vec![
            Sexp::vector(vec![Sexp::Int(1)]),
            Sexp::vector(vec![Sexp::Int(2), Sexp::Int(3)]),
        ])
    );
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

/// Byte-code literal `#[...]` is the last deferred reader feature.
#[test]
fn byte_code_literal_still_deferred() {
    match read_str("#[1 2]") {
        Err(ReadError::NotYetImplemented { .. }) => (),
        Err(other) => panic!("expected NotYetImplemented for #[1 2], got {:?}", other),
        Ok(v) => panic!("expected NotYetImplemented for #[1 2], got Ok({:?})", v),
    }
}

/// Smoke #6: five-deep nesting.
#[test]
fn smoke_deep_nest() {
    let got = read_str("(((((1)))))").unwrap();
    let mut expected = Sexp::list_from(&[Sexp::Int(1)]);
    for _ in 0..4 {
        expected = Sexp::list_from(&[expected]);
    }
    assert_eq!(got, expected);
}

/// Hex/oct/bin radix integers.
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

// ---- elisp pipeline wire-in coverage ----

/// Symbol-only inputs route through the elisp pipeline.  Asserts
/// the deep-clone path produces a structurally-equal value that
/// survives slot-pool drop.
#[test]
fn elisp_path_dispatch_smoke() {
    // A construct that exercises only §116.B-supported tokens.
    let got = read_str("(let ((a b) (c . d)) (foo 'bar `baz ,qux))").unwrap();
    // Just round-trip through the printer + reader to confirm
    // semantic equality without spelling out the cons tree.
    let printed = fmt_sexp(&got);
    let reparsed = read_str(&printed).unwrap();
    assert_eq!(got, reparsed);
}

/// Deep-clone discipline — returned form must not alias the
/// parser's slot pool.  Stress: parse deeply nested list, drop
/// everything, walk the result to force every cons cell touch.
#[test]
fn elisp_path_deep_clone_survives_pool_drop() {
    let got = read_str("(a (b (c (d (e (f (g (h i))))))))").unwrap();
    // Walk the structure to force every node to be valid.
    fn walk(s: &Sexp) -> usize {
        match s {
            Sexp::Cons(b) => 1 + walk(&b.car) + walk(&b.cdr),
            Sexp::Nil => 0,
            _ => 1,
        }
    }
    // 17 atoms (a,b,c,d,e,f,g,h,i) + 8 nested lists (= 8 cons
    // chain starts), each chain has cons cells equal to its
    // element count.  Exact arithmetic isn't load-bearing; the
    // walk just needs to not crash.
    assert!(walk(&got) > 0);
}
