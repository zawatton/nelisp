//! S-expression value type for the Phase 8.0.1 reader.
//!
//! Doc 44 §3.2 LOCKED scope subset:
//!   - integer (signed/unsigned, decimal/hex/octal)
//!   - float (with exponent)
//!   - string (= "..." with backslash escapes \n \t \\ \")
//!   - symbol (= alphanumeric + - _ . :)
//!   - cons cell (= (a . b))
//!   - list (= (a b c) → (a . (b . (c . nil))))
//!   - vector (= [a b c])
//!   - nil + t literals
//!   - quote-family prefixes (`'x`, `` `x ``, `,x`, `,@x`, `#'x`)
//!
//! Deferred:
//!   - meta char literals (?\\M-a, multi-modifier combinations)
//!   - sharpsign read forms (#[...] byte-code, #s structure)
//!   - multibyte / non-ASCII string literal handling
//!
//! The enum is intentionally NOT a Lisp_Object yet — that is the
//! evaluator's concern (Phase 7.5.4.2 = next sub-phase).  The reader
//! must NOT depend on the evaluator (= layer separation, see prompt
//! constraints).

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

/// A parsed s-expression.  The variants form the minimal value
/// universe that the Phase 7.5.4.1 reader can produce; the evaluator
/// (Phase 7.5.4.2) will widen this with `Lambda`, `Macro`, `Function`
/// etc.
///
/// `Cons` is intentionally `Box`ed so the enum stays a fixed size and
/// list traversal is O(n) without a recursion-blow risk on the *enum*
/// itself.  Deeply nested forms are still bounded by parser recursion
/// depth which is checked separately.
#[derive(Debug, Clone, PartialEq)]
pub enum Sexp {
    /// `nil` literal — also the empty list `()`.
    Nil,
    /// `t` literal.
    T,
    /// 64-bit signed integer.  Per Doc 44 §3.2 the bootstrap subset
    /// only needs fixnum-width integers; bignum promotion is deferred
    /// to Phase 7.5.4.2 with the rest of the evaluator's numeric
    /// tower.
    Int(i64),
    /// IEEE-754 double.
    Float(f64),
    /// Interned-style symbol string.  The reader does NOT intern; the
    /// evaluator will own the obarray (Doc 44 §4.2 case 1 = inject at
    /// takeover).
    Symbol(String),
    /// String literal.  Stored as a Rust `String` for now; multibyte
    /// handling is deferred (Phase 7.5.4.2 + Phase 7.4 NeLisp coding).
    Str(String),
    /// Cons cell.  Lists are encoded as right-leaning `Cons` chains
    /// terminated by `Nil`; dotted pairs (`(a . b)`) leave the cdr as
    /// any non-`Nil` value.
    Cons(Box<Sexp>, Box<Sexp>),
    /// `[a b c]` vector literal.
    ///
    /// Wrapped in `Rc<RefCell<...>>' to support `aset' / in-place
    /// mutation while keeping `Sexp: Clone` cheap (Rc bump only).
    /// Identity comparison goes through `Rc::ptr_eq'; structural
    /// equality (the derived `PartialEq') unwraps the inner `Vec'.
    Vector(Rc<RefCell<Vec<Sexp>>>),
}

impl Sexp {
    /// Build a proper list from a slice of values.  The empty slice
    /// returns `Nil`.
    pub fn list_from(items: &[Sexp]) -> Sexp {
        let mut acc = Sexp::Nil;
        for item in items.iter().rev() {
            acc = Sexp::Cons(Box::new(item.clone()), Box::new(acc));
        }
        acc
    }

    /// Build a vector Sexp from an owned `Vec<Sexp>` without forcing
    /// every call site to spell out `Rc::new(RefCell::new(...))'.
    pub fn vector(items: Vec<Sexp>) -> Sexp {
        Sexp::Vector(Rc::new(RefCell::new(items)))
    }

    /// Wrap a form in `(quote <form>)` (the desugaring of `'x`).
    pub fn quote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("quote".to_string()), inner])
    }

    pub fn backquote(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("backquote".to_string()), inner])
    }

    pub fn comma(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma".to_string()), inner])
    }

    pub fn comma_at(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("comma-at".to_string()), inner])
    }

    pub fn function(inner: Sexp) -> Sexp {
        Sexp::list_from(&[Sexp::Symbol("function".to_string()), inner])
    }

    /// Convenience accessor: is this the `nil` literal (or the empty
    /// list, which is the same thing in Elisp)?
    pub fn is_nil(&self) -> bool {
        matches!(self, Sexp::Nil)
    }
}

/// Pretty-printer used by Phase 8.0.1 debug + tests.  This is *not*
/// the evaluator's `prin1`; it is intentionally lossy where Elisp's
/// printer would diverge (e.g., we do not promise round-trip identity
/// on floats with no fractional part).  Phase 7.5.4.2 will own a
/// faithful `prin1-to-string` once the evaluator owns the value
/// representation.
pub fn fmt_sexp(s: &Sexp) -> String {
    let mut out = String::new();
    write_sexp(&mut out, s);
    out
}

impl fmt::Display for Sexp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&fmt_sexp(self))
    }
}

fn write_sexp(out: &mut String, s: &Sexp) {
    if write_reader_macro(out, s) {
        return;
    }
    match s {
        Sexp::Nil => out.push_str("nil"),
        Sexp::T => out.push('t'),
        Sexp::Int(n) => out.push_str(&n.to_string()),
        Sexp::Float(x) => {
            // Match Elisp printer behaviour for the common case: keep
            // a decimal point so round-tripping back through the reader
            // does not coerce to Int.
            let s = format!("{}", x);
            if s.contains('.') || s.contains('e') || s.contains('E') || s == "inf" || s == "-inf"
                || s == "NaN"
            {
                out.push_str(&s);
            } else {
                out.push_str(&s);
                out.push_str(".0");
            }
        }
        Sexp::Symbol(name) => out.push_str(name),
        Sexp::Str(text) => {
            out.push('"');
            for ch in text.chars() {
                match ch {
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    '\t' => out.push_str("\\t"),
                    '\r' => out.push_str("\\r"),
                    c => out.push(c),
                }
            }
            out.push('"');
        }
        Sexp::Cons(_, _) => {
            out.push('(');
            write_list_body(out, s);
            out.push(')');
        }
        Sexp::Vector(items) => {
            out.push('[');
            let borrowed = items.borrow();
            for (i, item) in borrowed.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                write_sexp(out, item);
            }
            out.push(']');
        }
    }
}

fn write_reader_macro(out: &mut String, s: &Sexp) -> bool {
    let Some((head, arg)) = list_tag_and_arg(s) else {
        return false;
    };
    let prefix = match head {
        "quote" => "'",
        "backquote" => "`",
        "comma" => ",",
        "comma-at" => ",@",
        "function" => "#'",
        _ => return false,
    };
    out.push_str(prefix);
    write_sexp(out, arg);
    true
}

fn list_tag_and_arg(s: &Sexp) -> Option<(&str, &Sexp)> {
    match s {
        Sexp::Cons(car, cdr) => match (&**car, &**cdr) {
            (Sexp::Symbol(tag), Sexp::Cons(arg, tail)) if matches!(**tail, Sexp::Nil) => {
                Some((tag.as_str(), arg.as_ref()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Walk a (possibly improper) list, printing the body without the
/// enclosing parens.  A non-`Nil` final cdr is rendered with the
/// classic ` . tail` notation per Elisp printer.
fn write_list_body(out: &mut String, s: &Sexp) {
    let mut cur = s;
    let mut first = true;
    loop {
        match cur {
            Sexp::Cons(car, cdr) => {
                if !first {
                    out.push(' ');
                }
                first = false;
                write_sexp(out, car);
                cur = cdr;
            }
            Sexp::Nil => return,
            other => {
                out.push_str(" . ");
                write_sexp(out, other);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn list_from_empty_is_nil() {
        assert_eq!(Sexp::list_from(&[]), Sexp::Nil);
    }

    #[test]
    fn list_from_three_elements_chains_right() {
        let got = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let expected = Sexp::Cons(
            Box::new(Sexp::Int(1)),
            Box::new(Sexp::Cons(
                Box::new(Sexp::Int(2)),
                Box::new(Sexp::Cons(Box::new(Sexp::Int(3)), Box::new(Sexp::Nil))),
            )),
        );
        assert_eq!(got, expected);
    }

    #[test]
    fn quote_wraps_form() {
        let got = Sexp::quote(Sexp::Symbol("x".into()));
        assert_eq!(fmt_sexp(&got), "'x");
    }

    #[test]
    fn fmt_reader_macros() {
        assert_eq!(
            fmt_sexp(&Sexp::backquote(Sexp::Symbol("x".into()))),
            "`x"
        );
        assert_eq!(fmt_sexp(&Sexp::comma(Sexp::Symbol("x".into()))), ",x");
        assert_eq!(fmt_sexp(&Sexp::comma_at(Sexp::Symbol("x".into()))), ",@x");
        assert_eq!(fmt_sexp(&Sexp::function(Sexp::Symbol("x".into()))), "#'x");
    }

    #[test]
    fn fmt_dotted_pair() {
        let dotted = Sexp::Cons(
            Box::new(Sexp::Symbol("a".into())),
            Box::new(Sexp::Symbol("b".into())),
        );
        assert_eq!(fmt_sexp(&dotted), "(a . b)");
    }

    #[test]
    fn fmt_string_escapes() {
        assert_eq!(
            fmt_sexp(&Sexp::Str("hi\n\"\\".into())),
            "\"hi\\n\\\"\\\\\""
        );
    }

    #[test]
    fn fmt_float_keeps_decimal() {
        assert_eq!(fmt_sexp(&Sexp::Float(1.0)), "1.0");
        assert_eq!(fmt_sexp(&Sexp::Float(3.14)), "3.14");
    }
}
