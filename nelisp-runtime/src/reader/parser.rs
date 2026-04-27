//! Recursive-descent s-expression parser for Phase 8.0.1.
//!
//! Consumes the token stream produced by `lexer::tokenize` and
//! produces `Sexp` values.  No error recovery — the first failure
//! aborts the read with a `ReadError`.
//!
//! Design notes:
//!   - Recursion depth is bounded (`MAX_DEPTH = 256`) to avoid stack
//!     overflow on pathological input; Doc 44 §3.2 lists a 5-deep
//!     nest as the smoke target so 256 is two orders above the
//!     bootstrap working set.
//!   - `nil` and `t` are recognised here, not in the lexer, so the
//!     symbol-shape decision lives in one place.
//!   - Quote (`'x`) is desugared to `(quote x)` at parse time per
//!     Doc 44 §3.2 spec.

use super::error::{ReadError, SourcePos};
use super::lexer::{PositionedToken, Token};
use super::sexp::Sexp;

/// Maximum nesting depth of `(...)` / `[...]` / quote chains.  Pure
/// safety net; the bootstrap source per Doc 44 §3.2 nests at most
/// ~10-deep, so 256 is a generous ceiling.
const MAX_DEPTH: usize = 256;

/// Parse a single top-level form from the token stream.  Returns the
/// form plus the index of the next unread token.  Used by the
/// public `read_str` helper to enforce "exactly one form" semantics.
pub fn parse_one(tokens: &[PositionedToken]) -> Result<(Sexp, usize), ReadError> {
    if tokens.is_empty() {
        return Err(ReadError::unexpected_eof(
            "input contained no forms",
            SourcePos { line: 1, col: 1 },
        ));
    }
    let mut p = Parser { tokens, pos: 0 };
    let form = p.parse_form(0)?;
    Ok((form, p.pos))
}

/// Parse every top-level form in the token stream.  Used by
/// `read_all` for `.el` file consumption.
pub fn parse_all(tokens: &[PositionedToken]) -> Result<Vec<Sexp>, ReadError> {
    let mut p = Parser { tokens, pos: 0 };
    let mut out = Vec::new();
    while p.pos < tokens.len() {
        out.push(p.parse_form(0)?);
    }
    Ok(out)
}

struct Parser<'a> {
    tokens: &'a [PositionedToken],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&'a PositionedToken> {
        self.tokens.get(self.pos)
    }

    fn bump(&mut self) -> Option<&'a PositionedToken> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    /// Parse one form.  `depth` is the current nesting depth so we
    /// can bail before blowing the host stack.
    fn parse_form(&mut self, depth: usize) -> Result<Sexp, ReadError> {
        if depth > MAX_DEPTH {
            let pos = self
                .peek()
                .map(|t| t.pos)
                .unwrap_or(SourcePos { line: 1, col: 1 });
            return Err(ReadError::parse(
                format!("nesting deeper than {} forms", MAX_DEPTH),
                pos,
            ));
        }
        let tok = self.bump().ok_or_else(|| {
            ReadError::unexpected_eof("expected a form", SourcePos { line: 1, col: 1 })
        })?;

        match &tok.token {
            Token::LParen => self.parse_list(tok.pos, depth + 1),
            Token::LBracket => self.parse_vector(tok.pos, depth + 1),
            Token::Quote => {
                let inner = self.parse_form(depth + 1)?;
                Ok(Sexp::quote(inner))
            }
            Token::Int(n) => Ok(Sexp::Int(*n)),
            Token::Float(f) => Ok(Sexp::Float(*f)),
            Token::Str(s) => Ok(Sexp::Str(s.clone())),
            Token::Symbol(name) => Ok(symbol_to_sexp(name)),
            Token::RParen => Err(ReadError::parse(
                "unexpected `)` (no matching `(`)".to_string(),
                tok.pos,
            )),
            Token::RBracket => Err(ReadError::parse(
                "unexpected `]` (no matching `[`)".to_string(),
                tok.pos,
            )),
            Token::Dot => Err(ReadError::parse(
                "unexpected `.` outside dotted-pair context".to_string(),
                tok.pos,
            )),
        }
    }

    /// Parse the body of a list after the opening `(` was consumed.
    /// Handles three shapes:
    ///   - `()` → Nil
    ///   - `(a b c)` → proper list
    ///   - `(a . b)` / `(a b . c)` → dotted pair tail
    ///
    /// We accumulate elements into a flat `Vec` so the dotted-pair
    /// branch can take the last element as the tail; this keeps the
    /// hot path (proper list) one allocation cheaper than building
    /// the cons chain incrementally.
    fn parse_list(&mut self, open_pos: SourcePos, depth: usize) -> Result<Sexp, ReadError> {
        let mut items: Vec<Sexp> = Vec::new();
        loop {
            let next = self.peek().ok_or_else(|| {
                ReadError::unexpected_eof("unbalanced `(` (no matching `)`)", open_pos)
            })?;
            match &next.token {
                Token::RParen => {
                    self.bump();
                    return Ok(Sexp::list_from(&items));
                }
                Token::Dot => {
                    let dot_pos = next.pos;
                    self.bump();
                    if items.is_empty() {
                        return Err(ReadError::parse(
                            "`.` at start of list",
                            dot_pos,
                        ));
                    }
                    let tail = self.parse_form(depth)?;
                    let close = self.bump().ok_or_else(|| {
                        ReadError::unexpected_eof(
                            "unbalanced `(` after dotted tail",
                            open_pos,
                        )
                    })?;
                    if !matches!(close.token, Token::RParen) {
                        return Err(ReadError::parse(
                            format!(
                                "expected `)` after dotted tail, got {:?}",
                                close.token
                            ),
                            close.pos,
                        ));
                    }
                    return Ok(build_dotted(items, tail));
                }
                _ => {
                    let form = self.parse_form(depth)?;
                    items.push(form);
                }
            }
        }
    }

    fn parse_vector(&mut self, open_pos: SourcePos, depth: usize) -> Result<Sexp, ReadError> {
        let mut items = Vec::new();
        loop {
            let next = self.peek().ok_or_else(|| {
                ReadError::unexpected_eof("unbalanced `[` (no matching `]`)", open_pos)
            })?;
            match &next.token {
                Token::RBracket => {
                    self.bump();
                    return Ok(Sexp::Vector(items));
                }
                Token::Dot => {
                    return Err(ReadError::parse(
                        "`.` not allowed inside vector literal",
                        next.pos,
                    ));
                }
                _ => {
                    let form = self.parse_form(depth)?;
                    items.push(form);
                }
            }
        }
    }
}

/// Recognise the two reserved symbol literals.  Anything else is a
/// regular `Symbol`.
fn symbol_to_sexp(name: &str) -> Sexp {
    match name {
        "nil" => Sexp::Nil,
        "t" => Sexp::T,
        _ => Sexp::Symbol(name.to_string()),
    }
}

/// Build a (possibly improper) cons chain from `head` items and a
/// non-Nil `tail`.  Pre: `items` is non-empty (the dotted-pair
/// branch verified that).
fn build_dotted(items: Vec<Sexp>, tail: Sexp) -> Sexp {
    let mut acc = tail;
    for item in items.into_iter().rev() {
        acc = Sexp::Cons(Box::new(item), Box::new(acc));
    }
    acc
}

#[cfg(test)]
mod tests {
    use super::super::lexer::tokenize;
    use super::*;

    fn parse(s: &str) -> Sexp {
        let toks = tokenize(s).expect("lex");
        let (form, consumed) = parse_one(&toks).expect("parse");
        assert_eq!(consumed, toks.len(), "leftover tokens after parse");
        form
    }

    #[test]
    fn parse_nil_and_t() {
        assert_eq!(parse("nil"), Sexp::Nil);
        assert_eq!(parse("t"), Sexp::T);
    }

    #[test]
    fn parse_empty_list_is_nil() {
        assert_eq!(parse("()"), Sexp::Nil);
    }

    #[test]
    fn parse_proper_list() {
        let got = parse("(1 2 3)");
        assert_eq!(got, Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]));
    }

    #[test]
    fn parse_dotted_pair() {
        let got = parse("(a . b)");
        assert_eq!(
            got,
            Sexp::Cons(
                Box::new(Sexp::Symbol("a".into())),
                Box::new(Sexp::Symbol("b".into())),
            )
        );
    }

    #[test]
    fn parse_quote_desugars() {
        let got = parse("'x");
        assert_eq!(got, Sexp::quote(Sexp::Symbol("x".into())));
    }

    #[test]
    fn parse_vector() {
        let got = parse("[1 2 3]");
        assert_eq!(
            got,
            Sexp::Vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
        );
    }

    #[test]
    fn parse_nested() {
        let got = parse("(((((1)))))");
        // Build five Cons-of-Cons-of-... wrapping (1).
        let one = Sexp::list_from(&[Sexp::Int(1)]);
        let mut expected = one;
        for _ in 0..4 {
            expected = Sexp::list_from(&[expected]);
        }
        assert_eq!(got, expected);
    }

    #[test]
    fn parse_leading_dot_errors() {
        let toks = tokenize("(. b)").unwrap();
        let err = parse_one(&toks).unwrap_err();
        assert!(format!("{}", err).contains("`.` at start"));
    }

    #[test]
    fn parse_unbalanced_open_errors() {
        let toks = tokenize("(a b").unwrap();
        let err = parse_one(&toks).unwrap_err();
        assert!(format!("{}", err).contains("unbalanced"));
    }

    #[test]
    fn parse_stray_close_errors() {
        let toks = tokenize(")").unwrap();
        let err = parse_one(&toks).unwrap_err();
        assert!(format!("{}", err).contains("unexpected `)`"));
    }

    #[test]
    fn parse_dot_in_vector_errors() {
        let toks = tokenize("[a . b]").unwrap();
        let err = parse_one(&toks).unwrap_err();
        assert!(format!("{}", err).contains("not allowed inside vector"));
    }
}
