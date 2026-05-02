//! Hand-rolled tokenizer for the Phase 8.0.1 minimal Elisp reader.
//!
//! Design constraints (Doc 44 §3.2 + prompt):
//!   - no external dep (no `nom`, no `pest`) — hand-roll
//!   - source location tracking (line:col) for actionable errors
//!   - explicit `NotYetImplemented` tokens for still-deferred lex
//!     shapes (= `#[...]`, `#s(...)`)
//!
//! The lexer produces a stream of `Token`s.  Whitespace and comments
//! are eaten silently; numeric/symbol disambiguation is handled here
//! (= we know "123" is `Int`, "1.5" is `Float`, "1e3" is `Float`,
//! "1.5.x" is `Symbol`).  Pure recursive-descent grammar lives in
//! `parser.rs`.

use super::error::{ReadError, SourcePos};

/// Tokens emitted by the lexer.  `Quote` and `Dot` are kept distinct
/// from `Symbol` because the parser treats them syntactically.
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `'` quote prefix.  The parser desugars to `(quote ...)`.
    Quote,
    /// `` ` `` quasiquote prefix.  The parser desugars to
    /// `(backquote ...)`.
    Backquote,
    /// `,` unquote prefix.  Valid only within a backquote context.
    Comma,
    /// `,@` splice prefix.  Valid only within a backquote context.
    CommaAt,
    /// `#'` function quote prefix.  The parser desugars to
    /// `(function ...)`.
    FunctionQuote,
    /// `.` (only inside a list, used for dotted pairs).  Outside a
    /// list this would be a number prefix or symbol char; the lexer
    /// keeps the standalone `.` token distinct so the parser can
    /// reject misuse.
    Dot,
    /// Integer literal (decimal, hex `#x`, octal `#o`, binary `#b`).
    Int(i64),
    /// Float literal.
    Float(f64),
    /// String literal (already with escapes resolved).
    Str(String),
    /// Symbol-shaped atom (`foo`, `nil`, `t`, `+`, `let*`, ...).  We
    /// do NOT classify nil/t here — the parser does (one source of
    /// truth for keyword recognition).
    Symbol(String),
}

/// A token with the source position of its first character.
#[derive(Debug, Clone, PartialEq)]
pub struct PositionedToken {
    pub token: Token,
    pub pos: SourcePos,
}

/// Tokenize an entire input string.  Returns the full vector eagerly;
/// for the bootstrap subset (= ~2000 LOC of `.el` per Doc 44 §4.4)
/// the memory is negligible and a streaming iterator would just add
/// lifetime noise to the parser.
pub fn tokenize(input: &str) -> Result<Vec<PositionedToken>, ReadError> {
    let mut lx = Lexer::new(input);
    let mut out = Vec::new();
    while let Some(tok) = lx.next_token()? {
        out.push(tok);
    }
    Ok(out)
}

struct Lexer<'a> {
    src: &'a [u8],
    /// Byte offset, not char offset.  All ASCII-only constructs the
    /// reader handles let us advance one byte at a time; multibyte
    /// inside string literals is passed through as raw bytes (Phase
    /// 7.4 NeLisp coding will own re-decoding).
    pos: usize,
    line: u32,
    col: u32,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            src: input.as_bytes(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line,
            col: self.col,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    /// Advance one byte, updating line/col tracking.  Treats `\n` as
    /// the line break (also covers `\r\n`: `\r` is consumed as a
    /// regular char for col purposes, and the following `\n` triggers
    /// the line bump — net behaviour matches Elisp's view).
    fn bump(&mut self) -> Option<u8> {
        let b = self.peek()?;
        self.pos += 1;
        if b == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(b)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(b) if is_whitespace(b) => {
                    self.bump();
                }
                Some(b';') => {
                    // Comment runs to end of line (or EOF).
                    while let Some(b) = self.peek() {
                        if b == b'\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                _ => break,
            }
        }
    }

    fn next_token(&mut self) -> Result<Option<PositionedToken>, ReadError> {
        self.skip_whitespace_and_comments();
        let pos = self.current_pos();
        let b = match self.peek() {
            Some(b) => b,
            None => return Ok(None),
        };

        let token = match b {
            b'(' => {
                self.bump();
                Token::LParen
            }
            b')' => {
                self.bump();
                Token::RParen
            }
            b'[' => {
                self.bump();
                Token::LBracket
            }
            b']' => {
                self.bump();
                Token::RBracket
            }
            b'\'' => {
                self.bump();
                Token::Quote
            }
            b'"' => self.read_string(pos)?,
            b'#' => self.read_sharpsign(pos)?,
            b'`' => {
                self.bump();
                Token::Backquote
            }
            b',' => {
                self.bump();
                if self.peek() == Some(b'@') {
                    self.bump();
                    Token::CommaAt
                } else {
                    Token::Comma
                }
            }
            b'?' => Token::Int(self.read_char_literal(pos)?),
            _ => self.read_atom(pos)?,
        };
        Ok(Some(PositionedToken { token, pos }))
    }

    fn read_string(&mut self, start: SourcePos) -> Result<Token, ReadError> {
        // Consume the opening `"`.
        self.bump();
        let mut out = String::new();
        loop {
            let b = match self.peek() {
                Some(b) => b,
                None => {
                    return Err(ReadError::unexpected_eof(
                        "unterminated string literal",
                        start,
                    ));
                }
            };
            match b {
                b'"' => {
                    self.bump();
                    return Ok(Token::Str(out));
                }
                b'\\' => {
                    self.bump();
                    let esc_pos = self.current_pos();
                    let next = self.bump().ok_or_else(|| {
                        ReadError::unexpected_eof("unterminated escape in string", esc_pos)
                    })?;
                    match next {
                        b'\n' => continue,
                        b'\r' => {
                            if self.peek() == Some(b'\n') {
                                self.bump();
                            }
                            continue;
                        }
                        b'n' => out.push('\n'),
                        b't' => out.push('\t'),
                        b'r' => out.push('\r'),
                        b'\\' => out.push('\\'),
                        b'"' => out.push('"'),
                        b'0' => out.push('\0'),
                        // `\xNN` hex byte escape — useful for the
                        // bootstrap form `(string ?\xff)` style; Doc
                        // 44 §3.2 lists \xNN as deferred but only
                        // because the *char literal* surface is
                        // deferred.  Inside string literals we accept
                        // the 2-digit form so cold-init constants
                        // round-trip.
                        b'x' => {
                            let h1 = self.bump().ok_or_else(|| {
                                ReadError::unexpected_eof("\\x needs 2 hex digits", esc_pos)
                            })?;
                            let h2 = self.bump().ok_or_else(|| {
                                ReadError::unexpected_eof("\\x needs 2 hex digits", esc_pos)
                            })?;
                            let v = (hex_digit(h1).ok_or_else(|| {
                                ReadError::lex(format!("invalid hex digit: {:?}", h1 as char), esc_pos)
                            })? << 4)
                                | hex_digit(h2).ok_or_else(|| {
                                    ReadError::lex(
                                        format!("invalid hex digit: {:?}", h2 as char),
                                        esc_pos,
                                    )
                                })?;
                            out.push(v as u8 as char);
                        }
                        // Doc 51 Phase 3-A''-1 — Emacs reader compatibility:
                        // any unknown `\X' inside a string literal is treated
                        // as the literal character `X' (the leading backslash
                        // is dropped).  This matches Emacs's behaviour and is
                        // required to parse the standard `\(fn ARGLIST)'
                        // docstring marker that appears throughout the
                        // vendored Emacs source tree.
                        other => {
                            out.push(other as char);
                        }
                    }
                }
                _ => {
                    self.bump();
                    out.push(b as char);
                }
            }
        }
    }

    fn read_sharpsign(&mut self, start: SourcePos) -> Result<Token, ReadError> {
        // Consume `#`.
        self.bump();
        let after = self.peek();
        match after {
            Some(b'x') | Some(b'X') => {
                self.bump();
                let n = self.read_radix_int(16, start)?;
                Ok(Token::Int(n))
            }
            Some(b'o') | Some(b'O') => {
                self.bump();
                let n = self.read_radix_int(8, start)?;
                Ok(Token::Int(n))
            }
            Some(b'b') | Some(b'B') => {
                self.bump();
                let n = self.read_radix_int(2, start)?;
                Ok(Token::Int(n))
            }
            Some(b'\'') => {
                self.bump();
                Ok(Token::FunctionQuote)
            }
            Some(b'[') => Err(ReadError::not_yet_implemented(
                "byte-code literal #[...] is deferred (Doc 44 §3.3 risks)",
                start,
            )),
            Some(b's') => Err(ReadError::not_yet_implemented(
                "structure literal #s(...) is deferred",
                start,
            )),
            Some(other) => Err(ReadError::lex(
                format!("unsupported sharpsign form: #{}", other as char),
                start,
            )),
            None => Err(ReadError::unexpected_eof(
                "unexpected EOF after #",
                start,
            )),
        }
    }

    fn read_radix_int(&mut self, radix: u32, start: SourcePos) -> Result<i64, ReadError> {
        let mut text = String::new();
        // Optional sign prefix on radix-tagged integers (Elisp accepts
        // `#x-10` etc).
        if let Some(b @ (b'+' | b'-')) = self.peek() {
            text.push(b as char);
            self.bump();
        }
        let mut digits_seen = false;
        while let Some(b) = self.peek() {
            if (b as char).is_digit(radix) || b == b'_' {
                if b != b'_' {
                    text.push(b as char);
                    digits_seen = true;
                }
                self.bump();
            } else if !is_atom_terminator(b) {
                return Err(ReadError::lex(
                    format!(
                        "invalid digit {:?} for radix {} integer",
                        b as char, radix
                    ),
                    start,
                ));
            } else {
                break;
            }
        }
        if !digits_seen {
            return Err(ReadError::lex(
                format!("radix-{} integer requires at least one digit", radix),
                start,
            ));
        }
        i64::from_str_radix(&text, radix).map_err(|e| {
            ReadError::lex(
                format!("integer overflow / parse failure: {}", e),
                start,
            )
        })
    }

    /// Read an atom that is not delimited by a prefix sigil — i.e. a
    /// symbol or a base-10 number.  We use the classic "scan all
    /// non-terminator bytes, then try-parse" trick because Elisp's
    /// numeric grammar is wide (`+1`, `-.5`, `1.`, `1e3`, `1.5e-2`)
    /// and trying to commit early to `Symbol` vs `Int` vs `Float`
    /// would force backtracking.
    fn read_atom(&mut self, start: SourcePos) -> Result<Token, ReadError> {
        let mut text = String::new();
        while let Some(b) = self.peek() {
            if is_atom_terminator(b) {
                break;
            }
            text.push(b as char);
            self.bump();
        }
        if text.is_empty() {
            return Err(ReadError::lex(
                format!("unexpected character: {:?}", self.peek().unwrap_or(0) as char),
                start,
            ));
        }
        // Lone `.` is a syntactic token, not a symbol.
        if text == "." {
            return Ok(Token::Dot);
        }
        if let Some(n) = try_parse_int(&text) {
            return Ok(Token::Int(n));
        }
        if let Some(f) = try_parse_float(&text) {
            return Ok(Token::Float(f));
        }
        Ok(Token::Symbol(text))
    }

    fn read_char_literal(&mut self, start: SourcePos) -> Result<i64, ReadError> {
        self.bump();
        let b = self.peek().ok_or_else(|| {
            ReadError::unexpected_eof("unterminated char literal after `?`", start)
        })?;
        if is_atom_terminator(b) {
            return Err(ReadError::lex("empty char literal after `?`", start));
        }
        if b != b'\\' {
            self.bump();
            if b.is_ascii() {
                return Ok(i64::from(b));
            }
            return Err(ReadError::not_yet_implemented(
                "multibyte char literal is deferred",
                start,
            ));
        }

        self.bump();
        let esc_pos = self.current_pos();
        let next = self.bump().ok_or_else(|| {
            ReadError::unexpected_eof("unterminated escape in char literal", esc_pos)
        })?;
        match next {
            b'n' => Ok(10),
            b't' => Ok(9),
            b'r' => Ok(13),
            b'\\' => Ok(92),
            b'\'' => Ok(39),
            b'"' => Ok(34),
            b'x' => {
                let h1 = self.bump().ok_or_else(|| {
                    ReadError::unexpected_eof("\\x needs 2 hex digits in char literal", esc_pos)
                })?;
                let h2 = self.bump().ok_or_else(|| {
                    ReadError::unexpected_eof("\\x needs 2 hex digits in char literal", esc_pos)
                })?;
                let hi = hex_digit(h1).ok_or_else(|| {
                    ReadError::lex(
                        format!("invalid hex digit in char literal: {:?}", h1 as char),
                        esc_pos,
                    )
                })?;
                let lo = hex_digit(h2).ok_or_else(|| {
                    ReadError::lex(
                        format!("invalid hex digit in char literal: {:?}", h2 as char),
                        esc_pos,
                    )
                })?;
                Ok(((hi << 4) | lo) as i64)
            }
            b'C' => {
                if self.bump() != Some(b'-') {
                    return Err(ReadError::lex(
                        "expected `-` after \\C in char literal",
                        esc_pos,
                    ));
                }
                let ctrl = self.bump().ok_or_else(|| {
                    ReadError::unexpected_eof("missing control target in char literal", esc_pos)
                })?;
                if !ctrl.is_ascii() {
                    return Err(ReadError::not_yet_implemented(
                        "multibyte control char literal is deferred",
                        esc_pos,
                    ));
                }
                Ok(i64::from(ctrl & 0x1f))
            }
            // Doc 51 Phase 3-A''-2 — Emacs Meta-modifier char literal.
            // `?\M-X' encodes the base char X with bit 27 (0x8000000) set
            // so keybinding tables can store key sequences as raw integers.
            // Handles a single level of nested escape (e.g. `?\M-\C-x') for
            // the common cases that appear in vendored Emacs sources.
            b'M' => {
                if self.bump() != Some(b'-') {
                    return Err(ReadError::lex(
                        "expected `-' after \\M in char literal",
                        esc_pos,
                    ));
                }
                let nb = self.bump().ok_or_else(|| {
                    ReadError::unexpected_eof(
                        "missing meta target in char literal",
                        esc_pos,
                    )
                })?;
                let base: i64 = if nb == b'\\' {
                    let inner = self.bump().ok_or_else(|| {
                        ReadError::unexpected_eof(
                            "unterminated nested escape after \\M-\\",
                            esc_pos,
                        )
                    })?;
                    match inner {
                        b'C' => {
                            if self.bump() != Some(b'-') {
                                return Err(ReadError::lex(
                                    "expected `-' after nested \\C",
                                    esc_pos,
                                ));
                            }
                            let c = self.bump().ok_or_else(|| {
                                ReadError::unexpected_eof(
                                    "missing control target after \\M-\\C-",
                                    esc_pos,
                                )
                            })?;
                            i64::from(c & 0x1f)
                        }
                        b'n' => 10,
                        b't' => 9,
                        b'r' => 13,
                        other => i64::from(other),
                    }
                } else {
                    i64::from(nb)
                };
                Ok(base | 0x8000000)
            }
            // Doc 51 Phase 3-A''-1 — Emacs reader compatibility: any
            // unknown `?\X' falls through as the literal char `X', matching
            // Emacs's char-reader behaviour.  This unblocks `?\(' / `?\)' /
            // `?\,' which appear in vendored Emacs sources (= e.g. subr.el).
            other => Ok(i64::from(other)),
        }
    }
}

fn is_whitespace(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\n' | b'\r' | 0x0B | 0x0C)
}

/// Bytes that end an atom run.  Mirrors Elisp's reader: parens,
/// brackets, quote, backquote, comma, comment, string delimiter, and
/// whitespace.
fn is_atom_terminator(b: u8) -> bool {
    is_whitespace(b)
        || matches!(
            b,
            b'(' | b')' | b'[' | b']' | b'\'' | b'`' | b',' | b';' | b'"'
        )
}

fn hex_digit(b: u8) -> Option<u32> {
    match b {
        b'0'..=b'9' => Some((b - b'0') as u32),
        b'a'..=b'f' => Some((b - b'a' + 10) as u32),
        b'A'..=b'F' => Some((b - b'A' + 10) as u32),
        _ => None,
    }
}

/// Try to parse `text` as a decimal integer.  Accepts leading `+` /
/// `-` and `_` digit separators (matching Rust convention; Elisp does
/// not have separators but the common cold-init source we will read
/// is hand-written and never uses `_` so the extra leniency is safe).
fn try_parse_int(text: &str) -> Option<i64> {
    let stripped: String = text.chars().filter(|c| *c != '_').collect();
    if stripped.is_empty() {
        return None;
    }
    // Reject anything containing decimal-point or exponent — those
    // belong to `try_parse_float`.
    if stripped.contains('.') || stripped.contains('e') || stripped.contains('E') {
        return None;
    }
    // The byte-level scan above let `.` etc terminate, so the only
    // shapes we see here are ASCII digits with at most a leading sign.
    let bytes = stripped.as_bytes();
    let (sign_offset, _) = match bytes[0] {
        b'+' | b'-' => (1usize, true),
        _ => (0usize, false),
    };
    if bytes.len() == sign_offset {
        return None;
    }
    if !bytes[sign_offset..].iter().all(|b| b.is_ascii_digit()) {
        return None;
    }
    stripped.parse::<i64>().ok()
}

/// Try to parse `text` as a float.  Recognises `1.0`, `.5`, `1.`,
/// `1e3`, `-1.5e-2`.  Rejects shapes that have neither `.` nor
/// exponent (those should be parsed as Int, or fall through to
/// Symbol).
fn try_parse_float(text: &str) -> Option<f64> {
    if !text.contains('.') && !text.contains('e') && !text.contains('E') {
        return None;
    }
    // Reject leading `.` followed by no digit (would be a malformed
    // form).  Rust's f64::from_str already handles `.5` and `1.` so we
    // mostly delegate.
    let f: f64 = text.parse().ok()?;
    // Reject NaN / Inf coming from "nan"/"inf" symbol shapes — Elisp
    // spells those `0.0e+NaN` etc and the reader should not silently
    // turn the symbol `inf` into +Inf.
    if f.is_nan() || f.is_infinite() {
        // Allow if the source actually used scientific notation.
        if text.to_ascii_lowercase().contains('e') {
            return Some(f);
        }
        return None;
    }
    Some(f)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(s: &str) -> Vec<Token> {
        tokenize(s)
            .unwrap()
            .into_iter()
            .map(|p| p.token)
            .collect()
    }

    #[test]
    fn skips_whitespace_and_comments() {
        let toks = lex(" \t\n; comment\n  42 ; trailing\n ");
        assert_eq!(toks, vec![Token::Int(42)]);
    }

    #[test]
    fn parens_and_brackets() {
        let toks = lex("()[]");
        assert_eq!(
            toks,
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket
            ]
        );
    }

    #[test]
    fn quote_prefix() {
        let toks = lex("'foo");
        assert_eq!(toks, vec![Token::Quote, Token::Symbol("foo".into())]);
    }

    #[test]
    fn quasiquote_prefixes() {
        let toks = lex("`(a ,b ,@c)");
        assert_eq!(
            toks,
            vec![
                Token::Backquote,
                Token::LParen,
                Token::Symbol("a".into()),
                Token::Comma,
                Token::Symbol("b".into()),
                Token::CommaAt,
                Token::Symbol("c".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn function_quote_prefix() {
        let toks = lex("#'foo");
        assert_eq!(toks, vec![Token::FunctionQuote, Token::Symbol("foo".into())]);
    }

    #[test]
    fn dotted_pair_dot_is_token() {
        let toks = lex("(a . b)");
        assert_eq!(
            toks,
            vec![
                Token::LParen,
                Token::Symbol("a".into()),
                Token::Dot,
                Token::Symbol("b".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn integer_shapes() {
        assert_eq!(lex("0 42 -7 +3"), vec![
            Token::Int(0),
            Token::Int(42),
            Token::Int(-7),
            Token::Int(3),
        ]);
    }

    #[test]
    fn radix_integers() {
        assert_eq!(lex("#x10 #o17 #b1010 #x-ff"), vec![
            Token::Int(16),
            Token::Int(15),
            Token::Int(10),
            Token::Int(-255),
        ]);
    }

    #[test]
    fn float_shapes() {
        let toks = lex("3.14 .5 1. 1e3 -1.5e-2");
        assert_eq!(
            toks,
            vec![
                Token::Float(3.14),
                Token::Float(0.5),
                Token::Float(1.0),
                Token::Float(1000.0),
                Token::Float(-0.015),
            ]
        );
    }

    #[test]
    fn string_with_escapes() {
        let toks = lex("\"hi\\n\\t\\\\\\\"end\"");
        assert_eq!(toks, vec![Token::Str("hi\n\t\\\"end".into())]);
    }

    #[test]
    fn string_backslash_newline_continues_line() {
        let toks = lex("\"foo\\\nbar\"");
        assert_eq!(toks, vec![Token::Str("foobar".into())]);
    }

    #[test]
    fn unterminated_string_errors() {
        assert!(tokenize("\"oops").is_err());
    }

    #[test]
    fn string_unknown_escape_errors() {
        let err = tokenize("\"\\q\"").unwrap_err();
        assert!(format!("{}", err).contains("unknown escape"));
    }

    #[test]
    fn char_literal_shapes() {
        assert_eq!(
            lex("?a ?\\n ?\\t ?\\r ?\\\\ ?\\' ?\\\" ?\\xff ?\\C-a"),
            vec![
                Token::Int(97),
                Token::Int(10),
                Token::Int(9),
                Token::Int(13),
                Token::Int(92),
                Token::Int(39),
                Token::Int(34),
                Token::Int(255),
                Token::Int(1),
            ]
        );
    }

    #[test]
    fn char_literal_meta_is_deferred() {
        let err = tokenize("?\\M-a").unwrap_err();
        assert!(matches!(err, ReadError::NotYetImplemented { .. }));
    }

    #[test]
    fn char_literal_bad_hex_errors() {
        let err = tokenize("?\\xfg").unwrap_err();
        assert!(format!("{}", err).contains("invalid hex digit"));
    }

    #[test]
    fn symbol_with_punctuation() {
        assert_eq!(
            lex("foo-bar let* my:ns/name"),
            vec![
                Token::Symbol("foo-bar".into()),
                Token::Symbol("let*".into()),
                Token::Symbol("my:ns/name".into()),
            ]
        );
    }
}
