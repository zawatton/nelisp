//! Rust-side minimal Elisp reader / sexp parser (Doc 44 §3.2 LOCKED).
//!
//! Public surface (= `read_str' / `read_all') is intentionally
//! narrow — the reader stops at the syntactic value (`Sexp'), never
//! depending on the evaluator.  Supported subset includes the Doc 44
//! §3.2 lockset plus the Doc 51 / Doc 52 extensions
//! (`?\M-X' meta chars, bare `,X' / `,@X' outside backquote,
//! `#s(...)' struct literals, unknown `\X' as literal X).
//!
//! Doc 126.B (2026-05-18) re-promoted this module to the production
//! boot path — `Env::new_global' dispatches to `read_all + eval' per
//! top-level STDLIB form, replacing the pre-126 NELIMG v3 frozen-heap
//! `decode_v3_into' route.  User-visible `read' / `read-from-string'
//! delegate to the elisp implementation in `lisp/nelisp-stdlib-reader.el'.
//! The Rust reader also survives in `image-baker' feature builds where
//! the relocated `iterative_bake_one' (now in `bin/nelisp-baker.rs'
//! per Doc 126.E) parses each stdlib source on the way to its v3
//! image, plus the reader's own ERTs.
//!
//! Doc 116 §116.C top-level wire-in: `read_str' / `read_all' route
//! every input through the pure-elisp pipeline composed of §116.A
//! `nelisp_reader_lex_one' + §116.B `nelisp_reader_parse_one' (via
//! `crate::elisp_cc_spike::reader_lex_one' /
//! `crate::elisp_cc_spike::reader_parse_one').  Doc 116 §116.B+
//! extended the elisp side to cover char literals `?X' (kind 24),
//! radix integers `#x..'/`#o..'/`#b..' (kind 25), and bare-sign
//! symbols (= the saw-digit bit in the atom classifier).  Doc 122
//! §122.G further extended the elisp parser to handle Float
//! literals `1.5' / `1e3' (kind 21) via the `nl_str_to_float'
//! extern.
//!
//! Doc 116 §116.D (2026-05-13) deleted the legacy Rust `lexer.rs' +
//! `parser.rs' modules (-1370 LOC).  Doc 116 §116.B+ vector arm
//! (2026-05-18) wired the elisp parser to handle `[..]' (kind 3) —
//! `lisp/nelisp-cc-reader-parser.el' grew `nelisp_reader_p_parse_vector'
//! (= cons-list accumulator + `vector-make' + `vector-slot-set' fill
//! loop), unblocking the `NELISP_EVAL_BOOT=1' path through
//! `nelisp-stdlib-os.el' (which uses `[:type :type ...]' literals for
//! `nl-ffi-call' signatures).  Remaining unsupported features —
//! record `#s(..)' (kind 11) and byte-code `#[..]' — still surface
//! as `NotYetImplemented' (the boot stdlib never uses them; the
//! user-facing reader in `nelisp-stdlib-reader.el' has its own
//! `nelisp--read-parse-vector' for `read' / `read-from-string').
//! This module retains the dispatch + deep-clone layer + `ReadError'
//! re-export only.
//!
//! The cons-make MVP refcount discipline (= raw 32-byte payload
//! copy without inner-box refcount bump, see
//! `nelisp-phase47-compiler--emit-cons-make') means the parser's
//! `Sexp::Cons' output shares heap boxes with the caller-owned slot
//! pool.  We rebuild a refcount-1-owning copy via [`deep_clone`]
//! before returning so the slot pool can be safely dropped.  A
//! proper §123.F-style refcount-aware `cons-make' is a future Doc
//! 123 follow-up that retires this helper.

// Value type (`Sexp', `CharTableInner', tag constants, `fmt_sexp')
// and read-side error type (`ReadError', `SourcePos') live in
// `eval/sexp.rs' and `eval/error.rs' since Doc 73 Stage 8.1; re-
// exported here for any remaining `use crate::reader::Sexp' callsites.
pub use crate::eval::error::{ReadError, SourcePos};
pub use crate::eval::sexp::{fmt_sexp, Sexp};

use crate::elisp_cc_spike;

/// Slot-pool capacity for the §116.B parser.  The parser depth
/// counter increments per-element-in-list (= one slot triple per
/// list element walked) plus per-nesting-level, so a 12-deep
/// `(cons 1 (cons 2 (...)))' chain consumes ~3*12 = 36 depth slots.
/// Doc 116 §116.B+ raised the cap to 1024 so the elisp pipeline
/// can handle the full stdlib subset without OOB indexing into the
/// pool (= the pre-§116.B+ cap of 32 silently passed all probe
/// tests but SIGSEGV'd on real stdlib code whose `defmacro' bodies
/// nest cons chains 12+ deep).  Matches
/// `tests/elisp_cc_reader_parser_probe.rs::POOL_SIZE'.
const PARSER_POOL_SIZE: usize = 3 + 4 * 1024;

/// Initial scratch MutStr capacity.  64 bytes covers ~all atoms in
/// the boot stdlib without reallocation; mut-str grows as needed.
const SCRATCH_CAP: i64 = 64;

/// Parse exactly one top-level form from `input`.  Trailing
/// non-whitespace tokens (after one form is read) are an error — use
/// [`read_all`] if you want every form.
///
/// Doc 116 §116.D: every input routes through the §116.A/B/B+/§122.G
/// pure-elisp pipeline.  Features the elisp parser cannot handle
/// (record `#s(..)', byte-code `#[..]') surface as
/// `NotYetImplemented' read errors.  Vectors `[..]' are now
/// supported (Doc 116 §116.B+ vector arm, 2026-05-18).
pub fn read_str(input: &str) -> Result<Sexp, ReadError> {
    match try_elisp_read_str(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(
            "feature unsupported by §116.A/B/B+ elisp Reader \
             (record `#s(..)', byte-code `#[..]', or syntax error)",
            SourcePos { line: 1, col: 1 },
        )),
    }
}

/// Parse every top-level form in `input`.  Returns an empty vector
/// if the input is empty (or whitespace + comments only).
///
/// Doc 116 §116.D: every input routes through the §116.A/B/B+/§122.G
/// pure-elisp pipeline.  Features the elisp parser cannot handle
/// surface as `NotYetImplemented' read errors.
pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    match try_elisp_read_all(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(
            "feature unsupported by §116.A/B/B+ elisp Reader \
             (record `#s(..)', byte-code `#[..]', or syntax error)",
            SourcePos { line: 1, col: 1 },
        )),
    }
}

// ---------------------------------------------------------------------------
// §116.C elisp dispatch — pure-elisp lex + parse via §116.A + §116.B
// ---------------------------------------------------------------------------

/// Try to parse one top-level form through the §116.A / §116.B
/// elisp pipeline.  Returns:
///   - `Some(Ok(form))` — elisp succeeded; `form` is a deep-clone'd
///     refcount-1-owning tree (= slot-pool independent).
///   - `Some(Err(_))`   — elisp succeeded but found genuine input
///     issues (= trailing tokens after the first form, or empty
///     input).  These are surface errors that Rust would also raise.
///   - `None`           — elisp's parser bailed out (= unsupported
///     feature or genuine syntax error).  Caller falls back to Rust.
fn try_elisp_read_str(input: &str) -> Option<Result<Sexp, ReadError>> {
    let mut state = ElispReadState::new(input);
    match state.parse_one_form()? {
        ElispParseOutcome::Form(form) => {
            // Trailing-token check: lex one more token; if anything
            // other than EOF appears, surface a Rust-style trailing
            // error so behaviour matches the pre-wire-in body.
            if state.has_trailing_token() {
                return Some(Err(ReadError::parse(
                    "trailing token after first form".to_string(),
                    SourcePos { line: 1, col: 1 },
                )));
            }
            Some(Ok(form))
        }
        ElispParseOutcome::Empty => Some(Err(ReadError::unexpected_eof(
            "empty input",
            SourcePos { line: 1, col: 1 },
        ))),
    }
}

/// Try to parse every top-level form in `input` through the elisp
/// pipeline.  Returns `None` on any elisp-side error (= caller
/// falls back to Rust); returns `Some(Ok(forms))` with a vector of
/// deep-cloned forms on success.
fn try_elisp_read_all(input: &str) -> Option<Result<Vec<Sexp>, ReadError>> {
    let mut state = ElispReadState::new(input);
    let mut forms = Vec::new();
    loop {
        match state.parse_one_form()? {
            ElispParseOutcome::Form(form) => forms.push(form),
            ElispParseOutcome::Empty => break,
        }
    }
    Some(Ok(forms))
}

/// Outcome of one `parse_one_form' call against the elisp parser:
/// either a parsed `Sexp' or "no more forms" (= the cursor is past
/// every meaningful token).
enum ElispParseOutcome {
    Form(Sexp),
    Empty,
}

/// Per-call lex+parse state carrying the slot pool, scratch MutStr,
/// cursor slot, and result slot.  Allocated fresh per `read_str' /
/// `read_all' call (= no cross-call sharing).
///
/// On `Drop`, the slot pool + result slot are intentionally
/// `mem::forget'-ed (= leaked) rather than recursively dropped:
/// the §116.B parser's cons-make MVP creates Sexp::Cons aliases
/// across the result slot, the per-depth working slots, and any
/// in-flight intermediates without bumping inner-box refcounts.
/// Letting Rust drop normally would double-free those shared boxes
/// (= refcount-1 → 0 once per alias).  The leaked memory is
/// reclaimed by the process exit between test runs; in production
/// the wrapper is invoked from image-baker / dev paths where the
/// leak is acceptable.  A future §123.F refcount-aware cons-make
/// retires the leak by making the byte-copy aliases actual
/// refcount bumps.
struct ElispReadState {
    /// Sexp::Str source (= owns the input bytes for the call).
    src: Sexp,
    /// Sexp::Int(byte-cursor) — advanced in place by the lexer /
    /// parser.
    cursor_slot: Sexp,
    /// Receives the parsed form.
    result_slot: Sexp,
    /// `Sexp::Vector(N)' slot pool.  Layout matches the §116.B
    /// parser ABI: slot 0 = MutStr scratch, slot 1 = payload, slot
    /// 2 = const-Nil, slots 3+ = per-depth working slots.
    pool_slot: Sexp,
}

impl Drop for ElispReadState {
    fn drop(&mut self) {
        // See struct-level doc: `mem::forget' the pool + result to
        // avoid double-free of cons-make MVP aliased boxes.  The
        // `src' / `cursor_slot' fields don't carry refcounted boxes
        // (Sexp::Str owns its String; Sexp::Int is Copy) so we let
        // them drop normally.
        let stale_result = std::mem::replace(&mut self.result_slot, Sexp::Nil);
        std::mem::forget(stale_result);
        let stale_pool = std::mem::replace(&mut self.pool_slot, Sexp::Nil);
        std::mem::forget(stale_pool);
    }
}

impl ElispReadState {
    fn new(input: &str) -> Self {
        let src = Sexp::Str(input.to_string());
        let cursor_slot = Sexp::Int(0);
        let result_slot = Sexp::Nil;
        let mut slots = vec![Sexp::Nil; PARSER_POOL_SIZE];
        // Pre-allocate the scratch MutStr at slot 0.  The parser
        // resets it between lex calls via `mut-str-make-empty', so
        // capacity 64 here is just a non-zero seed.
        slots[0] = Sexp::mut_str(String::with_capacity(SCRATCH_CAP as usize));
        let pool_slot = Sexp::vector(slots);
        ElispReadState {
            src,
            cursor_slot,
            result_slot,
            pool_slot,
        }
    }

    /// Read the current cursor value out of the i64-int slot.
    fn cursor(&self) -> i64 {
        match self.cursor_slot {
            Sexp::Int(n) => n,
            _ => 0,
        }
    }

    /// Lex one token at the current cursor (= no parser involvement).
    /// Returns the kind code and leaves `cursor_slot' pointing at
    /// the byte just past the lexed token.  Side-effect: the
    /// payload slot at pool slot 1 may be updated.
    fn lex_peek_advancing(&mut self) -> i64 {
        // Reset scratch (= drain any prior accumulated bytes) before
        // each lex call, matching the §116.B parser discipline.
        unsafe {
            let pool_ptr = &self.pool_slot as *const Sexp;
            let scratch_ptr =
                vector_slot_ptr(pool_ptr, 0) as *mut Sexp;
            elisp_cc_spike::mut_str_make_empty(scratch_ptr, SCRATCH_CAP);
        }
        let cursor = self.cursor();
        let payload_ptr;
        let scratch_ptr;
        let cursor_out_ptr;
        unsafe {
            let pool_ptr = &self.pool_slot as *const Sexp;
            payload_ptr = vector_slot_ptr(pool_ptr, 1) as *mut Sexp;
            scratch_ptr = vector_slot_ptr(pool_ptr, 0) as *mut Sexp;
            cursor_out_ptr = &mut self.cursor_slot as *mut Sexp;
        }
        unsafe {
            elisp_cc_spike::reader_lex_one(
                &self.src as *const Sexp,
                cursor,
                payload_ptr,
                cursor_out_ptr,
                scratch_ptr,
            )
        }
    }

    /// Skip leading whitespace + comments by peeking one token; if
    /// it's EOF (kind 0), we have no more forms.  Otherwise reset
    /// the cursor and return `false' so the caller can parse.
    fn at_eof_after_skip(&mut self) -> bool {
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        if kind == 0 {
            // EOF: leave cursor at end-of-input.
            true
        } else {
            // Restore cursor so the parser sees the same starting
            // position the peek did.
            self.cursor_slot = Sexp::Int(saved);
            false
        }
    }

    /// Parse one top-level form.  Returns `None` if elisp errored
    /// (= unsupported feature or genuine syntax error — caller falls
    /// back to Rust), `Some(Form)` on success, `Some(Empty)` if the
    /// remaining input is whitespace/comments only.
    fn parse_one_form(&mut self) -> Option<ElispParseOutcome> {
        if self.at_eof_after_skip() {
            return Some(ElispParseOutcome::Empty);
        }
        // Reset result slot to Nil before the parse (= the parser
        // overwrites it with the parsed Sexp).
        //
        // NB: we don't simply assign `self.result_slot = Sexp::Nil'
        // here, because the previous form's `Sexp::Cons' may have
        // been raw-byte-copied into the slot pool via cons-make MVP
        // (= no refcount bump on inner boxes).  If we drop the old
        // `result_slot' value normally the destructor would
        // decrement a refcount the pool also aliases; that's the
        // exact double-free pattern `mem::forget(pool_slot)' in the
        // §116.B probe sidesteps.  Forget the old value here for
        // the same reason — the second-form deep-clone has already
        // detached the live data.
        let stale = std::mem::replace(&mut self.result_slot, Sexp::Nil);
        std::mem::forget(stale);
        let status = unsafe {
            elisp_cc_spike::reader_parse_one(
                &self.src as *const Sexp,
                &mut self.cursor_slot as *mut Sexp,
                &mut self.result_slot as *mut Sexp,
                &self.pool_slot as *const Sexp,
                0,
            )
        };
        if status != 1 {
            return None;
        }
        // Deep-clone the result so it stops sharing heap boxes with
        // the slot pool.  See `deep_clone' for rationale.
        let cloned = deep_clone(&self.result_slot);
        Some(ElispParseOutcome::Form(cloned))
    }

    /// Peek one more token at the current cursor.  Returns `true' if
    /// it's anything other than EOF (= a real trailing token, which
    /// `read_str' rejects).
    fn has_trailing_token(&mut self) -> bool {
        let saved = self.cursor();
        let kind = self.lex_peek_advancing();
        let trailing = kind != 0;
        // Restore cursor so the caller's bookkeeping stays accurate
        // (we don't need it past this point, but the discipline
        // avoids future surprises).
        self.cursor_slot = Sexp::Int(saved);
        trailing
    }
}

/// Return a `*const Sexp` pointing at slot INDEX of `pool_slot' (=
/// must be a `Sexp::Vector`).  Used to hand individual slots to the
/// §116.A / §116.B C ABI.
///
/// # Safety
/// `pool_slot' must point at a live `Sexp::Vector' with at least
/// `index + 1' slots, and the caller must not invalidate the
/// returned pointer (= drop `pool_slot' or shrink the vector)
/// before the C call returns.
unsafe fn vector_slot_ptr(pool_slot: *const Sexp, index: usize) -> *const Sexp {
    match &*pool_slot {
        Sexp::Vector(v) => {
            // SAFETY: the vector is alive (caller invariant) and we
            // borrow its backing slice + index without taking a
            // mutable handle.
            let slice: &[Sexp] = v.value.as_slice();
            &slice[index] as *const Sexp
        }
        _ => unreachable!("pool_slot must be a Sexp::Vector"),
    }
}

// ---------------------------------------------------------------------------
// §116.C deep-clone helper — escape the slot-pool refcount sharing
// ---------------------------------------------------------------------------

/// Walk `s' and produce a fresh refcount-1-owning copy.  Required
/// because the §116.B parser's `cons-make' grammar op is MVP-shape
/// (= raw 32-byte payload copy from per-depth pool slots into the
/// result without bumping the inner boxes' refcounts).  Without
/// this, dropping the slot pool would free heap boxes the returned
/// `Sexp::Cons' still references.
///
/// For leaf variants (`Nil` / `T` / `Int` / `Float` / `Symbol` /
/// `Str` — none of which the §116.B parser shares with the pool
/// since each call allocates a fresh String) the simple `clone()`
/// is already a fresh copy.  Only `Cons' / `Vector' / `Record' /
/// `CharTable' / `Cell' / `BoolVector' / `MutStr' need to walk
/// children — and of those, only `Cons` is reachable from the
/// §116.B parser output (the others are unsupported by the §116.B
/// MVP and route through the Rust fallback path).  The walk is
/// nonetheless exhaustive so a future §116.B extension that adds
/// vector / record support can rely on the same helper.
///
/// A future §123.F refcount-aware `cons-make' (= bump inner boxes'
/// refcounts on the payload copy) retires this helper.
fn deep_clone(s: &Sexp) -> Sexp {
    match s {
        Sexp::Nil => Sexp::Nil,
        Sexp::T => Sexp::T,
        Sexp::Int(n) => Sexp::Int(*n),
        Sexp::Float(f) => Sexp::Float(*f),
        Sexp::Symbol(name) => Sexp::Symbol(name.clone()),
        Sexp::Str(text) => Sexp::Str(text.clone()),
        Sexp::MutStr(r) => Sexp::mut_str(r.value.clone()),
        Sexp::Cons(b) => {
            // Allocate a fresh box (= refcount 1) holding deep-
            // cloned children.  This is the load-bearing arm.
            Sexp::cons(deep_clone(&b.car), deep_clone(&b.cdr))
        }
        Sexp::Vector(v) => {
            let items: Vec<Sexp> = v.value.iter().map(deep_clone).collect();
            Sexp::vector(items)
        }
        Sexp::CharTable(_) | Sexp::BoolVector(_) | Sexp::Cell(_) | Sexp::Record(_) => {
            // The §116.B parser does not produce these.  Falling
            // back to a refcount-bump clone keeps the API total —
            // any future caller that synthesises one of these from
            // the parser output through an extension should add an
            // explicit deep-walk arm.
            s.clone()
        }
    }
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
        let expected = Sexp::cons(Sexp::Symbol("a".into()), Sexp::Symbol("b".into()));
        assert_eq!(got, expected);
    }

    /// Doc 116 §116.B+ vector arm (2026-05-18) — `[..]' is now
    /// handled by the elisp parser (= `nelisp_reader_p_parse_vector'
    /// + `_step' + `_fill_vec' + `_cons_list_len_walk'), unblocking
    /// the eval-boot stdlib path which hits `nl-ffi-call' vector
    /// signature literals.  Empty `[]', nested vectors, and vectors
    /// inside lists / quotes all compose via `p_dispatch' kind 3.
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

    /// Doc 44 §3.2 byte-code-literal `#[...]` is the last remaining
    /// deferred reader feature (reader-side; the bytecode evaluator
    /// itself is a Phase 8 concern).  `?\\M-a' was promoted from
    /// deferred to supported in Doc 51 Phase 3-A''-2.
    #[test]
    fn byte_code_literal_still_deferred() {
        match read_str("#[1 2]") {
            Err(ReadError::NotYetImplemented { .. }) => (),
            Err(other) => panic!("expected NotYetImplemented for #[1 2], got {:?}", other),
            Ok(v) => panic!("expected NotYetImplemented for #[1 2], got Ok({:?})", v),
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

    // ---- §116.C wire-in coverage ----

    /// §116.C smoke — symbol-only inputs route through the elisp
    /// pipeline (= no `?' sentinel, no Float, no `[..]', no `#x..').
    /// Asserts the deep-clone path produces a structurally-equal
    /// value that survives slot-pool drop.
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

    /// §116.C deep-clone discipline — the returned form must not
    /// alias the parser's slot pool.  Stress-test: parse a deeply
    /// nested list, drop everything, then walk the result to force
    /// every cons cell to be touched.  A refcount-leaked alias to
    /// freed pool memory would surface as garbage `Sexp' variants
    /// during the walk (or a SIGSEGV under Miri / sanitizers).
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
}
