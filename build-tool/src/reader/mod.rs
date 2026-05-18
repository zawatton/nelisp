//! Rust-side minimal Elisp reader / sexp parser.  Public surface
//! (`read_str' / `read_all') is narrow — stops at `Sexp', never
//! depends on the evaluator.  Every input routes through the pure-
//! elisp pipeline `nelisp_reader_lex_one' + `nelisp_reader_parse_one'
//! (via `elisp_cc_spike').  This module retains only the dispatch
//! layer + deep-clone helper + `ReadError' re-export.  User-visible
//! `read' / `read-from-string' delegate to the elisp implementation
//! in `lisp/nelisp-stdlib-reader.el'.  Unsupported features (record
//! `#s(..)', byte-code `#[..]') surface as `NotYetImplemented'.
//!
//! The cons-make MVP (= raw 32-byte payload copy without inner-box
//! refcount bump) means parser output aliases the caller-owned slot
//! pool; [`deep_clone`] rebuilds a refcount-1-owning copy before
//! return so the pool can be dropped safely.

// Value type + read-side error type live in `eval/sexp.rs' and
// `eval/error.rs'; re-exported here for legacy `use crate::reader::Sexp'.
pub use crate::eval::error::{ReadError, SourcePos};
pub use crate::eval::sexp::{fmt_sexp, Sexp};

use crate::elisp_cc_spike;

/// Slot-pool capacity for the elisp parser.  Depth counter increments
/// per-list-element + per-nesting-level (= ~3*N slots for N-deep cons
/// chains).  Cap 1024 covers stdlib `defmacro' bodies whose cons
/// chains nest 12+ deep.  Matches
/// `tests/elisp_cc_reader_parser_probe.rs::POOL_SIZE'.
const PARSER_POOL_SIZE: usize = 3 + 4 * 1024;

/// Initial scratch MutStr capacity.  64 bytes covers ~all atoms in
/// the boot stdlib without reallocation; mut-str grows as needed.
const SCRATCH_CAP: i64 = 64;

/// Parse exactly one top-level form from `input`.  Trailing non-
/// whitespace tokens are an error — use [`read_all`] for multi-form.
/// Unsupported features (record `#s(..)', byte-code `#[..]') surface
/// as `NotYetImplemented'.
pub fn read_str(input: &str) -> Result<Sexp, ReadError> {
    match try_elisp_read_str(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(
            "feature unsupported by elisp Reader \
             (record `#s(..)', byte-code `#[..]', or syntax error)",
            SourcePos { line: 1, col: 1 },
        )),
    }
}

/// Parse every top-level form in `input`.  Returns an empty vector
/// for empty / whitespace-only input.  Unsupported features surface
/// as `NotYetImplemented' read errors.
pub fn read_all(input: &str) -> Result<Vec<Sexp>, ReadError> {
    match try_elisp_read_all(input) {
        Some(result) => result,
        None => Err(ReadError::not_yet_implemented(
            "feature unsupported by elisp Reader \
             (record `#s(..)', byte-code `#[..]', or syntax error)",
            SourcePos { line: 1, col: 1 },
        )),
    }
}

// ---------------------------------------------------------------------------
// elisp dispatch — pure-elisp lex + parse via `elisp_cc_spike'
// ---------------------------------------------------------------------------

/// Try to parse one top-level form through the elisp pipeline.
/// Returns:
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
/// cursor slot, and result slot.  Allocated fresh per call (no
/// cross-call sharing).
///
/// On `Drop`, the slot pool + result slot are `mem::forget'-ed: the
/// parser's cons-make MVP creates Sexp::Cons aliases across the
/// result slot, per-depth working slots, and in-flight intermediates
/// without bumping inner-box refcounts; letting Rust drop normally
/// would double-free those shared boxes.  Leaked memory is reclaimed
/// by process exit (test runs) or kept short-lived (dev paths).  A
/// future refcount-aware cons-make retires the leak.
struct ElispReadState {
    /// Sexp::Str source (= owns the input bytes for the call).
    src: Sexp,
    /// Sexp::Int(byte-cursor) — advanced in place by the lexer /
    /// parser.
    cursor_slot: Sexp,
    /// Receives the parsed form.
    result_slot: Sexp,
    /// `Sexp::Vector(N)' slot pool.  Layout: slot 0 = MutStr scratch,
    /// slot 1 = payload, slot 2 = const-Nil, slots 3+ = per-depth.
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
        // Pre-allocate scratch MutStr at slot 0; parser resets it
        // between lex calls via `mut-str-make-empty'.
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
        // Reset scratch before each lex call (parser discipline).
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
        // NB: we forget the old result rather than assigning Nil:
        // the previous form's Sexp::Cons may alias the slot pool
        // (cons-make MVP byte-copy, no refcount bump), so a normal
        // drop would double-free.  The deep-clone returned to the
        // caller already detached the live data.
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
/// elisp parser C ABI.
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
// deep-clone helper — escape slot-pool refcount sharing
// ---------------------------------------------------------------------------

/// Walk `s' and produce a fresh refcount-1-owning copy.  The
/// parser's `cons-make' grammar op is MVP-shape (raw 32-byte payload
/// copy without bumping inner boxes' refcounts), so dropping the
/// slot pool would free heap boxes the returned `Sexp::Cons' still
/// references without this rebuild.  The walk is exhaustive so
/// future parser extensions covering vector / record can reuse it.
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
            // Load-bearing arm — fresh refcount-1 box.
            Sexp::cons(deep_clone(&b.car), deep_clone(&b.cdr))
        }
        Sexp::Vector(v) => {
            let items: Vec<Sexp> = v.value.iter().map(deep_clone).collect();
            Sexp::vector(items)
        }
        Sexp::CharTable(_) | Sexp::BoolVector(_) | Sexp::Cell(_) | Sexp::Record(_) => {
            // Parser does not produce these; refcount-bump clone
            // keeps the API total for future extensions.
            s.clone()
        }
    }
}

