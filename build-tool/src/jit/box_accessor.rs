//! Doc 84 §84.3 — box accessor trampolines reached via the existing
//! `nl-jit-call-out-{1,1i,2i}' bridge primitives.  Replace the
//! deleted `strategy.rs' Box accessor 6 `bi_*' fns; semantics now
//! live in `lisp/nelisp-jit-strategy.el'.
//!
//! # Doc 120 §120.B swap status (2026-05-18)
//!
//! 4 of 11 trampolines moved to Phase-47-compiled elisp on
//! linux-x86_64:
//!
//!   - `nl_jit_record_type'  → `lisp/nelisp-cc-jit-record.el'
//!     (= `record-type-tag' grammar op + sexp-tag guard).
//!   - `nl_jit_record_len'   → ditto (= `record-slot-count' +
//!     `sexp-int-make' compose).
//!   - `nl_jit_record_ref'   → ditto (= `record-slot-ref' refcount-
//!     aware slot copy + 3-arm guard).
//!   - `nl_jit_record_set'   → ditto (= `record-slot-set' refcount-
//!     safe overwrite + `nl_sexp_clone_into' extern-call for out).
//!
//! Skipped (= blockers documented inline):
//!
//!   - `nl_jit_mut_str_len' — needs UTF-8 char-count primitive.  The
//!     trampoline returns `rc.value.chars().count()' (= codepoint
//!     count) but Phase 47's `(str-len H)' reads the byte-length
//!     field at `String' header offset 16, NOT the char count.  For
//!     ASCII the two are equal but mut-strings can hold multi-byte
//!     UTF-8 (`elisp `length' of a string returns char count, not
//!     byte count — see `nelisp--mut-str-len' caller convention in
//!     `lisp/nelisp-jit-strategy.el').  Needs a new `(str-char-count
//!     H)' grammar op that walks the UTF-8 byte stream and counts
//!     codepoints; out of scope for §120.B.
//!
//!   - `nl_jit_str_codepoint_at' — needs UTF-8 codepoint decode.
//!     The trampoline does `s.chars().nth(idx)' (= char-indexed
//!     codepoint).  Phase 47's `(str-byte-at H N)' reads byte N at
//!     the byte index, not char N at the char index.  An ASCII
//!     fast path (= `if byte < 128 return byte') plus fallback to
//!     a slow UTF-8 multi-byte decoder would work but requires a
//!     new grammar primitive for the variable-length decode.  Out
//!     of scope for §120.B.
//!
//!   - `nl_jit_mut_str_set_codepoint' — needs in-place UTF-8 write
//!     (= encode codepoint then either rewrite the matching byte
//!     range or fall back to `Vec<char>::collect` reassembly).
//!     Phase 47 has no mutable string write grammar; the Rust impl
//!     rebuilds the string from `chars()' which has no Phase 47
//!     analogue.  Out of scope for §120.B.
//!
//!   - `nl_jit_bool_vector_len' — no bool-vector grammar ops yet.
//!     Trampoline returns `v.value.len() as i64' from an inline
//!     `BoolVector' payload.  Adding a `(bool-vector-len H)' op is
//!     mechanical (same shape as `vector-len' at a different offset
//!     constant) but expanding the grammar is out of scope.
//!
//!   - `nl_jit_char_table_aref' — no char-table grammar primitives.
//!     The Rust impl delegates to `char_table_get' which performs a
//!     parent-chain walk through nested CharTableInner records; that
//!     is a complete sub-algorithm not yet expressible in Phase 47.
//!     Out of scope for §120.B.
//!
//!   - `nl_jit_char_table_aset' — same blocker as `_aref' plus
//!     refcount-aware char-table mutation (= `with_inner_mut'
//!     closure).  Out of scope for §120.B.
//!
//!   - `nl_jit_record_alloc' — needs list-walk-to-count + dynamic
//!     slot fill.  The trampoline walks a cons list of slot values
//!     and allocates a record sized to the list length.  Phase 47's
//!     `record-make' grammar op wants a static slot count at emit
//!     time (= the SLOT-COUNT arg is itself a value form but the
//!     fill loop afterwards has no Phase 47 expression — it would
//!     need either a 2-pass walk (count, then fill via `record-
//!     slot-set' in a loop) or a new `(record-fill-from-list)'
//!     primitive.  The 2-pass version is structurally feasible but
//!     would push this trampoline past 30 LOC of elisp; deferred
//!     until either approach is justified by a hot path.
//!
//! On linux-x86_64 the Rust `nl_jit_record_*' functions below are
//! kept for fallback parity + in-file unit tests (= dead code that
//! the linker keeps via `#[no_mangle]' + `-rdynamic', similar to
//! other arch-specific reference impls).  Other targets still route
//! through these Rust trampolines via the `box_accessor_link' stub
//! in `bridge.rs' until the §120.B elisp emit is generalized.
use crate::eval::sexp::Sexp;
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Doc 120 §120.B — helper for the Phase-47-compiled `nl_jit_record_type'
/// swap.  Returns a `*const Sexp' pointing at the record's inline
/// `type_tag' field (= byte offset `nelisp-nlrecord--offset-type-tag'
/// inside the NlRecord allocation), or a null pointer when `arg' does
/// not tag-decode as `Sexp::Record'.
///
/// The §120.B elisp body (= `nelisp_jit_record_type' in
/// `lisp/nelisp-cc-jit-record.el') already tag-checks `arg' before
/// calling this helper, so the `_ => null' arm only fires on bugs;
/// keeping it makes the helper safe to call from other Phase 47
/// objects in the future without an extra tag-check round-trip.
///
/// Composes with `nl_sexp_clone_into' (= refcount-aware copy of the
/// type-tag into the caller's out-slot) the same way `record-slot-ref'
/// composes through `--emit-record-slot-ptr-core' + `nl_sexp_clone_into'.
///
/// # Safety
/// - `arg' must be non-null and point at an initialized `Sexp'.
#[no_mangle]
pub unsafe extern "C" fn nl_record_type_tag_ptr(arg: *const Sexp) -> *const Sexp {
    match &*arg {
        Sexp::Record(rec) => {
            // NlRecord's `type_tag' lives at offset 0 inside the
            // allocation (= `nelisp-nlrecord--offset-type-tag').
            // `NlRecordRef' Derefs to `NlRecord' so `&rec.type_tag'
            // resolves to the inline field address; cast to a raw
            // pointer keeps the lifetime relationship local (= caller
            // is responsible for not outliving `*arg').
            &rec.type_tag as *const Sexp
        }
        _ => std::ptr::null(),
    }
}

/// MutStr length (char count).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_mut_str_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::MutStr(rc) => { *out = Sexp::Int(rc.value.chars().count() as i64); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// BoolVector length.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_bool_vector_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::BoolVector(v) => { *out = Sexp::Int(v.value.len() as i64); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// Char-indexed codepoint for `Str' / `MutStr'; ERR on OOR or wrong tag.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_str_codepoint_at(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    if idx < 0 { return TRAMPOLINE_ERR; }
    let s: &str = match &*arg {
        Sexp::Str(s) => s.as_str(),
        Sexp::MutStr(rc) => &rc.value,
        _ => return TRAMPOLINE_ERR,
    };
    match s.chars().nth(idx as usize) {
        Some(c) => { *out = Sexp::Int(c as i64); TRAMPOLINE_OK }
        None => TRAMPOLINE_ERR,
    }
}

/// In-place MutStr codepoint mutation; writes `*out = CP'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_mut_str_set_codepoint(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    if idx < 0 { return TRAMPOLINE_ERR; }
    let rc = match &*arg { Sexp::MutStr(r) => r, _ => return TRAMPOLINE_ERR };
    let cp = match &*val { Sexp::Int(n) => *n, _ => return TRAMPOLINE_ERR };
    let new_ch = match char::from_u32(cp as u32) {
        Some(c) => c, None => return TRAMPOLINE_ERR,
    };
    let chars: Vec<char> = rc.value.chars().collect();
    if (idx as usize) >= chars.len() { return TRAMPOLINE_ERR; }
    let new_str: String = chars.iter().enumerate()
        .map(|(i, c)| if i == idx as usize { new_ch } else { *c }).collect();
    // SAFETY: Phase A.4.2 — `new_str' is locally owned; the `chars()'
    // borrow ended at `collect' before `set_value' replaces in-place.
    rc.set_value(new_str);
    *out = (*val).clone();
    TRAMPOLINE_OK
}

/// CharTable read via `char_table_get' (parent-chain walk).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aref(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::CharTable(r) => {
            *out = crate::eval::builtins::char_table_get(r, idx); TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// In-place CharTable set; writes `*out = V'.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_char_table_aset(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::CharTable(r) => {
            // SAFETY: Phase A.4.6 — `with_inner_mut' closure owns the
            // only live `&mut CharTableInner' borrow; `(*val).clone()'
            // completes before the closure observes any aliasing.
            r.with_inner_mut(|i|
                crate::eval::builtins::char_table_set_one(i, idx, (*val).clone()));
            *out = (*val).clone(); TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

// Doc 86 §86.1.c (2026-05-10) — record family box accessor trampolines.
// Each replaces a deleted `bi_record_*' Rust dispatch arm and reaches
// elisp through the existing `nl-jit-call-out-{1,1i,2i,2}' bridge
// primitives — no new ABI mode, same shape as Doc 84 §84.3 above.

/// `(nelisp--record-type RECORD)' trampoline.  Returns the record's
/// `type_tag' symbol; ERR for non-Record input.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Record(rec) => { *out = rec.type_tag.clone(); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(nelisp--record-length RECORD)' trampoline.  Returns the user-slot
/// count (= `slots' vec len, type_tag NOT included); ERR for non-Record.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
    match &*arg {
        Sexp::Record(rec) => { *out = Sexp::Int(rec.slots.len() as i64); TRAMPOLINE_OK }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(nelisp--record-ref RECORD INDEX)' trampoline.  Returns slot INDEX
/// (0-based); ERR for non-Record or out-of-range index.  The elisp
/// wrapper distinguishes the two ERR causes by checking `recordp' before
/// calling so OOR surfaces as `arith-error' / "out-of-range".
#[no_mangle]
pub unsafe extern "C" fn nl_jit_record_ref(
    arg: *const Sexp, idx: i64, out: *mut Sexp,
) -> i64 {
    match &*arg {
        Sexp::Record(rec) => {
            if idx < 0 || (idx as usize) >= rec.slots.len() { return TRAMPOLINE_ERR; }
            *out = rec.slots[idx as usize].clone();
            TRAMPOLINE_OK
        }
        _ => TRAMPOLINE_ERR,
    }
}

/// `(nelisp--record-set RECORD INDEX VALUE)' trampoline.  Overwrites
/// slot INDEX with VALUE in-place, writes `*out = VALUE' (per pre-Doc-86
/// `bi_record_set' contract).  ERR for non-Record or OOR index.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_record_set(
    arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
) -> i64 {
    let rec = match &*arg { Sexp::Record(r) => r, _ => return TRAMPOLINE_ERR };
    let len = rec.slots.len();
    if idx < 0 || (idx as usize) >= len { return TRAMPOLINE_ERR; }
    let value = (*val).clone();
    // SAFETY: §86.1.c — `rec' is a deref-handle backed by the caller's
    // owned `Sexp::Record'; `with_slots_mut' closure mutates exactly one
    // slot and `value' is a fresh clone, no other live borrow into
    // `rec.slots'.  Phase A.2.1 setcar discipline applies.
    rec.with_slots_mut(|slots| { slots[idx as usize] = value.clone(); });
    *out = value;
    TRAMPOLINE_OK
}

/// `(nelisp--make-record TAG SLOTS-LIST)' trampoline.  Allocates a
/// fresh record with `type_tag = TAG' (= Symbol or Nil per pre-Doc-86
/// contract) and slots walked from SLOTS-LIST (= a proper Cons list or
/// Nil).  ERR for non-symbol/non-nil tag.  Routed through the existing
/// `nl-jit-call-out-2' bridge primitive; the elisp `nelisp--make-record'
/// wrapper builds the slots list from `&rest' args.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_record_alloc(
    tag: *const Sexp, list: *const Sexp, out: *mut Sexp,
) -> i64 {
    let tag = (*tag).clone();
    if !matches!(tag, Sexp::Symbol(_) | Sexp::Nil) {
        return TRAMPOLINE_ERR;
    }
    let mut slots: Vec<Sexp> = Vec::new();
    let mut p: Sexp = (*list).clone();
    loop {
        match p {
            Sexp::Nil => break,
            Sexp::Cons(b) => {
                slots.push(b.car.clone());
                p = b.cdr.clone();
            }
            // Improper list / non-cons mid-chain — treat as ERR so the
            // elisp wrapper can re-signal as wrong-type-argument.
            _ => return TRAMPOLINE_ERR,
        }
    }
    *out = Sexp::record(tag, slots);
    TRAMPOLINE_OK
}
