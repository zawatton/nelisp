//! Box accessor trampolines reached via `nl-jit-call-out-{1,1i,2i}'
//! bridge primitives.  4 record-family trampolines (type / len / ref /
//! set) run in Phase 47-compiled elisp on linux-x86_64.
//!
//! Skipped (grammar gaps):
//!   - mut_str_len / str_codepoint_at / mut_str_set_codepoint — need
//!     UTF-8 char-count + codepoint decode + in-place write ops.
//!   - bool_vector_len — needs `(bool-vector-len H)' grammar op.
//!   - char_table_aref / _aset — parent-chain walk not yet expressible.
//!   - record_alloc — list-walk-to-count + dynamic slot fill.
use crate::eval::sexp::Sexp;
const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

/// Helper for the Phase-47-compiled `nl_jit_record_type' swap.
/// Returns `*const Sexp' to the record's inline `type_tag' field, or
/// null when `arg' is not `Sexp::Record'.  Composes with
/// `nl_sexp_clone_into' for the refcount-aware copy into the out-slot,
/// same way `record-slot-ref'
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
