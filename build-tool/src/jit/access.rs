//! Phase 7.1.6 cluster takeover (Doc 28 §3.6 COMPLETE) — access trampolines,
//! dlsym-exported.
//!
//! The 4 `nl_jit_access_*' trampolines below are `#[no_mangle] pub unsafe
//! extern "C"'.  Body shape: tag check on the `#[repr(C, u8)]' Sexp byte
//! → `*_box_ptr()' deref → field clone, `OK = 0' / `ERR = 1' status.
//! The inline-NIL fast path for `length' is the `tag == SEXP_TAG_NIL'
//! arm (= returns `OK' with `Sexp::Int(0)' in `out').
//!
//! Two callers reach the trampolines at runtime: (1) nelisp-cc compiled
//! hot paths via `:ssa-call-primitive' + `nelisp-cc--dlsym-resolve'
//! direct fixup (which can also emit the inline-NIL short-circuit as
//! host machine code before the CALL), and (2) `nelisp-jit-substrate.el'
//! / `-strategy.el' via `bridge::unified_fn_ptr's name → fn-ptr table.
//!
//! # Doc 120 §120.D swap status (2026-05-18)
//!
//! 4 of 4 trampolines moved to Phase-47-compiled elisp on linux-x86_64:
//!
//!   - `nl_jit_access_length' → `lisp/nelisp-cc-jit-length.el'
//!     (= Nil + Vector arms inline; Str arm via narrow
//!     `nl_jit_access_length_str_inner' extern — UTF-8 codepoint
//!     count not yet expressible in Phase 47, kept in Rust).
//!   - `nl_jit_access_aref'   → `lisp/nelisp-cc-jit-aref.el'
//!     (= Vector arm via `vector-ref' + inline bounds check;
//!     BoolVector arm via `extern-call' to the narrow
//!     `nl_jit_access_aref_bool_vector_inner' helper below).
//!   - `nl_jit_access_aset'   → `lisp/nelisp-cc-jit-aset.el'
//!     (= Vector arm via `vector-slot-set' + `nl_sexp_clone_into';
//!     BoolVector arm via narrow `_aset_bool_vector_inner' helper).
//!   - `nl_jit_access_elt'    → `lisp/nelisp-cc-jit-elt.el'
//!     (= Vector arm same as `aref'; Cons arm via recursive
//!     `cons-cdr-raw-from-box' walker — same shape §101.B `length'
//!     established).
//!
//! Sub-arms covered by narrow Rust externs (= `extern-call' from
//! Phase 47 elisp body; each helper is ~10 LOC of bounded scope):
//!
//!   - `length' Str arm — `nl_jit_access_length_str_inner' performs
//!     `s.chars().count()'.  Needs UTF-8 codepoint-count grammar
//!     primitive to fully swap (= same blocker as §120.B
//!     `nl_jit_mut_str_len' and §120.C `nl_jit_intern' /
//!     `_split_by_non_alnum').  Adding a `(str-char-count H)' op
//!     that walks the UTF-8 byte stream and counts codepoints would
//!     unblock; Phase 47 currently only reads `String::len' (= byte
//!     count) via `str-len'.  See Doc 122 §122.A `mut-str-char-
//!     count' cluster.
//!
//!   - `aref' / `aset' BoolVector arms — needs `bool-vector-{len,
//!     bit,set-bit}' grammar primitives.  Phase 47 has no
//!     bool-vector ops yet (= mechanical to add per §120.B
//!     blocker note; same shape as `vector-len' / `vector-ref' at a
//!     different offset constant + a bit-shift decode).  The narrow
//!     `nl_jit_access_{aref,aset}_bool_vector_inner' externs below
//!     are the minimum viable swap — they keep the bool-vector
//!     codepath alive while consolidating the rest of each
//!     trampoline body into Phase 47.  See Doc 122 §122.B
//!     `bool-vector-*' cluster.
//!
//! On linux-x86_64 the Rust `nl_jit_access_*' functions below are
//! kept for fallback parity + in-file unit tests (= dead code that
//! the linker keeps via `#[no_mangle]' + `-rdynamic', similar to
//! other arch-specific reference impls).  Other targets still route
//! through these Rust trampolines via the `access_link' stub in
//! `bridge.rs' until the §120.D elisp emit is generalized.

use crate::eval::sexp::{
    Sexp, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CONS, SEXP_TAG_NIL, SEXP_TAG_STR,
    SEXP_TAG_VECTOR,
};

const TRAMPOLINE_OK: i64 = 0;
const TRAMPOLINE_ERR: i64 = 1;

// Phase A.5 (Doc 77c §2.1.4): trampolines dispatch on `Sexp::tag()' and
// read box pointers via `Sexp::*_box_ptr()' instead of `match'.  Each
// arm collapses to "tag check → ptr deref → field op".

/// `(length OBJ)' fast path: `Nil' / `Vector' / `Str'.  Other types
/// return `TRAMPOLINE_ERR' so the caller falls through to `bi_length'.
///
/// Phase 7.1.6.b (Doc 28 §3.6.b / Doc 81 §5.4): `#[no_mangle]' so the
/// dlsym bridge (`bi_dlsym_resolve') can locate this symbol at runtime
/// when the recognition pass emits `:ssa-call-primitive :symbol
/// nl_jit_access_length'.  The dynamic symbol export requires
/// `-rdynamic' (= `.cargo/config.toml' `[build] rustflags').
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
    let tag = (*arg).tag();
    if tag == SEXP_TAG_NIL {
        *out = Sexp::Int(0);
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        *out = Sexp::Int(box_ref.value.len() as i64);
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_STR {
        // Str is `String' inline at offset 8; the simplest correct read
        // is through the match arm (= no separate box).
        if let Sexp::Str(s) = &*arg {
            *out = Sexp::Int(s.chars().count() as i64);
            return TRAMPOLINE_OK;
        }
    }
    TRAMPOLINE_ERR
}

/// `(aref VECTOR INDEX)' fast path: `Sexp::Vector' / `Sexp::BoolVector'
/// with non-negative INDEX in range.  Out-of-range / wrong-type /
/// negative index returns `TRAMPOLINE_ERR' for canonical-error fall-
/// through (= the dispatcher's `aref_helper' surfaces the proper
/// out-of-range / wrong-type message).
///
/// Phase 7.1.6.b dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        if (idx as usize) >= box_ref.value.len() {
            return TRAMPOLINE_ERR;
        }
        let idx_sexp = Sexp::Int(idx);
        let returned = crate::elisp_cc_spike::aref_vector(
            arg,
            &idx_sexp as *const Sexp,
            out,
        );
        if returned == out {
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    if tag == SEXP_TAG_BOOL_VECTOR {
        return nl_jit_access_aref_bool_vector_inner(arg, idx, out);
    }
    TRAMPOLINE_ERR
}

/// `(aset VECTOR INDEX VALUE)' fast path: `Sexp::Vector' /
/// `Sexp::BoolVector'.  Returns VALUE per Emacs' `aset' contract.
/// `MutStr' aset (= codepoint mutation) is left to the dispatcher
/// because the rebuild-String path is not worth a JIT helper.
///
/// Phase 7.1.6.b dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aset(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ptr = (*arg).vector_box_ptr() as *mut crate::eval::nlvector::NlVector;
        let len = (&*box_ptr).value.len();
        if (idx as usize) >= len {
            return TRAMPOLINE_ERR;
        }
        // SAFETY: Phase A.4.3 — bounds-checked, no other `&Vec<Sexp>'
        // borrow live.  Phase A.2.1 setcar discipline applies.
        let value_ref = &mut (*box_ptr).value;
        value_ref[idx as usize] = (*val).clone();
        *out = (*val).clone();
        return TRAMPOLINE_OK;
    }
    if tag == SEXP_TAG_BOOL_VECTOR {
        return nl_jit_access_aset_bool_vector_inner(arg, idx, val, out);
    }
    TRAMPOLINE_ERR
}

/// `(elt SEQUENCE INDEX)' fast path: `Sexp::Vector' (= aref) /
/// `Sexp::Cons' (= list walk).  Other sequence types fall through.
///
/// Phase 7.1.6.b dlsym-exported.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let tag = (*arg).tag();
    if tag == SEXP_TAG_VECTOR {
        let box_ref = &*(*arg).vector_box_ptr();
        if let Some(elem) = box_ref.value.get(idx as usize) {
            *out = elem.clone();
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    if tag == SEXP_TAG_CONS {
        let mut cur_ptr: *const Sexp = arg;
        let mut remaining = idx;
        loop {
            let cur_tag = (*cur_ptr).tag();
            if cur_tag == SEXP_TAG_CONS {
                let box_ref = &*(*cur_ptr).cons_box_ptr();
                if remaining == 0 {
                    *out = box_ref.car.clone();
                    return TRAMPOLINE_OK;
                }
                remaining -= 1;
                cur_ptr = std::ptr::addr_of!(box_ref.cdr);
                continue;
            }
            return TRAMPOLINE_ERR;
        }
    }
    TRAMPOLINE_ERR
}

// ---- Doc 120 §120.D narrow sub-arm externs ----
//
// Reached from the Phase 47 elisp bodies in `lisp/nelisp-cc-jit-
// aref.el' / `lisp/nelisp-cc-jit-aset.el' via the `(extern-call SYM
// ARG...)' grammar form (= same shape `nl_sexp_eq' uses for the
// §120.A predicate-eq slow path).  Phase 47 has no `bool-vector-*'
// grammar primitives yet (see Doc 122 §122.B cluster) so the
// bool-vector arms can't be expressed entirely in elisp; these
// narrow helpers shrink the surface area to just the bit decode +
// `Sexp::T' / `Sexp::Nil' tag-byte write.
//
// Both helpers reuse the pre-§120.D trampoline body logic (= bounds
// check + bool-vector box ptr deref) so the on-disk semantics match
// the deleted trampoline arm bit-for-bit.

/// `(length STR)' narrow `Sexp::Str' arm — reached from the
/// Phase 47 `nelisp_jit_length' body's Str tag arm.  Returns
/// codepoint count via `s.chars().count()' — the same Unicode-
/// aware char-walker the pre-§120.D trampoline used, kept in
/// Rust until Phase 47 grows a `(str-char-count H)' grammar op
/// (= Doc 122 §122.A `mut-str-char-count' cluster).
///
/// # Safety
/// - `arg' must point at `Sexp::Str(_)' — elisp tag-checks.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_length_str_inner(
    arg: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if let Sexp::Str(s) = &*arg {
        *out = Sexp::Int(s.chars().count() as i64);
        TRAMPOLINE_OK
    } else {
        TRAMPOLINE_ERR
    }
}

/// `(aref BV INDEX)' narrow BoolVector arm — reached from the
/// Phase 47 `nelisp_jit_aref' body's BoolVector tag arm.
///
/// # Safety
/// - `arg' must point at `Sexp::BoolVector(_)' — the elisp body
///   tag-checks before calling.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aref_bool_vector_inner(
    arg: *const Sexp,
    idx: i64,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let box_ref = &*(*arg).bool_vector_box_ptr();
    if let Some(b) = box_ref.value.get(idx as usize) {
        *out = if *b { Sexp::T } else { Sexp::Nil };
        return TRAMPOLINE_OK;
    }
    TRAMPOLINE_ERR
}

/// `(aset BV INDEX VALUE)' narrow BoolVector arm — reached from the
/// Phase 47 `nelisp_jit_aset' body's BoolVector tag arm.
///
/// # Safety
/// - `arg' must point at `Sexp::BoolVector(_)' — elisp tag-checks.
/// - `val' must point at an initialized `Sexp' for truthiness test.
/// - `out' must be non-null + writable for one 32-byte Sexp slot.
#[no_mangle]
pub unsafe extern "C" fn nl_jit_access_aset_bool_vector_inner(
    arg: *const Sexp,
    idx: i64,
    val: *const Sexp,
    out: *mut Sexp,
) -> i64 {
    if idx < 0 {
        return TRAMPOLINE_ERR;
    }
    let box_ptr = (*arg).bool_vector_box_ptr()
        as *mut crate::eval::nlboolvector::NlBoolVector;
    let len = (&*box_ptr).value.len();
    if (idx as usize) >= len {
        return TRAMPOLINE_ERR;
    }
    let bit = crate::eval::special_forms::is_truthy(&*val);
    // SAFETY: Phase A.4.4 — same discipline as the pre-§120.D
    // BoolVector arm above.
    let value_ref = &mut (*box_ptr).value;
    value_ref[idx as usize] = bit;
    *out = (*val).clone();
    TRAMPOLINE_OK
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jit_length_nil_vector_str() {
        let mut out = Sexp::Nil;

        let nil = Sexp::Nil;
        assert_eq!(
            unsafe { nl_jit_access_length(&nil as *const _, &mut out as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(0));

        let vec = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        assert_eq!(
            unsafe { nl_jit_access_length(&vec as *const _, &mut out as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(3));

        let s = Sexp::Str("hello".into());
        assert_eq!(
            unsafe { nl_jit_access_length(&s as *const _, &mut out as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Int(5));
    }

    #[test]
    fn jit_length_unsupported_returns_err() {
        let mut out = Sexp::Nil;
        let i = Sexp::Int(42);
        assert_eq!(
            unsafe { nl_jit_access_length(&i as *const _, &mut out as *mut _) },
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_vector_in_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![
            Sexp::Symbol("a".into()),
            Sexp::Symbol("b".into()),
            Sexp::Symbol("c".into()),
        ]);
        assert_eq!(
            unsafe { nl_jit_access_aref(&vec as *const _, 1, &mut out as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(out, Sexp::Symbol("b".into()));
    }

    #[test]
    fn jit_aref_out_of_range() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            unsafe { nl_jit_access_aref(&vec as *const _, 5, &mut out as *mut _) },
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_negative_index() {
        let mut out = Sexp::Nil;
        let vec = Sexp::vector(vec![Sexp::Int(7)]);
        assert_eq!(
            unsafe { nl_jit_access_aref(&vec as *const _, -1, &mut out as *mut _) },
            TRAMPOLINE_ERR
        );
    }

    #[test]
    fn jit_aref_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        assert_eq!(
            unsafe { nl_jit_access_aref(&s as *const _, 0, &mut out as *mut _) },
            TRAMPOLINE_ERR
        );
    }

    // --- Doc 77 Stage 1.B (2026-05-09) — BoolVector trampoline coverage ---

    #[test]
    fn jit_aref_bool_vector_in_range() {
        // bv = [false, true, false] → aref(bv, 0) = Nil, aref(bv, 1) = T.
        let bv = Sexp::bool_vector(3, false);
        if let Sexp::BoolVector(rc) = &bv {
            // SAFETY: no other borrow live in this test setup.
            unsafe {
                rc.with_value_mut(|v| v[1] = true);
            }
        } else {
            panic!("bool_vector did not produce BoolVector");
        }
        let mut out = Sexp::Nil;
        let r = unsafe { nl_jit_access_aref(&bv as *const _, 0, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        let r = unsafe { nl_jit_access_aref(&bv as *const _, 1, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::T);
        let r = unsafe { nl_jit_access_aref(&bv as *const _, 2, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
    }

    #[test]
    fn jit_aref_bool_vector_out_of_range() {
        let bv = Sexp::bool_vector(2, true);
        let mut out = Sexp::Nil;
        let r = unsafe { nl_jit_access_aref(&bv as *const _, 5, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_bool_vector_in_range_mutates() {
        // bv = [true, true, true] → aset(bv, 1, nil) flips slot 1 to false.
        let bv = Sexp::bool_vector(3, true);
        let val_nil = Sexp::Nil;
        let mut out = Sexp::T;
        let r = unsafe {
            nl_jit_access_aset(
                &bv as *const _,
                1,
                &val_nil as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Nil);
        // Confirm the mutation: aref returns Nil at slot 1, T elsewhere.
        let mut probe = Sexp::Nil;
        assert_eq!(
            unsafe { nl_jit_access_aref(&bv as *const _, 0, &mut probe as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
        assert_eq!(
            unsafe { nl_jit_access_aref(&bv as *const _, 1, &mut probe as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::Nil);
        // Truthy non-Nil value sets slot to true.
        let val_int = Sexp::Int(42);
        let r = unsafe {
            nl_jit_access_aset(
                &bv as *const _,
                1,
                &val_int as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val_int);
        assert_eq!(
            unsafe { nl_jit_access_aref(&bv as *const _, 1, &mut probe as *mut _) },
            TRAMPOLINE_OK
        );
        assert_eq!(probe, Sexp::T);
    }

    #[test]
    fn jit_aset_bool_vector_out_of_range_returns_err() {
        let bv = Sexp::bool_vector(2, false);
        let val = Sexp::T;
        let mut out = Sexp::Nil;
        let r = unsafe {
            nl_jit_access_aset(
                &bv as *const _,
                5,
                &val as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    // --- Stage 5.6 (2026-05-07) — aset / elt trampolines ---

    #[test]
    fn jit_aset_vector_in_range_mutates() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)]);
        let val = Sexp::Symbol("replaced".into());
        let r = unsafe {
            nl_jit_access_aset(
                &v as *const _,
                1,
                &val as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, val);
        // Confirm the mutation through aref.
        let mut got = Sexp::Nil;
        let r = unsafe { nl_jit_access_aref(&v as *const _, 1, &mut got as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(got, val);
    }

    #[test]
    fn jit_aset_out_of_range_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(10)]);
        let val = Sexp::Int(99);
        let r = unsafe {
            nl_jit_access_aset(
                &v as *const _,
                5,
                &val as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_aset_non_vector_returns_err() {
        let mut out = Sexp::Nil;
        let s = Sexp::Str("abc".into());
        let val = Sexp::Int(42);
        let r = unsafe {
            nl_jit_access_aset(
                &s as *const _,
                0,
                &val as *const _,
                &mut out as *mut _,
            )
        };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_vector_path() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![
            Sexp::Symbol("x".into()),
            Sexp::Symbol("y".into()),
            Sexp::Symbol("z".into()),
        ]);
        let r = unsafe { nl_jit_access_elt(&v as *const _, 2, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Symbol("z".into()));
    }

    #[test]
    fn jit_elt_list_walks_to_index() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
            Sexp::Int(4),
        ]);
        let r = unsafe { nl_jit_access_elt(&lst as *const _, 2, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_OK);
        assert_eq!(out, Sexp::Int(3));
    }

    #[test]
    fn jit_elt_list_overrun_returns_err() {
        let mut out = Sexp::Nil;
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2)]);
        let r = unsafe { nl_jit_access_elt(&lst as *const _, 5, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_nil_returns_err() {
        let mut out = Sexp::Nil;
        let nil = Sexp::Nil;
        let r = unsafe { nl_jit_access_elt(&nil as *const _, 0, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_ERR);
    }

    #[test]
    fn jit_elt_negative_index_returns_err() {
        let mut out = Sexp::Nil;
        let v = Sexp::vector(vec![Sexp::Int(1)]);
        let r = unsafe { nl_jit_access_elt(&v as *const _, -1, &mut out as *mut _) };
        assert_eq!(r, TRAMPOLINE_ERR);
    }
}
