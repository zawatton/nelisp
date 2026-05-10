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

use crate::eval::sexp::{
    Sexp, SEXP_TAG_BOOL_VECTOR, SEXP_TAG_CONS, SEXP_TAG_NIL, SEXP_TAG_STR,
    SEXP_TAG_VECTOR,
};

pub(super) const TRAMPOLINE_OK: i64 = 0;
pub(super) const TRAMPOLINE_ERR: i64 = 1;

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
        if let Some(elem) = box_ref.value.get(idx as usize) {
            *out = elem.clone();
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
    }
    if tag == SEXP_TAG_BOOL_VECTOR {
        let box_ref = &*(*arg).bool_vector_box_ptr();
        if let Some(b) = box_ref.value.get(idx as usize) {
            *out = if *b { Sexp::T } else { Sexp::Nil };
            return TRAMPOLINE_OK;
        }
        return TRAMPOLINE_ERR;
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
        let box_ptr = (*arg).bool_vector_box_ptr()
            as *mut crate::eval::nlboolvector::NlBoolVector;
        let len = (&*box_ptr).value.len();
        if (idx as usize) >= len {
            return TRAMPOLINE_ERR;
        }
        let bit = crate::eval::special_forms::is_truthy(&*val);
        // SAFETY: Phase A.4.4 — same discipline as the Vector arm above.
        let value_ref = &mut (*box_ptr).value;
        value_ref[idx as usize] = bit;
        *out = (*val).clone();
        return TRAMPOLINE_OK;
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
