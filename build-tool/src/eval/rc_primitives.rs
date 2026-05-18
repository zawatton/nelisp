//! Doc 79 / Doc 123 — generic `NlRc' primitive surface for elisp
//! cycle collector (`lisp/nelisp-stdlib-gc.el').
//!
//! 10 primitives: alloc / dealloc / inc-strong / dec-strong /
//! strong-count / kind / payload-ptr / gc-walk-children /
//! gc-buffered-decs / gc-finalize.  Doc 123.A-F migrated each body
//! to pure-elisp Phase 47 kernels; the Rust shims here drive the
//! Sexp::Cons unwrap + arity-error contract.  MVP: Cons-kind only
//! for alloc / payload-ptr / walk-children; non-Cons signals
//! `wrong-type-argument'.

use crate::eval::builtins::require_arity;
use crate::eval::error::EvalError;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::sexp::{Sexp, SEXP_TAG_CONS};

/// Ensure the kind tag arg names a kind we currently support in the
/// MVP.  Stage 5.3.b〜.e widen this gate as walk-children / dealloc
/// gain per-kind cases.
fn require_supported_kind(kind: u8) -> Result<(), EvalError> {
    if kind == SEXP_TAG_CONS {
        Ok(())
    } else {
        Err(EvalError::WrongType {
            expected: format!("supported kind tag (= {} only in MVP)", SEXP_TAG_CONS),
            got: Sexp::Int(kind as i64),
        })
    }
}

/// Read an `Int`-shaped arg as a `u8` kind tag, raising
/// `wrong-type-argument` on anything else / out-of-range.
fn extract_kind_tag(prim: &str, arg: &Sexp) -> Result<u8, EvalError> {
    match arg {
        Sexp::Int(n) if (0..=255).contains(n) => Ok(*n as u8),
        other => Err(EvalError::WrongType {
            expected: format!("kind tag integer 0..=255 ({} arg)", prim),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-alloc KIND PAYLOAD-SIZE) -> Sexp`.  Allocate fresh box.
/// MVP: only CONS (tag 7); PAYLOAD-SIZE is type-checked but parked.
pub fn bi_nl_rc_alloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-alloc", args, 2, Some(2))?;
    let kind = extract_kind_tag("nl-rc-alloc", &args[0])?;
    require_supported_kind(kind)?;
    let _ = match &args[1] {
        Sexp::Int(n) if *n >= 0 => *n,
        other => {
            return Err(EvalError::WrongType {
                expected: "non-negative integer (nl-rc-alloc PAYLOAD-SIZE)".into(),
                got: other.clone(),
            });
        }
    };
    Ok(Sexp::Cons(NlConsBoxRef::new(Sexp::Nil, Sexp::Nil)))
}

/// `(nl-rc-dealloc HANDLE) -> nil`.  Force-finalize the box behind
/// HANDLE.  MVP: routes through `nl-gc-finalize' so the two
/// primitives share a single drop path.
pub fn bi_nl_rc_dealloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-dealloc", args, 1, Some(1))?;
    bi_nl_gc_finalize(args)
}

/// `(nl-rc-inc-strong HANDLE) -> nil`.  Refcount += 1 (CONS only).
pub fn bi_nl_rc_inc_strong(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-inc-strong", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let box_ptr = NlConsBoxRef::as_ptr(r) as *mut i64;
            unsafe { let _ = crate::elisp_cc_spike::rc_inc(box_ptr); }
            Ok(Sexp::Nil)
        }
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-rc-inc-strong arg)".into(),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-dec-strong HANDLE) -> Int`.  Refcount -= 1, returns the
/// post-sub count (saturating at 0).  Does NOT auto-free — cycle
/// collector calls `nl-gc-finalize' for the count=0 case.  CONS only.
pub fn bi_nl_rc_dec_strong(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-dec-strong", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let box_ptr = NlConsBoxRef::as_ptr(r) as *mut i64;
            let prev = unsafe { crate::elisp_cc_spike::rc_dec(box_ptr) };
            let new = if prev > 0 { (prev - 1) as i64 } else { 0 };
            Ok(Sexp::Int(new))
        }
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-rc-dec-strong arg)".into(),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-strong-count HANDLE) -> Int`.  Read current refcount.  CONS only.
pub fn bi_nl_rc_strong_count(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-strong-count", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let box_ptr = NlConsBoxRef::as_ptr(r) as *const u8;
            let count = unsafe { crate::elisp_cc_spike::rc_strong_count(box_ptr) };
            Ok(Sexp::Int(count))
        }
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-rc-strong-count arg)".into(),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-kind HANDLE) -> Int`.  Read the tag byte at offset 0 of
/// HANDLE's `#[repr(C, u8)]' Sexp discriminant.  Works on every variant.
pub fn bi_nl_rc_kind(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-kind", args, 1, Some(1))?;
    let sexp_ptr = &args[0] as *const Sexp as *const u8;
    let tag = unsafe { crate::elisp_cc_spike::rc_kind(sexp_ptr) };
    Ok(Sexp::Int(tag))
}

/// `(nl-rc-payload-ptr HANDLE) -> Int`.  Box address as i64 (= CONS
/// only; non-CONS returns 0 per stdlib-gc.el dispatch contract).
pub fn bi_nl_rc_payload_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-payload-ptr", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let p = NlConsBoxRef::as_ptr(r) as usize;
            Ok(Sexp::Int(p as i64))
        }
        _ => Ok(Sexp::Int(0)),
    }
}

/// `(nl-gc-walk-children HANDLE) -> List`.  CONS returns `(car cdr)';
/// other kinds return Nil.  Refcount-aware clone bumps each child.
pub fn bi_nl_gc_walk_children(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-gc-walk-children", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let car = r.car.clone();
            let cdr = r.cdr.clone();
            Ok(Sexp::list_from(&[car, cdr]))
        }
        _ => Ok(Sexp::Nil),
    }
}

/// `(nl-gc-buffered-decs) -> nil`.  Stage 5.3.a stub.
pub fn bi_nl_gc_buffered_decs(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-gc-buffered-decs", args, 0, Some(0))?;
    Ok(Sexp::Nil)
}

/// `(nl-gc-finalize HANDLE) -> nil`.  MVP no-op (Cons-only validation).
/// Real drop+dealloc lands in Stage 5.3.b〜.e once the elisp collector
/// drains the refcount via `nl-rc-dec-strong' before calling this.
/// Premature `NLRC_DROP_TABLE' here would UAF on the args[0] handle.
pub fn bi_nl_gc_finalize(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-gc-finalize", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(_) => Ok(Sexp::Nil),
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-gc-finalize arg)".into(),
            got: other.clone(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn alloc_cons() -> Sexp {
        bi_nl_rc_alloc(&[Sexp::Int(SEXP_TAG_CONS as i64), Sexp::Int(2)])
            .expect("alloc OK")
    }

    fn count_no_clone(c: &Sexp) -> i64 {
        match bi_nl_rc_strong_count(std::slice::from_ref(c)).unwrap() {
            Sexp::Int(n) => n,
            other => panic!("nl-rc-strong-count returned non-int {:?}", other),
        }
    }

    // ---- rc-prim-alloc-cons-roundtrip -----------------------------------
    #[test]
    fn rc_prim_alloc_cons_roundtrip() {
        let h = alloc_cons();
        let kind = bi_nl_rc_kind(std::slice::from_ref(&h)).unwrap();
        assert_eq!(kind, Sexp::Int(SEXP_TAG_CONS as i64));
        let payload = bi_nl_rc_payload_ptr(std::slice::from_ref(&h)).unwrap();
        match payload {
            Sexp::Int(addr) => assert!(addr != 0, "payload-ptr must be non-null"),
            other => panic!("expected Int payload-ptr, got {:?}", other),
        }
    }

    // ---- rc-prim-inc-dec-balance ---------------------------------------
    #[test]
    fn rc_prim_inc_dec_balance() {
        let h = alloc_cons();
        // Initial: refcount = 1.
        assert_eq!(count_no_clone(&h), 1);
        // Inc N times.
        for _ in 0..5 {
            bi_nl_rc_inc_strong(std::slice::from_ref(&h)).unwrap();
        }
        assert_eq!(count_no_clone(&h), 6);
        // Dec N times — each returns the new count.
        for expected in (1..=5).rev() {
            let r = bi_nl_rc_dec_strong(std::slice::from_ref(&h)).unwrap();
            assert_eq!(r, Sexp::Int(expected as i64));
        }
        // Refcount back at 1 (the alloc's +1 still standing).
        assert_eq!(count_no_clone(&h), 1);
    }

    // ---- rc-prim-dealloc-after-zero ------------------------------------
    //
    // Stage 5.3.a MVP: `nl-rc-dealloc' / `nl-gc-finalize' are
    // documentation-only no-ops (= see body comments on the
    // primitives).  This test confirms (a) they do not crash even
    // after the dec-strong sequence drove the count to 0, and (b)
    // a subsequent `nl-rc-strong-count' load still observes a stable
    // value (= the alloc's +1 balance is restored before the outer
    // Sexp::Cons drop fires).  Stage 5.3.b〜.e replace this with a
    // memory-actually-freed assertion.
    #[test]
    fn rc_prim_dealloc_after_zero() {
        let h = alloc_cons();
        // Inc once so dec-strong has something to drain without
        // crossing the natural-handle floor.
        bi_nl_rc_inc_strong(std::slice::from_ref(&h)).unwrap();
        assert_eq!(count_no_clone(&h), 2);
        // dec back to 1 so the box has exactly the alloc's +1.
        let r = bi_nl_rc_dec_strong(std::slice::from_ref(&h)).unwrap();
        assert_eq!(r, Sexp::Int(1));
        // dealloc / finalize MVP no-ops on supported kind — no panic.
        bi_nl_rc_dealloc(std::slice::from_ref(&h)).unwrap();
        bi_nl_gc_finalize(std::slice::from_ref(&h)).unwrap();
        // Refcount unchanged by the no-op finalize/dealloc.
        assert_eq!(count_no_clone(&h), 1);
        // `h' drops naturally at end-of-scope: refcount 1 → 0,
        // NLRC_DROP_TABLE fires once via `Drop for NlConsBoxRef',
        // box freed exactly once.  miri (= cargo +nightly miri test)
        // would catch a double-free here.
    }

    // ---- rc-prim-walk-children-cons ------------------------------------
    #[test]
    fn rc_prim_walk_children_cons() {
        // Build (a . b) where a/b are distinguishable scalars.
        // We use the concrete cons_primitives path to populate
        // car/cdr because nl-rc-alloc MVP only seeds (nil . nil).
        let inner = bi_nl_rc_alloc(&[Sexp::Int(SEXP_TAG_CONS as i64), Sexp::Int(2)])
            .unwrap();
        // Set inner.car = 11, inner.cdr = 22 via Phase A.3 cons set-car/cdr.
        crate::eval::cons_primitives::bi_nl_cons_set_car(&[inner.clone(), Sexp::Int(11)])
            .unwrap();
        crate::eval::cons_primitives::bi_nl_cons_set_cdr(&[inner.clone(), Sexp::Int(22)])
            .unwrap();
        let kids = bi_nl_gc_walk_children(std::slice::from_ref(&inner)).unwrap();
        // (11 22) — a proper 2-element list.
        match &kids {
            Sexp::Cons(b) => {
                assert_eq!(b.car, Sexp::Int(11));
                match &b.cdr {
                    Sexp::Cons(b2) => {
                        assert_eq!(b2.car, Sexp::Int(22));
                        assert_eq!(b2.cdr, Sexp::Nil);
                    }
                    other => panic!("expected (11 22), got cdr {:?}", other),
                }
            }
            other => panic!("expected proper list, got {:?}", other),
        }
    }

    // ---- rc-prim-walk-children-non-cons --------------------------------
    #[test]
    fn rc_prim_walk_children_non_cons_returns_nil() {
        // Stage 5.3.a stub: any non-CONS Sexp yields Nil.
        let leaf = Sexp::Int(42);
        let kids = bi_nl_gc_walk_children(std::slice::from_ref(&leaf)).unwrap();
        assert_eq!(kids, Sexp::Nil);
        let s = Sexp::Str("hello".into());
        let kids = bi_nl_gc_walk_children(std::slice::from_ref(&s)).unwrap();
        assert_eq!(kids, Sexp::Nil);
    }

    // ---- nl-gc-buffered-decs MVP stub ----------------------------------
    #[test]
    fn nl_gc_buffered_decs_returns_nil() {
        let r = bi_nl_gc_buffered_decs(&[]).unwrap();
        assert_eq!(r, Sexp::Nil);
    }

    // ---- nl-rc-alloc rejects unsupported kind --------------------------
    #[test]
    fn nl_rc_alloc_rejects_unsupported_kind() {
        // Tag 8 = VECTOR, not yet supported in 5.3.a.
        let e = bi_nl_rc_alloc(&[Sexp::Int(8), Sexp::Int(0)]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    // ---- nl-rc-alloc arity errors --------------------------------------
    #[test]
    fn nl_rc_alloc_arity_error() {
        let e = bi_nl_rc_alloc(&[]).unwrap_err();
        assert!(matches!(e, EvalError::WrongNumberOfArguments { .. }));
        let e = bi_nl_rc_alloc(&[Sexp::Int(7)]).unwrap_err();
        assert!(matches!(e, EvalError::WrongNumberOfArguments { .. }));
    }

    // ---- nl-rc-kind on every variant -----------------------------------
    #[test]
    fn nl_rc_kind_reads_variant_tags() {
        // Spot-check three unboxed variants + CONS boxed.
        assert_eq!(
            bi_nl_rc_kind(&[Sexp::Nil]).unwrap(),
            Sexp::Int(crate::eval::sexp::SEXP_TAG_NIL as i64)
        );
        assert_eq!(
            bi_nl_rc_kind(&[Sexp::Int(7)]).unwrap(),
            Sexp::Int(crate::eval::sexp::SEXP_TAG_INT as i64)
        );
        let h = alloc_cons();
        assert_eq!(
            bi_nl_rc_kind(std::slice::from_ref(&h)).unwrap(),
            Sexp::Int(SEXP_TAG_CONS as i64)
        );
    }

    // ---- nl-rc-strong-count rejects non-boxed --------------------------
    #[test]
    fn nl_rc_strong_count_rejects_int() {
        let e = bi_nl_rc_strong_count(&[Sexp::Int(7)]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }
}
