//! Doc 79 / Doc 123 — `NlRc' primitive surface for `lisp/nelisp-stdlib-gc.el'.
//! 7 live primitives: dealloc / dec-strong / strong-count / kind / payload-ptr /
//! gc-walk-children / gc-finalize (Cons-kind MVP).

use crate::eval::builtins::require_arity;
use crate::eval::error::EvalError;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::sexp::Sexp;

/// `(nl-rc-dealloc HANDLE) -> nil`.  Route through `nl-gc-finalize'.
pub fn bi_nl_rc_dealloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-dealloc", args, 1, Some(1))?;
    bi_nl_gc_finalize(args)
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

    // ---- rc-prim-walk-children-non-cons --------------------------------
    #[test]
    fn rc_prim_walk_children_non_cons_returns_nil() {
        let leaf = Sexp::Int(42);
        assert_eq!(bi_nl_gc_walk_children(std::slice::from_ref(&leaf)).unwrap(), Sexp::Nil);
        let s = Sexp::Str("hello".into());
        assert_eq!(bi_nl_gc_walk_children(std::slice::from_ref(&s)).unwrap(), Sexp::Nil);
    }

    #[test]
    fn nl_rc_kind_reads_unboxed_variant_tags() {
        assert_eq!(
            bi_nl_rc_kind(&[Sexp::Nil]).unwrap(),
            Sexp::Int(crate::eval::sexp::SEXP_TAG_NIL as i64)
        );
        assert_eq!(
            bi_nl_rc_kind(&[Sexp::Int(7)]).unwrap(),
            Sexp::Int(crate::eval::sexp::SEXP_TAG_INT as i64)
        );
    }

    #[test]
    fn nl_rc_strong_count_rejects_int() {
        let e = bi_nl_rc_strong_count(&[Sexp::Int(7)]).unwrap_err();
        assert!(matches!(e, EvalError::WrongType { .. }));
    }
}
