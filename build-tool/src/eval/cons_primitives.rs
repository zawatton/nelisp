//! Doc 77c Phase A.3 — Layer 2 elisp primitives that operate directly
//! on [`NlConsBox`] / [`NlConsBoxRef`] (= the layout-pinned cons cell
//! shipped in Phase A.2.0 / A.2.1).
//!
//! These eight primitives are the elisp-facing surface for manual
//! cons-cell manipulation that bypasses the public `cons' / `car' /
//! `cdr' / `setcar' / `setcdr' wrappers.  Phase B onward the elisp
//! image's stdlib will re-implement the public names on top of these
//! `nl-cons-*' / `nl-rc-*' building blocks (= Doc 77c §2 self-host
//! commitment).
//!
//! Surface (Doc 77c §2.1.3):
//!
//! | primitive            | signature       | semantics                     |
//! |---------------------|-----------------|--------------------------------|
//! | `nl-cons-alloc`     | `Sexp Sexp -> Sexp`  | malloc + `Sexp::Cons' wrap |
//! | `nl-cons-car`       | `Sexp -> Sexp`       | read `car' field             |
//! | `nl-cons-cdr`       | `Sexp -> Sexp`       | read `cdr' field             |
//! | `nl-cons-set-car`   | `Sexp Sexp -> Sexp`  | write `car', return new val  |
//! | `nl-cons-set-cdr`   | `Sexp Sexp -> Sexp`  | write `cdr', return new val  |
//! | `nl-rc-inc`         | `Sexp -> Nil`        | refcount += 1                 |
//! | `nl-rc-dec`         | `Sexp -> Nil`        | refcount -= 1, free if 0     |
//! | `nl-rc-count`       | `Sexp -> Int`        | read current refcount         |
//!
//! All eight raise `wrong-type-argument' (= [`EvalError::WrongType`])
//! if the cons-typed arg is anything other than `Sexp::Cons(_)'.  Arity
//! mismatches raise the same `wrong-number-of-arguments' shape used by
//! every other Rust primitive (via [`require_arity`]).

use crate::eval::builtins::require_arity;
use crate::eval::error::EvalError;
use crate::eval::nlconsbox::NlConsBoxRef;
use crate::eval::sexp::Sexp;

/// Extract the [`NlConsBoxRef`] from a `Sexp::Cons' arg, or raise
/// `wrong-type-argument' citing PRIM as the originating primitive.
fn require_cons<'a>(prim: &str, arg: &'a Sexp) -> Result<&'a NlConsBoxRef, EvalError> {
    match arg {
        Sexp::Cons(r) => Ok(r),
        other => Err(EvalError::WrongType {
            expected: format!("cons ({} arg)", prim),
            got: other.clone(),
        }),
    }
}

/// `(nl-cons-alloc CAR CDR) -> Sexp::Cons'.  Heap-allocate a fresh
/// [`NlConsBox`](crate::eval::nlconsbox::NlConsBox) with `refcount = 1'
/// and return it wrapped as `Sexp::Cons'.  Equivalent to the public
/// `cons' builtin but kept under a `nl-' name so Phase B elisp can
/// rewrite the public `cons' wrapper without recursing into itself.
pub fn bi_nl_cons_alloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-cons-alloc", args, 2, Some(2))?;
    Ok(Sexp::Cons(NlConsBoxRef::new(
        args[0].clone(),
        args[1].clone(),
    )))
}

/// `(nl-cons-car CONS) -> Sexp'.  Read the `car' field of the box.
/// Returns a structural clone (= refcount +1 if `car' is itself a
/// `Sexp::Cons').
pub fn bi_nl_cons_car(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-cons-car", args, 1, Some(1))?;
    let r = require_cons("nl-cons-car", &args[0])?;
    Ok(r.car.clone())
}

/// `(nl-cons-cdr CONS) -> Sexp'.  Read the `cdr' field of the box.
pub fn bi_nl_cons_cdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-cons-cdr", args, 1, Some(1))?;
    let r = require_cons("nl-cons-cdr", &args[0])?;
    Ok(r.cdr.clone())
}

/// `(nl-cons-set-car CONS VAL) -> VAL'.  Mutate `car' in place; the
/// previous `car' value is dropped.  Returns the freshly-installed
/// VAL so the elisp wrapper can chain.
pub fn bi_nl_cons_set_car(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-cons-set-car", args, 2, Some(2))?;
    let r = require_cons("nl-cons-set-car", &args[0])?;
    let val = args[1].clone();
    // SAFETY: at primitive-dispatch time `args' is the only borrow
    // path into the box's `car' field — Layer 2 elisp wrappers do
    // not retain `&Sexp' aliases across primitive calls (= same
    // discipline `setcar' relies on, Phase A.2.1).
    unsafe { r.set_car(val.clone()) };
    Ok(val)
}

/// `(nl-cons-set-cdr CONS VAL) -> VAL'.  Mutate `cdr' in place; the
/// previous `cdr' value is dropped.  See [`bi_nl_cons_set_car`] for
/// the safety contract.
pub fn bi_nl_cons_set_cdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-cons-set-cdr", args, 2, Some(2))?;
    let r = require_cons("nl-cons-set-cdr", &args[0])?;
    let val = args[1].clone();
    // SAFETY: see `bi_nl_cons_set_car'.
    unsafe { r.set_cdr(val.clone()) };
    Ok(val)
}

/// `(nl-rc-inc CONS) -> nil'.  Bump the refcount of CONS's underlying
/// box by 1.  Layer 2 elisp uses this to keep a box alive across
/// scopes Rust ownership cannot see (= raw pointer stash, table
/// entry by handle, etc.).  Must be balanced by exactly one
/// [`bi_nl_rc_dec`] call later.
pub fn bi_nl_rc_inc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-inc", args, 1, Some(1))?;
    let r = require_cons("nl-rc-inc", &args[0])?;
    // SAFETY: caller (= elisp Layer 2) takes responsibility for
    // matching `nl-rc-dec'.  See `NlConsBoxRef::rc_inc_raw' contract.
    unsafe { NlConsBoxRef::rc_inc_raw(r) };
    Ok(Sexp::Nil)
}

/// `(nl-rc-dec CONS) -> nil'.  Decrement the refcount of CONS's
/// underlying box by 1.  Frees the box when the count reaches zero.
/// Must be paired with a prior [`bi_nl_rc_inc`] (or be the final
/// balancing decrement of a normal-lifecycle handle).
pub fn bi_nl_rc_dec(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-dec", args, 1, Some(1))?;
    let r = require_cons("nl-rc-dec", &args[0])?;
    // SAFETY: caller (= elisp Layer 2) guarantees the matching
    // `nl-rc-inc' invariant + lifetime of any other handle to this
    // box.  See `NlConsBoxRef::rc_dec_raw' contract.
    unsafe { NlConsBoxRef::rc_dec_raw(r) };
    Ok(Sexp::Nil)
}

/// `(nl-rc-count CONS) -> Int'.  Read the current strong-reference
/// count of CONS's underlying box.  `Acquire' ordering, so callers
/// observing the value are also synchronized with all writes
/// published by earlier `Release' decrements.
pub fn bi_nl_rc_count(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-count", args, 1, Some(1))?;
    let r = require_cons("nl-rc-count", &args[0])?;
    Ok(Sexp::Int(NlConsBoxRef::strong_count(r) as i64))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cons(a: Sexp, b: Sexp) -> Sexp {
        bi_nl_cons_alloc(&[a, b]).expect("alloc OK")
    }

    #[test]
    fn alloc_returns_cons_with_payload() {
        let c = cons(Sexp::Int(7), Sexp::Int(8));
        assert_eq!(
            bi_nl_cons_car(&[c.clone()]).unwrap(),
            Sexp::Int(7)
        );
        assert_eq!(
            bi_nl_cons_cdr(&[c]).unwrap(),
            Sexp::Int(8)
        );
    }

    #[test]
    fn alloc_starts_at_refcount_one() {
        let c = cons(Sexp::Nil, Sexp::Nil);
        assert_eq!(count_no_clone(&c), 1);
    }

    #[test]
    fn set_car_mutates_in_place_and_returns_new_val() {
        let c = cons(Sexp::Int(1), Sexp::Int(2));
        let r = bi_nl_cons_set_car(&[c.clone(), Sexp::Int(99)]).unwrap();
        assert_eq!(r, Sexp::Int(99));
        assert_eq!(bi_nl_cons_car(&[c]).unwrap(), Sexp::Int(99));
    }

    #[test]
    fn set_cdr_mutates_in_place_and_returns_new_val() {
        let c = cons(Sexp::Int(1), Sexp::Int(2));
        let r = bi_nl_cons_set_cdr(&[c.clone(), Sexp::Int(42)]).unwrap();
        assert_eq!(r, Sexp::Int(42));
        assert_eq!(bi_nl_cons_cdr(&[c]).unwrap(), Sexp::Int(42));
    }

    /// Read refcount without inflating it.  `bi_nl_rc_count(&[c.clone()])'
    /// would clone `c' on the way into the args slice, bumping refcount
    /// during the call and obscuring the underlying state by exactly +1.
    /// `slice::from_ref' borrows `c' as a 1-element slice instead, so
    /// no clone is materialized.
    fn count_no_clone(c: &Sexp) -> i64 {
        match bi_nl_rc_count(std::slice::from_ref(c)).unwrap() {
            Sexp::Int(n) => n,
            other => panic!("nl-rc-count returned non-int {:?}", other),
        }
    }

    #[test]
    fn set_car_drops_previous_value() {
        // Use a nested cons as the previous car so we can observe its
        // refcount drop after `set-car' replaces the slot with Nil.
        let inner = cons(Sexp::Int(1), Sexp::Int(2));
        let outer = cons(inner.clone(), Sexp::Nil);
        // outer.car holds inner (refcount 2: outer.car + `inner' binding).
        assert_eq!(count_no_clone(&inner), 2);
        // Replace outer.car with Nil — inner's refcount drops to 1.
        bi_nl_cons_set_car(&[outer, Sexp::Nil]).unwrap();
        assert_eq!(count_no_clone(&inner), 1);
    }

    #[test]
    fn rc_inc_dec_round_trips() {
        let c = cons(Sexp::Int(1), Sexp::Int(2));
        assert_eq!(count_no_clone(&c), 1);
        bi_nl_rc_inc(std::slice::from_ref(&c)).unwrap();
        assert_eq!(count_no_clone(&c), 2);
        bi_nl_rc_inc(std::slice::from_ref(&c)).unwrap();
        assert_eq!(count_no_clone(&c), 3);
        bi_nl_rc_dec(std::slice::from_ref(&c)).unwrap();
        assert_eq!(count_no_clone(&c), 2);
        bi_nl_rc_dec(std::slice::from_ref(&c)).unwrap();
        assert_eq!(count_no_clone(&c), 1);
    }

    #[test]
    fn rc_inc_returns_nil() {
        let c = cons(Sexp::Nil, Sexp::Nil);
        assert_eq!(bi_nl_rc_inc(&[c.clone()]).unwrap(), Sexp::Nil);
        // Balance the inc so the test doesn't leak.
        bi_nl_rc_dec(&[c]).unwrap();
    }

    #[test]
    fn rc_dec_returns_nil() {
        let c = cons(Sexp::Nil, Sexp::Nil);
        bi_nl_rc_inc(&[c.clone()]).unwrap();
        assert_eq!(bi_nl_rc_dec(&[c]).unwrap(), Sexp::Nil);
    }

    #[test]
    fn alloc_arity_error_zero_args() {
        let e = bi_nl_cons_alloc(&[]).unwrap_err();
        match e {
            EvalError::WrongNumberOfArguments { .. } => {}
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn alloc_arity_error_three_args() {
        let e = bi_nl_cons_alloc(&[Sexp::Nil, Sexp::Nil, Sexp::Nil]).unwrap_err();
        match e {
            EvalError::WrongNumberOfArguments { .. } => {}
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn car_wrong_type_for_int_arg() {
        let e = bi_nl_cons_car(&[Sexp::Int(7)]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn cdr_wrong_type_for_nil_arg() {
        let e = bi_nl_cons_cdr(&[Sexp::Nil]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn set_car_wrong_type_for_str_arg() {
        let e = bi_nl_cons_set_car(&[Sexp::Str("not-cons".into()), Sexp::Nil]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn rc_count_wrong_type_for_symbol_arg() {
        let e = bi_nl_rc_count(&[Sexp::Symbol("foo".into())]).unwrap_err();
        match e {
            EvalError::WrongType { .. } => {}
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn nested_cons_payload_round_trip() {
        // (1 . (2 . (3 . nil)))  built bottom-up.
        let l3 = cons(Sexp::Int(3), Sexp::Nil);
        let l2 = cons(Sexp::Int(2), l3);
        let l1 = cons(Sexp::Int(1), l2);
        assert_eq!(bi_nl_cons_car(&[l1.clone()]).unwrap(), Sexp::Int(1));
        let tail = bi_nl_cons_cdr(&[l1]).unwrap();
        assert_eq!(bi_nl_cons_car(&[tail.clone()]).unwrap(), Sexp::Int(2));
        let tail2 = bi_nl_cons_cdr(&[tail]).unwrap();
        assert_eq!(bi_nl_cons_car(&[tail2.clone()]).unwrap(), Sexp::Int(3));
        assert_eq!(bi_nl_cons_cdr(&[tail2]).unwrap(), Sexp::Nil);
    }
}
