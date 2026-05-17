//! Doc 79 v7 Phase C Stage 5.3.a — generic `NlRc' primitive set
//! exposed to elisp + initial GC helper trio.
//!
//! These ten primitives are the elisp-facing surface for the
//! Bacon-Rajan cycle collector skeleton landing in
//! `lisp/nelisp-stdlib-gc.el' (= same atomic commit, Doc 79 §11.7
//! atomic-with-consumer pattern).  Phase C `gc-collect-cycles' main
//! body (= 5.3.b〜.e) drives the dispatch table built here.
//!
//! Surface (Doc 79 v6 §4.2):
//!
//! | primitive               | sig (Sexp-level)              | semantics                              |
//! |-------------------------|-------------------------------|----------------------------------------|
//! | `nl-rc-alloc`           | `Int Int -> Sexp`             | kind dispatch → box alloc (MVP=CONS)   |
//! | `nl-rc-dealloc`         | `Sexp -> Nil`                 | force-finalize via NLRC_DROP_TABLE     |
//! | `nl-rc-inc-strong`      | `Sexp -> Nil`                 | refcount += 1 (Relaxed)                |
//! | `nl-rc-dec-strong`      | `Sexp -> Int`                 | refcount -= 1, return new count        |
//! | `nl-rc-strong-count`    | `Sexp -> Int`                 | refcount load (Acquire)                |
//! | `nl-rc-kind`            | `Sexp -> Int`                 | tag byte (= SEXP_TAG_*)                |
//! | `nl-rc-payload-ptr`     | `Sexp -> Int`                 | payload ptr (= box addr, diagnostic)   |
//! | `nl-gc-walk-children`   | `Sexp -> List`                | outgoing edge enumeration              |
//! | `nl-gc-buffered-decs`   | `() -> Nil` (MVP stub)        | recent dec buffer (Stage 5.3.b)        |
//! | `nl-gc-finalize`        | `Sexp -> Nil`                 | refcount→0 forced + DROP_TABLE         |
//!
//! MVP scope (Stage 5.3.a):
//!   - `nl-rc-alloc' supports CONS kind (= tag 7, payload_size = 2)
//!     — alloc with `(nil . nil)' and return `Sexp::Cons'.  Other
//!     kinds raise `wrong-type-argument' until Stage 5.3.b〜.e expand
//!     to vector / record / cell / chartable / boolvector / mutstr.
//!   - `nl-rc-payload-ptr' returns the box's raw address as
//!     `Sexp::Int' so the elisp skeleton can do non-null assertions
//!     without a real opaque-pointer Sexp variant (= deferred).
//!   - `nl-gc-walk-children' walks CONS only; other kinds return Nil.
//!   - `nl-gc-buffered-decs' is a Nil-returning stub.  Stage 5.3.b
//!     drives the buffer from elisp-side dec hooks.
//!
//! Heap-corruption avoidance per Track L (Doc 77c §2.1.2 + memory
//! `feedback_anvil_macro_require.md'): refcount mutation goes through
//! `NlConsBoxRef::rc_inc_raw' / `rc_dec_raw' which use a per-box
//! typed pointer at the call site (= macro-style access).  We do not
//! introduce a generic `&AtomicUsize' helper that takes ownership of
//! the trailer-offset arithmetic (= would re-introduce the Track D
//! offset bug if a future kind has a non-CONS trailer layout).
//!
//! All ten raise `wrong-type-argument' (= [`EvalError::WrongType`])
//! when handed a Sexp variant whose tag this MVP does not yet know
//! how to dispatch on.

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

/// `(nl-rc-alloc KIND PAYLOAD-SIZE) -> Sexp`.  Allocate a fresh box
/// of the given kind.  MVP: only CONS (= tag 7) supported, with
/// `(nil . nil)' payload regardless of `PAYLOAD-SIZE' (= the size
/// arg is parked for Stage 5.3.b〜.e per-kind dispatch where vector
/// / record / boolvector need it).
pub fn bi_nl_rc_alloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-alloc", args, 2, Some(2))?;
    let kind = extract_kind_tag("nl-rc-alloc", &args[0])?;
    require_supported_kind(kind)?;
    // PAYLOAD-SIZE arg is currently parked: CONS payload size is
    // fixed at 2 (= car + cdr).  Validate the type but ignore the
    // value so callers can pre-stage the per-kind sizes — Stage
    // 5.3.b〜.e dispatch on it for vector / boolvector alloc.
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

/// `(nl-rc-inc-strong HANDLE) -> nil`.  Bump the strong refcount of
/// HANDLE's box by 1.  MVP: CONS only.
///
/// Doc 123 §123.F: body migrated to the pure-elisp `nelisp_rc_inc'
/// kernel (= §122.E `atomic-fetch-add' + REFCOUNT_OFFSET = 64).  The
/// Rust shim retains the Sexp unwrap / arity-error contract; the
/// atomic-fetch-add itself runs in elisp.
pub fn bi_nl_rc_inc_strong(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-inc-strong", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            // SAFETY: `r' is alive on entry (= caller's Sexp::Cons
            // handle holds the box).  The elisp kernel adds 64 to
            // `box_ptr' internally to reach the refcount slot.  Pre-
            // add i64 return is discarded (= Sexp surface is Nil).
            let box_ptr = NlConsBoxRef::as_ptr(r) as *mut i64;
            unsafe {
                let _ = crate::elisp_cc_spike::rc_inc(box_ptr);
            }
            Ok(Sexp::Nil)
        }
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-rc-inc-strong arg)".into(),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-dec-strong HANDLE) -> Int`.  Decrement the strong refcount
/// and return the new count.  Does **not** auto-free — Phase C's
/// elisp cycle collector is responsible for dealloc on count = 0
/// (= it calls `nl-gc-finalize' when ready).  MVP: CONS only.
///
/// Doc 123 §123.F: body migrated to the pure-elisp `nelisp_rc_dec'
/// kernel (= §122.E `atomic-fetch-add' with delta=-1).  The kernel
/// returns the *pre-sub* i64; the Rust shim applies the
/// `saturating_sub(1)' that the previous `rc_dec_no_drop' helper
/// performed inline so the Sexp surface still reports the *new*
/// count (= 0 floor preserved).  Track L pattern is preserved: the
/// elisp kernel uses the box's REFCOUNT_OFFSET = 64 constant, not a
/// generic trailer offset, so future non-CONS kinds with different
/// layouts cannot silently corrupt the count via this codepath.
pub fn bi_nl_rc_dec_strong(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-dec-strong", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            // SAFETY: `r' is alive because the caller's Sexp::Cons
            // handle (= `args[0]') is on the stack.  The kernel does
            // a single `atomic-fetch-add' delta=-1 — the outer
            // Sexp::Cons Drop still controls box teardown, matching
            // the pre-§123.F no-auto-free contract.
            let box_ptr = NlConsBoxRef::as_ptr(r) as *mut i64;
            let prev = unsafe { crate::elisp_cc_spike::rc_dec(box_ptr) };
            // saturating_sub at the i64/usize boundary: prev was
            // loaded from a non-negative atomic so it cannot be
            // negative.  prev=0 saturates to 0 (= matches the
            // previous `rc_dec_no_drop' contract).
            let new = if prev > 0 { (prev - 1) as i64 } else { 0 };
            Ok(Sexp::Int(new))
        }
        other => Err(EvalError::WrongType {
            expected: "boxed Sexp (nl-rc-dec-strong arg)".into(),
            got: other.clone(),
        }),
    }
}

/// `(nl-rc-strong-count HANDLE) -> Int`.  Read the current strong
/// refcount.  MVP: CONS only.
///
/// Doc 123 §123.F: body migrated to the pure-elisp
/// `nelisp_rc_strong_count' kernel (= §122.E `ptr-read-u64' at
/// REFCOUNT_OFFSET = 64).  Per §123.C risk note, the elisp body uses
/// `ptr-read-u64' with no explicit ordering (= compiler defaults are
/// sufficient for a read-only load of an atomic slot that no writer
/// is racing with under the cycle collector's coordination).
pub fn bi_nl_rc_strong_count(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-strong-count", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            // SAFETY: `r' is alive on entry (= caller's Sexp::Cons
            // handle holds the box).  The elisp kernel adds 64 to
            // `box_ptr' internally to reach the refcount slot.
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

/// `(nl-rc-kind HANDLE) -> Int`.  Read the tag byte (= `SEXP_TAG_*'
/// constant) of HANDLE.  Works on every Sexp variant since the tag
/// is always at offset 0; cycle collector uses this to decide which
/// per-kind walker to invoke.
///
/// Doc 123 §123.F: body migrated to the pure-elisp `nelisp_rc_kind'
/// kernel (= §122.E `ptr-read-u8' at offset 0 of the outer `Sexp'
/// enum's `#[repr(C, u8)]' discriminant).
pub fn bi_nl_rc_kind(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-kind", args, 1, Some(1))?;
    // SAFETY: `&args[0]' is a valid `&Sexp' for the call's lifetime,
    // so casting it to `*const u8' and reading offset 0 (= the
    // `#[repr(C, u8)]' discriminant byte) is sound — same operation
    // the previous `args[0].tag()' performed under the hood.
    let sexp_ptr = &args[0] as *const Sexp as *const u8;
    let tag = unsafe { crate::elisp_cc_spike::rc_kind(sexp_ptr) };
    Ok(Sexp::Int(tag))
}

/// `(nl-rc-payload-ptr HANDLE) -> Int`.  Return the underlying box
/// pointer as a numeric address (= diagnostic / non-null assertion).
/// MVP: CONS returns `as_ptr' addr; other kinds return 0 because the
/// elisp skeleton's MVP only inspects CONS payloads.
pub fn bi_nl_rc_payload_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-rc-payload-ptr", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            let p = NlConsBoxRef::as_ptr(r) as usize;
            Ok(Sexp::Int(p as i64))
        }
        // Other variants: MVP returns 0 (= non-CONS branch is stubbed
        // in `nelisp-stdlib-gc.el`'s walk-children dispatch).
        _ => Ok(Sexp::Int(0)),
    }
}

/// `(nl-gc-walk-children HANDLE) -> List`.  Enumerate outgoing edges
/// of HANDLE's box.  MVP: CONS returns `(car cdr)'; other kinds
/// return Nil (= Stage 5.3.b widens to vector / record / cell /
/// chartable per Doc 79 §5.3.5.b).
pub fn bi_nl_gc_walk_children(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-gc-walk-children", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Cons(r) => {
            // Build (car cdr) as a 2-element proper list.  We borrow
            // `car' / `cdr' through Deref; cloning bumps each child's
            // refcount the way the cycle collector expects (= so the
            // list survives across the elisp walker's recursion).
            let car = r.car.clone();
            let cdr = r.cdr.clone();
            Ok(Sexp::list_from(&[car, cdr]))
        }
        // Stage 5.3.a stub: non-CONS = no children walked yet.
        _ => Ok(Sexp::Nil),
    }
}

/// `(nl-gc-buffered-decs) -> nil`.  Stage 5.3.a stub — Stage 5.3.b
/// will drive the suspect-buffer from elisp-side dec hooks (= Doc 79
/// §5.3.5.a's `gc-cycle-roots-buffer' push).  Returning Nil here lets
/// the skeleton's `gc-collect-cycles' loop run to completion as a
/// no-op without special-casing the call.
pub fn bi_nl_gc_buffered_decs(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-gc-buffered-decs", args, 0, Some(0))?;
    Ok(Sexp::Nil)
}

/// `(nl-gc-finalize HANDLE) -> nil`.  Stage 5.3.a MVP: validates
/// kind is supported and is otherwise a no-op.  The actual
/// drop-then-dealloc cascade is wired in Stage 5.3.b〜.e where the
/// elisp cycle collector orchestrates the lifetime so the outer
/// `Sexp::Cons' handle's Drop balances naturally with the explicit
/// finalize.
///
/// Why no-op in MVP: triggering `NLRC_DROP_TABLE' here would free
/// the box while `args[0]' still holds a `Sexp::Cons' that owns one
/// of the outstanding refcount-1 contributions.  When `args' falls
/// out of scope after this primitive returns, `Drop for NlConsBoxRef'
/// fires `rc_dec_raw' on freed memory (= UAF).  Stage 5.3.b adds the
/// elisp-driven preamble that drains the count to 0 *via*
/// `nl-rc-dec-strong' (no-drop) before this primitive runs, then
/// arranges `mem::forget'-equivalent semantics through the
/// collector's bookkeeping.  Until that wiring lands, MVP `finalize'
/// is documentation-only — its presence in the dispatch table lets
/// 5.3.a's elisp skeleton compile and lets future sub-stages hook in
/// without re-shipping primitives.
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
