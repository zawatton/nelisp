//! Doc 123 §123.D probe — pure-elisp `nelisp_gc_walk_children' kernel.
//!
//! Validates the cycle-collector edge enumerator of
//! `bi_nl_gc_walk_children' (= `Sexp::list_from(&[r.car.clone(),
//! r.cdr.clone()])' at `build-tool/src/eval/rc_primitives.rs:247-262')
//! migrating to elisp via two composed §101.D `cons-make' allocations
//! driven by §122.E `ptr-read-u64' for the box-ptr extraction.
//!
//! The kernel is *Cons-only* in this stage; non-Cons inputs are
//! filtered out by the §123.F sweep stage's Rust shim before
//! reaching the elisp body.  These probes therefore exercise the
//! Cons arm exhaustively, matching the Rust unit test
//! `rc_primitives::tests::rc_prim_walk_children_cons' bit-for-bit.
//!
//! Caller contract:
//!   - `sexp_ptr' must point at a `Sexp::Cons' (= caller pre-checked
//!     tag via §123.C `rc_kind').
//!   - `result_slot' / `tail_slot' must each be writable
//!     32-byte-aligned Sexp slots (= the elisp body raw-copies
//!     into them via `cons-make').
//!
//! Test cases (≥ 3):
//!   1. (11 . 22) — Int car/cdr round-trip; the resulting 2-list
//!      is `(11 22) = (Cons 11 (Cons 22 Nil))'.
//!   2. (Nil . T) — atom-pair with distinct tag bytes; verifies
//!      the raw-copy preserves the discriminant for both kids.
//!   3. ((1 . 2) . (3 . 4)) — nested Cons; verifies the inner
//!      box pointers survive the raw-copy round-trip (= the
//!      MVP byte-copy refcount semantics from the kernel commentary).

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlconsbox::NlConsBoxRef;
use nelisp_build_tool::eval::sexp::Sexp;

/// Materialize a fresh result-slot + tail-slot pair the kernel can
/// write into.  Pre-fills each slot with `Sexp::Nil' so the
/// `cons-make' raw-copies land on initialized memory.
fn fresh_slots() -> (Box<Sexp>, Box<Sexp>) {
    (Box::new(Sexp::Nil), Box::new(Sexp::Nil))
}

/// Run the elisp kernel against an input `Sexp::Cons' and return the
/// materialized 2-list head from `result_slot'.  The boxes survive
/// for the duration of the returned `Sexp' because `Sexp::Cons'
/// owns its `NlConsBoxRef'.
fn call_walk(input: &Sexp) -> (Sexp, Sexp) {
    let (mut result_slot, mut tail_slot) = fresh_slots();
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::gc_walk_children(
            input as *const Sexp,
            &mut *result_slot as *mut Sexp,
            &mut *tail_slot as *mut Sexp,
        )
    };
    assert_eq!(
        returned,
        &mut *result_slot as *mut Sexp,
        "nelisp_gc_walk_children must return its result_slot argument \
         (= matches §101.D `cons-make' caller-ergonomics contract)"
    );
    // Take ownership of the two slots' final Sexp values so the
    // caller can match on them.  We `mem::replace' to leave a Nil
    // in each box (so the Boxes' Drop frees harmless memory).
    let result = std::mem::replace(&mut *result_slot, Sexp::Nil);
    let tail = std::mem::replace(&mut *tail_slot, Sexp::Nil);
    (result, tail)
}

// ---- Case 1: (11 . 22) → (11 22) ----

#[test]
fn gc_walk_children_int_pair_builds_2_list() {
    let cons = NlConsBoxRef::new(Sexp::Int(11), Sexp::Int(22));
    let input = Sexp::Cons(cons);

    let (result, _tail) = call_walk(&input);

    // result = Sexp::Cons(11 . tail) where tail = Sexp::Cons(22 . Nil).
    match &result {
        Sexp::Cons(outer) => {
            assert_eq!(
                outer.car,
                Sexp::Int(11),
                "outer car must copy the input's car (= 11)"
            );
            match &outer.cdr {
                Sexp::Cons(inner) => {
                    assert_eq!(
                        inner.car,
                        Sexp::Int(22),
                        "inner car must copy the input's cdr (= 22)"
                    );
                    assert_eq!(
                        inner.cdr,
                        Sexp::Nil,
                        "inner cdr must be Nil (= 2-element proper list)"
                    );
                }
                other => panic!("inner must be Sexp::Cons, got {:?}", other),
            }
        }
        other => panic!("result must be Sexp::Cons, got {:?}", other),
    }
}

// ---- Case 2: (Nil . T) → (Nil T) ----
//
// Atom-pair with distinct tag bytes — the raw-copy mechanism in
// `cons-make' must preserve the SEXP_TAG_* discriminant for each
// kid independently.

#[test]
fn gc_walk_children_atom_pair_preserves_tags() {
    let cons = NlConsBoxRef::new(Sexp::Nil, Sexp::T);
    let input = Sexp::Cons(cons);

    let (result, _tail) = call_walk(&input);

    match &result {
        Sexp::Cons(outer) => {
            assert_eq!(outer.car, Sexp::Nil, "outer car must be Sexp::Nil");
            match &outer.cdr {
                Sexp::Cons(inner) => {
                    assert_eq!(inner.car, Sexp::T, "inner car must be Sexp::T");
                    assert_eq!(inner.cdr, Sexp::Nil);
                }
                other => panic!("inner must be Sexp::Cons, got {:?}", other),
            }
        }
        other => panic!("result must be Sexp::Cons, got {:?}", other),
    }
}

// ---- Case 3: Nested Cons kids → 2-list of Cons ----
//
// Input: ((1 . 2) . (3 . 4))
// Expected walk: ((1 . 2) (3 . 4)) = Cons(Cons(1,2), Cons(Cons(3,4), Nil)).
//
// The MVP `cons-make' is a raw 32-byte byte-copy (per
// `lisp/nelisp-phase47-compiler.el:3579-3635') — so the nested
// `Sexp::Cons' values are copied verbatim, including their
// `NlConsBoxRef' pointer bytes.  In the cycle-collector usage
// pattern the input outer cons is held alive across the call by the
// calling frame, so the byte-copies observe live inner boxes.
//
// NOTE: this case relies on `std::mem::forget' semantics in the
// `cons-make' lowering — i.e., the raw copies do NOT bump refcounts
// (yet — see commentary in `lisp/nelisp-cc-gc-walk-children.el').
// We compare by `ptr_eq' rather than full Sexp equality to confirm
// the inner boxes are *shared* between the input and the output's
// kids (= zero-copy walk for Cons children).

#[test]
fn gc_walk_children_nested_cons_shares_inner_boxes() {
    let inner_a = NlConsBoxRef::new(Sexp::Int(1), Sexp::Int(2));
    let inner_b = NlConsBoxRef::new(Sexp::Int(3), Sexp::Int(4));
    // Capture the original inner box pointers BEFORE moving into
    // the outer cons (= bit-for-bit comparison after the walk).
    let inner_a_ptr = NlConsBoxRef::as_ptr(&inner_a) as usize;
    let inner_b_ptr = NlConsBoxRef::as_ptr(&inner_b) as usize;

    let outer = NlConsBoxRef::new(Sexp::Cons(inner_a), Sexp::Cons(inner_b));
    let input = Sexp::Cons(outer);

    let (result, tail) = call_walk(&input);

    // result = Cons(Cons(1,2), tail).
    let walked_a_ptr = match &result {
        Sexp::Cons(o) => match &o.car {
            Sexp::Cons(b) => NlConsBoxRef::as_ptr(b) as usize,
            other => panic!("outer car must be Sexp::Cons, got {:?}", other),
        },
        other => panic!("result must be Sexp::Cons, got {:?}", other),
    };
    assert_eq!(
        walked_a_ptr, inner_a_ptr,
        "MVP cons-make raw-copies the 32-byte Sexp::Cons payload \
         verbatim — the NlConsBoxRef inner pointer must match"
    );

    // tail = Cons(Cons(3,4), Nil); we extract it via the call's
    // tail_slot which materialized the inner `(cdr . nil)' build.
    let walked_b_ptr = match &tail {
        Sexp::Cons(t) => match &t.car {
            Sexp::Cons(b) => NlConsBoxRef::as_ptr(b) as usize,
            other => panic!("tail car must be Sexp::Cons, got {:?}", other),
        },
        other => panic!("tail must be Sexp::Cons, got {:?}", other),
    };
    assert_eq!(
        walked_b_ptr, inner_b_ptr,
        "Second kid's box pointer must also survive the raw-copy"
    );

    // Defuse: the raw-copy meant the `inner_a' / `inner_b' boxes are
    // now referenced by both the input's outer NlConsBox AND the
    // walked result.  Forget the walked result so its Drop does not
    // call rc_dec_raw and free a box still owned by `input'.  (Same
    // pattern `nl_jit_cons_make' uses with `std::mem::forget' on the
    // copied car/cdr payloads.)
    std::mem::forget(result);
    std::mem::forget(tail);
}
