#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlconsbox::NlConsBoxRef;
use nelisp_build_tool::eval::sexp::Sexp;

fn sexp_as_ptr(s: &Sexp) -> *const u8 {
    s as *const Sexp as *const u8
}

// ---- Case 1: Sexp::Cons reads back NlConsBoxRef::as_ptr ----

#[test]
fn rc_payload_ptr_cons_returns_box_pointer() {
    // Build a cons via the public NlConsBoxRef constructor (= same
    // path the evaluator uses).
    let cons = NlConsBoxRef::new(Sexp::Int(11), Sexp::Int(22));
    // Capture the canonical box pointer BEFORE moving into Sexp::Cons,
    // so we can compare bit-for-bit with the kernel's load result.
    let expected_box_ptr = NlConsBoxRef::as_ptr(&cons) as usize as i64;
    let s = Sexp::Cons(cons);

    let loaded = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&s)) };
    assert_eq!(
        loaded, expected_box_ptr,
        "nelisp_rc_payload_ptr must return NlConsBoxRef::as_ptr \
         (= SEXP_PAYLOAD_OFFSET = 8 layout-share check)"
    );
    // Sanity: the box pointer is non-null (alloc::alloc returns null
    // only on failure, which would have panicked in `NlConsBoxRef::new').
    assert_ne!(loaded, 0, "box pointer must be non-null for live Cons");
}

// ---- Case 2: Two clones share the same box pointer ----
//
// `NlConsBoxRef::clone' increments the refcount and copies the
// `NonNull<NlConsBox>' inner — both handles point at the same
// physical box.  Wrapping both in `Sexp::Cons' and reading the
// payload pointer must return identical i64 values, confirming the
// kernel reads the underlying box-pointer field and not some
// stack-local Rust state.

#[test]
fn rc_payload_ptr_clone_shares_box_pointer() {
    let cons_a = NlConsBoxRef::new(Sexp::Int(7), Sexp::Int(8));
    let cons_b = cons_a.clone();
    // Confirm `ptr_eq` so the two handles point at the same box.
    assert!(
        NlConsBoxRef::ptr_eq(&cons_a, &cons_b),
        "Clone must share the underlying box (= refcount semantics)"
    );
    let sa = Sexp::Cons(cons_a);
    let sb = Sexp::Cons(cons_b);
    let pa = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&sa)) };
    let pb = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&sb)) };
    assert_eq!(
        pa, pb,
        "Two Sexp::Cons handles sharing the same NlConsBox must \
         yield the same payload pointer via nelisp_rc_payload_ptr"
    );
    assert_ne!(pa, 0);
}

// ---- Case 3: Sexp::Nil — atom variant, no panic / segfault ----

#[test]
fn rc_payload_ptr_nil_is_well_defined() {
    let s = Sexp::Nil;
    // The read is well-defined because `&Sexp::Nil' is a valid
    // 32-byte stack value; offset 8 lies inside the payload-union
    // bytes (= zero-initialized for Nil / T variants).  The result
    // value is meaningless — caller must tag-check via §123.C
    // `rc_kind' before using it as a pointer.
    let loaded = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&s)) };
    // No assertion on the value itself — atom-variant payload bytes
    // are unspecified.  This case proves the kernel does not crash
    // / mis-align / read out-of-bounds when given a non-boxed Sexp.
    let _ = loaded;
}

// ---- Case 4: Round-trip stability ----
//
// Re-reading the same Sexp twice must return the same i64; the
// kernel is a pure load with no internal state.

#[test]
fn rc_payload_ptr_repeated_read_stable() {
    let cons = NlConsBoxRef::new(Sexp::T, Sexp::Nil);
    let s = Sexp::Cons(cons);
    let a = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&s)) };
    let b = unsafe { nelisp_build_tool::elisp_cc_spike::rc_payload_ptr(sexp_as_ptr(&s)) };
    assert_eq!(
        a, b,
        "nelisp_rc_payload_ptr is a pure load — two reads of the same \
         Sexp must return identical i64 values"
    );
}
