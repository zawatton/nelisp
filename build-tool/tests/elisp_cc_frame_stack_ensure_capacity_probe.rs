//! Doc 111 §111.E #20 probe — `frame_stack_ensure_capacity' Phase 47
//! helper.  The elisp body checks current cap < needed via §111.C
//! `vector-len' + §111.B `record-slot-ref-ptr', then delegates the
//! grow to the `nl_frame_stack_ensure_capacity' Rust shim.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn build_empty_frames_record(initial_capacity: usize) -> Sexp {
    let backing = Sexp::vector(vec![Sexp::Nil; initial_capacity]);
    Sexp::record(
        Sexp::Symbol("nelisp-lexframe-stack".into()),
        vec![backing, Sexp::Int(0)],
    )
}

fn backing_capacity(frames: &Sexp) -> usize {
    if let Sexp::Record(r) = frames {
        if let Sexp::Vector(v) = &r.slots[0] {
            return v.value.len();
        }
    }
    panic!("malformed frames record");
}

#[test]
fn frame_stack_ensure_capacity_no_grow_when_already_sufficient() {
    let frames = build_empty_frames_record(8);
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_ensure_capacity(
            &frames as *const Sexp,
            4,
        )
    };
    // No grow: backing stays at 8.
    assert_eq!(backing_capacity(&frames), 8);
    // Either the unchanged path returns 1 (early elisp gate) or the
    // shim returns current capacity 8 — both are acceptable; we
    // accept any positive value.
    assert!(got > 0, "no-grow path must return a positive value, got {}", got);
}

#[test]
fn frame_stack_ensure_capacity_grows_to_next_pow2() {
    let frames = build_empty_frames_record(8);
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_ensure_capacity(
            &frames as *const Sexp,
            16,
        )
    };
    // Grow doubles to 16.
    assert_eq!(backing_capacity(&frames), 16);
    // Return value reflects the new capacity (= 16) for callers that
    // want to thread it through `if'.
    assert!(got >= 16, "grow path must return new capacity >= needed, got {}", got);
}

#[test]
fn frame_stack_ensure_capacity_grows_multiple_doublings() {
    let frames = build_empty_frames_record(8);
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_ensure_capacity(
            &frames as *const Sexp,
            100,
        );
    }
    // 8 → 16 → 32 → 64 → 128 (= first cap >= 100).
    assert_eq!(backing_capacity(&frames), 128);
}
