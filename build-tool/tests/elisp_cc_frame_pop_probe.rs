//! Doc 111 §111.E #22 probe — `frame_pop_rust_direct' Phase 47 helper.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn build_empty_frames_record(initial_capacity: usize) -> Sexp {
    let backing = Sexp::vector(vec![Sexp::Nil; initial_capacity]);
    Sexp::record(
        Sexp::Symbol("nelisp-lexframe-stack".into()),
        vec![backing, Sexp::Int(0)],
    )
}

fn current_depth(frames: &Sexp) -> i64 {
    if let Sexp::Record(r) = frames {
        if let Sexp::Int(n) = &r.slots[1] {
            return *n;
        }
    }
    panic!("malformed frames record");
}

#[test]
fn frame_pop_empty_stack_is_no_op() {
    let frames = build_empty_frames_record(8);
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_pop(&frames as *const Sexp)
    };
    assert_eq!(rc, 0, "pop on empty stack returns 0");
    assert_eq!(current_depth(&frames), 0);
}

#[test]
fn frame_pop_after_push_returns_to_empty() {
    let frames = build_empty_frames_record(8);
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
    }
    assert_eq!(current_depth(&frames), 1);
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_pop(&frames as *const Sexp)
    };
    assert_eq!(rc, 1, "pop on non-empty stack returns 1");
    assert_eq!(current_depth(&frames), 0);
    // Backing[0] is now Nil again (= the pushed lexframe was dropped).
    if let Sexp::Record(r) = &frames {
        if let Sexp::Vector(v) = &r.slots[0] {
            assert_eq!(v.value[0], Sexp::Nil);
        }
    }
}

#[test]
fn frame_pop_nested_stack_walks_down() {
    let frames = build_empty_frames_record(8);
    for _ in 0..3 {
        unsafe { nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp); }
    }
    assert_eq!(current_depth(&frames), 3);
    for expected_after_pop in [2, 1, 0] {
        let rc = unsafe {
            nelisp_build_tool::elisp_cc_spike::frame_pop(&frames as *const Sexp)
        };
        assert_eq!(rc, 1);
        assert_eq!(current_depth(&frames), expected_after_pop);
    }
    // One more pop is a no-op.
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_pop(&frames as *const Sexp)
    };
    assert_eq!(rc, 0);
}
