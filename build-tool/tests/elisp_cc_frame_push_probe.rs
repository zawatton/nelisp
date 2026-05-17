//! Doc 111 §111.E #21 probe — `frame_push_rust_direct' Phase 47 helper.

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

fn backing_slot(frames: &Sexp, idx: usize) -> Sexp {
    if let Sexp::Record(r) = frames {
        if let Sexp::Vector(v) = &r.slots[0] {
            return v.value[idx].clone();
        }
    }
    panic!("malformed frames record");
}

#[test]
fn frame_push_increments_depth() {
    let frames = build_empty_frames_record(8);
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp)
    };
    assert_eq!(rc, 1);
    assert_eq!(current_depth(&frames), 1);
}

#[test]
fn frame_push_installs_lexframe_record() {
    let frames = build_empty_frames_record(8);
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
    }
    let pushed = backing_slot(&frames, 0);
    match &pushed {
        Sexp::Record(r) => {
            assert!(
                matches!(&r.type_tag, Sexp::Symbol(s) if s == "nelisp-lexframe"),
                "pushed frame type-tag must be `nelisp-lexframe', got {:?}",
                r.type_tag,
            );
        }
        other => panic!("backing[0] must be Record, got {:?}", other),
    }
}

#[test]
fn frame_push_multiple_grows_backing() {
    let frames = build_empty_frames_record(2);
    for _ in 0..5 {
        let rc = unsafe {
            nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp)
        };
        assert_eq!(rc, 1);
    }
    assert_eq!(current_depth(&frames), 5);
    // Backing must have grown from 2 → 4 → 8 to fit 5 frames.
    if let Sexp::Record(r) = &frames {
        if let Sexp::Vector(v) = &r.slots[0] {
            assert!(v.value.len() >= 5,
                    "backing must hold ≥ 5 slots, got {}", v.value.len());
        }
    }
}
