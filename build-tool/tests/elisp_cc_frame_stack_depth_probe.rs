//! Doc 111 §111.E #19 probe — direct calls into the Phase 47-compiled
//! `frame_stack_depth' helper.  Verifies the elisp body's composition
//! of `record-slot-ref-ptr' (§111.B) + `sexp-int-unwrap' (§100) over
//! the `nelisp-lexframe-stack' frames-record layout.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

/// Build an empty frames-record matching the layout produced by
/// `Env::install_empty_frames_record_rust_direct': record `nelisp-
/// lexframe-stack' with slots `[Vector(8 Nils), Int(0)]'.
fn build_empty_frames_record(initial_capacity: usize, depth: i64) -> Sexp {
    let backing = Sexp::vector(vec![Sexp::Nil; initial_capacity]);
    Sexp::record(
        Sexp::Symbol("nelisp-lexframe-stack".into()),
        vec![backing, Sexp::Int(depth)],
    )
}

#[test]
fn frame_stack_depth_zero_for_empty_stack() {
    let frames = build_empty_frames_record(8, 0);
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_depth(&frames as *const Sexp)
    };
    assert_eq!(got, 0, "depth of freshly-installed frames-record is 0");
}

#[test]
fn frame_stack_depth_reads_int_payload() {
    // Manually craft a frames-record with depth=3 (no actual frames,
    // just exercising the int read).
    let frames = build_empty_frames_record(8, 3);
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_depth(&frames as *const Sexp)
    };
    assert_eq!(got, 3);
}

#[test]
fn frame_stack_depth_large_value() {
    // Edge: depth past the natural 8-slot initial capacity (= post-
    // grow scenario).  The helper just reads the Int slot, no
    // capacity check.
    let frames = build_empty_frames_record(32, 17);
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_depth(&frames as *const Sexp)
    };
    assert_eq!(got, 17);
}
