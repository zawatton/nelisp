//! Doc 111 §111.E #24 probe — `frame_stack_find_rust_direct' Phase 47
//! helper (with private helpers `frame_lookup_rust_direct' /
//! `frame_lookup_in' folded in).

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::nlcell::NlCellRef;
use nelisp_build_tool::eval::sexp::Sexp;

fn build_empty_frames_record() -> Sexp {
    let backing = Sexp::vector(vec![Sexp::Nil; 8]);
    Sexp::record(
        Sexp::Symbol("nelisp-lexframe-stack".into()),
        vec![backing, Sexp::Int(0)],
    )
}

#[test]
fn frame_stack_find_on_empty_stack_returns_null() {
    let frames = build_empty_frames_record();
    let name = Sexp::Symbol("alpha".into());
    let got = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &name as *const Sexp,
        )
    };
    assert!(got.is_null(), "find on empty stack returns null");
}

#[test]
fn frame_stack_find_innermost_frame_hit() {
    let frames = build_empty_frames_record();
    unsafe { nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp); }
    let name = Sexp::Symbol("alpha".into());
    let cell = Sexp::Cell(NlCellRef::new(Sexp::Int(42)));
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &name as *const Sexp,
            &cell as *const Sexp,
        );
    }
    let found = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &name as *const Sexp,
        )
    };
    assert!(!found.is_null());
    let slot = unsafe { &*found };
    assert!(matches!(slot, Sexp::Cell(_)));
}

#[test]
fn frame_stack_find_walks_to_outer_frame() {
    // Bind `alpha' in the outer frame, then push a new inner frame
    // that doesn't bind it.  Lookup must walk up to the outer.
    let frames = build_empty_frames_record();
    let alpha = Sexp::Symbol("alpha".into());
    let cell = Sexp::Cell(NlCellRef::new(Sexp::Int(7)));
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &alpha as *const Sexp,
            &cell as *const Sexp,
        );
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
    }
    // Inner frame is empty; lookup walks to outer.
    let found = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &alpha as *const Sexp,
        )
    };
    assert!(!found.is_null(), "outer frame binding must be reachable");
    let slot = unsafe { &*found };
    assert!(matches!(slot, Sexp::Cell(_)));
}

#[test]
fn frame_stack_find_inner_shadow_returns_innermost() {
    let frames = build_empty_frames_record();
    let alpha = Sexp::Symbol("alpha".into());
    let outer_cell = Sexp::Cell(NlCellRef::new(Sexp::Int(1)));
    let inner_cell = Sexp::Cell(NlCellRef::new(Sexp::Int(2)));
    unsafe {
        // outer
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &alpha as *const Sexp,
            &outer_cell as *const Sexp,
        );
        // inner shadow
        nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp);
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &alpha as *const Sexp,
            &inner_cell as *const Sexp,
        );
    }
    let found = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &alpha as *const Sexp,
        )
    };
    assert!(!found.is_null());
    let slot = unsafe { &*found };
    if let Sexp::Cell(c) = slot {
        assert_eq!(c.value, Sexp::Int(2),
                   "innermost binding shadows outer");
    } else {
        panic!("expected Cell, got {:?}", slot);
    }
}
