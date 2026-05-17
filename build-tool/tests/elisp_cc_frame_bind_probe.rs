//! Doc 111 §111.E #23 probe — `frame_bind_rust_direct' Phase 47 helper
//! (with private helper `frame_bind_into' folded in).

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
fn frame_bind_on_empty_stack_is_no_op() {
    let frames = build_empty_frames_record();
    let name = Sexp::Symbol("alpha".into());
    let cell = Sexp::Cell(NlCellRef::new(Sexp::Int(1)));
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &name as *const Sexp,
            &cell as *const Sexp,
        )
    };
    assert_eq!(rc, 0, "bind on empty stack must be a no-op");
}

#[test]
fn frame_bind_then_find_returns_cell() {
    let frames = build_empty_frames_record();
    unsafe { nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp); }
    let name = Sexp::Symbol("alpha".into());
    let cell = Sexp::Cell(NlCellRef::new(Sexp::Int(42)));
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &name as *const Sexp,
            &cell as *const Sexp,
        )
    };
    assert_eq!(rc, 1);
    // Verify via the sibling #24 probe.
    let found = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &name as *const Sexp,
        )
    };
    assert!(!found.is_null(), "after bind, find must return non-null");
    let slot = unsafe { &*found };
    assert!(matches!(slot, Sexp::Cell(_)), "found slot must be a Cell");
}

#[test]
fn frame_bind_update_existing_pair_in_place() {
    let frames = build_empty_frames_record();
    unsafe { nelisp_build_tool::elisp_cc_spike::frame_push(&frames as *const Sexp); }
    let name = Sexp::Symbol("alpha".into());
    let cell1 = Sexp::Cell(NlCellRef::new(Sexp::Int(1)));
    let cell2 = Sexp::Cell(NlCellRef::new(Sexp::Int(2)));
    unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &name as *const Sexp,
            &cell1 as *const Sexp,
        );
        nelisp_build_tool::elisp_cc_spike::frame_bind(
            &frames as *const Sexp,
            &name as *const Sexp,
            &cell2 as *const Sexp,
        );
    }
    let found = unsafe {
        nelisp_build_tool::elisp_cc_spike::frame_stack_find(
            &frames as *const Sexp,
            &name as *const Sexp,
        )
    };
    assert!(!found.is_null());
    // The cell now holds Int(2); pair was updated in place.
    let slot = unsafe { &*found };
    if let Sexp::Cell(c) = slot {
        assert_eq!(c.value, Sexp::Int(2),
                   "second bind must overwrite first via set_cdr");
    } else {
        panic!("expected Cell, got {:?}", slot);
    }
}
