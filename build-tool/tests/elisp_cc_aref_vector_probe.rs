//! Doc 111 §111.C probe — direct calls into the
//! Phase 47-compiled `aref` Vector object.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_aref_vector(vector: Sexp, idx: i64) -> Sexp {
    let idx_sexp = Sexp::Int(idx);
    let mut slot = Sexp::Nil;
    let slot_ptr = &mut slot as *mut Sexp;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::aref_vector(
            &vector as *const Sexp,
            &idx_sexp as *const Sexp,
            slot_ptr,
        )
    };
    assert_eq!(
        returned, slot_ptr,
        "extern must return the caller-provided slot pointer"
    );
    slot
}

#[test]
fn aref_vector_first_element() {
    assert_eq!(
        run_aref_vector(
            Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]),
            0,
        ),
        Sexp::Int(1),
    );
}

#[test]
fn aref_vector_middle_element() {
    assert_eq!(
        run_aref_vector(
            Sexp::vector(vec![Sexp::Int(10), Sexp::Int(20), Sexp::Int(30)]),
            1,
        ),
        Sexp::Int(20),
    );
}

#[test]
fn aref_vector_last_element() {
    assert_eq!(
        run_aref_vector(
            Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]),
            2,
        ),
        Sexp::Int(3),
    );
}

#[test]
fn aref_vector_large_middle_element() {
    let items: Vec<Sexp> = (0..100).map(|i| Sexp::Int(i as i64)).collect();
    assert_eq!(run_aref_vector(Sexp::vector(items), 57), Sexp::Int(57));
}

#[test]
fn aref_vector_heap_backed_element_is_stable() {
    let value = run_aref_vector(
        Sexp::vector(vec![
            Sexp::Str("left".into()),
            Sexp::Str("middle".into()),
            Sexp::Str("right".into()),
        ]),
        1,
    );
    assert_eq!(value, Sexp::Str("middle".into()));
}
