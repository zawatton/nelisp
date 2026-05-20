#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_make_vector(n: i64, init: Sexp) -> Sexp {
    let n_sexp = Sexp::Int(n);
    let mut slot = Sexp::Nil;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::bi_make_vector(
            &n_sexp as *const Sexp,
            &init as *const Sexp,
            &mut slot as *mut Sexp,
        )
    };
    assert_eq!(
        returned, 1,
        "bi_make_vector must return 1 on success (= `and' chain terminator)",
    );
    slot
}

#[test]
fn make_vector_zero_length() {
    // N=0 → fresh empty vector.  The fill helper exits on the first
    // `<' test (0 < 0 = false), so no `vector-slot-set' fires.
    assert_eq!(run_make_vector(0, Sexp::Nil), Sexp::vector(vec![]));
}

#[test]
fn make_vector_one_element() {
    assert_eq!(
        run_make_vector(1, Sexp::Int(42)),
        Sexp::vector(vec![Sexp::Int(42)]),
    );
}

#[test]
fn make_vector_five_with_int_init() {
    assert_eq!(
        run_make_vector(5, Sexp::Int(7)),
        Sexp::vector(vec![
            Sexp::Int(7),
            Sexp::Int(7),
            Sexp::Int(7),
            Sexp::Int(7),
            Sexp::Int(7),
        ]),
    );
}

#[test]
fn make_vector_three_with_nil_init() {
    assert_eq!(
        run_make_vector(3, Sexp::Nil),
        Sexp::vector(vec![Sexp::Nil, Sexp::Nil, Sexp::Nil]),
    );
}

#[test]
fn make_vector_string_init_refcount_preserved() {
    // Heap-tagged INIT (`Sexp::Str') exercises the refcount-aware
    // `vector-slot-set' clone path: each slot gets an independent
    // refcount bump, and the caller's `init' handle stays alive
    // after the call.
    let init = Sexp::Str("hello".into());
    let vec = run_make_vector(3, init.clone());
    assert_eq!(
        vec,
        Sexp::vector(vec![
            Sexp::Str("hello".into()),
            Sexp::Str("hello".into()),
            Sexp::Str("hello".into()),
        ]),
    );
    // INIT is still usable (= refcount bookkeeping survived).
    assert_eq!(init, Sexp::Str("hello".into()));
}
