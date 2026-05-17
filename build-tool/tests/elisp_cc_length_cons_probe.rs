#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn run_length(input: Sexp) -> Sexp {
    let mut slot: Sexp = Sexp::Nil;
    let returned = unsafe {
        nelisp_build_tool::elisp_cc_spike::length_cons(
            &input as *const Sexp,
            &mut slot as *mut Sexp,
        )
    };
    assert_eq!(
        returned,
        &mut slot as *mut Sexp,
        "length_cons must return the caller-provided slot pointer",
    );
    slot
}

fn list_of_len(n: usize) -> Sexp {
    let items: Vec<Sexp> = (0..n).map(|i| Sexp::Int(i as i64)).collect();
    Sexp::list_from(&items)
}

#[test]
fn length_cons_nil_is_zero() {
    assert_eq!(run_length(Sexp::Nil), Sexp::Int(0));
}

#[test]
fn length_cons_one_element_is_one() {
    assert_eq!(run_length(list_of_len(1)), Sexp::Int(1));
}

#[test]
fn length_cons_five_elements_is_five() {
    assert_eq!(run_length(list_of_len(5)), Sexp::Int(5));
}

#[test]
fn length_cons_hundred_elements_is_hundred() {
    assert_eq!(run_length(list_of_len(100)), Sexp::Int(100));
}
