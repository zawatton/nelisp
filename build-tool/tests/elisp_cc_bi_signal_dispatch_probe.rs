#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;

fn dispatch(tag_name: &str) -> i64 {
    let tag = Sexp::Symbol(tag_name.into());
    let q = Sexp::Symbol("quit".into());
    let a = Sexp::Symbol("arith-error".into());
    let w = Sexp::Symbol("wrong-type-argument".into());
    unsafe { nelisp_build_tool::elisp_cc_spike::bi_signal_dispatch(&tag, &q, &a, &w) }
}

// ---- Case 1: "quit" → 0 ----

#[test]
fn signal_dispatch_quit_returns_zero() {
    assert_eq!(
        dispatch("quit"),
        0,
        "\"quit\" tag must return discriminant 0 (= EvalError::Quit path)"
    );
}

// ---- Case 2: "arith-error" → 1 ----

#[test]
fn signal_dispatch_arith_error_returns_one() {
    assert_eq!(
        dispatch("arith-error"),
        1,
        "\"arith-error\" tag must return discriminant 1 (= EvalError::ArithError path)"
    );
}

// ---- Case 3: "wrong-type-argument" → 2 ----

#[test]
fn signal_dispatch_wrong_type_argument_returns_two() {
    assert_eq!(
        dispatch("wrong-type-argument"),
        2,
        "\"wrong-type-argument\" tag must return discriminant 2 \
         (= EvalError::WrongType path)"
    );
}

// ---- Case 4: unknown tag → 3 ----

#[test]
fn signal_dispatch_user_error_tag_returns_three() {
    assert_eq!(
        dispatch("my-custom-error"),
        3,
        "unknown tag must return discriminant 3 (= EvalError::UserError path)"
    );
    // A second distinct unknown name also returns 3.
    assert_eq!(
        dispatch("file-error"),
        3,
        "any non-builtin tag must return discriminant 3"
    );
}
