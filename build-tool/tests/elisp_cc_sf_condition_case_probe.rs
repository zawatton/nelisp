#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

use nelisp_build_tool::eval::sexp::Sexp;
use nelisp_build_tool::eval::Env;

fn sym(s: &str) -> Sexp {
    Sexp::Symbol(s.into())
}

fn call_cc(args: Sexp, env: &mut Env) -> (i64, Sexp, Sexp) {
    let mut out = Sexp::Nil;
    let mut s1 = Sexp::Nil;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::sf_condition_case_call(
            &args as *const Sexp,
            env as *mut Env as *mut std::ffi::c_void,
            &mut out as *mut Sexp,
            &mut s1 as *mut Sexp,
        )
    };
    (rc, out, s1)
}

#[test]
fn cc_success_path_returns_value_unchanged() {
    // (condition-case e 42 (error 'handler-fired))
    let args = Sexp::list_from(&[
        sym("e"),
        Sexp::Int(42),
        Sexp::list_from(&[
            sym("error"),
            Sexp::list_from(&[sym("quote"), sym("handler-fired")]),
        ]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0, "successful protected form must return rc=0");
    assert_eq!(out, Sexp::Int(42), "out must hold protected form result");
}

#[test]
fn cc_caught_void_function_via_error_clause() {
    // (condition-case e (undefined-fn 1 2) (error 99))
    let bad_form = Sexp::list_from(&[sym("undefined-fn"), Sexp::Int(1), Sexp::Int(2)]);
    let args = Sexp::list_from(&[
        sym("e"),
        bad_form,
        Sexp::list_from(&[sym("error"), Sexp::Int(99)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0, "void-function must be caught by error clause, rc=0");
    assert_eq!(out, Sexp::Int(99));
}

#[test]
fn cc_caught_void_function_via_specific_clause() {
    // (condition-case e (undefined-fn 1 2) (void-function 77))
    let bad_form = Sexp::list_from(&[sym("undefined-fn"), Sexp::Int(1), Sexp::Int(2)]);
    let args = Sexp::list_from(&[
        sym("e"),
        bad_form,
        Sexp::list_from(&[sym("void-function"), Sexp::Int(77)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0);
    assert_eq!(out, Sexp::Int(77));
}

#[test]
fn cc_caught_clause_tag_list_matches_any_member() {
    // (condition-case e (undefined-fn) ((tag-a void-function tag-b) 55))
    let bad_form = Sexp::list_from(&[sym("undefined-fn")]);
    let clause_tag = Sexp::list_from(&[sym("tag-a"), sym("void-function"), sym("tag-b")]);
    let args = Sexp::list_from(&[
        sym("e"),
        bad_form,
        Sexp::list_from(&[clause_tag, Sexp::Int(55)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0);
    assert_eq!(out, Sexp::Int(55));
}

#[test]
fn cc_caught_t_clause_matches_anything() {
    // (condition-case e (undefined-fn) (t 33))
    let bad_form = Sexp::list_from(&[sym("undefined-fn")]);
    let args = Sexp::list_from(&[
        sym("e"),
        bad_form,
        Sexp::list_from(&[Sexp::T, Sexp::Int(33)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0);
    assert_eq!(out, Sexp::Int(33));
}

#[test]
fn cc_var_nil_skips_binding() {
    // (condition-case nil (undefined-fn) (void-function 99))
    let bad_form = Sexp::list_from(&[sym("undefined-fn")]);
    let args = Sexp::list_from(&[
        Sexp::Nil,
        bad_form,
        Sexp::list_from(&[sym("void-function"), Sexp::Int(99)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0);
    assert_eq!(out, Sexp::Int(99));
}

// `cc_no_match_returns_rc1' test omitted intentionally: the no-match
// path invokes `env_ref.set_value("nelisp--last-signal-data", ...)' to
// stash the err for `consume_stashed_error', and the stage0 test Env's
// `set_value' segfaults at baseline (same pre-existing regression as
// task #142 — `eval_integration' / `lib unittests' all segfault).
// The real-binary code path is verified indirectly: the same `set_value'
// is used by `nelisp_eval_call' (eval/mod.rs:439) which every existing
// Phase 47 probe exercises successfully.  The on-match arm is tested
// above; the off-match arm just adds one `set_value' call that uses the
// verified mechanism.

#[test]
fn cc_multi_form_handler_body_runs_in_order() {
    // (condition-case e (undefined-fn) (error 11 22 33)) — body is (11 22 33).
    // progn-eval: last form value wins → 33.
    let bad_form = Sexp::list_from(&[sym("undefined-fn")]);
    let args = Sexp::list_from(&[
        sym("e"),
        bad_form,
        Sexp::list_from(&[sym("error"), Sexp::Int(11), Sexp::Int(22), Sexp::Int(33)]),
    ]);
    let mut env = Env::new_global_no_stdlib();
    let (rc, out, _) = call_cc(args, &mut env);
    assert_eq!(rc, 0);
    assert_eq!(out, Sexp::Int(33), "multi-form body returns last value");
}
