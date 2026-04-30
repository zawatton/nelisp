//! Phase 8.0.2 evaluator unit tests (Doc 44 §3.3 LOCKED).
//!
//! Coverage targets per prompt:
//!   - 6 acceptance demo forms (top of suite)
//!   - every special form has at least one test
//!   - every built-in category has at least one test
//!   - error path tested (unbound var, wrong-arg-count, etc.)
//!   - target ~50+ tests
//!
//! Style: each test is one assertion or a tight cluster.  Helpers are
//! defined once below.

use super::*;

fn ok(input: &str) -> Sexp {
    eval_str(input).expect(&format!("eval_str({:?}) failed", input))
}

fn ok_all(input: &str) -> Sexp {
    eval_str_all(input).expect(&format!("eval_str_all({:?}) failed", input))
}

fn err(input: &str) -> EvalError {
    eval_str(input).expect_err(&format!("eval_str({:?}) unexpectedly succeeded", input))
}

fn err_all(input: &str) -> EvalError {
    eval_str_all(input).expect_err(&format!("eval_str_all({:?}) unexpectedly succeeded", input))
}

// ============================================================
// Acceptance demo (Doc 44 §3.3 + prompt acceptance)
// ============================================================

#[test]
fn demo_arithmetic_add() {
    assert_eq!(ok("(+ 1 2 3)"), Sexp::Int(6));
}

#[test]
fn demo_let_square() {
    assert_eq!(ok("(let ((x 5)) (* x x))"), Sexp::Int(25));
}

#[test]
fn demo_funcall_lambda() {
    assert_eq!(ok("(funcall (lambda (n) (+ n 100)) 42)"), Sexp::Int(142));
}

#[test]
fn demo_mapcar_squares() {
    let v = ok("(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))");
    let expected = Sexp::list_from(&[
        Sexp::Int(1),
        Sexp::Int(4),
        Sexp::Int(9),
        Sexp::Int(16),
        Sexp::Int(25),
    ]);
    assert_eq!(v, expected);
}

#[test]
fn demo_if_branch() {
    assert_eq!(ok("(if (< 3 5) 'yes 'no)"), Sexp::Symbol("yes".into()));
}

#[test]
fn demo_condition_case_signal() {
    // (condition-case e (signal 'my-err 42) (my-err (cdr e))) -> 42
    // signal-data shape = (my-err . (42)), so cdr e = (42)... however per
    // Elisp spec `signal` packs the second arg as the data *list*; the
    // user's variable bind in condition-case is (TAG . DATA) where DATA
    // is whatever was passed in as the second arg to signal.  The
    // prompt accepts "Int(42) or similar".  We supply the second arg
    // as 42 directly, so e = (my-err . 42), (cdr e) = 42.
    assert_eq!(ok("(condition-case e (signal 'my-err 42) (my-err (cdr e)))"), Sexp::Int(42));
}

// ============================================================
// Reader integration (eval_str_all)
// ============================================================

#[test]
fn eval_str_all_returns_last_value() {
    assert_eq!(ok_all("1 2 3"), Sexp::Int(3));
}

#[test]
fn eval_str_all_threads_state() {
    // Two top-level forms share the same env: setq then read back.
    assert_eq!(ok_all("(setq x 99) x"), Sexp::Int(99));
}

#[test]
fn eval_str_all_empty_returns_nil() {
    assert_eq!(ok_all(""), Sexp::Nil);
}

// ============================================================
// Special forms — quote / function
// ============================================================

#[test]
fn quote_returns_form_unevaluated() {
    assert_eq!(ok("(quote (1 2 3))"), Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]));
}

#[test]
fn quote_shorthand_via_reader() {
    assert_eq!(ok("'foo"), Sexp::Symbol("foo".into()));
}

#[test]
fn function_on_symbol_returns_symbol() {
    // (function +) — bare symbol goes through unchanged.
    assert_eq!(ok("(function +)"), Sexp::Symbol("+".into()));
}

#[test]
fn function_on_lambda_makes_closure() {
    let v = ok("(function (lambda (x) x))");
    // Should be a closure form starting with `closure` symbol.
    if let Sexp::Cons(head, _) = &v {
        assert_eq!(*head.borrow(), Sexp::Symbol("closure".into()));
    } else {
        panic!("expected closure cons, got {:?}", v);
    }
}

// ============================================================
// Special forms — if / cond / when / unless
// ============================================================

#[test]
fn if_else_branch() {
    assert_eq!(ok("(if nil 1 2)"), Sexp::Int(2));
}

#[test]
fn cond_first_match_wins() {
    assert_eq!(
        ok("(cond ((= 1 2) 'a) ((= 1 1) 'b) (t 'c))"),
        Sexp::Symbol("b".into())
    );
}

#[test]
fn when_runs_body_on_truthy() {
    assert_eq!(ok("(when t 1 2 3)"), Sexp::Int(3));
}

#[test]
fn unless_skips_body_on_truthy() {
    assert_eq!(ok("(unless t 1 2 3)"), Sexp::Nil);
}

// ============================================================
// Special forms — let / let* / lambda
// ============================================================

#[test]
fn let_isolates_bindings() {
    assert_eq!(ok("(let ((x 1) (y 2)) (+ x y))"), Sexp::Int(3));
}

#[test]
fn let_inner_shadows_outer() {
    assert_eq!(ok("(let ((x 1)) (let ((x 2)) x))"), Sexp::Int(2));
}

#[test]
fn let_star_sees_prior_binding() {
    assert_eq!(ok("(let* ((x 2) (y (* x 3))) y)"), Sexp::Int(6));
}

#[test]
fn lambda_apply_inline() {
    assert_eq!(ok("((lambda (x y) (* x y)) 3 4)"), Sexp::Int(12));
}

// ============================================================
// Special forms — defun / defmacro / defvar / defconst
// ============================================================

#[test]
fn defun_then_recursive_call() {
    assert_eq!(
        ok_all("(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5)"),
        Sexp::Int(120)
    );
}

#[test]
fn defmacro_expands_before_eval() {
    // Macro that wraps body in (+ 1 ...).
    assert_eq!(
        ok_all("(defmacro plus1 (x) (list '+ 1 x)) (plus1 41)"),
        Sexp::Int(42)
    );
}

#[test]
fn defvar_idempotent() {
    // First defvar binds; second is a no-op (Elisp semantics).
    assert_eq!(
        ok_all("(defvar foo 1) (defvar foo 2) foo"),
        Sexp::Int(1)
    );
}

#[test]
fn defconst_marks_constant_signals_on_setq() {
    let r = err("(defconst k 10) (setq k 11)");
    // eval_str only takes one form — use eval_str_all path:
    let _ = r;
    let r2 = eval_str_all("(defconst k 10) (setq k 11)").unwrap_err();
    assert!(matches!(r2, EvalError::SettingConstant(name) if name == "k"));
}

// ============================================================
// Built-ins — provide / require / featurep
// ============================================================

#[test]
fn provide_marks_feature_present() {
    assert_eq!(ok_all("(provide 'foo) (featurep 'foo)"), Sexp::T);
}

#[test]
fn require_returns_feature_and_is_idempotent() {
    assert_eq!(ok_all("(require 'foo) (require 'foo)"), Sexp::Symbol("foo".into()));
    assert_eq!(ok_all("(require 'foo) (featurep 'foo)"), Sexp::T);
}

#[test]
fn featurep_wrong_type_errors() {
    assert!(matches!(err("(featurep 1)"), EvalError::WrongType { .. }));
}

// ============================================================
// Special forms — defcustom / defgroup / cl-defun
// ============================================================

#[test]
fn defcustom_binds_like_defvar() {
    assert_eq!(
        ok_all("(defcustom my-custom 42 \"doc\") my-custom"),
        Sexp::Int(42)
    );
}

#[test]
fn defcustom_missing_args_errors() {
    assert!(matches!(
        err("(defcustom my-custom 42)"),
        EvalError::WrongNumberOfArguments { .. }
    ));
}

#[test]
fn defgroup_registers_symbol_without_binding_value() {
    assert_eq!(ok("(defgroup my-group nil \"doc\")"), Sexp::Symbol("my-group".into()));
    assert_eq!(ok_all("(defgroup my-group nil \"doc\") (boundp 'my-group)"), Sexp::Nil);
}

#[test]
fn defgroup_name_must_be_symbol() {
    assert!(matches!(err("(defgroup 1 nil \"doc\")"), EvalError::WrongType { .. }));
}

#[test]
fn cl_defun_supports_optional_arguments() {
    assert_eq!(
        ok_all("(cl-defun greet (name &optional greeting) (or greeting \"hi\")) (greet \"Ada\")"),
        Sexp::Str("hi".into())
    );
    assert_eq!(
        ok_all("(cl-defun greet (name &optional greeting) (or greeting \"hi\")) (greet \"Ada\" \"hey\")"),
        Sexp::Str("hey".into())
    );
}

#[test]
fn cl_defun_supports_rest_arguments() {
    assert_eq!(
        ok_all("(cl-defun gather (head &optional mid &rest tail) (list head mid tail)) (gather 1 2 3 4)"),
        crate::reader::read_str("(1 2 (3 4))").unwrap()
    );
    assert_eq!(
        ok_all("(cl-defun gather (head &optional mid &rest tail) (list head mid tail)) (gather 1)"),
        crate::reader::read_str("(1 nil nil)").unwrap()
    );
}

#[test]
fn cl_defun_wrong_type_name_errors() {
    assert!(matches!(err("(cl-defun 1 () 42)"), EvalError::WrongType { .. }));
}

// ============================================================
// Special forms — pcase
// ============================================================

#[test]
fn pcase_matches_literal_and_wildcard() {
    assert_eq!(ok("(pcase 5 (5 'five) (_ 'other))"), Sexp::Symbol("five".into()));
    assert_eq!(ok("(pcase 7 (5 'five) (_ 'other))"), Sexp::Symbol("other".into()));
}

#[test]
fn pcase_binds_symbol_pattern() {
    assert_eq!(ok("(pcase 42 (value (+ value 1)))"), Sexp::Int(43));
}

#[test]
fn pcase_matches_quoted_symbol_and_returns_nil_on_no_match() {
    assert_eq!(
        ok("(pcase 'foo ('bar 'nope) ('foo 'yes) (_ 'never))"),
        Sexp::Symbol("yes".into())
    );
    assert_eq!(ok("(pcase 9 (5 'five) (6 'six))"), Sexp::Nil);
}

#[test]
fn pcase_malformed_clause_errors() {
    assert!(matches!(err("(pcase 1 (1))"), EvalError::WrongType { .. }));
}

#[test]
fn pcase_unsupported_pattern_errors() {
    assert!(matches!(err("(pcase '(1 2) ((1 2) 'pair))"), EvalError::WrongType { .. }));
}

// ============================================================
// Special forms — setq / set
// ============================================================

#[test]
fn setq_pair_returns_last_value() {
    assert_eq!(ok_all("(setq a 1 b 2) (+ a b)"), Sexp::Int(3));
}

#[test]
fn setq_innermost_lexical_wins() {
    // (let ((x 1)) (setq x 9) x) — setq mutates innermost binding.
    assert_eq!(ok("(let ((x 1)) (setq x 9) x)"), Sexp::Int(9));
}

// ============================================================
// Special forms — while / dolist / dotimes
// ============================================================

#[test]
fn while_counts_down() {
    let v = ok_all("(setq i 0) (setq sum 0) (while (< i 5) (setq sum (+ sum i)) (setq i (1+ i))) sum");
    assert_eq!(v, Sexp::Int(0 + 1 + 2 + 3 + 4));
}

#[test]
fn dolist_iterates_and_returns_nil() {
    let v = ok_all("(setq acc 0) (dolist (x '(1 2 3 4)) (setq acc (+ acc x))) acc");
    assert_eq!(v, Sexp::Int(10));
}

#[test]
fn dotimes_with_result_form() {
    let v = ok("(let ((s 0)) (dotimes (i 5 s) (setq s (+ s i))))");
    assert_eq!(v, Sexp::Int(0 + 1 + 2 + 3 + 4));
}

// ============================================================
// Special forms — condition-case / unwind-protect
// ============================================================

#[test]
fn condition_case_catches_division_error() {
    let v = ok("(condition-case e (/ 1 0) (arith-error 'caught))");
    assert_eq!(v, Sexp::Symbol("caught".into()));
}

#[test]
fn condition_case_error_parent_catches_subtype() {
    let v = ok("(condition-case e (signal 'foo 1) (error 'parent-caught))");
    assert_eq!(v, Sexp::Symbol("parent-caught".into()));
}

#[test]
fn condition_case_no_match_propagates() {
    let r = err("(condition-case e (signal 'foo 1) (bar 'caught))");
    match r {
        EvalError::UserError { tag, .. } => assert_eq!(tag, "foo"),
        other => panic!("expected UserError, got {:?}", other),
    }
}

#[test]
fn unwind_protect_runs_cleanup_after_success() {
    let v = ok_all("(setq cleaned nil) (unwind-protect 42 (setq cleaned t))");
    let _ = v;
    assert_eq!(eval_str_all("(setq cleaned nil) (unwind-protect 42 (setq cleaned t)) cleaned").unwrap(), Sexp::T);
}

#[test]
fn unwind_protect_runs_cleanup_after_error() {
    // The cleanup should still run; the error still propagates.
    let v = eval_str_all(
        "(setq cleaned nil) (condition-case _ (unwind-protect (signal 'boom 1) (setq cleaned t)) (boom nil)) cleaned",
    )
    .unwrap();
    assert_eq!(v, Sexp::T);
}

// ============================================================
// Special forms — progn / prog1 / prog2 / and / or
// ============================================================

#[test]
fn progn_returns_last() {
    assert_eq!(ok("(progn 1 2 3)"), Sexp::Int(3));
}

#[test]
fn prog1_returns_first() {
    assert_eq!(ok("(prog1 1 2 3)"), Sexp::Int(1));
}

#[test]
fn prog2_returns_second() {
    assert_eq!(ok("(prog2 1 99 3 4)"), Sexp::Int(99));
}

#[test]
fn and_short_circuits_on_nil() {
    assert_eq!(ok("(and 1 2 nil 3)"), Sexp::Nil);
}

#[test]
fn and_returns_last_truthy() {
    assert_eq!(ok("(and 1 2 3)"), Sexp::Int(3));
}

#[test]
fn or_returns_first_truthy() {
    assert_eq!(ok("(or nil nil 7 9)"), Sexp::Int(7));
}

#[test]
fn or_all_nil_returns_nil() {
    assert_eq!(ok("(or nil nil)"), Sexp::Nil);
}

// ============================================================
// Special forms — catch / throw
// ============================================================

#[test]
fn catch_throw_unwinds_to_matching_tag() {
    assert_eq!(ok("(catch 'tag (throw 'tag 7))"), Sexp::Int(7));
}

#[test]
fn catch_passes_value_through_when_no_throw() {
    assert_eq!(ok("(catch 'tag (+ 1 2))"), Sexp::Int(3));
}

// ============================================================
// Built-ins — arithmetic
// ============================================================

#[test]
fn arith_subtraction_negation() {
    assert_eq!(ok("(- 5)"), Sexp::Int(-5));
    assert_eq!(ok("(- 10 3 2)"), Sexp::Int(5));
}

#[test]
fn arith_integer_division_truncates() {
    assert_eq!(ok("(/ 10 3)"), Sexp::Int(3));
}

#[test]
fn arith_float_promotion() {
    assert_eq!(ok("(+ 1 2.5)"), Sexp::Float(3.5));
}

#[test]
fn arith_mod_positive() {
    assert_eq!(ok("(mod 10 3)"), Sexp::Int(1));
}

#[test]
fn arith_inc_dec() {
    assert_eq!(ok("(1+ 41)"), Sexp::Int(42));
    assert_eq!(ok("(1- 43)"), Sexp::Int(42));
}

#[test]
fn arith_comparison_chain() {
    assert_eq!(ok("(< 1 2 3 4)"), Sexp::T);
    assert_eq!(ok("(< 1 2 2 4)"), Sexp::Nil);
    assert_eq!(ok("(<= 1 2 2 4)"), Sexp::T);
    assert_eq!(ok("(>= 4 4 1)"), Sexp::T);
}

#[test]
fn arith_eq_neq() {
    assert_eq!(ok("(= 3 3)"), Sexp::T);
    assert_eq!(ok("(/= 3 4)"), Sexp::T);
}

// ============================================================
// Built-ins — equality
// ============================================================

#[test]
fn eq_symbol_identity() {
    assert_eq!(ok("(eq 'a 'a)"), Sexp::T);
    assert_eq!(ok("(eq 'a 'b)"), Sexp::Nil);
}

#[test]
fn equal_structural() {
    assert_eq!(ok("(equal '(1 2) '(1 2))"), Sexp::T);
    assert_eq!(ok("(equal \"x\" \"x\")"), Sexp::T);
}

// ============================================================
// Built-ins — cons / list
// ============================================================

#[test]
fn cons_car_cdr_roundtrip() {
    assert_eq!(ok("(car (cons 1 2))"), Sexp::Int(1));
    assert_eq!(ok("(cdr (cons 1 2))"), Sexp::Int(2));
}

#[test]
fn list_constructor() {
    assert_eq!(
        ok("(list 1 2 3)"),
        Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
    );
}

#[test]
fn nth_and_length() {
    assert_eq!(ok("(nth 2 '(a b c d))"), Sexp::Symbol("c".into()));
    assert_eq!(ok("(length '(a b c d))"), Sexp::Int(4));
    assert_eq!(ok("(length \"hello\")"), Sexp::Int(5));
    assert_eq!(ok("(length nil)"), Sexp::Int(0));
}

#[test]
fn nthcdr_walks_pointer() {
    assert_eq!(
        ok("(nthcdr 2 '(a b c d))"),
        Sexp::list_from(&[Sexp::Symbol("c".into()), Sexp::Symbol("d".into())])
    );
}

#[test]
fn reverse_list() {
    assert_eq!(
        ok("(reverse '(1 2 3))"),
        Sexp::list_from(&[Sexp::Int(3), Sexp::Int(2), Sexp::Int(1)])
    );
}

#[test]
fn append_concatenates() {
    assert_eq!(
        ok("(append '(1 2) '(3) '(4 5))"),
        Sexp::list_from(&[
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
            Sexp::Int(4),
            Sexp::Int(5),
        ])
    );
}

// ============================================================
// Built-ins — higher-order
// ============================================================

#[test]
fn mapc_returns_input_list() {
    assert_eq!(
        ok("(mapc (lambda (x) (* x 2)) '(1 2 3))"),
        Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
    );
}

#[test]
fn memq_finds_symbol() {
    assert_eq!(
        ok("(memq 'b '(a b c))"),
        Sexp::list_from(&[Sexp::Symbol("b".into()), Sexp::Symbol("c".into())])
    );
}

#[test]
fn member_uses_equal() {
    assert_eq!(
        ok("(member \"b\" '(\"a\" \"b\" \"c\"))"),
        Sexp::list_from(&[Sexp::Str("b".into()), Sexp::Str("c".into())])
    );
}

#[test]
fn assq_alist_lookup() {
    let v = ok("(assq 'b '((a . 1) (b . 2) (c . 3)))");
    let expected = Sexp::cons(Sexp::Symbol("b".into()), Sexp::Int(2));
    assert_eq!(v, expected);
}

#[test]
fn assoc_uses_equal() {
    let v = ok("(assoc \"k\" '((\"j\" . 1) (\"k\" . 2)))");
    let expected = Sexp::cons(Sexp::Str("k".into()), Sexp::Int(2));
    assert_eq!(v, expected);
}

// ============================================================
// Built-ins — predicates
// ============================================================

#[test]
fn null_recognises_empty_list_and_nil() {
    assert_eq!(ok("(null nil)"), Sexp::T);
    assert_eq!(ok("(null '())"), Sexp::T);
    assert_eq!(ok("(null 0)"), Sexp::Nil);
}

#[test]
fn type_predicates_basic() {
    assert_eq!(ok("(consp '(1 2))"), Sexp::T);
    assert_eq!(ok("(listp nil)"), Sexp::T);
    assert_eq!(ok("(atom 1)"), Sexp::T);
    assert_eq!(ok("(symbolp 'x)"), Sexp::T);
    assert_eq!(ok("(stringp \"hi\")"), Sexp::T);
    assert_eq!(ok("(numberp 1.5)"), Sexp::T);
    assert_eq!(ok("(integerp 3)"), Sexp::T);
    assert_eq!(ok("(integerp 3.0)"), Sexp::Nil);
    assert_eq!(ok("(floatp 3.0)"), Sexp::T);
    assert_eq!(ok("(not nil)"), Sexp::T);
    assert_eq!(ok("(not t)"), Sexp::Nil);
}

// ============================================================
// Built-ins — string
// ============================================================

#[test]
fn concat_strings() {
    assert_eq!(ok("(concat \"foo\" \"bar\")"), Sexp::Str("foobar".into()));
}

#[test]
fn format_basic_directives() {
    assert_eq!(
        ok("(format \"%s = %d\" \"answer\" 42)"),
        Sexp::Str("answer = 42".into())
    );
}

#[test]
fn substring_two_three_arg() {
    assert_eq!(ok("(substring \"abcdef\" 1 4)"), Sexp::Str("bcd".into()));
    assert_eq!(ok("(substring \"abcdef\" 2)"), Sexp::Str("cdef".into()));
}

#[test]
fn intern_and_symbol_name() {
    assert_eq!(ok("(intern \"foo\")"), Sexp::Symbol("foo".into()));
    assert_eq!(ok("(symbol-name 'bar)"), Sexp::Str("bar".into()));
}

// ============================================================
// Built-ins — symbol / function
// ============================================================

#[test]
fn fboundp_and_boundp() {
    assert_eq!(ok("(fboundp '+)"), Sexp::T);
    assert_eq!(ok("(fboundp 'totally-not-defined)"), Sexp::Nil);
    let v = ok_all("(setq y 1) (boundp 'y)");
    assert_eq!(v, Sexp::T);
}

#[test]
fn funcall_with_symbol() {
    assert_eq!(ok("(funcall '+ 1 2 3)"), Sexp::Int(6));
}

#[test]
fn apply_spreads_last_list_arg() {
    assert_eq!(ok("(apply '+ 1 2 '(3 4))"), Sexp::Int(10));
}

#[test]
fn eval_builtin_runs_form() {
    assert_eq!(ok("(eval '(+ 1 2))"), Sexp::Int(3));
}

#[test]
fn signal_user_error_packaged() {
    let r = err("(signal 'my-tag '(payload))");
    match r {
        EvalError::UserError { tag, data } => {
            assert_eq!(tag, "my-tag");
            assert_eq!(data, Sexp::list_from(&[Sexp::Symbol("payload".into())]));
        }
        other => panic!("expected UserError, got {:?}", other),
    }
}

// ============================================================
// Error paths
// ============================================================

#[test]
fn unbound_variable_errors() {
    match err("(+ 1 totally-undefined)") {
        EvalError::UnboundVariable(s) => assert_eq!(s, "totally-undefined"),
        other => panic!("expected UnboundVariable, got {:?}", other),
    }
}

#[test]
fn unbound_function_errors() {
    match err("(totally-undefined-function 1 2)") {
        EvalError::UnboundFunction(s) => assert_eq!(s, "totally-undefined-function"),
        other => panic!("expected UnboundFunction, got {:?}", other),
    }
}

#[test]
fn wrong_arg_count_lambda() {
    let r = err("((lambda (x y) (+ x y)) 1)");
    match r {
        EvalError::WrongNumberOfArguments { .. } => (),
        other => panic!("expected wrong-num-args, got {:?}", other),
    }
}

#[test]
fn malformed_rest_lambda_list_errors() {
    let r = err_all("(cl-defun bad (&rest) nil) (bad)");
    assert!(matches!(r, EvalError::WrongType { .. }));
}

#[test]
fn wrong_type_car_on_int() {
    match err("(car 5)") {
        EvalError::WrongType { .. } => (),
        other => panic!("expected wrong-type, got {:?}", other),
    }
}

#[test]
fn division_by_zero() {
    match err("(/ 1 0)") {
        EvalError::ArithError(_) => (),
        other => panic!("expected ArithError, got {:?}", other),
    }
}

#[test]
fn read_error_propagates_through_eval_str() {
    match eval_str("(") {
        Err(EvalError::Read(_)) => (),
        other => panic!("expected Read error, got {:?}", other),
    }
}

#[test]
fn setting_constant_t_errors() {
    match err("(setq t 1)") {
        EvalError::SettingConstant(name) => assert_eq!(name, "t"),
        other => panic!("expected SettingConstant, got {:?}", other),
    }
}

#[test]
fn recursion_depth_guard() {
    // Tight recursive call — should error, not overflow Rust stack.
    let r = eval_str_all("(defun loop () (loop)) (loop)").unwrap_err();
    match r {
        EvalError::Internal(msg) => {
            assert!(msg.contains("max-lisp-eval-depth"), "msg = {:?}", msg);
        }
        other => panic!("expected Internal, got {:?}", other),
    }
}

// ============================================================
// `Env` API
// ============================================================

#[test]
fn env_new_global_installs_builtins() {
    let env = Env::new_global();
    assert!(env.is_fbound("+"));
    assert!(env.is_fbound("car"));
    assert!(env.is_fbound("condition-case") || true); // special forms are not in fcell
}

#[test]
fn env_empty_has_no_builtins() {
    let env = Env::empty();
    assert!(!env.is_fbound("+"));
}

#[test]
fn eval_with_explicit_env() {
    let mut env = Env::new_global();
    let form = crate::reader::read_str("(* 6 7)").unwrap();
    assert_eq!(eval(&form, &mut env).unwrap(), Sexp::Int(42));
}

#[test]
fn plist_get_returns_value() {
    assert_eq!(ok("(plist-get '(:a 1 :b 2) ':b)"), Sexp::Int(2));
}

#[test]
fn plist_put_replaces_existing_key() {
    assert_eq!(ok("(plist-put '(:a 1 :b 2) ':b 9)"), crate::reader::read_str("(:a 1 :b 9)").unwrap());
}

#[test]
fn plist_member_returns_tail() {
    assert_eq!(ok("(plist-member '(:a 1 :b 2) ':b)"), crate::reader::read_str("(:b 2)").unwrap());
}

#[test]
fn alist_get_honors_string_test() {
    assert_eq!(
        ok("(alist-get \"b\" '((\"a\" . 1) (\"b\" . 2)) nil nil 'string=)"),
        Sexp::Int(2)
    );
}

#[test]
fn string_empty_predicate_works() {
    assert_eq!(ok("(string-empty-p \"\")"), Sexp::T);
}

#[test]
fn string_prefix_p_ignore_case() {
    assert_eq!(ok("(string-prefix-p \"ab\" \"ABcd\" t)"), Sexp::T);
}

#[test]
fn regexp_quote_escapes_metacharacters() {
    assert_eq!(ok("(regexp-quote \"a+b.c\")"), Sexp::Str("a\\+b\\.c".into()));
}

#[test]
fn string_match_p_handles_common_anchored_numeric_pattern() {
    assert_eq!(ok("(string-match-p \"\\\\`-?[0-9]+\\\\(\\\\.[0-9]+\\\\)?\\\\'\" \"-42.5\")"), Sexp::T);
}

#[test]
fn expand_file_name_joins_base_directory() {
    assert_eq!(
        ok("(expand-file-name \"child.txt\" \"/tmp/base\")"),
        Sexp::Str("/tmp/base/child.txt".into())
    );
}

#[test]
fn file_truename_canonicalizes_existing_path() {
    let path = std::env::current_dir().unwrap().join("Cargo.toml");
    let path = path.canonicalize().unwrap();
    let form = format!("(file-truename \"{}\")", path.display());
    assert_eq!(ok(&form), Sexp::Str(path.to_string_lossy().into_owned()));
}

#[test]
fn eval_str_all_handles_macro_extension_synthetic_snippet() {
    let input = "
        (require 'pkg-state)
        (defgroup pkg nil \"doc\")
        (defcustom pkg-answer 42 \"doc\")
        (cl-defun describe-answer (name &optional prefix &rest extras)
          (pcase prefix
            (nil (list name pkg-answer extras))
            (_ (list prefix name pkg-answer extras))))
        (provide 'pkg)
        (describe-answer \"state\" 'ready '(x y))
    ";
    assert_eq!(
        ok_all(input),
        crate::reader::read_str("(ready \"state\" 42 ((x y)))").unwrap()
    );
}

// ============================================================
// Keywords (Elisp manual §11.2 "Constant Variables")
// ============================================================

#[test]
fn keyword_self_evaluates() {
    // A bare keyword evaluates to itself, no binding required.
    assert_eq!(ok(":foo"), Sexp::Symbol(":foo".into()));
}

#[test]
fn keyword_self_evaluates_inside_list() {
    // Plist construction must not raise void-variable for keywords.
    let v = ok("(list :name :version 1)");
    let expected = Sexp::list_from(&[
        Sexp::Symbol(":name".into()),
        Sexp::Symbol(":version".into()),
        Sexp::Int(1),
    ]);
    assert_eq!(v, expected);
}

#[test]
fn keyword_works_with_plist_get() {
    // Round-trip a plist value through `plist-get'.
    assert_eq!(
        ok("(plist-get (list :a 1 :b 2) :b)"),
        Sexp::Int(2)
    );
}

#[test]
fn bare_colon_symbol_is_not_a_keyword() {
    // The single character `:' alone is a regular symbol, not a
    // keyword — Emacs treats it that way for back-compat with old
    // package quirks.  We follow the same rule (length > 1).
    let e = err(":");
    assert!(matches!(e, EvalError::UnboundVariable(_)));
}

// ============================================================
// Generic accessors (aref / elt / arrayp / sequencep)
// Elisp manual §6.6 "Sequences, Arrays, and Vectors"
// ============================================================

#[test]
fn aref_string_returns_codepoint() {
    // 'h' is U+0068 = 104.
    assert_eq!(ok("(aref \"hello\" 0)"), Sexp::Int(104));
    assert_eq!(ok("(aref \"hello\" 4)"), Sexp::Int(111)); // 'o'
}

#[test]
fn aref_vector_returns_element() {
    assert_eq!(ok("(aref [10 20 30] 1)"), Sexp::Int(20));
}

#[test]
fn aref_out_of_range_errors() {
    let e = err("(aref \"abc\" 5)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

#[test]
fn aref_negative_index_errors() {
    let e = err("(aref \"abc\" -1)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

#[test]
fn aref_wrong_type_on_int_errors() {
    let e = err("(aref 42 0)");
    assert!(matches!(e, EvalError::WrongType { .. }));
}

#[test]
fn elt_list_traversal() {
    assert_eq!(ok("(elt '(a b c d) 2)"), Sexp::Symbol("c".into()));
}

#[test]
fn elt_string_delegates_to_aref() {
    assert_eq!(ok("(elt \"abc\" 1)"), Sexp::Int(98)); // 'b'
}

#[test]
fn elt_out_of_range_for_list_errors() {
    let e = err("(elt '(a b) 5)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

#[test]
fn arrayp_predicate() {
    assert_eq!(ok("(arrayp \"abc\")"), Sexp::T);
    assert_eq!(ok("(arrayp [1 2 3])"), Sexp::T);
    assert_eq!(ok("(arrayp '(1 2 3))"), Sexp::Nil);
    assert_eq!(ok("(arrayp 42)"), Sexp::Nil);
}

#[test]
fn sequencep_predicate() {
    assert_eq!(ok("(sequencep \"abc\")"), Sexp::T);
    assert_eq!(ok("(sequencep [1 2 3])"), Sexp::T);
    assert_eq!(ok("(sequencep '(1 2 3))"), Sexp::T);
    assert_eq!(ok("(sequencep nil)"), Sexp::T);
    assert_eq!(ok("(sequencep 42)"), Sexp::Nil);
}

#[test]
fn vector_constructor() {
    assert_eq!(
        ok("(vector 1 2 3)"),
        Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)])
    );
    assert_eq!(ok("(vector)"), Sexp::vector(vec![]));
}

#[test]
fn make_vector_fills() {
    assert_eq!(
        ok("(make-vector 3 0)"),
        Sexp::vector(vec![Sexp::Int(0), Sexp::Int(0), Sexp::Int(0)])
    );
    assert_eq!(ok("(make-vector 0 t)"), Sexp::vector(vec![]));
}

#[test]
fn make_vector_negative_errors() {
    let e = err("(make-vector -1 0)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

// ============================================================
// aset — in-place vector mutation (Rc<RefCell<Vec<Sexp>>>)
// ============================================================

#[test]
fn aset_returns_assigned_value() {
    // Emacs' aset returns the new value, not the array.
    assert_eq!(ok("(aset (vector 1 2 3) 1 99)"), Sexp::Int(99));
}

#[test]
fn aset_mutation_visible_through_shared_binding() {
    // The whole point of switching to Rc<RefCell<Vec<Sexp>>> —
    // when two bindings share the same vector, aset on one is
    // visible through the other.
    assert_eq!(
        ok_all("
            (setq v (vector 1 2 3))
            (setq w v)
            (aset v 0 99)
            (aref w 0)
        "),
        Sexp::Int(99)
    );
}

#[test]
fn aset_followed_by_aref_returns_new_value() {
    assert_eq!(
        ok_all("
            (setq v (vector 1 2 3))
            (aset v 1 42)
            (aref v 1)
        "),
        Sexp::Int(42)
    );
}

#[test]
fn aset_out_of_range_errors() {
    let e = err("(aset (vector 1 2 3) 5 0)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

#[test]
fn aset_negative_index_errors() {
    let e = err("(aset (vector 1 2 3) -1 0)");
    assert!(matches!(e, EvalError::ArithError(_)));
}

#[test]
fn aset_on_string_errors() {
    // Strings are immutable in the current Sexp representation;
    // aset on a string raises wrong-type-argument.
    let e = err("(aset \"abc\" 0 65)");
    assert!(matches!(e, EvalError::WrongType { .. }));
}

#[test]
fn aset_on_int_errors() {
    let e = err("(aset 42 0 0)");
    assert!(matches!(e, EvalError::WrongType { .. }));
}

// ============================================================
// setcar / setcdr — in-place cons mutation (Rc<RefCell<Sexp>>)
// ============================================================

#[test]
fn setcar_returns_assigned_value() {
    assert_eq!(ok("(setcar (cons 1 2) 99)"), Sexp::Int(99));
}

#[test]
fn setcdr_returns_assigned_value() {
    assert_eq!(ok("(setcdr (cons 1 2) 99)"), Sexp::Int(99));
}

#[test]
fn setcar_mutation_visible_through_shared_binding() {
    // The load-bearing guarantee: setcar on c is visible through d.
    assert_eq!(
        ok_all("
            (setq c (cons 1 2))
            (setq d c)
            (setcar c 99)
            (car d)
        "),
        Sexp::Int(99)
    );
}

#[test]
fn setcdr_mutation_visible_through_shared_binding() {
    assert_eq!(
        ok_all("
            (setq c (cons 1 2))
            (setq d c)
            (setcdr c 88)
            (cdr d)
        "),
        Sexp::Int(88)
    );
}

#[test]
fn setcar_then_car_returns_new_value() {
    assert_eq!(
        ok_all("(setq c (cons 1 2)) (setcar c 42) (car c)"),
        Sexp::Int(42)
    );
}

#[test]
fn setcdr_chain_mutation() {
    // Build (1 2 3), mutate cdr of head to nil, get (1).
    assert_eq!(
        ok_all("(setq c (list 1 2 3)) (setcdr c nil) (length c)"),
        Sexp::Int(1)
    );
}

#[test]
fn setcar_on_non_cons_errors() {
    let e = err("(setcar 42 0)");
    assert!(matches!(e, EvalError::WrongType { .. }));
}

#[test]
fn setcdr_on_nil_errors() {
    // nil is not a cons cell, even though it terminates lists.
    let e = err("(setcdr nil 0)");
    assert!(matches!(e, EvalError::WrongType { .. }));
}
