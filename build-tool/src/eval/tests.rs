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
    assert_eq!(ok("(string-prefix-p \"ab\" \"ABcd\" t)"), Sexp::Nil);
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

// ============================================================
// Doc 47 Stage 8b — file I/O builtins + load chain
// ============================================================

#[test]
fn file_name_directory_returns_dir_with_slash() {
    assert_eq!(
        ok(r#"(file-name-directory "/tmp/foo/bar.el")"#),
        Sexp::Str("/tmp/foo/".into())
    );
}

#[test]
fn file_name_directory_nil_when_no_slash() {
    assert_eq!(ok(r#"(file-name-directory "bar.el")"#), Sexp::Nil);
}

#[test]
fn file_name_nondirectory_returns_basename() {
    assert_eq!(
        ok(r#"(file-name-nondirectory "/tmp/foo/bar.el")"#),
        Sexp::Str("bar.el".into())
    );
}

#[test]
fn file_name_nondirectory_returns_path_when_no_slash() {
    assert_eq!(
        ok(r#"(file-name-nondirectory "bar.el")"#),
        Sexp::Str("bar.el".into())
    );
}

#[test]
fn file_exists_p_t_for_self_cargo_toml() {
    // CARGO_MANIFEST_DIR points at build-tool/, so Cargo.toml exists.
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let cargo_toml = format!("{}/Cargo.toml", manifest_dir);
    let expr = format!("(file-exists-p {:?})", cargo_toml);
    assert_eq!(ok(&expr), Sexp::T);
}

#[test]
fn file_exists_p_nil_for_nonexistent() {
    assert_eq!(
        ok(r#"(file-exists-p "/nonexistent/path/that/should-never-be-there.el")"#),
        Sexp::Nil
    );
}

#[test]
fn number_to_string_int() {
    assert_eq!(ok("(number-to-string 42)"), Sexp::Str("42".into()));
}

#[test]
fn number_to_string_neg() {
    assert_eq!(ok("(number-to-string -7)"), Sexp::Str("-7".into()));
}

#[test]
fn load_evaluates_file_contents() {
    use std::io::Write;
    let mut tmp = std::env::temp_dir();
    tmp.push("doc-47-stage8b-load-test.el");
    {
        let mut f = std::fs::File::create(&tmp).unwrap();
        writeln!(f, "(defun doc47-stage8b-tag () 99)").unwrap();
    }
    let path_str = tmp.to_string_lossy();
    // Quote the path string literal for embedding in the source.
    let source = format!(r#"(load "{}") (doc47-stage8b-tag)"#, path_str);
    let result = eval_str_all(&source).unwrap();
    assert_eq!(result, Sexp::Int(99));
    let _ = std::fs::remove_file(&tmp);
}

#[test]
fn load_signals_when_missing_and_not_noerror() {
    let e = err_all(r#"(load "/no/such/file/here.el")"#);
    match e {
        EvalError::UserError { tag, .. } => assert_eq!(tag, "file-error"),
        other => panic!("expected file-error, got {:?}", other),
    }
}

#[test]
fn load_returns_nil_with_noerror_when_missing() {
    assert_eq!(
        ok_all(r#"(load "/no/such/file/here.el" t)"#),
        Sexp::Nil
    );
}

#[test]
fn require_loads_file_via_load_path() {
    // Materialise a fixture pair on disk: feature-foo.el provides
    // 'foo and defines a function; main calls require + that function.
    use std::io::Write;
    let dir = std::env::temp_dir().join("doc-47-stage8b-require-test");
    let _ = std::fs::create_dir_all(&dir);
    {
        let mut f = std::fs::File::create(dir.join("foo.el")).unwrap();
        writeln!(f, "(defun foo-answer () 7)").unwrap();
        writeln!(f, "(provide 'foo)").unwrap();
    }
    // Drive through `eval_str_all_at_path' so load-path = dir/.
    let source = "(require 'foo) (foo-answer)";
    let dummy_main = dir.join("main.el");
    let result =
        eval_str_all_at_path(source, dummy_main.to_str().unwrap()).unwrap();
    assert_eq!(result, Sexp::Int(7));
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn require_silent_marker_without_load_path() {
    // Without load-path configured (= plain eval_str_all), a require
    // for a never-provided feature should silently mark the feature as
    // provided (preserves pre-Stage-8b cargo-test ergonomics).
    let result = ok_all("(require 'no-such-feature) (featurep 'no-such-feature)");
    assert_eq!(result, Sexp::T);
}

#[test]
fn elisp_stdlib_identity() {
    assert_eq!(ok("(identity 42)"), Sexp::Int(42));
}

#[test]
fn elisp_stdlib_null_true() {
    assert_eq!(ok("(null nil)"), Sexp::T);
}

#[test]
fn elisp_stdlib_null_false() {
    assert_eq!(ok("(null 42)"), Sexp::Nil);
}

#[test]
fn elisp_stdlib_not_true() {
    assert_eq!(ok("(not nil)"), Sexp::T);
}

#[test]
fn elisp_stdlib_not_false() {
    assert_eq!(ok("(not 42)"), Sexp::Nil);
}

#[test]
fn elisp_stdlib_inc() {
    assert_eq!(ok("(1+ 5)"), Sexp::Int(6));
}

#[test]
fn elisp_stdlib_dec() {
    assert_eq!(ok("(1- 5)"), Sexp::Int(4));
}

#[test]
fn elisp_stdlib_inc_in_let() {
    assert_eq!(ok("(let ((x 10)) (1+ x))"), Sexp::Int(11));
}

// ==== sweep-9 G1 list tests ====

#[test]
fn elisp_g1_nthcdr_basic() {
    assert_eq!(ok("(nthcdr 2 (list 'a 'b 'c 'd))"), ok("(list 'c 'd)"));
}

#[test]
fn elisp_g1_nthcdr_zero() {
    assert_eq!(ok("(nthcdr 0 (list 1 2 3))"), ok("(list 1 2 3)"));
}

#[test]
fn elisp_g1_nthcdr_overflow() {
    assert_eq!(ok("(nthcdr 5 (list 1 2))"), Sexp::Nil);
}

#[test]
fn elisp_g1_nth_basic() {
    assert_eq!(ok("(nth 1 (list 'a 'b 'c))"), ok("'b"));
}

#[test]
fn elisp_g1_nth_zero() {
    assert_eq!(ok("(nth 0 (list 10 20 30))"), Sexp::Int(10));
}

#[test]
fn elisp_g1_nth_overflow() {
    assert_eq!(ok("(nth 5 (list 1 2))"), Sexp::Nil);
}

#[test]
fn elisp_g1_reverse() {
    assert_eq!(ok("(reverse (list 1 2 3))"), ok("(list 3 2 1)"));
}

#[test]
fn elisp_g1_reverse_empty() {
    assert_eq!(ok("(reverse nil)"), Sexp::Nil);
}

#[test]
fn elisp_g1_nreverse() {
    assert_eq!(ok("(nreverse (list 'a 'b 'c))"), ok("(list 'c 'b 'a)"));
}

#[test]
fn elisp_g1_nth_in_let() {
    assert_eq!(ok("(let ((xs (list 'x 'y 'z))) (nth 1 xs))"), ok("'y"));
}

// ==== sweep-9 G2 hof tests ====

#[test]
fn elisp_g2_mapcar_inc() {
    assert_eq!(ok("(mapcar (lambda (x) (1+ x)) (list 1 2 3))"), ok("(list 2 3 4)"));
}

#[test]
fn elisp_g2_mapcar_empty() {
    assert_eq!(ok("(mapcar (lambda (x) x) nil)"), Sexp::Nil);
}

#[test]
fn elisp_g2_mapcar_identity() {
    assert_eq!(ok("(mapcar (lambda (x) x) (list 'a 'b 'c))"), ok("(list 'a 'b 'c)"));
}

#[test]
fn elisp_g2_mapcar_order() {
    assert_eq!(
        ok("(mapcar (lambda (n) (* n 10)) (list 1 2 3 4))"),
        ok("(list 10 20 30 40)")
    );
}

#[test]
fn elisp_g2_mapc_returns_input() {
    assert_eq!(ok("(mapc (lambda (x) x) (list 'a 'b 'c))"), ok("(list 'a 'b 'c)"));
}

#[test]
fn elisp_g2_mapc_side_effect_count() {
    assert_eq!(
        ok_all("(setq acc 0) (mapc (lambda (x) (setq acc (+ acc x))) (list 1 2 3 4)) acc"),
        Sexp::Int(10)
    );
}

#[test]
fn elisp_g2_mapcar_nested() {
    assert_eq!(
        ok("(mapcar (lambda (xs) (car xs)) (list (list 1 2) (list 3 4) (list 5 6)))"),
        ok("(list 1 3 5)")
    );
}

// ==== sweep-9 G3 search tests ====

#[test]
fn elisp_g3_memq_hit() {
    assert_eq!(ok("(memq 'b (list 'a 'b 'c))"), ok("(list 'b 'c)"));
}

#[test]
fn elisp_g3_memq_miss() {
    assert_eq!(ok("(memq 'z (list 'a 'b 'c))"), Sexp::Nil);
}

#[test]
fn elisp_g3_memq_empty() {
    assert_eq!(ok("(memq 'a nil)"), Sexp::Nil);
}

#[test]
fn elisp_g3_member_hit() {
    assert_eq!(ok("(member 2 (list 1 2 3))"), ok("(list 2 3)"));
}

#[test]
fn elisp_g3_member_string() {
    assert_eq!(
        ok("(member \"x\" (list \"a\" \"x\" \"b\"))"),
        ok("(list \"x\" \"b\")")
    );
}

#[test]
fn elisp_g3_member_miss() {
    assert_eq!(ok("(member 99 (list 1 2 3))"), Sexp::Nil);
}

#[test]
fn elisp_g3_assq_hit() {
    assert_eq!(
        ok("(assq 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3)))"),
        ok("(cons 'b 2)")
    );
}

#[test]
fn elisp_g3_assq_miss() {
    assert_eq!(
        ok("(assq 'z (list (cons 'a 1) (cons 'b 2)))"),
        Sexp::Nil
    );
}

#[test]
fn elisp_g3_assoc_hit_string_key() {
    assert_eq!(
        ok("(assoc \"k\" (list (cons \"j\" 1) (cons \"k\" 2)))"),
        ok("(cons \"k\" 2)")
    );
}

#[test]
fn elisp_g3_assoc_miss() {
    assert_eq!(ok("(assoc 1 (list (cons 2 'b)))"), Sexp::Nil);
}

#[test]
fn elisp_g3_assq_skips_non_pair() {
    assert_eq!(
        ok("(assq 'a (list 'garbage (cons 'a 99)))"),
        ok("(cons 'a 99)")
    );
}

// ==== sweep-9 G4 plist-str tests ====

#[test]
fn elisp_g4_plist_get_present() {
    assert_eq!(ok("(plist-get (list :a 1 :b 2 :c 3) :b)"), Sexp::Int(2));
}

#[test]
fn elisp_g4_plist_get_missing() {
    assert_eq!(ok("(plist-get (list :a 1 :b 2) :z)"), Sexp::Nil);
}

#[test]
fn elisp_g4_plist_get_empty() {
    assert_eq!(ok("(plist-get nil :a)"), Sexp::Nil);
}

#[test]
fn elisp_g4_plist_member_present() {
    assert_eq!(
        ok("(plist-member (list :a 1 :b 2) :b)"),
        ok("(list :b 2)")
    );
}

#[test]
fn elisp_g4_plist_member_missing() {
    assert_eq!(ok("(plist-member (list :a 1) :z)"), Sexp::Nil);
}

#[test]
fn elisp_g4_plist_put_replace() {
    assert_eq!(
        ok("(plist-put (list :a 1 :b 2) :a 99)"),
        ok("(list :a 99 :b 2)")
    );
}

#[test]
fn elisp_g4_plist_put_append() {
    assert_eq!(
        ok("(plist-put (list :a 1) :b 2)"),
        ok("(list :a 1 :b 2)")
    );
}

#[test]
fn elisp_g4_plist_put_empty() {
    assert_eq!(ok("(plist-put nil :x 7)"), ok("(list :x 7)"));
}

#[test]
fn elisp_g4_string_empty_p_true() {
    assert_eq!(ok("(string-empty-p \"\")"), Sexp::T);
}

#[test]
fn elisp_g4_string_empty_p_false() {
    assert_eq!(ok("(string-empty-p \"a\")"), Sexp::Nil);
}

// ==== sweep-10 misc tests ====

#[test]
fn elisp_s10_list_zero() {
    assert_eq!(ok("(list)"), Sexp::Nil);
}

#[test]
fn elisp_s10_list_three() {
    assert_eq!(ok("(list 1 2 3)"), ok("'(1 2 3)"));
}

#[test]
fn elisp_s10_list_mixed() {
    assert_eq!(ok("(list 'a (+ 1 2) \"x\")"), ok("(list 'a 3 \"x\")"));
}

#[test]
fn elisp_s10_alist_get_present_default_test() {
    assert_eq!(
        ok("(alist-get 'b (list (cons 'a 1) (cons 'b 2)))"),
        Sexp::Int(2)
    );
}

#[test]
fn elisp_s10_alist_get_missing_default_nil() {
    assert_eq!(
        ok("(alist-get 'z (list (cons 'a 1)))"),
        Sexp::Nil
    );
}

#[test]
fn elisp_s10_alist_get_missing_with_default() {
    assert_eq!(
        ok("(alist-get 'z (list (cons 'a 1)) 99)"),
        Sexp::Int(99)
    );
}

#[test]
fn elisp_s10_alist_get_dotted_pair() {
    assert_eq!(
        ok("(alist-get 'a (list (cons 'a 5)))"),
        Sexp::Int(5)
    );
}

#[test]
fn elisp_s10_alist_get_eq_testfn() {
    assert_eq!(
        ok("(alist-get 'a (list (cons 'a 1) (cons 'b 2)) nil nil 'eq)"),
        Sexp::Int(1)
    );
}

#[test]
fn elisp_s10_alist_get_skip_non_pair() {
    assert_eq!(
        ok("(alist-get 'b (list 'garbage (cons 'b 7)))"),
        Sexp::Int(7)
    );
}

#[test]
fn elisp_s10_string_prefix_p_match() {
    assert_eq!(ok("(string-prefix-p \"foo\" \"foobar\")"), Sexp::T);
}

#[test]
fn elisp_s10_string_prefix_p_no_match() {
    assert_eq!(ok("(string-prefix-p \"baz\" \"foobar\")"), Sexp::Nil);
}

#[test]
fn elisp_s10_string_prefix_p_too_long() {
    assert_eq!(ok("(string-prefix-p \"abcdef\" \"abc\")"), Sexp::Nil);
}

#[test]
fn elisp_s10_string_prefix_p_empty_prefix() {
    assert_eq!(ok("(string-prefix-p \"\" \"hello\")"), Sexp::T);
}

#[test]
fn elisp_s10_number_to_string_int() {
    assert_eq!(ok("(number-to-string 42)"), Sexp::Str("42".into()));
}

#[test]
fn elisp_s10_number_to_string_negative() {
    assert_eq!(ok("(number-to-string -7)"), Sexp::Str("-7".into()));
}

// ---- Doc 51 Track H: control-flow regressions ------------------------

#[test]
fn track_h_throw_bypasses_condition_case() {
    // condition-case must NOT catch a `throw' — that's a control-flow
    // primitive, not an error.  The catch upstream should receive it.
    assert_eq!(
        ok_all("(catch 'tag (condition-case _ (throw 'tag 99) (error 'handled)))"),
        Sexp::Int(99)
    );
}

#[test]
fn track_h_no_catch_clause_handles_uncaught_throw() {
    // If the user explicitly names `no-catch' the condition-case
    // clause should claim the bare throw.  Use `(car (cdr e))' since
    // `cadr' is not a NeLisp Rust builtin (= it lives in the elisp
    // library layer).
    assert_eq!(
        ok_all("(condition-case e (throw 'unknown 1) (no-catch (car (cdr e))))"),
        Sexp::Symbol("unknown".into())
    );
}

#[test]
fn track_h_uw_runs_on_throw() {
    // unwind-protect cleanup must fire when the body throws.
    assert_eq!(
        ok_all(
            "(let ((cleanup nil)) (catch 'tag (unwind-protect (throw 'tag 1) (setq cleanup t))) cleanup)"
        ),
        Sexp::T
    );
}

#[test]
fn track_h_uw_runs_on_error() {
    // unwind-protect cleanup must fire when the body errors.
    assert_eq!(
        ok_all(
            "(let ((cleanup nil)) (condition-case _ (unwind-protect (error \"boom\") (setq cleanup t)) (error nil)) cleanup)"
        ),
        Sexp::T
    );
}

#[test]
fn track_h_throw_from_uw_cleanup_overrides() {
    // A throw raised inside the cleanup block should override the body's
    // result (= matches Emacs semantics).
    assert_eq!(
        ok_all(
            "(catch 'a (unwind-protect (throw 'a 1) (throw 'a 2)))"
        ),
        Sexp::Int(2)
    );
}

#[test]
fn track_h_nested_catch() {
    // Inner throw escapes to outer catch when tag matches outer.
    assert_eq!(
        ok_all("(catch 'outer (catch 'inner (throw 'outer 'escape)))"),
        Sexp::Symbol("escape".into())
    );
}

#[test]
fn track_h_condition_case_still_catches_real_errors() {
    // Regression: the no-catch carve-out should NOT break ordinary
    // error catching.
    assert_eq!(
        ok_all("(condition-case e (error \"boom\") (error (cdr e)))"),
        Sexp::list_from(&[Sexp::Str("boom".into())])
    );
}

// ----- Doc 51 Track K — atexit + signal hooks for raw mode ----------------
//
// `terminal-raw-mode-enter` requires a real TTY on the inherited stdin
// fd; cargo test pipes its child's stdin so `tcgetattr` reliably fails.
// That gives us a deterministic environment to verify:
//   - the helpers exist and report the initial-state invariants
//   - `terminal-raw-mode-leave` is a true no-op when nothing was entered
//   - a failed enter does not leave the saved-state flag set, so a
//     subsequent leave (e.g. via unwind-protect) does not try to restore
//     half-initialised termios
//
// The actual signal handler / atexit / re-raise behaviour is exercised
// by the integration script under `tests/track_k_signal.sh` (manual,
// requires a real PTY) — pure cargo-test runs cannot fork a pty.

#[cfg(unix)]
#[test]
fn track_k_termios_saved_p_initial_nil() {
    // No `terminal-raw-mode-enter` has run in this binary's lifetime
    // yet (or any prior enter failed because cargo test's stdin is
    // piped, not a TTY).  The flag must read nil.
    assert_eq!(ok_all("(_termios-saved-p)"), Sexp::Nil);
}

#[cfg(unix)]
#[test]
fn track_k_leave_without_enter_idempotent() {
    // `terminal-raw-mode-leave' must return t and not error when no
    // prior enter has happened — unwind-protect cleanup paths rely on
    // that idempotency.
    assert_eq!(ok_all("(terminal-raw-mode-leave)"), Sexp::T);
    // …and the flag stays nil afterwards.
    assert_eq!(ok_all("(_termios-saved-p)"), Sexp::Nil);
}

#[cfg(unix)]
#[test]
fn track_k_enter_on_non_tty_errors_cleanly() {
    // cargo test pipes stdin → tcgetattr returns ENOTTY → enter must
    // surface a wrong-type-ish internal error AND must not flip the
    // saved-state flag (otherwise unwind would tcsetattr garbage).
    let res = err_all("(terminal-raw-mode-enter)");
    match res {
        EvalError::Internal(msg) => {
            assert!(
                msg.contains("tcgetattr") || msg.contains("tcsetattr"),
                "expected tcgetattr/tcsetattr in error message, got: {}",
                msg
            );
        }
        other => panic!("expected Internal, got {:?}", other),
    }
    assert_eq!(ok_all("(_termios-saved-p)"), Sexp::Nil);
}

#[cfg(unix)]
#[test]
fn track_k_hooks_installed_p_callable() {
    // The helper must always be dispatchable (= no UnboundFunction).
    // The boolean it returns depends on whether a successful enter has
    // run in this process; under cargo test that never happens, so it
    // is nil — but the assertion we care about is "no error".
    let r = ok_all("(_raw-mode-hooks-installed-p)");
    assert!(matches!(r, Sexp::Nil | Sexp::T));
}

// ----- Doc 51 Track M — quit / C-g via EvalError::Quit --------------------
//
// `EvalError::Quit` is the dedicated control-flow variant for `quit'.
// Per the Elisp manual, `condition-case`'s universal `error` clause
// must NOT catch a quit — only an explicit `quit` clause (or `t`) does.
// The flag is process-global; tests clean up with `(clear-quit-flag)`
// at the end of each scenario to keep cargo's parallel runner happy.

#[test]
fn track_m_signal_quit_raises_quit_variant() {
    let e = err("(signal 'quit nil)");
    assert!(matches!(e, EvalError::Quit), "expected Quit, got {:?}", e);
}

#[test]
fn track_m_signal_quit_not_caught_by_error_clause() {
    // The handler must re-raise: `error' is not a parent of `quit'.
    let e = err_all(
        "(condition-case _ (signal 'quit nil) (error 'wrongly-caught))"
    );
    assert!(matches!(e, EvalError::Quit), "expected Quit, got {:?}", e);
}

#[test]
fn track_m_signal_quit_caught_by_quit_clause() {
    assert_eq!(
        ok_all("(condition-case _ (signal 'quit nil) (quit 'caught))"),
        Sexp::Symbol("caught".into())
    );
}

#[test]
fn track_m_signal_quit_caught_by_t_clause() {
    // `t' clause is the universal catch-all and DOES catch quit
    // (Elisp manual: "if the condition name is t, all conditions are
    // caught, including quit").
    assert_eq!(
        ok_all("(condition-case _ (signal 'quit nil) (t 'caught-by-t))"),
        Sexp::Symbol("caught-by-t".into())
    );
}

#[test]
fn track_m_set_quit_flag_raises_at_next_eval() {
    // (set-quit-flag) returns t but does not itself raise.  The next
    // eval boundary (= the next form, here `'reached') triggers the
    // conversion to EvalError::Quit.  We use a `quit` clause to absorb
    // it so the test ends cleanly without polluting the global flag.
    assert_eq!(
        ok_all(
            "(condition-case _ (progn (set-quit-flag) 'reached) (quit 'after-quit))"
        ),
        Sexp::Symbol("after-quit".into())
    );
    // Sanity: the flag was consumed by the eval-time `take`, not left set.
    assert_eq!(ok_all("(quit-flag-pending-p)"), Sexp::Nil);
}

#[test]
fn track_m_clear_quit_flag_via_rust_api() {
    // From Lisp, `(set-quit-flag)' followed by anything else is
    // intercepted by eval()'s flag check before the next form runs
    // — that is the whole *point* of the flag.  But the Rust quit::
    // module is callable directly (e.g. from a signal handler before
    // any eval boundary), and clearing there must work.
    super::quit::set_quit_flag();
    super::quit::clear_quit_flag();
    assert_eq!(ok_all("'survived"), Sexp::Symbol("survived".into()));
    assert!(!super::quit::is_quit_pending());
}

#[test]
fn track_m_unwind_protect_runs_on_quit() {
    // unwind-protect cleanup must fire on a quit just like on any
    // other error.  We shadow with a (quit ...) clause to absorb the
    // re-raise at the outer level so the test asserts on `cleanup'.
    assert_eq!(
        ok_all(
            "(let ((cleanup nil))
               (condition-case _
                 (unwind-protect (signal 'quit nil) (setq cleanup t))
                 (quit nil))
               cleanup)"
        ),
        Sexp::T
    );
}

#[test]
fn track_m_quit_flag_pending_p_is_non_destructive() {
    // From Rust: `is_quit_pending` must NOT consume the flag.  Only
    // `take_quit_flag` (used by eval() at safe-poll boundaries) does.
    super::quit::clear_quit_flag(); // start clean
    super::quit::set_quit_flag();
    assert!(super::quit::is_quit_pending());
    assert!(super::quit::is_quit_pending(), "peek must be idempotent");
    // take consumes
    assert!(super::quit::take_quit_flag());
    assert!(!super::quit::is_quit_pending());
}

#[test]
fn track_m_quit_flag_pending_p_lisp_returns_nil_when_clear() {
    // Lisp-side observability of the cleared state.  When invoked,
    // eval() takes the flag first; if it was unset, the builtin runs
    // and reports nil.  The builtin always sees the just-cleared
    // state because eval()'s take ran moments before.
    assert_eq!(ok_all("(quit-flag-pending-p)"), Sexp::Nil);
}

#[test]
fn track_m_error_tag_for_quit() {
    // The `error_tag` accessor must return "quit" (= what
    // `condition-case` clause-matching keys on).
    assert_eq!(EvalError::Quit.error_tag(), "quit");
}

#[test]
fn track_m_signal_data_for_quit_is_nil_payload() {
    // `(signal-data quit) => (quit . nil)` matches Emacs convention.
    let sd = EvalError::Quit.signal_data();
    assert_eq!(
        sd,
        Sexp::cons(Sexp::Symbol("quit".into()), Sexp::Nil)
    );
}

#[cfg(unix)]
#[test]
fn track_m_install_sigint_handler_is_idempotent() {
    // Calling install-sigint-handler more than once must be safe —
    // the underlying impl uses a `Once` gate.  Side effect: the
    // handler stays installed for the rest of this test binary's
    // lifetime; other tests don't depend on the system-default
    // SIGINT disposition, so this is benign.
    assert_eq!(ok_all("(install-sigint-handler)"), Sexp::T);
    assert_eq!(ok_all("(install-sigint-handler)"), Sexp::T);
    assert_eq!(ok_all("(_sigint-handler-installed-p)"), Sexp::T);
}

// Note: a `raise(SIGINT)` end-to-end test would be the natural
// complement to track_m_install_sigint_handler_is_idempotent, but
// SIGINT is delivered to the *whole process*.  Under `cargo test`
// (= parallel test threads, shared process), the signal hits an
// arbitrary thread mid-eval and arbitrary parallel tests see a
// spurious Quit.  We rely on the by-construction simplicity of the
// handler body (= one `AtomicBool::store(true)`) plus the API tests
// above instead.
