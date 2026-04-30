//! Doc 47 Stage 9e — AST → ChainOp translator.
//!
//! Stage 9d let drivers describe a function body as a `ChainOp` slice
//! (= textual `OPS' spec).  Stage 9e lifts the build-tool one rung
//! higher: walks an Elisp `(lambda (PARAM) BODY)' Sexp and synthesises
//! the ChainOp sequence directly from the AST.  The driver no longer
//! mediates the chain shape — only the lambda source.
//!
//! Supported BODY shape (walking-skeleton):
//!   - bare PARAM symbol         (= identity lambda → [LoadHeapHead])
//!   - `(+ EXPR INT [INT ...])`  (variadic add of integer constants)
//!   - `(- EXPR INT [INT ...])`  (variadic subtract of integer constants)
//!   - `(* EXPR INT [INT ...])`  (variadic multiply by integer constants)
//!   - `(- EXPR)`                (unary negate)
//!   - `(+ INT EXPR)` / `(* INT EXPR)`
//!         (constant-then-chain forms for commutative operators)
//!   - any of the above where EXPR is itself a supported chain shape
//!     (= recursive composition; the param appears exactly once, and
//!     every other operand is an integer literal)
//!
//! Out of scope (errors with a descriptive message): conditionals,
//! `let`, calls, multi-parameter lambdas, non-integer literals, second
//! reference to PARAM in the same body.
//!
//! The translator never emits `LoadHeapHead' more than once nor a
//! trailing `Ret'.  [`translate_lambda`] is the public entry that adds
//! the `Ret' suffix; callers feed the result straight to
//! [`crate::native_emit::compose`].

use crate::native_emit::ChainOp;
use crate::reader::Sexp;

/// Top-level entry: walk a `(lambda (PARAM) BODY)' Sexp and return the
/// full ChainOp sequence (LoadHeapHead included via the body's first
/// PARAM reference; trailing Ret appended by us).
pub fn translate_lambda(form: &Sexp) -> Result<Vec<ChainOp>, String> {
    let head = form.cons_car();
    match &head {
        Sexp::Symbol(s) if s == "lambda" => {}
        _ => return Err(format!("translate_lambda: not a lambda form (head={:?})", head)),
    }
    let rest = form.cons_cdr();
    let params_form = rest.cons_car();
    let body_list = rest.cons_cdr();

    // (PARAM) — exactly one symbol, then nil.
    let params = collect_proper_list(&params_form)
        .map_err(|e| format!("translate_lambda: param list malformed: {}", e))?;
    if params.len() != 1 {
        return Err(format!(
            "translate_lambda: only single-arg lambdas are supported in Stage 9e (got {} params)",
            params.len()
        ));
    }
    let param = match &params[0] {
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(format!(
                "translate_lambda: parameter must be a symbol, got {:?}",
                other
            ))
        }
    };

    // Body must be exactly one form for the walking-skeleton.
    let body_forms = collect_proper_list(&body_list)
        .map_err(|e| format!("translate_lambda: body list malformed: {}", e))?;
    if body_forms.len() != 1 {
        return Err(format!(
            "translate_lambda: exactly one body form supported in Stage 9e (got {} forms)",
            body_forms.len()
        ));
    }

    let mut chain = translate_expr(&body_forms[0], &param)?;
    chain.push(ChainOp::Ret);
    Ok(chain)
}

/// Walk one expression `EXPR` against the bound `param` symbol.  The
/// returned chain produces EXPR's value in the accumulator; if EXPR
/// references `param` it begins with a `LoadHeapHead' op (emitted by
/// the leaf case below).
fn translate_expr(expr: &Sexp, param: &str) -> Result<Vec<ChainOp>, String> {
    match expr {
        Sexp::Symbol(s) if s == param => Ok(vec![ChainOp::LoadHeapHead]),
        Sexp::Symbol(s) => Err(format!(
            "unknown symbol {:?} (only param {:?} is bound in Stage 9e)",
            s, param
        )),
        Sexp::Int(_) => Err(
            "constant body without parameter reference unsupported in Stage 9e".into(),
        ),
        Sexp::Cons(_, _) => translate_call(expr, param),
        other => Err(format!("unsupported expression shape: {:?}", other)),
    }
}

/// Translate a `(OP arg1 arg2 ...)' call form.  Recognised heads:
///   `+` / `-` / `*`     — arithmetic chain ops (Stage 9e)
///   `if`                — Stage 9f structured branch with `(< PARAM IMM)' guard
fn translate_call(call: &Sexp, param: &str) -> Result<Vec<ChainOp>, String> {
    let head = call.cons_car();
    let op_name = match head {
        Sexp::Symbol(s) => s,
        other => return Err(format!("call head not a symbol: {:?}", other)),
    };
    let args_form = call.cons_cdr();
    let args = collect_proper_list(&args_form)
        .map_err(|e| format!("call args malformed: {}", e))?;

    if op_name == "if" {
        return translate_if(&args, param);
    }

    match op_name.as_str() {
        "+" | "-" | "*" => {}
        other => return Err(format!("unsupported operator {:?}", other)),
    }

    // Unary `(- EXPR)' = negate.
    if op_name == "-" && args.len() == 1 {
        let mut chain = translate_expr(&args[0], param)?;
        chain.push(ChainOp::Neg);
        return Ok(chain);
    }

    if args.is_empty() {
        return Err(format!("zero-arg `{}` not supported in Stage 9e", op_name));
    }

    // Locate the chain-bearing operand.  Exactly one operand may
    // reference `param` (the chain root); the rest must be integer
    // literals (used as immediates for AddImm/SubImm/MulImm).  For non-
    // commutative `-' the chain operand must be first.
    let mut chain_idx: Option<usize> = None;
    for (i, a) in args.iter().enumerate() {
        if expr_contains_param(a, param) {
            if chain_idx.is_some() {
                return Err(format!(
                    "operator `{}` references param {:?} more than once \
                     (Stage 9e supports at most one chain root per call)",
                    op_name, param
                ));
            }
            chain_idx = Some(i);
        }
    }
    let chain_idx = match chain_idx {
        Some(i) => i,
        None => {
            return Err(format!(
                "operator `{}` has no reference to param {:?} \
                 — body must be a chain on the lambda parameter",
                op_name, param
            ))
        }
    };
    if op_name == "-" && chain_idx != 0 {
        return Err(
            "non-commutative `-` requires the chain operand to be first \
             (Stage 9e refuses to constant-fold arg reorderings)"
                .into(),
        );
    }

    let mut chain = translate_expr(&args[chain_idx], param)?;
    for (i, a) in args.iter().enumerate() {
        if i == chain_idx {
            continue;
        }
        let imm = expect_i32(a)?;
        chain.push(match op_name.as_str() {
            "+" => ChainOp::AddImm(imm),
            "-" => ChainOp::SubImm(imm),
            "*" => ChainOp::MulImm(imm),
            _ => unreachable!("operator filter above"),
        });
    }
    Ok(chain)
}

/// Stage 9f — translate `(if (< PARAM IMM) THEN ELSE)`.  Walking-skeleton
/// guard form is exactly `< PARAM IMM`; THEN and ELSE are themselves
/// arbitrary Stage 9e/9f chains over PARAM, so nested ifs work via
/// recursion.  Both branches must be non-empty (= reference PARAM at
/// least via a `LoadHeapHead' from a leaf or sub-chain).
fn translate_if(args: &[Sexp], param: &str) -> Result<Vec<ChainOp>, String> {
    if args.len() != 3 {
        return Err(format!(
            "if: expected 3 args (cond, then, else), got {}",
            args.len()
        ));
    }
    let cond = &args[0];
    let then_form = &args[1];
    let else_form = &args[2];

    let threshold = match cond {
        Sexp::Cons(_, _) => {
            let chead = cond.cons_car();
            let cargs = collect_proper_list(&cond.cons_cdr())
                .map_err(|e| format!("if cond args malformed: {}", e))?;
            match (chead, cargs.as_slice()) {
                (Sexp::Symbol(ref s), [lhs, rhs]) if s == "<" => {
                    match lhs {
                        Sexp::Symbol(p) if p == param => {}
                        other => {
                            return Err(format!(
                                "if cond: lhs of `<` must be param `{}` in Stage 9f \
                                 (got {:?})",
                                param, other
                            ))
                        }
                    }
                    expect_i32(rhs)?
                }
                _ => {
                    return Err(
                        "if: cond must be `(< PARAM IMM)` in Stage 9f \
                         (other predicates / orderings are reserved for later stages)"
                            .into(),
                    )
                }
            }
        }
        _ => {
            return Err(
                "if: cond must be a `(< PARAM IMM)` form in Stage 9f".into(),
            )
        }
    };

    let then_chain = translate_expr(then_form, param)?;
    let else_chain = translate_expr(else_form, param)?;
    Ok(vec![ChainOp::IfLtImm {
        threshold,
        then_chain,
        else_chain,
    }])
}

/// Walk a Sexp list (any depth) checking whether `param` is referenced
/// anywhere inside.  Used to decide which call argument is the chain
/// root in [`translate_call`].
fn expr_contains_param(expr: &Sexp, param: &str) -> bool {
    match expr {
        Sexp::Symbol(s) => s == param,
        Sexp::Cons(_, _) => {
            let mut cur = expr.clone();
            loop {
                match cur {
                    Sexp::Nil => return false,
                    Sexp::Cons(_, _) => {
                        if expr_contains_param(&cur.cons_car(), param) {
                            return true;
                        }
                        cur = cur.cons_cdr();
                    }
                    // Improper list tail — treat as leaf and check it.
                    other => return matches!(other, Sexp::Symbol(ref s) if s == param),
                }
            }
        }
        _ => false,
    }
}

/// Materialise a proper Lisp list as a `Vec<Sexp>'.  Errors on dotted /
/// improper lists since Stage 9e never emits them.
fn collect_proper_list(form: &Sexp) -> Result<Vec<Sexp>, String> {
    let mut out = Vec::new();
    let mut cur = form.clone();
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(_, _) => {
                out.push(cur.cons_car());
                cur = cur.cons_cdr();
            }
            other => {
                return Err(format!(
                    "expected proper list, found dotted tail {:?}",
                    other
                ))
            }
        }
    }
}

/// Extract an i32 literal from `expr`; errors on non-Int or out-of-i32-range.
fn expect_i32(expr: &Sexp) -> Result<i32, String> {
    match expr {
        Sexp::Int(n) => {
            if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                Ok(*n as i32)
            } else {
                Err(format!("integer literal {} out of i32 range", n))
            }
        }
        other => Err(format!(
            "expected integer literal, got {:?} \
             (Stage 9e immediates must be i32-range constants)",
            other
        )),
    }
}

/// Whether the AST translator can emit on this target.  Mirrors
/// [`crate::native_emit::HAS_CHAIN_OP_COMPOSE`] so callers can fail
/// fast with a single flag.
pub const HAS_AST_TRANSLATE: bool = crate::native_emit::HAS_CHAIN_OP_COMPOSE;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::reader::read_str;

    fn translate(src: &str) -> Result<Vec<ChainOp>, String> {
        let lambda = read_str(src).map_err(|e| format!("read error: {}", e))?;
        translate_lambda(&lambda)
    }

    #[test]
    fn identity_lambda_emits_load_then_ret() {
        let ops = translate("(lambda (n) n)").unwrap();
        assert_eq!(ops, vec![ChainOp::LoadHeapHead, ChainOp::Ret]);
    }

    #[test]
    fn add_immediate() {
        let ops = translate("(lambda (n) (+ n 5))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::AddImm(5), ChainOp::Ret]
        );
    }

    #[test]
    fn sub_immediate() {
        let ops = translate("(lambda (n) (- n 7))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::SubImm(7), ChainOp::Ret]
        );
    }

    #[test]
    fn mul_immediate() {
        let ops = translate("(lambda (n) (* n 3))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::MulImm(3), ChainOp::Ret]
        );
    }

    #[test]
    fn unary_negate() {
        let ops = translate("(lambda (n) (- n))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::Neg, ChainOp::Ret]
        );
    }

    #[test]
    fn nested_mul_then_add() {
        // (* n 3) → AddImm(5)
        let ops = translate("(lambda (n) (+ (* n 3) 5))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::MulImm(3),
                ChainOp::AddImm(5),
                ChainOp::Ret
            ]
        );
    }

    #[test]
    fn nested_add_then_mul() {
        let ops = translate("(lambda (n) (* (+ n 1) 2))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::AddImm(1),
                ChainOp::MulImm(2),
                ChainOp::Ret
            ]
        );
    }

    #[test]
    fn variadic_add_collapses_to_chain() {
        let ops = translate("(lambda (n) (+ n 3 -1 5))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::AddImm(3),
                ChainOp::AddImm(-1),
                ChainOp::AddImm(5),
                ChainOp::Ret
            ]
        );
    }

    #[test]
    fn variadic_sub_collapses_to_chain() {
        let ops = translate("(lambda (n) (- n 1 2 3))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::SubImm(1),
                ChainOp::SubImm(2),
                ChainOp::SubImm(3),
                ChainOp::Ret
            ]
        );
    }

    #[test]
    fn commutative_constant_first_add() {
        // (+ 5 n) is equivalent to (+ n 5) — handled.
        let ops = translate("(lambda (n) (+ 5 n))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::AddImm(5), ChainOp::Ret]
        );
    }

    #[test]
    fn commutative_constant_first_mul() {
        let ops = translate("(lambda (n) (* 4 n))").unwrap();
        assert_eq!(
            ops,
            vec![ChainOp::LoadHeapHead, ChainOp::MulImm(4), ChainOp::Ret]
        );
    }

    #[test]
    fn rejects_non_commutative_constant_first() {
        let err = translate("(lambda (n) (- 10 n))").unwrap_err();
        assert!(err.contains("non-commutative"), "err: {}", err);
    }

    #[test]
    fn rejects_two_param_lambda() {
        let err = translate("(lambda (a b) (+ a b))").unwrap_err();
        assert!(err.contains("single-arg"), "err: {}", err);
    }

    #[test]
    fn rejects_non_integer_constant() {
        let err = translate("(lambda (n) (+ n 1.5))").unwrap_err();
        assert!(err.contains("integer literal"), "err: {}", err);
    }

    #[test]
    fn rejects_unknown_operator() {
        let err = translate("(lambda (n) (/ n 2))").unwrap_err();
        assert!(err.contains("unsupported operator"), "err: {}", err);
    }

    #[test]
    fn rejects_unbound_symbol_as_operand() {
        // `(+ n m)' — `n' is the chain root, `m' isn't param so the
        // translator demands an integer literal in that slot and
        // surfaces the type-mismatch error.  Either error wording is
        // acceptable; the contract is just "do not silently accept".
        let err = translate("(lambda (n) (+ n m))").unwrap_err();
        assert!(
            err.contains("integer literal") || err.contains("unknown symbol"),
            "err: {}",
            err
        );
    }

    #[test]
    fn rejects_unbound_symbol_in_unary_neg() {
        // `(- m)' — only operand is unbound, no chain found; surfaces
        // the unknown-symbol error from translate_expr.
        let err = translate("(lambda (n) (- m))").unwrap_err();
        assert!(err.contains("unknown symbol"), "err: {}", err);
    }

    #[test]
    fn rejects_constant_body_without_param() {
        let err = translate("(lambda (n) 42)").unwrap_err();
        assert!(err.contains("constant body"), "err: {}", err);
    }

    #[test]
    fn rejects_multi_form_body() {
        let err = translate("(lambda (n) n n)").unwrap_err();
        assert!(err.contains("one body form"), "err: {}", err);
    }

    #[test]
    fn rejects_double_param_reference() {
        // `(+ n n)' uses n twice — Stage 9e refuses; would need
        // accumulator save/restore which we don't have.
        let err = translate("(lambda (n) (+ n n))").unwrap_err();
        assert!(err.contains("more than once"), "err: {}", err);
    }

    #[test]
    fn complex_chain_full_arithmetic() {
        // (+ (* (- n 1) 5) 7) on n=4: ((4-1)*5)+7 = 22
        let ops = translate("(lambda (n) (+ (* (- n 1) 5) 7))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::SubImm(1),
                ChainOp::MulImm(5),
                ChainOp::AddImm(7),
                ChainOp::Ret
            ]
        );
    }

    // ============================================================
    // Stage 9f — if-branch translation
    // ============================================================

    #[test]
    fn if_lt_basic_doubles_else() {
        let ops = translate("(lambda (n) (if (< n 2) n (* n 2)))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::IfLtImm {
                    threshold: 2,
                    then_chain: vec![ChainOp::LoadHeapHead],
                    else_chain: vec![ChainOp::LoadHeapHead, ChainOp::MulImm(2)],
                },
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn if_lt_abs_value_via_negate() {
        // (lambda (n) (if (< n 0) (- n) n))
        let ops = translate("(lambda (n) (if (< n 0) (- n) n))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::IfLtImm {
                    threshold: 0,
                    then_chain: vec![ChainOp::LoadHeapHead, ChainOp::Neg],
                    else_chain: vec![ChainOp::LoadHeapHead],
                },
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn if_lt_nested_else_branch() {
        // (lambda (n) (if (< n 2) n (if (< n 5) (* n 2) (* n 3))))
        let ops = translate("(lambda (n) (if (< n 2) n (if (< n 5) (* n 2) (* n 3))))")
            .unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::IfLtImm {
                    threshold: 2,
                    then_chain: vec![ChainOp::LoadHeapHead],
                    else_chain: vec![ChainOp::IfLtImm {
                        threshold: 5,
                        then_chain: vec![ChainOp::LoadHeapHead, ChainOp::MulImm(2)],
                        else_chain: vec![ChainOp::LoadHeapHead, ChainOp::MulImm(3)],
                    }],
                },
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn if_with_arithmetic_then_branch() {
        // (lambda (n) (if (< n 10) (+ n 100) (- n 50)))
        let ops = translate("(lambda (n) (if (< n 10) (+ n 100) (- n 50)))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::IfLtImm {
                    threshold: 10,
                    then_chain: vec![ChainOp::LoadHeapHead, ChainOp::AddImm(100)],
                    else_chain: vec![ChainOp::LoadHeapHead, ChainOp::SubImm(50)],
                },
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn if_with_negative_threshold() {
        let ops = translate("(lambda (n) (if (< n -3) (- n) n))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::IfLtImm {
                    threshold: -3,
                    then_chain: vec![ChainOp::LoadHeapHead, ChainOp::Neg],
                    else_chain: vec![ChainOp::LoadHeapHead],
                },
                ChainOp::Ret,
            ]
        );
    }

    #[test]
    fn rejects_if_with_non_param_lhs() {
        let err = translate("(lambda (n) (if (< 5 n) n n))").unwrap_err();
        assert!(err.contains("lhs of `<` must be param"), "err: {}", err);
    }

    #[test]
    fn rejects_if_with_unsupported_predicate() {
        let err = translate("(lambda (n) (if (= n 0) n (* n 2)))").unwrap_err();
        assert!(err.contains("(< PARAM IMM)"), "err: {}", err);
    }

    #[test]
    fn rejects_if_with_wrong_arity() {
        let err = translate("(lambda (n) (if (< n 5) n))").unwrap_err();
        assert!(err.contains("3 args"), "err: {}", err);
    }

    #[test]
    fn rejects_if_with_non_int_threshold() {
        let err = translate("(lambda (n) (if (< n 1.5) n (* n 2)))").unwrap_err();
        assert!(err.contains("integer literal"), "err: {}", err);
    }

    #[test]
    fn neg_then_add() {
        // (+ (- n) 10) → load n, neg, add 10
        let ops = translate("(lambda (n) (+ (- n) 10))").unwrap();
        assert_eq!(
            ops,
            vec![
                ChainOp::LoadHeapHead,
                ChainOp::Neg,
                ChainOp::AddImm(10),
                ChainOp::Ret
            ]
        );
    }
}
