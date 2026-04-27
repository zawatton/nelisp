//! Built-in function registry — Phase 8.0.2 (Doc 44 §3.3 LOCKED).
//!
//! Each built-in is registered in [`install_builtins`] which writes a
//! `(builtin <NAME>)` sentinel into the function cell of the symbol.
//! At call time the evaluator pulls the name back out and routes to
//! [`dispatch`].
//!
//! Categories (per prompt §6):
//!   - Arithmetic  : + - * / mod 1+ 1- < > <= >= = /=
//!   - Equality    : eq equal
//!   - Cons / list : car cdr cons list nth length nthcdr nreverse
//!                   reverse append
//!   - Higher-order: mapcar mapc memq member assq assoc
//!   - Predicates  : null consp listp atom symbolp stringp numberp
//!                   integerp floatp not
//!   - String      : concat format substring length intern symbol-name
//!   - Symbol/func : symbol-value symbol-function fboundp boundp
//!                   funcall apply eval signal error
//!
//! Total registered: ~60 (counts above + a handful of glue helpers
//! such as `print`, `princ`, `message`, `identity`).

use super::env::Env;
use super::error::EvalError;
use super::sexp::Sexp;
use super::special_forms::{is_truthy, sexp_eq};

/// Install every built-in into the given environment.  Idempotent —
/// re-running just overwrites the function cells.
pub fn install_builtins(env: &mut Env) {
    let names: &[&str] = &[
        // arithmetic
        "+", "-", "*", "/", "mod", "1+", "1-", "<", ">", "<=", ">=", "=", "/=",
        // equality
        "eq", "equal",
        // cons / list
        "car", "cdr", "cons", "list", "nth", "length", "nthcdr", "nreverse", "reverse", "append",
        // higher-order
        "mapcar", "mapc", "memq", "member", "assq", "assoc",
        // predicates
        "null", "consp", "listp", "atom", "symbolp", "stringp", "numberp", "integerp", "floatp",
        "not", "functionp",
        // string
        "concat", "format", "substring", "intern", "symbol-name", "string-equal", "string=",
        // symbol / function
        "symbol-value", "symbol-function", "fboundp", "boundp", "funcall", "apply", "eval",
        "signal", "error", "identity", "print", "princ", "prin1-to-string", "message",
    ];
    for n in names {
        let sentinel = Sexp::list_from(&[
            Sexp::Symbol("builtin".into()),
            Sexp::Symbol((*n).into()),
        ]);
        env.set_function(n, sentinel);
    }
}

/// Dispatch a built-in by name.  Called from `super::apply_builtin`.
pub fn dispatch(name: &str, args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    match name {
        // ---- arithmetic ----
        "+" => bi_add(args),
        "-" => bi_sub(args),
        "*" => bi_mul(args),
        "/" => bi_div(args),
        "mod" => bi_mod(args),
        "1+" => bi_inc(args),
        "1-" => bi_dec(args),
        "<" => bi_lt(args),
        ">" => bi_gt(args),
        "<=" => bi_le(args),
        ">=" => bi_ge(args),
        "=" => bi_eq_num(args),
        "/=" => bi_neq_num(args),
        // ---- equality ----
        "eq" => bi_eq(args),
        "equal" => bi_equal(args),
        // ---- cons / list ----
        "car" => bi_car(args),
        "cdr" => bi_cdr(args),
        "cons" => bi_cons(args),
        "list" => Ok(Sexp::list_from(args)),
        "nth" => bi_nth(args),
        "length" => bi_length(args),
        "nthcdr" => bi_nthcdr(args),
        "nreverse" | "reverse" => bi_reverse(args),
        "append" => bi_append(args),
        // ---- higher-order ----
        "mapcar" => bi_mapcar(args, env),
        "mapc" => bi_mapc(args, env),
        "memq" => bi_memq(args),
        "member" => bi_member(args),
        "assq" => bi_assq(args),
        "assoc" => bi_assoc(args),
        // ---- predicates ----
        "null" | "not" => bi_null(args),
        "consp" => bi_predicate(args, |v| matches!(v, Sexp::Cons(_, _))),
        "listp" => bi_predicate(args, |v| matches!(v, Sexp::Cons(_, _) | Sexp::Nil)),
        "atom" => bi_predicate(args, |v| !matches!(v, Sexp::Cons(_, _))),
        "symbolp" => bi_predicate(args, |v| matches!(v, Sexp::Symbol(_) | Sexp::Nil | Sexp::T)),
        "stringp" => bi_predicate(args, |v| matches!(v, Sexp::Str(_))),
        "numberp" => bi_predicate(args, |v| matches!(v, Sexp::Int(_) | Sexp::Float(_))),
        "integerp" => bi_predicate(args, |v| matches!(v, Sexp::Int(_))),
        "floatp" => bi_predicate(args, |v| matches!(v, Sexp::Float(_))),
        "functionp" => bi_predicate(args, |v| matches!(v,
            Sexp::Cons(h, _) if matches!(h.as_ref(),
                Sexp::Symbol(s) if s == "lambda" || s == "closure" || s == "builtin"))),
        // ---- string ----
        "concat" => bi_concat(args),
        "format" => bi_format(args),
        "substring" => bi_substring(args),
        "intern" => bi_intern(args),
        "symbol-name" => bi_symbol_name(args),
        "string-equal" | "string=" => bi_string_eq(args),
        // ---- symbol / function ----
        "symbol-value" => bi_symbol_value(args, env),
        "symbol-function" => bi_symbol_function(args, env),
        "fboundp" => bi_fboundp(args, env),
        "boundp" => bi_boundp(args, env),
        "funcall" => bi_funcall(args, env),
        "apply" => bi_apply(args, env),
        "eval" => bi_eval(args, env),
        "signal" => bi_signal(args),
        "error" => bi_error(args),
        "identity" => bi_identity(args),
        "print" | "princ" => bi_princ(args),
        "prin1-to-string" => bi_prin1_to_string(args),
        "message" => bi_princ(args),
        _ => Err(EvalError::UnboundFunction(name.to_string())),
    }
}

// ---------- arity helpers ----------

fn require_arity(name: &str, args: &[Sexp], min: usize, max: Option<usize>) -> Result<(), EvalError> {
    if args.len() < min || max.map_or(false, |m| args.len() > m) {
        let expected = match max {
            Some(m) if m == min => format!("{}", min),
            Some(m) => format!("{}-{}", min, m),
            None => format!("≥{}", min),
        };
        return Err(EvalError::WrongNumberOfArguments {
            function: name.into(),
            expected,
            got: args.len(),
        });
    }
    Ok(())
}

fn as_int(name: &str, v: &Sexp) -> Result<i64, EvalError> {
    match v {
        Sexp::Int(n) => Ok(*n),
        Sexp::Float(x) => Ok(*x as i64),
        other => Err(EvalError::WrongType {
            expected: format!("number ({} arg)", name),
            got: other.clone(),
        }),
    }
}

/// Numeric promotion: if any input is float, output is float.
fn numeric_promote(args: &[Sexp]) -> Result<(bool, Vec<f64>), EvalError> {
    let mut any_float = false;
    let mut out = Vec::with_capacity(args.len());
    for a in args {
        match a {
            Sexp::Int(n) => out.push(*n as f64),
            Sexp::Float(x) => {
                any_float = true;
                out.push(*x);
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "number".into(),
                    got: other.clone(),
                })
            }
        }
    }
    Ok((any_float, out))
}

fn pack_number(any_float: bool, x: f64) -> Sexp {
    if any_float {
        Sexp::Float(x)
    } else {
        Sexp::Int(x as i64)
    }
}

// ---------- arithmetic implementations ----------

fn bi_add(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (af, vs) = numeric_promote(args)?;
    let s = vs.iter().sum();
    Ok(pack_number(af, s))
}

fn bi_sub(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Ok(Sexp::Int(0));
    }
    let (af, vs) = numeric_promote(args)?;
    if vs.len() == 1 {
        return Ok(pack_number(af, -vs[0]));
    }
    let mut acc = vs[0];
    for v in vs.iter().skip(1) {
        acc -= v;
    }
    Ok(pack_number(af, acc))
}

fn bi_mul(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (af, vs) = numeric_promote(args)?;
    let p = vs.iter().product();
    Ok(pack_number(af, p))
}

fn bi_div(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("/", args, 1, None)?;
    let (af, vs) = numeric_promote(args)?;
    if vs.len() == 1 {
        if vs[0] == 0.0 {
            return Err(EvalError::ArithError("division by zero".into()));
        }
        return Ok(pack_number(af, 1.0 / vs[0]));
    }
    let mut acc = vs[0];
    for v in vs.iter().skip(1) {
        if *v == 0.0 {
            return Err(EvalError::ArithError("division by zero".into()));
        }
        acc /= v;
    }
    if !af {
        // Integer truncation for all-int inputs.
        Ok(Sexp::Int(acc.trunc() as i64))
    } else {
        Ok(Sexp::Float(acc))
    }
}

fn bi_mod(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("mod", args, 2, Some(2))?;
    let a = as_int("mod", &args[0])?;
    let b = as_int("mod", &args[1])?;
    if b == 0 {
        return Err(EvalError::ArithError("mod by zero".into()));
    }
    // Elisp `mod` is floor-mod (result has sign of divisor).
    let r = a.rem_euclid(b.abs());
    let signed = if b < 0 { -r } else { r };
    Ok(Sexp::Int(signed))
}

fn bi_inc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("1+", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(n) => Ok(Sexp::Int(n + 1)),
        Sexp::Float(x) => Ok(Sexp::Float(x + 1.0)),
        other => Err(EvalError::WrongType {
            expected: "number".into(),
            got: other.clone(),
        }),
    }
}

fn bi_dec(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("1-", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Int(n) => Ok(Sexp::Int(n - 1)),
        Sexp::Float(x) => Ok(Sexp::Float(x - 1.0)),
        other => Err(EvalError::WrongType {
            expected: "number".into(),
            got: other.clone(),
        }),
    }
}

fn cmp_vararg(name: &str, args: &[Sexp], cmp: fn(f64, f64) -> bool) -> Result<Sexp, EvalError> {
    require_arity(name, args, 2, None)?;
    let (_, vs) = numeric_promote(args)?;
    for w in vs.windows(2) {
        if !cmp(w[0], w[1]) {
            return Ok(Sexp::Nil);
        }
    }
    Ok(Sexp::T)
}

fn bi_lt(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("<", args, |a, b| a < b)
}
fn bi_gt(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg(">", args, |a, b| a > b)
}
fn bi_le(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("<=", args, |a, b| a <= b)
}
fn bi_ge(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg(">=", args, |a, b| a >= b)
}
fn bi_eq_num(args: &[Sexp]) -> Result<Sexp, EvalError> {
    cmp_vararg("=", args, |a, b| (a - b).abs() < 1e-15)
}
fn bi_neq_num(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("/=", args, 2, Some(2))?;
    let (_, vs) = numeric_promote(args)?;
    Ok(if (vs[0] - vs[1]).abs() < 1e-15 {
        Sexp::Nil
    } else {
        Sexp::T
    })
}

// ---------- equality ----------

fn bi_eq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("eq", args, 2, Some(2))?;
    Ok(if sexp_eq(&args[0], &args[1]) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_equal(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("equal", args, 2, Some(2))?;
    Ok(if args[0] == args[1] { Sexp::T } else { Sexp::Nil })
}

// ---------- cons / list ----------

fn bi_car(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("car", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Nil),
        Sexp::Cons(a, _) => Ok((**a).clone()),
        other => Err(EvalError::WrongType {
            expected: "listp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_cdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("cdr", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Nil),
        Sexp::Cons(_, d) => Ok((**d).clone()),
        other => Err(EvalError::WrongType {
            expected: "listp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_cons(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("cons", args, 2, Some(2))?;
    Ok(Sexp::Cons(
        Box::new(args[0].clone()),
        Box::new(args[1].clone()),
    ))
}

fn bi_nth(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nth", args, 2, Some(2))?;
    let n = as_int("nth", &args[0])?;
    let mut cur = args[1].clone();
    let mut i = 0;
    while i < n {
        cur = match cur {
            Sexp::Cons(_, d) => *d,
            Sexp::Nil => return Ok(Sexp::Nil),
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other,
                })
            }
        };
        i += 1;
    }
    match cur {
        Sexp::Cons(a, _) => Ok(*a),
        Sexp::Nil => Ok(Sexp::Nil),
        other => Err(EvalError::WrongType {
            expected: "listp".into(),
            got: other,
        }),
    }
}

fn bi_length(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("length", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Nil => Ok(Sexp::Int(0)),
        Sexp::Str(s) => Ok(Sexp::Int(s.chars().count() as i64)),
        Sexp::Vector(v) => Ok(Sexp::Int(v.len() as i64)),
        Sexp::Cons(_, _) => {
            let mut n = 0i64;
            let mut cur = &args[0];
            loop {
                match cur {
                    Sexp::Nil => return Ok(Sexp::Int(n)),
                    Sexp::Cons(_, d) => {
                        n += 1;
                        cur = d;
                    }
                    other => {
                        return Err(EvalError::WrongType {
                            expected: "sequence".into(),
                            got: other.clone(),
                        })
                    }
                }
            }
        }
        other => Err(EvalError::WrongType {
            expected: "sequence".into(),
            got: other.clone(),
        }),
    }
}

fn bi_nthcdr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nthcdr", args, 2, Some(2))?;
    let n = as_int("nthcdr", &args[0])?;
    let mut cur = args[1].clone();
    let mut i = 0;
    while i < n {
        cur = match cur {
            Sexp::Cons(_, d) => *d,
            Sexp::Nil => return Ok(Sexp::Nil),
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other,
                })
            }
        };
        i += 1;
    }
    Ok(cur)
}

fn bi_reverse(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("reverse", args, 1, Some(1))?;
    let mut cur = &args[0];
    let mut acc = Sexp::Nil;
    loop {
        match cur {
            Sexp::Nil => return Ok(acc),
            Sexp::Cons(a, d) => {
                acc = Sexp::Cons(Box::new((**a).clone()), Box::new(acc));
                cur = d;
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other.clone(),
                })
            }
        }
    }
}

fn bi_append(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Ok(Sexp::Nil);
    }
    let mut all_but_last: Vec<Sexp> = Vec::new();
    for a in &args[..args.len() - 1] {
        let mut cur = a;
        loop {
            match cur {
                Sexp::Nil => break,
                Sexp::Cons(h, t) => {
                    all_but_last.push((**h).clone());
                    cur = t;
                }
                other => {
                    return Err(EvalError::WrongType {
                        expected: "listp".into(),
                        got: other.clone(),
                    })
                }
            }
        }
    }
    let mut acc = args.last().unwrap().clone();
    for item in all_but_last.into_iter().rev() {
        acc = Sexp::Cons(Box::new(item), Box::new(acc));
    }
    Ok(acc)
}

// ---------- higher-order ----------

fn list_to_vec(v: &Sexp) -> Result<Vec<Sexp>, EvalError> {
    let mut out = Vec::new();
    let mut cur = v;
    loop {
        match cur {
            Sexp::Nil => return Ok(out),
            Sexp::Cons(a, d) => {
                out.push((**a).clone());
                cur = d;
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "listp".into(),
                    got: other.clone(),
                })
            }
        }
    }
}

fn bi_mapcar(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("mapcar", args, 2, Some(2))?;
    let func = resolve_callable(&args[0], env)?;
    let items = list_to_vec(&args[1])?;
    let mut out = Vec::with_capacity(items.len());
    for it in items {
        out.push(super::apply_function(&func, &[it], env)?);
    }
    Ok(Sexp::list_from(&out))
}

fn bi_mapc(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("mapc", args, 2, Some(2))?;
    let func = resolve_callable(&args[0], env)?;
    let items = list_to_vec(&args[1])?;
    for it in &items {
        super::apply_function(&func, &[it.clone()], env)?;
    }
    Ok(args[1].clone())
}

fn bi_memq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("memq", args, 2, Some(2))?;
    let needle = &args[0];
    let mut cur = args[1].clone();
    loop {
        match &cur {
            Sexp::Nil => return Ok(Sexp::Nil),
            Sexp::Cons(a, _) => {
                if sexp_eq(a, needle) {
                    return Ok(cur);
                }
            }
            _ => return Ok(Sexp::Nil),
        }
        cur = match cur {
            Sexp::Cons(_, d) => *d,
            _ => return Ok(Sexp::Nil),
        };
    }
}

fn bi_member(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("member", args, 2, Some(2))?;
    let needle = &args[0];
    let mut cur = args[1].clone();
    loop {
        match &cur {
            Sexp::Nil => return Ok(Sexp::Nil),
            Sexp::Cons(a, _) => {
                if **a == *needle {
                    return Ok(cur);
                }
            }
            _ => return Ok(Sexp::Nil),
        }
        cur = match cur {
            Sexp::Cons(_, d) => *d,
            _ => return Ok(Sexp::Nil),
        };
    }
}

fn bi_assq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("assq", args, 2, Some(2))?;
    let key = &args[0];
    let mut cur = &args[1];
    loop {
        match cur {
            Sexp::Nil => return Ok(Sexp::Nil),
            Sexp::Cons(pair, rest) => {
                if let Sexp::Cons(k, _) = pair.as_ref() {
                    if sexp_eq(k, key) {
                        return Ok((**pair).clone());
                    }
                }
                cur = rest;
            }
            _ => return Ok(Sexp::Nil),
        }
    }
}

fn bi_assoc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("assoc", args, 2, Some(2))?;
    let key = &args[0];
    let mut cur = &args[1];
    loop {
        match cur {
            Sexp::Nil => return Ok(Sexp::Nil),
            Sexp::Cons(pair, rest) => {
                if let Sexp::Cons(k, _) = pair.as_ref() {
                    if **k == *key {
                        return Ok((**pair).clone());
                    }
                }
                cur = rest;
            }
            _ => return Ok(Sexp::Nil),
        }
    }
}

// ---------- predicates ----------

fn bi_null(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("null", args, 1, Some(1))?;
    Ok(if matches!(args[0], Sexp::Nil) {
        Sexp::T
    } else {
        Sexp::Nil
    })
}

fn bi_predicate(args: &[Sexp], pred: fn(&Sexp) -> bool) -> Result<Sexp, EvalError> {
    require_arity("predicate", args, 1, Some(1))?;
    Ok(if pred(&args[0]) { Sexp::T } else { Sexp::Nil })
}

// ---------- string ----------

fn bi_concat(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let mut out = String::new();
    for a in args {
        match a {
            Sexp::Str(s) => out.push_str(s),
            Sexp::Nil => {}
            Sexp::Cons(_, _) => {
                // list of integers (chars) → string
                let chars = list_to_vec(a)?;
                for c in chars {
                    if let Sexp::Int(n) = c {
                        if let Some(ch) = char::from_u32(n as u32) {
                            out.push(ch);
                        }
                    }
                }
            }
            other => {
                return Err(EvalError::WrongType {
                    expected: "string or sequence".into(),
                    got: other.clone(),
                })
            }
        }
    }
    Ok(Sexp::Str(out))
}

/// Tiny `format` implementation — enough for the bootstrap (= `%s`,
/// `%d`, `%S`, `%%`).  Doc 44 §3.3 keeps this scope minimal.
fn bi_format(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("format", args, 1, None)?;
    let template = match &args[0] {
        Sexp::Str(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "stringp".into(),
                got: other.clone(),
            })
        }
    };
    let mut chars = template.chars().peekable();
    let mut out = String::new();
    let mut arg_idx = 1usize;
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('%') => out.push('%'),
            Some('s') => {
                if let Some(arg) = args.get(arg_idx) {
                    arg_idx += 1;
                    match arg {
                        Sexp::Str(s) => out.push_str(s),
                        other => out.push_str(&format!("{}", other)),
                    }
                }
            }
            Some('d') | Some('i') => {
                if let Some(arg) = args.get(arg_idx) {
                    arg_idx += 1;
                    match arg {
                        Sexp::Int(n) => out.push_str(&n.to_string()),
                        Sexp::Float(x) => out.push_str(&((*x) as i64).to_string()),
                        other => {
                            return Err(EvalError::WrongType {
                                expected: "integerp".into(),
                                got: other.clone(),
                            })
                        }
                    }
                }
            }
            Some('S') | Some('o') => {
                if let Some(arg) = args.get(arg_idx) {
                    arg_idx += 1;
                    out.push_str(&format!("{}", arg));
                }
            }
            Some(other) => {
                return Err(EvalError::Internal(format!(
                    "format: unsupported conversion %{}",
                    other
                )))
            }
            None => out.push('%'),
        }
    }
    Ok(Sexp::Str(out))
}

fn bi_substring(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("substring", args, 2, Some(3))?;
    let s = match &args[0] {
        Sexp::Str(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "stringp".into(),
                got: other.clone(),
            })
        }
    };
    let chars: Vec<char> = s.chars().collect();
    let len = chars.len() as i64;
    let from = normalise_index(as_int("substring", &args[1])?, len);
    let to = if args.len() == 3 && !matches!(args[2], Sexp::Nil) {
        normalise_index(as_int("substring", &args[2])?, len)
    } else {
        len
    };
    if from < 0 || to < from || to > len {
        return Err(EvalError::ArithError("substring out of range".into()));
    }
    let slice: String = chars[from as usize..to as usize].iter().collect();
    Ok(Sexp::Str(slice))
}

fn normalise_index(i: i64, len: i64) -> i64 {
    if i < 0 {
        len + i
    } else {
        i
    }
}

fn bi_intern(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("intern", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Str(s) => Ok(Sexp::Symbol(s.clone())),
        other => Err(EvalError::WrongType {
            expected: "stringp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_symbol_name(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("symbol-name", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(Sexp::Str(s.clone())),
        Sexp::Nil => Ok(Sexp::Str("nil".into())),
        Sexp::T => Ok(Sexp::Str("t".into())),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_string_eq(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("string-equal", args, 2, Some(2))?;
    let a = string_value(&args[0])?;
    let b = string_value(&args[1])?;
    Ok(if a == b { Sexp::T } else { Sexp::Nil })
}

fn string_value(v: &Sexp) -> Result<String, EvalError> {
    match v {
        Sexp::Str(s) => Ok(s.clone()),
        Sexp::Symbol(s) => Ok(s.clone()),
        Sexp::Nil => Ok("nil".into()),
        Sexp::T => Ok("t".into()),
        other => Err(EvalError::WrongType {
            expected: "stringp or symbolp".into(),
            got: other.clone(),
        }),
    }
}

// ---------- symbol / function ----------

fn bi_symbol_value(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("symbol-value", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => env.lookup_value(s),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_symbol_function(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("symbol-function", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => env.lookup_function(s),
        other => Err(EvalError::WrongType {
            expected: "symbolp".into(),
            got: other.clone(),
        }),
    }
}

fn bi_fboundp(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("fboundp", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(if env.is_fbound(s) { Sexp::T } else { Sexp::Nil }),
        _ => Ok(Sexp::Nil),
    }
}

fn bi_boundp(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("boundp", args, 1, Some(1))?;
    match &args[0] {
        Sexp::Symbol(s) => Ok(if env.is_bound(s) { Sexp::T } else { Sexp::Nil }),
        _ => Ok(Sexp::Nil),
    }
}

/// Resolve `arg` to a callable: a symbol points to its function cell,
/// a quoted lambda `(lambda ...)` / `(closure ...)` / `(builtin ...)`
/// is returned as-is.
fn resolve_callable(arg: &Sexp, env: &mut Env) -> Result<Sexp, EvalError> {
    match arg {
        Sexp::Symbol(s) => env.lookup_function(s),
        _ => Ok(arg.clone()),
    }
}

fn bi_funcall(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("funcall", args, 1, None)?;
    let func = resolve_callable(&args[0], env)?;
    super::apply_function(&func, &args[1..], env)
}

fn bi_apply(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("apply", args, 2, None)?;
    let func = resolve_callable(&args[0], env)?;
    // The last arg must be a list; preceding args are spread.
    let mut all_args: Vec<Sexp> = args[1..args.len() - 1].to_vec();
    let tail = list_to_vec(&args[args.len() - 1])?;
    all_args.extend(tail);
    super::apply_function(&func, &all_args, env)
}

fn bi_eval(args: &[Sexp], env: &mut Env) -> Result<Sexp, EvalError> {
    require_arity("eval", args, 1, Some(2))?;
    super::eval(&args[0], env)
}

fn bi_signal(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("signal", args, 2, Some(2))?;
    let tag = match &args[0] {
        Sexp::Symbol(s) => s.clone(),
        other => {
            return Err(EvalError::WrongType {
                expected: "symbolp".into(),
                got: other.clone(),
            })
        }
    };
    // Per Elisp, the second arg is the *data list*.
    Err(EvalError::UserError {
        tag,
        data: args[1].clone(),
    })
}

fn bi_error(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let msg = if args.is_empty() {
        String::new()
    } else if let Sexp::Str(s) = &args[0] {
        // Substitute %s / %d as `format` would.
        let sub_args: Vec<Sexp> = std::iter::once(Sexp::Str(s.clone()))
            .chain(args.iter().skip(1).cloned())
            .collect();
        match bi_format(&sub_args)? {
            Sexp::Str(s) => s,
            _ => s.clone(),
        }
    } else {
        format!("{}", args[0])
    };
    Err(EvalError::UserError {
        tag: "error".into(),
        data: Sexp::list_from(&[Sexp::Str(msg)]),
    })
}

fn bi_identity(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("identity", args, 1, Some(1))?;
    Ok(args[0].clone())
}

fn bi_princ(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.is_empty() {
        return Ok(Sexp::Nil);
    }
    Ok(args[0].clone())
}

fn bi_prin1_to_string(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("prin1-to-string", args, 1, Some(1))?;
    Ok(Sexp::Str(format!("{}", args[0])))
}

#[allow(dead_code)]
fn _unused_truthy(v: &Sexp) -> bool {
    is_truthy(v)
}
