//! `nl-jit-call-*' bridge primitives.  Elisp `lowered_X' wrappers in
//! `lisp/nelisp-jit-strategy.el' call these to invoke JIT entries by
//! name.  Shapes: `i64-i64' / `ptr-ptr' / `syscall' (i64×7) / float /
//! out-param.  Name-keyed lookup in [`unified_fn_ptr`]; `unsafe' fn-ptr
//! casts confined to this module.
//!
//! Supported targets (= Doc 114, gated by `build.rs::link_elisp_cc_spike'):
//! linux-x86_64 / linux-aarch64 / macos-aarch64.  On unsupported targets
//! `unified_fn_ptr' returns `None' for every name (build.rs bails out
//! before the bridge is ever called).

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// Elisp-side trampolines linked from Phase-47-compiled `.o' files.
// Three target gates: (a) arith/float/math = supported set, (b) predicate/
// box_accessor/access record/access = linux-x86_64 only.  On unsupported
// targets the modules are absent and `unified_fn_ptr' is compiled empty.
#[cfg(any(
    all(target_os = "linux", any(target_arch = "x86_64", target_arch = "aarch64")),
    all(target_os = "macos", target_arch = "aarch64"),
))]
mod arith_link {
    extern "C" {
        pub fn nelisp_jit_add2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_sub2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_mul2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_eq2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_lt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_gt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_le2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ge2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logior2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logand2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logxor2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ash(n: i64, c: i64) -> i64;
    }
}

#[cfg(any(
    all(target_os = "linux", any(target_arch = "x86_64", target_arch = "aarch64")),
    all(target_os = "macos", target_arch = "aarch64"),
))]
mod float_link {
    // 9 f64 trampolines: arith (add/sub/mul/div) + ordered cmp + EQ-EPS
    // (|a-b| <= ε via SUBSD/FSUB + abs + UCOMISD/FCMP).
    extern "C" {
        pub fn nl_jit_float_add(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_sub(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_mul(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_div(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_lt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_gt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_le(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_ge(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64;
    }
}

#[cfg(any(
    all(target_os = "linux", any(target_arch = "x86_64", target_arch = "aarch64")),
    all(target_os = "macos", target_arch = "aarch64"),
))]
mod math_link {
    // Unary f64 (identity / exp / log) — `exp'/`log' emit CALL libm-name
    // via `(f64-call SYM ARG)' grammar; static linker resolves via std.
    extern "C" {
        pub fn nl_jit_float_float(x: f64) -> f64;
        pub fn nl_jit_float_exp(x: f64) -> f64;
        pub fn nl_jit_float_log(x: f64) -> f64;
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod predicate_link {
    use crate::eval::sexp::Sexp;
    #[allow(improper_ctypes)]
    extern "C" {
        pub fn nelisp_jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64;
        pub fn nelisp_jit_ref_eq(a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64;
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod box_accessor_link {
    use crate::eval::sexp::Sexp;
    #[allow(improper_ctypes)]
    extern "C" {
        pub fn nelisp_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_ref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_set(
            arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
        ) -> i64;
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
mod access_link {
    use crate::eval::sexp::Sexp;
    #[allow(improper_ctypes)]
    extern "C" {
        pub fn nelisp_jit_length(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aset(
            arg: *const Sexp, idx: i64, val: *const Sexp, out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
    }
}

/// Extract JIT-entry name: accepts `Symbol' or `Str' (2 forms elisp
/// wrappers produce).
fn as_name<'a>(name_arg: &'a str, v: &'a Sexp) -> Result<&'a str, EvalError> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Ok(s.as_str()),
        other => Err(EvalError::WrongType {
            expected: format!("symbol or string ({} arg 0)", name_arg),
            got: other.clone(),
        }),
    }
}

/// Resolve a `nelisp_jit_*' name to the matching trampoline fn ptr.
/// Returned as a raw `*const u8' so each call shape casts independently.
/// Unknown names → `None' (caller raises `EvalError::Internal').
///
/// Three resolution classes by target:
///  (a) supported-set elisp trampolines (arith / float / math) — gated;
///  (b) linux-x86_64-only elisp trampolines (predicate-eq / record-* /
///      access-*) — fall back to legacy Rust impls on other targets;
///  (c) always-Rust trampolines (cons / strings / syscall / time / hash
///      / regex / box-accessor non-record / record_alloc / type_of /
///      sxhash).
pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    // Gate (a): supported set only.  On unsupported targets these names
    // are not resolvable — `build.rs' has already bailed out, so the
    // bridge is unreachable from elisp.
    #[cfg(any(
        all(target_os = "linux", any(target_arch = "x86_64", target_arch = "aarch64")),
        all(target_os = "macos", target_arch = "aarch64"),
    ))]
    {
        let p: *const u8 = match name {
            // arith (12)
            "nelisp_jit_add2" => arith_link::nelisp_jit_add2 as *const u8,
            "nelisp_jit_sub2" => arith_link::nelisp_jit_sub2 as *const u8,
            "nelisp_jit_mul2" => arith_link::nelisp_jit_mul2 as *const u8,
            "nelisp_jit_eq2" => arith_link::nelisp_jit_eq2 as *const u8,
            "nelisp_jit_lt2" => arith_link::nelisp_jit_lt2 as *const u8,
            "nelisp_jit_gt2" => arith_link::nelisp_jit_gt2 as *const u8,
            "nelisp_jit_le2" => arith_link::nelisp_jit_le2 as *const u8,
            "nelisp_jit_ge2" => arith_link::nelisp_jit_ge2 as *const u8,
            "nelisp_jit_logior2" => arith_link::nelisp_jit_logior2 as *const u8,
            "nelisp_jit_logand2" => arith_link::nelisp_jit_logand2 as *const u8,
            "nelisp_jit_logxor2" => arith_link::nelisp_jit_logxor2 as *const u8,
            "nelisp_jit_ash" => arith_link::nelisp_jit_ash as *const u8,
            // float arith + cmp + eq-eps (9)
            "nl_jit_float_add" => float_link::nl_jit_float_add as *const u8,
            "nl_jit_float_sub" => float_link::nl_jit_float_sub as *const u8,
            "nl_jit_float_mul" => float_link::nl_jit_float_mul as *const u8,
            "nl_jit_float_div" => float_link::nl_jit_float_div as *const u8,
            "nl_jit_float_eq_eps" => float_link::nl_jit_float_eq_eps as *const u8,
            "nl_jit_float_lt" => float_link::nl_jit_float_lt as *const u8,
            "nl_jit_float_gt" => float_link::nl_jit_float_gt as *const u8,
            "nl_jit_float_le" => float_link::nl_jit_float_le as *const u8,
            "nl_jit_float_ge" => float_link::nl_jit_float_ge as *const u8,
            // unary f64 math (3)
            "nl_jit_float_float" => math_link::nl_jit_float_float as *const u8,
            "nl_jit_float_exp" => math_link::nl_jit_float_exp as *const u8,
            "nl_jit_float_log" => math_link::nl_jit_float_log as *const u8,
            _ => return resolve_rest(name),
        };
        return Some(p);
    }
    #[cfg(not(any(
        all(target_os = "linux", any(target_arch = "x86_64", target_arch = "aarch64")),
        all(target_os = "macos", target_arch = "aarch64"),
    )))]
    {
        resolve_rest(name)
    }
}

/// Gate (b) + (c): names available on all supported (= the small linux-
/// x86_64-only subset routes to elisp trampolines; same names on other
/// targets fall back to legacy Rust impls in `predicate.rs' / `access.rs'
/// / `box_accessor.rs').  Always-Rust names share one tail block.
fn resolve_rest(name: &str) -> Option<*const u8> {
    // Gate (b) — linux-x86_64-only elisp trampolines.
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    let gated: Option<*const u8> = match name {
        "nelisp_jit_eq_inline" => Some(predicate_link::nelisp_jit_predicate_eq as *const u8),
        "nelisp_jit_ref_eq" => Some(predicate_link::nelisp_jit_ref_eq as *const u8),
        "nelisp_jit_length" => Some(access_link::nelisp_jit_length as *const u8),
        "nelisp_jit_aref" => Some(access_link::nelisp_jit_aref as *const u8),
        "nelisp_jit_aset" => Some(access_link::nelisp_jit_aset as *const u8),
        "nelisp_jit_elt" => Some(access_link::nelisp_jit_elt as *const u8),
        "nl_jit_record_type" => Some(box_accessor_link::nelisp_jit_record_type as *const u8),
        "nl_jit_record_len" => Some(box_accessor_link::nelisp_jit_record_len as *const u8),
        "nl_jit_record_ref" => Some(box_accessor_link::nelisp_jit_record_ref as *const u8),
        "nl_jit_record_set" => Some(box_accessor_link::nelisp_jit_record_set as *const u8),
        _ => None,
    };
    // Doc 114: crate is linux-x86_64-only (`compile_error!' at `lib.rs:30').
    // The Phase 47 `.o' archive provides every gated name via `*_link'
    // above; no Rust trampoline fallback survives.
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    let gated: Option<*const u8> = {
        let _ = name;
        None
    };
    if gated.is_some() {
        return gated;
    }
    // Gate (c) — always-Rust trampolines (target-independent).
    let p: *const u8 = match name {
        // cons (5)
        "nelisp_jit_car" => super::cons::nl_jit_cons_car as *const u8,
        "nelisp_jit_cdr" => super::cons::nl_jit_cons_cdr as *const u8,
        "nelisp_jit_cons" => super::cons::nl_jit_cons_make as *const u8,
        "nelisp_jit_setcar" => super::cons::nl_jit_cons_setcar as *const u8,
        "nelisp_jit_setcdr" => super::cons::nl_jit_cons_setcdr as *const u8,
        // predicate residue (type_of / sxhash stay Rust — grammar gap)
        "nelisp_jit_type_of" => super::predicate::nl_jit_type_of as *const u8,
        "nelisp_jit_sxhash" => super::predicate::nl_jit_sxhash as *const u8,
        // intern / symbol (3)
        "nelisp_jit_intern" => super::strings::nl_jit_intern as *const u8,
        "nelisp_jit_symbol_name" => super::strings::nl_jit_symbol_name as *const u8,
        "nelisp_jit_make_symbol" => super::strings::nl_jit_make_symbol as *const u8,
        // syscall (2)
        "nelisp_jit_syscall" => super::syscall::nl_jit_syscall_call as *const u8,
        "nelisp_jit_syscall_supported_p" => super::syscall::nl_jit_syscall_supported_p as *const u8,
        // box accessor (6 non-record)
        "nl_jit_mut_str_len" => super::box_accessor::nl_jit_mut_str_len as *const u8,
        "nl_jit_bool_vector_len" => super::box_accessor::nl_jit_bool_vector_len as *const u8,
        "nl_jit_str_codepoint_at" => super::box_accessor::nl_jit_str_codepoint_at as *const u8,
        "nl_jit_mut_str_set_codepoint" => super::box_accessor::nl_jit_mut_str_set_codepoint as *const u8,
        "nl_jit_char_table_aref" => super::box_accessor::nl_jit_char_table_aref as *const u8,
        "nl_jit_char_table_aset" => super::box_accessor::nl_jit_char_table_aset as *const u8,
        // record_alloc (list-walk grammar gap)
        "nl_jit_record_alloc" => super::box_accessor::nl_jit_record_alloc as *const u8,
        // Tier 2 (time / hash / case / tokenize / regex)
        "nl_jit_current_unix_time" => super::time::nl_jit_current_unix_time as *const u8,
        "nl_jit_format_unix_time" => super::time::nl_jit_format_unix_time as *const u8,
        "nl_jit_secure_hash" => super::hash::nl_jit_secure_hash as *const u8,
        "nl_jit_downcase" => super::strings::nl_jit_downcase as *const u8,
        "nl_jit_upcase" => super::strings::nl_jit_upcase as *const u8,
        "nl_jit_split_by_non_alnum" => super::strings::nl_jit_split_by_non_alnum as *const u8,
        "nl_jit_string_match_p" => super::regex::nl_jit_string_match_p as *const u8,
        // Tier 2 simple (3)
        "nl_jit_concat_ints" => super::strings::nl_jit_concat_ints as *const u8,
        "nl_jit_make_mut_str" => super::strings::nl_jit_make_mut_str as *const u8,
        "nl_jit_format_float" => super::strings::nl_jit_format_float as *const u8,
        _ => return None,
    };
    Some(p)
}

/// Lookup preamble: arity check + name extract + resolve.  Returns raw
/// fn-ptr the caller transmutes to its specific shape.
fn jit_lookup(
    prim: &str,
    args: &[Sexp],
    arity: usize,
) -> Result<*const u8, EvalError> {
    require_arity(prim, args, arity, Some(arity))?;
    let name = as_name(prim, &args[0])?;
    unified_fn_ptr(name).ok_or_else(|| {
        EvalError::Internal(format!(
            "{}: unknown JIT entry name `{}'", prim, name
        ))
    })
}

/// `(nl-jit-call-i64-i64 NAME A B) -> Int'.  Resolves NAME, casts to
/// `extern "C" fn(i64, i64) -> i64', calls with `(A, B)'.
pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-i64-i64", args, 3)?;
    let a = as_int("nl-jit-call-i64-i64", &args[1])?;
    let b = as_int("nl-jit-call-i64-i64", &args[2])?;
    // SAFETY: NAME resolves to an arith slot with `fn(i64, i64) -> i64'.
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(a, b)))
}

/// `(nl-jit-call-ptr-ptr NAME A B) -> Int'.  Passes raw `*const Sexp'
/// pointers (cons / access / predicate JIT entries).
pub fn bi_nl_jit_call_ptr_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-ptr-ptr", args, 3)?;
    // SAFETY: NAME resolves to `(*const Sexp, *const Sexp) -> i64' shape
    // (= `nelisp_jit_eq_inline').
    let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(&args[1] as *const _, &args[2] as *const _)))
}

/// `(nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5) -> Int'.  Calls
/// `fn(i64×7) -> i64' (= `nelisp_jit_syscall').
pub fn bi_nl_jit_call_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-syscall", args, 8)?;
    let nr = as_int("nl-jit-call-syscall", &args[1])?;
    let a0 = as_int("nl-jit-call-syscall", &args[2])?;
    let a1 = as_int("nl-jit-call-syscall", &args[3])?;
    let a2 = as_int("nl-jit-call-syscall", &args[4])?;
    let a3 = as_int("nl-jit-call-syscall", &args[5])?;
    let a4 = as_int("nl-jit-call-syscall", &args[6])?;
    let a5 = as_int("nl-jit-call-syscall", &args[7])?;
    // SAFETY: NAME resolves to `(i64 × 7) -> i64'.
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
        unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(nr, a0, a1, a2, a3, a4, a5)))
}

// Float-family bridge — `:trampoline-binary-float-{arith,cmp}' ABI.
// Resolve NAME → `nl_jit_float_*' in `float.rs', coerce args via
// `num_pair', transmute, call.  Arith → Float; cmp → Int 0/1.
fn float_pair(args: &[Sexp], name: &str) -> Result<(*const u8, f64, f64), EvalError> {
    let p = jit_lookup(name, args, 3)?;
    let (a, b, _) = crate::eval::builtins::num_pair(&args[1..], name)?;
    Ok((p, a, b))
}

/// `(nl-jit-call-float-float NAME A B) -> Float'.
pub fn bi_nl_jit_call_float_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-float")?;
    // SAFETY: NAME resolves to `nl_jit_float_{add,sub,mul,div}' shape.
    let f: extern "C" fn(f64, f64) -> f64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Float(f(a, b)))
}

/// `(nl-jit-call-float-cmp NAME A B) -> Int' (0 or 1).
pub fn bi_nl_jit_call_float_cmp(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-cmp")?;
    // SAFETY: NAME resolves to `nl_jit_float_{eq_eps,lt,gt,le,ge}' shape.
    let f: extern "C" fn(f64, f64) -> i64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Int(f(a, b)))
}

/// Coerce Sexp → f64: Int cast, Float identity, Nil → 0.0, else WrongType.
fn to_f64(v: &Sexp, expected: &str) -> Result<f64, EvalError> {
    match v {
        Sexp::Int(i) => Ok(*i as f64),
        Sexp::Float(f) => Ok(*f),
        Sexp::Nil => Ok(0.0),
        other => Err(EvalError::WrongType {
            expected: expected.into(),
            got: other.clone(),
        }),
    }
}

fn float_unary(args: &[Sexp], name: &str) -> Result<(*const u8, f64), EvalError> {
    let p = jit_lookup(name, args, 2)?;
    Ok((p, to_f64(&args[1], "number")?))
}

/// `(nl-jit-call-float-unary NAME X) -> Float'.  `fn(f64) -> f64' shape
/// (= `nl_jit_float_{float,exp,log}').  Elisp wrappers in
/// `lisp/nelisp-stdlib-math.el'.
pub fn bi_nl_jit_call_float_unary(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, x) = float_unary(args, "nl-jit-call-float-unary")?;
    // SAFETY: NAME resolves to `nl_jit_float_{float,exp,log}' shape.
    let f: extern "C" fn(f64) -> f64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Float(f(x)))
}

// Out-param trampoline primitives (9 lowered_X fns: 5 cons + 4 access).
// Each trampoline takes input Sexp(s) by `*const Sexp', writes result
// into a `*mut Sexp' out-slot, returns `i64' as OK=0 / ERR=1.  4
// primitives cover the 4 shapes (2 / 3 / 3+i64 / 4+i64).  ERR → generic
// `WrongType { expected: "jit-call-out-N", got: first-arg }' for elisp
// wrapper to `condition-case' + re-signal.

const TRAMPOLINE_OK: i64 = 0;

/// Wrap `(rc, out)' as `Result<Sexp, EvalError>': OK → out, else WrongType.
fn out_result(rc: i64, out: Sexp, prim: &str, arg: &Sexp) -> Result<Sexp, EvalError> {
    if rc == TRAMPOLINE_OK {
        Ok(out)
    } else {
        Err(EvalError::WrongType {
            expected: prim.into(),
            got: arg.clone(),
        })
    }
}

/// `(nl-jit-call-out-1 NAME ARG) -> Sexp'.  `fn(*const Sexp, *mut Sexp)
/// -> i64' shape (car / cdr / length).
pub fn bi_nl_jit_call_out_1(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1", args, 2)?;
    // SAFETY: NAME resolves to `(*const Sexp, *mut Sexp) -> i64' shape.
    let f: extern "C" fn(*const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1", &args[1])
}

/// `(nl-jit-call-out-2 NAME ARG1 ARG2) -> Sexp'.  `fn(*const Sexp,
/// *const Sexp, *mut Sexp) -> i64' (cons / setcar / setcdr).
pub fn bi_nl_jit_call_out_2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2", args, 3)?;
    // SAFETY: NAME resolves to `(*const Sexp, *const Sexp, *mut Sexp)
    // -> i64' shape.
    let f: extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &args[2] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2", &args[1])
}

/// `(nl-jit-call-out-1i NAME ARG IDX) -> Sexp'.  `fn(*const Sexp, i64,
/// *mut Sexp) -> i64' (aref / elt).
pub fn bi_nl_jit_call_out_1i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-1i", args, 3)?;
    let idx = as_int("nl-jit-call-out-1i", &args[2])?;
    // SAFETY: NAME resolves to `(*const Sexp, i64, *mut Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &mut out as *mut _);
    out_result(r, out, "jit-call-out-1i", &args[1])
}

/// `(nl-jit-call-out-2i NAME ARG IDX VAL) -> Sexp'.  `fn(*const Sexp,
/// i64, *const Sexp, *mut Sexp) -> i64' (aset).
pub fn bi_nl_jit_call_out_2i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let p = jit_lookup("nl-jit-call-out-2i", args, 4)?;
    let idx = as_int("nl-jit-call-out-2i", &args[2])?;
    // SAFETY: NAME resolves to `(*const Sexp, i64, *const Sexp, *mut
    // Sexp) -> i64'.
    let f: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &args[3] as *const _, &mut out as *mut _);
    out_result(r, out, "jit-call-out-2i", &args[1])
}

/// `(nl-jit-call-format-float NAME X CONV PREC) -> Sexp'.  `fn(f64,
/// u32, i64, *mut Sexp) -> i64' (= `:trampoline-format-float' in
/// `src/nelisp-cc-runtime.el').  Sole caller: `lisp/nelisp-stdlib-
/// format.el' `nelisp--format-float-body'.
pub fn bi_nl_jit_call_format_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-format-float", args, 4, Some(4))?;
    let name = as_name("nl-jit-call-format-float", &args[0])?;
    let x = to_f64(&args[1], "numberp")?;
    let conv = as_int("nl-jit-call-format-float", &args[2])?;
    let prec = as_int("nl-jit-call-format-float", &args[3])?;
    let p = unified_fn_ptr(name).ok_or_else(|| {
        EvalError::Internal(format!(
            "nl-jit-call-format-float: unknown JIT entry name `{}'", name
        ))
    })?;
    // SAFETY: NAME resolves to `nl_jit_format_float' shape (= xmm0 +
    // rsi + rdx + rcx).
    let f: extern "C" fn(f64, u32, i64, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(x, conv as u32, prec, &mut out as *mut _);
    out_result(r, out, "jit-call-format-float", &args[1])
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- unified_fn_ptr coverage ---

    #[test]
    fn unified_fn_ptr_resolves_all_24_entries() {
        // The complete name set the bridge supports (= 12 arith + 5
        // cons + 4 access + 1 predicate + 2 syscall = 24).  Each must
        // resolve to a non-null fn ptr.
        let names = [
            "nelisp_jit_add2",
            "nelisp_jit_sub2",
            "nelisp_jit_mul2",
            "nelisp_jit_eq2",
            "nelisp_jit_lt2",
            "nelisp_jit_gt2",
            "nelisp_jit_le2",
            "nelisp_jit_ge2",
            "nelisp_jit_logior2",
            "nelisp_jit_logand2",
            "nelisp_jit_logxor2",
            "nelisp_jit_ash",
            "nelisp_jit_car",
            "nelisp_jit_cdr",
            "nelisp_jit_cons",
            "nelisp_jit_setcar",
            "nelisp_jit_setcdr",
            "nelisp_jit_length",
            "nelisp_jit_aref",
            "nelisp_jit_aset",
            "nelisp_jit_elt",
            "nelisp_jit_eq_inline",
            "nelisp_jit_type_of",
            "nelisp_jit_intern",
            "nelisp_jit_symbol_name",
            "nelisp_jit_make_symbol",
            "nelisp_jit_ref_eq",
            "nelisp_jit_syscall",
            "nelisp_jit_syscall_supported_p",
        ];
        for n in names {
            let p = unified_fn_ptr(n);
            assert!(
                p.is_some(),
                "unified_fn_ptr missing entry `{}'", n
            );
            assert!(!p.unwrap().is_null(), "fn ptr for `{}' is null", n);
        }
    }

    #[test]
    fn unified_fn_ptr_unknown_returns_none() {
        assert!(unified_fn_ptr("nelisp_jit_does_not_exist").is_none());
        assert!(unified_fn_ptr("").is_none());
    }

    // --- bi_nl_jit_call_i64_i64 ---

    #[test]
    fn call_i64_i64_add2_smoke() {
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(7),
            Sexp::Int(8),
        ])
        .expect("add2 must succeed");
        assert_eq!(r, Sexp::Int(15));
    }

    #[test]
    fn call_i64_i64_accepts_string_name() {
        // Symbol or Str both supported (= flexibility for elisp callers
        // that pass `'nelisp_jit_mul2' or `"nelisp_jit_mul2"').
        let r = bi_nl_jit_call_i64_i64(&[
            Sexp::Str("nelisp_jit_mul2".into()),
            Sexp::Int(6),
            Sexp::Int(7),
        ])
        .expect("mul2 (str name) must succeed");
        assert_eq!(r, Sexp::Int(42));
    }

    #[test]
    fn call_i64_i64_unknown_name_errors() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_no_such".into()),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        match err {
            EvalError::Internal(msg) => {
                assert!(msg.contains("nl-jit-call-i64-i64"));
                assert!(msg.contains("nelisp_jit_no_such"));
            }
            other => panic!("expected Internal, got {:?}", other),
        }
    }

    #[test]
    fn call_i64_i64_arity_too_few() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(1),
        ])
        .expect_err("arity 2 must reject");
        match err {
            EvalError::WrongNumberOfArguments { function, .. } => {
                assert_eq!(function, "nl-jit-call-i64-i64");
            }
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn call_i64_i64_arity_too_many() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Symbol("nelisp_jit_add2".into()),
            Sexp::Int(1),
            Sexp::Int(2),
            Sexp::Int(3),
        ])
        .expect_err("arity 4 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }

    #[test]
    fn call_i64_i64_bad_name_type_errors() {
        let err = bi_nl_jit_call_i64_i64(&[
            Sexp::Int(0),
            Sexp::Int(1),
            Sexp::Int(2),
        ])
        .expect_err("Int as name must error");
        assert!(matches!(err, EvalError::WrongType { .. }));
    }

    // --- bi_nl_jit_call_ptr_ptr ---

    #[test]
    fn call_ptr_ptr_eq_inline_match() {
        // Two equal ints → eq returns 1.
        let r = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(7),
            Sexp::Int(7),
        ])
        .expect("eq_inline must succeed");
        assert_eq!(r, Sexp::Int(1));
    }

    #[test]
    fn call_ptr_ptr_eq_inline_mismatch() {
        // Two different ints → eq returns 0.
        let r = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(7),
            Sexp::Int(8),
        ])
        .expect("eq_inline must succeed");
        assert_eq!(r, Sexp::Int(0));
    }

    #[test]
    fn call_ptr_ptr_unknown_name_errors() {
        let err = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_no_such_ptr".into()),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        assert!(matches!(err, EvalError::Internal(_)));
    }

    #[test]
    fn call_ptr_ptr_arity_too_few() {
        let err = bi_nl_jit_call_ptr_ptr(&[
            Sexp::Symbol("nelisp_jit_eq_inline".into()),
            Sexp::Int(1),
        ])
        .expect_err("arity 2 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }

    // --- bi_nl_jit_call_syscall ---

    #[test]
    fn call_syscall_arity_check() {
        // Wrong arity (= 7 args instead of 8) must reject before
        // touching the syscall fn ptr.
        let err = bi_nl_jit_call_syscall(&[
            Sexp::Symbol("nelisp_jit_syscall".into()),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("arity 7 must reject");
        match err {
            EvalError::WrongNumberOfArguments { function, .. } => {
                assert_eq!(function, "nl-jit-call-syscall");
            }
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn call_syscall_unknown_name_errors() {
        let err = bi_nl_jit_call_syscall(&[
            Sexp::Symbol("nelisp_jit_no_syscall".into()),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
            Sexp::Int(0),
        ])
        .expect_err("unknown name must error");
        match err {
            EvalError::Internal(msg) => {
                assert!(msg.contains("nl-jit-call-syscall"));
                assert!(msg.contains("nelisp_jit_no_syscall"));
            }
            other => panic!("expected Internal, got {:?}", other),
        }
    }

    // --- Stage b.2.5 — bi_nl_jit_call_out_1 (car / cdr / length) ---

    #[test]
    fn call_out_1_car_returns_head() {
        // (1 2 3) cons → car = 1.
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()),
            lst,
        ])
        .expect("car must succeed");
        assert_eq!(r, Sexp::Int(1));
    }

    #[test]
    fn call_out_1_cdr_returns_tail() {
        // (1 2 3) cons → cdr = (2 3).
        let lst = Sexp::list_from(&[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let expected_tail =
            Sexp::list_from(&[Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_cdr".into()),
            lst,
        ])
        .expect("cdr must succeed");
        assert_eq!(r, expected_tail);
    }

    #[test]
    fn call_out_1_car_nil_returns_nil() {
        // Nil → trampoline OK with out = Nil.
        let r = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()),
            Sexp::Nil,
        ])
        .expect("car nil must succeed");
        assert_eq!(r, Sexp::Nil);
    }

    #[test]
    fn call_out_1_car_wrong_type_errors() {
        // Int(7) → trampoline ERR → WrongType.
        let err = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()),
            Sexp::Int(7),
        ])
        .expect_err("car of int must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-1");
                assert_eq!(got, Sexp::Int(7));
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn call_out_1_length_vector() {
        // (length [1 2 3]) → 3.
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_length".into()),
            v,
        ])
        .expect("length vec must succeed");
        assert_eq!(r, Sexp::Int(3));
    }

    #[test]
    fn call_out_1_arity_too_few() {
        let err = bi_nl_jit_call_out_1(&[Sexp::Symbol("nelisp_jit_car".into())])
            .expect_err("arity 1 must reject");
        match err {
            EvalError::WrongNumberOfArguments { function, .. } => {
                assert_eq!(function, "nl-jit-call-out-1");
            }
            other => panic!("expected WrongNumberOfArguments, got {:?}", other),
        }
    }

    #[test]
    fn call_out_1_unknown_name_errors() {
        let err = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_no_such_out".into()),
            Sexp::Nil,
        ])
        .expect_err("unknown name must error");
        match err {
            EvalError::Internal(msg) => {
                assert!(msg.contains("nl-jit-call-out-1"));
                assert!(msg.contains("nelisp_jit_no_such_out"));
            }
            other => panic!("expected Internal, got {:?}", other),
        }
    }

    // --- Stage b.2.5 — bi_nl_jit_call_out_2 (cons / setcar / setcdr) ---

    #[test]
    fn call_out_2_cons_constructs_pair() {
        // (cons 1 2) → (1 . 2).
        let r = bi_nl_jit_call_out_2(&[
            Sexp::Symbol("nelisp_jit_cons".into()),
            Sexp::Int(1),
            Sexp::Int(2),
        ])
        .expect("cons must succeed");
        // Verify the result is (1 . 2) by extracting via car / cdr
        // through bi_nl_jit_call_out_1.
        let car = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()),
            r.clone(),
        ])
        .expect("car of constructed pair");
        assert_eq!(car, Sexp::Int(1));
        let cdr = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_cdr".into()),
            r,
        ])
        .expect("cdr of constructed pair");
        assert_eq!(cdr, Sexp::Int(2));
    }

    #[test]
    fn call_out_2_setcar_returns_value_and_mutates() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let val = Sexp::Symbol("new-head".into());
        let r = bi_nl_jit_call_out_2(&[
            Sexp::Symbol("nelisp_jit_setcar".into()),
            pair.clone(),
            val.clone(),
        ])
        .expect("setcar must succeed");
        // setcar returns VALUE per Emacs contract.
        assert_eq!(r, val);
        // And the pair is mutated.
        let car = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_car".into()),
            pair,
        ])
        .expect("car after setcar");
        assert_eq!(car, val);
    }

    #[test]
    fn call_out_2_setcdr_returns_value_and_mutates() {
        let pair = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let val = Sexp::Str("new-tail".into());
        let r = bi_nl_jit_call_out_2(&[
            Sexp::Symbol("nelisp_jit_setcdr".into()),
            pair.clone(),
            val.clone(),
        ])
        .expect("setcdr must succeed");
        assert_eq!(r, val);
        let cdr = bi_nl_jit_call_out_1(&[
            Sexp::Symbol("nelisp_jit_cdr".into()),
            pair,
        ])
        .expect("cdr after setcdr");
        assert_eq!(cdr, val);
    }

    #[test]
    fn call_out_2_setcar_wrong_type_errors() {
        let err = bi_nl_jit_call_out_2(&[
            Sexp::Symbol("nelisp_jit_setcar".into()),
            Sexp::Int(42),
            Sexp::Nil,
        ])
        .expect_err("setcar of int must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-2");
                assert_eq!(got, Sexp::Int(42));
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    // --- Stage b.2.5 — bi_nl_jit_call_out_1i (aref / elt) ---

    #[test]
    fn call_out_1i_aref_in_range() {
        // (aref [1 2 3] 1) → 2.
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()),
            v,
            Sexp::Int(1),
        ])
        .expect("aref in-range must succeed");
        assert_eq!(r, Sexp::Int(2));
    }

    #[test]
    fn call_out_1i_aref_out_of_range_errors() {
        // (aref [1 2 3] 5) → trampoline ERR → WrongType.
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let err = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()),
            v.clone(),
            Sexp::Int(5),
        ])
        .expect_err("aref out-of-range must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-1i");
                assert_eq!(got, v);
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn call_out_1i_elt_vector() {
        // (elt [10 20 30] 2) → 30.
        let v = Sexp::vector(vec![
            Sexp::Int(10),
            Sexp::Int(20),
            Sexp::Int(30),
        ]);
        let r = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_elt".into()),
            v,
            Sexp::Int(2),
        ])
        .expect("elt in-range must succeed");
        assert_eq!(r, Sexp::Int(30));
    }

    #[test]
    fn call_out_1i_arity_too_few() {
        let err = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()),
            Sexp::Nil,
        ])
        .expect_err("arity 2 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }

    // --- Stage b.2.5 — bi_nl_jit_call_out_2i (aset) ---

    #[test]
    fn call_out_2i_aset_returns_value_and_mutates() {
        // (aset [1 2 3] 1 99) → 99, vector becomes [1 99 3].
        let v = Sexp::vector(vec![Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)]);
        let r = bi_nl_jit_call_out_2i(&[
            Sexp::Symbol("nelisp_jit_aset".into()),
            v.clone(),
            Sexp::Int(1),
            Sexp::Int(99),
        ])
        .expect("aset must succeed");
        assert_eq!(r, Sexp::Int(99));
        // Verify slot 1 mutated via aref.
        let got = bi_nl_jit_call_out_1i(&[
            Sexp::Symbol("nelisp_jit_aref".into()),
            v,
            Sexp::Int(1),
        ])
        .expect("aref after aset");
        assert_eq!(got, Sexp::Int(99));
    }

    #[test]
    fn call_out_2i_aset_out_of_range_errors() {
        let v = Sexp::vector(vec![Sexp::Int(1)]);
        let err = bi_nl_jit_call_out_2i(&[
            Sexp::Symbol("nelisp_jit_aset".into()),
            v.clone(),
            Sexp::Int(5),
            Sexp::Int(99),
        ])
        .expect_err("aset out-of-range must error");
        match err {
            EvalError::WrongType { expected, got } => {
                assert_eq!(expected, "jit-call-out-2i");
                assert_eq!(got, v);
            }
            other => panic!("expected WrongType, got {:?}", other),
        }
    }

    #[test]
    fn call_out_2i_arity_too_few() {
        let err = bi_nl_jit_call_out_2i(&[
            Sexp::Symbol("nelisp_jit_aset".into()),
            Sexp::Nil,
            Sexp::Int(0),
        ])
        .expect_err("arity 3 must reject");
        assert!(matches!(err, EvalError::WrongNumberOfArguments { .. }));
    }
}
