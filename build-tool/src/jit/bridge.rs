//! Doc 77b Stage b.2 — `nl-jit-call-*` bridge primitives.
//!
//! Provides the Rust-side primitives that elisp `lowered_X` wrappers
//! (= the future replacements for `lowered_X` Rust fns shipped in
//! Stage b.4) call to invoke JIT entries by name.  The 3 primitives
//! cover the 3 calling shapes the registered JIT entries need:
//!
//! - `nl-jit-call-i64-i64 NAME A B` — `extern "C" fn(i64, i64) -> i64'.
//!   Used by the 12 arith / cmp / bitwise + `ash' entries (= raw i64
//!   in / i64 out: `nelisp_jit_add2', `nelisp_jit_eq2' etc.).
//! - `nl-jit-call-ptr-ptr NAME A B` — `extern "C" fn(*const Sexp,
//!   *const Sexp) -> i64'.  Used by `nelisp_jit_eq_inline' (= the only
//!   ptr/ptr/i64 entry today).
//! - `nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5` — `extern "C"
//!   fn(i64 × 7) -> i64'.  Used by `nelisp_jit_syscall' /
//!   `nelisp_jit_syscall_supported_p' (= the latter takes 0 i64s but
//!   the trampoline shape forwards 7 padding zeros which Cranelift
//!   ignores when the sig has no params; for supported_p we route
//!   through the same primitive with NR/A0..A5 = 0 for uniformity).
//!
//! The name-keyed lookup is centralized in [`unified_fn_ptr`] which
//! maps every `nelisp_jit_*' symbol to the matching field in
//! [`super::UnifiedJit`].  Returns the raw `*const u8' so the call site
//! casts to the correct shape — the 3 primitives below are the
//! *only* place where that cast happens, keeping `unsafe' centralized.
//!
//! Out of scope for Stage b.2:
//! - elisp wrapper file (= `lisp/nelisp-jit-strategy.el', Stage b.4).
//! - DSL-driven rule registration via `nelisp--jit-bootstrap' (= Stage
//!   b.3 — the registry today is the typed `UnifiedJit' struct, not the
//!   yet-to-be-built name → FuncId alist from elisp).

use crate::eval::builtins::{as_int, require_arity};
use crate::eval::error::EvalError;
use crate::eval::sexp::Sexp;

// Phase 7.1.6.e (Doc 28 §3.6.e): `super::unified_jit' import deleted —
// `UnifiedJit' struct + `unified_jit()' OnceLock are gone now that
// every cluster (cons / access / arith / predicate / syscall) goes
// through `unified_fn_ptr's direct `#[no_mangle]' trampoline mapping.

/// Extract the JIT-entry name argument: accepts `Symbol' or `Str'
/// (= the 2 forms elisp wrappers will produce — `'nelisp_jit_add2'
/// reads as `Symbol("nelisp_jit_add2")', `"nelisp_jit_add2"' as `Str').
fn as_name<'a>(name_arg: &'a str, v: &'a Sexp) -> Result<&'a str, EvalError> {
    match v {
        Sexp::Symbol(s) | Sexp::Str(s) => Ok(s.as_str()),
        other => Err(EvalError::WrongType {
            expected: format!("symbol or string ({} arg 0)", name_arg),
            got: other.clone(),
        }),
    }
}

/// Resolve a `nelisp_jit_*' name to the matching JIT-compiled fn ptr
/// stored in [`super::UnifiedJit`].  Returned as a raw `*const u8' so
/// each call shape (= i64/i64, ptr/ptr, 7×i64) casts independently.
///
/// Unknown names return `None' (= the bridge primitive raises
/// `EvalError::Internal' wrapping the bad name).
pub(super) fn unified_fn_ptr(name: &str) -> Option<*const u8> {
    // Phase 7.1.6.e (Doc 28 §3.6.e): `let u = unified_jit()' deleted
    // — every name now resolves directly to a `#[no_mangle] extern
    // "C"' Rust trampoline.  No Cranelift wrapper page is constructed
    // anywhere in `jit::' anymore.
    let p: *const u8 = match name {
        // ---- arith (12) ----
        // Phase 7.1.6.c (Doc 28 §3.6.c): resolve arith names directly
        // to the `#[no_mangle] extern "C"' trampolines now that the
        // Cranelift `JitArith' wrapper page has been deleted.  Unlike
        // cons / access takeover, arith had no pre-existing Rust
        // trampoline body — the Cranelift IR was the implementation —
        // so 12 plain Rust trampolines (`nl_jit_arith_*') were added
        // in this commit that mirror the Cranelift IR semantics 1-to-1.
        // nelisp-cc compiled hot paths skip this bridge entirely and
        // emit the host arithmetic instruction inline via existing
        // SSA opcodes (no `:ssa-call-primitive' detour needed).
        "nelisp_jit_add2" => super::arith::nl_jit_arith_add2 as *const u8,
        "nelisp_jit_sub2" => super::arith::nl_jit_arith_sub2 as *const u8,
        "nelisp_jit_mul2" => super::arith::nl_jit_arith_mul2 as *const u8,
        "nelisp_jit_eq2" => super::arith::nl_jit_arith_eq2 as *const u8,
        "nelisp_jit_lt2" => super::arith::nl_jit_arith_lt2 as *const u8,
        "nelisp_jit_gt2" => super::arith::nl_jit_arith_gt2 as *const u8,
        "nelisp_jit_le2" => super::arith::nl_jit_arith_le2 as *const u8,
        "nelisp_jit_ge2" => super::arith::nl_jit_arith_ge2 as *const u8,
        "nelisp_jit_logior2" => super::arith::nl_jit_arith_logior2 as *const u8,
        "nelisp_jit_logand2" => super::arith::nl_jit_arith_logand2 as *const u8,
        "nelisp_jit_logxor2" => super::arith::nl_jit_arith_logxor2 as *const u8,
        "nelisp_jit_ash" => super::arith::nl_jit_arith_ash as *const u8,
        // ---- cons (5) ----
        // Phase 7.1.6.a.2 (Doc 28 §3.6.a): resolve cons names directly
        // to the `#[no_mangle] extern "C"' trampolines now that the
        // Cranelift `JitCons' wrapper page has been deleted.  The
        // inline-NIL fast path (= the deleted `declare_unary_with_nil_
        // inline' Cranelift IR shape) is no longer present here; for
        // car / cdr the NIL case is handled by the trampoline's first
        // arm (`tag == SEXP_TAG_NIL → OK') without further work, so
        // semantic behaviour is preserved.  nelisp-cc compiled hot
        // paths skip this bridge entirely via dlsym + direct CALL.
        "nelisp_jit_car" => super::cons::nl_jit_cons_car as *const u8,
        "nelisp_jit_cdr" => super::cons::nl_jit_cons_cdr as *const u8,
        "nelisp_jit_cons" => super::cons::nl_jit_cons_make as *const u8,
        "nelisp_jit_setcar" => super::cons::nl_jit_cons_setcar as *const u8,
        "nelisp_jit_setcdr" => super::cons::nl_jit_cons_setcdr as *const u8,
        // ---- access (4) ----
        // Phase 7.1.6.b (Doc 28 §3.6.b): resolve access names directly
        // to the `#[no_mangle] extern "C"' trampolines now that the
        // Cranelift `JitAccess' wrapper page has been deleted.  The
        // inline-NIL fast path for `length' (= the deleted `declare_
        // length_with_inline_nil' Cranelift IR shape) is no longer
        // present here; the NIL case is handled by the trampoline's
        // first arm (`tag == SEXP_TAG_NIL → write Sexp::Int(0) /
        // return OK') without further work, so semantic behaviour is
        // preserved.  nelisp-cc compiled hot paths skip this bridge
        // entirely via dlsym + direct CALL.
        "nelisp_jit_length" => super::access::nl_jit_access_length as *const u8,
        "nelisp_jit_aref" => super::access::nl_jit_access_aref as *const u8,
        "nelisp_jit_aset" => super::access::nl_jit_access_aset as *const u8,
        "nelisp_jit_elt" => super::access::nl_jit_access_elt as *const u8,
        // ---- predicate (1) ----
        // Phase 7.1.6.d (Doc 28 §3.6.d): resolve predicate name directly
        // to the `#[no_mangle] extern "C"' trampoline now that the
        // Cranelift `JitPredicate' wrapper page has been deleted.  Like
        // arith (7.1.6.c), predicate had only a partial Rust helper
        // body (`nl_jit_pred_eq') covering the slow `sexp_eq' arm; the
        // full 7-block fast-path semantics (= same-ref / tag-eq / int-
        // payload) lived in Cranelift IR.  The takeover consolidates
        // all 7 blocks into a single `nl_jit_predicate_eq' trampoline
        // body (see `jit::predicate').  nelisp-cc compiled hot paths
        // skip this bridge entirely via dlsym + direct CALL.  The `_u'
        // binding is now unused since no field of `UnifiedJit' is
        // looked up on this match arm.
        "nelisp_jit_eq_inline" => super::predicate::nl_jit_predicate_eq as *const u8,
        // Doc 86 §86.1.a (2026-05-10): `type-of' migrated to elisp on
        // top of this trampoline (= reachable via `(nl-jit-call-out-1
        // "nelisp_jit_type_of" x)' from `lisp/nelisp-stdlib.el').
        "nelisp_jit_type_of" => super::predicate::nl_jit_type_of as *const u8,
        // ---- intern / symbol (3) ---- Doc 86 §86.1.d (2026-05-10).
        "nelisp_jit_intern" => super::strings::nl_jit_intern as *const u8,
        "nelisp_jit_symbol_name" => super::strings::nl_jit_symbol_name as *const u8,
        "nelisp_jit_make_symbol" => super::strings::nl_jit_make_symbol as *const u8,
        // Doc 86 §86.1.b (2026-05-10): `nelisp--ref-eq' migrated to
        // elisp on top of this trampoline (= reachable via
        // `(nl-jit-call-out-2 "nelisp_jit_ref_eq" a b)' from
        // `lisp/nelisp-jit-strategy.el').  Returns `Sexp::T' / `Sexp::Nil'
        // directly so the elisp wrapper can avoid the `nelisp--int-eq-
        // zero' convert dance that `eq' uses.
        "nelisp_jit_ref_eq" => super::predicate::nl_jit_ref_eq as *const u8,
        // Doc 86 §86.1.b — `sxhash' trampoline (1:1 port, kept in
        // Rust for `DefaultHasher' bit-exactness, Doc 87 §3.2).
        "nelisp_jit_sxhash" => super::predicate::nl_jit_sxhash as *const u8,
        // ---- syscall (2) ----
        // Phase 7.1.6.e (Doc 28 §3.6.e): resolve syscall names directly
        // to the `#[no_mangle] extern "C"' trampolines now that the
        // Cranelift `JitSyscall' wrapper page has been deleted.  Like
        // arith / predicate, syscall's Cranelift IR was an
        // `iconst+return' (supported_p) + a single-`call' pass-through
        // to the existing `nl_jit_syscall_call' Rust helper; the
        // takeover re-exposes that helper as `#[no_mangle]' (no body
        // change) and adds a sibling `#[no_mangle]' for the const
        // predicate.  nelisp-cc compiled hot paths skip this bridge
        // entirely via dlsym + direct CALL.
        "nelisp_jit_syscall" => super::syscall::nl_jit_syscall_call as *const u8,
        "nelisp_jit_syscall_supported_p" => {
            super::syscall::nl_jit_syscall_supported_p as *const u8
        }
        // ---- float (8) ---- Doc 84 §84.1, xmm-marshalling Float trampolines.
        "nl_jit_float_add" => super::float::nl_jit_float_add as *const u8,
        "nl_jit_float_sub" => super::float::nl_jit_float_sub as *const u8,
        "nl_jit_float_mul" => super::float::nl_jit_float_mul as *const u8,
        // Doc 86 §86.1.b (2026-05-10): `/' migrated to elisp via this
        // binary trampoline (= same `:trampoline-binary-float-arith'
        // ABI as add/sub/mul); division-by-zero check + integer-trunc
        // gating live in `lisp/nelisp-stdlib.el' `(defun / ...)'.
        "nl_jit_float_div" => super::float::nl_jit_float_div as *const u8,
        "nl_jit_float_eq_eps" => super::float::nl_jit_float_eq_eps as *const u8,
        "nl_jit_float_lt" => super::float::nl_jit_float_lt as *const u8,
        "nl_jit_float_gt" => super::float::nl_jit_float_gt as *const u8,
        "nl_jit_float_le" => super::float::nl_jit_float_le as *const u8,
        "nl_jit_float_ge" => super::float::nl_jit_float_ge as *const u8,
        // ---- box accessor (6) ---- Doc 84 §84.3 (2026-05-10).
        "nl_jit_mut_str_len" => super::box_accessor::nl_jit_mut_str_len as *const u8,
        "nl_jit_bool_vector_len" => super::box_accessor::nl_jit_bool_vector_len as *const u8,
        "nl_jit_str_codepoint_at" => super::box_accessor::nl_jit_str_codepoint_at as *const u8,
        "nl_jit_mut_str_set_codepoint" => super::box_accessor::nl_jit_mut_str_set_codepoint as *const u8,
        "nl_jit_char_table_aref" => super::box_accessor::nl_jit_char_table_aref as *const u8,
        "nl_jit_char_table_aset" => super::box_accessor::nl_jit_char_table_aset as *const u8,
        // ---- record family (5) ---- Doc 86 §86.1.c (2026-05-10).
        // Same `nl-jit-call-out-{1,1i,2i,2}' bridge primitives as box
        // accessor above; trampolines live in `box_accessor.rs'.
        "nl_jit_record_type" => super::box_accessor::nl_jit_record_type as *const u8,
        "nl_jit_record_len" => super::box_accessor::nl_jit_record_len as *const u8,
        "nl_jit_record_ref" => super::box_accessor::nl_jit_record_ref as *const u8,
        "nl_jit_record_set" => super::box_accessor::nl_jit_record_set as *const u8,
        "nl_jit_record_alloc" => super::box_accessor::nl_jit_record_alloc as *const u8,
        // ---- Doc 87 §86.1.f (2026-05-10): Tier 2 trampolines ----
        // Time + hash (= 2-arg / 0-arg `bi_*' helpers retired in
        // `eval/builtins.rs').  Reachable via `nl-jit-call-i64-i64'
        // (= 0-arg current_unix_time padding) and `nl-jit-call-out-2'
        // (= format_unix_time / secure_hash 2-arg out-Sexp shape).
        "nl_jit_current_unix_time" => super::time::nl_jit_current_unix_time as *const u8,
        "nl_jit_format_unix_time" => super::time::nl_jit_format_unix_time as *const u8,
        "nl_jit_secure_hash" => super::hash::nl_jit_secure_hash as *const u8,
        // String case + tokenize trampolines (= `nl-jit-call-out-1' /
        // `nl-jit-call-out-2' bridges).
        "nl_jit_downcase" => super::strings::nl_jit_downcase as *const u8,
        "nl_jit_upcase" => super::strings::nl_jit_upcase as *const u8,
        "nl_jit_split_by_non_alnum"
            => super::strings::nl_jit_split_by_non_alnum as *const u8,
        // Regex backend — single trampoline replacing
        // `bi_string_match_p'.  Reachable via `nl-jit-call-out-2'.
        "nl_jit_string_match_p" => super::regex::nl_jit_string_match_p as *const u8,
        // Unary float math (= `:trampoline-unary-float' ABI mode,
        // Doc 87 §5).  Reachable via `nl-jit-call-float-unary' (new
        // bridge primitive shipped in this commit).
        "nl_jit_float_float" => super::math::nl_jit_float_float as *const u8,
        "nl_jit_float_exp" => super::math::nl_jit_float_exp as *const u8,
        "nl_jit_float_log" => super::math::nl_jit_float_log as *const u8,
        _ => return None,
    };
    Some(p)
}

fn unknown_name_err(prim: &str, name: &str) -> EvalError {
    EvalError::Internal(format!(
        "{}: unknown JIT entry name `{}'", prim, name
    ))
}

/// `(nl-jit-call-i64-i64 NAME A B) -> Int'.  Looks up NAME in the
/// unified registry, casts the resolved fn ptr to
/// `extern "C" fn(i64, i64) -> i64', calls it with `(A, B)', wraps the
/// `i64' result as `Sexp::Int'.
pub fn bi_nl_jit_call_i64_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-i64-i64", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-i64-i64", &args[0])?;
    let a = as_int("nl-jit-call-i64-i64", &args[1])?;
    let b = as_int("nl-jit-call-i64-i64", &args[2])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-i64-i64", name))?;
    // SAFETY: every name resolved by `unified_fn_ptr' to an arith /
    // syscall_supported_p slot has the `extern "C" fn(i64, i64) -> i64'
    // shape (or 0-arg supported_p which we route through
    // call-syscall instead — caller responsibility).
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(p) };
    let v = f(a, b);
    Ok(Sexp::Int(v))
}

/// `(nl-jit-call-ptr-ptr NAME A B) -> Int'.  Same shape as i64-i64 but
/// passes raw `*const Sexp' pointers (= what the cons / access /
/// predicate JIT entries expect).  Wrapping result as `Sexp::Int' so
/// the elisp wrapper can do `(if (= 0 v) nil t)' for predicate-style
/// entries or pass the result through for trampoline-OK / -ERR codes.
pub fn bi_nl_jit_call_ptr_ptr(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-ptr-ptr", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-ptr-ptr", &args[0])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-ptr-ptr", name))?;
    // SAFETY: caller is responsible for passing a name whose JIT entry
    // has the `(*const Sexp, *const Sexp) -> i64' shape — currently
    // only `nelisp_jit_eq_inline' qualifies.  Passing a name with a
    // different shape is UB but the surface stays useful for the
    // single-purpose elisp-side `eq' wrapper Stage b.4 will ship.
    let f: extern "C" fn(*const Sexp, *const Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let v = f(&args[1] as *const _, &args[2] as *const _);
    Ok(Sexp::Int(v))
}

/// `(nl-jit-call-syscall NAME NR A0 A1 A2 A3 A4 A5) -> Int'.  Calls
/// `extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64'.  The
/// shape matches `nelisp_jit_syscall' precisely; for
/// `nelisp_jit_syscall_supported_p' (which has no params) callers
/// should use `nl-jit-call-i64-i64' with dummy a/b (Cranelift does
/// not exercise extra args of a 0-arg fn under the host C ABI but
/// this is technically UB; supported_p is rarely on the hot path).
pub fn bi_nl_jit_call_syscall(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-syscall", args, 8, Some(8))?;
    let name = as_name("nl-jit-call-syscall", &args[0])?;
    let nr = as_int("nl-jit-call-syscall", &args[1])?;
    let a0 = as_int("nl-jit-call-syscall", &args[2])?;
    let a1 = as_int("nl-jit-call-syscall", &args[3])?;
    let a2 = as_int("nl-jit-call-syscall", &args[4])?;
    let a3 = as_int("nl-jit-call-syscall", &args[5])?;
    let a4 = as_int("nl-jit-call-syscall", &args[6])?;
    let a5 = as_int("nl-jit-call-syscall", &args[7])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-syscall", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(i64 × 7) -> i64' shape — currently `nelisp_jit_syscall'.
    let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
        unsafe { std::mem::transmute(p) };
    let v = f(nr, a0, a1, a2, a3, a4, a5);
    Ok(Sexp::Int(v))
}

// Doc 84 §84.1 — Float-family bridge primitives (xmm marshalling).
// 2 ABI modes (Doc 81 §6): `:trampoline-binary-float-{arith,cmp}'.
// Resolve NAME via `unified_fn_ptr' → 8 `nl_jit_float_*' in `float.rs',
// coerce Sexp args via `num_pair' (Float promotion + canonical WrongType),
// transmute, call.  Arith → Sexp::Float; cmp → Sexp::Int 0/1.
fn float_pair(args: &[Sexp], name: &str) -> Result<(*const u8, f64, f64), EvalError> {
    require_arity(name, args, 3, Some(3))?;
    let sym = as_name(name, &args[0])?;
    let p = unified_fn_ptr(sym).ok_or_else(|| unknown_name_err(name, sym))?;
    let (a, b, _) = crate::eval::builtins::num_pair(&args[1..], name)?;
    Ok((p, a, b))
}

/// `(nl-jit-call-float-float NAME A B) -> Float'.
pub fn bi_nl_jit_call_float_float(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, a, b) = float_pair(args, "nl-jit-call-float-float")?;
    // SAFETY: NAME resolves to `nl_jit_float_{add,sub,mul}' shape.
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

// Doc 87 §86.1.f / §5 (2026-05-10) — `:trampoline-unary-float' bridge.
// `extern "C" fn(f64) -> f64' shape.  System V AMD64 passes the f64
// arg in xmm0 and returns the f64 result in xmm0; arm64 AAPCS uses
// d0 → d0.  Resolves NAME via `unified_fn_ptr', coerces the Sexp arg
// to f64 via the same Int / Float / Nil → 0.0 path the binary float
// bridge takes (= `to_f64'-equivalent inlined here to keep the helper
// local-only), transmutes, calls.

fn float_unary(args: &[Sexp], name: &str) -> Result<(*const u8, f64), EvalError> {
    require_arity(name, args, 2, Some(2))?;
    let sym = as_name(name, &args[0])?;
    let p = unified_fn_ptr(sym).ok_or_else(|| unknown_name_err(name, sym))?;
    let x = match &args[1] {
        Sexp::Int(i) => *i as f64,
        Sexp::Float(f) => *f,
        Sexp::Nil => 0.0,
        other => {
            return Err(EvalError::WrongType {
                expected: "number".into(),
                got: other.clone(),
            });
        }
    };
    Ok((p, x))
}

/// `(nl-jit-call-float-unary NAME X) -> Float'.  Looks up NAME in the
/// unified registry, casts the resolved fn ptr to
/// `extern "C" fn(f64) -> f64', calls it with X, wraps result as
/// `Sexp::Float'.  See `lisp/nelisp-stdlib-math.el' for the elisp
/// wrappers (= `float' / `exp' / `log').
pub fn bi_nl_jit_call_float_unary(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, x) = float_unary(args, "nl-jit-call-float-unary")?;
    // SAFETY: NAME resolves to `nl_jit_float_{float,exp,log}' shape.
    let f: extern "C" fn(f64) -> f64 = unsafe { std::mem::transmute(p) };
    Ok(Sexp::Float(f(x)))
}

// ---------------------------------------------------------------
// Doc 77b Stage b.2.5 — out-param trampoline primitives.
//
// The remaining 9 lowered_X fns (5 cons + 4 access) follow a
// different trampoline shape from Stage b.2's 3 primitives: each
// trampoline takes the input Sexp(s) by `*const Sexp', writes the
// result Sexp into a caller-supplied `*mut Sexp' out-slot, and
// returns `i64' as `TRAMPOLINE_OK = 0' / `TRAMPOLINE_ERR = 1'.
//
// To keep `unsafe' / fn-ptr casts centralized, we expose 4
// primitives covering the 4 trampoline shapes (= 2-arg / 3-arg /
// 3-arg-with-i64 / 4-arg-with-i64).  The ERR case bubbles up as a
// generic `WrongType { expected: "jit-call-out-N", got: <first
// arg> }' so the elisp wrapper (Stage b.4) can `condition-case'
// + re-signal with the proper user-facing message (= same model
// as the existing `lowered_X' fns in cons.rs / access.rs).
// ---------------------------------------------------------------

/// Trampoline ABI: `OK = 0' / `ERR = 1' — matches `cons.rs' /
/// `access.rs'.  Only `OK' is needed here; non-zero is treated as
/// ERR uniformly so we never spell `TRAMPOLINE_ERR' explicitly.
const TRAMPOLINE_OK: i64 = 0;

/// `(nl-jit-call-out-1 NAME ARG) -> Sexp'.  For `car' / `cdr' /
/// `length' (= shape `extern "C" fn(*const Sexp, *mut Sexp) -> i64').
/// Allocates an `out = Sexp::Nil' on the host stack, invokes the
/// resolved trampoline with `(&arg, &mut out)', and returns `out' on
/// `TRAMPOLINE_OK' or a generic `WrongType' on `TRAMPOLINE_ERR'.
pub fn bi_nl_jit_call_out_1(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-out-1", args, 2, Some(2))?;
    let name = as_name("nl-jit-call-out-1", &args[0])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-out-1", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(*const Sexp, *mut Sexp) -> i64' shape — `nelisp_jit_car' /
    // `_cdr' / `_length' qualify.  Other shapes are UB.
    let f: extern "C" fn(*const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        Err(EvalError::WrongType {
            expected: "jit-call-out-1".into(),
            got: args[1].clone(),
        })
    }
}

/// `(nl-jit-call-out-2 NAME ARG1 ARG2) -> Sexp'.  For `cons' /
/// `setcar' / `setcdr' (= shape `extern "C" fn(*const Sexp, *const
/// Sexp, *mut Sexp) -> i64').
pub fn bi_nl_jit_call_out_2(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-out-2", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-out-2", &args[0])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-out-2", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(*const Sexp, *const Sexp, *mut Sexp) -> i64' shape —
    // `nelisp_jit_cons' / `_setcar' / `_setcdr' qualify.
    let f: extern "C" fn(*const Sexp, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(
        &args[1] as *const _,
        &args[2] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        Err(EvalError::WrongType {
            expected: "jit-call-out-2".into(),
            got: args[1].clone(),
        })
    }
}

/// `(nl-jit-call-out-1i NAME ARG IDX) -> Sexp'.  For `aref' / `elt'
/// (= shape `extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64').
pub fn bi_nl_jit_call_out_1i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-out-1i", args, 3, Some(3))?;
    let name = as_name("nl-jit-call-out-1i", &args[0])?;
    let idx = as_int("nl-jit-call-out-1i", &args[2])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-out-1i", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(*const Sexp, i64, *mut Sexp) -> i64' shape — `nelisp_jit_aref'
    // / `_elt' qualify.
    let f: extern "C" fn(*const Sexp, i64, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(&args[1] as *const _, idx, &mut out as *mut _);
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        Err(EvalError::WrongType {
            expected: "jit-call-out-1i".into(),
            got: args[1].clone(),
        })
    }
}

/// `(nl-jit-call-out-2i NAME ARG IDX VAL) -> Sexp'.  For `aset' (=
/// shape `extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) ->
/// i64').
pub fn bi_nl_jit_call_out_2i(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-jit-call-out-2i", args, 4, Some(4))?;
    let name = as_name("nl-jit-call-out-2i", &args[0])?;
    let idx = as_int("nl-jit-call-out-2i", &args[2])?;
    let p = unified_fn_ptr(name)
        .ok_or_else(|| unknown_name_err("nl-jit-call-out-2i", name))?;
    // SAFETY: caller passes a name whose JIT entry has the
    // `(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64' shape —
    // `nelisp_jit_aset' qualifies.
    let f: extern "C" fn(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64 =
        unsafe { std::mem::transmute(p) };
    let mut out = Sexp::Nil;
    let r = f(
        &args[1] as *const _,
        idx,
        &args[3] as *const _,
        &mut out as *mut _,
    );
    if r == TRAMPOLINE_OK {
        Ok(out)
    } else {
        Err(EvalError::WrongType {
            expected: "jit-call-out-2i".into(),
            got: args[1].clone(),
        })
    }
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
