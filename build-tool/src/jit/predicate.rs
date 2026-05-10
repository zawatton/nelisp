//! Phase 7.1.6.d (Doc 28 §3.6.d) — predicate trampoline, dlsym-exported.
//!
//! Pre-7.1.6.d this module hosted a 7-block Cranelift IR builder that
//! emitted the `eq' predicate's same-ref / tag-byte / int-payload fast
//! paths inline plus a slow-path call to `nl_jit_pred_eq' (= a Rust
//! wrapper around `eval::special_forms::sexp_eq').  The Cranelift IR
//! was wrapped by a `JitPredicate' fn-ptr struct brought up at first-
//! access by the unified JITModule — same architecture as the cons /
//! access / arith clusters deleted in 7.1.6.a.2 / .b / .c.
//!
//! Doc 81 Stage 81.4 + Phase 7.1.6.a.1 dlsym precursor (`6666e61')
//! shipped the elisp-side replacement infrastructure.  Per Doc 28
//! §3.6.d, the predicate cluster mirrors the arith pattern: predicate
//! had no pre-existing single-Rust-trampoline body that fully covered
//! the Cranelift IR's fast paths — the IR `was' the implementation for
//! the same-ref / tag-eq / int-payload arms, with `nl_jit_pred_eq' only
//! handling the slow variant-specific arm.  This sub-stage collapses
//! the 7-block IR into a single Rust trampoline body that mirrors the
//! IR's control flow 1-to-1 (= same-ref check, tag-byte equality test,
//! Int payload fast path, then `sexp_eq' fallback for other variants).
//!
//! Phase 7.1.6.d (this commit) deletes:
//!
//!   - `JitPredicate' / `PredicateIds' fn-ptr structs.
//!   - `declare_eq_inline' Cranelift IR builder.
//!   - `register_symbols' / `declare_funcs' / `collect_funcs' wiring
//!     (= `unified_jit()' no longer constructs a predicate cluster JIT
//!     wrapper page).
//!   - `nl_jit_pred_eq' helper trampoline (= subsumed into the new
//!     `nl_jit_predicate_eq' body's slow-path arm).
//!
//! What stays (= the surface this module owns post-7.1.6.d):
//!
//!   - 1 plain Rust trampoline `nl_jit_predicate_eq' that mirrors the
//!     deleted Cranelift IR semantics 1-to-1: same-ref short-circuit,
//!     tag-byte equality test (= early-out when variants differ), Int
//!     payload fast path (= avoids any helper call for `eq' between
//!     two Sexp::Int), and `sexp_eq' slow path for variant-specific
//!     equality (= Symbol-by-name, Cons-by-Rc-ptr-eq, Str-by-content,
//!     etc.).  `#[no_mangle] pub unsafe extern "C"' so the dlsym bridge
//!     can resolve it at runtime.
//!
//! `nelisp-jit-strategy.el' still calls `(nl-jit-call-ptr-ptr
//! "nelisp_jit_eq_inline" a b)' which goes through `bridge::
//! unified_fn_ptr'.  Post-7.1.6.d that name resolves directly to
//! `nl_jit_predicate_eq' — no Cranelift wrapper page in between (= one
//! fewer indirection, same as cons / access / arith takeover).
//!
//! The `-rdynamic' link flag in `.cargo/config.toml' (= already added
//! by Phase 7.1.6.a.2; predicate trampoline just inherits) pushes the
//! `#[no_mangle]' symbol into the binary's dynamic symbol table so
//! `dlsym(RTLD_DEFAULT, ...)' can locate it at runtime.

use crate::eval::sexp::{Sexp, SEXP_TAG_INT};

/// `(eq A B) -> 1 if equal, 0 otherwise' trampoline.
///
/// Mirrors the deleted 7-block Cranelift IR semantics:
///   1. same-ref short-circuit (= `a_ptr == b_ptr' → return 1
///      regardless of variant; idiom in pcase / cl-typecase guard
///      chains).
///   2. tag-byte equality test (= different variants → return 0
///      without entering the helper).
///   3. Int payload fast path (= matching `Sexp::Int' tags → compare
///      i64 payloads at offset 8 directly).
///   4. `sexp_eq' slow path for variant-specific equality (= same as
///      the Cranelift IR's `block_slow' arm).
///
/// Phase 7.1.6.d dlsym-exported.
///
/// SAFETY: caller must pass valid `*const Sexp' pointers.  Same
/// contract as the deleted Cranelift IR (= identical PTR ABI).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_predicate_eq(
    a: *const Sexp,
    b: *const Sexp,
) -> i64 {
    // 1. Same-ref short-circuit.
    if a == b {
        return 1;
    }
    // 2. Tag-byte equality test (= load discriminant byte at offset 0).
    //    `Sexp' is `#[repr(C, u8)]' so the tag occupies byte 0; matches
    //    the deleted IR's `load.i8 [a_ptr + 0]' / `load.i8 [b_ptr + 0]'.
    let a_tag = *(a as *const u8);
    let b_tag = *(b as *const u8);
    if a_tag != b_tag {
        return 0;
    }
    // 3. Int fast path: matching `SEXP_TAG_INT' → compare i64 payload at
    //    offset 8.  Mirrors the deleted IR's `block_int_eq' which loaded
    //    `[a_ptr + 8]' / `[b_ptr + 8]' as i64 and emitted `icmp Equal +
    //    uextend'.
    if a_tag == SEXP_TAG_INT {
        let a_int = *((a as *const u8).add(8) as *const i64);
        let b_int = *((b as *const u8).add(8) as *const i64);
        return (a_int == b_int) as i64;
    }
    // 4. Slow path: variant-specific equality through `sexp_eq' (=
    //    Symbol-by-name, Cons-by-Rc-ptr-eq, Str-by-content, etc.).
    //    Mirrors the deleted IR's `block_slow' arm which called the
    //    `nl_jit_pred_eq' helper (= itself a thin wrapper around
    //    `sexp_eq').  Inlining the helper body here saves a hop.
    if crate::eval::special_forms::sexp_eq(&*a, &*b) {
        1
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Inline fast paths ---

    #[test]
    fn jit_eq_int_equal_inline() {
        // Same Int → trampoline int fast path returns 1.
        let a = Sexp::Int(7);
        let b = Sexp::Int(7);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 1);
    }

    #[test]
    fn jit_eq_int_unequal_inline() {
        // Different Int → trampoline int fast path returns 0.
        let a = Sexp::Int(7);
        let b = Sexp::Int(8);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
    }

    #[test]
    fn jit_eq_tag_mismatch_short_circuits() {
        // Different tags → trampoline tag-byte arm returns 0 without
        // entering the slow path.  Verifies the Sexp::Int vs Sexp::Float
        // pairing (= same payload size, different tag) routes to the
        // diff arm, not the int_eq fast path.
        let a = Sexp::Int(0);
        let b = Sexp::Float(0.0);
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
    }

    #[test]
    fn jit_eq_nil_t_via_helper() {
        // Nil/T have matching tags but no payload — the slow path
        // handles this through `sexp_eq''s `(Nil, Nil) | (T, T) => true'
        // arm.
        let nil = Sexp::Nil;
        let t = Sexp::T;
        assert_eq!(unsafe { nl_jit_predicate_eq(&nil, &nil) }, 1);
        assert_eq!(unsafe { nl_jit_predicate_eq(&t, &t) }, 1);
        // Mismatched tags → diff arm inline.
        assert_eq!(unsafe { nl_jit_predicate_eq(&nil, &t) }, 0);
    }

    // --- Slow paths via sexp_eq ---

    #[test]
    fn jit_eq_symbol_by_name_via_helper() {
        // Symbol matches via `sexp_eq''s name-equality arm.
        let a = Sexp::Symbol("foo".into());
        let b = Sexp::Symbol("foo".into());
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 1);
    }

    #[test]
    fn jit_eq_cons_identity_via_helper() {
        // Two separately-constructed cons cells with same value are
        // NOT eq (= identity check via Rc::ptr_eq inside `sexp_eq',
        // reached after both same-ref check and tag-equal branches).
        let a = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        let b = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &b) }, 0);
        // The same cell IS eq with itself — same-ref short-circuit
        // returns 1 from the first arm before any helper call.
        assert_eq!(unsafe { nl_jit_predicate_eq(&a, &a) }, 1);
    }

    // --- Same-ref short-circuit ---

    #[test]
    fn jit_eq_same_ref_short_circuit() {
        // For every variant, comparing a Sexp ref to itself must
        // return 1 without entering the slow path.  Pre-7.1.6.d this
        // worked for Int via the `block_int_eq' Cranelift IR arm and
        // for non-Int variants via `block_same' (= the inline same-ref
        // check); now both are unified in the trampoline's first arm.
        let int = Sexp::Int(42);
        let flt = Sexp::Float(3.14);
        let nil = Sexp::Nil;
        let t = Sexp::T;
        let sym = Sexp::Symbol("x".into());
        let s = Sexp::Str("hello".into());
        let cons = Sexp::cons(Sexp::Int(1), Sexp::Int(2));
        for r in [&int, &flt, &nil, &t, &sym, &s, &cons] {
            let p = r as *const Sexp;
            assert_eq!(unsafe { nl_jit_predicate_eq(p, p) }, 1);
        }
    }
}
