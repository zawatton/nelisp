//! Phase 5 — Cranelift JIT lowering for hot-path primitives.
//!
//! Stage 5.0 (2026-05-07, Doc 62) — scaffold only.
//!
//! This module hosts the `lower_entries` registry that maps a
//! primitive name (e.g. `nelisp--syscall`, `nelisp--add2`, `car`) to
//! a JIT-compiled lowering of that primitive.  The eval loop
//! (`build-tool/src/eval/mod.rs`) consults `lower_entries` BEFORE the
//! generic `dispatch` for every call site:
//!
//! ```text
//!   call site name → lower_entries.get(name)
//!     ├─ Some(jit_fn) → jit_fn(args, env)        (= JIT path)
//!     └─ None         → dispatch(name, args, env) (= fallback)
//! ```
//!
//! In Stage 5.0 the registry is intentionally **empty**; the eval-loop
//! hook is wired so subsequent stages (5.1〜5.5) can plug their lower
//! entries in without further plumbing changes.  All existing
//! primitives continue to flow through the dispatch fallback, giving
//! us a no-op build with the JIT scaffold present.
//!
//! The 5 lowering submodules each follow the same pattern: a
//! `register(&mut HashMap<...>)` entry-point that the global builder
//! calls once at startup.  A submodule with no lowerings yet (= all
//! 5 today) is a no-op.

use std::collections::HashMap;
use std::sync::OnceLock;

use crate::eval::error::EvalError;
use crate::eval::env::Env;
use crate::reader::sexp::Sexp;

/// Lowered primitive function signature.  Identical to
/// `eval::builtins::dispatch` so a JIT lowering and the fallback
/// dispatcher are interchangeable from the eval loop's perspective.
pub type LowerFn = fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>;

mod access;
mod arith;
mod cons;
mod predicate;
mod syscall;

/// Return the global lowering registry.  Initialized lazily on first
/// access (= the first eval-loop call site that consults it).
///
/// In Stage 5.0 every call to `lower_entries().get(name)` returns
/// `None`, routing the eval loop to the existing dispatcher.  As
/// subsequent stages register entries via the `register(...)`
/// entry-points below, hot-path primitives short-circuit through the
/// JIT path here without any further eval-loop changes.
pub fn lower_entries() -> &'static HashMap<&'static str, LowerFn> {
    static ENTRIES: OnceLock<HashMap<&'static str, LowerFn>> = OnceLock::new();
    ENTRIES.get_or_init(|| {
        let mut map: HashMap<&'static str, LowerFn> = HashMap::new();
        // Stage 5.1: SyscallIR — `nelisp--syscall*' family.
        syscall::register(&mut map);
        // Stage 5.2: ArithIR — 2-arg arithmetic / comparison.
        arith::register(&mut map);
        // Stage 5.3: ConsIR — car / cdr / cons / setcar / setcdr.
        cons::register(&mut map);
        // Stage 5.4: AccessIR — aref / aset / length / elt.
        access::register(&mut map);
        // Stage 5.5: PredicateIR — eq / atom / consp / listp / null / *p.
        predicate::register(&mut map);
        map
    })
}

/// Try to lower a call site through the JIT registry.  Returns
/// `Some(result)` when a lowering is registered for `name`, else
/// `None` so the caller falls back to the generic dispatcher.
pub fn try_lower(
    name: &str,
    args: &[Sexp],
    env: &mut Env,
) -> Option<Result<Sexp, EvalError>> {
    lower_entries().get(name).map(|f| f(args, env))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_covers_stage_5_2_arithmetic() {
        // Stage 5.2 contract: 11 binary arithmetic / comparison /
        // bitwise primitives + `ash' are JIT-lowered.  Stage 5.1
        // ships `nelisp--syscall' / `-supported-p' (= 2 entries) which
        // are checked separately.  As 5.3 / 5.4 / 5.5 land this
        // assertion grows accordingly.
        let needed = [
            "nelisp--add2", "nelisp--sub2", "nelisp--mul2",
            "nelisp--num-eq2", "nelisp--num-lt2", "nelisp--num-gt2",
            "nelisp--num-le2", "nelisp--num-ge2",
            "nelisp--logior2", "nelisp--logand2", "nelisp--logxor2",
            "ash",
            "nelisp--syscall", "nelisp--syscall-supported-p",
        ];
        for name in needed {
            assert!(
                lower_entries().contains_key(name),
                "lower_entries missing `{}'; found: {:?}",
                name,
                lower_entries().keys().collect::<Vec<_>>(),
            );
        }
    }
}
