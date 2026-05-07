//! Phase 5 Stage 5.1 — SyscallIR lower (= 25 specialized + 1 generic).
//!
//! Stage 5.0 stub: empty `register` only.  Stage 5.1 lands the actual
//! Cranelift IR + emit logic; until then this submodule is a no-op so
//! `lower_entries` reports zero entries and every call falls through
//! to the existing `bi_syscall_*` dispatchers.

use std::collections::HashMap;

use super::LowerFn;

#[allow(unused_variables)]
pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.0 — no entries yet.
}
