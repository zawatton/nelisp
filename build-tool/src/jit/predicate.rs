//! Phase 5 Stage 5.5 — PredicateIR lower (= eq / atom / consp / listp /
//! null / integerp / stringp / symbolp / numberp / floatp / vectorp).
//!
//! Stage 5.0 stub.  Stage 5.5 will lower `Sexp` variant tag tests to a
//! `movzx + cmp + sete` sequence (= 3 host instructions per predicate)
//! once the `Sexp` representation is stable enough to expose the tag
//! offset to the JIT.  Until then every predicate flows through the
//! generic dispatcher.

use std::collections::HashMap;

use super::LowerFn;

#[allow(unused_variables)]
pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.0 — no entries yet.
}
