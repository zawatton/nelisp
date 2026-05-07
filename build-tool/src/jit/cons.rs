//! Phase 5 Stage 5.3 — ConsIR lower (= car / cdr / cons / setcar / setcdr).
//!
//! Stage 5.0 stub.  Stage 5.3 will lower `bi_car` / `bi_cdr` / `bi_cons`
//! / `bi_setcar` / `bi_setcdr`.  Inline emit of `Rc<RefCell<Sexp>>`
//! offsets is fragile (= depends on std internals); the lowering will
//! call into stable `unsafe extern "C"` helpers added on the Rust
//! side in the same stage.

use std::collections::HashMap;

use super::LowerFn;

#[allow(unused_variables)]
pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.0 — no entries yet.
}
