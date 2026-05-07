//! Phase 5 Stage 5.4 — AccessIR lower (= aref / aset / length / elt).
//!
//! Stage 5.0 stub.  Stage 5.4 will lower vector/string indexing on top
//! of the Stage 5.3 ConsIR helpers (= same `Rc<RefCell<Vec<Sexp>>>`
//! ABI fragility, same helper-fn workaround).

use std::collections::HashMap;

use super::LowerFn;

#[allow(unused_variables)]
pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.0 — no entries yet.
}
