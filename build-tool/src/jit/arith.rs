//! Phase 5 Stage 5.2 — ArithIR lower (= 13 arithmetic primitives).
//!
//! Stage 5.0 stub.  Stage 5.2 will register lower entries for:
//! `nelisp--add2` / `-sub2` / `-mul2` / `-num-{eq,lt,gt,le,ge}2` /
//! `-logior2` / `-logand2` / `-logxor2` / `lognot` / `ash`.

use std::collections::HashMap;

use super::LowerFn;

#[allow(unused_variables)]
pub fn register(map: &mut HashMap<&'static str, LowerFn>) {
    // Stage 5.0 — no entries yet.
}
