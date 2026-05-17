//! Doc 111 §111.E Group E — `extern "C"` shims that let Phase 47-
//! compiled elisp helpers in `lisp/nelisp-cc-frame-*.el' drive the
//! lexical-frame-stack operations without re-implementing the inner
//! tight loops (= hash-bucket walk, vector grow + copy, etc.) in elisp.
//!
//! Mirrors the design-doc §5.3 trade-off ("first ship can use the Rust
//! helper") and §111.E §3.E table where Group E helpers are wired to
//! `extern-call' targets next to their elisp-side composition.
//!
//! These shims operate on raw `*const Sexp' / `*mut Sexp' pointers
//! (= the `frames_record' Sexp slot owned by `Env') and never touch
//! the surrounding `Env' fields, so they're safe to call from the
//! Phase 47 ABI which only knows the frames-record pointer.
//!
//! The existing private impls in `env_helpers.rs' (`Env::frame_*',
//! previously in `env_lexframe.rs' before Doc 114 Step 5)
//! stay in place — production callers route through `Env::*' while
//! Phase 47 probes drive these shims directly.  Once all Group E
//! helpers ship, a follow-up commit will rewire the `Env::*' methods
//! to dispatch through these shims (= the canonical "extern wrapper"
//! pattern from §3.E).

use crate::eval::env::FrameCell;
use crate::eval::env_helpers::mirror_fnv1a;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlvector::NlVectorRef;
use crate::eval::sexp::Sexp;

// ---- Helpers shared between shims ----------------------------------

/// Read `(stack_rec, backing, depth)' from a `*const Sexp' pointing at
/// the `frames_record' slot inside `Env'.  Returns `None' when the
/// mirror is unbuilt (= `Sexp::Nil', pre-`install_stage0').  Mirrors
/// `Env::frame_stack_view' exactly so the Phase 47 helpers see the
/// same view the Rust impl sees.
unsafe fn frame_stack_view_from_ptr(
    frames_ptr: *const Sexp,
) -> Option<(NlRecordRef, NlVectorRef, usize)> {
    let frames = unsafe { &*frames_ptr };
    let stack_rec = match frames {
        Sexp::Record(r) => r.clone(),
        _ => return None,
    };
    let backing = match stack_rec.slots.get(0)? {
        Sexp::Vector(v) => v.clone(),
        _ => return None,
    };
    let depth = match stack_rec.slots.get(1)? {
        Sexp::Int(n) => *n as usize,
        _ => return None,
    };
    Some((stack_rec, backing, depth))
}

// ---- #20 frame_stack_ensure_capacity --------------------------------
//
// Doc 115 §115.1 — the Rust shim `nl_frame_stack_ensure_capacity'
// has been replaced by the pure-elisp implementation in
// `lisp/nelisp-cc-frame-ensure-capacity.el'.  The grow algorithm
// (= capacity-doubling + depth-aware copy + slot 0 install) now
// runs in Phase 47-compiled elisp via the `vector-make' (§115.1)
// + `vector-slot-set' (§111.E) + `record-slot-set' (§111.B) ops.
// See the safe wrapper `Spike::frame_stack_ensure_capacity' in
// `build-tool/src/lib.rs' for the public entry point.

// ---- #21 frame_push_rust_direct -------------------------------------

/// Doc 111 §111.E #21 — push a fresh empty frame onto the mirror stack.
/// Mirrors `Env::frame_push_rust_direct' bit-for-bit (= allocate a
/// `nelisp-lexframe' record with one `fast-hash-table' slot, install
/// it at `backing[depth]', increment depth).
///
/// Returns 1 on success, 0 when the mirror is unbuilt.  The pushed
/// frame is owned by the backing vector; callers don't get a handle.
///
/// # Safety
/// - `frames_ptr' must be non-null and point at `Env::frames_record'.
/// - No other `&Sexp' borrow into `*frames_ptr' may be live.
#[no_mangle]
pub unsafe extern "C" fn nl_frame_push(frames_ptr: *const Sexp) -> i64 {
    let Some((stack_rec, backing, depth)) = (unsafe { frame_stack_view_from_ptr(frames_ptr) }) else {
        return 0;
    };
    // Grow if needed (same logic as #20 inlined here to avoid double-
    // walking the frames-record).
    let new_backing = {
        let cap = backing.value.len();
        if cap >= depth + 1 {
            backing
        } else {
            let mut new_cap = cap.max(1);
            while new_cap < depth + 1 {
                new_cap *= 2;
            }
            let mut new_buf: Vec<Sexp> = Vec::with_capacity(new_cap);
            for i in 0..depth {
                new_buf.push(backing.value.get(i).cloned().unwrap_or(Sexp::Nil));
            }
            while new_buf.len() < new_cap {
                new_buf.push(Sexp::Nil);
            }
            let new_vec_sexp = Sexp::vector(new_buf);
            let new_vec_box = match &new_vec_sexp {
                Sexp::Vector(v) => v.clone(),
                _ => unreachable!(),
            };
            unsafe { stack_rec.with_slots_mut(|s| s[0] = new_vec_sexp) };
            new_vec_box
        }
    };
    // Build empty frame (= record `nelisp-lexframe' with one
    // `fast-hash-table' slot containing 16 empty buckets).
    const BUCKET_COUNT: usize = 16;
    let buckets = Sexp::vector(vec![Sexp::Nil; BUCKET_COUNT]);
    let ht_record = Sexp::record(
        Sexp::Symbol("fast-hash-table".into()),
        vec![Sexp::Int(BUCKET_COUNT as i64), buckets, Sexp::Int(0)],
    );
    let frame = Sexp::record(
        Sexp::Symbol("nelisp-lexframe".into()),
        vec![ht_record],
    );
    // SAFETY: see `Env::frame_push_rust_direct'.
    unsafe {
        new_backing.with_value_mut(|v| v[depth] = frame);
        stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth + 1) as i64));
    }
    1
}

// ---- #22 frame_pop_rust_direct --------------------------------------
//
// Doc 115 §115.2 — the Rust shim `nl_frame_pop' has been replaced by
// the pure-elisp implementation in `lisp/nelisp-cc-frame-pop.el'.
// The two-step refcount-safe write (= backing[depth-1] := Nil + depth
// := Int(depth-1)) now runs in Phase 47-compiled elisp via the
// `vector-slot-set' (§111.E) + `sexp-int-make' (§100) + `record-slot-
// set' (§111.B) ops.  See the safe wrapper `Spike::frame_pop' in
// `build-tool/src/lib.rs' for the public entry point.

// ---- #23 frame_bind_rust_direct -------------------------------------

/// Doc 111 §111.E #23 — bind NAME → CELL in the innermost frame.
/// Hashes NAME via `mirror_fnv1a' + walks the bucket alist to update-
/// in-place or prepend `(NAME . CELL)'.  No-op when the stack is empty
/// or the mirror is unbuilt.
///
/// Returns 1 on bind, 0 on no-op.
///
/// # Safety
/// - `frames_ptr' must be non-null and point at `Env::frames_record'.
/// - `name_ptr' must point at a `Sexp::Str' or `Sexp::Symbol'.
/// - `cell_ptr' must point at the new cell value (commonly `Sexp::Cell').
#[no_mangle]
pub unsafe extern "C" fn nl_frame_bind(
    frames_ptr: *const Sexp,
    name_ptr: *const Sexp,
    cell_ptr: *const Sexp,
) -> i64 {
    let Some((_stack_rec, backing, depth)) = (unsafe { frame_stack_view_from_ptr(frames_ptr) }) else {
        return 0;
    };
    if depth == 0 {
        return 0;
    }
    let frame = match backing.value.get(depth - 1) {
        Some(f) => f.clone(),
        None => return 0,
    };
    let name: String = match unsafe { &*name_ptr } {
        Sexp::Str(s) => s.clone(),
        Sexp::Symbol(s) => s.clone(),
        _ => return 0,
    };
    let cell = unsafe { (*cell_ptr).clone() };
    bind_into_frame(&frame, &name, cell);
    1
}

/// Internal — mirror of `Env::frame_bind_into' but standalone (no
/// `Env' self).  Hashes NAME via `mirror_fnv1a' + walks the bucket
/// alist; the in-place pair-cdr update path uses
/// `NlConsBoxRef::set_cdr' so refcounts stay tidy.
fn bind_into_frame(frame: &Sexp, name: &str, cell: Sexp) {
    let Sexp::Record(frame_rec) = frame else { return };
    let ht_rec = match frame_rec.slots.get(0) {
        Some(Sexp::Record(r)) => r.clone(),
        _ => return,
    };
    let bucket_count = match ht_rec.slots.get(0) {
        Some(Sexp::Int(n)) => *n as u32,
        _ => return,
    };
    let buckets = match ht_rec.slots.get(1) {
        Some(Sexp::Vector(v)) => v.clone(),
        _ => return,
    };
    let h = mirror_fnv1a(name);
    let idx = (if bucket_count & (bucket_count - 1) == 0 {
        h & (bucket_count - 1)
    } else {
        h % bucket_count
    }) as usize;
    let bucket = match buckets.value.get(idx) {
        Some(b) => b.clone(),
        None => return,
    };
    let mut cur = bucket;
    while let Sexp::Cons(c) = &cur {
        if let Sexp::Cons(pair) = &c.car {
            if let Sexp::Str(k) = &pair.car {
                if k == name {
                    // SAFETY: in-place pair-cdr replacement.
                    unsafe { pair.set_cdr(cell) };
                    return;
                }
            }
        }
        cur = c.cdr.clone();
    }
    // Not found — prepend `(NAME . CELL)' to the bucket.
    let pair = Sexp::cons(Sexp::Str(name.to_string()), cell);
    // SAFETY: see `Env::frame_bind_into'.
    unsafe {
        buckets.with_value_mut(|v| {
            let old = v.get(idx).cloned().unwrap_or(Sexp::Nil);
            v[idx] = Sexp::cons(pair, old);
        });
        ht_rec.with_slots_mut(|s| {
            if let Some(Sexp::Int(n)) = s.get_mut(2) {
                *n += 1;
            }
        });
    }
}

// ---- #24 frame_stack_find_rust_direct -------------------------------
//
// Doc 115 §115.6 — the Rust shim `nl_frame_stack_find' + private
// `lookup_in_frame' helper have been replaced by the pure-elisp
// implementation in `lisp/nelisp-cc-frame-stack-find.el'.  The
// innermost-first stack walk + per-frame hash-bucket lookup (= cons-
// walk + str-eq) now runs in Phase 47-compiled elisp via the
// `record-slot-ref-ptr' / `vector-ref-ptr' / `sexp-payload-ptr' /
// `cons-cdr-raw-from-box' / `str-eq' / `logand' / `extern-call'
// (= `nl_mirror_fnv1a_sexp', itself slated for §115.7 elisp rewrite)
// ops.  See the safe wrapper `Spike::frame_stack_find' in
// `build-tool/src/lib.rs' for the public entry point.

// ---- #26 wrap_alist_cells -------------------------------------------

/// Doc 111 §111.E #26 — walk ALIST = `((NAME . VALUE-OR-CELL) ...)' and
/// produce a new alist where every cdr is a `Sexp::Cell'.  Bare values
/// are wrapped in fresh `NlCellRef' so the elisp lexframe-bind path
/// stores write-through cells.  Writes the result into `result_slot'.
///
/// Returns 1 on success, 0 on malformed input (= matches the
/// `Env::wrap_alist_cells' Err arm).  The result slot is initialised
/// to `Sexp::Nil' on error.
///
/// # Safety
/// - `alist_ptr' must be non-null and point at a proper-list `Sexp'.
/// - `result_slot' must be non-null, writable, and either pre-set to
///   `Sexp::Nil' (Copy-shape) or treated by the caller as
///   uninitialised; the helper drops the prior contents before write.
#[no_mangle]
pub unsafe extern "C" fn nl_wrap_alist_cells(
    alist_ptr: *const Sexp,
    result_slot: *mut Sexp,
) -> i64 {
    let alist = unsafe { &*alist_ptr };
    let mut entries: Vec<(Sexp, Sexp)> = Vec::new();
    let mut cur = alist;
    while let Sexp::Cons(outer) = cur {
        let inner = match &outer.car {
            Sexp::Cons(c) => c,
            _ => {
                unsafe { core::ptr::write(result_slot, Sexp::Nil) };
                return 0;
            }
        };
        let name = inner.car.clone();
        let cell = match &inner.cdr {
            Sexp::Cell(_) => inner.cdr.clone(),
            v => Sexp::Cell(FrameCell::new(v.clone())),
        };
        entries.push((name, cell));
        cur = &outer.cdr;
    }
    if !matches!(cur, Sexp::Nil) {
        unsafe { core::ptr::write(result_slot, Sexp::Nil) };
        return 0;
    }
    let mut acc = Sexp::Nil;
    for (name, cell) in entries.into_iter().rev() {
        acc = Sexp::cons(Sexp::cons(name, cell), acc);
    }
    // SAFETY: result_slot is caller-owned per the contract above.
    unsafe { core::ptr::write(result_slot, acc) };
    1
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_frames_record() -> Sexp {
        const INITIAL_CAPACITY: usize = 8;
        let backing = Sexp::vector(vec![Sexp::Nil; INITIAL_CAPACITY]);
        Sexp::record(
            Sexp::Symbol("nelisp-lexframe-stack".into()),
            vec![backing, Sexp::Int(0)],
        )
    }

    // Doc 115 §115.2 — the `nl_frame_push_pop_roundtrip' unit test was
    // tied to the deleted `nl_frame_pop' Rust shim.  Pop-side coverage
    // moves to the integration probe at
    // `tests/elisp_cc_frame_pop_probe.rs' which drives the pure-elisp
    // implementation end-to-end (= 3 tests covering empty-stack
    // no-op, single push+pop, and nested 3x push + 3x pop walk).

    // Doc 115 §115.6 — the `nl_frame_bind_then_find_roundtrip' test
    // was tied to the deleted `nl_frame_stack_find' Rust shim.
    // Coverage moves to the integration probe at
    // `tests/elisp_cc_frame_stack_find_probe.rs' which drives the
    // pure-elisp implementation end-to-end (= 4 tests covering empty
    // stack, innermost hit, outer-frame walk, and inner-shadow
    // priority).

    #[test]
    fn nl_wrap_alist_cells_basic() {
        let alist = Sexp::list_from(&[
            Sexp::cons(Sexp::Symbol("a".into()), Sexp::Int(1)),
            Sexp::cons(Sexp::Symbol("b".into()), Sexp::Int(2)),
        ]);
        let mut out = Sexp::Nil;
        let rc = unsafe {
            nl_wrap_alist_cells(&alist as *const Sexp, &mut out as *mut Sexp)
        };
        assert_eq!(rc, 1);
        // Walk: ((a . #<cell 1>) (b . #<cell 2>))
        let Sexp::Cons(outer) = &out else { panic!("expected cons") };
        let Sexp::Cons(first) = &outer.car else { panic!("inner not cons") };
        assert!(matches!(&first.cdr, Sexp::Cell(_)));
    }

    // Doc 115 §115.1 — the `nl_frame_stack_ensure_capacity_grows'
    // test was tied to the deleted Rust shim.  Coverage moves to the
    // integration probe at `tests/elisp_cc_frame_stack_ensure_capacity_probe.rs'
    // which drives the pure-elisp implementation end-to-end (= 3
    // tests covering no-grow, grow-to-next-pow2, and multi-doubling).
}
