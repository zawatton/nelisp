//! Doc 102 Phase 8 — lexical frame stack helpers extracted from
//! `eval/env.rs`.  The Rust-direct walk of the elisp-side
//! `nelisp-lexframe-stack' record (= `Env::frames_record').  Replaces
//! the pre-Doc-104 `frames: Vec<HashMap<String, FrameCell>>' field;
//! Doc 104 Stage 3.a-3.e covers the Vec → Sexp::Record migration,
//! Doc 102 Phase 4.b-4.b adds the elisp-dispatch path for
//! `capture_lexical' / `push_captured'.  See
//! `lisp/nelisp-lexframe.el' for the elisp layout side.

use super::env::{Env, FrameCell};
use super::env_mirror::mirror_fnv1a;
use super::error::EvalError;
use super::sexp::Sexp;

impl Env {
    /// Doc 104 Stage 3.b — Rust-direct empty lexframe-stack constructor.
    /// Pre-allocates the `nelisp-lexframe-stack' record graph without
    /// invoking elisp `nelisp-lexframe-stack-make' (= no `apply_function'
    /// detour, no STDLIB dependency).  Matches the layout in
    /// `lisp/nelisp-lexframe.el':
    ///   frames_record = Sexp::Record(`nelisp-lexframe-stack')
    ///     slots[0] = Sexp::Vector(BACKING, length = INITIAL_CAPACITY,
    ///                             all entries Sexp::Nil)
    ///     slots[1] = Sexp::Int(0)            (= DEPTH)
    pub(crate) fn install_empty_frames_record_rust_direct(&mut self) {
        const INITIAL_CAPACITY: usize = 8;
        let backing = Sexp::vector(vec![Sexp::Nil; INITIAL_CAPACITY]);
        self.frames_record = Sexp::record(
            Sexp::Symbol("nelisp-lexframe-stack".into()),
            vec![backing, Sexp::Int(0)],
        );
    }

    // ---- Doc 104 Stage 3.b — Rust-direct frame helpers ----
    //
    // These parallel the `mirror_*' globals helpers (= walk the elisp
    // `nelisp-lexframe-stack' record via NlRecordRef / NlVectorRef
    // instead of `apply_function').

    /// Layout walker — fetch the backing vector + current depth from
    /// `self.frames_record'.  Returns None when the mirror is unbuilt
    /// (= `Sexp::Nil', pre-`install_stage0').
    pub(crate) fn frame_stack_view(&self) -> Option<(crate::eval::nlrecord::NlRecordRef, crate::eval::nlvector::NlVectorRef, usize)> {
        let stack_rec = match &self.frames_record { Sexp::Record(r) => r.clone(), _ => return None };
        let backing = match stack_rec.slots.get(0)? { Sexp::Vector(v) => v.clone(), _ => return None };
        let depth = match stack_rec.slots.get(1)? { Sexp::Int(n) => *n as usize, _ => return None };
        Some((stack_rec, backing, depth))
    }

    /// Build a fresh empty `nelisp-lexframe' record (= type-tagged
    /// record containing a single fast-hash-table slot).  Internal —
    /// callers use `frame_push_rust_direct' to push it.
    fn make_empty_frame_record() -> Sexp {
        const BUCKET_COUNT: usize = 16;
        let buckets = Sexp::vector(vec![Sexp::Nil; BUCKET_COUNT]);
        let ht_record = Sexp::record(
            Sexp::Symbol("fast-hash-table".into()),
            vec![Sexp::Int(BUCKET_COUNT as i64), buckets, Sexp::Int(0)],
        );
        Sexp::record(
            Sexp::Symbol("nelisp-lexframe".into()),
            vec![ht_record],
        )
    }

    /// Grow `backing' if `needed > backing.len()' via capacity doubling
    /// + copy-across, mutating `stack_rec.slots[0]' in place to point at
    /// the new backing vector.  Mirrors
    /// `nelisp-lexframe-stack--ensure-capacity'.
    pub(crate) fn frame_stack_ensure_capacity(
        stack_rec: &crate::eval::nlrecord::NlRecordRef,
        backing: &crate::eval::nlvector::NlVectorRef,
        depth: usize,
        needed: usize,
    ) -> crate::eval::nlvector::NlVectorRef {
        let cap = backing.value.len();
        if cap >= needed {
            return backing.clone();
        }
        let mut new_cap = cap.max(1);
        while new_cap < needed {
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
        let new_vec_ref = match &new_vec_sexp { Sexp::Vector(v) => v.clone(), _ => unreachable!() };
        // SAFETY: no outstanding borrow into `stack_rec.slots'.
        unsafe { stack_rec.with_slots_mut(|s| s[0] = new_vec_sexp) };
        new_vec_ref
    }

    /// Push a fresh empty frame onto the mirror stack.  No-op when the
    /// mirror is unbuilt.  Returns the frame's `Sexp::Record' clone.
    pub(crate) fn frame_push_rust_direct(&mut self) -> Option<Sexp> {
        let (stack_rec, backing, depth) = self.frame_stack_view()?;
        let backing = Env::frame_stack_ensure_capacity(&stack_rec, &backing, depth, depth + 1);
        let frame = Env::make_empty_frame_record();
        // SAFETY: no outstanding borrow into `backing.value' /
        // `stack_rec.slots'.
        unsafe {
            backing.with_value_mut(|v| v[depth] = frame.clone());
            stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth + 1) as i64));
        }
        Some(frame)
    }

    /// Pop the innermost frame.  Silently no-ops on under-pop (= same
    /// semantics as `Vec::pop' on empty).  No-op when the mirror is
    /// unbuilt.
    pub(crate) fn frame_pop_rust_direct(&mut self) {
        let Some((stack_rec, backing, depth)) = self.frame_stack_view() else { return };
        if depth == 0 { return; }
        let new_depth = depth - 1;
        // SAFETY: see `frame_push_rust_direct'.
        unsafe {
            backing.with_value_mut(|v| v[new_depth] = Sexp::Nil);
            stack_rec.with_slots_mut(|s| s[1] = Sexp::Int(new_depth as i64));
        }
    }

    /// Bind NAME → CELL in the innermost frame.  No-op when the stack
    /// is empty OR the mirror is unbuilt.  CELL is stored verbatim
    /// (= callers wrap in `Sexp::Cell' for closure write-through).
    pub(crate) fn frame_bind_rust_direct(&mut self, name: &str, cell: Sexp) {
        let Some((_stack_rec, backing, depth)) = self.frame_stack_view() else { return };
        if depth == 0 { return; }
        let frame = match backing.value.get(depth - 1) { Some(f) => f.clone(), None => return };
        Env::frame_bind_into(&frame, name, cell);
    }

    /// Internal — `nelisp-lexframe-bind' for a frame record handle.
    /// Hashes NAME via `mirror_fnv1a' + walks the bucket alist to
    /// update-in-place or prepend a fresh `(NAME . CELL)' pair.
    fn frame_bind_into(frame: &Sexp, name: &str, cell: Sexp) {
        let Sexp::Record(frame_rec) = frame else { return };
        let ht_rec = match frame_rec.slots.get(0) { Some(Sexp::Record(r)) => r.clone(), _ => return };
        let bucket_count = match ht_rec.slots.get(0) { Some(Sexp::Int(n)) => *n as u32, _ => return };
        let buckets = match ht_rec.slots.get(1) { Some(Sexp::Vector(v)) => v.clone(), _ => return };
        let h = mirror_fnv1a(name);
        let idx = (if bucket_count & (bucket_count - 1) == 0 {
            h & (bucket_count - 1)
        } else {
            h % bucket_count
        }) as usize;
        // First pass — see if NAME already lives in the bucket; if so,
        // mutate the existing pair's cdr in place.
        let bucket = match buckets.value.get(idx) { Some(b) => b.clone(), None => return };
        let mut cur = bucket;
        while let Sexp::Cons(c) = &cur {
            if let Sexp::Cons(pair) = &c.car {
                if let Sexp::Str(k) = &pair.car {
                    if k == name {
                        // SAFETY: replace pair.cdr in place.
                        unsafe { pair.set_cdr(cell) };
                        return;
                    }
                }
            }
            cur = c.cdr.clone();
        }
        // Not found — prepend.
        let pair = Sexp::cons(Sexp::Str(name.to_string()), cell);
        // SAFETY: no outstanding borrow into `buckets.value' /
        // `ht_rec.slots'.
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

    /// Internal — `nelisp-lexframe-lookup' for a frame record handle.
    /// Returns `Some(cell)' when NAME is bound, `None' when absent.
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by stack walks + tests
    fn frame_lookup_in(frame: &Sexp, name: &str) -> Option<Sexp> {
        let Sexp::Record(frame_rec) = frame else { return None };
        let ht_rec = match frame_rec.slots.get(0)? { Sexp::Record(r) => r, _ => return None };
        let bucket_count = match ht_rec.slots.get(0)? { Sexp::Int(n) => *n as u32, _ => return None };
        let buckets = match ht_rec.slots.get(1)? { Sexp::Vector(v) => v, _ => return None };
        let h = mirror_fnv1a(name);
        let idx = (if bucket_count & (bucket_count - 1) == 0 {
            h & (bucket_count - 1)
        } else {
            h % bucket_count
        }) as usize;
        let bucket = buckets.value.get(idx)?;
        let mut cur = bucket;
        while let Sexp::Cons(c) = cur {
            if let Sexp::Cons(pair) = &c.car {
                if let Sexp::Str(k) = &pair.car {
                    if k == name {
                        return Some(pair.cdr.clone());
                    }
                }
            }
            cur = &c.cdr;
        }
        None
    }

    /// Look up NAME in the innermost frame only.  Returns `None' when
    /// stack is empty / mirror unbuilt / NAME absent.
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by stack walks + tests
    pub(crate) fn frame_lookup_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        if depth == 0 { return None; }
        let frame = backing.value.get(depth - 1)?;
        Env::frame_lookup_in(frame, name)
    }

    /// Innermost-first walk across the entire mirror stack.  Returns
    /// the first NAME hit.  Mirrors `nelisp-lexframe-stack-find' +
    /// `find_frame_cell'.  Doc 104 Stage 3.c — `find_frame_cell' now
    /// delegates here.
    pub(crate) fn frame_stack_find_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        for i in (0..depth).rev() {
            let Some(frame) = backing.value.get(i) else { continue };
            if let Some(cell) = Env::frame_lookup_in(frame, name) {
                return Some(cell);
            }
        }
        None
    }

    /// Walk ALIST = `((NAME . VALUE-OR-CELL) ...)' and produce a new
    /// alist where every cdr is a `Sexp::Cell'.  Bare values are
    /// wrapped in fresh `NlCellRef's so the elisp lexframe-bind path
    /// stores write-through cells.  Used by `push_captured' before
    /// the elisp dispatch.
    pub(crate) fn wrap_alist_cells(alist: &Sexp) -> Result<Sexp, EvalError> {
        let mut entries: Vec<(Sexp, Sexp)> = Vec::new();
        let mut cur = alist;
        while let Sexp::Cons(outer) = cur {
            let Sexp::Cons(inner) = &outer.car else {
                return Err(EvalError::Internal("closure env entry not a cons".into()));
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
            return Err(EvalError::Internal("closure env not a proper list".into()));
        }
        let mut acc = Sexp::Nil;
        for (name, cell) in entries.into_iter().rev() {
            acc = Sexp::cons(Sexp::cons(name, cell), acc);
        }
        Ok(acc)
    }
}
