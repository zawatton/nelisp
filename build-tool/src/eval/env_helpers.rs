//! Globals mirror + lexical frame stack helpers — Rust-direct walks
//! of the elisp `nelisp-env' / `nelisp-lexframe-stack' records
//! (= `Env::globals_record' / `Env::frames_record').  See
//! `lisp/nelisp-env.el' + `lisp/nelisp-lexframe.el' for the layout.

use std::collections::HashMap;

use super::env::{Env, FrameCell};
use super::env::SymbolEntry;
use super::error::EvalError;
use super::sexp::Sexp;

// SAFETY (applies to every `unsafe' block in this file unless noted):
// pointers passed to `elisp_cc_spike::*' helpers refer to stack-local
// `Sexp' values or `Env'-owned fields (`globals_record',
// `frames_record', `unbound_marker') that outlive the call.  The
// helpers perform refcount-aware clones into result slots; mutating
// helpers (`*_or_insert', `mirror_clear_*') handle both hit (=
// in-place slot-set) and miss (= alloc-entry + bucket-prepend, Doc
// 119 §119.A auto-vivify) paths internally.

impl Env {
    // ---- Globals mirror helpers ----

    /// Set the value cell.  Doc 119 §119.A — hit + miss both in elisp.
    pub(crate) fn mirror_set_value(&mut self, name: &str, value: Sexp) {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let val_ptr = &value as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_set_value_or_insert(
                mirror_ptr, sym_ptr, val_ptr, unbound_ptr,
            );
        }
    }

    /// Function-cell counterpart of `mirror_set_value'.
    pub(crate) fn mirror_set_function(&mut self, name: &str, func: Sexp) {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let val_ptr = &func as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_set_function_or_insert(
                mirror_ptr, sym_ptr, val_ptr, unbound_ptr,
            );
        }
    }

    /// `makunbound' — write `unbound_marker' into slot 0 in place.
    /// Silent no-op when the entry is absent (no auto-vivify).
    pub(crate) fn mirror_clear_value(&mut self, name: &str) {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_clear_value(
                mirror_ptr, sym_ptr, unbound_ptr,
            );
        }
    }

    /// `fmakunbound' — function-cell counterpart of `mirror_clear_value'.
    pub(crate) fn mirror_clear_function(&mut self, name: &str) {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_clear_function(
                mirror_ptr, sym_ptr, unbound_ptr,
            );
        }
    }

    /// Full symbol-entry installer (value + function + plist + constant).
    /// Hit-path overwrites all 4 slots; miss-path auto-vivifies (Doc
    /// 119 §119.A).  Used by image decode + `intern_constant'.
    /// `pub' (not `pub(crate)') so the `image-baker'-feature-gated
    /// `bin/nelisp-baker.rs' bin (= separate crate) can call it.
    pub fn mirror_install_entry(
        &mut self,
        name: &str,
        value: Option<Sexp>,
        function: Option<Sexp>,
        plist: Option<Sexp>,
        constant: bool,
    ) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let unbound = self.unbound_marker.clone();
        let value_slot = value.unwrap_or_else(|| unbound.clone());
        let function_slot = function.unwrap_or(unbound);
        let plist_slot = plist.unwrap_or(Sexp::Nil);
        let constant_slot = if constant { Sexp::T } else { Sexp::Nil };

        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let value_ptr = &value_slot as *const Sexp;
        let function_ptr = &function_slot as *const Sexp;
        let plist_ptr = &plist_slot as *const Sexp;
        let constant_ptr = &constant_slot as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_install_entry_or_insert(
                mirror_ptr, sym_ptr,
                value_ptr, function_ptr, plist_ptr, constant_ptr,
            );
        }
    }

    /// Read symbol-entry slot 3 (constant flag).  False on miss / unbuilt mirror.
    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return false;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let flag = unsafe {
            crate::elisp_cc_spike::mirror_is_constant(mirror_ptr, sym_ptr)
        };
        flag != 0
    }

    /// Write symbol-entry slot 3 (constant flag).  Hit + miss both in
    /// elisp; miss-path vivifies with value/function = `unbound_marker'
    /// and plist = Nil.
    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let flag_ptr = &value as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        unsafe {
            crate::elisp_cc_spike::mirror_set_constant_or_insert(
                mirror_ptr, sym_ptr, flag_ptr, unbound_ptr,
            );
        }
    }

    /// Walk every bucket of the mirror and invoke `callback(name,
    /// entry)' for each live symbol-entry.  Baker-only (replaces
    /// `env.globals.iter()').  Callback receives a `NlRecordRef'
    /// (= refcount-bumped clone) so it may read all four slots.
    /// Doc 130 (2026-05-18) — ungated to keep `mirror_snapshot_globals'
    /// compilable on default features (integration test binary uses it).
    pub(crate) fn mirror_iter_entries<F>(&self, mut callback: F)
    where
        F: FnMut(&str, &crate::eval::nlrecord::NlRecordRef),
    {
        let env_rec = match &self.globals_record {
            Sexp::Record(r) => r,
            _ => return,
        };
        let ht_rec = match env_rec.slots.get(0) {
            Some(Sexp::Record(r)) => r,
            _ => return,
        };
        let buckets = match ht_rec.slots.get(1) {
            Some(Sexp::Vector(v)) => v,
            _ => return,
        };
        for bucket in buckets.value.iter() {
            let mut cur = bucket;
            while let Sexp::Cons(c) = cur {
                if let Sexp::Cons(pair) = &c.car {
                    if let (Sexp::Str(k), Sexp::Record(r)) = (&pair.car, &pair.cdr) {
                        callback(k, r);
                    }
                }
                cur = &c.cdr;
            }
        }
    }

    /// Snapshot the elisp mirror as a `HashMap<String, SymbolEntry>'
    /// so the baker can diff before / after eval'ing a stdlib `.el'.
    /// Baker-only — called by `iterative_bake_one' + `encode_v3' in
    /// `bin/nelisp-baker.rs'.
    pub fn mirror_snapshot_globals(&self) -> HashMap<String, SymbolEntry> {
        let mut out: HashMap<String, SymbolEntry> = HashMap::new();
        let unbound = self.unbound_marker.clone();
        self.mirror_iter_entries(|name, record| {
            let slots = &record.slots;
            let value = match slots.get(0) {
                Some(s) if *s != unbound => Some(s.clone()),
                _ => None,
            };
            let function = match slots.get(1) {
                Some(s) if *s != unbound => Some(s.clone()),
                _ => None,
            };
            let plist = match slots.get(2) {
                Some(Sexp::Nil) | None => None,
                Some(s) => Some(s.clone()),
            };
            let constant = matches!(slots.get(3), Some(Sexp::T));
            out.insert(
                name.to_string(),
                SymbolEntry { value, function, plist, constant },
            );
        });
        out
    }

    /// Rust-direct empty mirror constructor.  Pre-allocates the
    /// `nelisp-env' / `fast-hash-table' / `buckets' record graph
    /// without invoking elisp `nelisp-env-make' (= no `apply_function'
    /// detour, no STDLIB dependency).  Sets `self.unbound_marker' to
    /// a Rust-defined sentinel `Sexp::Symbol("nelisp--unbound-marker")';
    /// STDLIB's defvar of the same name will overwrite the post-decode
    /// value, after which `install_globals_record' refreshes in-place
    /// sentinels.  `pub' so `bin/nelisp-baker.rs' can call it.
    pub fn install_empty_mirror_rust_direct(&mut self) {
        const BUCKET_COUNT: usize = 1024;
        // Sentinel for absent slots — replaced post-decode by the
        // baked elisp `nelisp--unbound-marker'.
        self.unbound_marker = Sexp::Symbol("nelisp--unbound-marker".into());
        let buckets = Sexp::vector(vec![Sexp::Nil; BUCKET_COUNT]);
        let ht_record = Sexp::record(
            Sexp::Symbol("fast-hash-table".into()),
            vec![Sexp::Int(BUCKET_COUNT as i64), buckets, Sexp::Int(0)],
        );
        self.globals_record = Sexp::record(
            Sexp::Symbol("nelisp-env".into()),
            vec![ht_record, Sexp::Nil, Sexp::Nil],
        );
        self.install_empty_frames_record_rust_direct();
    }

    /// Fast read from the elisp env mirror via Rust-direct
    /// `Sexp::Record' walk.  Returns the symbol-entry record's
    /// `NlRecordRef' handle, or None if absent.  Layout:
    ///   globals_record = Sexp::Record(`nelisp-env')
    ///     slots[0] = Sexp::Record(`fast-hash-table')
    ///       slots[0] = Sexp::Int (= bucket count, power-of-2)
    ///       slots[1] = Sexp::Vector (= buckets, each elt an alist)
    ///       slots[2] = Sexp::Int (= entry count)
    ///   Bucket alist cell: (Sexp::Cons (Sexp::Str . Sexp::Record(`symbol-entry')))
    ///   Symbol entry slots: [value, function, plist, constant]
    #[allow(dead_code)] // thin wrapper; callers dispatch direct to Phase 47.
    pub(crate) fn mirror_lookup_entry(
        env_record: &Sexp,
        name: &str,
    ) -> Option<crate::eval::nlrecord::NlRecordRef> {
        if !matches!(env_record, Sexp::Record(_)) {
            return None;
        }
        let sym = Sexp::Symbol(name.into());
        let entry_ptr = unsafe {
            crate::elisp_cc_spike::mirror_lookup_entry(
                env_record as *const Sexp,
                &sym as *const Sexp,
            )
        };
        if entry_ptr.is_null() {
            return None;
        }
        // SAFETY: returned slot is owned by the bucket's `NlConsBox'
        // pair and stays live as long as `*env_record' is unchanged.
        let entry_sexp: &Sexp = unsafe { &*entry_ptr };
        match entry_sexp {
            Sexp::Record(r) => Some(r.clone()),
            _ => None,
        }
    }

    /// Value-cell read.  Returns `self.unbound_marker' on miss
    /// (sentinel re-injected here since the Phase 47 helper writes
    /// `Sexp::Nil' on miss).
    pub(crate) fn mirror_lookup_value(&self, name: &str) -> Sexp {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return self.unbound_marker.clone();
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let entry_ptr = unsafe {
            crate::elisp_cc_spike::mirror_lookup_entry(mirror_ptr, sym_ptr)
        };
        if entry_ptr.is_null() {
            return self.unbound_marker.clone();
        }
        let mut slot = Sexp::Nil;
        unsafe {
            crate::elisp_cc_spike::mirror_lookup_value(
                mirror_ptr, sym_ptr,
                &mut slot as *mut Sexp,
            );
        }
        slot
    }

    /// Function-cell read.  Same sentinel-reinjection as `mirror_lookup_value'.
    pub(crate) fn mirror_lookup_function(&self, name: &str) -> Sexp {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return self.unbound_marker.clone();
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let entry_ptr = unsafe {
            crate::elisp_cc_spike::mirror_lookup_entry(mirror_ptr, sym_ptr)
        };
        if entry_ptr.is_null() {
            return self.unbound_marker.clone();
        }
        let mut slot = Sexp::Nil;
        unsafe {
            crate::elisp_cc_spike::mirror_lookup_function(
                mirror_ptr, sym_ptr,
                &mut slot as *mut Sexp,
            );
        }
        slot
    }

    /// `boundp' equivalent — compares entry's slot 0 against `unbound_marker'.
    pub(crate) fn mirror_is_bound(&self, name: &str) -> bool {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return false;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        let flag = unsafe {
            crate::elisp_cc_spike::mirror_is_bound(
                mirror_ptr, sym_ptr, unbound_ptr,
            )
        };
        flag != 0
    }

    /// `fboundp' equivalent — function-cell counterpart of `mirror_is_bound'.
    pub(crate) fn mirror_is_fbound(&self, name: &str) -> bool {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return false;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        let flag = unsafe {
            crate::elisp_cc_spike::mirror_is_fbound(
                mirror_ptr, sym_ptr, unbound_ptr,
            )
        };
        flag != 0
    }

    // ---- Lexical frame stack helpers ----

    /// Rust-direct empty lexframe-stack constructor.  Matches the
    /// layout in `lisp/nelisp-lexframe.el':
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
    /// Hashes NAME via the pure-elisp `nelisp_fnv1a' (Doc 115 §115.7)
    /// + walks the bucket alist to update-in-place or prepend a
    /// fresh `(NAME . CELL)' pair.
    fn frame_bind_into(frame: &Sexp, name: &str, cell: Sexp) {
        let Sexp::Record(frame_rec) = frame else { return };
        let ht_rec = match frame_rec.slots.get(0) { Some(Sexp::Record(r)) => r.clone(), _ => return };
        let bucket_count = match ht_rec.slots.get(0) { Some(Sexp::Int(n)) => *n as u32, _ => return };
        let buckets = match ht_rec.slots.get(1) { Some(Sexp::Vector(v)) => v.clone(), _ => return };
        // Doc 115 §115.7 — pure-elisp FNV-1a.
        let name_sym = Sexp::Symbol(name.into());
        let h = unsafe {
            crate::elisp_cc_spike::fnv1a(&name_sym as *const Sexp)
        } as u32;
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
    /// Doc 115 §115.7 — hashes NAME via the pure-elisp `nelisp_fnv1a'.
    #[allow(dead_code)] // used by stack walks + tests
    fn frame_lookup_in(frame: &Sexp, name: &str) -> Option<Sexp> {
        let Sexp::Record(frame_rec) = frame else { return None };
        let ht_rec = match frame_rec.slots.get(0)? { Sexp::Record(r) => r, _ => return None };
        let bucket_count = match ht_rec.slots.get(0)? { Sexp::Int(n) => *n as u32, _ => return None };
        let buckets = match ht_rec.slots.get(1)? { Sexp::Vector(v) => v, _ => return None };
        // Doc 115 §115.7 — pure-elisp FNV-1a.
        let name_sym = Sexp::Symbol(name.into());
        let h = unsafe {
            crate::elisp_cc_spike::fnv1a(&name_sym as *const Sexp)
        } as u32;
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
    #[allow(dead_code)] // used by stack walks + tests
    pub(crate) fn frame_lookup_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        if depth == 0 { return None; }
        let frame = backing.value.get(depth - 1)?;
        Env::frame_lookup_in(frame, name)
    }

    /// Innermost-first walk across the entire mirror stack.  Returns
    /// the first NAME hit.  Mirrors `nelisp-lexframe-stack-find';
    /// `find_frame_cell' delegates here.
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

