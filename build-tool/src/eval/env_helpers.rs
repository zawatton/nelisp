//! Doc 102 Phase 8 — globals mirror + lexical frame stack helpers
//! extracted from `eval/env.rs'.  Doc 114 Step 5 (2026-05-17)
//! consolidated the previously-separate `env_mirror.rs' +
//! `env_lexframe.rs' modules into this single helpers file (= the
//! files were deleted outright; the `pub mod' declarations were
//! removed from `eval/mod.rs').
//!
//! Two responsibilities live here:
//!
//! 1. **Globals mirror** — Rust-direct walk of the elisp-side
//!    `nelisp-env' record (= `Env::globals_record').  Replaces the
//!    pre-Phase-2.b `globals: HashMap<String, SymbolEntry>' field.
//!    Phase 5.c sentinel-return convention applies to the lookup
//!    helpers.  See `lisp/nelisp-env.el' for the elisp layout side
//!    and Doc 102 §2.1 / §2.2 for the rep + bucket-count rationale.
//!
//! 2. **Lexical frame stack** — Rust-direct walk of the elisp-side
//!    `nelisp-lexframe-stack' record (= `Env::frames_record').
//!    Replaces the pre-Doc-104 `frames: Vec<HashMap<String,
//!    FrameCell>>' field; Doc 104 Stage 3.a-3.e covers the Vec →
//!    Sexp::Record migration, Doc 102 Phase 4.b-4.b adds the elisp-
//!    dispatch path for `capture_lexical' / `push_captured'.  See
//!    `lisp/nelisp-lexframe.el' for the elisp layout side.

#[cfg(any(test, feature = "image-baker"))]
use std::collections::HashMap;

use super::env::{Env, FrameCell};
#[cfg(any(test, feature = "image-baker"))]
use super::env::SymbolEntry;
use super::error::EvalError;
use super::sexp::Sexp;

impl Env {
    // ============================================================
    // Globals mirror helpers (formerly `eval/env_mirror.rs').
    // ============================================================

    /// Set the value cell.  Phase 47 in-place update on hit; Rust
    /// auto-vivify on miss.  No-op when mirror is uninitialised.
    pub(crate) fn mirror_set_value(&mut self, name: &str, value: Sexp) {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let val_ptr = &value as *const Sexp;
        // SAFETY: all pointers point at stack-locals or `&self.globals_record'
        // which outlive the call; helper expects `Sexp::Record' layout.
        let hit = unsafe {
            crate::elisp_cc_spike::mirror_set_value(mirror_ptr, sym_ptr, val_ptr)
        };
        if hit != 0 {
            return;
        }
        self.mirror_insert_new_entry(name, 0, value);
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
        // SAFETY: see `mirror_set_value'.
        let hit = unsafe {
            crate::elisp_cc_spike::mirror_set_function(mirror_ptr, sym_ptr, val_ptr)
        };
        if hit != 0 {
            return;
        }
        self.mirror_insert_new_entry(name, 1, func);
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
        // SAFETY: see `mirror_set_value'.  `unbound_ptr' points at
        // the `Env'-owned sentinel which outlives the call.
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
        // SAFETY: see `mirror_clear_value'.
        unsafe {
            crate::elisp_cc_spike::mirror_clear_function(
                mirror_ptr, sym_ptr, unbound_ptr,
            );
        }
    }

    /// Insert a fresh symbol-entry record into the bucket alist.
    /// `slot' selects which cell the caller is filling (0 = value,
    /// 1 = function); the other cell is initialised to
    /// `self.unbound_marker'.
    fn mirror_insert_new_entry(&mut self, name: &str, slot: usize, cell: Sexp) {
        let unbound = self.unbound_marker.clone();
        let (value_slot, function_slot) = if slot == 0 {
            (cell, unbound)
        } else {
            (unbound, cell)
        };
        let entry = Sexp::record(
            Sexp::Symbol("symbol-entry".into()),
            vec![value_slot, function_slot, Sexp::Nil, Sexp::Nil],
        );
        self.mirror_prepend_to_bucket(name, entry);
    }

    /// Full symbol-entry installer (value + function + plist + constant).
    /// Phase 47 hit-path overwrites all 4 slots; miss-path falls back to
    /// `mirror_prepend_to_bucket' auto-vivify.  Used by image decode +
    /// `intern_constant'.
    pub(crate) fn mirror_install_entry(
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
        // SAFETY: all pointers refer to stack-local `Sexp' values
        // (or `&self.globals_record') that outlive the call.  The
        // helper performs refcount-aware clones into each entry
        // slot.
        let hit = unsafe {
            crate::elisp_cc_spike::mirror_install_entry(
                mirror_ptr, sym_ptr,
                value_ptr, function_ptr, plist_ptr, constant_ptr,
            )
        };
        if hit != 0 {
            return;
        }
        // Miss — fall back to the Rust prepend.  The four `_slot'
        // locals were *not* consumed by the helper (it cloned
        // through pointers); we move them into the fresh entry
        // record below.
        let entry = Sexp::record(
            Sexp::Symbol("symbol-entry".into()),
            vec![value_slot, function_slot, plist_slot, constant_slot],
        );
        self.mirror_prepend_to_bucket(name, entry);
    }

    /// Read symbol-entry slot 3 (constant flag).  False on miss / unbuilt mirror.
    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return false;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        // SAFETY: `mirror_ptr' points at a live `Sexp::Record(_)'
        // and `sym_ptr' at a stack-local `Sexp::Symbol(_)' which
        // outlives the call.
        let flag = unsafe {
            crate::elisp_cc_spike::mirror_is_constant(mirror_ptr, sym_ptr)
        };
        flag != 0
    }

    /// Write symbol-entry slot 3 (constant flag).  Phase 47 hit-path;
    /// miss-path auto-vivifies an entry with value/function = unbound_marker.
    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let flag_ptr = &value as *const Sexp;
        // SAFETY: see `mirror_set_value'.
        let hit = unsafe {
            crate::elisp_cc_spike::mirror_set_constant(
                mirror_ptr, sym_ptr, flag_ptr,
            )
        };
        if hit != 0 {
            return;
        }
        let unbound = self.unbound_marker.clone();
        let entry = Sexp::record(
            Sexp::Symbol("symbol-entry".into()),
            vec![unbound.clone(), unbound, Sexp::Nil, value],
        );
        self.mirror_prepend_to_bucket(name, entry);
    }

    /// Bucket-prepend primitive.  Shared by `mirror_insert_new_entry'
    /// / `mirror_install_entry' / `mirror_set_constant'.  Hashes NAME,
    /// locates the bucket, builds `(KEY . ENTRY)', prepends onto the
    /// bucket head, bumps the entry-count slot.
    fn mirror_prepend_to_bucket(&mut self, name: &str, entry: Sexp) {
        let env_rec = match &self.globals_record {
            Sexp::Record(r) => r.clone(),
            _ => return,
        };
        let ht_rec = match env_rec.slots.get(0) {
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
        // Doc 115 §115.7 — pure-elisp FNV-1a via the
        // `nelisp_fnv1a' Phase 47 `.o'.  Replaces the deleted
        // Rust `mirror_fnv1a' free fn.  For the ASCII identifier
        // names that land here the result is bit-equal.
        let name_sym = Sexp::Symbol(name.into());
        let h = unsafe {
            crate::elisp_cc_spike::fnv1a(&name_sym as *const Sexp)
        } as u32;
        let idx = (if bucket_count & (bucket_count - 1) == 0 {
            h & (bucket_count - 1)
        } else {
            h % bucket_count
        }) as usize;
        let pair = Sexp::cons(Sexp::Str(name.to_string()), entry);
        // SAFETY: no other borrow into `buckets.value' / `ht_rec.slots'
        // is live across the closure boundary.
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

    /// Walk every bucket of the mirror and invoke `callback(name,
    /// entry)' for each live symbol-entry.  Used by the image baker
    /// (= replaces `env.globals.iter()').  Callback receives a
    /// `NlRecordRef' (= refcount-bumped clone) so it may read all
    /// four slots (value / function / plist / constant).
    ///
    /// Doc 102 Phase 7 — gated; only consumer is
    /// `mirror_snapshot_globals' which is itself baker-only.
    #[cfg(any(test, feature = "image-baker"))]
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
    /// so the baker can diff before / after eval'ing a stdlib `.el'
    /// file.  Captures elisp-driven mutations (= env_shim `(defun)' /
    /// `(fset)' that wrote ONLY to the mirror).
    ///
    /// Doc 102 Phase 7 — gated; called only by `iterative_bake_one' +
    /// `encode_v3' (= both image-baker-only paths).
    #[cfg(any(test, feature = "image-baker"))]
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

    /// Per-file diff of the elisp mirror against `before' (= a prior
    /// `mirror_snapshot_globals' result).  Returns a fresh `Env' whose
    /// **mirror** is populated with the changed entries.  Used by
    /// `image::iterative_bake_one'.
    ///
    /// Doc 102 Phase 7 — gated; only used by `iterative_bake_one'.
    #[cfg(any(test, feature = "image-baker"))]
    pub fn mirror_diff_view(&self, before: &HashMap<String, SymbolEntry>) -> Env {
        let mut diff = Env::empty();
        diff.install_empty_mirror_rust_direct();
        let after = self.mirror_snapshot_globals();
        for (name, entry) in after.into_iter().filter(|(k, v)| {
            before.get(k).map_or(true, |prev| prev != v)
        }) {
            diff.mirror_install_entry(
                &name,
                entry.value,
                entry.function,
                entry.plist,
                entry.constant,
            );
        }
        diff
    }

    /// Sprint B Stage 2 — Rust-direct empty mirror constructor.
    /// Pre-allocates the `nelisp-env' / `fast-hash-table' /
    /// `buckets' record graph without invoking elisp `nelisp-env-make'
    /// (= no `apply_function' detour, no STDLIB dependency).  Sets
    /// `self.unbound_marker' to a Rust-defined sentinel
    /// `Sexp::Symbol("nelisp--unbound-marker")'; STDLIB's defvar of
    /// the same name will overwrite the post-decode value, after
    /// which `install_globals_record' captures the post-decode value
    /// and walks the mirror to refresh in-place sentinels.
    pub(crate) fn install_empty_mirror_rust_direct(&mut self) {
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
    /// `Sexp::Record' walk.  No `apply_function' (= no FNV-1a
    /// interpreted loop overhead).  Returns the symbol-entry record's
    /// `NlRecordRef' handle (= refcount-bumped clone), or None if the
    /// key is absent.
    ///
    /// Layout (= `lisp/nelisp-env.el' + `nelisp-stdlib-fast-hash.el'):
    ///   globals_record = Sexp::Record(`nelisp-env')
    ///     slots[0] = Sexp::Record(`fast-hash-table')
    ///       slots[0] = Sexp::Int (= bucket count, power-of-2)
    ///       slots[1] = Sexp::Vector (= buckets, each elt an alist)
    ///       slots[2] = Sexp::Int (= entry count)
    ///   Bucket alist cell: (Sexp::Cons (Sexp::Str . Sexp::Record(`symbol-entry')))
    ///   Symbol entry slots: [value, function, plist, constant]
    #[allow(dead_code)] // Doc 114 x86_64-linux pivot: callers dispatch
                        // directly to Phase 47; the static method is
                        // retained as a thin wrapper for any future
                        // call sites.
    pub(crate) fn mirror_lookup_entry(
        env_record: &Sexp,
        name: &str,
    ) -> Option<crate::eval::nlrecord::NlRecordRef> {
        // Doc 111 §111.E cutover — dispatch to Phase 47-compiled
        // `nelisp_mirror_lookup_entry'.  The helper returns the raw
        // `*const Sexp' of the matching entry slot (= `(KEY . ENTRY)'
        // pair's CDR slot, which holds a `Sexp::Record(_)' shape), or
        // 0 on miss / empty mirror.  We deref and clone the
        // `NlRecordRef' to satisfy the existing `Option<NlRecordRef>'
        // return shape; the deref is sound because the returned slot
        // is owned by the bucket's `NlConsBox' pair and stays live as
        // long as `*env_record' is unchanged.
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
        // SAFETY: Phase 47 returned a non-null `*const Sexp' that
        // borrows a slot in the live bucket chain owned by
        // `*env_record'.  The slot value is `Sexp::Record(_)' by
        // construction of `install_empty_mirror_rust_direct' +
        // `mirror_prepend_to_bucket'.
        let entry_sexp: &Sexp = unsafe { &*entry_ptr };
        match entry_sexp {
            Sexp::Record(r) => Some(r.clone()),
            _ => None,
        }
    }

    /// Value-cell read.  Returns `self.unbound_marker' on miss (sentinel
    /// re-injected via `mirror_lookup_entry' since Phase 47 helper writes
    /// `Sexp::Nil' on miss instead of the sentinel).
    pub(crate) fn mirror_lookup_value(&self, name: &str) -> Sexp {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return self.unbound_marker.clone();
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        // Detect miss via #1 first so we can re-inject the sentinel.
        // SAFETY: pointers are stack-local references to live data.
        let entry_ptr = unsafe {
            crate::elisp_cc_spike::mirror_lookup_entry(mirror_ptr, sym_ptr)
        };
        if entry_ptr.is_null() {
            return self.unbound_marker.clone();
        }
        let mut slot = Sexp::Nil;
        // SAFETY: `slot' is a stack-local writable Sexp.  The
        // helper performs a refcount-aware clone into the slot.
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
        // SAFETY: see `mirror_lookup_value'.
        unsafe {
            crate::elisp_cc_spike::mirror_lookup_function(
                mirror_ptr, sym_ptr,
                &mut slot as *mut Sexp,
            );
        }
        slot
    }

    /// `boundp' equivalent — helper compares entry's slot 0 against `unbound_marker'.
    pub(crate) fn mirror_is_bound(&self, name: &str) -> bool {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return false;
        }
        let sym = Sexp::Symbol(name.into());
        let mirror_ptr = &self.globals_record as *const Sexp;
        let sym_ptr = &sym as *const Sexp;
        let unbound_ptr = &self.unbound_marker as *const Sexp;
        // SAFETY: pointers are live references to `Env'-owned or
        // stack-local `Sexp' values.
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
        // SAFETY: see `mirror_is_bound'.
        let flag = unsafe {
            crate::elisp_cc_spike::mirror_is_fbound(
                mirror_ptr, sym_ptr, unbound_ptr,
            )
        };
        flag != 0
    }

    // ============================================================
    // Lexical frame stack helpers (formerly `eval/env_lexframe.rs').
    // ============================================================

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
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by stack walks + tests
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

// Doc 115 §115.7 — the `mirror_fnv1a' free fn + `nl_mirror_fnv1a_sexp'
// extern wrapper have been deleted.  Every call site (= the three
// callers above in `mirror_prepend_to_bucket' / `frame_bind_into' /
// `frame_lookup_in', plus the elisp `extern-call' sites in
// `lisp/nelisp-cc-mirror-lookup-entry.el' / `nelisp-cc-frame-bind.el' /
// `nelisp-cc-frame-stack-find.el') now dispatches through the
// pure-elisp `nelisp_fnv1a' Phase 47 `.o' compiled from
// `lisp/nelisp-cc-fnv1a.el'.  The Rust hash bit-equality test for
// ASCII reference vectors moves to the integration probe at
// `tests/elisp_cc_fnv1a_probe.rs'.
