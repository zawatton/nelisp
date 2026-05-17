//! Doc 102 Phase 8 — globals mirror helpers extracted from
//! `eval/env.rs`.  The Rust-direct walk of the elisp-side
//! `nelisp-env' record (= `Env::globals_record').  Replaces the
//! pre-Phase-2.b `globals: HashMap<String, SymbolEntry>' field;
//! Phase 5.c sentinel-return convention applies to the lookup
//! helpers.  See `lisp/nelisp-env.el' for the elisp layout side
//! and Doc 102 §2.1 / §2.2 for the rep + bucket-count rationale.

#[cfg(any(test, feature = "image-baker"))]
use std::collections::HashMap;

use super::env::Env;
#[cfg(any(test, feature = "image-baker"))]
use super::env::SymbolEntry;
use super::sexp::Sexp;

impl Env {
    /// Doc 111 §111.E cutover — `mirror_set_value' now dispatches to
    /// the Phase 47-compiled `nelisp_mirror_set_value' helper for the
    /// in-place slot update.  Phase 47 returns 1 on hit (entry slot was
    /// overwritten) and 0 on miss; on miss we fall back to the existing
    /// Rust `mirror_insert_new_entry' auto-vivify path.  No-op when the
    /// mirror isn't ready (= `globals_record == Sexp::Nil', early
    /// bootstrap before `install_globals_record').
    ///
    /// Replaces the Session-6 Rust-direct walk; same semantics, but the
    /// FNV-1a + bucket walk now executes in elisp-compiled code (= part
    /// of the Doc 102 Phase 8.b condition (a) "Rust −520 LOC" target).
    pub(crate) fn mirror_set_value(&mut self, name: &str, value: Sexp) {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
            if !matches!(&self.globals_record, Sexp::Record(_)) {
                return;
            }
            let sym = Sexp::Symbol(name.into());
            let mirror_ptr = &self.globals_record as *const Sexp;
            let sym_ptr = &sym as *const Sexp;
            let val_ptr = &value as *const Sexp;
            // SAFETY: `mirror_ptr' points at a live `Sexp::Record' (=
            // `globals_record') with the layout the Phase 47 helper
            // expects.  `sym_ptr' / `val_ptr' point at stack-local
            // `Sexp' values that outlive the call.
            let hit = unsafe {
                crate::elisp_cc_spike::mirror_set_value(mirror_ptr, sym_ptr, val_ptr)
            };
            if hit != 0 {
                return;
            }
            // Phase 47 reported a miss — fall back to the existing
            // auto-vivify path.  `value' was *not* consumed by the
            // helper (it operates through `*val_ptr' refcount-aware
            // clones), so the move below is sound.
            self.mirror_insert_new_entry(name, /* slot */ 0, value);
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if matches!(&self.globals_record, Sexp::Nil) {
                return;
            }
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe { entry.with_slots_mut(|s| s[0] = value) };
                return;
            }
            self.mirror_insert_new_entry(name, /* slot */ 0, value);
        }
    }

    /// Doc 111 §111.E cutover — function-cell counterpart of
    /// `mirror_set_value', dispatches to Phase 47-compiled
    /// `nelisp_mirror_set_function'.  See `mirror_set_value' for the
    /// hit/miss + fallback contract.
    pub(crate) fn mirror_set_function(&mut self, name: &str, func: Sexp) {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            self.mirror_insert_new_entry(name, /* slot */ 1, func);
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if matches!(&self.globals_record, Sexp::Nil) {
                return;
            }
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe { entry.with_slots_mut(|s| s[1] = func) };
                return;
            }
            self.mirror_insert_new_entry(name, /* slot */ 1, func);
        }
    }

    /// Doc 111 §111.E cutover — clear the entry's value cell (=
    /// `makunbound').  Dispatches to Phase 47-compiled
    /// `nelisp_mirror_clear_value', which writes
    /// `*unbound_ptr' (= `self.unbound_marker') into slot 0 in place.
    /// No-op (returns 0) when the entry is absent; we silently swallow
    /// that case to match the original Rust impl (= no auto-vivify on
    /// `makunbound').
    pub(crate) fn mirror_clear_value(&mut self, name: &str) {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if matches!(&self.globals_record, Sexp::Nil) {
                return;
            }
            let unbound = self.unbound_marker.clone();
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe { entry.with_slots_mut(|s| s[0] = unbound) };
            }
        }
    }

    /// Doc 111 §111.E cutover — function-cell counterpart of
    /// `mirror_clear_value' (= `fmakunbound').  Dispatches to
    /// Phase 47-compiled `nelisp_mirror_clear_function'.
    pub(crate) fn mirror_clear_function(&mut self, name: &str) {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if matches!(&self.globals_record, Sexp::Nil) {
                return;
            }
            let unbound = self.unbound_marker.clone();
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe { entry.with_slots_mut(|s| s[1] = unbound) };
            }
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

    /// Doc 111 §111.E cutover — full symbol-entry installer.
    /// Dispatches to Phase 47-compiled `nelisp_mirror_install_entry'
    /// for the hit-path slot overwrite (= all four slots in place).
    /// Phase 47 returns 1 on hit, 0 on miss; on miss we fall back to
    /// the Rust `mirror_prepend_to_bucket' auto-vivify path with the
    /// composed entry record.
    ///
    /// Used by image decode (= replaces the old `env.globals.insert'
    /// call at `image.rs::decode_v3_into') and by `intern_constant'.
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

        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe {
                    entry.with_slots_mut(|s| {
                        s[0] = value_slot;
                        s[1] = function_slot;
                        s[2] = plist_slot;
                        s[3] = constant_slot;
                    });
                }
                return;
            }
            let entry = Sexp::record(
                Sexp::Symbol("symbol-entry".into()),
                vec![value_slot, function_slot, plist_slot, constant_slot],
            );
            self.mirror_prepend_to_bucket(name, entry);
        }
    }

    /// Doc 111 §111.E cutover — read symbol-entry slot 3 (= constant
    /// flag).  Dispatches to Phase 47-compiled
    /// `nelisp_mirror_is_constant', which composes #1 with a tag
    /// check on the matched entry's slot 3.  Returns false when the
    /// entry is absent or the mirror is unbuilt.
    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return flag != 0;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            match Env::mirror_lookup_entry(&self.globals_record, name) {
                Some(entry) => matches!(entry.slots.get(3), Some(Sexp::T)),
                None => false,
            }
        }
    }

    /// Doc 111 §111.E cutover — write symbol-entry slot 3.  Dispatches
    /// to Phase 47-compiled `nelisp_mirror_set_constant'.  Returns 1 on
    /// hit, 0 on miss; on miss we fall back to the Rust auto-vivify
    /// path (= construct a fresh symbol-entry with both value+function
    /// = unbound_marker, plist = nil, constant = `truthy', then
    /// prepend onto the bucket).
    ///
    /// Used by `env_shim::bi_globals_op set-constant' + `intern_constant'.
    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
                unsafe { entry.with_slots_mut(|s| s[3] = value) };
                return;
            }
            let unbound = self.unbound_marker.clone();
            let entry = Sexp::record(
                Sexp::Symbol("symbol-entry".into()),
                vec![unbound.clone(), unbound, Sexp::Nil, value],
            );
            self.mirror_prepend_to_bucket(name, entry);
        }
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
        let h = mirror_fnv1a(name);
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
    #[allow(dead_code)] // Doc 111 §111.E cutover: x86_64 callers dispatch
                        // directly; the static method is kept as the
                        // non-x86 fallback path for portability.
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
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            let env_rec = match env_record { Sexp::Record(r) => r, _ => return None };
            let ht_rec = match env_rec.slots.get(0)? { Sexp::Record(r) => r, _ => return None };
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
                            if let Sexp::Record(r) = &pair.cdr {
                                return Some(r.clone());
                            }
                        }
                    }
                }
                cur = &c.cdr;
            }
            None
        }
    }

    /// Doc 111 §111.E cutover — value-cell read.  Dispatches to
    /// Phase 47-compiled `nelisp_mirror_lookup_value'.  Phase 47 writes
    /// `Sexp::Nil' to the result slot on miss; we re-introduce the
    /// `self.unbound_marker' sentinel by detecting miss via
    /// `mirror_lookup_entry' (= the helper returns 0 on miss, non-zero
    /// on hit) and returning the sentinel.
    ///
    /// Doc 102 Phase 5.c sentinel-return convention: returns
    /// `self.unbound_marker' when the entry is absent OR holds the
    /// sentinel; the slot's stored `Sexp' otherwise.
    pub(crate) fn mirror_lookup_value(&self, name: &str) -> Sexp {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return slot;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) else {
                return self.unbound_marker.clone();
            };
            let Some(cell) = entry.slots.get(0) else {
                return self.unbound_marker.clone();
            };
            cell.clone()
        }
    }

    /// Doc 111 §111.E cutover — function-cell read.  Same sentinel-
    /// reinjection pattern as `mirror_lookup_value' (= dispatch to #1
    /// for miss detection, then #3 to fill the slot).
    pub(crate) fn mirror_lookup_function(&self, name: &str) -> Sexp {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return slot;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) else {
                return self.unbound_marker.clone();
            };
            let Some(cell) = entry.slots.get(1) else {
                return self.unbound_marker.clone();
            };
            cell.clone()
        }
    }

    /// Doc 111 §111.E cutover — `boundp' equivalent.  Dispatches to
    /// Phase 47-compiled `nelisp_mirror_is_bound', supplying the
    /// `self.unbound_marker' sentinel via a borrowed pointer so the
    /// helper can compare the entry's slot 0 against it.
    pub(crate) fn mirror_is_bound(&self, name: &str) -> bool {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return flag != 0;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            self.mirror_lookup_value(name) != self.unbound_marker
        }
    }

    /// Doc 111 §111.E cutover — `fboundp' equivalent.  Dispatches to
    /// Phase 47-compiled `nelisp_mirror_is_fbound'.
    pub(crate) fn mirror_is_fbound(&self, name: &str) -> bool {
        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
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
            return flag != 0;
        }
        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            self.mirror_lookup_function(name) != self.unbound_marker
        }
    }
}

/// FNV-1a 32-bit hash matching the elisp `nelisp--fast-hash--hash'
/// (iterates Unicode codepoints).  Native Rust replacement for the
/// interpreted hash loop.  Shared by `mirror_*' + `frame_*' helpers
/// (`env_lexframe.rs` imports this).
pub(crate) fn mirror_fnv1a(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for c in s.chars() {
        h ^= c as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

/// Doc 111 §111.E Group C — C-callable FNV-1a wrapper for the
/// Phase 47-compiled `mirror_lookup_entry' helper.  The elisp body
/// emits an `(extern-call nl_mirror_fnv1a_sexp sym-ptr)' which lands
/// here; we reach into the `Sexp::Symbol' / `Sexp::Str' payload, run
/// the same Unicode-codepoint loop as [`mirror_fnv1a`], and return
/// the hash as `i64' (= rax-shaped result so the elisp call site can
/// feed it into the `(logand H (- COUNT 1))' index mask without an
/// extra zero-extend).
///
/// The hash itself stays in Rust (= Group C "stay Rust" per
/// Doc 111 §3.E) because the inner loop is performance-critical
/// (called once per `defvar' / `boundp' / `fboundp' / `intern' /
/// global ref) and the Phase 47 grammar does not yet have a
/// Unicode-codepoint iterator.  Only the surrounding bucket walk
/// moves to elisp.
///
/// # Safety
/// - `sym' must be non-null, properly aligned, and point at an
///   initialized `Sexp::Symbol(_)' or `Sexp::Str(_)' value.  Any
///   other tag returns 0 (= the FNV-1a hash of the empty string is
///   `0x811C9DC5', not 0, so this sentinel cannot collide with a
///   legitimate empty-string hash and the elisp walker will simply
///   fall through to the bucket lookup with idx 0 and miss).
#[no_mangle]
pub unsafe extern "C" fn nl_mirror_fnv1a_sexp(sym: *const Sexp) -> i64 {
    match unsafe { &*sym } {
        Sexp::Symbol(s) | Sexp::Str(s) => mirror_fnv1a(s) as i64,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mirror_fnv1a_matches_elisp_hash_loop() {
        // FNV-1a 32-bit reference vectors (= ASCII inputs, same algorithm
        // as `nelisp--fast-hash--hash').  Confirms the Rust port matches
        // the elisp implementation bit-for-bit.
        assert_eq!(mirror_fnv1a(""), 0x811C9DC5);
        assert_eq!(mirror_fnv1a("a"), 0xE40C292C);
        assert_eq!(mirror_fnv1a("foobar"), 0xBF9CF968);
    }
}
