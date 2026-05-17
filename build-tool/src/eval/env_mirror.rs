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
    /// Doc 102 Phase 8 sprint Session 6 — Rust-direct mirror write
    /// path.  Replaces the Session 3 `apply_function' shim, which paid
    /// ~800µs per call for the interpreted `nelisp--fast-hash--hash'
    /// loop; the direct path runs the same FNV-1a in Rust + slot
    /// mutation via `with_slots_mut' for ~10µs per call.  No-op when
    /// the mirror isn't ready (= `globals_record == Sexp::Nil', early
    /// bootstrap before `install_globals_record').  Auto-vivifies an
    /// absent entry.
    pub(crate) fn mirror_set_value(&mut self, name: &str, value: Sexp) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: no other `&Vec<Sexp>` borrow into `entry.slots' is
            // live (Phase A.2.1 setcar discipline).
            unsafe { entry.with_slots_mut(|s| s[0] = value) };
            return;
        }
        self.mirror_insert_new_entry(name, /* slot */ 0, value);
    }

    /// Function-cell counterpart of `mirror_set_value'.  Mutates
    /// symbol-entry slot 1 in place.
    pub(crate) fn mirror_set_function(&mut self, name: &str, func: Sexp) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: see `mirror_set_value'.
            unsafe { entry.with_slots_mut(|s| s[1] = func) };
            return;
        }
        self.mirror_insert_new_entry(name, /* slot */ 1, func);
    }

    /// Clear the entry's value cell (= `makunbound').  Mirrors
    /// `nelisp-env-clear-value' from `lisp/nelisp-env.el': preserves
    /// the entry record, sets slot 0 to `nelisp--unbound-marker' so
    /// subsequent `mirror_lookup_value' returns the sentinel.
    pub(crate) fn mirror_clear_value(&mut self, name: &str) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let unbound = self.unbound_marker.clone();
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: see `mirror_set_value'.
            unsafe { entry.with_slots_mut(|s| s[0] = unbound) };
        }
    }

    /// Function-cell counterpart of `mirror_clear_value' (=
    /// `fmakunbound').  Slot 1 → unbound.
    pub(crate) fn mirror_clear_function(&mut self, name: &str) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let unbound = self.unbound_marker.clone();
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: see `mirror_set_value'.
            unsafe { entry.with_slots_mut(|s| s[1] = unbound) };
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

    /// Full symbol-entry installer.  Used by image decode (= replaces
    /// the old `env.globals.insert' call at `image.rs::decode_v3_into')
    /// and by `intern_constant'.  When the entry exists, mutates all
    /// four slots in place; otherwise allocates a fresh
    /// `symbol-entry' record and prepends it onto the appropriate
    /// bucket.
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
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: see `mirror_set_value'.
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

    /// Read symbol-entry slot 3 (= constant flag) for the elisp
    /// `boundp' / `setting-constant' check paths.  Returns false when
    /// the entry is absent or the mirror is unbuilt.
    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        match Env::mirror_lookup_entry(&self.globals_record, name) {
            Some(entry) => matches!(entry.slots.get(3), Some(Sexp::T)),
            None => false,
        }
    }

    /// Write symbol-entry slot 3.  Used by
    /// `env_shim::bi_globals_op set-constant' + `intern_constant'.
    /// Auto-vivifies a fresh entry when the name is unknown so callers
    /// can mark a symbol constant before its value is set.
    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        if matches!(&self.globals_record, Sexp::Nil) {
            return;
        }
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        if let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) {
            // SAFETY: see `mirror_set_value'.
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
    pub(crate) fn mirror_lookup_entry(
        env_record: &Sexp,
        name: &str,
    ) -> Option<crate::eval::nlrecord::NlRecordRef> {
        let env_rec = match env_record { Sexp::Record(r) => r, _ => return None };
        let ht_rec = match env_rec.slots.get(0)? { Sexp::Record(r) => r, _ => return None };
        let bucket_count = match ht_rec.slots.get(0)? { Sexp::Int(n) => *n as u32, _ => return None };
        let buckets = match ht_rec.slots.get(1)? { Sexp::Vector(v) => v, _ => return None };
        let h = mirror_fnv1a(name);
        // power-of-2 fast path matches `nelisp--fast-hash--hash'.
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

    /// Value-cell read.  Returns `self.unbound_marker' when the entry
    /// is absent OR holds the sentinel.  Doc 102 Phase 5.c sentinel-
    /// return convention.
    pub(crate) fn mirror_lookup_value(&self, name: &str) -> Sexp {
        let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) else {
            return self.unbound_marker.clone();
        };
        let Some(cell) = entry.slots.get(0) else {
            return self.unbound_marker.clone();
        };
        cell.clone()
    }

    /// Function-cell read.  Returns `self.unbound_marker' when absent
    /// or unbound.
    pub(crate) fn mirror_lookup_function(&self, name: &str) -> Sexp {
        let Some(entry) = Env::mirror_lookup_entry(&self.globals_record, name) else {
            return self.unbound_marker.clone();
        };
        let Some(cell) = entry.slots.get(1) else {
            return self.unbound_marker.clone();
        };
        cell.clone()
    }

    /// `boundp' equivalent against the elisp env mirror.  Convenience
    /// over `mirror_lookup_value' + sentinel comparison.
    pub(crate) fn mirror_is_bound(&self, name: &str) -> bool {
        self.mirror_lookup_value(name) != self.unbound_marker
    }

    /// `fboundp' equivalent against the elisp env mirror.
    pub(crate) fn mirror_is_fbound(&self, name: &str) -> bool {
        self.mirror_lookup_function(name) != self.unbound_marker
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
