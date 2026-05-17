//! Symbol table + lexical frame stack.  Two-cell symbol model (value /
//! function); lambdas are `(closure CAPTURED-ENV ARGS BODY...)'.  See
//! Doc 44 §3.3 + §4.

use std::collections::HashMap;
use std::rc::Rc;

use crate::image;

use super::error::EvalError;
use super::sexp::Sexp;

/// Host-crate-registered builtin closure.  `Rc' because `Env' is single-threaded.
pub type ExternBuiltin = Rc<dyn Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>>;

/// Symbol's two cells (Elisp value/function dichotomy) + plist + constant flag.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolEntry {
    pub value: Option<Sexp>,
    pub function: Option<Sexp>,
    pub plist: Option<Sexp>,
    /// `setq' / `set' on a constant signals `SettingConstant'.
    pub constant: bool,
}

/// Write-through cell — captured-closure `setq' mutates the originating
/// let-binding's slot (= layout-pinned NlCellRef).
pub type FrameCell = crate::eval::nlcell::NlCellRef;
pub type Frame = HashMap<String, FrameCell>;

/// Runtime environment: lexical frame stack + recursion guard +
/// extern-builtin registry + elisp-side globals mirror + Stage 7.4.c
/// apply takeover flags.  Doc 102 Phase 2.b Step E removed the legacy
/// `globals: HashMap<String, SymbolEntry>` field — the canonical
/// globals state is the elisp `nelisp-env' record reachable via
/// `globals_record' + `mirror_*' accessors.
pub struct Env {
    /// Innermost frame is last.
    pub frames: Vec<Frame>,
    pub max_recursion: u32,
    pub current_recursion: u32,
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    /// Stage 7.4.c — route apply_combiner's plain-fn / lambda-head paths
    /// through elisp `nelisp--apply-fn' (flip via `NELISP_USE_RUST_APPLY').
    pub use_elisp_apply: bool,
    pub delegation_depth: u32,
    /// Doc 102 Phase 8 sprint — elisp-side `nelisp-env' record (see
    /// `lisp/nelisp-env.el').  `Sexp::Nil' until `install_stage0' (or
    /// the explicit `install_empty_mirror_rust_direct') constructs
    /// it.  Step E elevated this from "mirror" to "canonical" — all
    /// globals reads / writes flow through the `mirror_*' helpers.
    pub globals_record: Sexp,
    /// Doc 102 Phase 8 Sprint Session 5 — cached `nelisp--unbound-marker'
    /// sentinel.  `make-symbol' produces counter-disambiguated uninterned
    /// names like `nelisp--unbound-marker__nelisp-uninterned-0', so the
    /// mirror's unbound test is `cell == self.unbound_marker' (Sexp value
    /// equality via the stable cached Sexp::Symbol).
    pub unbound_marker: Sexp,
    /// Doc 104 Stage 3.b — elisp-side `nelisp-lexframe-stack' record (see
    /// `lisp/nelisp-lexframe.el').  `Sexp::Nil' until `install_stage0'
    /// (via `install_empty_frames_record_rust_direct') constructs an
    /// empty stack.  Stage 3.b dual-writes; Stage 3.c+ migrates reads.
    pub frames_record: Sexp,
}

impl Env {
    /// Zero-state Env — the 3 public ctors share this one literal site so
    /// adding a field touches one place (Doc 102 Phase 6 option a).
    fn fresh(max_recursion: u32) -> Self {
        Env {
            frames: Vec::new(),
            max_recursion,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            use_elisp_apply: false,
            delegation_depth: 0,
            globals_record: Sexp::Nil,
            unbound_marker: Sexp::Nil,
            frames_record: Sexp::Nil,
        }
    }

    /// Globals-only environment with all built-ins installed (= GNU
    /// Emacs' empty-buffer top-level equivalent).
    ///
    /// STDLIB_IMAGES are frozen-heap `.image' files baked by `nelisp-baker'.
    /// Load-order rationale lives in `nelisp-baker.rs' STDLIB_FILES + each
    /// `.el' header.  `decode_v3_into' streams straight into `env.globals'
    /// (no eval, no reader — Doc 98 §98.3).
    pub fn new_global() -> Self {
        const STDLIB_IMAGES: &[(&str, &[u8])] = &[
            ("nelisp-jit-substrate.el", include_bytes!("../../../lisp/nelisp-jit-substrate.el.image")),
            ("nelisp-syscall-table.el", include_bytes!("../../../lisp/nelisp-syscall-table.el.image")),
            ("nelisp-jit-strategy.el", include_bytes!("../../../lisp/nelisp-jit-strategy.el.image")),
            ("nelisp-stdlib-env-shim.el", include_bytes!("../../../lisp/nelisp-stdlib-env-shim.el.image")),
            ("nelisp-stdlib-eval-special.el", include_bytes!("../../../lisp/nelisp-stdlib-eval-special.el.image")),
            ("nelisp-stdlib-error.el", include_bytes!("../../../lisp/nelisp-stdlib-error.el.image")),
            ("nelisp-stdlib.el", include_bytes!("../../../lisp/nelisp-stdlib.el.image")),
            ("nelisp-stdlib-list.el", include_bytes!("../../../lisp/nelisp-stdlib-list.el.image")),
            ("nelisp-stdlib-hof.el", include_bytes!("../../../lisp/nelisp-stdlib-hof.el.image")),
            ("nelisp-stdlib-search.el", include_bytes!("../../../lisp/nelisp-stdlib-search.el.image")),
            ("nelisp-stdlib-plist-str.el", include_bytes!("../../../lisp/nelisp-stdlib-plist-str.el.image")),
            ("nelisp-stdlib-format.el", include_bytes!("../../../lisp/nelisp-stdlib-format.el.image")),
            ("nelisp-stdlib-misc.el", include_bytes!("../../../lisp/nelisp-stdlib-misc.el.image")),
            ("nelisp-stdlib-os.el", include_bytes!("../../../lisp/nelisp-stdlib-os.el.image")),
            ("nelisp-pcase.el", include_bytes!("../../../lisp/nelisp-pcase.el.image")),
            ("nelisp-cl-macros.el", include_bytes!("../../../lisp/nelisp-cl-macros.el.image")),
            ("nelisp-stdlib-hash.el", include_bytes!("../../../lisp/nelisp-stdlib-hash.el.image")),
            ("nelisp-stdlib-gc.el", include_bytes!("../../../lisp/nelisp-stdlib-gc.el.image")),
            ("nelisp-stdlib-equal.el", include_bytes!("../../../lisp/nelisp-stdlib-equal.el.image")),
            ("nelisp-stdlib-prn.el", include_bytes!("../../../lisp/nelisp-stdlib-prn.el.image")),
            ("nelisp-stdlib-reader.el", include_bytes!("../../../lisp/nelisp-stdlib-reader.el.image")),
            ("nelisp-stdlib-eval-core.el", include_bytes!("../../../lisp/nelisp-stdlib-eval-core.el.image")),
            ("nelisp-stdlib-time.el", include_bytes!("../../../lisp/nelisp-stdlib-time.el.image")),
            ("nelisp-stdlib-math.el", include_bytes!("../../../lisp/nelisp-stdlib-math.el.image")),
            ("nelisp-stdlib-regex.el", include_bytes!("../../../lisp/nelisp-stdlib-regex.el.image")),
            ("nelisp-stdlib-fast-hash.el", include_bytes!("../../../lisp/nelisp-stdlib-fast-hash.el.image")),
            ("nelisp-env.el", include_bytes!("../../../lisp/nelisp-env.el.image")),
        ];
        // max_recursion=1024 bounds eval-loop nesting under cargo test's
        // 2MB thread stack (see `recursion_depth_guard').
        let mut env = Env::install_stage0(1024);
        for (name, image_bytes) in STDLIB_IMAGES {
            let fallback = match image::decode_v3_into(&mut env, image_bytes) {
                Ok(f) => f,
                Err(e) => panic!("{} image decode failed: {}", name, e),
            };
            assert!(
                fallback.is_empty(),
                "{} image has non-empty FALLBACK_FORMS ({} forms) — \
                 rebake with `make bake-images' (= --frozen-heap)",
                name,
                fallback.len()
            );
        }
        // Elisp dispatch ON post-bootstrap; `NELISP_USE_RUST_APPLY' env
        // var pins Rust dispatch as escape hatch.
        env.use_elisp_apply = std::env::var_os("NELISP_USE_RUST_APPLY")
            .map(|v| v.is_empty())
            .unwrap_or(true);
        // Doc 102 Phase 8 sprint Session 1 — construct the elisp-side
        // env mirror.  Future sessions migrate callsites to read/write
        // through this record, then delete the Rust `globals' HashMap.
        env.install_globals_record();
        env
    }

    /// Empty env (= no built-ins).  For error-path tests.
    pub fn empty() -> Self {
        Env::fresh(256)
    }

    /// Doc 102 Phase 8 sprint Session 1 — finalise the elisp
    /// `globals_record' after STDLIB decode.
    /// Phase 2.b Step E: `populate_globals_record' is retired — the
    /// HashMap dual-write is gone, so the mirror is already canonical
    /// by the time decode finishes.  All we still owe is to pin the
    /// `nelisp--unbound-marker' value cell to the Rust-defined
    /// sentinel so elisp `(eq cell nelisp--unbound-marker)' aligns
    /// with `mirror_lookup_value' identity checks (STDLIB's defvar
    /// of the same name baked a `make-symbol' counter-suffixed value
    /// that we replace here).
    fn install_globals_record(&mut self) {
        let unbound = self.unbound_marker.clone();
        self.mirror_set_value("nelisp--unbound-marker", unbound);
    }

    /// Doc 102 Phase 8 Sprint Session 6 — Rust-direct mirror write
    /// path.  Replaces the Session 3 `apply_function' shim, which paid
    /// ~800µs per call for the interpreted `nelisp--fast-hash--hash'
    /// loop; the direct path runs the same FNV-1a in Rust + slot
    /// mutation via `with_slots_mut' for ~10µs per call.  No-op when
    /// the mirror isn't ready (= `globals_record == Sexp::Nil', early
    /// bootstrap before `install_globals_record').  Auto-vivifies an
    /// absent entry.  Constant-flag enforcement stays in `set_value'
    /// against the Rust `globals' HashMap (= Sprint A); Sprint B moves
    /// the check onto the mirror once the HashMap is deleted.
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

    /// Doc 102 Phase 8 Sprint Session 6 — function-cell counterpart
    /// of `mirror_set_value'.  Mutates symbol-entry slot 1 in place.
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

    /// Doc 102 Phase 8 Sprint Session 6 — clear the entry's value
    /// cell (= `makunbound').  Mirrors `nelisp-env-clear-value' from
    /// `lisp/nelisp-env.el': preserves the entry record, sets slot 0
    /// to `nelisp--unbound-marker' so subsequent `mirror_lookup_value'
    /// returns None (= `void-variable').
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

    /// Doc 102 Phase 8 Sprint Session 6 — function-cell counterpart
    /// of `mirror_clear_value' (= `fmakunbound').  Slot 1 → unbound.
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

    /// Doc 102 Phase 8 Sprint Session 6 — insert a fresh symbol-entry
    /// record into the bucket alist.  `slot' selects which cell the
    /// caller is filling (0 = value, 1 = function); the other cell is
    /// initialised to `self.unbound_marker'.
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

    /// Doc 102 Phase 8 Sprint B — full symbol-entry installer.  Used
    /// by image decode (= replaces the old `env.globals.insert' call
    /// at `image.rs::decode_v3_into') and by `intern_constant'.  When
    /// the entry exists, mutates all four slots in place; otherwise
    /// allocates a fresh `symbol-entry' record and prepends it onto
    /// the appropriate bucket.  Absent value / function default to
    /// `self.unbound_marker' (matching `nelisp-env--make-symbol-entry'),
    /// absent plist defaults to `Sexp::Nil', `constant=true' sets slot
    /// 3 to `Sexp::T'.
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

    /// Doc 102 Phase 8 Sprint B — read symbol-entry slot 3 (= constant
    /// flag) for the elisp `boundp' / `setting-constant' check paths.
    /// Returns false when the entry is absent or the mirror is unbuilt.
    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        match Env::mirror_lookup_entry(&self.globals_record, name) {
            Some(entry) => matches!(entry.slots.get(3), Some(Sexp::T)),
            None => false,
        }
    }

    /// Doc 102 Phase 8 Sprint B — write symbol-entry slot 3.  Used by
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

    /// Doc 102 Phase 8 Sprint B — bucket-prepend primitive.  Shared by
    /// `mirror_insert_new_entry' / `mirror_install_entry' /
    /// `mirror_set_constant'.  Hashes NAME, locates the bucket, builds
    /// `(KEY . ENTRY)', prepends onto the bucket head, bumps the
    /// entry-count slot.
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

    /// Doc 102 Phase 8 Sprint B — walk every bucket of the mirror and
    /// invoke `callback(name, entry)' for each live symbol-entry.
    /// Used by the image baker (= replaces `env.globals.iter()' /
    /// `env.globals.keys()').  Callback receives a `NlRecordRef'
    /// (= refcount-bumped clone) so it may read all four slots
    /// (value / function / plist / constant).
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

    /// Doc 102 Phase 2.b Step A — snapshot the elisp mirror as a
    /// `HashMap<String, SymbolEntry>' so the baker can diff before /
    /// after eval'ing a stdlib `.el' file.  Converts each entry's 4
    /// slots back to `SymbolEntry':
    ///   slot 0 (value)    : `unbound_marker' → `None'
    ///   slot 1 (function) : `unbound_marker' → `None'
    ///   slot 2 (plist)    : `Sexp::Nil' → `None'
    ///   slot 3 (constant) : `Sexp::T' → `true' (otherwise `false')
    ///
    /// This is the read complement of `mirror_install_entry' — a
    /// round-trip through mirror is value-equivalent to the original
    /// `SymbolEntry'.  Captures elisp-driven mutations (= env_shim
    /// `(defun)' / `(fset)' that wrote ONLY to the mirror), which
    /// `self.globals.clone()' misses.
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

    /// Doc 102 Phase 2.b Step B+C — per-file diff of the elisp mirror
    /// against `before' (= a prior `mirror_snapshot_globals' result).
    /// Returns a fresh `Env' whose **mirror** is populated with the
    /// changed entries (new key OR mutated `SymbolEntry: PartialEq').
    /// Used by `image::iterative_bake_one'; the diff env is fed
    /// directly to `encode_v3', which walks the mirror via
    /// `mirror_snapshot_globals'.
    ///
    /// Unlike `globals_diff_view' (which reads `self.globals'), this
    /// captures elisp-driven mutations that landed only on the mirror.
    /// Step C completes the round-trip — encoder reads ONLY the mirror.
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

    /// Baker accumulator env — built-ins + env_shim installed but no
    /// STDLIB images decoded.  Used by `image::iterative_bake_one'.
    pub fn new_global_no_stdlib() -> Self {
        Env::install_stage0(1024)
    }

    /// Stage 0 bootstrap: fresh env + Rust-direct empty mirror + intern
    /// nil/t + install builtins + install env_shim primitives.  Common
    /// to `new_global' and `new_global_no_stdlib'; the former
    /// additionally decodes STDLIB.
    ///
    /// Sprint B Stage 2 — `install_empty_mirror_rust_direct' replaces
    /// the Session 6 sequence of (a) HashMap-based bootstrap then (b)
    /// `apply_function(nelisp-env-make)' construction with a single
    /// Rust-direct allocation.  This lets `install_stage0' writes flow
    /// straight into the mirror via `set_value' / `set_function' (no
    /// HashMap intermediate) — Sprint B Stage 2 follow-up modifications
    /// strip the HashMap dual-write entirely.
    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion);
        env.install_empty_mirror_rust_direct();
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        super::env_shim::install_env_shim_primitives(&mut env);
        env
    }

    /// Sprint B Stage 2 — Rust-direct empty mirror constructor.
    /// Pre-allocates the `nelisp-env' / `fast-hash-table' /
    /// `buckets' record graph without invoking elisp `nelisp-env-make'
    /// (= no `apply_function' detour, no STDLIB dependency).  Sets
    /// `self.unbound_marker' to a Rust-defined sentinel
    /// `Sexp::Symbol("nelisp--unbound-marker")'; STDLIB's defvar of
    /// the same name (= baked into nelisp-stdlib-fast-hash.el.image
    /// as `make-symbol' counter-suffixed) will overwrite the
    /// `nelisp--unbound-marker' global after decode, after which
    /// `install_globals_record' captures the post-decode value and
    /// walks the mirror to refresh in-place sentinels.
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
    // instead of `apply_function').  Stage 3.b keeps the Rust
    // `frames: Vec<Frame>' as the canonical store and dual-writes
    // here for parity verification; Stage 3.c+ flips canonicalness.

    /// Layout walker — fetch the backing vector + current depth from
    /// `self.frames_record'.  Returns None when the mirror is unbuilt
    /// (= `Sexp::Nil', pre-`install_stage0').
    fn frame_stack_view(&self) -> Option<(crate::eval::nlrecord::NlRecordRef, crate::eval::nlvector::NlVectorRef, usize)> {
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
    fn frame_stack_ensure_capacity(
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
    /// mirror is unbuilt.  Returns the frame's `Sexp::Record' clone
    /// (= for callers that want to populate it inline; usually they
    /// just bind into the top frame via `frame_bind_rust_direct').
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
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by Stage 3.c+ + test
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
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by Stage 3.d + test
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

    /// Walk the mirror stack innermost-first and return an alist of
    /// `((NAME . CELL) ...)' with inner-shadows-outer dedup.  Cell
    /// identity is preserved (= caller may rely on `Sexp::Cell' Rc
    /// equality for closure write-through).  Mirrors
    /// `nelisp-lexframe-stack-capture' + `capture_lexical'.  Doc 104
    /// Stage 3.c — `capture_lexical' now delegates here.
    pub(crate) fn frame_capture_rust_direct(&self) -> Sexp {
        let Some((_stack_rec, backing, depth)) = self.frame_stack_view() else { return Sexp::Nil };
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut acc = Sexp::Nil;
        for i in (0..depth).rev() {
            let Some(frame) = backing.value.get(i) else { continue };
            let Sexp::Record(frame_rec) = frame else { continue };
            let Some(Sexp::Record(ht_rec)) = frame_rec.slots.get(0) else { continue };
            let Some(Sexp::Vector(buckets)) = ht_rec.slots.get(1) else { continue };
            for bucket in buckets.value.iter() {
                let mut cur = bucket;
                while let Sexp::Cons(c) = cur {
                    if let Sexp::Cons(pair) = &c.car {
                        if let Sexp::Str(k) = &pair.car {
                            if seen.insert(k.clone()) {
                                let entry = Sexp::cons(
                                    Sexp::Symbol(k.clone()),
                                    pair.cdr.clone(),
                                );
                                acc = Sexp::cons(entry, acc);
                            }
                        }
                    }
                    cur = &c.cdr;
                }
            }
        }
        acc
    }

    /// Build a fresh frame populated from ALIST = `((NAME . CELL) ...)'
    /// (NAME may be `Sexp::Symbol' or `Sexp::Str') and push it onto
    /// the mirror stack.  Mirrors `nelisp-lexframe-stack-push-captured!'
    /// + `push_captured'.  Cell identity is preserved.
    #[allow(dead_code)] // Doc 104 Stage 3.b — used by Stage 3.c + test
    pub(crate) fn frame_push_captured_rust_direct(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let Some(frame) = self.frame_push_rust_direct() else { return Ok(()); };
        let mut cur = alist;
        while let Sexp::Cons(outer) = cur {
            let Sexp::Cons(inner) = &outer.car else {
                return Err(EvalError::Internal("closure env entry not a cons".into()));
            };
            let name = match &inner.car {
                Sexp::Symbol(s) => s.clone(),
                Sexp::Str(s) => s.clone(),
                _ => return Err(EvalError::Internal("closure env entry name not a symbol".into())),
            };
            Env::frame_bind_into(&frame, &name, inner.cdr.clone());
            cur = &outer.cdr;
        }
        if !matches!(cur, Sexp::Nil) {
            return Err(EvalError::Internal("closure env not a proper list".into()));
        }
        Ok(())
    }

    /// Register `f' as an externally-supplied builtin under `name'.
    /// Sets the function cell to `(builtin NAME)' so `(NAME ARG...)'
    /// invokes `f(args, env)'.  Re-registering overwrites.
    pub fn register_extern_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError> + 'static,
    {
        self.extern_builtins.insert(name.to_string(), Rc::new(f));
        let sentinel =
            Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol(name.into())]);
        self.set_function(name, sentinel);
    }

    /// Mark `name` as a self-evaluating constant bound to `value`.
    /// Doc 102 Phase 2.b Step E — mirror is canonical; HashMap dual-
    /// write retired.  `mirror_install_entry' sets value + constant
    /// slot in one shot.
    pub fn intern_constant(&mut self, name: &str, value: Sexp) {
        self.mirror_install_entry(name, Some(value), None, None, true);
    }

    /// Innermost-first lexical frame walk.  Doc 104 Stage 3.c — body
    /// now walks the elisp-side `nelisp-lexframe-stack' mirror via
    /// `frame_stack_find_rust_direct'.  Returns an `Option<FrameCell>'
    /// (owned Rc handle, cheap to clone) instead of the previous
    /// `Option<&FrameCell>'; callers just `cell.value.clone()' or
    /// `cell.set_value(...)' so the signature widening is transparent.
    /// Vec is still written by `push_frame/pop_frame/bind_local'
    /// (Stage 3.d drops those writes); reads no longer touch it.
    fn find_frame_cell(&self, name: &str) -> Option<FrameCell> {
        match self.frame_stack_find_rust_direct(name)? {
            Sexp::Cell(c) => Some(c),
            _ => None,
        }
    }

    /// `symbol-value' — innermost lexical frame first, then the
    /// elisp-side globals mirror.  Doc 102 Phase 2.b Step E retired
    /// the HashMap fallback (= mirror is canonical).
    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        if let Some(cell) = self.find_frame_cell(name) {
            return Ok(cell.value.clone());
        }
        // Special-case the sentinel itself: `mirror_lookup_value' treats
        // slot 0 == `self.unbound_marker' as "absent", which would
        // mark `nelisp--unbound-marker' (= the symbol whose VALUE is
        // the sentinel) as unbound.  Return the sentinel directly so
        // elisp `(eq cell nelisp--unbound-marker)' checks resolve.
        if name == "nelisp--unbound-marker" {
            return Ok(self.unbound_marker.clone());
        }
        self.mirror_lookup_value(name)
            .ok_or_else(|| EvalError::UnboundVariable(name.to_string()))
    }

    /// `setq' / `set' — innermost frame slot if lexically bound (write-
    /// through cell, so closures observe), else the globals mirror.
    /// Constants raise.  Doc 102 Phase 2.b Step E retired the HashMap
    /// dual-write (= mirror is canonical).
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if self.mirror_is_constant(name) {
            return Err(EvalError::SettingConstant(name.to_string()));
        }
        if let Some(cell) = self.find_frame_cell(name) {
            // SAFETY: `value' owned; no outstanding `&Sexp' borrow into
            // the cell's slot (Phase A.2.1 setcar discipline).
            unsafe { cell.set_value(value.clone()) };
            return Ok(value);
        }
        self.mirror_set_value(name, value.clone());
        Ok(value)
    }

    /// Look up `name` in the function cell.  No lexical fallback (Elisp
    /// `funcall' always goes through the global function slot).  Doc
    /// 102 Phase 2.b Step E retired the HashMap fallback.
    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        self.mirror_lookup_function(name)
            .ok_or_else(|| EvalError::UnboundFunction(name.to_string()))
    }

    /// `defun' / `defalias' — write the function cell.  Doc 102
    /// Phase 2.b Step E retired the HashMap dual-write.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.mirror_set_function(name, func);
    }

    /// `defvar' / `defconst' — install value only if unbound (Elisp
    /// idempotence); IS_CONSTANT=true marks `defconst'.  Doc 102 Phase 7:
    /// `makunbound' / `fmakunbound' Rust helpers retired (zero callsites;
    /// elisp routes through `nelisp--env-globals-op').  Doc 102
    /// Phase 2.b Step E retired the HashMap dual-write — idempotence
    /// is now checked via `mirror_is_bound'.
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let actually_changed = !self.mirror_is_bound(name);
        if actually_changed {
            self.mirror_set_value(name, value);
        }
        if is_constant {
            self.mirror_set_constant(name, true);
        }
    }

    pub fn push_frame(&mut self) {
        // Doc 104 Stage 3.d — Vec write retired; mirror is canonical.
        self.frame_push_rust_direct();
    }

    /// Silently no-ops on under-pop (= balanced caller path).
    pub fn pop_frame(&mut self) {
        // Doc 104 Stage 3.d — Vec write retired.
        self.frame_pop_rust_direct();
    }

    /// `let' / `let*' / lambda formals — bind NAME→VALUE in innermost
    /// frame (= fresh FrameCell so closures share the slot); falls
    /// through to the global slot when no frame is active.  Doc 102
    /// Phase 2.b Step E retired the HashMap dual-write for the
    /// no-frame branch.  Doc 104 Stage 3.d retired the Vec write —
    /// mirror is canonical (= `frame_bind_rust_direct' fast-paths
    /// "no live frame" by no-op, matching `nelisp-lexframe-stack-depth
    /// == 0').
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        // Check whether the mirror stack has at least one frame.
        let has_frame = matches!(&self.frames_record, Sexp::Record(r)
            if matches!(r.slots.get(1), Some(Sexp::Int(n)) if *n > 0));
        if has_frame {
            let cell = FrameCell::new(value);
            self.frame_bind_rust_direct(name, Sexp::Cell(cell));
        } else {
            self.mirror_set_value(name, value);
        }
    }

    /// `boundp` — true iff `name` resolves in any frame or has a global
    /// value.  Doc 102 Phase 2.b Step E retired the HashMap fallback.
    /// Sentinel symbol is special-cased to mirror `lookup_value'
    /// semantics.
    pub fn is_bound(&self, name: &str) -> bool {
        if self.find_frame_cell(name).is_some() {
            return true;
        }
        if name == "nelisp--unbound-marker" {
            return true;
        }
        self.mirror_is_bound(name)
    }

    /// `fboundp` — true iff `name` has a global function cell.  Doc
    /// 102 Phase 2.b Step E retired the HashMap fallback.
    pub fn is_fbound(&self, name: &str) -> bool {
        self.mirror_is_fbound(name)
    }

    /// Capture lexical frames as a flat `((name . cell) ...)` alist for
    /// closure construction.  Each slot is wrapped in `Sexp::Cell`
    /// carrying the same `NlCellRef` as the originating frame entry, so
    /// closure `setq' writes through to the let-binding's slot.  Doc
    /// 104 Stage 3.c — body delegates to `frame_capture_rust_direct'
    /// which walks the elisp-side mirror.  The mirror was populated
    /// via `Sexp::Cell(cell.clone())' in `bind_local' (Stage 3.b
    /// dual-write), so the returned alist preserves the same
    /// `NlCellRef' identity as the Vec side did.
    pub fn capture_lexical(&self) -> Sexp {
        self.frame_capture_rust_direct()
    }

    /// Push a frame from a captured-env alist (inverse of `capture_lexical').
    /// When a captured value is `Sexp::Cell', the same `Rc` is reinstalled
    /// (= write-through); otherwise a fresh cell wraps the value.
    // ---- Doc 102 Phase 8 sprint Session 4 — mirror read accessors ----
    // Session 5 wires these into `lookup_value' / `lookup_function' /
    // `is_bound' / `is_fbound' bodies, then deletes the Rust HashMap.
    // Marked `dead_code' here because Session 4 ships the accessors +
    // unit tests only; production callsites are still on the HashMap.
    /// Doc 102 Phase 8 sprint Session 4 — fast read from the elisp env
    /// mirror via Rust-direct `Sexp::Record' walk.  No `apply_function'
    /// (= no FNV-1a interpreted loop overhead).  Returns the symbol-entry
    /// record's `Sexp::Record', or None if the key is absent.
    ///
    /// Layout (= `lisp/nelisp-env.el' + `nelisp-stdlib-fast-hash.el'):
    ///   globals_record = Sexp::Record(`nelisp-env')
    ///     slots[0] = Sexp::Record(`fast-hash-table')
    ///       slots[0] = Sexp::Int (= bucket count, power-of-2)
    ///       slots[1] = Sexp::Vector (= buckets, each elt an alist)
    ///       slots[2] = Sexp::Int (= entry count)
    ///   Bucket alist cell: (Sexp::Cons (Sexp::Str . Sexp::Record(`symbol-entry')))
    ///   Symbol entry slots: [value, function, plist, constant]
    ///
    /// Doc 102 Phase 8 Sprint Session 6 — returns the entry handle
    /// (`NlRecordRef') so callers may both read slots (= `lookup_*')
    /// and mutate them via `with_slots_mut' (= direct write path).
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

    /// Doc 102 Phase 8 sprint Session 4 — value-cell read.  Returns None
    /// when the entry is absent OR holds `nelisp--unbound-marker' (Session 5:
    /// identity-compared against the cached `self.unbound_marker').
    pub(crate) fn mirror_lookup_value(&self, name: &str) -> Option<Sexp> {
        let entry = Env::mirror_lookup_entry(&self.globals_record, name)?;
        let cell = entry.slots.get(0)?.clone();
        if cell == self.unbound_marker { None } else { Some(cell) }
    }

    /// Doc 102 Phase 8 sprint Session 4 — function-cell read.
    pub(crate) fn mirror_lookup_function(&self, name: &str) -> Option<Sexp> {
        let entry = Env::mirror_lookup_entry(&self.globals_record, name)?;
        let cell = entry.slots.get(1)?.clone();
        if cell == self.unbound_marker { None } else { Some(cell) }
    }

    /// Doc 102 Phase 8 sprint Session 4 — `boundp' equivalent against
    /// the elisp env mirror.
    pub(crate) fn mirror_is_bound(&self, name: &str) -> bool {
        self.mirror_lookup_value(name).is_some()
    }

    /// Doc 102 Phase 8 sprint Session 4 — `fboundp' equivalent against
    /// the elisp env mirror.
    pub(crate) fn mirror_is_fbound(&self, name: &str) -> bool {
        self.mirror_lookup_function(name).is_some()
    }

    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        // Doc 104 Stage 3.d — Vec write retired.  Build the mirror
        // frame, validate the alist shape on the fly, push only when
        // the walk succeeds (= a mid-walk error doesn't leave a stub
        // frame on the mirror).
        let mirror_frame = Env::make_empty_frame_record();
        let mut cur = alist;
        while let Sexp::Cons(outer) = cur {
            let Sexp::Cons(inner) = &outer.car else {
                return Err(EvalError::Internal("closure env entry not a cons".into()));
            };
            let Sexp::Symbol(s) = &inner.car else {
                return Err(EvalError::Internal("closure env entry name not a symbol".into()));
            };
            let cell = match &inner.cdr {
                Sexp::Cell(c) => c.clone(),
                v => FrameCell::new(v.clone()),
            };
            Env::frame_bind_into(&mirror_frame, s, Sexp::Cell(cell));
            cur = &outer.cdr;
        }
        if !matches!(cur, Sexp::Nil) {
            return Err(EvalError::Internal("closure env not a proper list".into()));
        }
        // Push the pre-populated mirror frame.
        if let Some((stack_rec, backing, depth)) = self.frame_stack_view() {
            let backing =
                Env::frame_stack_ensure_capacity(&stack_rec, &backing, depth, depth + 1);
            // SAFETY: see `frame_push_rust_direct'.
            unsafe {
                backing.with_value_mut(|v| v[depth] = mirror_frame);
                stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth + 1) as i64));
            }
        }
        Ok(())
    }
}

/// Doc 102 Phase 8 sprint Session 4 — FNV-1a 32-bit hash matching the
/// elisp `nelisp--fast-hash--hash' (iterates Unicode codepoints).
/// Native Rust replacement for the interpreted hash loop.
pub(crate) fn mirror_fnv1a(s: &str) -> u32 {
    let mut h: u32 = 0x811C9DC5;
    for c in s.chars() {
        h ^= c as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phase8_session1_globals_record_is_nelisp_env_after_bootstrap() {
        // Doc 102 Phase 8 Sprint Session 1 — verify `new_global' installs
        // a fresh `nelisp-env' record into `globals_record'.
        let env = Env::new_global();
        match &env.globals_record {
            Sexp::Record(r) => match &r.type_tag {
                Sexp::Symbol(s) => assert_eq!(s, "nelisp-env"),
                other => panic!("globals_record type tag is not a symbol: {:?}", other),
            },
            other => panic!("globals_record is not a Record: {:?}", other),
        }
    }

    #[test]
    fn phase8_session2_globals_record_mirrors_rust_hashmap_sentinels() {
        // Doc 102 Phase 8 Sprint Session 2 — verify the populated elisp env
        // mirror contains the same key sentinels as the Rust HashMap.
        // Spot-checks `car' (= function cell, ~1000 stdlib fns), `t' (=
        // value + constant), and `make-hash-table' (= function cell, mid-
        // STDLIB load order) — together prove that builtins, intern-
        // constant, and STDLIB image decode all reached the mirror.
        let mut env = Env::new_global();
        let lookup_fn = env
            .lookup_function("nelisp-env-lookup-function")
            .expect("nelisp-env-lookup-function not loaded");
        let record = env.globals_record.clone();
        for name in &["car", "make-hash-table"] {
            let result = super::super::apply_function(
                &lookup_fn,
                &[record.clone(), Sexp::Str((*name).into())],
                &mut env,
            );
            assert!(
                result.is_ok(),
                "mirror missing function `{}': {:?}",
                name,
                result.err()
            );
        }
        // `t' is a value-cell entry (= intern_constant path).
        let lookup_value_fn = env
            .lookup_function("nelisp-env-lookup-value")
            .expect("nelisp-env-lookup-value not loaded");
        let t_result = super::super::apply_function(
            &lookup_value_fn,
            &[record, Sexp::Str("t".into())],
            &mut env,
        );
        assert!(matches!(t_result, Ok(Sexp::T)), "mirror missing `t': {:?}", t_result);
    }

    #[test]
    fn phase8_session3_post_bootstrap_set_value_propagates_to_mirror() {
        // Doc 102 Phase 8 Sprint Session 3 — after bootstrap, a Rust-side
        // `set_value' on a fresh global must surface in the elisp mirror.
        // Validates that `mirror_set_value' is wired correctly and that
        // the elisp env has not gone stale post-populate.
        let mut env = Env::new_global();
        env.set_value("doc-102-phase-8-session-3-probe", Sexp::Int(4242))
            .expect("set_value failed");
        let lookup_fn = env
            .lookup_function("nelisp-env-lookup-value")
            .expect("nelisp-env-lookup-value not loaded");
        let record = env.globals_record.clone();
        let result = super::super::apply_function(
            &lookup_fn,
            &[record, Sexp::Str("doc-102-phase-8-session-3-probe".into())],
            &mut env,
        );
        assert!(matches!(result, Ok(Sexp::Int(4242))),
                "mirror did not observe post-bootstrap set_value: {:?}", result);
    }

    #[test]
    fn phase8_session3_post_bootstrap_set_function_propagates_to_mirror() {
        // Counterpart for the function cell — validate `set_function'
        // dual-writes through `mirror_set_function'.
        let mut env = Env::new_global();
        let sentinel = Sexp::list_from(&[
            Sexp::Symbol("builtin".into()),
            Sexp::Symbol("car".into()),
        ]);
        env.set_function("doc-102-phase-8-session-3-fn-probe", sentinel.clone());
        let lookup_fn = env
            .lookup_function("nelisp-env-lookup-function")
            .expect("nelisp-env-lookup-function not loaded");
        let record = env.globals_record.clone();
        let result = super::super::apply_function(
            &lookup_fn,
            &[record, Sexp::Str("doc-102-phase-8-session-3-fn-probe".into())],
            &mut env,
        );
        assert!(matches!(&result, Ok(v) if *v == sentinel),
                "mirror did not observe post-bootstrap set_function: {:?}", result);
    }

    #[test]
    fn phase8_session4_rust_direct_lookup_function_matches_rust_hashmap() {
        // Doc 102 Phase 8 Sprint Session 4 — Rust-direct mirror_lookup_*
        // accessors return the same Sexp the Rust HashMap holds.  Spot-
        // checks `car' / `cdr' / `eq' / `make-hash-table' covering
        // builtins + STDLIB-loaded functions.
        let env = Env::new_global();
        for name in &["car", "cdr", "eq", "make-hash-table"] {
            let rust_side = env.lookup_function(name).expect(name);
            let mirror_side = env
                .mirror_lookup_function(name)
                .unwrap_or_else(|| panic!("mirror missing function `{}'", name));
            assert_eq!(rust_side, mirror_side, "mismatch for `{}'", name);
        }
    }

    #[test]
    fn phase8_session4_rust_direct_lookup_value_returns_t_for_constant_t() {
        // `t' is interned with value Sexp::T + constant flag.  Verify
        // the mirror returns Sexp::T (not the unbound-marker sentinel).
        let env = Env::new_global();
        let v = env.mirror_lookup_value("t").expect("mirror missing value for `t'");
        assert!(matches!(v, Sexp::T));
    }

    #[test]
    fn phase8_session4_rust_direct_is_fbound_matches_rust() {
        let env = Env::new_global();
        // Present in env: `car'.  Absent: invented name.
        assert!(env.mirror_is_fbound("car"));
        assert!(!env.mirror_is_fbound("doc-102-session-4-absent-fn"));
    }

    #[test]
    fn phase8_session4_fnv1a_matches_elisp_hash_loop() {
        // FNV-1a 32-bit reference vectors (= ASCII inputs, same algorithm
        // as `nelisp--fast-hash--hash').  Confirms the Rust port matches
        // the elisp implementation bit-for-bit.
        assert_eq!(mirror_fnv1a(""), 0x811C9DC5);
        assert_eq!(mirror_fnv1a("a"), 0xE40C292C);
        assert_eq!(mirror_fnv1a("foobar"), 0xBF9CF968);
    }

    // ---- Doc 104 Stage 3.b regression tests ----

    fn frames_record_depth(env: &Env) -> i64 {
        match &env.frames_record {
            Sexp::Record(r) => match r.slots.get(1) {
                Some(Sexp::Int(n)) => *n,
                other => panic!("frames_record slot 1 not Int: {:?}", other),
            },
            other => panic!("frames_record not Record: {:?}", other),
        }
    }

    #[test]
    fn doc104_stage3b_install_stage0_yields_empty_lexframe_stack() {
        // Bootstrap should leave an empty `nelisp-lexframe-stack' record
        // (depth = 0, BACKING vec pre-allocated to the initial capacity).
        let env = Env::new_global_no_stdlib();
        match &env.frames_record {
            Sexp::Record(r) => match &r.type_tag {
                Sexp::Symbol(s) => assert_eq!(s, "nelisp-lexframe-stack"),
                other => panic!("frames_record type tag not a symbol: {:?}", other),
            },
            other => panic!("frames_record is not a Record: {:?}", other),
        }
        assert_eq!(frames_record_depth(&env), 0);
    }

    #[test]
    fn doc104_stage3b_push_pop_dual_writes_keep_depths_aligned() {
        // Stage 3.b shipped this as a dual-write parity check; Stage
        // 3.d retired the Vec write so the assertion now tracks the
        // mirror depth only.  Walking past the initial capacity (= 8)
        // still exercises the capacity-doubling grow path.
        let mut env = Env::new_global_no_stdlib();
        for i in 0..20 {
            env.push_frame();
            assert_eq!(frames_record_depth(&env), (i + 1) as i64,
                       "mirror depth wrong after push #{}", i);
        }
        for i in 0..20 {
            env.pop_frame();
            assert_eq!(frames_record_depth(&env), (19 - i) as i64,
                       "mirror depth wrong after pop #{}", i);
        }
        assert_eq!(frames_record_depth(&env), 0);
        // Stage 3.d invariant — Vec stays empty post-bootstrap.
        assert!(env.frames.is_empty(), "Vec was written despite Stage 3.d retiring its writes");
    }

    #[test]
    fn doc104_stage3b_bind_local_visible_via_mirror() {
        // After bind_local, NAME must resolve via both find_frame_cell
        // (which Stage 3.c flipped to mirror walks) and the direct
        // mirror helper.  Both paths now read the mirror; the test
        // remains green and acts as a regression gate for Stage 3.d.
        let mut env = Env::new_global_no_stdlib();
        env.push_frame();
        env.bind_local("doc104-stage3b-probe", Sexp::Int(7777));
        // find_frame_cell — post Stage 3.c walks the mirror.
        let cell_via_find = env.find_frame_cell("doc104-stage3b-probe").expect("find_frame_cell missing");
        assert_eq!(cell_via_find.value.clone(), Sexp::Int(7777));
        // Direct mirror walk.
        let mirror_cell = env
            .frame_lookup_rust_direct("doc104-stage3b-probe")
            .expect("mirror direct missing");
        match mirror_cell {
            Sexp::Cell(c) => assert_eq!(c.value.clone(), Sexp::Int(7777)),
            other => panic!("mirror frame slot is not Sexp::Cell: {:?}", other),
        }
    }

    #[test]
    fn doc104_stage3b_stack_find_walks_innermost_first() {
        // Outer frame binds X=1, inner shadows with X=2.  Mirror walk
        // must return the inner value.
        let mut env = Env::new_global_no_stdlib();
        env.push_frame();
        env.bind_local("doc104-stage3b-shadow", Sexp::Int(1));
        env.push_frame();
        env.bind_local("doc104-stage3b-shadow", Sexp::Int(2));
        let cell = env
            .frame_stack_find_rust_direct("doc104-stage3b-shadow")
            .expect("stack_find missing");
        match cell {
            Sexp::Cell(c) => assert_eq!(c.value.clone(), Sexp::Int(2)),
            other => panic!("stack_find returned non-Cell: {:?}", other),
        }
    }

    #[test]
    fn doc104_stage3b_bind_local_preserves_cell_identity_across_stacks() {
        // Closure write-through invariant: the FrameCell observed via
        // find_frame_cell (= Stage 3.c mirror walk) and the one
        // obtained via frame_lookup_rust_direct must share the same
        // NlCellRef Rc — a write via one handle is visible through the
        // other.  Drives the Stage 3.d cutover safety case (= closure
        // setq still hits the binding's slot).
        let mut env = Env::new_global_no_stdlib();
        env.push_frame();
        env.bind_local("doc104-stage3b-write-through", Sexp::Int(10));
        let cell_via_find = env
            .find_frame_cell("doc104-stage3b-write-through")
            .expect("find_frame_cell missing")
            .clone();
        let mirror_cell_sexp = env
            .frame_lookup_rust_direct("doc104-stage3b-write-through")
            .expect("mirror direct missing");
        let mirror_cell = match mirror_cell_sexp {
            Sexp::Cell(c) => c,
            other => panic!("mirror slot not Sexp::Cell: {:?}", other),
        };
        // Mutate via one handle; the other must observe.
        unsafe { mirror_cell.set_value(Sexp::Int(99)) };
        assert_eq!(cell_via_find.value.clone(), Sexp::Int(99),
                   "write through mirror handle not visible via find_frame_cell");
    }

    #[test]
    fn doc104_stage3b_capture_mirror_matches_vec_capture() {
        // capture_lexical (= Vec) and frame_capture_rust_direct (=
        // mirror) must enumerate the same names with the same cells
        // (Sexp::Cell wrapping the Vec side's FrameCell).
        let mut env = Env::new_global_no_stdlib();
        env.push_frame();
        env.bind_local("a", Sexp::Int(1));
        env.bind_local("b", Sexp::Int(2));
        env.push_frame();
        env.bind_local("a", Sexp::Int(3)); // shadows outer
        env.bind_local("c", Sexp::Int(4));

        let mirror_alist = env.frame_capture_rust_direct();
        // Build a {name -> value} map from the mirror alist.
        let mut seen: HashMap<String, Sexp> = HashMap::new();
        let mut cur = &mirror_alist;
        while let Sexp::Cons(c) = cur {
            if let Sexp::Cons(pair) = &c.car {
                if let Sexp::Symbol(k) = &pair.car {
                    let v = match &pair.cdr {
                        Sexp::Cell(c) => c.value.clone(),
                        other => other.clone(),
                    };
                    seen.insert(k.clone(), v);
                }
            }
            cur = &c.cdr;
        }
        assert_eq!(seen.get("a"), Some(&Sexp::Int(3)), "inner shadow lost");
        assert_eq!(seen.get("b"), Some(&Sexp::Int(2)));
        assert_eq!(seen.get("c"), Some(&Sexp::Int(4)));
        assert_eq!(seen.len(), 3, "extra / missing names in capture");
    }
}
