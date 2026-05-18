//! Symbol table + lexical frame stack — type definitions and the
//! globals-mirror / lexframe-stack helpers that walk the elisp-side
//! `nelisp-env' + `nelisp-lexframe-stack' records (= `Env::globals_record'
//! / `Env::frames_record').  See `lisp/nelisp-env.el' +
//! `lisp/nelisp-lexframe.el' for the record layout.
//!
//! Doc 131 (2026-05-18) — merged the former `eval/env.rs' content here
//! (struct Env, type aliases, public methods on Env).  Single-file
//! consolidation cuts -342 LOC out of `src/eval/'.

use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;
use super::sexp::Sexp;

// ===========================================================================
// Public types (formerly `eval/env.rs').  Doc 44 §3.3 + §4 — two-cell
// symbol model; lambdas are `(closure CAPTURED-ENV ARGS BODY...)'.
// ===========================================================================

/// Host-crate-registered builtin closure.  Doc 102 Phase 7 + Doc 130 —
/// extension point for `tests/eval_integration.rs'.  Production binary
/// never inserts (`nelisp--env-globals-op' is a `builtins::dispatch'
/// match arm as of Phase 6).
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

/// Runtime environment.  Doc 102 Phase 2.b Step E removed the legacy
/// `globals: HashMap` (canonical is the elisp `nelisp-env' record at
/// `globals_record').  Doc 104 Stage 3.e removed the legacy
/// `frames: Vec<HashMap>` (canonical is the elisp `nelisp-lexframe-stack'
/// record at `frames_record').
pub struct Env {
    pub max_recursion: u32,
    pub current_recursion: u32,
    /// Doc 102 Phase 7 + Doc 130 — extension point for the integration
    /// test binary.  Empty HashMap in production = no allocation cost.
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    /// Stage 7.4.c — route apply_combiner's plain-fn / lambda-head paths
    /// through elisp `nelisp--apply-fn' (flip via `NELISP_USE_RUST_APPLY').
    pub use_elisp_apply: bool,
    pub delegation_depth: u32,
    /// Elisp `nelisp-env' record (see `lisp/nelisp-env.el').  `Sexp::Nil'
    /// until `install_stage0' constructs it.  Step E elevated this from
    /// "mirror" to "canonical" — all globals reads/writes flow through
    /// the `mirror_*' helpers.
    pub globals_record: Sexp,
    /// Cached `nelisp--unbound-marker' sentinel (Doc 102 Phase 8 Session 5).
    /// Mirror's unbound test is `cell == self.unbound_marker' (stable
    /// cached `Sexp::Symbol').
    pub unbound_marker: Sexp,
    /// Elisp `nelisp-lexframe-stack' record (Doc 104 Stage 3.b).
    /// `Sexp::Nil' until `install_stage0' constructs an empty stack.
    pub frames_record: Sexp,
}

impl Env {
    /// Zero-state Env — the 3 public ctors share this one literal site so
    /// adding a field touches one place (Doc 102 Phase 6 option a).
    fn fresh(max_recursion: u32) -> Self {
        Env {
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
    /// Emacs' empty-buffer top-level equivalent).  Doc 126.B — boot
    /// path is `reader::read_all(source)' + `eval(form, env)' per
    /// top-level STDLIB form.  `NELISP_EVAL_BOOT_TRACE=1' prints
    /// per-form load progress.
    pub fn new_global() -> Self {
        // STDLIB load order — see each `.el' header for rationale.
        // max_recursion=1024 bounds eval-loop nesting under cargo test's
        // 2MB thread stack (see `recursion_depth_guard').
        macro_rules! e { ($n:literal) => { ($n, include_str!(concat!("../../../lisp/", $n))) } }
        const STDLIB_FILES: &[(&str, &str)] = &[
            e!("nelisp-jit-substrate.el"),    e!("nelisp-syscall-table.el"),
            e!("nelisp-jit-strategy.el"),     e!("nelisp-stdlib-env-shim.el"),
            e!("nelisp-stdlib-eval-special.el"), e!("nelisp-stdlib-error.el"),
            e!("nelisp-stdlib.el"),           e!("nelisp-stdlib-list.el"),
            e!("nelisp-stdlib-hof.el"),       e!("nelisp-stdlib-search.el"),
            e!("nelisp-stdlib-plist-str.el"), e!("nelisp-stdlib-format.el"),
            e!("nelisp-stdlib-misc.el"),      e!("nelisp-cc-ffi-int-helpers.el"),
            e!("nelisp-stdlib-os.el"),
            e!("nelisp-pcase.el"),            e!("nelisp-cl-macros.el"),
            e!("nelisp-stdlib-hash.el"),      e!("nelisp-stdlib-gc.el"),
            e!("nelisp-stdlib-equal.el"),     e!("nelisp-stdlib-prn.el"),
            e!("nelisp-stdlib-reader.el"),    e!("nelisp-stdlib-eval-core.el"),
            e!("nelisp-stdlib-time.el"),      e!("nelisp-stdlib-math.el"),
            e!("nelisp-stdlib-regex.el"),     e!("nelisp-stdlib-fast-hash.el"),
            e!("nelisp-env.el"),              e!("nelisp-lexframe.el"),
            e!("nelisp-cli.el"),
        ];
        let mut env = Env::install_stage0(1024);
        let trace = std::env::var_os("NELISP_EVAL_BOOT_TRACE")
            .map(|v| !v.is_empty() && v != "0")
            .unwrap_or(false);
        for (name, source) in STDLIB_FILES {
            let forms = match crate::reader::read_all(source) {
                Ok(f) => f,
                Err(e) => panic!("{} eval-boot read failed: {}", name, e),
            };
            for (idx, form) in forms.iter().enumerate() {
                if trace {
                    eprintln!("[eval-boot] {}: form #{}", name, idx);
                }
                if let Err(e) = crate::eval::eval(form, &mut env) {
                    panic!(
                        "{} eval-boot eval failed at form #{}: {}\nform: {}",
                        name, idx, e, crate::eval::sexp::fmt_sexp(form),
                    );
                }
            }
        }
        // Elisp dispatch ON post-bootstrap; `NELISP_USE_RUST_APPLY' pins
        // Rust dispatch as escape hatch.
        env.use_elisp_apply = std::env::var_os("NELISP_USE_RUST_APPLY")
            .map(|v| v.is_empty())
            .unwrap_or(true);
        // Pin `nelisp--unbound-marker' value cell to the Rust-defined
        // sentinel (STDLIB's defvar bakes a counter-suffixed value).
        let unbound = env.unbound_marker.clone();
        env.mirror_set_value("nelisp--unbound-marker", unbound);
        env
    }

    /// Empty env (no built-ins).  Integration tests only.
    pub fn empty() -> Self {
        Env::fresh(256)
    }

    /// Built-ins + env_shim installed, no STDLIB.  Used by Doc 104
    /// Stage 3.b mirror_* direct-write tests in `tests/env_integration.rs'.
    pub fn new_global_no_stdlib() -> Self {
        Env::install_stage0(1024)
    }

    /// Stage 0 bootstrap: fresh env + empty mirror + intern nil/t +
    /// install builtins + env_shim primitives.
    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion);
        env.install_empty_mirror_rust_direct();
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        super::env_shim::install_env_shim_primitives(&mut env);
        env
    }

    /// Register `f' as an externally-supplied builtin (test-only).
    /// Integration test fixture entry — `extern_builtins' is consumed
    /// by the test scaffold only (production never registers).
    pub fn register_extern_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError> + 'static,
    {
        self.extern_builtins.insert(name.to_string(), Rc::new(f));
        let sentinel =
            Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol(name.into())]);
        self.set_function(name, sentinel);
    }

    /// Mark `name' as a self-evaluating constant bound to `value'.
    pub fn intern_constant(&mut self, name: &str, value: Sexp) {
        self.mirror_install_entry(name, Some(value), None, None, true);
    }

    /// Innermost-first lexical frame walk (Doc 104 Stage 3.c — body
    /// walks the elisp-side `nelisp-lexframe-stack' mirror).
    fn find_frame_cell(&self, name: &str) -> Option<FrameCell> {
        match self.frame_stack_find_rust_direct(name)? {
            Sexp::Cell(c) => Some(c),
            _ => None,
        }
    }

    /// `symbol-value' — innermost lexical frame first, then globals mirror.
    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        if let Some(cell) = self.find_frame_cell(name) {
            return Ok(cell.value.clone());
        }
        // The sentinel symbol resolves to the sentinel value, not "unbound"
        // — required for elisp `(eq cell nelisp--unbound-marker)' identity.
        if name == "nelisp--unbound-marker" {
            return Ok(self.unbound_marker.clone());
        }
        let v = self.mirror_lookup_value(name);
        if v == self.unbound_marker {
            Err(EvalError::UnboundVariable(name.to_string()))
        } else {
            Ok(v)
        }
    }

    /// `setq' / `set' — write-through frame cell if bound, else globals
    /// mirror.  Constants raise.
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if self.mirror_is_constant(name) {
            return Err(EvalError::SettingConstant(name.to_string()));
        }
        if let Some(cell) = self.find_frame_cell(name) {
            // SAFETY: value owned; no outstanding `&Sexp' borrow into the slot.
            unsafe { cell.set_value(value.clone()) };
            return Ok(value);
        }
        self.mirror_set_value(name, value.clone());
        Ok(value)
    }

    /// Look up `name' in the function cell.  No lexical fallback.
    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        let f = self.mirror_lookup_function(name);
        if f == self.unbound_marker {
            Err(EvalError::UnboundFunction(name.to_string()))
        } else {
            Ok(f)
        }
    }

    /// `defun' / `defalias' — write the function cell.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.mirror_set_function(name, func);
    }

    /// `defvar' / `defconst' — install only if unbound.  Post-bootstrap
    /// dispatches to elisp `nelisp-env-defvar'; pre-bootstrap uses the
    /// Rust `mirror_*' helpers directly.
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let f = match self.lookup_function("nelisp-env-defvar") {
            Ok(f) => f,
            Err(_) => {
                if !self.mirror_is_bound(name) {
                    self.mirror_set_value(name, value);
                }
                if is_constant {
                    self.mirror_set_constant(name, true);
                }
                return;
            }
        };
        // `Sexp::Str' — `nelisp--fast-hash-put' FNV-1a hashes byte
        // sequences; a Symbol would crash on `(length symbol)'.
        let args = [
            self.globals_record.clone(),
            Sexp::Str(name.to_string()),
            value,
            if is_constant { Sexp::T } else { Sexp::Nil },
        ];
        let _ = super::apply_function(&f, &args, self);
    }

    pub fn push_frame(&mut self) {
        self.frame_push_rust_direct();
    }

    /// Silently no-ops on under-pop (= balanced caller path).
    pub fn pop_frame(&mut self) {
        self.frame_pop_rust_direct();
    }

    /// `let' / `let*' / lambda formals — bind NAME→VALUE in innermost
    /// frame (= fresh FrameCell so closures share the slot); falls
    /// through to the global slot when no frame is active.
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        let has_frame = matches!(&self.frames_record, Sexp::Record(r)
            if matches!(r.slots.get(1), Some(Sexp::Int(n)) if *n > 0));
        if has_frame {
            let cell = FrameCell::new(value);
            self.frame_bind_rust_direct(name, Sexp::Cell(cell));
        } else {
            self.mirror_set_value(name, value);
        }
    }

    /// `boundp' — true iff `name' resolves in any frame or has a global value.
    pub fn is_bound(&self, name: &str) -> bool {
        if self.find_frame_cell(name).is_some() {
            return true;
        }
        if name == "nelisp--unbound-marker" {
            return true;
        }
        self.mirror_is_bound(name)
    }

    /// `fboundp' — true iff `name' has a global function cell.
    pub fn is_fbound(&self, name: &str) -> bool {
        self.mirror_is_fbound(name)
    }

    /// Capture lexical frames as a flat `((name . cell) ...)' alist for
    /// closure construction.  Each captured slot reuses the same
    /// `NlCellRef' so closure `setq' writes through to the let-binding.
    /// Pre-bootstrap (helper not loaded) returns Nil.
    pub fn capture_lexical(&mut self) -> Sexp {
        // Snapshot depth BEFORE dispatch — apply_lambda_inner pushes
        // its argument frame onto the same record during the call.
        let depth = match &self.frames_record {
            Sexp::Record(r) => match r.slots.get(1) {
                Some(Sexp::Int(n)) => *n,
                _ => return Sexp::Nil,
            },
            _ => return Sexp::Nil,
        };
        let f = match self.lookup_function("nelisp-lexframe-stack-capture-to-depth") {
            Ok(f) => f,
            Err(_) => return Sexp::Nil,
        };
        let args = [self.frames_record.clone(), Sexp::Int(depth)];
        super::apply_function(&f, &args, self).unwrap_or(Sexp::Nil)
    }

    /// Push a frame from a captured-env alist (inverse of `capture_lexical').
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let normalized = Env::wrap_alist_cells(alist)?;
        let f = match self.lookup_function("nelisp-lexframe-make-from-alist") {
            Ok(f) => f,
            Err(_) => return Ok(()),
        };
        let frame = super::apply_function(&f, &[normalized], self)?;
        if let Some((stack_rec, backing, depth)) = self.frame_stack_view() {
            let backing =
                Env::frame_stack_ensure_capacity(&stack_rec, &backing, depth, depth + 1);
            // SAFETY: see `frame_push_rust_direct'.
            unsafe {
                backing.with_value_mut(|v| v[depth] = frame);
                stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth + 1) as i64));
            }
        }
        Ok(())
    }
}

// ===========================================================================
// Globals mirror + lexical frame stack helpers.
// ===========================================================================

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
    /// 119 §119.A).  Used by `intern_constant'.
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
    /// entry)' for each live symbol-entry.  Callback receives a
    /// `NlRecordRef' (= refcount-bumped clone) so it may read all
    /// four slots.  Backs `mirror_snapshot_globals' (integration-
    /// test only consumer post-baker-retirement).
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

    /// Snapshot the elisp mirror as a `HashMap<String, SymbolEntry>'.
    /// Integration-test only post-baker-retirement (see
    /// `tests/eval_integration.rs').  Kept `pub' so the integration
    /// test binary in the separate `tests/' crate can reach it.
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
    /// sentinels.
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

    /// Value-cell read.  Returns `self.unbound_marker' on miss
    /// (sentinel re-injected here since the Phase 47 helper writes
    /// `Sexp::Nil' on miss).
    pub fn mirror_lookup_value(&self, name: &str) -> Sexp {
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
    pub fn mirror_lookup_function(&self, name: &str) -> Sexp {
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
    pub fn mirror_is_fbound(&self, name: &str) -> bool {
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
    pub fn frame_lookup_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        if depth == 0 { return None; }
        let frame = backing.value.get(depth - 1)?;
        Env::frame_lookup_in(frame, name)
    }

    /// Innermost-first walk across the entire mirror stack.  Returns
    /// the first NAME hit.  Mirrors `nelisp-lexframe-stack-find';
    /// `find_frame_cell' delegates here.
    pub fn frame_stack_find_rust_direct(&self, name: &str) -> Option<Sexp> {
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

