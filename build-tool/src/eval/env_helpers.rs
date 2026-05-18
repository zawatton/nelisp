//! Env struct + type defs + globals-mirror / lexframe-stack helpers walking
//! the elisp `nelisp-env' / `nelisp-lexframe-stack' records.  See
//! `lisp/nelisp-env.el' + `lisp/nelisp-lexframe.el' for layout.

use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;
use super::sexp::Sexp;

/// Test-fixture builtin closure (`tests/eval_integration.rs').  Production
/// never inserts; `extern_builtins' HashMap stays zero-state.
pub type ExternBuiltin = Rc<dyn Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>>;

/// Symbol's two cells + plist + constant flag.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolEntry {
    pub value: Option<Sexp>,
    pub function: Option<Sexp>,
    pub plist: Option<Sexp>,
    pub constant: bool,
}

/// Write-through cell — captured-closure `setq' mutates the binding slot.
pub type FrameCell = crate::eval::nlcell::NlCellRef;

/// Runtime environment.  `globals_record` / `frames_record` are the
/// canonical elisp records (Doc 102 Phase 2.b + Doc 104 Stage 3.e).
pub struct Env {
    pub max_recursion: u32,
    pub current_recursion: u32,
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    pub use_elisp_apply: bool,
    pub delegation_depth: u32,
    pub globals_record: Sexp,
    pub unbound_marker: Sexp,
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
        macro_rules! e { ($n:literal) => { ($n, include_str!(concat!("../../../lisp/", $n))) } }
        const STDLIB_FILES: &[(&str, &str)] = &[
            e!("nelisp-jit-substrate.el"),    e!("nelisp-syscall-table.el"),
            e!("nelisp-jit-strategy.el"),     e!("nelisp-stdlib-env-shim.el"),
            e!("nelisp-stdlib-eval-special.el"), e!("nelisp-stdlib-error.el"),
            e!("nelisp-stdlib.el"),           e!("nelisp-stdlib-list.el"),
            e!("nelisp-stdlib-hof.el"),       e!("nelisp-stdlib-search.el"),
            e!("nelisp-stdlib-plist-str.el"), e!("nelisp-stdlib-format.el"),
            e!("nelisp-stdlib-misc.el"),      e!("nelisp-stdlib-os-int-helpers.el"),
            e!("nelisp-stdlib-os.el"),
            e!("nelisp-pcase.el"),            e!("nelisp-cl-macros.el"),
            e!("nelisp-stdlib-hash.el"),
            e!("nelisp-stdlib-equal.el"),     e!("nelisp-stdlib-prn.el"),
            e!("nelisp-stdlib-reader.el"),    e!("nelisp-stdlib-eval-core.el"),
            e!("nelisp-stdlib-math.el"),
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
        env.use_elisp_apply = std::env::var_os("NELISP_USE_RUST_APPLY")
            .map(|v| v.is_empty())
            .unwrap_or(true);
        let unbound = env.unbound_marker.clone();
        env.mirror_set_value("nelisp--unbound-marker", unbound);
        env
    }

    /// Empty env (no built-ins) — integration tests only.
    pub fn empty() -> Self {
        Env::fresh(256)
    }

    /// Built-ins + env_shim installed, no STDLIB — test fixture.
    pub fn new_global_no_stdlib() -> Self {
        Env::install_stage0(1024)
    }

    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion);
        env.install_empty_mirror_rust_direct();
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        super::env_shim::install_env_shim_primitives(&mut env);
        env
    }

    /// Test-fixture builtin — production never registers.
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

    /// Innermost-first lexical frame walk via elisp `nelisp-lexframe-stack'.
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
        // Sentinel: `(eq cell nelisp--unbound-marker)` identity requires
        // the symbol resolve to the sentinel value, not "unbound".
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

    /// `fboundp' — true iff `name' has a global function cell.
    pub fn is_fbound(&self, name: &str) -> bool {
        self.mirror_is_fbound(name)
    }

    /// Capture lexical frames as `((name . cell) ...)' alist (closure
    /// share via NlCellRef; pre-bootstrap returns Nil).
    pub fn capture_lexical(&mut self) -> Sexp {
        // Snapshot depth BEFORE dispatch (apply_lambda_inner pushes its
        // own argument frame onto the same record during the call).
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

// SAFETY (file-wide): pointers passed to `elisp_cc_spike::*' refer to
// stack-local `Sexp' or `Env' fields outliving the call.  `*_or_insert'
// auto-vivify on miss; `*_clear_*' no-op on miss.

impl Env {
    // ---- Globals mirror helpers ----

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

    /// `makunbound' — write unbound_marker into slot 0; no-op on miss.
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

    /// Full 4-slot installer (value/function/plist/constant); auto-vivifies on miss.
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

    /// Walk every bucket, invoking `callback(name, entry)' per live symbol-entry.
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

    /// Integration-test snapshot — `pub' for separate `tests/' crate access.
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

    /// Pre-allocate the nelisp-env/fast-hash-table/buckets record graph
    /// without STDLIB dependency.  Sentinel `nelisp--unbound-marker' gets
    /// overwritten by STDLIB's defvar post-bootstrap.
    pub fn install_empty_mirror_rust_direct(&mut self) {
        const BUCKET_COUNT: usize = 1024;
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

    /// Value-cell read.  Returns `unbound_marker' on miss (Phase 47
    /// helper writes Nil; sentinel re-injected here).
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

    /// Build empty `nelisp-lexframe-stack' record: slots = [BACKING_VEC, depth=0].
    pub(crate) fn install_empty_frames_record_rust_direct(&mut self) {
        const INITIAL_CAPACITY: usize = 8;
        let backing = Sexp::vector(vec![Sexp::Nil; INITIAL_CAPACITY]);
        self.frames_record = Sexp::record(
            Sexp::Symbol("nelisp-lexframe-stack".into()),
            vec![backing, Sexp::Int(0)],
        );
    }

    /// Fetch (stack_rec, backing_vec, depth) from `frames_record'; None if unbuilt.
    pub(crate) fn frame_stack_view(&self) -> Option<(crate::eval::nlrecord::NlRecordRef, crate::eval::nlvector::NlVectorRef, usize)> {
        let stack_rec = match &self.frames_record { Sexp::Record(r) => r.clone(), _ => return None };
        let backing = match stack_rec.slots.get(0)? { Sexp::Vector(v) => v.clone(), _ => return None };
        let depth = match stack_rec.slots.get(1)? { Sexp::Int(n) => *n as usize, _ => return None };
        Some((stack_rec, backing, depth))
    }

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

    /// Grow `backing' via capacity doubling if `needed > backing.len()'.
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

    /// Bind NAME → CELL in innermost frame.  Caller wraps cell in Sexp::Cell.
    pub(crate) fn frame_bind_rust_direct(&mut self, name: &str, cell: Sexp) {
        let Some((_stack_rec, backing, depth)) = self.frame_stack_view() else { return };
        if depth == 0 { return; }
        let frame = match backing.value.get(depth - 1) { Some(f) => f.clone(), None => return };
        Env::frame_bind_into(&frame, name, cell);
    }

    /// `nelisp-lexframe-bind' inline: hash NAME via fnv1a, update-or-prepend.
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
