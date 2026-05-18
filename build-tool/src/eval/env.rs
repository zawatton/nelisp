//! Symbol table + lexical frame stack.  Two-cell symbol model
//! (value / function); lambdas are `(closure CAPTURED-ENV ARGS BODY...)'.
//! Doc 44 §3.3 + §4.  Doc 131 trimmed narrative + carved test mod (= Doc
//! 130 pattern) — see `tests/env_integration.rs'.

use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;
use super::sexp::Sexp;

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
    /// Stage 7.4.c — route apply_combiner's plain-fn / lambda-head
    /// paths through elisp `nelisp--apply-fn' (flip via `NELISP_USE_RUST_APPLY').
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
            e!("nelisp-stdlib-misc.el"),      e!("nelisp-stdlib-os.el"),
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
