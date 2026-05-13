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

/// Runtime environment: globals + lexical frame stack + recursion guard
/// + extern-builtin registry + Stage 7.4.c apply takeover flags.
pub struct Env {
    pub globals: HashMap<String, SymbolEntry>,
    /// Innermost frame is last.
    pub frames: Vec<Frame>,
    pub max_recursion: u32,
    pub current_recursion: u32,
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    /// Stage 7.4.c — route apply_combiner's plain-fn / lambda-head paths
    /// through elisp `nelisp--apply-fn' (flip via `NELISP_USE_RUST_APPLY').
    pub use_elisp_apply: bool,
    pub delegation_depth: u32,
    /// Doc 102 Phase 8 sprint — elisp-side mirror of `globals' as a
    /// `nelisp-env' record (see `lisp/nelisp-env.el').  `Sexp::Nil' until
    /// `new_global' constructs it after STDLIB decode.  Session 1 (this
    /// commit) just allocates; sessions 2-5 migrate callsites + delete
    /// the Rust `HashMap'.
    pub globals_record: Sexp,
}

impl Env {
    /// Zero-state Env — the 3 public ctors share this one literal site so
    /// adding a field touches one place (Doc 102 Phase 6 option a).
    fn fresh(max_recursion: u32) -> Self {
        Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            max_recursion,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            use_elisp_apply: false,
            delegation_depth: 0,
            globals_record: Sexp::Nil,
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

    /// Doc 102 Phase 8 sprint Session 1 — invoke elisp `nelisp-env-make'
    /// (= `lisp/nelisp-env.el', loaded during STDLIB decode) and store the
    /// resulting nelisp-env record into `globals_record'.  Session 2 also
    /// populates the elisp env with the Rust HashMap's contents.
    fn install_globals_record(&mut self) {
        let make_fn = self
            .lookup_function("nelisp-env-make")
            .expect("nelisp-env-make not loaded (= nelisp-env.el missing from STDLIB)");
        self.globals_record = super::apply_function(&make_fn, &[], self)
            .expect("nelisp-env-make failed at bootstrap");
        self.populate_globals_record();
    }

    /// Doc 102 Phase 8 sprint Session 3 — keep the elisp env mirror
    /// current after a Rust-side `set_value' / `defvar' on a global.
    /// No-op when the mirror isn't ready yet (= early bootstrap, before
    /// `nelisp-env-set-value' has been loaded).  Lookup-on-each-call
    /// rather than caching the function Sexp so that `unload-feature'
    /// or test-time reload sees the new definition.
    fn mirror_set_value(&mut self, name: &str, value: Sexp) {
        let Ok(set_fn) = self.lookup_function("nelisp-env-set-value") else {
            return;
        };
        let record = self.globals_record.clone();
        let _ = super::apply_function(
            &set_fn,
            &[record, Sexp::Str(name.into()), value],
            self,
        );
    }

    /// Doc 102 Phase 8 sprint Session 3 — function-cell counterpart of
    /// `mirror_set_value'.  Triggered by `set_function' (= `defun' /
    /// `defalias' / `register_extern_builtin' callers).
    fn mirror_set_function(&mut self, name: &str, func: Sexp) {
        let Ok(set_fn) = self.lookup_function("nelisp-env-set-function") else {
            return;
        };
        let record = self.globals_record.clone();
        let _ = super::apply_function(
            &set_fn,
            &[record, Sexp::Str(name.into()), func],
            self,
        );
    }

    /// Doc 102 Phase 8 sprint Session 2 — mirror the Rust `globals'
    /// HashMap into the elisp `globals_record'.  Called once after the
    /// elisp record is constructed.  Session 3-4 migrate read/write
    /// callsites to use the mirror; session 5 deletes the Rust HashMap.
    /// Skips the constant flag (session 3 fills it once `nelisp-env-
    /// set-constant' lands or via direct record-set).
    fn populate_globals_record(&mut self) {
        let set_value_fn = self
            .lookup_function("nelisp-env-set-value")
            .expect("nelisp-env-set-value not loaded");
        let set_function_fn = self
            .lookup_function("nelisp-env-set-function")
            .expect("nelisp-env-set-function not loaded");
        // Snapshot keys to avoid mutable-borrow conflict during the
        // apply_function loop (= apply_function takes `&mut env').
        let snapshot: Vec<(String, SymbolEntry)> =
            self.globals.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        let record = self.globals_record.clone();
        for (name, entry) in snapshot {
            // `nelisp--fast-hash' uses `aref' / `length' on the key, so
            // pass the symbol name as a Sexp::Str (not Sexp::Symbol).
            if let Some(v) = entry.value {
                super::apply_function(
                    &set_value_fn,
                    &[record.clone(), Sexp::Str(name.clone()), v],
                    self,
                )
                .expect("nelisp-env-set-value failed at populate");
            }
            if let Some(f) = entry.function {
                super::apply_function(
                    &set_function_fn,
                    &[record.clone(), Sexp::Str(name), f],
                    self,
                )
                .expect("nelisp-env-set-function failed at populate");
            }
        }
    }

    /// Baker accumulator env — built-ins + env_shim installed but no
    /// STDLIB images decoded.  Used by `image::iterative_bake_one'.
    pub fn new_global_no_stdlib() -> Self {
        Env::install_stage0(1024)
    }

    /// Stage 0 bootstrap: fresh env + intern nil/t + install builtins +
    /// install env_shim primitives.  Common to `new_global' and
    /// `new_global_no_stdlib'; the former additionally decodes STDLIB.
    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion);
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        super::env_shim::install_env_shim_primitives(&mut env);
        env
    }

    /// Per-file globals diff vs. `before' (= new key OR mutated entry).
    /// Used by `image::iterative_bake_one'.
    pub fn globals_diff_view(&self, before: &HashMap<String, SymbolEntry>) -> Env {
        let mut diff = Env::empty();
        diff.globals = self
            .globals
            .iter()
            .filter(|(k, v)| before.get(*k).map_or(true, |prev| prev != *v))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        diff
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
    pub fn intern_constant(&mut self, name: &str, value: Sexp) {
        let entry = self.globals.entry(name.to_string()).or_default();
        entry.value = Some(value);
        entry.constant = true;
    }

    /// Innermost-first lexical frame walk.
    fn find_frame_cell(&self, name: &str) -> Option<&FrameCell> {
        self.frames.iter().rev().find_map(|f| f.get(name))
    }

    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        if let Some(cell) = self.find_frame_cell(name) {
            return Ok(cell.value.clone());
        }
        match self.globals.get(name) {
            Some(SymbolEntry { value: Some(v), .. }) => Ok(v.clone()),
            _ => Err(EvalError::UnboundVariable(name.to_string())),
        }
    }

    /// `setq' / `set' — innermost frame slot if lexically bound (write-
    /// through cell, so closures observe), else globals.  Constants raise.
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if matches!(self.globals.get(name), Some(SymbolEntry { constant: true, .. })) {
            return Err(EvalError::SettingConstant(name.to_string()));
        }
        if let Some(cell) = self.find_frame_cell(name) {
            // SAFETY: `value' owned; no outstanding `&Sexp' borrow into
            // the cell's slot (Phase A.2.1 setcar discipline).
            unsafe { cell.set_value(value.clone()) };
            return Ok(value);
        }
        self.globals.entry(name.to_string()).or_default().value = Some(value.clone());
        self.mirror_set_value(name, value.clone());
        Ok(value)
    }

    /// Look up `name` in the function cell.  No lexical fallback (Elisp
    /// `funcall' always goes through the global function slot).
    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        match self.globals.get(name) {
            Some(SymbolEntry { function: Some(f), .. }) => Ok(f.clone()),
            _ => Err(EvalError::UnboundFunction(name.to_string())),
        }
    }

    /// `defun' / `defalias' — write the function cell.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.globals.entry(name.to_string()).or_default().function = Some(func.clone());
        self.mirror_set_function(name, func);
    }

    /// `defvar' / `defconst' — install value only if unbound (Elisp
    /// idempotence); IS_CONSTANT=true marks `defconst'.  Doc 102 Phase 7:
    /// `makunbound' / `fmakunbound' Rust helpers retired (zero callsites;
    /// elisp routes through `nelisp--env-globals-op').
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let entry = self.globals.entry(name.to_string()).or_default();
        let actually_changed = entry.value.is_none();
        if actually_changed {
            entry.value = Some(value.clone());
        }
        if is_constant {
            entry.constant = true;
        }
        if actually_changed {
            self.mirror_set_value(name, value);
        }
    }

    pub fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    /// Silently no-ops on under-pop (= balanced caller path).
    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    /// `let' / `let*' / lambda formals — bind NAME→VALUE in innermost
    /// frame (= fresh FrameCell so closures share the slot); falls
    /// through to the global slot when no frame is active.
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.to_string(), FrameCell::new(value));
        } else {
            self.globals.entry(name.to_string()).or_default().value = Some(value);
        }
    }

    /// `boundp` — true iff `name` resolves in any frame or has a global value.
    pub fn is_bound(&self, name: &str) -> bool {
        self.find_frame_cell(name).is_some()
            || matches!(self.globals.get(name), Some(SymbolEntry { value: Some(_), .. }))
    }

    /// `fboundp` — true iff `name` has a global function cell.
    pub fn is_fbound(&self, name: &str) -> bool {
        matches!(self.globals.get(name), Some(SymbolEntry { function: Some(_), .. }))
    }

    /// Capture lexical frames as a flat `((name . cell) ...)` alist for
    /// closure construction.  Each slot is wrapped in `Sexp::Cell`
    /// carrying the same `NlCellRef` as the originating frame entry, so
    /// closure `setq' writes through to the let-binding's slot.
    pub fn capture_lexical(&self) -> Sexp {
        // Innermost-first dedup so the innermost binding wins;
        // `push_captured' consumes the result order-independently.
        let mut acc = Sexp::Nil;
        let mut seen = std::collections::HashSet::new();
        for frame in self.frames.iter().rev() {
            for (k, cell) in frame {
                if seen.insert(k.clone()) {
                    let pair = Sexp::cons(Sexp::Symbol(k.clone()), Sexp::Cell(cell.clone()));
                    acc = Sexp::cons(pair, acc);
                }
            }
        }
        acc
    }

    /// Push a frame from a captured-env alist (inverse of `capture_lexical').
    /// When a captured value is `Sexp::Cell', the same `Rc` is reinstalled
    /// (= write-through); otherwise a fresh cell wraps the value.
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let mut frame: Frame = HashMap::new();
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
            frame.insert(s.clone(), cell);
            cur = &outer.cdr;
        }
        if !matches!(cur, Sexp::Nil) {
            return Err(EvalError::Internal("closure env not a proper list".into()));
        }
        self.frames.push(frame);
        Ok(())
    }
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
}
