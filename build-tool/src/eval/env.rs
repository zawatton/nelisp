//! Symbol table + lexical-scope frame stack — Phase 8.0.2 (Doc 44 §3.3).
//!
//! Design notes (Doc 44 §3.3 + §4.1 + §4.2):
//!   - Heap = leak-tolerant Rust ownership (each [`Sexp`] is `Box`ed
//!     by the reader, the evaluator clones on read).  No GC, no arena.
//!     Doc 44 §4.1 case 2 (`arena drop`) is the takeover plan.
//!   - Symbol table = `HashMap<String, SymbolEntry>` per Doc 44 §3.3.
//!     Two-cell model (value cell + function cell) matches Elisp.
//!   - Environment = lexical-scope frame stack (`Vec<HashMap<...>>`).
//!     Even though Elisp default is dynamic binding, the bootstrap
//!     interpreter implements **lexical** binding for `let`, `lambda`
//!     argument lists, and `let*` because (a) the prompt §6.5 names
//!     this scope explicitly and (b) the Phase 7.1 native compiler
//!     takeover only sees lexical-binding code anyway.
//!
//! Lambdas are stored as `(closure CAPTURED-ENV ARGS BODY...)` cons
//! cells in the function cell of a symbol (or as anonymous values).
//! `CAPTURED-ENV` is encoded as a flat alist `((name . value) ...)`
//! so that the closure stays a plain [`Sexp`] and survives `eq` /
//! `equal` semantics.

use std::collections::HashMap;
use std::rc::Rc;

use crate::image;

use super::error::EvalError;
use super::sexp::Sexp;

/// Extension-point alias for builtin closures registered from outside
/// `nelisp-build-tool` (= host crates like `nelisp-emacs-gtk' that
/// expose GTK / SDL / native-OS primitives as elisp callables).
///
/// The signature mirrors the internal `bi_*' helpers' shape so the
/// dispatch fallback can call either uniformly.  `Rc` is used (not
/// `Arc`) because the `Env' is single-threaded; the closure itself
/// must be `'static' so the registry outlives any caller's borrow.
pub type ExternBuiltin = Rc<dyn Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>>;

/// A symbol's two cells per Elisp's value/function dichotomy.
///
/// `plist` is a property list, kept as a single [`Sexp`] (a proper
/// list of even length).  We only need it for `(put SYM PROP VAL)` /
/// `(get SYM PROP)`-shaped code paths the bootstrap may reach; it is
/// initialised to `nil`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SymbolEntry {
    pub value: Option<Sexp>,
    pub function: Option<Sexp>,
    pub plist: Option<Sexp>,
    /// `t` if this symbol is a constant (`nil`, `t`, keyword
    /// symbols).  `setq` / `set` on a constant signals
    /// [`EvalError::SettingConstant`].
    pub constant: bool,
}

impl SymbolEntry {
    pub fn new() -> Self {
        Self::default()
    }
}

/// One lexical frame.  A `let`, `let*`, or lambda body push one
/// frame; `setq` against a name in a frame mutates the frame slot.
///
/// Each slot is `Rc<RefCell<Sexp>>' (= a write-through cell) so a
/// closure that captures the frame can `setq' through the cell and
/// have the change visible at the originating let-binding's view.
/// Without the cell layer, `Env::capture_lexical' would have to
/// copy-by-value into the closure's captured-env alist and `setq'
/// would silently mutate only the copy (= bug fixed 2026-05-06).
/// Phase A.4 (Doc 77c §4.5, 2026-05-09): migrated from
/// `Rc<RefCell<Sexp>>' to layout-pinned [`NlCellRef`](crate::eval::nlcell::NlCellRef).
/// Same shared-ownership semantics + write-through closure invariant —
/// callers retain the pattern of cloning the handle to install in a
/// closure's captured-env alist + reading the slot via Deref / writing
/// via `unsafe set_value'.
pub type FrameCell = crate::eval::nlcell::NlCellRef;
pub type Frame = HashMap<String, FrameCell>;

/// The evaluator's runtime environment.
///
/// `globals` is the dynamic / global namespace (= `defvar` / `defun`
/// / `setq` of an unbound name).  `frames` is the lexical-binding
/// stack (innermost frame last).
///
/// `catch_stack` and `condition_stack` are runtime book-keeping for
/// `(catch TAG ...)` / `(throw TAG VAL)` and `(condition-case ...)`
/// respectively.  They are tracked as plain depth counters here and
/// the actual unwind happens via the `Result` return — this matches
/// Doc 44 §4.3's "use Result not Rust panic" directive.
pub struct Env {
    /// Global symbol table — `defvar`, `defun`, primitives, anything
    /// not in a lexical frame.
    pub globals: HashMap<String, SymbolEntry>,
    /// Lexical scope stack; innermost frame is last.
    pub frames: Vec<Frame>,
    /// Recursion depth guard so a runaway `(defun f () (f))` returns
    /// an error instead of overflowing the Rust stack.  Set lower than
    /// you might expect (256 instead of Emacs' default 1600) because
    /// debug-mode Rust frames are *much* fatter than Emacs C stack
    /// frames; the bootstrap source needs ~50 deep at most.  Callers
    /// can bump it for testing.
    pub max_recursion: u32,
    pub current_recursion: u32,
    /// External builtin registry — host crates (= `nelisp-emacs-gtk'
    /// for GTK4 GUI ops, future SDL2 / Win32 backends) register Rust
    /// closures here via [`Env::register_extern_builtin`] which then
    /// become callable from elisp under the registered name.  The
    /// `builtins::dispatch' fallback consults this map before
    /// surfacing `EvalError::UnboundFunction`.
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    /// Stage 7.4.c takeover flag — `true' routes apply_combiner's
    /// plain-fn / lambda-head paths through elisp `nelisp--apply-fn'
    /// (= default post-bootstrap; flip via `NELISP_USE_RUST_APPLY'
    /// env var or the `nelisp--set-use-elisp-apply' builtin).
    pub use_elisp_apply: bool,
    /// Re-entry guard for the elisp-apply takeover; when > 0 the
    /// dispatcher uses Rust direct-apply to avoid recursing back into
    /// the elisp `nelisp--apply-fn' through its own helpers.
    pub delegation_depth: u32,
}

impl Env {
    /// Construct a globals-only environment with all built-ins
    /// installed.  Equivalent to GNU Emacs' empty-buffer top-level.
    pub fn new_global() -> Self {
        // Bootstrap stdlib (Doc 72 Stage 7.7.b → Doc 98 §98.3):
        // each `lisp/*.el' has a sibling `*.image' produced by
        // `make bake-images' (= `nelisp-baker' running
        // `image::iterative_bake_one' on the source).  At startup
        // `image::decode_v3_into' streams the frozen-heap globals
        // straight into `env.globals' — no eval, no reader.  Load
        // order mirrors the pre-Stage-7.7.b STDLIB_SOURCES list
        // (= dep ordering rationale per-file in each `.el' header).
        const STDLIB_IMAGES: &[(&str, &[u8])] = &[
            // Doc 80 Stage 80.1 (2026-05-09): Pre-stdlib elisp
            // substrate (`cond' / `when' / `unless' / `defmacro'
            // self-boot / signal helpers).  Loads FIRST so the
            // jit-strategy wrappers below can express fall-through
            // dispatch in elisp.  See `lisp/nelisp-jit-substrate.el'.
            ("nelisp-jit-substrate.el", include_bytes!("../../../lisp/nelisp-jit-substrate.el.image")),
            // Doc 84 §84.2 (2026-05-10): AUTO-GENERATED by `build.rs'.
            ("nelisp-syscall-table.el", include_bytes!("../../../lisp/nelisp-syscall-table.el.image")),
            // Doc 77b Stage b.4 (2026-05-09): JIT-strategy wrappers
            // (= 24 elisp `(fset 'X (lambda ...))' overrides for the
            // pre-b.4 `lowered_X' Rust strategy fns).  Loads SECOND so
            // wrappers install BEFORE any later stdlib form invokes
            // the wrapped names.  See `lisp/nelisp-jit-strategy.el'.
            ("nelisp-jit-strategy.el", include_bytes!("../../../lisp/nelisp-jit-strategy.el.image")),
            // Doc 86 §86.3.a / Doc 89 Option C (2026-05-10): Tier 0 env
            // shim SKELETON.  See `nelisp-baker.rs' STDLIB_FILES for the
            // bootstrap-order rationale.  Slim primitives
            // (`nelisp--env-globals-*') are installed by
            // `env_shim::install_env_shim_primitives' BEFORE this loop
            // runs, so the wrappers below can `funcall' them at load
            // time.
            ("nelisp-stdlib-env-shim.el", include_bytes!("../../../lisp/nelisp-stdlib-env-shim.el.image")),
            ("nelisp-stdlib-eval-special.el", include_bytes!("../../../lisp/nelisp-stdlib-eval-special.el.image")),
            // Doc 86 §86.2 (2026-05-10): elisp condition system
            // substrate.  See `nelisp-baker.rs' for the load-order
            // rationale.
            ("nelisp-stdlib-error.el", include_bytes!("../../../lisp/nelisp-stdlib-error.el.image")),
            ("nelisp-stdlib.el", include_bytes!("../../../lisp/nelisp-stdlib.el.image")),
            ("nelisp-stdlib-list.el", include_bytes!("../../../lisp/nelisp-stdlib-list.el.image")),
            ("nelisp-stdlib-hof.el", include_bytes!("../../../lisp/nelisp-stdlib-hof.el.image")),
            ("nelisp-stdlib-search.el", include_bytes!("../../../lisp/nelisp-stdlib-search.el.image")),
            ("nelisp-stdlib-plist-str.el", include_bytes!("../../../lisp/nelisp-stdlib-plist-str.el.image")),
            // Doc 86 §86.1.e (2026-05-10): Tier 2 simple wrappers
            // (`concat-ints' / `make-mut-string' / `format-float-body').
            // Loads AFTER `stdlib-plist-str.el' (= which references the
            // wrapper bodies in `concat' / `make-string' / `format'
            // dispatchers via late-binding function-cell lookup) and
            // BEFORE `stdlib-misc.el' (= no consumer there).
            ("nelisp-stdlib-format.el", include_bytes!("../../../lisp/nelisp-stdlib-format.el.image")),
            ("nelisp-stdlib-misc.el", include_bytes!("../../../lisp/nelisp-stdlib-misc.el.image")),
            ("nelisp-stdlib-os.el", include_bytes!("../../../lisp/nelisp-stdlib-os.el.image")),
            ("nelisp-pcase.el", include_bytes!("../../../lisp/nelisp-pcase.el.image")),
            ("nelisp-cl-macros.el", include_bytes!("../../../lisp/nelisp-cl-macros.el.image")),
            ("nelisp-stdlib-hash.el", include_bytes!("../../../lisp/nelisp-stdlib-hash.el.image")),
            // Doc 79 v7 Phase C Stage 5.3.a (2026-05-09): Bacon-Rajan
            // cycle collector skeleton.  See `nelisp-baker.rs'
            // `STDLIB_FILES' for the load-order rationale (= depends
            // on `make-hash-table' from `nelisp-stdlib-hash').
            ("nelisp-stdlib-gc.el", include_bytes!("../../../lisp/nelisp-stdlib-gc.el.image")),
            ("nelisp-stdlib-equal.el", include_bytes!("../../../lisp/nelisp-stdlib-equal.el.image")),
            ("nelisp-stdlib-prn.el", include_bytes!("../../../lisp/nelisp-stdlib-prn.el.image")),
            ("nelisp-stdlib-reader.el", include_bytes!("../../../lisp/nelisp-stdlib-reader.el.image")),
            ("nelisp-stdlib-eval-core.el", include_bytes!("../../../lisp/nelisp-stdlib-eval-core.el.image")),
            // Doc 87 §86.1.f (2026-05-10): Tier 2 wrappers — see
            // `nelisp-baker.rs' STDLIB_FILES for the load-order
            // rationale.
            ("nelisp-stdlib-time.el", include_bytes!("../../../lisp/nelisp-stdlib-time.el.image")),
            ("nelisp-stdlib-math.el", include_bytes!("../../../lisp/nelisp-stdlib-math.el.image")),
            ("nelisp-stdlib-regex.el", include_bytes!("../../../lisp/nelisp-stdlib-regex.el.image")),
            // Doc 102 Phase 2.b (2026-05-13): elisp-side env modules.
            // See `nelisp-baker.rs' STDLIB_FILES for the load-order
            // rationale; these load LAST so `(provide)' + record
            // primitives are available.
            ("nelisp-stdlib-fast-hash.el", include_bytes!("../../../lisp/nelisp-stdlib-fast-hash.el.image")),
            ("nelisp-env.el", include_bytes!("../../../lisp/nelisp-env.el.image")),
        ];
        let mut env = Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            // Phase 7 Stage 7.3.d (Doc 67) — Tier 2 special forms now
            // expand via elisp macros at runtime; each `cond' / `or' /
            // `when' / `unless' / `dolist' adds 2-3 frames per call.
            // The Rust prn-* family walks deep cons structures, so 256
            // is too tight (the cl-defun expansion depth alone needs
            // ~280 frames after macros pile up).  We bump to 512 — far
            // below host Emacs's 1600 to keep the Rust call stack
            // bounded under cargo-test default thread stacks.  See
            // `recursion_depth_guard' for the upper-bound test.
            // Phase 7 Stage 7.4.c (Doc 68): bumped from 512 to 1024
            // because the elisp-apply takeover adds ~10-15 eval
            // frames per outermost user-level call (= cond expansion
            // + apply-closure helper + apply-lambda-inner + body
            // eval).  1024 absorbs that overhead with margin while
            // staying within cargo test's default 2MB thread stack
            // (= 4096 was tested and overflowed `recursion_depth_guard'
            // under the test runner; 1024 fits with ~256-byte
            // release-mode frames).  Stage 7.4.c's
            // `delegation_depth' counter ensures the takeover only
            // multiplies depth at the outermost user-call boundary,
            // not on every recursive entry.
            max_recursion: 1024,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            // Phase 7 Stage 7.4.c (Doc 68 §2.7): start with elisp
            // dispatch *off* during bootstrap.  The dispatcher
            // (`nelisp--apply-fn' et al., installed by the LAST
            // STDLIB_SOURCES entry `nelisp-stdlib-eval-core.el')
            // doesn't exist until bootstrap completes; if the flag
            // were on from the start, every defun load along the way
            // would try to delegate through a `void-function:
            // nelisp--apply-fn' panic.  Stage 7.4.e.2 post-bootstrap
            // step then flips it ON by default (= Doc 70).
            use_elisp_apply: false,
            delegation_depth: 0,
        };
        // `nil` and `t` self-evaluate; mark them constant so that
        // (setq nil 1) is a hard error per Elisp.
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        // Doc 86 §86.3.a / Doc 89 Option C — Tier 0 env shim primitives.
        // Registered AFTER `install_builtins' (= so the function-cell
        // sentinel install is the final write) and BEFORE the elisp
        // shim file (`nelisp-stdlib-env-shim.el', loaded inside the
        // STDLIB_IMAGES loop below) so the shim can `funcall' the 11
        // `nelisp--env-globals-*' primitives at load time without a
        // chicken-and-egg gap.  See `eval/env_shim.rs' for the bodies.
        super::env_shim::install_env_shim_primitives(&mut env);
        // Doc 98 §98.3 (2026-05-11): boot path switched from form-shim
        // re-eval (`decode_image' → `reader::read_all' → `eval')
        // to frozen-heap direct decode (`decode_v3_into' = globals
        // streamed straight into `env.globals').  The baker now
        // emits proper node-pool images via `iterative_bake_one'
        // (`nelisp-baker --frozen-heap'), so `FALLBACK_FORMS' is
        // empty and `reader/' is no longer reachable from boot.
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
        // Phase 7 Stage 7.4.e.2 (Doc 70) — default ON post-bootstrap.
        // Stage 7.4.d で発見した frame-capture leak は Stage 7.4.e.1
        // で `nelisp--apply-lambda-inner' を Rust builtin に降ろすこと
        // で解消済 → helper の state slot は lexical env に出ず、
        // `sf_lambda' が user code 評価中の env を snapshot しても
        // dispatcher 内部状態は捕捉されない.
        //
        // escape hatch: `NELISP_USE_RUST_APPLY' env var が非空なら
        // Rust dispatch を維持 (= 旧 Stage 7.3.d behaviour に戻す).
        // 観測未知の dispatch regression が出た場合の bisect 補助.
        let force_rust = std::env::var_os("NELISP_USE_RUST_APPLY")
            .map(|v| !v.is_empty())
            .unwrap_or(false);
        env.use_elisp_apply = !force_rust;
        env
    }

    /// Empty environment with no built-ins — useful for tests that
    /// want to assert error paths in isolation.  Most callers want
    /// [`Env::new_global`].
    pub fn empty() -> Self {
        Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            max_recursion: 256,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            // `Env::empty' is for stand-alone error-path tests that
            // never reach the apply-fn dispatch; default `false' is
            // safe.
            use_elisp_apply: false,
            delegation_depth: 0,
        }
    }
    /// Doc 98 §98.1 — globals-only environment with built-ins +
    /// env_shim primitives installed but the STDLIB_IMAGES decode
    /// loop skipped.  The iterative baker (= `image::iterative_bake_one')
    /// uses this as its accumulating starting point so successive
    /// `iterative_bake_one' calls accumulate global state file-by-file.
    /// `use_elisp_apply' is kept `false' because the elisp dispatcher
    /// (`nelisp--apply-fn') is itself defined inside stdlib (= position
    /// 22 in STDLIB_IMAGES).
    pub fn new_global_no_stdlib() -> Self {
        let mut env = Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            max_recursion: 1024,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            use_elisp_apply: false,
            delegation_depth: 0,
        };
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        super::env_shim::install_env_shim_primitives(&mut env);
        env
    }

    /// Doc 98 §98.2 — return an `Env::empty'-shaped clone whose
    /// `globals' map carries every entry that changed vs. `before':
    /// either a new key (= absent in `before') OR a mutated entry
    /// (= present but `SymbolEntry' no longer equal).  Used by
    /// `image::iterative_bake_one' to encode the per-file diff of
    /// an accumulating baker env.  Equality uses derived
    /// `SymbolEntry: PartialEq', which falls through to
    /// `Sexp: PartialEq' (= structural).  The encoder reads only
    /// `globals'; lighter fields stay defaulted.
    pub fn globals_diff_view(
        &self,
        before: &std::collections::HashMap<String, SymbolEntry>,
    ) -> Env {
        let mut diff = Env::empty();
        for (k, v) in &self.globals {
            let changed = match before.get(k) {
                None => true,
                Some(prev) => prev != v,
            };
            if changed {
                diff.globals.insert(k.clone(), v.clone());
            }
        }
        diff
    }


    /// Register `f' as an externally-supplied builtin under `name'.
    /// After this call, evaluating `(NAME ARG...)` in elisp invokes
    /// `f(args, env)` — the same dispatch path the internal `bi_*'
    /// helpers go through, just with the body lifted out of
    /// `eval/builtins.rs'.
    ///
    /// Used by host crates that need to expose Rust APIs as elisp
    /// callables without forking the upstream interpreter:
    ///
    ///   - `nelisp-emacs-gtk' registers GTK4 GUI ops
    ///     (`nelisp-gtk-grid-put' / `nelisp-gtk-redraw' / …).
    ///   - Future SDL2 / Win32 / native-macOS backends will register
    ///     their own primitives the same way.
    ///
    /// Re-registering an existing name overwrites the previous closure.
    /// As a side effect, the symbol's function-cell is set to the
    /// `(builtin NAME)' sentinel so it's immediately callable from
    /// elisp `(funcall NAME ...)` / `(NAME ...)` forms.
    pub fn register_extern_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError> + 'static,
    {
        self.extern_builtins
            .insert(name.to_string(), Rc::new(f));
        let sentinel = Sexp::list_from(&[
            Sexp::Symbol("builtin".into()),
            Sexp::Symbol(name.into()),
        ]);
        self.set_function(name, sentinel);
    }

    /// Mark `name` as a self-evaluating constant bound to `value`.
    pub fn intern_constant(&mut self, name: &str, value: Sexp) {
        let entry = self
            .globals
            .entry(name.to_string())
            .or_insert_with(SymbolEntry::new);
        entry.value = Some(value);
        entry.constant = true;
    }

    /// Walk the lexical frames inside-out, then fall through to the
    /// global value cell.  Used by symbol evaluation.
    /// Innermost-first lexical frame walk (Doc 102 §3.b helper).
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

    /// `setq`/`set` semantics: mutate the **innermost** lexical frame
    /// that already binds `name`; otherwise update the global value
    /// cell (creating the symbol entry if needed).  Constants are
    /// rejected.  Lexical writes go through the FrameCell so any
    /// closure that captured the same cell sees the new value.
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if matches!(self.globals.get(name), Some(SymbolEntry { constant: true, .. })) {
            return Err(EvalError::SettingConstant(name.to_string()));
        }
        if let Some(cell) = self.find_frame_cell(name) {
            // SAFETY: `value' is owned, no outstanding `&Sexp' borrow
            // into the cell's slot (Phase A.2.1 setcar discipline).
            unsafe { cell.set_value(value.clone()) };
            return Ok(value);
        }
        let entry = self.globals.entry(name.to_string()).or_insert_with(SymbolEntry::new);
        entry.value = Some(value.clone());
        Ok(value)
    }

    /// Look up `name` in the function cell.  No lexical fallback —
    /// Elisp `funcall` always goes through the global function slot.
    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        match self.globals.get(name) {
            Some(SymbolEntry {
                function: Some(f), ..
            }) => Ok(f.clone()),
            _ => Err(EvalError::UnboundFunction(name.to_string())),
        }
    }

    /// `defun` / `defalias` semantics — write to the function cell
    /// without disturbing the value cell.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        let entry = self
            .globals
            .entry(name.to_string())
            .or_insert_with(SymbolEntry::new);
        entry.function = Some(func);
    }

    /// `fmakunbound' semantics — drop the function cell.
    pub fn clear_function(&mut self, name: &str) {
        if let Some(entry) = self.globals.get_mut(name) {
            entry.function = None;
        }
    }

    /// `makunbound' semantics — drop the value cell.  The constant
    /// flag is preserved so re-binding via `defconst' still errors.
    pub fn clear_value(&mut self, name: &str) {
        if let Some(entry) = self.globals.get_mut(name) {
            entry.value = None;
        }
    }

    /// `defvar`/`defconst` semantics — install a value but only if the
    /// symbol is not already bound (per Elisp `defvar` idempotence).
    /// `is_constant=true` is `defconst`.
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let entry = self
            .globals
            .entry(name.to_string())
            .or_insert_with(SymbolEntry::new);
        if entry.value.is_none() {
            entry.value = Some(value);
        }
        if is_constant {
            entry.constant = true;
        }
    }

    /// Push a new (initially empty) lexical frame.  Returns the frame
    /// depth so callers can `pop_frame` symmetrically even if errors
    /// fire mid-body.
    pub fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    /// Pop the innermost lexical frame.  Must be paired with
    /// [`Env::push_frame`].  Silently no-ops on under-pop because the
    /// caller path is expected to be balanced — but we never panic.
    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    /// Bind `name` to `value` in the innermost frame.  Used by `let`,
    /// `let*`, and lambda parameter binding.  If no frame exists, the
    /// binding falls through to the global slot — this is intentional
    /// so top-level `(setq x 1)` works without an outer `(let)`.
    /// Each binding is wrapped in a fresh FrameCell so a closure
    /// capturing this name shares the cell (= setq write-through).
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.to_string(), FrameCell::new(value));
        } else {
            let entry = self
                .globals
                .entry(name.to_string())
                .or_insert_with(SymbolEntry::new);
            entry.value = Some(value);
        }
    }

    /// `boundp` — true iff `name` resolves in any lexical frame or
    /// has a global value cell.
    pub fn is_bound(&self, name: &str) -> bool {
        if self.find_frame_cell(name).is_some() {
            return true;
        }
        matches!(
            self.globals.get(name),
            Some(SymbolEntry { value: Some(_), .. })
        )
    }

    /// `fboundp` — true iff `name` has a global function cell.
    pub fn is_fbound(&self, name: &str) -> bool {
        matches!(
            self.globals.get(name),
            Some(SymbolEntry {
                function: Some(_),
                ..
            })
        )
    }

    /// Capture the current lexical frames as a flat alist so a
    /// `lambda` can keep its closure environment as plain [`Sexp`]
    /// data.  Inner frames take precedence over outer.
    ///
    /// Each captured slot is wrapped in `Sexp::Cell` carrying the
    /// **same** [`NlCellRef`](crate::eval::nlcell::NlCellRef) as the
    /// original frame entry, so `setq' inside the closure mutates the
    /// cell visible at the originating let-binding (= write-through
    /// closures, fixed 2026-05-06; storage migrated to layout-pinned
    /// NlCellRef in Phase A.4, 2026-05-09).
    pub fn capture_lexical(&self) -> Sexp {
        // Single-pass: walk frames innermost-first for first-occurrence
        // dedup (= innermost binding wins); `push_captured' consumes
        // the result order-independently.
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

    /// Push a frame populated from a captured-env alist (the inverse
    /// of [`Env::capture_lexical`]).  When a captured value is wrapped
    /// in `Sexp::Cell`, the same `Rc` is reinstalled so mutation is
    /// shared with the original frame; otherwise the value is wrapped
    /// in a fresh cell (= legacy alist input still works).
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        // Closure write-through: when value is `Sexp::Cell', install the
        // SAME Rc so the closure shares the originating frame's slot.
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
