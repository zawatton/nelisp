//! Symbol table (= `HashMap<String, SymbolEntry>`) + lexical frame stack
//! (= `Vec<HashMap<String, FrameCell>>`).  Two-cell symbol model (value /
//! function); lambdas are `(closure CAPTURED-ENV ARGS BODY...)' with the
//! captured-env as a `((name . cell) ...)' alist.  See Doc 44 §3.3 + §4.

use std::collections::HashMap;
use std::rc::Rc;

use crate::image;

use super::error::EvalError;
use super::sexp::Sexp;

/// Host-crate-registered builtin closure (= same shape as internal
/// `bi_*' helpers).  `Rc' because `Env' is single-threaded.
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

impl SymbolEntry {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Write-through cell — `setq' through the captured copy mutates the
/// originating let-binding's slot (= layout-pinned NlCellRef).
pub type FrameCell = crate::eval::nlcell::NlCellRef;
pub type Frame = HashMap<String, FrameCell>;

/// Runtime environment: globals + lexical frame stack + recursion
/// guard + host-crate extern-builtin registry + Stage 7.4.c apply
/// takeover flags.
pub struct Env {
    pub globals: HashMap<String, SymbolEntry>,
    /// Innermost frame is last.
    pub frames: Vec<Frame>,
    pub max_recursion: u32,
    pub current_recursion: u32,
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    /// Stage 7.4.c takeover flag — `true' routes apply_combiner's
    /// plain-fn / lambda-head paths through elisp `nelisp--apply-fn'
    /// (= default post-bootstrap; flip via `NELISP_USE_RUST_APPLY').
    pub use_elisp_apply: bool,
    /// Re-entry guard for the elisp-apply takeover.
    pub delegation_depth: u32,
}

impl Env {
    /// Construct a globals-only environment with all built-ins
    /// installed.  Equivalent to GNU Emacs' empty-buffer top-level.
    pub fn new_global() -> Self {
        // STDLIB_IMAGES: frozen-heap `.image' files baked by
        // `nelisp-baker' from `lisp/*.el'.  Load-order rationale
        // (= which file depends on which) lives in `nelisp-baker.rs'
        // STDLIB_FILES + each `.el' file header.  `decode_v3_into'
        // streams globals straight into `env.globals' — no eval, no
        // reader (Doc 98 §98.3).
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
        let mut env = Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            // 1024 = elisp-apply takeover (~10-15 eval frames per
            // outermost user-level call) + macro expansion overhead;
            // bounded under cargo test's 2MB thread stack.  See
            // `recursion_depth_guard'.
            max_recursion: 1024,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            // Elisp dispatch starts OFF during bootstrap (= the
            // dispatcher in `nelisp-stdlib-eval-core.el' isn't loaded
            // yet); flipped ON post-bootstrap below.
            use_elisp_apply: false,
            delegation_depth: 0,
        };
        // `nil` and `t` self-evaluate; mark them constant so that
        // (setq nil 1) is a hard error per Elisp.
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
        super::builtins::install_builtins(&mut env);
        // env shim primitives installed AFTER builtins, BEFORE the
        // stdlib loop, so the shim file can `funcall' them at load.
        super::env_shim::install_env_shim_primitives(&mut env);
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
        // Elisp dispatch ON post-bootstrap; escape hatch
        // `NELISP_USE_RUST_APPLY' env var keeps Rust dispatch.
        env.use_elisp_apply = std::env::var_os("NELISP_USE_RUST_APPLY")
            .map(|v| v.is_empty())
            .unwrap_or(true);
        env
    }

    /// Empty env (= no built-ins).  For error-path tests; most callers
    /// want [`Env::new_global`].
    pub fn empty() -> Self {
        Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            max_recursion: 256,
            current_recursion: 0,
            extern_builtins: HashMap::new(),
            use_elisp_apply: false,
            delegation_depth: 0,
        }
    }
    /// Baker accumulator env — built-ins + env_shim installed but
    /// STDLIB_IMAGES decode loop skipped.  Used by
    /// `image::iterative_bake_one' to accumulate per-file state.
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

    /// Per-file globals diff vs. `before' (= new key OR mutated
    /// entry).  Used by `image::iterative_bake_one'.
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
    /// Sets the symbol's function-cell to the `(builtin NAME)'
    /// sentinel so elisp `(NAME ARG...)' invokes `f(args, env)'.
    /// Re-registering overwrites the previous closure.
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

    /// `setq' / `set' — mutate innermost frame slot, else update
    /// globals; constants rejected; lexical writes go through the
    /// FrameCell so captured closures see the new value.
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

    /// `defun' / `defalias' — write the function cell.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.globals
            .entry(name.to_string())
            .or_insert_with(SymbolEntry::new)
            .function = Some(func);
    }

    /// `fmakunbound' — drop the function cell.
    pub fn clear_function(&mut self, name: &str) {
        if let Some(e) = self.globals.get_mut(name) {
            e.function = None;
        }
    }

    /// `makunbound' — drop the value cell; constant flag preserved.
    pub fn clear_value(&mut self, name: &str) {
        if let Some(e) = self.globals.get_mut(name) {
            e.value = None;
        }
    }

    /// `defvar' / `defconst' — install value only if unbound (Elisp
    /// idempotence); IS_CONSTANT=true marks `defconst'.
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let entry = self.globals.entry(name.to_string()).or_insert_with(SymbolEntry::new);
        if entry.value.is_none() {
            entry.value = Some(value);
        }
        if is_constant {
            entry.constant = true;
        }
    }

    pub fn push_frame(&mut self) {
        self.frames.push(HashMap::new());
    }

    /// Silently no-ops on under-pop (= balanced caller path).
    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    /// `let' / `let*' / lambda formals — bind NAME→VALUE in the
    /// innermost frame; if no frame, fall through to the global slot
    /// so top-level `(setq x 1)' works.  Each binding wraps a fresh
    /// FrameCell so closures capturing NAME share the cell.
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.to_string(), FrameCell::new(value));
        } else {
            self.globals
                .entry(name.to_string())
                .or_insert_with(SymbolEntry::new)
                .value = Some(value);
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
