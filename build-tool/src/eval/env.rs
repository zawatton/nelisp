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

use crate::reader;

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
#[derive(Debug, Clone, Default)]
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
pub type FrameCell = std::rc::Rc<std::cell::RefCell<Sexp>>;
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
    /// Phase 7 Stage 7.4.c (Doc 68 §2.7) — takeover flag for the
    /// elisp-side `nelisp--apply-fn' dispatch.  When `true',
    /// `eval/mod.rs::apply_combiner' delegates the plain-function and
    /// lambda-head paths to elisp instead of invoking the Rust
    /// `apply_function' helpers directly.  Stage 7.4.e (Doc 70):
    /// default is `true' once bootstrap completes (= elisp dispatch
    /// is the runtime path).  Set the `NELISP_USE_RUST_APPLY' env var
    /// to a non-empty value to keep Rust dispatch as an escape hatch.
    /// Runtime-mutable via the `nelisp--set-use-elisp-apply' builtin
    /// so ERT can flip it inside a single subprocess.
    pub use_elisp_apply: bool,
    /// Phase 7 Stage 7.4.c (Doc 68 §2.7) — re-entry guard for the
    /// elisp-apply takeover.  Counts active `delegate_to_elisp_apply'
    /// frames; when > 0 the apply_combiner skips its delegate gate so
    /// that the dispatcher's own machinery (= helpers, predicates,
    /// macro expansion phases, the elisp defuns they internally
    /// invoke like `consp' / `null' / `nth') runs through the Rust
    /// `apply_function' path without recursing back through itself.
    /// Trade-off: only the *outermost* user-level call is exercised
    /// through the elisp dispatcher.  Cross-equivalence ERT verifies
    /// that one entry through the elisp path computes the same final
    /// value as Rust dispatch would; deep / recursive coverage is
    /// Stage 7.4.d's default-flip job (= every user-entry-point call
    /// flows through elisp by default).
    pub delegation_depth: u32,
}

impl Env {
    /// Construct a globals-only environment with all built-ins
    /// installed.  Equivalent to GNU Emacs' empty-buffer top-level.
    pub fn new_global() -> Self {
        const STDLIB_SOURCES: &[(&str, &str)] = &[
            // Phase 7 Stage 7.3.a (2026-05-07, Doc 67): Tier 2 special
            // forms as elisp macros.  Layer A — loaded BEFORE
            // `nelisp-stdlib.el' so its `defmacro' bodies don't depend
            // on any other elisp helper (Tier 1-only macro body
            // constraint, Doc 67 §2.2).  Stage 7.3.a is parallel
            // install only — `apply_special' match arms still preempt
            // these macros at runtime; ERT exercises them via
            // `macroexpand-1'.  Stage 7.3.d will retire the Rust arms
            // and the elisp macros activate.
            ("nelisp-stdlib-eval-special.el", include_str!("../../../lisp/nelisp-stdlib-eval-special.el")),
            ("nelisp-stdlib.el", include_str!("../../../lisp/nelisp-stdlib.el")),
            ("nelisp-stdlib-list.el", include_str!("../../../lisp/nelisp-stdlib-list.el")),
            ("nelisp-stdlib-hof.el", include_str!("../../../lisp/nelisp-stdlib-hof.el")),
            ("nelisp-stdlib-search.el", include_str!("../../../lisp/nelisp-stdlib-search.el")),
            ("nelisp-stdlib-plist-str.el", include_str!("../../../lisp/nelisp-stdlib-plist-str.el")),
            ("nelisp-stdlib-misc.el", include_str!("../../../lisp/nelisp-stdlib-misc.el")),
            // Doc 53 Phase 1 (2026-05-07) — POSIX OS surface (Minimal-5).
            // Loaded after `nelisp-stdlib-misc.el' so the platform-detect
            // `defconst' can call `nelisp--syscall-supported-p' (always
            // available; the rest of the family Errs on non-Linux but the
            // detect itself is safe).  No upstream stdlib file requires
            // this module — `(require 'nelisp-stdlib-os)' from caller code.
            ("nelisp-stdlib-os.el", include_str!("../../../lisp/nelisp-stdlib-os.el")),
            // Rust-min migration (2026-05-06): pcase moved out of
            // special_forms.rs into elisp; loaded here so it's
            // available before any subsequent elisp file uses it.
            ("nelisp-pcase.el", include_str!("../../../lisp/nelisp-pcase.el")),
            // Rust-min migration (2026-05-06 #2): cl-loop / cl-block /
            // cl-return-from / cl-return as elisp.  Previously each
            // consumer (= nelisp-emacs / nelisp-cc) shipped its own
            // minimal stub; now NeLisp stdlib carries the richer
            // implementation so all consumers share a single source.
            ("nelisp-cl-macros.el", include_str!("../../../lisp/nelisp-cl-macros.el")),
            // Doc 50 stage 4f (2026-05-07): hash-table re-implemented
            // in elisp on top of Stage 4c record primitives.  Loaded
            // AFTER cl-macros so a future setf/gv path can land here
            // without further reorder.  The elisp defuns override the
            // prior Rust dispatch arms for make-hash-table / puthash /
            // gethash / remhash / clrhash / hash-table-p /
            // nelisp--hash-pairs (= function-cell override at load).
            ("nelisp-stdlib-hash.el", include_str!("../../../lisp/nelisp-stdlib-hash.el")),
            // Doc 50 stage 5b (2026-05-07): cycle-safe `equal' in elisp
            // built on `nelisp--ref-eq' + visited hash-table.  Loaded
            // AFTER nelisp-stdlib-hash.el because it allocates the
            // visited table via `make-hash-table'.  Function-cell
            // override shadows the prior Rust `bi_equal' arm.
            ("nelisp-stdlib-equal.el", include_str!("../../../lisp/nelisp-stdlib-equal.el")),
            // Phase 7 Stage 7.1.2 (2026-05-07, Doc 64): elisp Sexp
            // printer / serializer.  Loaded AFTER all type-providing
            // stdlib modules (record / hash / equal) so the cond-based
            // dispatcher in `nelisp--prn-to-string' has the full type
            // landscape available.  Overrides the `prin1-to-string'
            // function-cell installed by `bi_prin1_to_string' (= Stage
            // 7.1.4 also removes that Rust arm in the same commit).
            ("nelisp-stdlib-prn.el", include_str!("../../../lisp/nelisp-stdlib-prn.el")),
            // Phase 7 Stage 7.2.a (2026-05-07, Doc 66): elisp Sexp
            // tokenizer (= lexer).  Parallel impl to the Rust reader
            // (build-tool/src/reader/lexer.rs); the Rust reader still
            // drives every read-from-string call, the elisp tokenizer
            // is exercised through ERT only.  Stage 7.2.b will add the
            // parser + the read-from-string takeover hook.
            ("nelisp-stdlib-reader.el", include_str!("../../../lisp/nelisp-stdlib-reader.el")),
            // Phase 7 Stage 7.4.b (2026-05-08, Doc 68): elisp apply /
            // call / closure / env helpers.  Loaded LAST so all
            // upstream stdlib modules (= cadr/cddr/nth/nthcdr from
            // -list, memq from -search, the Stage 7.3 special-form
            // macros) are available in the body.  Stage 7.4.b is
            // parallel install only — Rust apply_function preempts
            // these defuns at runtime; ERT exercises them via direct
            // (nelisp--apply-fn ...) calls.  Stage 7.4.c adds the
            // --use-elisp-apply takeover hook.
            ("nelisp-stdlib-eval-core.el", include_str!("../../../lisp/nelisp-stdlib-eval-core.el")),
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
        for (name, src) in STDLIB_SOURCES {
            let forms = match reader::read_all(src) {
                Ok(forms) => forms,
                Err(e) => panic!("{} bootstrap failed: {}", name, e),
            };
            for form in &forms {
                if let Err(e) = super::eval(form, &mut env) {
                    panic!("{} bootstrap failed: {}", name, e);
                }
            }
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
    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        for frame in self.frames.iter().rev() {
            if let Some(cell) = frame.get(name) {
                return Ok(cell.borrow().clone());
            }
        }
        match self.globals.get(name) {
            Some(SymbolEntry {
                value: Some(v), ..
            }) => Ok(v.clone()),
            _ => Err(EvalError::UnboundVariable(name.to_string())),
        }
    }

    /// `setq`/`set` semantics: mutate the **innermost** lexical frame
    /// that already binds `name`; otherwise update the global value
    /// cell (creating the symbol entry if needed).  Constants are
    /// rejected.  Lexical writes go through the FrameCell so any
    /// closure that captured the same cell sees the new value.
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if let Some(entry) = self.globals.get(name) {
            if entry.constant {
                return Err(EvalError::SettingConstant(name.to_string()));
            }
        }
        for frame in self.frames.iter().rev() {
            if let Some(cell) = frame.get(name) {
                *cell.borrow_mut() = value.clone();
                return Ok(value);
            }
        }
        let entry = self
            .globals
            .entry(name.to_string())
            .or_insert_with(SymbolEntry::new);
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
            frame.insert(
                name.to_string(),
                std::rc::Rc::new(std::cell::RefCell::new(value)),
            );
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
        for frame in self.frames.iter().rev() {
            if frame.contains_key(name) {
                return true;
            }
        }
        matches!(
            self.globals.get(name),
            Some(SymbolEntry {
                value: Some(_),
                ..
            })
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
    /// **same** `Rc<RefCell<Sexp>>` as the original frame entry, so
    /// `setq' inside the closure mutates the cell visible at the
    /// originating let-binding (= write-through closures, fixed
    /// 2026-05-06).
    pub fn capture_lexical(&self) -> Sexp {
        let mut seen = std::collections::HashSet::new();
        let mut entries: Vec<(String, FrameCell)> = Vec::new();
        for frame in self.frames.iter().rev() {
            for (k, cell) in frame {
                if seen.insert(k.clone()) {
                    entries.push((k.clone(), cell.clone()));
                }
            }
        }
        let mut acc = Sexp::Nil;
        for (k, cell) in entries.into_iter().rev() {
            let pair = Sexp::cons(Sexp::Symbol(k), Sexp::Cell(cell));
            acc = Sexp::cons(pair, acc);
        }
        acc
    }

    /// Push a frame populated from a captured-env alist (the inverse
    /// of [`Env::capture_lexical`]).  When a captured value is wrapped
    /// in `Sexp::Cell`, the same `Rc` is reinstalled so mutation is
    /// shared with the original frame; otherwise the value is wrapped
    /// in a fresh cell (= legacy alist input still works).
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let mut frame: Frame = HashMap::new();
        let mut cur: Sexp = alist.clone();
        loop {
            let next = match &cur {
                Sexp::Nil => break,
                Sexp::Cons(car, cdr) => {
                    let car_inner = car.borrow().clone();
                    if let Sexp::Cons(name, value) = &car_inner {
                        if let Sexp::Symbol(s) = &*name.borrow() {
                            // Closure write-through: when value is a
                            // `Sexp::Cell`, install the SAME Rc so the
                            // closure shares the originating frame's
                            // slot; otherwise wrap in a fresh cell.
                            let value_inner = value.borrow().clone();
                            let cell = match value_inner {
                                Sexp::Cell(rc) => rc,
                                v => std::rc::Rc::new(std::cell::RefCell::new(v)),
                            };
                            frame.insert(s.clone(), cell);
                        } else {
                            return Err(EvalError::Internal(
                                "closure env entry name not a symbol".into(),
                            ));
                        }
                    } else {
                        return Err(EvalError::Internal(
                            "closure env entry not a cons".into(),
                        ));
                    }
                    cdr.borrow().clone()
                }
                _ => {
                    return Err(EvalError::Internal(
                        "closure env not a proper list".into(),
                    ))
                }
            };
            cur = next;
        }
        self.frames.push(frame);
        Ok(())
    }
}
