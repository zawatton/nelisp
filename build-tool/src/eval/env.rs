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

use std::collections::{HashMap, HashSet};

use crate::reader;

use super::error::EvalError;
use super::sexp::Sexp;

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
pub type Frame = HashMap<String, Sexp>;

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
    /// Global feature registry used by `provide` / `require` /
    /// `featurep`.  Phase 8.0.2 keeps this in-memory only; file-based
    /// loading is deferred to the bridge in Phase 8.0.3.
    pub features: HashSet<String>,
}

impl Env {
    /// Construct a globals-only environment with all built-ins
    /// installed.  Equivalent to GNU Emacs' empty-buffer top-level.
    pub fn new_global() -> Self {
        const STDLIB_SOURCES: &[(&str, &str)] = &[
            ("nelisp-stdlib.el", include_str!("../../../lisp/nelisp-stdlib.el")),
            ("nelisp-stdlib-list.el", include_str!("../../../lisp/nelisp-stdlib-list.el")),
            ("nelisp-stdlib-hof.el", include_str!("../../../lisp/nelisp-stdlib-hof.el")),
            ("nelisp-stdlib-search.el", include_str!("../../../lisp/nelisp-stdlib-search.el")),
            ("nelisp-stdlib-plist-str.el", include_str!("../../../lisp/nelisp-stdlib-plist-str.el")),
            ("nelisp-stdlib-misc.el", include_str!("../../../lisp/nelisp-stdlib-misc.el")),
        ];
        let mut env = Env {
            globals: HashMap::new(),
            frames: Vec::new(),
            max_recursion: 256,
            current_recursion: 0,
            features: HashSet::new(),
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
            features: HashSet::new(),
        }
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
            if let Some(v) = frame.get(name) {
                return Ok(v.clone());
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
    /// rejected.
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if let Some(entry) = self.globals.get(name) {
            if entry.constant {
                return Err(EvalError::SettingConstant(name.to_string()));
            }
        }
        for frame in self.frames.iter_mut().rev() {
            if frame.contains_key(name) {
                frame.insert(name.to_string(), value.clone());
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
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.to_string(), value);
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

    /// Register a provided/required feature in the global feature
    /// table.
    pub fn provide_feature(&mut self, name: &str) {
        self.features.insert(name.to_string());
    }

    /// `featurep` — true iff the feature has been recorded globally.
    pub fn has_feature(&self, name: &str) -> bool {
        self.features.contains(name)
    }

    /// Capture the current lexical frames as a flat alist so a
    /// `lambda` can keep its closure environment as plain [`Sexp`]
    /// data.  Inner frames take precedence over outer.
    pub fn capture_lexical(&self) -> Sexp {
        let mut seen = std::collections::HashSet::new();
        let mut entries: Vec<(String, Sexp)> = Vec::new();
        for frame in self.frames.iter().rev() {
            for (k, v) in frame {
                if seen.insert(k.clone()) {
                    entries.push((k.clone(), v.clone()));
                }
            }
        }
        let mut acc = Sexp::Nil;
        for (k, v) in entries.into_iter().rev() {
            let pair = Sexp::cons(Sexp::Symbol(k), v);
            acc = Sexp::cons(pair, acc);
        }
        acc
    }

    /// Push a frame populated from a captured-env alist (the inverse
    /// of [`Env::capture_lexical`]).
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
                            frame.insert(s.clone(), value.borrow().clone());
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
