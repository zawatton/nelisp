//! Symbol table + lexical frame stack.  Two-cell symbol model (value /
//! function); lambdas are `(closure CAPTURED-ENV ARGS BODY...)'.  See
//! Doc 44 §3.3 + §4.

#[cfg(test)]
use std::collections::HashMap;
#[cfg(test)]
use std::rc::Rc;

use crate::image;

use super::error::EvalError;
use super::sexp::Sexp;

/// Host-crate-registered builtin closure.  `Rc' because `Env' is single-threaded.
///
/// Doc 102 Phase 7 (2026-05-17) — gated behind `#[cfg(test)]`.
/// Production binary doesn't ship this surface; `nelisp--env-globals-op'
/// (= the sole former production extern_builtin) is now a regular
/// `builtins::dispatch' match arm (= Doc 102 Phase 6).  Tests retain
/// the extension point for verifying the dispatch fall-through path.
#[cfg(test)]
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

/// Runtime environment: lexical frame stack + recursion guard +
/// extern-builtin registry + elisp-side globals mirror + Stage 7.4.c
/// apply takeover flags.  Doc 102 Phase 2.b Step E removed the legacy
/// `globals: HashMap<String, SymbolEntry>` field — the canonical
/// globals state is the elisp `nelisp-env' record reachable via
/// `globals_record' + `mirror_*' accessors.  Doc 104 Stage 3.e removed
/// the legacy `frames: Vec<HashMap<String, FrameCell>>' field — the
/// canonical lexical scope stack is the elisp `nelisp-lexframe-stack'
/// record reachable via `frames_record' + `frame_*_rust_direct'.
pub struct Env {
    pub max_recursion: u32,
    pub current_recursion: u32,
    /// Doc 102 Phase 7 — extension-point HashMap for tests; production
    /// binary builds without this field (`nelisp--env-globals-op' is a
    /// regular dispatch arm as of Phase 6).
    #[cfg(test)]
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
            max_recursion,
            current_recursion: 0,
            #[cfg(test)]
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
            ("nelisp-lexframe.el", include_bytes!("../../../lisp/nelisp-lexframe.el.image")),
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

    /// Empty env (= no built-ins).  For error-path tests + the
    /// image baker's `decode_v3` round-trip helper.  Doc 102 Phase 7
    /// (2026-05-17) — gated behind `#[cfg(any(test, feature =
    /// "image-baker"))]` so the production binary doesn't carry the
    /// symbol.
    #[cfg(any(test, feature = "image-baker"))]
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

    /// Baker accumulator env — built-ins + env_shim installed but no
    /// STDLIB images decoded.  Used by `image::iterative_bake_one'.
    /// Doc 102 Phase 7 (2026-05-17) — gated behind `#[cfg(any(test,
    /// feature = "image-baker"))]'; production binary doesn't ship
    /// this entry point.
    #[cfg(any(test, feature = "image-baker"))]
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

    // ---- frame_* helpers moved to `eval/env_lexframe.rs' in Doc 102
    // Phase 8 (2026-05-17). ----

    /// Register `f' as an externally-supplied builtin under `name'.
    /// Sets the function cell to `(builtin NAME)' so `(NAME ARG...)'
    /// invokes `f(args, env)'.  Re-registering overwrites.
    ///
    /// Doc 102 Phase 7 (2026-05-17) — gated behind `#[cfg(test)]'.
    /// Production binary's sole former extern_builtin
    /// (=nelisp--env-globals-op=) is now a regular `builtins::dispatch'
    /// match arm (Phase 6).
    #[cfg(test)]
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
        // Special-case the sentinel itself: a `mirror_lookup_value'
        // returning `self.unbound_marker' would otherwise be treated as
        // "absent", which would mark `nelisp--unbound-marker' (= the
        // symbol whose VALUE is the sentinel) as unbound.  Return the
        // sentinel directly so elisp
        // `(eq cell nelisp--unbound-marker)' checks resolve.
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
        let f = self.mirror_lookup_function(name);
        if f == self.unbound_marker {
            Err(EvalError::UnboundFunction(name.to_string()))
        } else {
            Ok(f)
        }
    }

    /// `defun' / `defalias' — write the function cell.  Doc 102
    /// Phase 2.b Step E retired the HashMap dual-write.
    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.mirror_set_function(name, func);
    }

    /// `defvar' / `defconst' — install value only if unbound (Elisp
    /// idempotence); IS_CONSTANT=true marks `defconst'.
    ///
    /// Doc 102 Phase 5 Step A (2026-05-17) — body dispatches to elisp
    /// `nelisp-env-defvar' via `apply_function'.  The elisp helper
    /// performs the idempotence check + constant-flag setting against
    /// the globals mirror.  Pre-bootstrap (= helper not yet loaded),
    /// falls back to the Rust mirror_* helpers so STDLIB decode-time
    /// `defvar's still install correctly.
    pub fn defvar(&mut self, name: &str, value: Sexp, is_constant: bool) {
        let f = match self.lookup_function("nelisp-env-defvar") {
            Ok(f) => f,
            Err(_) => {
                // Bootstrap fallback — same logic as the pre-Phase-5
                // body, used until `nelisp-env.el' loads.
                if !self.mirror_is_bound(name) {
                    self.mirror_set_value(name, value);
                }
                if is_constant {
                    self.mirror_set_constant(name, true);
                }
                return;
            }
        };
        let args = [
            self.globals_record.clone(),
            // Name is passed as `Sexp::Str' because the underlying
            // `nelisp--fast-hash-put' hashes string keys via FNV-1a
            // over the char sequence; symbols would crash on
            // `(length symbol)' inside the elisp hash function.
            Sexp::Str(name.to_string()),
            value,
            if is_constant { Sexp::T } else { Sexp::Nil },
        ];
        let _ = super::apply_function(&f, &args, self);
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
    /// closure `setq' writes through to the let-binding's slot.
    ///
    /// Doc 102 Phase 4.b (2026-05-17) — body dispatches to elisp
    /// `nelisp-lexframe-stack-capture-to-depth' via `apply_function'.
    /// The pre-apply depth snapshot caps the elisp walk at the
    /// caller-time stack depth so apply_lambda_inner's argument
    /// frame (= pushed onto the same record during dispatch) is
    /// excluded from the result.  Pre-bootstrap (= helper not yet
    /// loaded) falls back to a `Sexp::Nil' result (= empty captured
    /// env; correct for the empty initial stack).
    pub fn capture_lexical(&mut self) -> Sexp {
        // Snapshot the current depth BEFORE the dispatch — apply_lambda_inner
        // will push its argument frame at this index during the helper call.
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

    // ---- mirror_lookup_entry / mirror_lookup_* / mirror_is_bound /
    // mirror_is_fbound : moved to `eval/env_mirror.rs' in Doc 102
    // Phase 8 (2026-05-17). ----

    /// Push a frame from a captured-env alist (inverse of `capture_lexical').
    /// When a captured value is `Sexp::Cell', the same `Rc` is reinstalled
    /// (= write-through); plain values are wrapped in a fresh
    /// `Sexp::Cell' so `setq' on the binding still hits a real
    /// `NlCellRef' slot.
    ///
    /// Doc 102 Phase 4.b (2026-05-17) — body dispatches to elisp
    /// `nelisp-lexframe-make-from-alist' via `apply_function' to
    /// build the frame, then pushes the returned frame onto the
    /// mirror stack via Rust-direct (= the apply_lambda_inner
    /// argument-frame contamination is contained inside the helper
    /// call; only the build is delegated, the push stays Rust-side
    /// so it lands at the correct depth).  The `wrap_alist_cells'
    /// normalisation stays Rust-side because elisp can't construct
    /// an `NlCellRef' without a dedicated primitive.
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let normalized = Env::wrap_alist_cells(alist)?;
        let f = match self.lookup_function("nelisp-lexframe-make-from-alist") {
            Ok(f) => f,
            Err(_) => return Ok(()),
        };
        let frame = super::apply_function(&f, &[normalized], self)?;
        // Push the returned frame onto the mirror stack (Rust-direct,
        // so the depth lands at the caller's stack — apply argument
        // frame contamination from the helper call has already been
        // popped by apply_lambda_inner's return path).
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

    // `wrap_alist_cells' moved to `eval/env_lexframe.rs' in Doc 102
    // Phase 8 (2026-05-17).
}

// `mirror_fnv1a' moved to `eval/env_mirror.rs' in Doc 102 Phase 8.

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
            let mirror_side = env.mirror_lookup_function(name);
            assert!(
                mirror_side != env.unbound_marker,
                "mirror missing function `{}'", name,
            );
            assert_eq!(rust_side, mirror_side, "mismatch for `{}'", name);
        }
    }

    #[test]
    fn phase8_session4_rust_direct_lookup_value_returns_t_for_constant_t() {
        // `t' is interned with value Sexp::T + constant flag.  Verify
        // the mirror returns Sexp::T (not the unbound-marker sentinel).
        let env = Env::new_global();
        let v = env.mirror_lookup_value("t");
        assert!(v != env.unbound_marker, "mirror missing value for `t'");
        assert!(matches!(v, Sexp::T));
    }

    #[test]
    fn phase8_session4_rust_direct_is_fbound_matches_rust() {
        let env = Env::new_global();
        // Present in env: `car'.  Absent: invented name.
        assert!(env.mirror_is_fbound("car"));
        assert!(!env.mirror_is_fbound("doc-102-session-4-absent-fn"));
    }

    // `phase8_session4_fnv1a_matches_elisp_hash_loop' moved to
    // `eval/env_mirror.rs::tests::mirror_fnv1a_matches_elisp_hash_loop'
    // in Doc 102 Phase 8.

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
        // capture_lexical walks the elisp lexframe stack (via Phase
        // 4.b apply_function dispatch into
        // `nelisp-lexframe-stack-capture-to-depth') and returns an
        // alist of `(NAME . CELL)' with inner-shadows-outer dedup.
        // The captured-env alist key type is either Sexp::Str or
        // Sexp::Symbol depending on how the elisp helper preserves
        // the stored key (= the fast-hash bucket stores Sexp::Str).
        let mut env = Env::new_global();
        env.push_frame();
        env.bind_local("a", Sexp::Int(1));
        env.bind_local("b", Sexp::Int(2));
        env.push_frame();
        env.bind_local("a", Sexp::Int(3)); // shadows outer
        env.bind_local("c", Sexp::Int(4));

        let alist = env.capture_lexical();
        // Build a {name -> value} map from the alist.
        let mut seen: HashMap<String, Sexp> = HashMap::new();
        let mut cur = &alist;
        while let Sexp::Cons(c) = cur {
            if let Sexp::Cons(pair) = &c.car {
                let key = match &pair.car {
                    Sexp::Str(k) | Sexp::Symbol(k) => Some(k.clone()),
                    _ => None,
                };
                if let Some(k) = key {
                    let v = match &pair.cdr {
                        Sexp::Cell(c) => c.value.clone(),
                        other => other.clone(),
                    };
                    seen.insert(k, v);
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
