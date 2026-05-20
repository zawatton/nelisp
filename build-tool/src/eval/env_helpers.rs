//! Env + globals-mirror / lexframe-stack helpers over elisp records.

use std::collections::HashMap;
use std::rc::Rc;

use super::error::EvalError;
use super::sexp::Sexp;
use crate::eval::nlrecord::NlRecordRef;
use crate::eval::nlvector::NlVectorRef;

pub type ExternBuiltin = Rc<dyn Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>>;

pub type FrameCell = crate::eval::nlcell::NlCellRef;

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

macro_rules! mirror_op {
    (mutate: $name:ident => $extern_fn:ident) => {
        pub(crate) fn $name(&mut self, name: &str, value: Sexp) {
            self.mirror_mutate_with(name, &value, crate::elisp_cc_spike::$extern_fn);
        }
    };
    (clear: $name:ident => $extern_fn:ident) => {
        pub(crate) fn $name(&mut self, name: &str) {
            self.with_mirror_unbound(name, |m, s, u| unsafe {
                crate::elisp_cc_spike::$extern_fn(m, s, u);
            });
        }
    };
    (pred: $name:ident($vis:vis) => $extern_fn:ident) => {
        $vis fn $name(&self, name: &str) -> bool {
            self.with_mirror_unbound(name, |m, s, u| unsafe {
                crate::elisp_cc_spike::$extern_fn(m, s, u) != 0
            }).unwrap_or(false)
        }
    };
    (lookup: $name:ident($vis:vis) => $extern_fn:ident) => {
        $vis fn $name(&self, name: &str) -> Sexp {
            self.with_mirror_symbol(name, |mirror_ptr, sym_ptr| unsafe {
                if crate::elisp_cc_spike::mirror_lookup_entry(mirror_ptr, sym_ptr).is_null() {
                    return self.unbound_marker.clone();
                }
                let mut slot = Sexp::Nil;
                crate::elisp_cc_spike::$extern_fn(mirror_ptr, sym_ptr, &mut slot);
                slot
            }).unwrap_or_else(|| self.unbound_marker.clone())
        }
    };
}

impl Env {
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

    pub fn new_global() -> Self {
        macro_rules! e {
            ($n:literal) => {
                ($n, include_str!(concat!("../../../lisp/", $n)))
            };
        }
        const STDLIB_FILES: &[(&str, &str)] = &[
            e!("nelisp-jit-substrate.el"),
            e!("nelisp-syscall-table.el"),
            e!("nelisp-jit-strategy.el"),
            e!("nelisp-stdlib-env-shim.el"),
            e!("nelisp-stdlib-eval-special.el"),
            e!("nelisp-stdlib-error.el"),
            e!("nelisp-stdlib.el"),
            e!("nelisp-stdlib-list.el"),
            e!("nelisp-stdlib-hof.el"),
            e!("nelisp-stdlib-search.el"),
            e!("nelisp-stdlib-plist-str.el"),
            e!("nelisp-stdlib-format.el"),
            e!("nelisp-stdlib-misc.el"),
            e!("nelisp-stdlib-os-int-helpers.el"),
            e!("nelisp-stdlib-os.el"),
            e!("nelisp-pcase.el"),
            e!("nelisp-cl-macros.el"),
            e!("nelisp-stdlib-hash.el"),
            e!("nelisp-stdlib-equal.el"),
            e!("nelisp-stdlib-prn.el"),
            e!("nelisp-stdlib-reader.el"),
            e!("nelisp-stdlib-eval-core.el"),
            e!("nelisp-stdlib-math.el"),
            e!("nelisp-stdlib-regex.el"),
            e!("nelisp-stdlib-fast-hash.el"),
            e!("nelisp-env.el"),
            e!("nelisp-lexframe.el"),
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
                        name,
                        idx,
                        e,
                        crate::eval::sexp::fmt_sexp(form),
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

    pub fn empty() -> Self {
        Env::fresh(256)
    }

    pub fn new_global_no_stdlib() -> Self {
        Env::install_stage0(1024)
    }

    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion);
        env.install_empty_mirror_rust_direct();
        {
            let unbound = env.unbound_marker.clone();
            let plist_slot = Sexp::Nil;
            let constant_slot = Sexp::T;
            env.with_mirror_symbol("nil", |mirror_ptr, sym_ptr| unsafe {
                crate::elisp_cc_spike::mirror_install_entry_or_insert(
                    mirror_ptr, sym_ptr, &Sexp::Nil, &unbound, &plist_slot, &constant_slot,
                );
            });
            let unbound2 = env.unbound_marker.clone();
            env.with_mirror_symbol("t", |mirror_ptr, sym_ptr| unsafe {
                crate::elisp_cc_spike::mirror_install_entry_or_insert(
                    mirror_ptr, sym_ptr, &Sexp::T, &unbound2, &plist_slot, &constant_slot,
                );
            });
        }
        super::builtins::install_builtins(&mut env);
        env.register_extern_builtin("nelisp--env-globals-op", |args, env| {
            super::env_shim::bi_globals_op(args, env)
        });
        env
    }

    pub fn register_extern_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError> + 'static,
    {
        self.extern_builtins.insert(name.to_string(), Rc::new(f));
        let sentinel =
            Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol(name.into())]);
        self.mirror_set_function(name, sentinel);
    }

    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return if name == "nelisp--unbound-marker" { Ok(self.unbound_marker.clone()) }
                   else { Err(EvalError::unbound_var(name)) };
        }
        let n = Sexp::Symbol(name.into()); let mut o = Sexp::Nil;
        let rc = unsafe { crate::elisp_cc_spike::env_lookup_value(&self.globals_record, &self.frames_record, &n, &mut o) };
        if rc == 0 { Ok(o) } else { Err(EvalError::unbound_var(name)) }
    }

    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return Err(EvalError::unbound_var(name)); }
        let n = Sexp::Symbol(name.into());
        let sc = crate::elisp_cc_spike::build_or_insert_scratch_vec(value.clone(), self.unbound_marker.clone(), Sexp::Nil, Sexp::Nil);
        let rc = unsafe { crate::elisp_cc_spike::env_set_value(&self.globals_record, &self.frames_record, &n, &value, &sc, 0) };
        if rc == 0 { Ok(value) } else { Err(EvalError::setting_constant(name)) }
    }

    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return Err(EvalError::unbound_fn(name)); }
        let n = Sexp::Symbol(name.into()); let mut o = Sexp::Nil;
        let rc = unsafe { crate::elisp_cc_spike::env_lookup_function(&self.globals_record, &self.unbound_marker, &n, &mut o) };
        if rc == 0 { Ok(o) } else { Err(EvalError::unbound_fn(name)) }
    }



    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        // Wave b: delegate to Phase 47 .o.  Guard: mirror not ready → no-op.
        if !matches!(&self.globals_record, Sexp::Record(_)) { return; }
        let n = Sexp::Symbol(name.into());
        let sc = crate::elisp_cc_spike::build_or_insert_scratch_vec(
            value.clone(), self.unbound_marker.clone(), Sexp::Nil, Sexp::Nil);
        unsafe { crate::elisp_cc_spike::env_bind_local(
            &self.globals_record, &self.frames_record, &n, &value, &sc, 0) };
    }

    /// Push a fresh empty lexframe onto the frame stack.
    pub fn push_frame(&mut self) {
        self.frame_push_rust_direct();
    }

    /// Pop the top lexframe from the frame stack.
    pub fn pop_frame(&mut self) {
        self.frame_pop_rust_direct();
    }

    /// Set (or insert) the function cell for NAME in the globals mirror.
    pub fn set_function(&mut self, name: &str, value: Sexp) {
        self.mirror_set_function(name, value);
    }

    pub fn capture_lexical(&mut self) -> Sexp {
        // Wave g: compressed with let..else; same logic, -7 LOC.
        let Sexp::Record(r) = &self.frames_record else { return Sexp::Nil };
        let Some(Sexp::Int(depth)) = r.slots.get(1) else { return Sexp::Nil };
        let depth = *depth;
        let Ok(f) = self.lookup_function("nelisp-lexframe-stack-capture-to-depth") else { return Sexp::Nil };
        let args = [self.frames_record.clone(), Sexp::Int(depth)];
        super::apply_function(&f, &args, self).unwrap_or(Sexp::Nil)
    }

    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        // Wave i: delegate install+depth-bump to Phase 47 .o.
        let normalized = Env::wrap_alist_cells(alist)?;
        let Ok(f) = self.lookup_function("nelisp-lexframe-make-from-alist") else { return Ok(()) };
        let frame = super::apply_function(&f, &[normalized], self)?;
        unsafe { crate::elisp_cc_spike::frame_stack_install(&self.frames_record, &frame); }
        Ok(())
    }
}

impl Env {
    fn with_mirror_symbol<T>(
        &self,
        name: &str,
        f: impl FnOnce(*const Sexp, *const Sexp) -> T,
    ) -> Option<T> {
        if !matches!(&self.globals_record, Sexp::Record(_)) {
            return None;
        }
        let sym = Sexp::Symbol(name.into());
        Some(f(&self.globals_record, &sym))
    }

    fn with_mirror_unbound<T>(
        &self,
        name: &str,
        f: impl FnOnce(*const Sexp, *const Sexp, *const Sexp) -> T,
    ) -> Option<T> {
        self.with_mirror_symbol(name, |mirror_ptr, sym_ptr| {
            f(mirror_ptr, sym_ptr, &self.unbound_marker)
        })
    }

    fn mirror_mutate_with(
        &mut self,
        name: &str,
        payload: &Sexp,
        op: unsafe fn(*const Sexp, *const Sexp, *const Sexp, *const Sexp) -> i64,
    ) {
        self.with_mirror_unbound(name, |m, s, u| unsafe {
            op(m, s, payload, u);
        });
    }

    mirror_op!(mutate: mirror_set_value => mirror_set_value_or_insert);
    mirror_op!(mutate: mirror_set_function => mirror_set_function_or_insert);

    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        self.mirror_mutate_with(
            name,
            &value,
            crate::elisp_cc_spike::mirror_set_constant_or_insert,
        );
    }

    pub fn install_empty_mirror_rust_direct(&mut self) {
        self.unbound_marker = Sexp::Symbol("nelisp--unbound-marker".into());
        unsafe {
            crate::elisp_cc_spike::env_install_empty_globals_frames(
                &mut self.globals_record, &mut self.frames_record);
        }
    }

    mirror_op!(lookup: mirror_lookup_value(pub) => mirror_lookup_value);
    mirror_op!(lookup: mirror_lookup_function(pub) => mirror_lookup_function);

    mirror_op!(pred: mirror_is_fbound(pub) => mirror_is_fbound);

    pub(crate) fn frame_stack_view(&self) -> Option<(NlRecordRef, NlVectorRef, usize)> {
        let stack_rec = match &self.frames_record {
            Sexp::Record(r) => r.clone(),
            _ => return None,
        };
        let backing = match stack_rec.slots.get(0)? {
            Sexp::Vector(v) => v.clone(),
            _ => return None,
        };
        let depth = match stack_rec.slots.get(1)? {
            Sexp::Int(n) => *n as usize,
            _ => return None,
        };
        Some((stack_rec, backing, depth))
    }

    pub(crate) fn frame_push_rust_direct(&mut self) -> Option<Sexp> {
        // Wave f: delegate to nelisp_frame_push .o.
        if !matches!(&self.frames_record, Sexp::Record(_)) {
            return None;
        }
        unsafe { crate::elisp_cc_spike::frame_push(&self.frames_record as *const Sexp) };
        Some(Sexp::Nil) // return value is always discarded by callers
    }

    pub(crate) fn frame_pop_rust_direct(&mut self) {
        let Some((stack_rec, backing, depth)) = self.frame_stack_view() else {
            return;
        };
        if depth == 0 {
            return;
        }
        unsafe {
            backing.with_value_mut(|v| v[depth - 1] = Sexp::Nil);
            stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth - 1) as i64));
        }
    }

    pub(crate) fn frame_bind_rust_direct(&mut self, name: &str, cell: Sexp) {
        // Wave f: delegate to nelisp_frame_bind .o.
        if !matches!(&self.frames_record, Sexp::Record(_)) {
            return;
        }
        let name_sexp = Sexp::Str(name.to_string());
        unsafe { crate::elisp_cc_spike::frame_bind(&self.frames_record, &name_sexp, &cell) };
    }

    pub fn frame_lookup_rust_direct(&self, name: &str) -> Option<Sexp> {
        // Wave f: delegate to nelisp_frame_stack_find .o (innermost frame).
        // Build a temporary 1-depth frames record pointing at only the top frame.
        if !matches!(&self.frames_record, Sexp::Record(_)) {
            return None;
        }
        let name_sexp = Sexp::Str(name.to_string());
        let raw = unsafe { crate::elisp_cc_spike::frame_stack_find_raw(&self.frames_record, &name_sexp) };
        if raw.is_null() {
            return None;
        }
        // raw points at the CDR slot of the (NAME . CELL) pair — i.e. the cell Sexp.
        Some(unsafe { (*raw).clone() })
    }

    pub fn frame_stack_find_rust_direct(&self, name: &str) -> Option<Sexp> {
        // Wave f: delegate to nelisp_frame_stack_find .o (full stack walk).
        if !matches!(&self.frames_record, Sexp::Record(_)) {
            return None;
        }
        let name_sexp = Sexp::Str(name.to_string());
        let raw = unsafe { crate::elisp_cc_spike::frame_stack_find_raw(&self.frames_record, &name_sexp) };
        if raw.is_null() {
            return None;
        }
        Some(unsafe { (*raw).clone() })
    }

    pub(crate) fn wrap_alist_cells(alist: &Sexp) -> Result<Sexp, EvalError> {
        let mut result = Sexp::Nil;
        let rc = unsafe {
            crate::elisp_cc_spike::wrap_alist_cells(alist as *const Sexp, &mut result)
        };
        if rc == 1 {
            Ok(result)
        } else {
            Err(EvalError::internal("wrap_alist_cells: malformed closure env alist"))
        }
    }
}
