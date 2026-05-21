use std::collections::HashMap;
use std::rc::Rc;
use super::error::EvalError;
use super::sexp::Sexp;
pub type ExternBuiltin = Rc<dyn Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError>>;
pub type FrameCell = crate::eval::nlcell::NlCellRef;
pub struct Env {
    pub max_recursion: u32, pub current_recursion: u32,
    pub extern_builtins: HashMap<String, ExternBuiltin>,
    pub use_elisp_apply: bool, pub delegation_depth: u32,
    pub globals_record: Sexp, pub unbound_marker: Sexp, pub frames_record: Sexp,
}
macro_rules! mirror_op {
    (mutate: $name:ident => $extern_fn:ident) => {
        pub(crate) fn $name(&mut self, name: &str, value: Sexp) { self.mirror_mutate_with(name, &value, crate::elisp_cc_spike::$extern_fn); }
    };
    (clear: $name:ident => $extern_fn:ident) => {
        pub(crate) fn $name(&mut self, name: &str) { self.with_mirror_unbound(name, |m,s,u| unsafe { crate::elisp_cc_spike::$extern_fn(m,s,u); }); }
    };
    (pred: $name:ident($vis:vis) => $extern_fn:ident) => {
        $vis fn $name(&self, name: &str) -> bool { self.with_mirror_unbound(name, |m,s,u| unsafe { crate::elisp_cc_spike::$extern_fn(m,s,u)!=0 }).unwrap_or(false) }
    };
    (lookup: $name:ident($vis:vis) => $extern_fn:ident) => {
        $vis fn $name(&self, name: &str) -> Sexp {
            self.with_mirror_symbol(name, |m,s| unsafe {
                if crate::elisp_cc_spike::mirror_lookup_entry(m,s).is_null() { return self.unbound_marker.clone(); }
                let mut slot=Sexp::Nil; crate::elisp_cc_spike::$extern_fn(m,s,&mut slot); slot
            }).unwrap_or_else(|| self.unbound_marker.clone())
        }
    };
}
impl Env {
    fn fresh(max_recursion: u32) -> Self { Env { max_recursion, current_recursion: 0, extern_builtins: HashMap::new(), use_elisp_apply: false, delegation_depth: 0, globals_record: Sexp::Nil, unbound_marker: Sexp::Nil, frames_record: Sexp::Nil } }
    pub fn new_global() -> Self {
        macro_rules! e { ($n:literal) => { ($n, include_str!(concat!("../../../lisp/", $n))) }; }
        const STDLIB_FILES: &[(&str,&str)] = &[e!("nelisp-jit-substrate.el"),e!("nelisp-syscall-table.el"),e!("nelisp-jit-strategy.el"),e!("nelisp-stdlib-env-shim.el"),e!("nelisp-stdlib-eval-special.el"),e!("nelisp-stdlib-error.el"),e!("nelisp-stdlib.el"),e!("nelisp-stdlib-list.el"),e!("nelisp-stdlib-hof.el"),e!("nelisp-stdlib-search.el"),e!("nelisp-stdlib-plist-str.el"),e!("nelisp-stdlib-format.el"),e!("nelisp-stdlib-misc.el"),e!("nelisp-stdlib-os-int-helpers.el"),e!("nelisp-stdlib-os.el"),e!("nelisp-pcase.el"),e!("nelisp-cl-macros.el"),e!("nelisp-stdlib-hash.el"),e!("nelisp-stdlib-equal.el"),e!("nelisp-stdlib-prn.el"),e!("nelisp-stdlib-reader.el"),e!("nelisp-stdlib-eval-core.el"),e!("nelisp-stdlib-math.el"),e!("nelisp-stdlib-regex.el"),e!("nelisp-stdlib-fast-hash.el"),e!("nelisp-env.el"),e!("nelisp-lexframe.el"),e!("nelisp-cli.el")];
        let mut env = Env::install_stage0(1024);
        let trace = std::env::var_os("NELISP_EVAL_BOOT_TRACE").map_or(false, |v| !v.is_empty() && v != "0");
        for (name, source) in STDLIB_FILES { for (idx, form) in crate::reader::read_all(source).unwrap_or_else(|e| panic!("{name} eval-boot read failed: {e}")).iter().enumerate() {
            if trace { eprintln!("[eval-boot] {name}: form #{idx}"); } crate::eval::eval(form, &mut env).unwrap_or_else(|e| panic!("{name} eval-boot eval failed at form #{idx}: {e}\nform: {}", crate::eval::sexp::fmt_sexp(form))); } }
        env.use_elisp_apply = std::env::var_os("NELISP_USE_RUST_APPLY").map_or(true, |v| v.is_empty());
        env.mirror_set_value("nelisp--unbound-marker", env.unbound_marker.clone()); env
    }
    pub fn empty() -> Self { Env::fresh(256) }
    pub fn new_global_no_stdlib() -> Self { Env::install_stage0(1024) }
    fn install_stage0(max_recursion: u32) -> Self {
        let mut env = Env::fresh(max_recursion); env.install_empty_mirror_rust_direct();
        let (u, p, c) = (env.unbound_marker.clone(), Sexp::Nil, Sexp::T);
        for (n, v) in [("nil", Sexp::Nil), ("t", Sexp::T)] { env.with_mirror_symbol(n, |m, s| unsafe { crate::elisp_cc_spike::mirror_install_entry_or_insert(m, s, &v, &u, &p, &c); }); }
        super::builtins::install_builtins(&mut env);
        env.register_extern_builtin("nelisp--env-globals-op", |args, env| super::env_shim::bi_globals_op(args, env)); env }
    pub fn register_extern_builtin<F: Fn(&[Sexp], &mut Env) -> Result<Sexp, EvalError> + 'static>(&mut self, name: &str, f: F) { self.extern_builtins.insert(name.to_string(), Rc::new(f));
        self.mirror_set_function(name, Sexp::list_from(&[Sexp::Symbol("builtin".into()), Sexp::Symbol(name.into())])); }
    pub fn lookup_value(&self, name: &str) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return if name == "nelisp--unbound-marker" { Ok(self.unbound_marker.clone()) } else { Err(EvalError::unbound_var(name)) }; }
        let (n, mut o) = (Sexp::Symbol(name.into()), Sexp::Nil); if unsafe { crate::elisp_cc_spike::env_lookup_value(&self.globals_record, &self.frames_record, &n, &mut o) } == 0 { Ok(o) } else { Err(EvalError::unbound_var(name)) } }
    fn unbound_scratch(&self, value: Sexp) -> Sexp { crate::elisp_cc_spike::build_or_insert_scratch_vec(value, self.unbound_marker.clone(), Sexp::Nil, Sexp::Nil) }
    pub fn set_value(&mut self, name: &str, value: Sexp) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return Err(EvalError::unbound_var(name)); }
        let n = Sexp::Symbol(name.into()); let sc = self.unbound_scratch(value.clone()); if unsafe { crate::elisp_cc_spike::env_set_value(&self.globals_record, &self.frames_record, &n, &value, &sc, 0) } == 0 { Ok(value) } else { Err(EvalError::setting_constant(name)) } }
    pub fn lookup_function(&self, name: &str) -> Result<Sexp, EvalError> {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return Err(EvalError::unbound_fn(name)); }
        let (n, mut o) = (Sexp::Symbol(name.into()), Sexp::Nil); if unsafe { crate::elisp_cc_spike::env_lookup_function(&self.globals_record, &self.unbound_marker, &n, &mut o) } == 0 { Ok(o) } else { Err(EvalError::unbound_fn(name)) } }
    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        if !matches!(&self.globals_record, Sexp::Record(_)) { return; }
        let n = Sexp::Symbol(name.into()); let sc = self.unbound_scratch(value.clone()); unsafe { crate::elisp_cc_spike::env_bind_local(&self.globals_record, &self.frames_record, &n, &value, &sc, 0) }; }
    pub fn capture_lexical(&mut self) -> Sexp { let Sexp::Record(r) = &self.frames_record else { return Sexp::Nil }; let (Some(Sexp::Int(d)), Ok(f)) = (r.slots.get(1), self.lookup_function("nelisp-lexframe-stack-capture-to-depth")) else { return Sexp::Nil };
        super::apply_function(&f, &[self.frames_record.clone(), Sexp::Int(*d)], self).unwrap_or(Sexp::Nil) }
    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let n = Env::wrap_alist_cells(alist)?; let Ok(f) = self.lookup_function("nelisp-lexframe-make-from-alist") else { return Ok(()) }; let frame = super::apply_function(&f, &[n], self)?; unsafe { crate::elisp_cc_spike::frame_stack_install(&self.frames_record, &frame); } Ok(()) }
    fn with_mirror_symbol<T>(&self, name: &str, f: impl FnOnce(*const Sexp, *const Sexp) -> T) -> Option<T> { if !matches!(&self.globals_record, Sexp::Record(_)) { return None; } let s = Sexp::Symbol(name.into()); Some(f(&self.globals_record, &s)) }
    fn with_mirror_unbound<T>(&self, name: &str, f: impl FnOnce(*const Sexp, *const Sexp, *const Sexp) -> T) -> Option<T> { self.with_mirror_symbol(name, |m,s| f(m,s,&self.unbound_marker)) }
    fn mirror_mutate_with(&mut self, name: &str, payload: &Sexp, op: unsafe fn(*const Sexp, *const Sexp, *const Sexp, *const Sexp) -> i64) { self.with_mirror_unbound(name, |m,s,u| unsafe { op(m,s,payload,u); }); }
    mirror_op!(mutate: mirror_set_value => mirror_set_value_or_insert);
    mirror_op!(mutate: mirror_set_function => mirror_set_function_or_insert);
    pub fn install_empty_mirror_rust_direct(&mut self) { self.unbound_marker = Sexp::Symbol("nelisp--unbound-marker".into()); unsafe { crate::elisp_cc_spike::env_install_empty_globals_frames(&mut self.globals_record, &mut self.frames_record); } }
    mirror_op!(lookup: mirror_lookup_value(pub) => mirror_lookup_value);
    mirror_op!(lookup: mirror_lookup_function(pub) => mirror_lookup_function);
    mirror_op!(pred: mirror_is_fbound(pub) => mirror_is_fbound);
    pub(crate) fn frame_push_rust_direct(&mut self) { if matches!(&self.frames_record, Sexp::Record(_)) { unsafe { crate::elisp_cc_spike::frame_push(&self.frames_record as *const Sexp) }; } }
    pub(crate) fn frame_pop_rust_direct(&mut self) { let Sexp::Record(sr) = &self.frames_record else { return }; let sr = sr.clone(); let (Some(Sexp::Vector(bk)), Some(Sexp::Int(d))) = (sr.slots.get(0), sr.slots.get(1)) else { return };
        let (d, bk) = (*d as usize, bk.clone()); if d == 0 { return; } unsafe { bk.with_value_mut(|v| v[d-1]=Sexp::Nil); sr.with_slots_mut(|s| s[1]=Sexp::Int(d as i64-1)) } }
    pub fn frame_stack_find_rust_direct(&self, name: &str) -> Option<Sexp> { if !matches!(&self.frames_record, Sexp::Record(_)) { return None; } let raw = unsafe { crate::elisp_cc_spike::frame_stack_find_raw(&self.frames_record, &Sexp::Str(name.to_string())) }; if raw.is_null() { None } else { Some(unsafe { (*raw).clone() }) } }
    pub(crate) fn wrap_alist_cells(alist: &Sexp) -> Result<Sexp, EvalError> { let mut r = Sexp::Nil; if unsafe { crate::elisp_cc_spike::wrap_alist_cells(alist, &mut r) } == 1 { Ok(r) } else { Err(EvalError::internal("wrap_alist_cells: malformed closure env alist")) } }
}
