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
        env.intern_constant("nil", Sexp::Nil);
        env.intern_constant("t", Sexp::T);
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
        self.set_function(name, sentinel);
    }

    pub fn intern_constant(&mut self, name: &str, value: Sexp) {
        self.mirror_install_entry(name, Some(value), None, None, true);
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

    pub fn set_function(&mut self, name: &str, func: Sexp) {
        self.mirror_set_function(name, func);
    }


    pub fn bind_local(&mut self, name: &str, value: Sexp) {
        let has_frame = matches!(&self.frames_record, Sexp::Record(r)
            if matches!(r.slots.get(1), Some(Sexp::Int(n)) if *n > 0));
        if has_frame {
            self.frame_bind_rust_direct(name, Sexp::Cell(FrameCell::new(value)));
        } else {
            self.mirror_set_value(name, value);
        }
    }

    pub fn capture_lexical(&mut self) -> Sexp {
        let depth = match &self.frames_record {
            Sexp::Record(r) => match r.slots.get(1) {
                Some(Sexp::Int(n)) => *n,
                _ => return Sexp::Nil,
            },
            _ => return Sexp::Nil,
        };
        let Ok(f) = self.lookup_function("nelisp-lexframe-stack-capture-to-depth") else {
            return Sexp::Nil;
        };
        let args = [self.frames_record.clone(), Sexp::Int(depth)];
        super::apply_function(&f, &args, self).unwrap_or(Sexp::Nil)
    }

    pub fn push_captured(&mut self, alist: &Sexp) -> Result<(), EvalError> {
        let normalized = Env::wrap_alist_cells(alist)?;
        let f = match self.lookup_function("nelisp-lexframe-make-from-alist") {
            Ok(f) => f,
            Err(_) => return Ok(()),
        };
        let frame = super::apply_function(&f, &[normalized], self)?;
        if let Some((stack_rec, backing, depth)) = self.frame_stack_view() {
            Env::frame_stack_install(&stack_rec, &backing, depth, frame);
        }
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

    fn frame_bucket(frame: &Sexp, name: &str) -> Option<(NlRecordRef, NlVectorRef, usize)> {
        let Sexp::Record(frame_rec) = frame else {
            return None;
        };
        let ht_rec = match frame_rec.slots.get(0)? {
            Sexp::Record(r) => r.clone(),
            _ => return None,
        };
        let bucket_count = match ht_rec.slots.get(0)? {
            Sexp::Int(n) => *n as u32,
            _ => return None,
        };
        let buckets = match ht_rec.slots.get(1)? {
            Sexp::Vector(v) => v.clone(),
            _ => return None,
        };
        let name_sym = Sexp::Symbol(name.into());
        let h = unsafe { crate::elisp_cc_spike::fnv1a(&name_sym) } as u32;
        let idx = (if bucket_count & (bucket_count - 1) == 0 {
            h & (bucket_count - 1)
        } else {
            h % bucket_count
        }) as usize;
        Some((ht_rec, buckets, idx))
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
    mirror_op!(clear: mirror_clear_value => mirror_clear_value);
    mirror_op!(clear: mirror_clear_function => mirror_clear_function);

    pub fn mirror_install_entry(
        &mut self,
        name: &str,
        value: Option<Sexp>,
        function: Option<Sexp>,
        plist: Option<Sexp>,
        constant: bool,
    ) {
        let unbound = self.unbound_marker.clone();
        let value_slot = value.unwrap_or_else(|| unbound.clone());
        let function_slot = function.unwrap_or(unbound);
        let plist_slot = plist.unwrap_or(Sexp::Nil);
        let constant_slot = if constant { Sexp::T } else { Sexp::Nil };
        self.with_mirror_symbol(name, |mirror_ptr, sym_ptr| unsafe {
            crate::elisp_cc_spike::mirror_install_entry_or_insert(
                mirror_ptr,
                sym_ptr,
                &value_slot,
                &function_slot,
                &plist_slot,
                &constant_slot,
            );
        });
    }

    pub(crate) fn mirror_is_constant(&self, name: &str) -> bool {
        self.with_mirror_symbol(name, |mirror_ptr, sym_ptr| unsafe {
            crate::elisp_cc_spike::mirror_is_constant(mirror_ptr, sym_ptr) != 0
        })
        .unwrap_or(false)
    }

    pub(crate) fn mirror_set_constant(&mut self, name: &str, truthy: bool) {
        let value = if truthy { Sexp::T } else { Sexp::Nil };
        self.mirror_mutate_with(
            name,
            &value,
            crate::elisp_cc_spike::mirror_set_constant_or_insert,
        );
    }

    fn make_fast_hash_table(bucket_count: usize) -> Sexp {
        let buckets = Sexp::vector(vec![Sexp::Nil; bucket_count]);
        Sexp::record(
            Sexp::Symbol("fast-hash-table".into()),
            vec![Sexp::Int(bucket_count as i64), buckets, Sexp::Int(0)],
        )
    }

    pub fn install_empty_mirror_rust_direct(&mut self) {
        self.unbound_marker = Sexp::Symbol("nelisp--unbound-marker".into());
        self.globals_record = Sexp::record(
            Sexp::Symbol("nelisp-env".into()),
            vec![Env::make_fast_hash_table(1024), Sexp::Nil, Sexp::Nil],
        );
        self.install_empty_frames_record_rust_direct();
    }

    mirror_op!(lookup: mirror_lookup_value(pub) => mirror_lookup_value);
    mirror_op!(lookup: mirror_lookup_function(pub) => mirror_lookup_function);

    mirror_op!(pred: mirror_is_bound(pub(crate)) => mirror_is_bound);
    mirror_op!(pred: mirror_is_fbound(pub) => mirror_is_fbound);

    pub(crate) fn install_empty_frames_record_rust_direct(&mut self) {
        const INITIAL_CAPACITY: usize = 8;
        let backing = Sexp::vector(vec![Sexp::Nil; INITIAL_CAPACITY]);
        self.frames_record = Sexp::record(
            Sexp::Symbol("nelisp-lexframe-stack".into()),
            vec![backing, Sexp::Int(0)],
        );
    }

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

    fn make_empty_frame_record() -> Sexp {
        Sexp::record(
            Sexp::Symbol("nelisp-lexframe".into()),
            vec![Env::make_fast_hash_table(16)],
        )
    }

    pub(crate) fn frame_stack_ensure_capacity(
        stack_rec: &NlRecordRef,
        backing: &NlVectorRef,
        depth: usize,
        needed: usize,
    ) -> NlVectorRef {
        let cap = backing.value.len();
        if cap >= needed {
            return backing.clone();
        }
        let mut new_cap = cap.max(1);
        while new_cap < needed {
            new_cap *= 2;
        }
        let mut new_buf: Vec<Sexp> = (0..depth)
            .map(|i| backing.value.get(i).cloned().unwrap_or(Sexp::Nil))
            .collect();
        new_buf.resize(new_cap, Sexp::Nil);
        let new_vec_sexp = Sexp::vector(new_buf);
        let new_vec_ref = match &new_vec_sexp {
            Sexp::Vector(v) => v.clone(),
            _ => unreachable!(),
        };
        unsafe { stack_rec.with_slots_mut(|s| s[0] = new_vec_sexp) };
        new_vec_ref
    }

    fn frame_stack_install(
        stack_rec: &NlRecordRef,
        backing: &NlVectorRef,
        depth: usize,
        frame: Sexp,
    ) {
        let backing = Env::frame_stack_ensure_capacity(stack_rec, backing, depth, depth + 1);
        unsafe {
            backing.with_value_mut(|v| v[depth] = frame);
            stack_rec.with_slots_mut(|s| s[1] = Sexp::Int((depth + 1) as i64));
        }
    }

    pub(crate) fn frame_push_rust_direct(&mut self) -> Option<Sexp> {
        let (stack_rec, backing, depth) = self.frame_stack_view()?;
        let frame = Env::make_empty_frame_record();
        Env::frame_stack_install(&stack_rec, &backing, depth, frame.clone());
        Some(frame)
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
        let Some((_, backing, depth)) = self.frame_stack_view() else {
            return;
        };
        if depth == 0 {
            return;
        }
        let Some(frame) = backing.value.get(depth - 1).cloned() else {
            return;
        };
        Env::frame_bind_into(&frame, name, cell);
    }

    fn frame_bind_into(frame: &Sexp, name: &str, cell: Sexp) {
        let Some((ht_rec, buckets, idx)) = Env::frame_bucket(frame, name) else {
            return;
        };
        let bucket = match buckets.value.get(idx) {
            Some(b) => b.clone(),
            None => return,
        };
        let mut cur = bucket;
        while let Sexp::Cons(c) = &cur {
            if let Sexp::Cons(pair) = &c.car {
                if let Sexp::Str(k) = &pair.car {
                    if k == name {
                        unsafe { pair.set_cdr(cell) };
                        return;
                    }
                }
            }
            cur = c.cdr.clone();
        }
        let pair = Sexp::cons(Sexp::Str(name.to_string()), cell);
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

    fn frame_lookup_in(frame: &Sexp, name: &str) -> Option<Sexp> {
        let (_, buckets, idx) = Env::frame_bucket(frame, name)?;
        let mut cur = buckets.value.get(idx)?;
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

    pub fn frame_lookup_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        if depth == 0 {
            return None;
        }
        Env::frame_lookup_in(backing.value.get(depth - 1)?, name)
    }

    pub fn frame_stack_find_rust_direct(&self, name: &str) -> Option<Sexp> {
        let (_stack_rec, backing, depth) = self.frame_stack_view()?;
        for i in (0..depth).rev() {
            let Some(frame) = backing.value.get(i) else {
                continue;
            };
            if let Some(cell) = Env::frame_lookup_in(frame, name) {
                return Some(cell);
            }
        }
        None
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
