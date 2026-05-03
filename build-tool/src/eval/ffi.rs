//! Doc 51 Phase 5 — single generic FFI primitive `nl-ffi-call'.
//!
//! Design intent (= "elisp で実装可能なものは elisp で" + "build-tool は
//! 内部 crate dep を持たない" Sweep 6 invariant): expose ONE primitive
//! that can call ANY function in ANY cdylib via libffi.  All higher-level
//! marshalling (sqlite-* / nelisp-syscall-* / ...) stays in pure Elisp on
//! top of this generic bridge.
//!
//! Wire signature mirrors the public elisp-ffi / nelisp-ffi API so the
//! existing wrapper repos (`zawatton/nelisp-ffi') can detect this
//! primitive at runtime and route in-process instead of spawning the
//! libffi-glue subprocess.
//!
//! ```elisp
//! (nl-ffi-call PATH FUNC SIG ARGS...)
//!   PATH = string (cdylib basename or absolute path) | nil (= main process)
//!   FUNC = string (C symbol name)
//!   SIG  = vector of type keywords; element 0 is the return type, the
//!          rest are argument types (= elisp-ffi convention).
//!   ARGS = the remaining call arguments, count + types must match SIG.
//!
//!   Type keywords (= elisp-ffi superset):
//!     :uint8 :uint16 :uint32 :uint64
//!     :sint8 :sint16 :sint32 :sint64
//!     :float :double :pointer :void :string
//!
//!   Returns: Sexp::Int for integral / pointer returns, Sexp::Float for
//!   float / double, Sexp::Str for :string (= NUL-terminated C-string at
//!   the returned pointer), Sexp::Nil for :void.
//!
//!   Errors: signal `ffi-error' with a descriptive message on dlopen /
//!   dlsym / argument-count / argument-type mismatch.
//! ```
//!
//! Library-handle caching: each PATH is dlopen'd once and the
//! `Library` is leaked into a global `OnceLock<Mutex<HashMap>>`.  This
//! matches Emacs Dynamic Module semantics (modules cannot be unloaded)
//! and avoids per-call dlopen cost.

use super::error::EvalError;
use super::sexp::Sexp;
use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;
use std::collections::HashMap;
use std::ffi::{c_void, CStr, CString};
use std::sync::{Mutex, OnceLock};

fn ffi_err(msg: String) -> EvalError {
    EvalError::UserError {
        tag: "ffi-error".into(),
        data: Sexp::list_from(&[Sexp::Str(msg)]),
    }
}

fn library_cache() -> &'static Mutex<HashMap<String, &'static Library>> {
    static CACHE: OnceLock<Mutex<HashMap<String, &'static Library>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn dlopen(path: &str) -> Result<&'static Library, EvalError> {
    {
        let cache = library_cache().lock().unwrap();
        if let Some(lib) = cache.get(path) {
            return Ok(*lib);
        }
    }
    let lib = unsafe { Library::new(path) }
        .map_err(|e| ffi_err(format!("nl-ffi-call: dlopen failed for {:?}: {}", path, e)))?;
    let leaked: &'static Library = Box::leak(Box::new(lib));
    library_cache()
        .lock()
        .unwrap()
        .insert(path.to_string(), leaked);
    Ok(leaked)
}

fn dlsym(lib: &'static Library, name: &str) -> Result<*const c_void, EvalError> {
    let cstr = CString::new(name)
        .map_err(|_| ffi_err(format!("nl-ffi-call: function name has interior NUL: {:?}", name)))?;
    let raw: libloading::Symbol<*const c_void> = unsafe { lib.get(cstr.as_bytes_with_nul()) }
        .map_err(|e| ffi_err(format!("nl-ffi-call: dlsym failed for {:?}: {}", name, e)))?;
    Ok(*raw)
}

enum ArgValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Ptr(*const c_void),
    /// `:string` argument: the CString owns the bytes; `ptr` is the
    /// libffi-visible pointer slot.  Stored alongside the owner so the
    /// reference passed to `Arg::new` lives for the entire `cif.call`.
    Cstr { _owner: CString, ptr: *const c_void },
}

fn parse_type(kw: &Sexp) -> Result<Type, EvalError> {
    let name = match kw {
        Sexp::Symbol(s) => s.as_str(),
        _ => return Err(ffi_err(format!("nl-ffi-call: type designator must be a keyword symbol, got {:?}", kw))),
    };
    Ok(match name {
        ":uint8" => Type::u8(),
        ":uint16" => Type::u16(),
        ":uint32" => Type::u32(),
        ":uint64" => Type::u64(),
        ":sint8" => Type::i8(),
        ":sint16" => Type::i16(),
        ":sint32" => Type::i32(),
        ":sint64" => Type::i64(),
        ":float" => Type::f32(),
        ":double" => Type::f64(),
        ":pointer" | ":string" => Type::pointer(),
        ":void" => Type::void(),
        other => return Err(ffi_err(format!("nl-ffi-call: unknown type {:?}", other))),
    })
}

fn type_name(kw: &Sexp) -> String {
    match kw {
        Sexp::Symbol(s) => s.clone(),
        _ => format!("{:?}", kw),
    }
}

fn coerce_int(arg: &Sexp) -> Result<i64, EvalError> {
    match arg {
        Sexp::Int(i) => Ok(*i),
        Sexp::Nil => Ok(0),
        Sexp::T => Ok(1),
        _ => Err(ffi_err(format!("nl-ffi-call: expected integer, got {:?}", arg))),
    }
}

fn coerce_float(arg: &Sexp) -> Result<f64, EvalError> {
    match arg {
        Sexp::Float(f) => Ok(*f),
        Sexp::Int(i) => Ok(*i as f64),
        _ => Err(ffi_err(format!("nl-ffi-call: expected float, got {:?}", arg))),
    }
}

fn build_arg(ty_kw: &Sexp, value: &Sexp) -> Result<ArgValue, EvalError> {
    let kw = match ty_kw {
        Sexp::Symbol(s) => s.as_str(),
        _ => return Err(ffi_err("nl-ffi-call: bad arg type kw".into())),
    };
    Ok(match kw {
        ":uint8" => ArgValue::U8(coerce_int(value)? as u8),
        ":uint16" => ArgValue::U16(coerce_int(value)? as u16),
        ":uint32" => ArgValue::U32(coerce_int(value)? as u32),
        ":uint64" => ArgValue::U64(coerce_int(value)? as u64),
        ":sint8" => ArgValue::I8(coerce_int(value)? as i8),
        ":sint16" => ArgValue::I16(coerce_int(value)? as i16),
        ":sint32" => ArgValue::I32(coerce_int(value)? as i32),
        ":sint64" => ArgValue::I64(coerce_int(value)?),
        ":float" => ArgValue::F32(coerce_float(value)? as f32),
        ":double" => ArgValue::F64(coerce_float(value)?),
        ":pointer" => match value {
            Sexp::Int(i) => ArgValue::Ptr(*i as *const c_void),
            Sexp::Nil => ArgValue::Ptr(std::ptr::null()),
            _ => return Err(ffi_err(format!("nl-ffi-call: :pointer arg expects integer (raw addr), got {:?}", value))),
        },
        ":string" => match value {
            Sexp::Str(s) => {
                let owner = CString::new(s.as_str())
                    .map_err(|_| ffi_err("nl-ffi-call: :string arg has interior NUL".into()))?;
                let ptr = owner.as_ptr() as *const c_void;
                ArgValue::Cstr { _owner: owner, ptr }
            }
            Sexp::Nil => ArgValue::Ptr(std::ptr::null()),
            _ => return Err(ffi_err(format!("nl-ffi-call: :string arg expects string or nil, got {:?}", value))),
        },
        ":void" => return Err(ffi_err("nl-ffi-call: :void cannot appear as an argument type".into())),
        other => return Err(ffi_err(format!("nl-ffi-call: unknown arg type {:?}", other))),
    })
}

fn arg_to_libffi(slot: &ArgValue) -> Arg {
    match slot {
        ArgValue::U8(v) => Arg::new(v),
        ArgValue::U16(v) => Arg::new(v),
        ArgValue::U32(v) => Arg::new(v),
        ArgValue::U64(v) => Arg::new(v),
        ArgValue::I8(v) => Arg::new(v),
        ArgValue::I16(v) => Arg::new(v),
        ArgValue::I32(v) => Arg::new(v),
        ArgValue::I64(v) => Arg::new(v),
        ArgValue::F32(v) => Arg::new(v),
        ArgValue::F64(v) => Arg::new(v),
        ArgValue::Ptr(p) => Arg::new(p),
        ArgValue::Cstr { ptr, .. } => Arg::new(ptr),
    }
}

pub fn nl_ffi_call(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() < 3 {
        return Err(ffi_err("nl-ffi-call: need at least PATH FUNC SIG (got fewer)".into()));
    }
    let path = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::Nil => return Err(ffi_err(
            "nl-ffi-call: nil PATH (main process symbols) not supported; pass an explicit cdylib path".into()
        )),
        _ => return Err(ffi_err(format!("nl-ffi-call: PATH must be string, got {:?}", args[0]))),
    };
    let func = match &args[1] {
        Sexp::Str(s) => s.clone(),
        Sexp::Symbol(s) => s.clone(),
        _ => return Err(ffi_err(format!("nl-ffi-call: FUNC must be string or symbol, got {:?}", args[1]))),
    };
    let sig_vec = match &args[2] {
        Sexp::Vector(v) => v.borrow().clone(),
        _ => return Err(ffi_err(format!("nl-ffi-call: SIG must be a vector, got {:?}", args[2]))),
    };
    if sig_vec.is_empty() {
        return Err(ffi_err("nl-ffi-call: SIG vector is empty (need return type + arg types)".into()));
    }
    let ret_kw = sig_vec[0].clone();
    let arg_kws: Vec<Sexp> = sig_vec.iter().skip(1).cloned().collect();
    let call_args = &args[3..];
    if call_args.len() != arg_kws.len() {
        return Err(ffi_err(format!(
            "nl-ffi-call: argument count mismatch: SIG declares {} arg(s), got {}",
            arg_kws.len(),
            call_args.len()
        )));
    }

    let mut slots: Vec<ArgValue> = Vec::with_capacity(arg_kws.len());
    for (kw, val) in arg_kws.iter().zip(call_args.iter()) {
        slots.push(build_arg(kw, val)?);
    }
    let arg_types: Vec<Type> = arg_kws.iter().map(parse_type).collect::<Result<_, _>>()?;
    let ret_type = parse_type(&ret_kw)?;
    let cif = Cif::new(arg_types.into_iter(), ret_type);

    let lib = dlopen(&path)?;
    let sym = dlsym(lib, &func)?;
    let code = CodePtr::from_ptr(sym);

    let arg_refs: Vec<Arg> = slots.iter().map(arg_to_libffi).collect();
    let ret_name = type_name(&ret_kw);
    let result = unsafe {
        match ret_name.as_str() {
            ":uint8" => Sexp::Int(cif.call::<u8>(code, &arg_refs) as i64),
            ":uint16" => Sexp::Int(cif.call::<u16>(code, &arg_refs) as i64),
            ":uint32" => Sexp::Int(cif.call::<u32>(code, &arg_refs) as i64),
            ":uint64" => Sexp::Int(cif.call::<u64>(code, &arg_refs) as i64),
            ":sint8" => Sexp::Int(cif.call::<i8>(code, &arg_refs) as i64),
            ":sint16" => Sexp::Int(cif.call::<i16>(code, &arg_refs) as i64),
            ":sint32" => Sexp::Int(cif.call::<i32>(code, &arg_refs) as i64),
            ":sint64" => Sexp::Int(cif.call::<i64>(code, &arg_refs)),
            ":float" => Sexp::Float(cif.call::<f32>(code, &arg_refs) as f64),
            ":double" => Sexp::Float(cif.call::<f64>(code, &arg_refs)),
            ":pointer" => Sexp::Int(cif.call::<*const c_void>(code, &arg_refs) as i64),
            ":string" => {
                let p = cif.call::<*const i8>(code, &arg_refs);
                if p.is_null() {
                    Sexp::Nil
                } else {
                    let s = CStr::from_ptr(p).to_string_lossy().into_owned();
                    Sexp::Str(s)
                }
            }
            ":void" => {
                cif.call::<()>(code, &arg_refs);
                Sexp::Nil
            }
            other => return Err(ffi_err(format!("nl-ffi-call: unknown return type {:?}", other))),
        }
    };
    drop(slots);
    Ok(result)
}

// ---- Generic out-buffer helpers --------------------------------------------
//
// nl_sqlite_query and similar "fill caller-provided buffer" C APIs need the
// Elisp side to allocate a buffer, pass its address as a :pointer arg, and
// then read N bytes back into a Lisp string.  Three small primitives keep
// this fully composable without per-function dispatch glue.

/// `(nl-ffi-malloc N)` → integer raw pointer (zeroed).
pub fn nl_ffi_malloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 1 {
        return Err(ffi_err(format!(
            "nl-ffi-malloc: expected 1 arg (size), got {}",
            args.len()
        )));
    }
    let n = coerce_int(&args[0])?;
    if n < 0 {
        return Err(ffi_err(format!("nl-ffi-malloc: negative size {}", n)));
    }
    let n = n as usize;
    let mut v: Vec<u8> = vec![0u8; n];
    let ptr = v.as_mut_ptr() as i64;
    // Leak the Vec so the buffer survives until nl-ffi-free.  Track in a
    // global so free() can reconstruct the Box for proper deallocation.
    let len = v.len();
    let _cap = v.capacity();
    let leaked: *mut u8 = Box::leak(v.into_boxed_slice()).as_mut_ptr();
    alloc_table().lock().unwrap().insert(leaked as i64, len);
    Ok(Sexp::Int(leaked as i64))
}

/// `(nl-ffi-read-bytes PTR N)` → string of N bytes copied from PTR.
pub fn nl_ffi_read_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return Err(ffi_err(format!(
            "nl-ffi-read-bytes: expected 2 args (ptr len), got {}",
            args.len()
        )));
    }
    let p = coerce_int(&args[0])? as *const u8;
    let n = coerce_int(&args[1])?;
    if n < 0 {
        return Err(ffi_err(format!("nl-ffi-read-bytes: negative length {}", n)));
    }
    if p.is_null() {
        return Err(ffi_err("nl-ffi-read-bytes: NULL pointer".into()));
    }
    let bytes = unsafe { std::slice::from_raw_parts(p, n as usize) };
    Ok(Sexp::Str(String::from_utf8_lossy(bytes).into_owned()))
}

/// `(nl-ffi-free PTR)` → t on success, signals on bad/double-free.
pub fn nl_ffi_free(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 1 {
        return Err(ffi_err(format!(
            "nl-ffi-free: expected 1 arg (ptr), got {}",
            args.len()
        )));
    }
    let p = coerce_int(&args[0])?;
    let len = alloc_table()
        .lock()
        .unwrap()
        .remove(&p)
        .ok_or_else(|| ffi_err(format!("nl-ffi-free: pointer {} not from nl-ffi-malloc", p)))?;
    unsafe {
        let slice = std::slice::from_raw_parts_mut(p as *mut u8, len);
        let _ = Box::from_raw(slice as *mut [u8]);
    }
    Ok(Sexp::T)
}

fn alloc_table() -> &'static Mutex<HashMap<i64, usize>> {
    static T: OnceLock<Mutex<HashMap<i64, usize>>> = OnceLock::new();
    T.get_or_init(|| Mutex::new(HashMap::new()))
}
