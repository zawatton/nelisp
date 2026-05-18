//! Generic libffi-backed `nl-ffi-call`.
//!
//! ```elisp
//! (nl-ffi-call PATH FUNC SIG ARGS...)
//!   PATH = cdylib basename / abs path / nil (= main process)
//!   FUNC = C symbol name
//!   SIG  = type-keyword vector — element 0 is return type, rest are args
//!   Type keywords: :uint{8,16,32,64} :sint{8,16,32,64}
//!                  :float :double :pointer :void :string
//! ```
//! Library handles are cached after `dlopen`.

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

fn resolve_libname(path: &str) -> &str {
    if path == "libc" {
        #[cfg(target_os = "linux")]
        { return "libc.so.6"; }
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        { return "libSystem.dylib"; }
        #[cfg(target_os = "windows")]
        { return "msvcrt.dll"; }
    }
    path
}

fn dlopen(path: &str) -> Result<&'static Library, EvalError> {
    let resolved = resolve_libname(path);
    {
        let cache = library_cache().lock().unwrap();
        if let Some(lib) = cache.get(resolved) {
            return Ok(*lib);
        }
    }
    let lib = unsafe { Library::new(resolved) }
        .map_err(|e| ffi_err(format!("nl-ffi-call: dlopen failed for {:?}: {}", resolved, e)))?;
    let leaked: &'static Library = Box::leak(Box::new(lib));
    library_cache().lock().unwrap().insert(resolved.to_string(), leaked);
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
    U8(u8), U16(u16), U32(u32), U64(u64),
    I8(i8), I16(i16), I32(i32), I64(i64),
    F32(f32), F64(f64),
    Ptr(*const c_void),
    Cstr { _owner: CString, ptr: *const c_void },
}

fn sym_str<'a>(kw: &'a Sexp, ctx: &str) -> Result<&'a str, EvalError> {
    match kw {
        Sexp::Symbol(s) => Ok(s.as_str()),
        _ => Err(ffi_err(format!("nl-ffi-call: {} must be a keyword symbol, got {:?}", ctx, kw))),
    }
}

fn parse_type(kw: &Sexp) -> Result<Type, EvalError> {
    Ok(match sym_str(kw, "type designator")? {
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
    Ok(match sym_str(ty_kw, "arg type")? {
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
            _ => return Err(ffi_err(format!("nl-ffi-call: :pointer arg expects integer, got {:?}", value))),
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
        ArgValue::U8(v) => Arg::new(v), ArgValue::U16(v) => Arg::new(v),
        ArgValue::U32(v) => Arg::new(v), ArgValue::U64(v) => Arg::new(v),
        ArgValue::I8(v) => Arg::new(v), ArgValue::I16(v) => Arg::new(v),
        ArgValue::I32(v) => Arg::new(v), ArgValue::I64(v) => Arg::new(v),
        ArgValue::F32(v) => Arg::new(v), ArgValue::F64(v) => Arg::new(v),
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
        Sexp::Vector(v) => v.value.clone(),
        _ => return Err(ffi_err(format!("nl-ffi-call: SIG must be a vector, got {:?}", args[2]))),
    };
    if sig_vec.is_empty() {
        return Err(ffi_err("nl-ffi-call: SIG vector is empty".into()));
    }
    let ret_kw = sig_vec[0].clone();
    let arg_kws: Vec<Sexp> = sig_vec.iter().skip(1).cloned().collect();
    let call_args = &args[3..];
    if call_args.len() != arg_kws.len() {
        return Err(ffi_err(format!(
            "nl-ffi-call: argument count mismatch: SIG declares {} arg(s), got {}",
            arg_kws.len(), call_args.len()
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
    let ret_name = sym_str(&ret_kw, "return type")?.to_string();
    let result = unsafe {
        match ret_name.as_str() {
            ":uint8"  => Sexp::Int(cif.call::<u8>(code, &arg_refs) as i64),
            ":uint16" => Sexp::Int(cif.call::<u16>(code, &arg_refs) as i64),
            ":uint32" => Sexp::Int(cif.call::<u32>(code, &arg_refs) as i64),
            ":uint64" => Sexp::Int(cif.call::<u64>(code, &arg_refs) as i64),
            ":sint8"  => Sexp::Int(cif.call::<i8>(code, &arg_refs) as i64),
            ":sint16" => Sexp::Int(cif.call::<i16>(code, &arg_refs) as i64),
            ":sint32" => Sexp::Int(cif.call::<i32>(code, &arg_refs) as i64),
            ":sint64" => Sexp::Int(cif.call::<i64>(code, &arg_refs)),
            ":float"  => Sexp::Float(cif.call::<f32>(code, &arg_refs) as f64),
            ":double" => Sexp::Float(cif.call::<f64>(code, &arg_refs)),
            ":pointer" => Sexp::Int(cif.call::<*const c_void>(code, &arg_refs) as i64),
            ":string" => {
                let p = cif.call::<*const i8>(code, &arg_refs);
                if p.is_null() { Sexp::Nil }
                else { Sexp::Str(CStr::from_ptr(p).to_string_lossy().into_owned()) }
            }
            ":void" => { cif.call::<()>(code, &arg_refs); Sexp::Nil }
            other => return Err(ffi_err(format!("nl-ffi-call: unknown return type {:?}", other))),
        }
    };
    drop(slots);
    Ok(result)
}

// ---- Tracked buffer registry -----------------------------------------------

fn alloc_table() -> &'static Mutex<HashMap<i64, usize>> {
    static T: OnceLock<Mutex<HashMap<i64, usize>>> = OnceLock::new();
    T.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Look up a tracked buffer length, returning an error tagged with NAME.
fn alloc_len(name: &str, p: i64) -> Result<usize, EvalError> {
    alloc_table().lock().unwrap().get(&p).copied().ok_or_else(|| {
        ffi_err(format!("{}: pointer {} not from nl-ffi-malloc", name, p))
    })
}

/// `(nl-ffi-malloc N)` -> integer raw pointer (zeroed).
pub fn nl_ffi_malloc(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-ffi-malloc", args, 1)?;
    let n = coerce_int(&args[0])?;
    if n < 0 {
        return Err(ffi_err(format!("nl-ffi-malloc: negative size {}", n)));
    }
    let v: Vec<u8> = vec![0u8; n as usize];
    let len = v.len();
    let leaked: *mut u8 = Box::leak(v.into_boxed_slice()).as_mut_ptr();
    alloc_table().lock().unwrap().insert(leaked as i64, len);
    Ok(Sexp::Int(leaked as i64))
}

/// `(nl-ffi-free PTR)` -> t on success, signals on bad/double-free.
pub fn nl_ffi_free(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-ffi-free", args, 1)?;
    let p = coerce_int(&args[0])?;
    let len = alloc_table().lock().unwrap().remove(&p).ok_or_else(|| {
        ffi_err(format!("nl-ffi-free: pointer {} not from nl-ffi-malloc", p))
    })?;
    unsafe {
        let slice = std::slice::from_raw_parts_mut(p as *mut u8, len);
        let _ = Box::from_raw(slice as *mut [u8]);
    }
    Ok(Sexp::T)
}

fn require_arity(name: &str, args: &[Sexp], expected: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        return Err(ffi_err(format!(
            "{}: expected {} arg(s), got {}", name, expected, args.len()
        )));
    }
    Ok(())
}

/// `(nl-ffi-read-bytes PTR LEN)` or `(nl-ffi-read-bytes-at PTR OFFSET LEN)`.
/// Public dispatch arm chooses based on arity (2 or 3).
pub fn nl_ffi_read_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, off, n, name) = match args.len() {
        2 => (coerce_int(&args[0])?, 0i64, coerce_int(&args[1])?, "nl-ffi-read-bytes"),
        3 => (coerce_int(&args[0])?, coerce_int(&args[1])?, coerce_int(&args[2])?, "nl-ffi-read-bytes-at"),
        _ => return Err(ffi_err(format!(
            "nl-ffi-read-bytes: expected 2 or 3 args, got {}", args.len()))),
    };
    if p == 0 { return Err(ffi_err(format!("{}: NULL pointer", name))); }
    if off < 0 || n < 0 {
        return Err(ffi_err(format!("{}: negative offset/len ({}, {})", name, off, n)));
    }
    // `nl-ffi-read-bytes` (2-arg) is grandfathered: it does not require
    // a tracked buffer (legacy callers pass FFI return pointers).
    // The 3-arg `-at' form does enforce the tracked-buffer bound.
    if args.len() == 3 {
        let len = alloc_len(name, p)?;
        if (off as usize) + (n as usize) > len {
            return Err(ffi_err(format!(
                "{}: read of {} bytes at offset {} exceeds buffer length {}",
                name, n, off, len)));
        }
    }
    let bytes = unsafe { std::slice::from_raw_parts((p + off) as *const u8, n as usize) };
    Ok(Sexp::Str(String::from_utf8_lossy(bytes).into_owned()))
}

/// `(nl-ffi-write-bytes PTR STR)` or `(nl-ffi-write-bytes-at PTR OFFSET STR)`.
pub fn nl_ffi_write_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let (p, off, str_idx, name) = match args.len() {
        2 => (coerce_int(&args[0])?, 0i64, 1usize, "nl-ffi-write-bytes"),
        3 => (coerce_int(&args[0])?, coerce_int(&args[1])?, 2usize, "nl-ffi-write-bytes-at"),
        _ => return Err(ffi_err(format!(
            "nl-ffi-write-bytes: expected 2 or 3 args, got {}", args.len()))),
    };
    if p == 0 { return Err(ffi_err(format!("{}: NULL pointer", name))); }
    if off < 0 {
        return Err(ffi_err(format!("{}: negative offset {}", name, off)));
    }
    let bytes_owned = args[str_idx].as_string_owned().ok_or_else(|| {
        ffi_err(format!("{}: expected string for str arg, got {:?}", name, args[str_idx]))
    })?;
    let bytes = bytes_owned.as_bytes();
    let len = alloc_len(name, p)?;
    if (off as usize) + bytes.len() > len {
        return Err(ffi_err(format!(
            "{}: write of {} bytes at offset {} exceeds buffer length {}",
            name, bytes.len(), off, len)));
    }
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), (p + off) as *mut u8, bytes.len());
    }
    Ok(Sexp::T)
}

// ---- Unaligned integer read/write (width-dispatched) ------------------------

fn bounds_check(name: &str, p: i64, off: i64, sz: usize) -> Result<(), EvalError> {
    let len = alloc_len(name, p)?;
    if (off as usize) + sz > len {
        return Err(ffi_err(format!(
            "{}: access at offset {} (size {}) exceeds buffer length {}",
            name, off, sz, len)));
    }
    Ok(())
}

/// `(nl-ffi-read-int PTR OFFSET WIDTH SIGNED)` -> Sexp::Int.
/// WIDTH ∈ {1, 2, 4, 8}; SIGNED = t/nil controls sign extension.
pub fn nl_ffi_read_int(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let name = "nl-ffi-read-int";
    require_arity(name, args, 4)?;
    let p = coerce_int(&args[0])?;
    let off = coerce_int(&args[1])?;
    let width = coerce_int(&args[2])? as usize;
    let signed = !matches!(&args[3], Sexp::Nil);
    if p == 0 { return Err(ffi_err(format!("{}: NULL pointer", name))); }
    if off < 0 { return Err(ffi_err(format!("{}: negative offset {}", name, off))); }
    bounds_check(name, p, off, width)?;
    let addr = (p + off) as *const u8;
    let v: i64 = unsafe {
        match (width, signed) {
            (1, true)  => std::ptr::read_unaligned(addr as *const i8) as i64,
            (1, false) => std::ptr::read_unaligned(addr) as i64,
            (2, true)  => std::ptr::read_unaligned(addr as *const i16) as i64,
            (2, false) => std::ptr::read_unaligned(addr as *const u16) as i64,
            (4, true)  => std::ptr::read_unaligned(addr as *const i32) as i64,
            (4, false) => std::ptr::read_unaligned(addr as *const u32) as i64,
            (8, _)     => std::ptr::read_unaligned(addr as *const i64),
            _ => return Err(ffi_err(format!("{}: unsupported width {}", name, width))),
        }
    };
    Ok(Sexp::Int(v))
}

/// `(nl-ffi-write-int PTR OFFSET WIDTH VALUE)` -> t.
pub fn nl_ffi_write_int(args: &[Sexp]) -> Result<Sexp, EvalError> {
    let name = "nl-ffi-write-int";
    require_arity(name, args, 4)?;
    let p = coerce_int(&args[0])?;
    let off = coerce_int(&args[1])?;
    let width = coerce_int(&args[2])? as usize;
    let v = coerce_int(&args[3])?;
    if p == 0 { return Err(ffi_err(format!("{}: NULL pointer", name))); }
    if off < 0 { return Err(ffi_err(format!("{}: negative offset {}", name, off))); }
    bounds_check(name, p, off, width)?;
    let addr = (p + off) as *mut u8;
    unsafe {
        match width {
            1 => std::ptr::write_unaligned(addr, v as u8),
            2 => std::ptr::write_unaligned(addr as *mut i16, v as i16),
            4 => std::ptr::write_unaligned(addr as *mut i32, v as i32),
            8 => std::ptr::write_unaligned(addr as *mut i64, v),
            _ => return Err(ffi_err(format!("{}: unsupported width {}", name, width))),
        }
    }
    Ok(Sexp::T)
}

/// `(nl-ffi-errno)` returns the current thread's libc errno value.
pub fn nl_ffi_errno(args: &[Sexp]) -> Result<Sexp, EvalError> {
    require_arity("nl-ffi-errno", args, 0)?;
    #[cfg(target_os = "linux")]
    let e = unsafe { *libc::__errno_location() } as i64;
    #[cfg(any(target_os = "macos", target_os = "ios", target_os = "freebsd",
              target_os = "netbsd", target_os = "openbsd", target_os = "dragonfly"))]
    let e = unsafe { *libc::__error() } as i64;
    #[cfg(target_os = "windows")]
    let e = unsafe { *libc::__errno() } as i64;
    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "ios",
                  target_os = "freebsd", target_os = "netbsd",
                  target_os = "openbsd", target_os = "dragonfly",
                  target_os = "windows")))]
    let e: i64 = 0;
    Ok(Sexp::Int(e))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn malloc_n(n: i64) -> i64 {
        match nl_ffi_malloc(&[Sexp::Int(n)]).unwrap() {
            Sexp::Int(p) => p,
            other => panic!("nl-ffi-malloc returned {:?}", other),
        }
    }

    fn read_n_str(p: i64, n: i64) -> String {
        match nl_ffi_read_bytes(&[Sexp::Int(p), Sexp::Int(n)]).unwrap() {
            Sexp::Str(s) => s,
            other => panic!("nl-ffi-read-bytes returned {:?}", other),
        }
    }

    #[test]
    fn write_bytes_round_trip_str() {
        let p = malloc_n(16);
        let r = nl_ffi_write_bytes(&[Sexp::Int(p), Sexp::Str("hello".into())]).unwrap();
        assert_eq!(r, Sexp::T);
        assert_eq!(read_n_str(p, 5), "hello");
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn write_bytes_rejects_null_ptr() {
        assert!(nl_ffi_write_bytes(&[Sexp::Int(0), Sexp::Str("x".into())]).is_err());
    }

    #[test]
    fn write_bytes_rejects_unknown_ptr() {
        assert!(nl_ffi_write_bytes(&[Sexp::Int(0xDEAD_BEEF), Sexp::Str("x".into())]).is_err());
    }

    #[test]
    fn write_bytes_rejects_oversize() {
        let p = malloc_n(4);
        assert!(nl_ffi_write_bytes(&[Sexp::Int(p), Sexp::Str("12345".into())]).is_err());
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn write_bytes_empty_str_is_noop() {
        let p = malloc_n(4);
        let r = nl_ffi_write_bytes(&[Sexp::Int(p), Sexp::Str(String::new())]).unwrap();
        assert_eq!(r, Sexp::T);
        assert_eq!(read_n_str(p, 4).as_bytes(), &[0u8, 0, 0, 0]);
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn errno_reads_thread_local() {
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            let bad = CString::new("/this-path-must-not-exist-for-test").unwrap();
            let _ = unsafe { libc::open(bad.as_ptr(), libc::O_RDONLY) };
            match nl_ffi_errno(&[]).unwrap() {
                Sexp::Int(e) => assert!(e > 0, "errno after failed open: {}", e),
                other => panic!("nl-ffi-errno returned {:?}", other),
            }
        }
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            assert!(matches!(nl_ffi_errno(&[]).unwrap(), Sexp::Int(_)));
        }
    }

    #[test]
    fn errno_rejects_args() {
        assert!(nl_ffi_errno(&[Sexp::Int(0)]).is_err());
    }
}
