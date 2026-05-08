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

/// Resolve `"libc"` to the platform-specific shared object name so
/// elisp wrappers can write `(nl-ffi-call "libc" ...)` portably.
/// Doc 76 Stage A.1 (2026-05-08): added so file-I/O wrappers don't
/// have to encode platform-specific filenames; covers the common
/// libc handle.  Other names pass through unchanged.
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
    library_cache()
        .lock()
        .unwrap()
        .insert(resolved.to_string(), leaked);
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
    let _ptr = v.as_mut_ptr() as i64;
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

// ---------------------------------------------------------------------------
// Doc 76 Stage 0 (2026-05-08) — nl-ffi 基盤拡張.
//
// `nl-ffi-write-bytes' + `nl-ffi-errno' add the missing pieces needed for
// the Doc 76 elisp-side OS surface migration (= Linux/Darwin/Windows
// unified `nl-ffi-call libc' 経由 syscall family).  Together with the
// existing `nl-ffi-malloc' / `nl-ffi-read-bytes' / `nl-ffi-free' the elisp
// side can now build / poke / read back arbitrary C structs (sockaddr_in,
// pollfd, sigset_t, msghdr+SCM_RIGHTS, etc.) without any per-syscall Rust
// glue.
// ---------------------------------------------------------------------------

/// `(nl-ffi-write-bytes PTR STR)` → t on success.  Copies STR bytes (=
/// raw bytes interpreting STR as Latin-1, matching `nl-ffi-read-bytes'
/// inverse) into the buffer at PTR.
///
/// Safety gate: PTR must be a pointer that came from `nl-ffi-malloc' (=
/// tracked in `alloc_table') and the write must fit in the buffer's
/// recorded length.  This is the same alloc-table gate that
/// `nl-ffi-free' uses; out-of-tracked-set pointers signal `ffi-error'
/// instead of doing a wild write.
pub fn nl_ffi_write_bytes(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return Err(ffi_err(format!(
            "nl-ffi-write-bytes: expected 2 args (ptr str), got {}",
            args.len()
        )));
    }
    let p = coerce_int(&args[0])?;
    if p == 0 {
        return Err(ffi_err("nl-ffi-write-bytes: NULL pointer".into()));
    }
    let bytes_owned = args[1].as_string_owned().ok_or_else(|| {
        ffi_err(format!(
            "nl-ffi-write-bytes: expected string for arg 2, got {:?}",
            args[1]
        ))
    })?;
    let bytes = bytes_owned.as_bytes();
    let alloc_len = *alloc_table().lock().unwrap().get(&p).ok_or_else(|| {
        ffi_err(format!(
            "nl-ffi-write-bytes: pointer {} not from nl-ffi-malloc",
            p
        ))
    })?;
    if bytes.len() > alloc_len {
        return Err(ffi_err(format!(
            "nl-ffi-write-bytes: write of {} bytes exceeds buffer length {}",
            bytes.len(),
            alloc_len
        )));
    }
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), p as *mut u8, bytes.len());
    }
    Ok(Sexp::T)
}

/// `(nl-ffi-read-i32 PTR OFFSET)` → integer (= sign-extended i32 LE
/// at PTR + OFFSET).  Doc 76 Stage A.2/A.3 (2026-05-08): added so
/// `nelisp-os-pipe' / `-fstat' can decode int[2] / struct stat fields
/// from `nl-ffi-malloc' buffers without going through `nl-ffi-read-bytes'
/// (= `String::from_utf8_lossy' munges non-UTF-8 bytes).
///
/// Safety gate: PTR must be from `nl-ffi-malloc' (= alloc_table) and
/// OFFSET + 4 ≤ recorded length.
pub fn nl_ffi_read_i32(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return Err(ffi_err(format!(
            "nl-ffi-read-i32: expected 2 args (ptr offset), got {}",
            args.len()
        )));
    }
    let p = coerce_int(&args[0])?;
    let off = coerce_int(&args[1])?;
    if p == 0 {
        return Err(ffi_err("nl-ffi-read-i32: NULL pointer".into()));
    }
    if off < 0 {
        return Err(ffi_err(format!("nl-ffi-read-i32: negative offset {}", off)));
    }
    let alloc_len = *alloc_table().lock().unwrap().get(&p).ok_or_else(|| {
        ffi_err(format!("nl-ffi-read-i32: pointer {} not from nl-ffi-malloc", p))
    })?;
    if (off as usize) + 4 > alloc_len {
        return Err(ffi_err(format!(
            "nl-ffi-read-i32: read at offset {} exceeds buffer length {}",
            off, alloc_len
        )));
    }
    let addr = (p + off) as *const i32;
    let v = unsafe { std::ptr::read_unaligned(addr) } as i64;
    Ok(Sexp::Int(v))
}

/// `(nl-ffi-read-i64 PTR OFFSET)` → integer (= i64 LE at PTR + OFFSET).
/// Same safety gate as `nl-ffi-read-i32' but 8-byte read.
pub fn nl_ffi_read_i64(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if args.len() != 2 {
        return Err(ffi_err(format!(
            "nl-ffi-read-i64: expected 2 args (ptr offset), got {}",
            args.len()
        )));
    }
    let p = coerce_int(&args[0])?;
    let off = coerce_int(&args[1])?;
    if p == 0 {
        return Err(ffi_err("nl-ffi-read-i64: NULL pointer".into()));
    }
    if off < 0 {
        return Err(ffi_err(format!("nl-ffi-read-i64: negative offset {}", off)));
    }
    let alloc_len = *alloc_table().lock().unwrap().get(&p).ok_or_else(|| {
        ffi_err(format!("nl-ffi-read-i64: pointer {} not from nl-ffi-malloc", p))
    })?;
    if (off as usize) + 8 > alloc_len {
        return Err(ffi_err(format!(
            "nl-ffi-read-i64: read at offset {} exceeds buffer length {}",
            off, alloc_len
        )));
    }
    let addr = (p + off) as *const i64;
    let v = unsafe { std::ptr::read_unaligned(addr) };
    Ok(Sexp::Int(v))
}

/// `(nl-ffi-errno)` → integer errno of the last libc call from the
/// current thread.  Cross-OS thin shim:
///   - Linux/glibc: `*__errno_location()`
///   - macOS / *BSD: `*__error()`
///   - Windows: `*_errno()` (= MSVCRT's CRT errno, distinct from
///     GetLastError; the libc crate exposes this as `__errno`).
///
/// Convention: elisp wrappers call this immediately after a libc
/// function that returned -1 / NULL to retrieve the canonical errno.
/// No second alloc must occur between the libc call and `(nl-ffi-errno)`
/// or the value can be clobbered (= same constraint as C: errno is
/// thread-local but easily overwritten by the next failing call).
pub fn nl_ffi_errno(args: &[Sexp]) -> Result<Sexp, EvalError> {
    if !args.is_empty() {
        return Err(ffi_err(format!(
            "nl-ffi-errno: expected 0 args, got {}",
            args.len()
        )));
    }
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
    let e: i64 = 0; // unsupported platform — return 0 (= "no error")
    Ok(Sexp::Int(e))
}

#[cfg(test)]
mod tests_doc76_stage0 {
    //! Doc 76 Stage 0 unit tests for `nl-ffi-write-bytes' / `nl-ffi-errno'.
    //! Covered: round-trip with `nl-ffi-malloc' + `nl-ffi-read-bytes',
    //! safety gates (NULL / unknown ptr / oversized write), errno read.
    use super::*;

    fn malloc_n(n: i64) -> i64 {
        match nl_ffi_malloc(&[Sexp::Int(n)]).unwrap() {
            Sexp::Int(p) => p,
            other => panic!("nl-ffi-malloc returned {:?}", other),
        }
    }

    fn read_n(p: i64, n: i64) -> String {
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
        // First 5 bytes should be "hello", rest is malloc-zeroed.
        let s = read_n(p, 5);
        assert_eq!(s, "hello");
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn write_bytes_rejects_null_ptr() {
        let r = nl_ffi_write_bytes(&[Sexp::Int(0), Sexp::Str("x".into())]);
        assert!(r.is_err(), "NULL ptr must be rejected");
    }

    #[test]
    fn write_bytes_rejects_unknown_ptr() {
        // 0xDEAD_BEEF is not a tracked alloc.
        let r = nl_ffi_write_bytes(&[Sexp::Int(0xDEAD_BEEF), Sexp::Str("x".into())]);
        assert!(r.is_err(), "unknown ptr must be rejected");
    }

    #[test]
    fn write_bytes_rejects_oversize() {
        let p = malloc_n(4);
        let r = nl_ffi_write_bytes(&[Sexp::Int(p), Sexp::Str("12345".into())]);
        assert!(r.is_err(), "5-byte write into 4-byte buffer must be rejected");
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn write_bytes_empty_str_is_noop() {
        let p = malloc_n(4);
        let r = nl_ffi_write_bytes(&[Sexp::Int(p), Sexp::Str(String::new())]).unwrap();
        assert_eq!(r, Sexp::T);
        // buffer should still be zeroed.
        let s = read_n(p, 4);
        assert_eq!(s.as_bytes(), &[0u8, 0, 0, 0]);
        nl_ffi_free(&[Sexp::Int(p)]).unwrap();
    }

    #[test]
    fn errno_reads_thread_local() {
        // Trigger a known-bad libc call to set errno: open(/nonexistent) → ENOENT (= 2 on Linux).
        // The exact value differs by OS, but on supported platforms it must be > 0
        // after a deliberately-failing libc call.
        #[cfg(any(target_os = "linux", target_os = "macos"))]
        {
            use std::ffi::CString;
            let bad = CString::new("/this-path-must-not-exist-for-test").unwrap();
            let _ = unsafe { libc::open(bad.as_ptr(), libc::O_RDONLY) };
            let r = nl_ffi_errno(&[]).unwrap();
            match r {
                Sexp::Int(e) => assert!(e > 0, "errno must be set after failing open, got {}", e),
                other => panic!("nl-ffi-errno returned {:?}", other),
            }
        }
        // On unsupported platforms, just check the call returns Int (= 0 stub).
        #[cfg(not(any(target_os = "linux", target_os = "macos")))]
        {
            let r = nl_ffi_errno(&[]).unwrap();
            assert!(matches!(r, Sexp::Int(_)));
        }
    }

    #[test]
    fn errno_rejects_args() {
        let r = nl_ffi_errno(&[Sexp::Int(0)]);
        assert!(r.is_err(), "nl-ffi-errno takes 0 args");
    }
}
