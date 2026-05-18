//! `#[no_mangle] extern "C"` surface for `Sexp::Str` / `Sexp::Symbol` /
//! `Sexp::MutStr` / `Sexp::Float` allocation + UTF-8 helpers.
//!
//! Extracted from `eval/nlstr.rs` to keep that file focused on the
//! `NlStr` / `NlStrRef` layout + handle types.  All functions here are
//! called from elisp-compiled grammar ops via `cc_wrap!` in `lib.rs`.
//! See Doc 122 §122.A / §122.B / §122.D / §122.G / §122.H for the
//! per-op contract.
//!
//! # Safety (file-wide for `unsafe extern "C"` ops)
//!
//! Unless overridden on a per-fn doc comment:
//! - `*const Sexp` / `*mut Sexp` args point at live `Sexp` values; for
//!   `MutStr` ops, no aliasing borrow into the inner `String` is live.
//! - `result_slot` / `slot` args are writable for one `Sexp` slot and
//!   pre-initialised to `Sexp::Nil`.
//! - `bytes_ptr` (with `len > 0`) is valid for `len` UTF-8 bytes;
//!   `len <= 0` is clamped to 0 (dangling `bytes_ptr` permitted).
//! - Out-pointer args (`out_codepoint`, `out_byte_width`) are writable
//!   for one `i64`.

use crate::eval::nlstr::{NlStr, NlStrRef};
use crate::eval::sexp::Sexp;

// ---- Doc 122 helpers (file-internal) ----

/// Borrow the `&str` payload from any string-y `Sexp`, else `None`.
fn sexp_as_str(s: &Sexp) -> Option<&str> {
    match s {
        Sexp::Str(s) | Sexp::Symbol(s) => Some(s.as_str()),
        Sexp::MutStr(rc) => Some(rc.value.as_str()),
        _ => None,
    }
}

/// Build a `String` from `(bytes_ptr, len)`, clamping `len <= 0` to empty.
unsafe fn build_string(bytes_ptr: *const u8, len: i64) -> String {
    let n = if len <= 0 { 0 } else { len as usize };
    let bytes = if n == 0 {
        Vec::new()
    } else {
        unsafe { std::slice::from_raw_parts(bytes_ptr, n) }.to_vec()
    };
    unsafe { String::from_utf8_unchecked(bytes) }
}

/// Write `sexp` into caller-owned `slot` and return it.
unsafe fn write_slot(slot: *mut Sexp, sexp: Sexp) -> *mut Sexp {
    unsafe { std::ptr::write(slot, sexp) };
    slot
}

/// Borrow `&mut String` from a `Sexp::MutStr`.
unsafe fn mut_str_value_mut<'a>(p: *mut Sexp) -> &'a mut String {
    let nlstr_ptr = unsafe { (*p).mut_str_box_ptr() } as *mut NlStr;
    unsafe { &mut (*nlstr_ptr).value }
}

/// Borrow `&String` from a `Sexp::MutStr`.
unsafe fn mut_str_value<'a>(p: *const Sexp) -> &'a String {
    let nlstr_ptr = unsafe { (*p).mut_str_box_ptr() };
    unsafe { &(*nlstr_ptr).value }
}

// ---- Doc 122 §122.A — Str / Symbol allocator externs ----

/// Doc 122 §122.A — allocate a fresh `Sexp::Str(String)` into `result_slot`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_str(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Str(build_string(bytes_ptr, len))) }
}

/// Doc 122 §122.A — `Sexp::Symbol` variant of [`nl_alloc_str`].
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_symbol(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Symbol(build_string(bytes_ptr, len))) }
}

// ---- Doc 122 §122.B — Mutable string builder externs ----

/// Doc 122 §122.B — allocate a fresh `Sexp::MutStr(NlStrRef)` with reserved cap.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_mut_str(cap: i64, result_slot: *mut Sexp) -> *mut Sexp {
    let n = if cap < 0 { 0 } else { cap as usize };
    unsafe { write_slot(result_slot, Sexp::MutStr(NlStrRef::new(String::with_capacity(n)))) }
}

/// Doc 122 §122.B — append a single byte to a `Sexp::MutStr` buffer.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(mut_str_ptr: *mut Sexp, byte: i64) {
    unsafe { mut_str_value_mut(mut_str_ptr).as_mut_vec() }.push((byte & 0xFF) as u8);
}

/// Doc 122 §122.B — UTF-8 encode `codepoint` and append (1–4 bytes).
/// Out-of-range / surrogate codepoints clamp to U+FFFD.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_codepoint(mut_str_ptr: *mut Sexp, codepoint: i64) {
    let cp_u32 = if !(0..=0x10_FFFF).contains(&codepoint) { 0xFFFD } else { codepoint as u32 };
    let ch = char::from_u32(cp_u32).unwrap_or('\u{FFFD}');
    unsafe { mut_str_value_mut(mut_str_ptr) }.push(ch);
}

/// Doc 122 §122.B — current byte length of a `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_len(mut_str_ptr: *const Sexp) -> i64 {
    unsafe { mut_str_value(mut_str_ptr) }.len() as i64
}

/// Doc 122 §122.B — build an immutable `Sexp::Str` from a `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_finalize(
    mut_str_ptr: *const Sexp,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    let cloned = unsafe { mut_str_value(mut_str_ptr) }.clone();
    unsafe { write_slot(result_slot, Sexp::Str(cloned)) }
}

// ---- Doc 122 §122.D — UTF-8 helper externs ----

/// Doc 122 §122.D — count UTF-8 codepoints (NOT bytes) in a string-y Sexp.
#[no_mangle]
pub unsafe extern "C" fn nl_str_char_count(str_ptr: *const Sexp) -> i64 {
    sexp_as_str(unsafe { &*str_ptr }).map_or(0, |s| s.chars().count() as i64)
}

/// Doc 122 §122.D — decode UTF-8 codepoint at `byte_idx`, write through out-slots.
/// Returns `1` on success, `0` on invalid index / boundary / malformed UTF-8.
#[no_mangle]
pub unsafe extern "C" fn nl_str_codepoint_at(
    str_ptr: *const Sexp,
    byte_idx: i64,
    out_codepoint: *mut i64,
    out_byte_width: *mut i64,
) -> i64 {
    let Some(s) = sexp_as_str(unsafe { &*str_ptr }) else { return 0 };
    if byte_idx < 0 {
        return 0;
    }
    let idx = byte_idx as usize;
    if idx >= s.len() || !s.is_char_boundary(idx) {
        return 0;
    }
    let Some(ch) = s[idx..].chars().next() else { return 0 };
    unsafe {
        *out_codepoint = ch as i64;
        *out_byte_width = ch.len_utf8() as i64;
    }
    1
}

/// Doc 122 §122.D — predicate: is the codepoint at `byte_idx` alphanumeric?
#[no_mangle]
pub unsafe extern "C" fn nl_str_is_alphanumeric_at(str_ptr: *const Sexp, byte_idx: i64) -> i64 {
    let Some(s) = sexp_as_str(unsafe { &*str_ptr }) else { return 0 };
    if byte_idx < 0 {
        return 0;
    }
    let idx = byte_idx as usize;
    let bytes = s.as_bytes();
    if idx >= bytes.len() {
        return 0;
    }
    if bytes[idx].is_ascii_alphanumeric() {
        return 1;
    }
    if !s.is_char_boundary(idx) {
        return 0;
    }
    match s[idx..].chars().next() {
        Some(c) if c.is_alphanumeric() => 1,
        _ => 0,
    }
}

// ---- Doc 122 §122.H — Outward `Str → *const u8` bytes-pointer op ----

/// Doc 122 §122.H — data-ptr of a string-y Sexp; `null` for other tags.
/// Empty-string yields a dangling-but-aligned non-null ptr (per
/// `String::as_ptr()` contract).
#[no_mangle]
pub unsafe extern "C" fn nl_str_bytes_ptr(str_ptr: *const Sexp) -> *const u8 {
    match unsafe { &*str_ptr } {
        Sexp::Str(s) | Sexp::Symbol(s) => s.as_ptr(),
        Sexp::MutStr(rc) => rc.value.as_ptr(),
        _ => std::ptr::null(),
    }
}

// ---- Doc 122 §122.G — Float allocator + str-to-float helper ----

/// Doc 122 §122.G — write `Sexp::Float(val)` into the caller's slot.
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(slot: *mut Sexp, val: f64) -> *mut Sexp {
    unsafe { write_slot(slot, Sexp::Float(val)) }
}

/// Doc 122 §122.G — parse UTF-8 byte range as f64; write `Sexp::Float` (or Nil).
/// Returns 1 on success, 0 on parse failure.
#[no_mangle]
pub unsafe extern "C" fn nl_str_to_float(
    bytes_ptr: *const u8,
    len: i64,
    slot: *mut Sexp,
) -> i64 {
    let n = if len < 0 { 0 } else { len as usize };
    let slice: &[u8] = if n == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(bytes_ptr, n) }
    };
    let text = unsafe { std::str::from_utf8_unchecked(slice) };
    match text.parse::<f64>() {
        Ok(f) => {
            unsafe { write_slot(slot, Sexp::Float(f)) };
            1
        }
        Err(_) => {
            unsafe { write_slot(slot, Sexp::Nil) };
            0
        }
    }
}
