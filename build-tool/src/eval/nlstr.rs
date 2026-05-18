//! `NlStr` — layout-pinned mutable String box.  `#[repr(C)]` with
//! value @ 0, refcount @ size_of::<String>().  Backs `Sexp::MutStr`.
//!
//! Method bodies for Clone/Drop are in `lisp/nelisp-cc-nlstr-{clone,drop}.el`
//! and dispatched through `crate::elisp_cc_spike::nlstr_{clone,drop}`.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::eval::sexp::Sexp;

#[repr(C)]
pub struct NlStr {
    pub value: String,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlStrRef {
    ptr: NonNull<NlStr>,
    _marker: PhantomData<NlStr>,
}

impl NlStr {
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) =
        crate::eval::nlrc::nlrc_payload_drop::<NlStr>;
}

impl NlStrRef {
    /// Allocate a fresh [`NlStr`] on the heap with `refcount = 1`.
    pub fn new(value: String) -> NlStrRef {
        let layout = Layout::new::<NlStr>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlStr;
        let ptr = NonNull::new(raw).unwrap_or_else(|| alloc::handle_alloc_error(layout));
        // SAFETY: freshly allocated, exclusively owned.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlStrRef { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: handle keeps the box alive.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.
    ///
    /// # Safety
    /// Caller must guarantee no other `&String` borrow into `self.value` is live.
    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.
    ///
    /// # Safety
    /// Same as [`set_value`]; reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    /// Doc 124 §124.F — refcount +1 via elisp `nelisp_nlstr_clone`.
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlstr_clone(self.ptr.as_ptr() as *mut i64) };
        NlStrRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlStrRef {
    /// Doc 124 §124.L — refcount-- + dealloc via elisp `nlstr_drop`.
    fn drop(&mut self) {
        unsafe { crate::elisp_cc_spike::nlstr_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlStrRef {
    type Target = NlStr;

    fn deref(&self) -> &NlStr {
        // SAFETY: handle keeps the box alive.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlStrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MutStr").field(&self.value).finish()
    }
}

impl PartialEq for NlStrRef {
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || self.value == other.value
    }
}

// ---- Doc 122 §122.A / §122.B / §122.D / §122.G / §122.H externs ----
//
// `Sexp::Str` / `Sexp::Symbol` carry their `String` header inline in the
// 40-byte Sexp; `Sexp::MutStr` carries an `NlStrRef` (pointer indirection).
// All `nl_*` externs below are called from elisp-compiled grammar ops via
// `cc_wrap!` in `lib.rs`.

/// Borrow the `&str` payload from any string-y `Sexp`, or `None` for other tags.
///
/// # Safety
/// `sexp_ref` must be a live `Sexp`.
fn sexp_as_str(sexp_ref: &Sexp) -> Option<&str> {
    match sexp_ref {
        Sexp::Str(s) | Sexp::Symbol(s) => Some(s.as_str()),
        Sexp::MutStr(rc) => Some(rc.value.as_str()),
        _ => None,
    }
}

/// Helper: build a `String` from `(bytes_ptr, len)`, clamping `len < 0` to 0.
///
/// # Safety
/// Caller contract: `bytes_ptr` valid for `len` UTF-8 bytes when `len > 0`.
unsafe fn build_string(bytes_ptr: *const u8, len: i64) -> String {
    let n = if len <= 0 { 0 } else { len as usize };
    let bytes: Vec<u8> = if n == 0 {
        Vec::new()
    } else {
        unsafe { std::slice::from_raw_parts(bytes_ptr, n) }.to_vec()
    };
    // SAFETY: caller contract guarantees valid UTF-8.
    unsafe { String::from_utf8_unchecked(bytes) }
}

/// Write `sexp` into `result_slot` (caller-owned, pre-init Nil) and return it.
///
/// # Safety
/// `result_slot` must be writable for one `Sexp` slot.
unsafe fn write_slot(result_slot: *mut Sexp, sexp: Sexp) -> *mut Sexp {
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

/// Doc 122 §122.A — allocate `Sexp::Str` (`as_symbol=0`) or `Sexp::Symbol` (`as_symbol=1`).
///
/// # Safety
/// `bytes_ptr` valid for `len` UTF-8 bytes (or dangling when `len == 0`);
/// `result_slot` writable for one `Sexp` slot, pre-init Nil.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_str(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Str(build_string(bytes_ptr, len))) }
}

/// Doc 122 §122.A — `Sexp::Symbol` variant of [`nl_alloc_str`].
///
/// # Safety
/// Identical contract to [`nl_alloc_str`].
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_symbol(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Symbol(build_string(bytes_ptr, len))) }
}

/// Doc 122 §122.B — allocate a fresh `Sexp::MutStr(NlStrRef)` with reserved cap.
///
/// # Safety
/// `result_slot` writable for one `Sexp` slot; `cap >= 0` fits in `usize`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_mut_str(cap: i64, result_slot: *mut Sexp) -> *mut Sexp {
    let n = if cap < 0 { 0 } else { cap as usize };
    let rc = NlStrRef::new(String::with_capacity(n));
    unsafe { write_slot(result_slot, Sexp::MutStr(rc)) }
}

/// Doc 122 §122.B — append a single byte to a `Sexp::MutStr` buffer.
///
/// # Safety
/// `mut_str_ptr` points at a live `Sexp::MutStr` with no aliasing borrow.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(mut_str_ptr: *mut Sexp, byte: i64) {
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    unsafe { value_ref.as_mut_vec() }.push((byte & 0xFF) as u8);
}

/// Doc 122 §122.B — UTF-8 encode `codepoint` and append (1–4 bytes).
/// Out-of-range / surrogate codepoints clamp to U+FFFD.
///
/// # Safety
/// Same as [`nl_mut_str_push_byte`].
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_codepoint(mut_str_ptr: *mut Sexp, codepoint: i64) {
    let cp_u32 = if !(0..=0x10_FFFF).contains(&codepoint) { 0xFFFD } else { codepoint as u32 };
    let ch = char::from_u32(cp_u32).unwrap_or('\u{FFFD}');
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    value_ref.push(ch);
}

/// Doc 122 §122.B — current byte length of a `Sexp::MutStr`.
///
/// # Safety
/// `mut_str_ptr` points at a live `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_len(mut_str_ptr: *const Sexp) -> i64 {
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    value_ref.len() as i64
}

/// Doc 122 §122.B — build an immutable `Sexp::Str` from a `Sexp::MutStr`.
///
/// # Safety
/// `mut_str_ptr` / `result_slot`: same as [`nl_mut_str_len`] / [`nl_alloc_mut_str`].
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_finalize(
    mut_str_ptr: *const Sexp,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    unsafe { write_slot(result_slot, Sexp::Str(value_ref.clone())) }
}

/// Doc 122 §122.D — count UTF-8 codepoints (NOT bytes) in a string-y Sexp.
///
/// # Safety
/// `str_ptr` points at a live `Sexp::Str` / `Sexp::Symbol` / `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_str_char_count(str_ptr: *const Sexp) -> i64 {
    match sexp_as_str(unsafe { &*str_ptr }) {
        Some(s) => s.chars().count() as i64,
        None => 0,
    }
}

/// Doc 122 §122.D — decode UTF-8 codepoint at `byte_idx`, write through out-slots.
/// Returns `1` on success, `0` on invalid index / boundary / malformed UTF-8.
///
/// # Safety
/// `str_ptr` same as [`nl_str_char_count`]; out-slots writable for one `i64`.
#[no_mangle]
pub unsafe extern "C" fn nl_str_codepoint_at(
    str_ptr: *const Sexp,
    byte_idx: i64,
    out_codepoint: *mut i64,
    out_byte_width: *mut i64,
) -> i64 {
    let s = match sexp_as_str(unsafe { &*str_ptr }) {
        Some(s) => s,
        None => return 0,
    };
    if byte_idx < 0 {
        return 0;
    }
    let idx = byte_idx as usize;
    if idx >= s.len() || !s.is_char_boundary(idx) {
        return 0;
    }
    let ch = match s[idx..].chars().next() {
        Some(c) => c,
        None => return 0,
    };
    // SAFETY: caller-supplied out-slots are non-null + writable.
    unsafe {
        *out_codepoint = ch as i64;
        *out_byte_width = ch.len_utf8() as i64;
    }
    1
}

/// Doc 122 §122.D — predicate: is the codepoint at `byte_idx` alphanumeric?
///
/// # Safety
/// `str_ptr` same as [`nl_str_char_count`].
#[no_mangle]
pub unsafe extern "C" fn nl_str_is_alphanumeric_at(str_ptr: *const Sexp, byte_idx: i64) -> i64 {
    let s = match sexp_as_str(unsafe { &*str_ptr }) {
        Some(s) => s,
        None => return 0,
    };
    if byte_idx < 0 {
        return 0;
    }
    let idx = byte_idx as usize;
    let bytes = s.as_bytes();
    if idx >= bytes.len() {
        return 0;
    }
    // ASCII fast path.
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

/// Doc 122 §122.H — data-ptr of a string-y Sexp; `null` for other tags.
///
/// # Safety
/// `str_ptr` points at a live `Sexp`.  Empty-string yields a dangling-but-aligned
/// non-null ptr (per `String::as_ptr()` contract).
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
///
/// # Safety
/// `slot` writable for one `Sexp` slot, pre-init Nil.
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(slot: *mut Sexp, val: f64) -> *mut Sexp {
    unsafe { write_slot(slot, Sexp::Float(val)) }
}

/// Doc 122 §122.G — parse UTF-8 byte range as f64; write `Sexp::Float` (or Nil).
/// Returns 1 on success, 0 on parse failure.
///
/// # Safety
/// `bytes_ptr` valid for `len` UTF-8 bytes when `len > 0`; `slot` writable.
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
        // SAFETY: caller contract — valid for `n` bytes.
        unsafe { std::slice::from_raw_parts(bytes_ptr, n) }
    };
    // SAFETY: caller contract — valid UTF-8.
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

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlStr, value) == 0);
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    assert!(size_of::<AtomicUsize>() == 8);
};
