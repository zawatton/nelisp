//! `NlStr` — layout-pinned mutable String box.  `#[repr(C)]' with
//! value @ 0, refcount @ sizeof(String).  Backs `Sexp::MutStr'.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

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
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlStr' and is exclusively owned.
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
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.
    ///
    /// # Safety
    /// Caller must guarantee no other `&String` borrow into `self.value' is live.
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
    /// Same as [`set_value`].  Reentrant calls are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    /// Doc 124 §124.F — refcount +1 dispatched to elisp `nelisp_nlstr_clone'.
    fn clone(&self) -> Self {
        unsafe {
            crate::elisp_cc_spike::nlstr_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlStrRef { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlStrRef {
    /// Doc 124 §124.L — elisp `nlstr_drop' kernel dispatches refcount-- + dealloc.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlstr_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlStrRef {
    type Target = NlStr;

    fn deref(&self) -> &NlStr {
        // SAFETY: `self.ptr' is alive because we hold a handle.
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
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

// ---- Doc 122 §122.A — String/Symbol allocator externs ----
//
// `Sexp::Str' / `Sexp::Symbol' carry their `String' header inline at payload
// offset 8..32; the allocator writes the full 40-byte `Sexp' into the
// caller-owned slot directly.  Pre-init slot to `Sexp::Nil'.

/// Doc 122 §122.A — allocate a fresh `Sexp::Str(String)` into `result_slot`.
///
/// # Safety
/// - `bytes_ptr` must be valid for `len` initialized UTF-8 bytes (or dangling
///   when `len == 0`).
/// - `result_slot` must be writable for one `Sexp' slot (40 bytes), pre-init Nil.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_str(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    let n = len as usize;
    let bytes: Vec<u8> = if n == 0 {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(bytes_ptr, n) };
        slice.to_vec()
    };
    // SAFETY: caller contract guarantees valid UTF-8.
    let s = unsafe { String::from_utf8_unchecked(bytes) };
    let sexp = crate::eval::sexp::Sexp::Str(s);
    // SAFETY: caller-owned slot, pre-init to Nil.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

/// Doc 122 §122.A — allocate a fresh `Sexp::Symbol(String)` into `result_slot`.
///
/// # Safety
/// Identical contract to [`nl_alloc_str`].
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_symbol(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    let n = len as usize;
    let bytes: Vec<u8> = if n == 0 {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(bytes_ptr, n) };
        slice.to_vec()
    };
    // SAFETY: caller contract guarantees valid UTF-8.
    let s = unsafe { String::from_utf8_unchecked(bytes) };
    let sexp = crate::eval::sexp::Sexp::Symbol(s);
    // SAFETY: caller-owned slot.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

// ---- Doc 122 §122.B — Mutable string builder externs ----

/// Doc 122 §122.B — allocate a fresh `Sexp::MutStr(NlStrRef)` with reserved cap.
///
/// # Safety
/// - `result_slot` must be writable for one `Sexp' slot, pre-init Nil.
/// - `cap >= 0` and fits in `usize`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_mut_str(
    cap: i64,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    let n = if cap < 0 { 0 } else { cap as usize };
    let rc = NlStrRef::new(String::with_capacity(n));
    let sexp = crate::eval::sexp::Sexp::MutStr(rc);
    // SAFETY: caller-owned slot pre-init to Nil.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

/// Doc 122 §122.B — append a single byte to a `Sexp::MutStr`'s buffer.
///
/// # Safety
/// - `mut_str_ptr` must point at a live `Sexp::MutStr` with no aliasing borrow.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(
    mut_str_ptr: *mut crate::eval::sexp::Sexp,
    byte: i64,
) {
    let b = (byte & 0xFF) as u8;
    // SAFETY: caller-asserted tag; cast to `*mut' for exclusive mut access.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    unsafe { value_ref.as_mut_vec() }.push(b);
}

/// Doc 122 §122.B — UTF-8 encode `codepoint` and append (1–4 bytes).
///
/// Out-of-range / surrogate codepoints clamp to U+FFFD.
///
/// # Safety
/// Same as [`nl_mut_str_push_byte`].
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_codepoint(
    mut_str_ptr: *mut crate::eval::sexp::Sexp,
    codepoint: i64,
) {
    let cp_u32 = if codepoint < 0 || codepoint > 0x10_FFFF {
        0xFFFD
    } else {
        codepoint as u32
    };
    let ch = char::from_u32(cp_u32).unwrap_or('\u{FFFD}');
    // SAFETY: caller-asserted tag, no aliasing borrow.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    value_ref.push(ch);
}

/// Doc 122 §122.B — current byte length of a `Sexp::MutStr`.
///
/// # Safety
/// `mut_str_ptr` must point at a live `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_len(
    mut_str_ptr: *const crate::eval::sexp::Sexp,
) -> i64 {
    // SAFETY: caller-asserted tag.  Explicit `&String' for autoref lint.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    value_ref.len() as i64
}

/// Doc 122 §122.B — build an immutable `Sexp::Str` from a `Sexp::MutStr`.
///
/// # Safety
/// - `mut_str_ptr`: same as [`nl_mut_str_len`].
/// - `result_slot`: same as [`nl_alloc_mut_str`].
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_finalize(
    mut_str_ptr: *const crate::eval::sexp::Sexp,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    // SAFETY: caller-asserted tag.  Explicit `&String' for autoref lint.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    let cloned = value_ref.clone();
    let sexp = crate::eval::sexp::Sexp::Str(cloned);
    // SAFETY: caller-owned slot.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

// ---- Doc 122 §122.D — UTF-8 helper externs ----

/// Doc 122 §122.D — count UTF-8 codepoints (NOT bytes) in a string-y Sexp.
///
/// # Safety
/// `str_ptr' must point at a live `Sexp::Str' / `Sexp::Symbol' / `Sexp::MutStr'.
#[no_mangle]
pub unsafe extern "C" fn nl_str_char_count(
    str_ptr: *const crate::eval::sexp::Sexp,
) -> i64 {
    // SAFETY: caller-asserted tag.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    let s: &str = match sexp_ref {
        crate::eval::sexp::Sexp::Str(text) => text.as_str(),
        crate::eval::sexp::Sexp::Symbol(text) => text.as_str(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_str(),
        _ => return 0,
    };
    s.chars().count() as i64
}

/// Doc 122 §122.D — decode UTF-8 codepoint at `byte_idx`, write through out-slots.
///
/// Returns `1` on success, `0` on invalid index / boundary / malformed UTF-8.
///
/// # Safety
/// - `str_ptr` same as [`nl_str_char_count`].
/// - `out_codepoint` and `out_byte_width` must be writable for one `i64`.
#[no_mangle]
pub unsafe extern "C" fn nl_str_codepoint_at(
    str_ptr: *const crate::eval::sexp::Sexp,
    byte_idx: i64,
    out_codepoint: *mut i64,
    out_byte_width: *mut i64,
) -> i64 {
    // SAFETY: caller-asserted tag.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    let s: &str = match sexp_ref {
        crate::eval::sexp::Sexp::Str(text) => text.as_str(),
        crate::eval::sexp::Sexp::Symbol(text) => text.as_str(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_str(),
        _ => return 0,
    };
    if byte_idx < 0 {
        return 0;
    }
    let idx = byte_idx as usize;
    let bytes = s.as_bytes();
    if idx >= bytes.len() {
        return 0;
    }
    if !s.is_char_boundary(idx) {
        return 0;
    }
    let ch = match s[idx..].chars().next() {
        Some(c) => c,
        None => return 0,
    };
    let width = ch.len_utf8() as i64;
    // SAFETY: caller-supplied out-slots are non-null + writable.
    unsafe {
        *out_codepoint = ch as i64;
        *out_byte_width = width;
    }
    1
}

/// Doc 122 §122.D — predicate: is the codepoint at `byte_idx` alphanumeric?
///
/// # Safety
/// `str_ptr` same as [`nl_str_char_count`].
#[no_mangle]
pub unsafe extern "C" fn nl_str_is_alphanumeric_at(
    str_ptr: *const crate::eval::sexp::Sexp,
    byte_idx: i64,
) -> i64 {
    // SAFETY: caller-asserted tag.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    let s: &str = match sexp_ref {
        crate::eval::sexp::Sexp::Str(text) => text.as_str(),
        crate::eval::sexp::Sexp::Symbol(text) => text.as_str(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_str(),
        _ => return 0,
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
    let b = bytes[idx];
    if b.is_ascii_alphanumeric() {
        return 1;
    }
    if !s.is_char_boundary(idx) {
        return 0;
    }
    let ch = match s[idx..].chars().next() {
        Some(c) => c,
        None => return 0,
    };
    if ch.is_alphanumeric() { 1 } else { 0 }
}

// ---- Doc 122 §122.H — Outward `Str → *const u8' bytes-pointer op ----

/// Doc 122 §122.H — data-ptr of a string-y Sexp; `null` for other tags.
///
/// # Safety
/// `str_ptr` must point at a live `Sexp` value.  Empty-string yields a
/// dangling-but-aligned non-null ptr (per `String::as_ptr()` contract).
#[no_mangle]
pub unsafe extern "C" fn nl_str_bytes_ptr(
    str_ptr: *const crate::eval::sexp::Sexp,
) -> *const u8 {
    // SAFETY: caller contract.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    match sexp_ref {
        crate::eval::sexp::Sexp::Str(s) => s.as_ptr(),
        crate::eval::sexp::Sexp::Symbol(s) => s.as_ptr(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_ptr(),
        _ => std::ptr::null(),
    }
}

// ---- Doc 122 §122.G — Float allocator + str-to-float helper ----

/// Doc 122 §122.G — write `Sexp::Float(val)` into the caller's slot.
///
/// # Safety
/// `slot` must be writable for one `Sexp' slot, pre-init Nil.
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(
    slot: *mut crate::eval::sexp::Sexp,
    val: f64,
) -> *mut crate::eval::sexp::Sexp {
    let sexp = crate::eval::sexp::Sexp::Float(val);
    // SAFETY: caller-owned slot pre-init Nil.
    unsafe { std::ptr::write(slot, sexp) };
    slot
}

/// Doc 122 §122.G — parse UTF-8 byte range as f64; write `Sexp::Float`.
///
/// Returns 1 on success, 0 on parse failure (slot set to Nil).
///
/// # Safety
/// - `bytes_ptr` valid for `len` UTF-8 bytes when `len > 0`.
/// - `slot` writable for one `Sexp' slot.
#[no_mangle]
pub unsafe extern "C" fn nl_str_to_float(
    bytes_ptr: *const u8,
    len: i64,
    slot: *mut crate::eval::sexp::Sexp,
) -> i64 {
    let n = if len < 0 { 0 } else { len as usize };
    let slice = if n == 0 {
        &[][..]
    } else {
        // SAFETY: caller contract — `bytes_ptr' valid for `n' bytes.
        unsafe { std::slice::from_raw_parts(bytes_ptr, n) }
    };
    // SAFETY: caller contract — valid UTF-8.
    let text = unsafe { std::str::from_utf8_unchecked(slice) };
    match text.parse::<f64>() {
        Ok(f) => {
            let sexp = crate::eval::sexp::Sexp::Float(f);
            // SAFETY: caller-owned slot.
            unsafe { std::ptr::write(slot, sexp) };
            1
        }
        Err(_) => {
            // SAFETY: caller-owned slot.
            unsafe { std::ptr::write(slot, crate::eval::sexp::Sexp::Nil) };
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
