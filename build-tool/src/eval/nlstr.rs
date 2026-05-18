//! `NlStr` backs `Sexp::MutStr`: `value` at offset 0, refcount trailer after
//! `String`. Clone/Drop dispatch through the elisp kernels.

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
    pub fn new(value: String) -> NlStrRef {
        let ptr = NonNull::from(Box::leak(Box::new(NlStr {
            value,
            refcount: AtomicUsize::new(1),
        })));
        Self { ptr, _marker: PhantomData }
    }

    pub fn strong_count(this: &Self) -> usize {
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlstr_clone(self.ptr.as_ptr() as *mut i64) };
        Self { ptr: self.ptr, _marker: PhantomData }
    }
}

impl Drop for NlStrRef {
    fn drop(&mut self) {
        unsafe { crate::elisp_cc_spike::nlstr_drop(self.ptr.as_ptr() as *mut i64) };
    }
}

impl Deref for NlStrRef {
    type Target = NlStr;
    fn deref(&self) -> &NlStr {
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

fn sexp_as_str(s: &Sexp) -> Option<&str> {
    match s {
        Sexp::Str(s) | Sexp::Symbol(s) => Some(s.as_str()),
        Sexp::MutStr(rc) => Some(rc.value.as_str()),
        _ => None,
    }
}

unsafe fn build_string(bytes_ptr: *const u8, len: i64) -> String {
    let n = if len <= 0 { 0 } else { len as usize };
    let slice = if n == 0 { &[] } else { unsafe { std::slice::from_raw_parts(bytes_ptr, n) } };
    unsafe { String::from_utf8_unchecked(slice.to_vec()) }
}

unsafe fn write_slot(slot: *mut Sexp, sexp: Sexp) -> *mut Sexp {
    unsafe { std::ptr::write(slot, sexp) };
    slot
}

unsafe fn mut_str_value_mut<'a>(p: *mut Sexp) -> &'a mut String {
    unsafe { &mut (*((*p).mut_str_box_ptr() as *mut NlStr)).value }
}

unsafe fn mut_str_value<'a>(p: *const Sexp) -> &'a String {
    unsafe { &(*(*p).mut_str_box_ptr()).value }
}

#[no_mangle]
pub unsafe extern "C" fn nl_alloc_str(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Str(build_string(bytes_ptr, len))) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_alloc_symbol(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    unsafe { write_slot(result_slot, Sexp::Symbol(build_string(bytes_ptr, len))) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_alloc_mut_str(cap: i64, result_slot: *mut Sexp) -> *mut Sexp {
    let n = if cap < 0 { 0 } else { cap as usize };
    unsafe { write_slot(result_slot, Sexp::MutStr(NlStrRef::new(String::with_capacity(n)))) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(mut_str_ptr: *mut Sexp, byte: i64) {
    unsafe { mut_str_value_mut(mut_str_ptr).as_mut_vec() }.push((byte & 0xFF) as u8);
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_codepoint(mut_str_ptr: *mut Sexp, codepoint: i64) {
    let cp_u32 = if !(0..=0x10_FFFF).contains(&codepoint) { 0xFFFD } else { codepoint as u32 };
    let ch = char::from_u32(cp_u32).unwrap_or('\u{FFFD}');
    unsafe { mut_str_value_mut(mut_str_ptr) }.push(ch);
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_len(mut_str_ptr: *const Sexp) -> i64 {
    unsafe { mut_str_value(mut_str_ptr) }.len() as i64
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_finalize(
    mut_str_ptr: *const Sexp,
    result_slot: *mut Sexp,
) -> *mut Sexp {
    let cloned = unsafe { mut_str_value(mut_str_ptr) }.clone();
    unsafe { write_slot(result_slot, Sexp::Str(cloned)) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_str_char_count(str_ptr: *const Sexp) -> i64 {
    sexp_as_str(unsafe { &*str_ptr }).map_or(0, |s| s.chars().count() as i64)
}

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

#[no_mangle]
pub unsafe extern "C" fn nl_str_bytes_ptr(str_ptr: *const Sexp) -> *const u8 {
    match unsafe { &*str_ptr } {
        Sexp::Str(s) | Sexp::Symbol(s) => s.as_ptr(),
        Sexp::MutStr(rc) => rc.value.as_ptr(),
        _ => std::ptr::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(slot: *mut Sexp, val: f64) -> *mut Sexp {
    unsafe { write_slot(slot, Sexp::Float(val)) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_str_to_float(bytes_ptr: *const u8, len: i64, slot: *mut Sexp) -> i64 {
    let n = if len < 0 { 0 } else { len as usize };
    let slice = if n == 0 { &[] } else { unsafe { std::slice::from_raw_parts(bytes_ptr, n) } };
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

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlStr, value) == 0);
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    assert!(size_of::<AtomicUsize>() == 8);
};
