//! `NlStr` backs `Sexp::MutStr`: `value` at offset 0, refcount trailer after
//! `String`. Clone/Drop dispatch through the elisp kernels.

use std::marker::PhantomData;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;

use std::sync::atomic::AtomicI64;

use crate::eval::sexp::Sexp;

/// Per-process uninterned-symbol counter.  Pointer surfaced to the
/// Phase 47 elisp body via `nl_make_symbol_counter_ptr'.
static MAKE_SYMBOL_COUNTER: AtomicI64 = AtomicI64::new(0);

/// Return `*mut i64' to `MAKE_SYMBOL_COUNTER' for use with the
/// Phase 47 `atomic-fetch-add' grammar op in the elisp body of
/// `nl_jit_make_symbol'.
#[no_mangle]
pub extern "C" fn nl_make_symbol_counter_ptr() -> *mut i64 {
    std::ptr::addr_of!(MAKE_SYMBOL_COUNTER) as *mut i64
}

/// IEEE-754 float body builder.  CONV ∈ {f/F/e/E/g/G}, PREC ≥ 0.
/// Writes unsigned/unpadded body; elisp does sign + padding.
/// Migrated from `build-tool/src/jit/strings.rs' (file deleted).
#[no_mangle]
pub unsafe extern "C" fn nl_jit_format_float(x: f64, conv: u32, prec: i64, out: *mut Sexp) -> i64 {
    let conv_ch = match char::from_u32(conv) {
        Some(c) => c,
        None => return 1,
    };
    if prec < 0 {
        return 1;
    }
    let p = prec as usize;
    let body = match conv_ch {
        'f' | 'F' => format!("{:.*}", p, x),
        'e' => format!("{:.*e}", p, x),
        'E' => format!("{:.*E}", p, x),
        'g' | 'G' => {
            let (f, e) = (format!("{:.*}", p, x), format!("{:.*e}", p, x));
            if f.len() <= e.len() { f } else { e }
        }
        _ => return 1,
    };
    unsafe { *out = Sexp::Str(body) };
    0
}

#[repr(C)]
pub struct NlStr {
    pub value: String,
    pub refcount: AtomicUsize,
}

crate::nl_ref_common!(NlStrRef, NlStr, drop_fn = crate::elisp_cc_spike::nlstr_drop);

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
        Self {
            ptr,
            _marker: PhantomData,
        }
    }

    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }
}

impl Clone for NlStrRef {
    fn clone(&self) -> Self {
        unsafe { crate::elisp_cc_spike::nlstr_clone(self.ptr.as_ptr() as *mut i64) };
        Self {
            ptr: self.ptr,
            _marker: PhantomData,
        }
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

unsafe fn write_slot(slot: *mut Sexp, sexp: Sexp) -> *mut Sexp {
    unsafe { std::ptr::write(slot, sexp) };
    slot
}

unsafe fn mut_str_value_mut<'a>(p: *mut Sexp) -> &'a mut String {
    unsafe { &mut (*((*p).mut_str_box_ptr() as *mut NlStr)).value }
}

/// Phase 47 delegate for `nl_str_is_alphanumeric_at' unicode slow-path.
/// `cp' is a Unicode codepoint (i64); returns 1 if alphanumeric, 0 otherwise.
#[no_mangle]
pub extern "C" fn nl_is_char_alphanumeric(cp: i64) -> i64 {
    char::from_u32(cp as u32).map_or(0, |c| c.is_alphanumeric() as i64)
}

/// Phase 47 delegate: replace codepoint at char-index `idx'; caller guards MutStr+idx.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_set_codepoint_raw(
    arg: *const Sexp,
    idx: i64,
    val_cp: i64,
    out: *mut Sexp,
) -> i64 {
    use crate::jit::{TRAMPOLINE_ERR, TRAMPOLINE_OK};
    let rc = match &*arg {
        Sexp::MutStr(r) => r,
        _ => return TRAMPOLINE_ERR,
    };
    let new_ch = match char::from_u32(val_cp as u32) {
        Some(c) => c,
        None => return TRAMPOLINE_ERR,
    };
    let (i, len) = (idx as usize, rc.value.chars().count());
    if i >= len {
        return TRAMPOLINE_ERR;
    }
    let new_str: String = rc
        .value
        .chars()
        .enumerate()
        .map(|(j, c)| if j == i { new_ch } else { c })
        .collect();
    rc.set_value(new_str);
    *out = Sexp::Int(val_cp);
    TRAMPOLINE_OK
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(mut_str_ptr: *mut Sexp, byte: i64) {
    unsafe { mut_str_value_mut(mut_str_ptr).as_mut_vec() }.push((byte & 0xFF) as u8);
}

#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_codepoint(mut_str_ptr: *mut Sexp, codepoint: i64) {
    let cp_u32 = if !(0..=0x10_FFFF).contains(&codepoint) {
        0xFFFD
    } else {
        codepoint as u32
    };
    let ch = char::from_u32(cp_u32).unwrap_or('\u{FFFD}');
    unsafe { mut_str_value_mut(mut_str_ptr) }.push(ch);
}

#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(slot: *mut Sexp, val: f64) -> *mut Sexp {
    unsafe { write_slot(slot, Sexp::Float(val)) }
}

#[no_mangle]
pub unsafe extern "C" fn nl_str_to_float(bytes_ptr: *const u8, len: i64, slot: *mut Sexp) -> i64 {
    let n = if len < 0 { 0 } else { len as usize };
    let slice = if n == 0 {
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

/// Append the decimal representation of `n` to `*buf` (must be `Sexp::MutStr`).
/// Returns 0 on success, 1 if `*buf` is not a MutStr.
#[no_mangle]
pub unsafe extern "C" fn nl_i64_append_to_mut_str(n: i64, buf: *mut Sexp) -> i64 {
    let s = unsafe { &mut *buf };
    if let Sexp::MutStr(rc) = s {
        unsafe { (*rc.ptr.as_ptr()).value.push_str(&n.to_string()) };
        0
    } else {
        1
    }
}

/// Append the decimal float representation of the f64 whose bit
/// pattern is `bits` to `*buf` (must be `Sexp::MutStr`).
/// Appends ".0" suffix when the formatted string lacks a decimal
/// point / exponent marker, preserving Elisp float-literal semantics.
/// Returns 0 on success, 1 if `*buf` is not a MutStr.
#[no_mangle]
pub unsafe extern "C" fn nl_f64_bits_append_to_mut_str(bits: i64, buf: *mut Sexp) -> i64 {
    let x = f64::from_bits(bits as u64);
    let s = unsafe { &mut *buf };
    if let Sexp::MutStr(rc) = s {
        let formatted = format!("{}", x);
        let val = unsafe { &mut (*rc.ptr.as_ptr()).value };
        val.push_str(&formatted);
        if !formatted.contains('.') && !formatted.contains('e') && !formatted.contains('E')
            && formatted != "inf" && formatted != "-inf" && formatted != "NaN"
        {
            val.push_str(".0");
        }
        0
    } else {
        1
    }
}

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlStr, value) == 0);
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    assert!(size_of::<AtomicUsize>() == 8);
};
