use std::sync::atomic::AtomicI64;

use crate::eval::sexp::Sexp;

static MAKE_SYMBOL_COUNTER: AtomicI64 = AtomicI64::new(0);

#[no_mangle]
pub extern "C" fn nl_make_symbol_counter_ptr() -> *mut i64 {
    std::ptr::addr_of!(MAKE_SYMBOL_COUNTER) as *mut i64
}

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

crate::define_nlbox!(
    inner          = NlStr,
    ref_ty         = NlStrRef,
    fields         = { value: String },
    clone_fn       = crate::elisp_cc_spike::nlstr_clone,
    drop_fn        = crate::elisp_cc_spike::nlstr_drop,
    layout_asserts = {
        use ::std::mem::{offset_of, size_of};
        assert!(offset_of!(NlStr, value) == 0);
        assert!(offset_of!(NlStr, refcount) == size_of::<String>());
        assert!(size_of::<AtomicUsize>() == 8);
    }
);

impl NlStrRef {
    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
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

#[no_mangle]
pub extern "C" fn nl_is_char_alphanumeric(cp: i64) -> i64 {
    char::from_u32(cp as u32).map_or(0, |c| c.is_alphanumeric() as i64)
}

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

