//! Per-type inner-drop ABI externs — each `nl_<type>_drop_inner' wraps
//! `drop_in_place::<T>' so elisp Drop kernels can call it via extern-call
//! before `dealloc-bytes'.

pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) {
    std::ptr::drop_in_place(ptr as *mut T);
}

macro_rules! drop_inner_extern {
    ($name:ident, $ty:path) => {
        #[no_mangle]
        pub unsafe extern "C" fn $name(box_ptr: *mut i64) -> i64 {
            <$ty>::DROP_FN(box_ptr as *mut std::ffi::c_void);
            1
        }
    };
}

drop_inner_extern!(nl_consbox_drop_inner,    crate::eval::nlconsbox::NlConsBox);
drop_inner_extern!(nl_vector_drop_inner,     crate::eval::nlvector::NlVector);
drop_inner_extern!(nl_cell_drop_inner,       crate::eval::nlcell::NlCell);
drop_inner_extern!(nl_record_drop_inner,     crate::eval::nlrecord::NlRecord);
drop_inner_extern!(nl_str_drop_inner,        crate::eval::nlstr::NlStr);
drop_inner_extern!(nl_boolvector_drop_inner, crate::eval::nlboolvector::NlBoolVector);
drop_inner_extern!(nl_chartable_drop_inner,  crate::eval::nlchartable::NlCharTable);
