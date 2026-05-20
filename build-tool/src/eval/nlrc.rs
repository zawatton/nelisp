pub unsafe fn nlrc_payload_drop<T>(ptr: *mut std::ffi::c_void) { std::ptr::drop_in_place(ptr as *mut T); }
#[macro_export]
macro_rules! nl_ref_common {
    ($ref:ident, $inner:ident, drop_fn = $drop:path) => {
        #[repr(transparent)]
        pub struct $ref { ptr: ::std::ptr::NonNull<$inner>, _marker: ::std::marker::PhantomData<$inner> }
        impl $ref {
            pub fn strong_count(this: &Self) -> usize { unsafe { (*this.ptr.as_ptr()).refcount.load(::std::sync::atomic::Ordering::Acquire) } }
            pub fn ptr_eq(a: &Self, b: &Self) -> bool { a.ptr.as_ptr() == b.ptr.as_ptr() }
        }
        impl ::std::ops::Drop for $ref {
            fn drop(&mut self) { unsafe { $drop(self.ptr.as_ptr() as *mut i64) }; }
        }
        impl ::std::ops::Deref for $ref { type Target = $inner; fn deref(&self) -> &$inner { unsafe { &*self.ptr.as_ptr() } } }
    };
}
#[macro_export]
macro_rules! define_nlbox {
    (inner=$inner:ident, ref_ty=$ref:ident, fields={$($fname:ident:$fty:ty),+},
     clone_fn=$clone_fn:path, drop_fn=$drop_fn:path, layout_asserts={$($la_tt:tt)*}) => {
        use ::std::marker::PhantomData;
        use ::std::ptr::NonNull;
        use ::std::sync::atomic::AtomicUsize;
        #[repr(C)]
        pub struct $inner { $(pub $fname: $fty,)+ pub refcount: AtomicUsize }
        crate::nl_ref_common!($ref, $inner, drop_fn = $drop_fn);
        impl $inner {
            pub(crate) const DROP_FN: unsafe fn(*mut ::std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<$inner>;
        }
        impl $ref {
            pub fn new($($fname: $fty),+) -> $ref { let ptr = NonNull::from(Box::leak(Box::new($inner { $($fname,)+ refcount: AtomicUsize::new(1) }))); $ref { ptr, _marker: PhantomData } }
        }
        impl Clone for $ref {
            fn clone(&self) -> Self { unsafe { $clone_fn(self.ptr.as_ptr() as *mut i64) }; $ref { ptr: self.ptr, _marker: PhantomData } }
        }
        const _: () = { $($la_tt)* };
    };
}
#[macro_export]
macro_rules! nlinner_set {
    ($name:ident, $field:ident, $ty:ty) => {
        pub unsafe fn $name(&self, val: $ty) {
            let p = ::std::ptr::addr_of!(self.$field) as *mut $ty;
            ::std::ptr::drop_in_place(p); ::std::ptr::write(p, val);
        }
    };
}
#[macro_export]
macro_rules! nlinner_with_mut {
    ($name:ident, $field:ident, $ty:ty) => {
        pub unsafe fn $name<R>(&self, f: impl FnOnce(&mut $ty) -> R) -> R {
            f(&mut *(::std::ptr::addr_of!(self.$field) as *mut $ty))
        }
    };
}
macro_rules! drop_inner_extern {
    ($name:ident, $ty:path) => {
        #[no_mangle]
        pub unsafe extern "C" fn $name(box_ptr: *mut i64) -> i64 { <$ty>::DROP_FN(box_ptr as *mut std::ffi::c_void); 1 }
    };
}
drop_inner_extern!(nl_consbox_drop_inner, crate::eval::nlconsbox::NlConsBox);
drop_inner_extern!(nl_vector_drop_inner, crate::eval::nlvector::NlVector);
drop_inner_extern!(nl_cell_drop_inner, crate::eval::nlcell::NlCell);
drop_inner_extern!(nl_record_drop_inner, crate::eval::nlrecord::NlRecord);
drop_inner_extern!(nl_str_drop_inner, crate::eval::nlstr::NlStr);
drop_inner_extern!(nl_boolvector_drop_inner, crate::eval::nlboolvector::NlBoolVector);
drop_inner_extern!(nl_chartable_drop_inner, crate::eval::nlchartable::NlCharTable);
