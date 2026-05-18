//! `NlCharTable` — layout-pinned char-table cell.  `#[repr(C)]' with
//! inner @ 0 (CharTableInner: subtype/default_val/entries/parent/extra)
//! and refcount @ sizeof(CharTableInner).  Backs `Sexp::CharTable'.
//! `parent: Option<NlCharTableRef>' is a self-reference; refcount
//! cascade handles dropping naturally (no cycle API).

use crate::eval::sexp::CharTableInner;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct NlCharTable {
    pub inner: CharTableInner,
    pub refcount: AtomicUsize,
}

#[repr(transparent)]
pub struct NlCharTableRef {
    ptr: NonNull<NlCharTable>,
    _marker: PhantomData<NlCharTable>,
}

impl NlCharTable { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlCharTable>; } // Doc 79 v4 C.4-atomic
impl NlCharTableRef {
    /// Allocate a fresh [`NlCharTable`] on the heap with `refcount = 1'.
    pub fn new(inner: CharTableInner) -> NlCharTableRef {
        let layout = Layout::new::<NlCharTable>();
        let raw = unsafe { alloc::alloc(layout) } as *mut NlCharTable;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlCharTable' and is
        // exclusively owned here.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).inner), inner);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlCharTableRef {
            ptr,
            _marker: PhantomData,
        }
    }

    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// In-place mutation closure for the full inner state.
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&CharTableInner` borrow into
    /// `self.inner' is live for the duration of the closure.
    /// Reentrant calls are UB.  Phase A.2.1 setcar discipline applies.
    pub unsafe fn with_inner_mut<R>(&self, f: impl FnOnce(&mut CharTableInner) -> R) -> R {
        let inner_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).inner);
        unsafe { f(&mut *inner_ptr) }
    }
}

impl Clone for NlCharTableRef {
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlCharTableRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlCharTableRef {
    /// Doc 124 §124.L+ — dispatch through the pure-elisp
    /// `nlchartable_drop' kernel.  Runs `atomic-fetch-add(-1)' then,
    /// on pre-sub == 1, calls `nl_chartable_drop_inner' (= `drop_in_place
    /// ::<NlCharTable>') + `dealloc-bytes(128, 8)'.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlchartable_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlCharTableRef {
    type Target = NlCharTable;

    fn deref(&self) -> &NlCharTable {
        // SAFETY: see `NlVectorRef::deref'.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlCharTableRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CharTable")
            .field("inner", &self.inner)
            .finish()
    }
}

impl PartialEq for NlCharTableRef {
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.inner == other.inner
    }
}

// ---- Compile-time layout assertions ----

const _: () = {
    use std::mem::{offset_of, size_of};
    assert!(offset_of!(NlCharTable, inner) == 0);
    assert!(offset_of!(NlCharTable, refcount) == size_of::<CharTableInner>());
    assert!(size_of::<AtomicUsize>() == 8);
};

