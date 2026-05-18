//! `NlConsBox` — layout-pinned cons cell.  `#[repr(C)]' with car / cdr
//! at byte offsets 0 / sizeof(Sexp) and refcount trailer at 2*sizeof(Sexp).
//! Phase 47 + elisp `nl-cons-*' / `nl-rc-*' primitives reach the fields
//! by direct offset arithmetic — no Rust runtime consultation.
//!
//! Not `Send` / `Sync` (eventual cross-thread access reserved for a
//! future review).

use crate::eval::sexp::Sexp;
use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned cons cell.  `#[repr(C)]' guarantees car @ 0,
/// cdr @ sizeof(Sexp), refcount @ 2*sizeof(Sexp) — read by Phase 47
/// and elisp `nl-rc-*' primitives via raw offset arithmetic.
#[repr(C)]
pub struct NlConsBox {
    pub car: Sexp,
    pub cdr: Sexp,
    pub refcount: AtomicUsize,
}

impl NlConsBox {
    /// Doc 79 v4 Stage C.4-atomic in-place destructor (= `NLRC_DROP_TABLE`).
    pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlConsBox>;

    /// Mutate `car' in place through a raw `&NlConsBox' borrow.  Used by
    /// the Phase A.5.2 cons trampolines after they resolve `Sexp::Cons'
    /// to `*const NlConsBox' via [`Sexp::cons_box_ptr`].  See
    /// [`NlConsBoxRef::set_car`] for the higher-level API + safety
    /// contract — they share the same drop-then-write semantics.
    ///
    /// # Safety
    ///
    /// Same contract as [`NlConsBoxRef::set_car`]: caller must guarantee
    /// no other `&Sexp' borrow into `self.car' is live for the duration
    /// of the call.
    #[inline]
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of!(self.car) as *mut Sexp;
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr' in place.  See [`NlConsBox::set_car`].
    ///
    /// # Safety
    ///
    /// Same contract as [`NlConsBox::set_car`].
    #[inline]
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of!(self.cdr) as *mut Sexp;
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }
}

/// Doc 101 §101.D — allocator helper for Phase 47-compiled elisp.
/// Returns a freshly-allocated NlConsBox with car=Nil, cdr=Nil,
/// refcount=1.  Caller is responsible for overwriting car/cdr.
///
/// # Safety
/// Caller must wrap the returned pointer into a `Sexp::Cons(_)` whose
/// `NlConsBoxRef::drop` decrements the refcount (standard ownership
/// transfer), or call into the standard Rust drop path.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_consbox() -> *mut NlConsBox {
    let boxed = Box::new(NlConsBox {
        car: Sexp::Nil,
        cdr: Sexp::Nil,
        refcount: AtomicUsize::new(1),
    });
    Box::into_raw(boxed)
}

/// Doc 101 §101.D — C-callable wrapper around [`NlConsBox::set_car`].
///
/// # Safety
/// `box_ptr` must point at a live `NlConsBox`; `val` must point at an
/// initialized `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_car(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_car((*val).clone());
}

/// Doc 101 §101.D — C-callable wrapper around [`NlConsBox::set_cdr`].
///
/// # Safety
/// `box_ptr` must point at a live `NlConsBox`; `val` must point at an
/// initialized `Sexp`.
#[no_mangle]
pub unsafe extern "C" fn nl_consbox_set_cdr(box_ptr: *mut NlConsBox, val: *const Sexp) {
    (*box_ptr).set_cdr((*val).clone());
}

/// Refcounted handle to an [`NlConsBox`].  API parity with
/// [`NlRc<T>`](super::nlrc::NlRc): `new` / `Clone` / `Drop` / `Deref`
/// (returns `&NlConsBox`).
///
/// The `NonNull<NlConsBox>' inner gives niche optimization
/// (= `Option<NlConsBoxRef>' is the same size as `NlConsBoxRef') and
/// rules out null-ptr UB by construction.
///
/// `#[repr(transparent)]' so the on-disk layout matches `NonNull<NlConsBox>'
/// — load-bearing for `Sexp::cons_box_ptr', which reads the box pointer
/// at offset `SEXP_PAYLOAD_OFFSET' of the outer `Sexp' enum.
#[repr(transparent)]
pub struct NlConsBoxRef {
    ptr: NonNull<NlConsBox>,
    _marker: PhantomData<NlConsBox>,
}

impl NlConsBoxRef {
    /// Allocate a fresh [`NlConsBox`] on the heap with `refcount = 1`
    /// and return the unique handle.  The supplied `car` / `cdr` are
    /// moved into the box.
    ///
    /// Panics on allocation failure (= matches `NlRc::new`, which
    /// calls `alloc::handle_alloc_error' internally).
    pub fn new(car: Sexp, cdr: Sexp) -> NlConsBoxRef {
        let layout = Layout::new::<NlConsBox>();
        // SAFETY: `Layout::new::<NlConsBox>()' is non-zero-sized
        // because the struct contains at least the 8-byte refcount
        // (and two `Sexp' fields besides).
        let raw = unsafe { alloc::alloc(layout) } as *mut NlConsBox;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlConsBox' and is
        // exclusively owned here.  We initialize all three fields
        // before anyone else can observe the box.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).car), car);
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).cdr), cdr);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlConsBoxRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  `Acquire' ordering
    /// pairs with the `Release' fetch_sub in Drop.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying box*.  Two clones of the
    /// same [`NlConsBoxRef::new`] invocation are pointer-equal; two
    /// distinct allocations are not.
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Raw pointer to the box, for ffi shims (elisp `nl-cons-*' / `nl-rc-*').
    #[inline]
    pub fn as_ptr(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }

    /// Bump the refcount without acquiring a Rust handle.  Backs elisp
    /// `nl-rc-inc'; caller is responsible for a matching `rc_dec_raw'.
    ///
    /// # Safety
    /// Caller must guarantee a matching [`Self::rc_dec_raw`] follows.
    pub unsafe fn rc_inc_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive because the caller holds the
        // handle backing `this'.  fetch_add cannot wrap (= would need
        // 2^64 handles, physically impossible).
        unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_add(1, Ordering::Relaxed);
        }
    }

    /// Decrement the refcount, freeing the box at 0.  Backs elisp
    /// `nl-rc-dec'; takes `&Self' so it can fire from `args: &[Sexp]'.
    ///
    /// # Safety
    /// (a) Must pair with a prior [`Self::rc_inc_raw`] or be the final
    ///     balancing decrement of a normal handle's lifecycle.
    /// (b) On refcount=0 the car/cdr are dropped and the allocation
    ///     freed; surviving dangling handles are caller's responsibility.
    pub unsafe fn rc_dec_raw(this: &Self) {
        // SAFETY: `this.ptr' is alive on entry.  The Release/Acquire
        // pattern matches `Drop' below — we cannot reuse `Drop' here
        // because we have `&Self', not `&mut Self', and re-creating a
        // temporary `NlConsBoxRef' for the sole purpose of dropping
        // would re-enter `Clone' on the way in (= +1) and Drop on the
        // way out (= -1), leaving the count unchanged.
        let prev = unsafe {
            (*this.ptr.as_ptr())
                .refcount
                .fetch_sub(1, Ordering::Release)
        };
        if prev != 1 {
            return;
        }
        std::sync::atomic::fence(Ordering::Acquire);
        // SAFETY: refcount just hit 0.  See `Drop' for the same
        // invariants.
        unsafe {
            std::ptr::drop_in_place(std::ptr::addr_of_mut!(
                (*this.ptr.as_ptr()).car
            ));
            std::ptr::drop_in_place(std::ptr::addr_of_mut!(
                (*this.ptr.as_ptr()).cdr
            ));
            let layout = Layout::new::<NlConsBox>();
            alloc::dealloc(this.ptr.as_ptr() as *mut u8, layout);
        }
    }

    /// Mutate `car' in place — drop previous + write new.  Clones of the
    /// same `NlConsBoxRef' observe the mutation (shared box semantics).
    ///
    /// # Safety
    /// Caller must guarantee no other `&Sexp' borrow into `self.car'
    /// is live for the duration of the call.
    pub unsafe fn set_car(&self, val: Sexp) {
        let car_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).car);
        std::ptr::drop_in_place(car_ptr);
        std::ptr::write(car_ptr, val);
    }

    /// Mutate `cdr` in place.  See [`NlConsBoxRef::set_car`] for the
    /// safety contract.
    ///
    /// # Safety
    ///
    /// Same as [`NlConsBoxRef::set_car`].
    pub unsafe fn set_cdr(&self, val: Sexp) {
        let cdr_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).cdr);
        std::ptr::drop_in_place(cdr_ptr);
        std::ptr::write(cdr_ptr, val);
    }

    /// Internal raw `*const NlConsBox' for box-walking shims.
    #[doc(hidden)]
    #[inline]
    #[allow(dead_code)]
    pub fn inner_raw(this: &Self) -> *const NlConsBox {
        this.ptr.as_ptr()
    }
}

impl Clone for NlConsBoxRef {
    /// Doc 124 §124.F — thin elisp dispatch via §124.A kernel.
    /// The pure-elisp body does the refcount +1 via §122.E
    /// `atomic-fetch-add' (SeqCst ⊇ Relaxed).
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            crate::elisp_cc_spike::nlconsbox_clone(self.ptr.as_ptr() as *mut i64);
        }
        NlConsBoxRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlConsBoxRef {
    /// Doc 124 §124.L — dispatch through the pure-elisp `nlconsbox_drop'
    /// kernel.  Runs `atomic-fetch-add(-1)' then, on pre-sub == 1,
    /// calls `nl_consbox_drop_inner' (= `drop_in_place::<NlConsBox>') +
    /// `dealloc-bytes(72, 8)'.
    fn drop(&mut self) {
        unsafe {
            crate::elisp_cc_spike::nlconsbox_drop(self.ptr.as_ptr() as *mut i64);
        }
    }
}

impl Deref for NlConsBoxRef {
    type Target = NlConsBox;

    /// Borrow the box.  `car' / `cdr' / `refcount' live at fixed
    /// offsets inside `NlConsBox' (asserted at compile time below),
    /// so this is a single load + offset.
    fn deref(&self) -> &NlConsBox {
        // SAFETY: `self.ptr' is alive because we hold a handle, and
        // the box's fields were initialized in `NlConsBoxRef::new' and
        // are only torn down by the last `Drop' (= when no handle
        // exists to call `deref').  The reference borrows `self', so
        // it cannot outlive the handle.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlConsBoxRef {
    /// Forward to the inner box so `Sexp::Cons' debug output keeps
    /// the legacy `Cons(<car>, <cdr>)' shape downstream consumers
    /// (= ERT panics, log lines) already expect.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Cons").field(&self.car).field(&self.cdr).finish()
    }
}

impl PartialEq for NlConsBoxRef {
    /// Structural equality — matches the legacy
    /// `Rc<RefCell<Sexp>> == Rc<RefCell<Sexp>>' derive that compared
    /// inner Sexps recursively.  We add a `ptr_eq' fast path *first*
    /// so a Sexp value compared to itself short-circuits without
    /// cycling — the legacy derived `Cons' arm could hang on
    /// `(let ((x (cons 1 nil))) (setcdr x x) (equal x x))' style
    /// inputs because `Rc<RefCell<>>' derive recursed unconditionally
    /// (only `sexp_equal_safe' had the bounded recursion guard).
    /// `ptr_eq' converts that pathology into a constant-time return
    /// for the self-compare case while still recursing for distinct
    /// allocations with the same logical shape.
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.car == other.car && self.cdr == other.cdr
    }
}

// ---- Compile-time layout assertions ----
//
// These guarantee that the JIT (Phase A.5) and the elisp `nl-cons-*' /
// `nl-rc-*' primitives (Phase A.3) can reach `car' / `cdr' / `refcount'
// at known byte offsets without consulting Rust at runtime.
//
// `car @ 0' is locked by `repr(C)' (= first field).
// `cdr @ size_of::<Sexp>()' is locked by `repr(C)' field ordering.
// `refcount @ 2 * size_of::<Sexp>()' is locked by the same rule.
// Any future struct reorder fails the build here.

const _: () = {
    // `AtomicUsize` is 8 bytes on x86_64 / aarch64; this assert
    // catches any 32-bit target accidentally pulled into the build.
    assert!(std::mem::size_of::<AtomicUsize>() == 8);
    // `car` is the first field of a `repr(C)' struct.
    assert!(std::mem::offset_of!(NlConsBox, car) == 0);
    // `cdr` follows `car' with `Sexp' alignment (= 8 on x86_64 /
    // aarch64).  `size_of::<Sexp>()' is the canonical offset.
    assert!(std::mem::offset_of!(NlConsBox, cdr) == std::mem::size_of::<Sexp>());
    // `refcount' follows `cdr' at the trailer position.
    assert!(
        std::mem::offset_of!(NlConsBox, refcount) == 2 * std::mem::size_of::<Sexp>()
    );
};

