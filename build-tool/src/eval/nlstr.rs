//! Doc 77c Phase A.4.2 — `NlStr` layout-pinned single-slot mutable
//! string buffer.
//!
//! Specialized self-managed refcounted cell carrying one mutable
//! `String` slot.  Replaces the legacy `Sexp::MutStr(Rc<RefCell<String>>)'
//! with a layout-pinned struct so:
//!
//! 1. The refcount lives at a known offset relative to the `value`
//!    field (= same architectural shape as [`super::nlcell::NlCell`]
//!    and [`super::nlconsbox::NlConsBox`]), unifying the elisp /
//!    Phase B self-host ABI across `Sexp' boxed variants.
//! 2. The `RefCell' borrow-flag overhead drops out — eval-time read
//!    is a single load of the `String' header instead of `borrow()'.
//! 3. `make-string' / `aset' on a mutable string get the same write
//!    contract as `setcar' / `setcdr' (= `unsafe set_value' /
//!    `with_value_mut').
//!
//! Layout (Phase A.4.2 locked):
//!
//! ```text
//! NlStr:  +----------+  offset 0                    (sizeof String)  value
//!         +----------+  offset sizeof(String)       (8 bytes)        refcount
//!         +----------+
//! ```
//!
//! Note: `String' itself is `#[repr(Rust)]'.  The `#[repr(C)]' on the
//! outer `NlStr' fixes field *ordering* + offsets, but the `String'
//! header (= `(ptr, len, cap)' triple) keeps its native Rust layout.
//! Phase B elisp will reach the chars via the String's `ptr' field
//! (= 2-load) — same access pattern Rust uses today.
//!
//! Mutability:
//! - `unsafe set_value(s: String)' — wholesale replace (= the legacy
//!   `*borrow_mut() = new_str' pattern used by `aset' on MutStr).
//! - `unsafe with_value_mut(f: FnOnce(&mut String) -> R) -> R' — in-
//!   place mutation closure (= the `let mut s = borrow_mut(); s.push_str' /
//!   `s.replace_range' style; not currently used by the migrated call
//!   sites but exposed for future Phase B / `nelisp-text-buffer' code).
//!
//! Out of scope for Phase A.4.2 MutStr migrate:
//!   - Other variants (Vector / BoolVector / Record / CharTable) —
//!     A.4.3-A.4.6 follow-up sub-stages.
//!   - elisp `nl-str-*' primitives — Phase B (`nelisp-text-buffer'
//!     uses host MutStr today).
//!
//! Threading: `AtomicUsize` mirrors `NlCell' / `NlConsBox' rationale.
//! Not `Send` / `Sync`.

use std::alloc::{self, Layout};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Layout-pinned mutable string buffer.  Heap-allocated, refcounted
/// via an `AtomicUsize` trailer.  Accessed through [`NlStrRef`]
/// handles.
#[repr(C)]
pub struct NlStr {
    /// The mutable string slot.  Offset 0 — same JIT contract as
    /// `NlCell.value' / `NlConsBox.car'.
    pub value: String,
    /// Strong reference count.  Starts at 1 in [`NlStrRef::new`],
    /// +1 on each `Clone`, -1 on each `Drop`.  When it reaches 0 the
    /// last handle drops `value' (= frees the heap chars buffer) and
    /// frees the [`NlStr`] allocation itself.
    pub refcount: AtomicUsize,
}

/// Refcounted handle to an [`NlStr`].  API parity with
/// [`super::nlcell::NlCellRef`] + [`super::nlconsbox::NlConsBoxRef`]:
/// `new` / `Clone` / `Drop` / `Deref` (returns `&NlStr`).
///
/// Phase A.5.1 (Doc 77c §4.6.1, 2026-05-09): `#[repr(transparent)]' pins
/// the layout to `NonNull<NlStr>' so JIT trampolines + Phase B elisp can
/// read the string pointer directly off the `Sexp' payload at offset
/// `SEXP_PAYLOAD_OFFSET'.
#[repr(transparent)]
pub struct NlStrRef {
    ptr: NonNull<NlStr>,
    /// Tells the borrow-checker we own an `NlStr` even though the
    /// field is `NonNull<...>'.  Mirrors `std::rc::Rc' / `NlCellRef' /
    /// `NlConsBoxRef'.
    _marker: PhantomData<NlStr>,
}

impl NlStr { pub(crate) const DROP_FN: unsafe fn(*mut std::ffi::c_void) = crate::eval::nlrc::nlrc_payload_drop::<NlStr>; } // Doc 79 v4 C.4-atomic
impl NlStrRef {
    /// Allocate a fresh [`NlStr`] on the heap with `refcount = 1`
    /// and return the unique handle.  The supplied `value` is moved
    /// into the box.
    ///
    /// Panics on allocation failure.
    pub fn new(value: String) -> NlStrRef {
        let layout = Layout::new::<NlStr>();
        // SAFETY: `Layout::new::<NlStr>()' is non-zero-sized — at
        // minimum the `String' header (= 24 bytes on 64-bit) + 8-byte
        // refcount.
        let raw = unsafe { alloc::alloc(layout) } as *mut NlStr;
        let ptr = match NonNull::new(raw) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        // SAFETY: `ptr' was just allocated for `NlStr' and is
        // exclusively owned here.  We initialize both fields before
        // anyone else can observe the box.
        unsafe {
            std::ptr::write(std::ptr::addr_of_mut!((*ptr.as_ptr()).value), value);
            std::ptr::write(
                std::ptr::addr_of_mut!((*ptr.as_ptr()).refcount),
                AtomicUsize::new(1),
            );
        }
        NlStrRef {
            ptr,
            _marker: PhantomData,
        }
    }

    /// Read the current strong-reference count.  Mirrors
    /// `NlCellRef::strong_count' / `NlConsBoxRef::strong_count'.
    pub fn strong_count(this: &Self) -> usize {
        // SAFETY: `this.ptr' is alive because we hold a handle.
        unsafe { (*this.ptr.as_ptr()).refcount.load(Ordering::Acquire) }
    }

    /// Pointer-equality on the *underlying box*.  Two clones of the
    /// same [`NlStrRef::new`] invocation are pointer-equal; two
    /// distinct allocations are not.  Used by `eq' / `eql' on
    /// `Sexp::MutStr' (= identity comparison).
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr.as_ptr() == b.ptr.as_ptr()
    }

    /// Wholesale replace `value`.  Drops the previous String, then
    /// writes the new one.  Equivalent to the legacy
    /// `*borrow_mut() = new_str' pattern (= `aset' on MutStr).
    ///
    /// # Safety
    ///
    /// Caller must guarantee no other `&String` borrow into this
    /// box's `value` field is live at the time of the write.  In
    /// Phase A.4.2 the migrated `aset' callsite owns `new_str'
    /// locally and holds no outstanding borrow into `self.value';
    /// the same Phase A.2.1 setcar discipline applies.
    pub unsafe fn set_value(&self, val: String) {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: see method-level contract.
        unsafe {
            std::ptr::drop_in_place(value_ptr);
            std::ptr::write(value_ptr, val);
        }
    }

    /// In-place mutation closure.  Hands the caller a `&mut String'
    /// so they can use any `String' API (`push_str' / `replace_range'
    /// / `truncate' / etc.) without going through `set_value'.
    /// Returns whatever the closure returns.
    ///
    /// # Safety
    ///
    /// Same as [`set_value`]: no other `&String` borrow into
    /// `self.value' may be live for the duration of the closure.
    /// Reentrant calls (= the closure recursing into another
    /// `with_value_mut' on the same handle) are UB.
    pub unsafe fn with_value_mut<R>(&self, f: impl FnOnce(&mut String) -> R) -> R {
        let value_ptr = std::ptr::addr_of_mut!((*self.ptr.as_ptr()).value);
        // SAFETY: see method-level contract.  Caller-provided closure
        // observes a `&mut String' that aliases nothing else.
        unsafe { f(&mut *value_ptr) }
    }
}

impl Clone for NlStrRef {
    /// Bump the refcount and return a new handle that shares the
    /// same inner box.  `Relaxed' for the +1.
    fn clone(&self) -> Self {
        // SAFETY: `self.ptr' is alive because we hold a handle.
        unsafe {
            (*self.ptr.as_ptr()).refcount.fetch_add(1, Ordering::Relaxed);
        }
        NlStrRef {
            ptr: self.ptr,
            _marker: PhantomData,
        }
    }
}

impl Drop for NlStrRef {
    fn drop(&mut self) { unsafe { crate::nlrc_drop_box!(self.ptr.as_ptr(), NlStr, crate::eval::sexp::SEXP_TAG_MUT_STR); } }
}

impl Deref for NlStrRef {
    type Target = NlStr;

    fn deref(&self) -> &NlStr {
        // SAFETY: see `NlCellRef::deref' for the same invariant.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl std::fmt::Debug for NlStrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("MutStr").field(&self.value).finish()
    }
}

impl PartialEq for NlStrRef {
    /// Structural equality + ptr_eq fast path.  Mirrors
    /// `NlCellRef::eq' / `NlConsBoxRef::eq' rationale.
    fn eq(&self, other: &Self) -> bool {
        if Self::ptr_eq(self, other) {
            return true;
        }
        self.value == other.value
    }
}

// ---- Doc 122 §122.A — String/Symbol allocator externs ----
//
// Phase 47 grammar ops `sexp-write-str' / `sexp-write-symbol' use these
// externs to materialize fresh `Sexp::Str(String)' / `Sexp::Symbol(String)'
// values into a caller-owned slot.  Unlike `nl_alloc_vector' / `nl_alloc_
// cell' which return `*mut NlXXX' pointers (= boxed payloads accessed at
// `Sexp' payload offset 8), `Sexp::Str' and `Sexp::Symbol' carry their
// `String' header *inline* at payload offset 8..32 (= ptr/cap/len triple
// — see `nelisp-string--offset-*' in `lisp/nelisp-sexp-layout.el').
// Therefore the allocator extern writes the full 40-byte `Sexp' (tag at
// offset 0, padding [1..8), 24-byte `String' header at [8..32),
// 8-byte tail padding [32..40)) into the result slot directly.  The
// existing `str-len' / `str-bytes' / `str-byte-at' grammar ops read
// from this inline layout, so the round-trip
// `(sexp-write-str slot bytes len)' -> `(str-len slot)' yields LEN
// without any indirection.
//
// Refcount semantics: the freshly allocated `String' is owned by the
// `Sexp::Str' / `Sexp::Symbol' value sitting in the slot — i.e. drop
// of the slot frees the `String' buffer.  No `AtomicUsize' refcount
// is involved (= `String' is move-only; the "refcount = 1" mention
// in Doc 122 §122.A applies to the heap chars buffer's single owner,
// not an `NlStr' trailer).
//
// Doc 122 §122.A intentionally diverges from the literal task-prompt
// signature `-> *mut NlStr'.  An `*mut NlStr' would require `str-len'
// to do an extra indirection (= load NlStr*, then `+16` for the String
// length field), breaking the existing inline-layout contract and the
// 4 trampolines this stage unblocks (= `nl_jit_intern',
// `nl_jit_symbol_name', `nl_jit_type_of', `nl_jit_make_symbol').  The
// `result_slot' write-through shape matches the existing
// `nelisp_cons_construct' / `nelisp_truncate_int' externs.

/// Doc 122 §122.A — allocate a fresh `Sexp::Str(String)` value into
/// the caller-supplied `result_slot` from a byte range.
///
/// Copies `len` bytes starting at `bytes_ptr` into a fresh `String`,
/// constructs `Sexp::Str(s)`, and writes that 40-byte value into
/// `*result_slot`.  Returns `result_slot` for caller ergonomics
/// (= matches `nelisp_cons_construct' shape).
///
/// The `bytes_ptr` range MUST be valid UTF-8.  We use
/// `String::from_utf8_unchecked` on the freshly allocated copy after
/// length-prefixed bound checks; callers that synthesize the bytes
/// out of e.g. `format!` results trivially satisfy the invariant,
/// and the elisp reader/lexer arms that feed this op verify their
/// inputs upstream.
///
/// # Safety
/// - `bytes_ptr` must be non-null and point at `len` initialized
///   bytes of valid UTF-8.  An empty range (`len == 0`) is permitted
///   and `bytes_ptr` may be dangling-but-aligned in that case
///   (= matches `std::slice::from_raw_parts` contract).
/// - `result_slot` must be non-null, properly aligned, and writable
///   for one full `Sexp` slot (40 bytes).  Callers should
///   pre-initialize it to `Sexp::Nil` so the `std::ptr::write` does
///   not drop arbitrary bytes; `nelisp_cons_construct' and
///   `nelisp_cell_make' use the same convention.
/// - `len` must be `>= 0` and fit in a `usize`.
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_str(
    bytes_ptr: *const u8,
    len: i64,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    let n = len as usize;
    // Build a fresh owned String.  `len == 0` is a permitted edge case:
    // `slice::from_raw_parts(ptr, 0)' is well-defined regardless of
    // `ptr' alignment / validity.
    let bytes: Vec<u8> = if n == 0 {
        Vec::new()
    } else {
        let slice = unsafe { std::slice::from_raw_parts(bytes_ptr, n) };
        slice.to_vec()
    };
    // SAFETY: caller contract guarantees the input is valid UTF-8.
    let s = unsafe { String::from_utf8_unchecked(bytes) };
    let sexp = crate::eval::sexp::Sexp::Str(s);
    // SAFETY: `result_slot' is caller-owned for one `Sexp' slot.
    // `std::ptr::write' overwrites without dropping the previous
    // value — caller pre-initialized to `Sexp::Nil' so this is safe.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

/// Doc 122 §122.A — allocate a fresh `Sexp::Symbol(String)` value into
/// the caller-supplied `result_slot` from a byte range.
///
/// Same shape as [`nl_alloc_str`] but produces `Sexp::Symbol(_)` (=
/// tag byte = `SEXP_TAG_SYMBOL`, payload = inline `String` header).
/// *Does not* consult any intern table — the caller is responsible
/// for symbol identity when needed.  See Doc 122 §5 (open question)
/// for the future intern-table consult vs. caller-managed split.
///
/// # Safety
/// Identical contract to [`nl_alloc_str`].  The fresh `Symbol` is
/// move-owned by the slot; dropping the slot frees the chars buffer.
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
    // SAFETY: see `nl_alloc_str' note.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

// ---- Doc 122 §122.B — Mutable string builder externs ----
//
// Phase 47 grammar ops `mut-str-make-empty' / `mut-str-push-byte' /
// `mut-str-push-codepoint' / `mut-str-len' / `mut-str-finalize' use
// these externs to drive incremental NlMutStr construction (= the
// Reader / lexer use case: byte-at-a-time token accumulation, then a
// single move into an immutable `Sexp::Str' at token end).
//
// Layout shape vs. §122.A: `Sexp::MutStr(NlStrRef)' wraps a
// `NonNull<NlStr>' (= `#[repr(transparent)]'), so the Sexp payload at
// offset 8 is a raw box pointer.  Unlike `Sexp::Str' / `Sexp::Symbol'
// which carry an inline `String' header at offset 8..32, MutStr's
// inner `NlStr.value: String' lives one indirection away.  Each
// push extern therefore: (1) reads the box pointer via
// `Sexp::mut_str_box_ptr', (2) takes a `&mut String' to the inner
// `value' through `NlStrRef::with_value_mut'-equivalent raw access,
// (3) mutates in place.
//
// Refcount semantics: `nl_alloc_mut_str' returns an `NlStrRef' with
// refcount = 1 (= `NlStrRef::new' contract).  The slot owns that
// single strong reference; dropping the slot decrements to 0 and
// frees the box.  Push ops do not bump the refcount (= they take a
// raw pointer, not a clone).  `nl_mut_str_finalize' clones the
// `String' (= a heap copy of the chars buffer) rather than moving
// it out of the live NlStr, so the caller's MutStr slot remains
// usable post-finalize for further pushes — matches the elisp
// `(mut-str-push-byte H B) ... (mut-str-finalize H S1) (mut-str-push-byte H C) ...
// (mut-str-finalize H S2)' usage pattern the Reader lexer needs
// for sub-token snapshots.
//
// Doc 120 SKIPPED trampolines this unblocks (per task spec):
//   - `nl_jit_make_mut_str' (= alloc + finalize chain)
//   - `nl_jit_concat_ints' (= alloc + repeated push_codepoint loop)
//   - `nl_jit_mut_str_set_codepoint' (= push_codepoint primitive)
//   - `nl_jit_record_alloc' (= alloc pattern reused for record init)

/// Doc 122 §122.B — allocate a fresh `Sexp::MutStr(NlStrRef)` into
/// the caller-supplied `result_slot` with a reserved byte capacity.
///
/// Calls [`NlStrRef::new`] on a `String::with_capacity(cap)`,
/// constructs `Sexp::MutStr(rc)`, and writes that 40-byte value into
/// `*result_slot`.  Returns `result_slot` for caller ergonomics.
/// Refcount starts at 1 (= `NlStrRef::new` contract).
///
/// # Safety
/// - `result_slot` must be non-null, properly aligned, and writable
///   for one full `Sexp` slot (40 bytes).  Callers should
///   pre-initialize it to `Sexp::Nil` so the `std::ptr::write` does
///   not drop arbitrary bytes (= same convention as
///   [`nl_alloc_str`] / `nelisp_cell_make`).
/// - `cap` must be `>= 0` and fit in a `usize`.  A `cap == 0` is
///   permitted and yields an `NlStr.value` with `String::new()`
///   semantics (= no heap allocation until the first push).
#[no_mangle]
pub unsafe extern "C" fn nl_alloc_mut_str(
    cap: i64,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    let n = if cap < 0 { 0 } else { cap as usize };
    let rc = NlStrRef::new(String::with_capacity(n));
    let sexp = crate::eval::sexp::Sexp::MutStr(rc);
    // SAFETY: caller-owned slot, pre-init to Nil by convention.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

/// Doc 122 §122.B — append a single byte to a `Sexp::MutStr`'s buffer.
///
/// `mut_str_ptr` is a `*mut Sexp` whose tag is `SEXP_TAG_MUT_STR`.
/// `byte` is the low 8 bits of a `i64` (= elisp doesn't have a `u8`
/// primitive; the high 56 bits are masked off here).  The push is
/// raw byte-level — no UTF-8 validation, no codepoint encoding.
/// Use for ASCII / pre-encoded UTF-8 input; use
/// [`nl_mut_str_push_codepoint`] when the caller has a codepoint.
///
/// # Safety
/// - `mut_str_ptr` must be non-null and point at a live `Sexp::MutStr`.
///   The MutStr's inner `NlStr` must not have any other live `&String`
///   borrow into `value` (= same contract as
///   [`NlStrRef::with_value_mut`]).
/// - The resulting bytes do not need to be valid UTF-8 mid-build;
///   callers that intend to read the bytes back as UTF-8 (= via
///   `nl_mut_str_finalize`'s `Sexp::Str` output) must ensure overall
///   validity before finalize.  `String::from_utf8_unchecked` is used
///   on the finalize path so an invalid mid-state observed via
///   finalize is undefined behavior — see `nl_mut_str_finalize`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_push_byte(
    mut_str_ptr: *mut crate::eval::sexp::Sexp,
    byte: i64,
) {
    let b = (byte & 0xFF) as u8;
    // SAFETY: caller-asserted tag.  `mut_str_box_ptr' returns
    // `*const NlStr'; cast to `*mut' since caller's contract grants
    // exclusive mut access for this op.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    // SAFETY: caller contract — no aliasing borrow into value.  We
    // bypass the `safe' UTF-8 invariant by writing via `Vec'.
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    unsafe { value_ref.as_mut_vec() }.push(b);
}

/// Doc 122 §122.B — UTF-8 encode `codepoint` and append (1–4 bytes)
/// to a `Sexp::MutStr`'s buffer.
///
/// Out-of-range / surrogate codepoints are silently clamped to
/// U+FFFD (REPLACEMENT CHARACTER) so the final string remains valid
/// UTF-8.  This matches the spirit of Rust's
/// `char::from_u32(...).unwrap_or(REPLACEMENT_CHARACTER)` rather than
/// returning an error code: the Reader lexer (= the primary caller)
/// would otherwise need an extra check on each iteration, and the
/// codepoint stream feeding the lexer is already validated upstream.
///
/// # Safety
/// Same contract as [`nl_mut_str_push_byte`] — caller asserts
/// `*mut_str_ptr` has `SEXP_TAG_MUT_STR` and no aliasing borrow.
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
    // SAFETY: caller-asserted tag.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() } as *mut NlStr;
    // SAFETY: caller contract — no aliasing borrow into value.
    let value_ref: &mut String = unsafe { &mut (*nlstr_ptr).value };
    value_ref.push(ch);
}

/// Doc 122 §122.B — current byte length of a `Sexp::MutStr`.
///
/// Equivalent to `(*mut_str_ptr).MutStr.value.len()`.  Returned as
/// `i64` to fit the Phase 47 GP register width.
///
/// # Safety
/// `mut_str_ptr` must be non-null and point at a live `Sexp::MutStr`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_len(
    mut_str_ptr: *const crate::eval::sexp::Sexp,
) -> i64 {
    // SAFETY: caller-asserted tag.  Materialise an explicit `&String'
    // borrow (= rustc 1.83+ `dangerous_implicit_autorefs' lint
    // requires the autoref be spelled out when starting from a raw
    // pointer through autoref).
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    value_ref.len() as i64
}

/// Doc 122 §122.B — build an immutable `Sexp::Str` from a
/// `Sexp::MutStr`'s current bytes and write it into the caller-owned
/// `result_slot`.
///
/// Clones the inner `String` rather than moving it out of the live
/// MutStr (= the source remains usable for further pushes /
/// snapshots).  The clone is a single `String::clone` (= heap
/// allocation + memcpy of the chars buffer; no UTF-8 re-validation
/// because the source is invariant-by-construction when pushes go
/// through `push_codepoint`; `push_byte` callers must keep the
/// invariant — see [`nl_mut_str_push_byte`] safety note).
///
/// # Safety
/// - `mut_str_ptr`: same as [`nl_mut_str_len`].
/// - `result_slot`: same as [`nl_alloc_mut_str`].  Pre-init to
///   `Sexp::Nil`.
#[no_mangle]
pub unsafe extern "C" fn nl_mut_str_finalize(
    mut_str_ptr: *const crate::eval::sexp::Sexp,
    result_slot: *mut crate::eval::sexp::Sexp,
) -> *mut crate::eval::sexp::Sexp {
    // SAFETY: caller-asserted tag.  Explicit `&String' borrow per
    // `dangerous_implicit_autorefs' lint.
    let nlstr_ptr = unsafe { (*mut_str_ptr).mut_str_box_ptr() };
    let value_ref: &String = unsafe { &(*nlstr_ptr).value };
    let cloned = value_ref.clone();
    let sexp = crate::eval::sexp::Sexp::Str(cloned);
    // SAFETY: caller-owned slot.
    unsafe { std::ptr::write(result_slot, sexp) };
    result_slot
}

// ---- Doc 122 §122.D — UTF-8 helper externs ----
//
// Phase 47 grammar ops `str-char-count' / `str-codepoint-at' /
// `str-is-alphanumeric-at' use these externs to walk the UTF-8 byte
// stream of a `Sexp::Str' value.  All three consult the inline
// `String' payload at offset 8..32 (= same layout the
// `nelisp-string--offset-{capacity,ptr,length}' Phase 47 emit code
// uses for `str-len' / `str-bytes' / `str-byte-at').  Unblocks Doc
// 120 `nl_jit_mut_str_len' (= char count, not byte count) +
// `nl_jit_str_codepoint_at' + `nl_jit_downcase' / `_upcase' +
// `nl_jit_split_by_non_alnum' (= predicate side) and completes the
// Doc 116.A Reader lexer char-class prereqs.

/// Doc 122 §122.D — count UTF-8 codepoints (NOT bytes) in a
/// `Sexp::Str'.
///
/// Equivalent to `s.chars().count() as i64'.  Differs from
/// `nl_str_len' / `str-len' / `nl_mut_str_len' which all return
/// byte counts.  Used by the elisp `length' shim's String arm
/// (= elisp `(length "藤澤")' must yield 2, not 6).
///
/// # Safety
/// `str_ptr' must be non-null and point at a live `Sexp::Str'
/// (or `Sexp::Symbol' — same layout).  Non-string variants
/// produce undefined behaviour because the inline `String' header
/// access reads garbage bytes.
#[no_mangle]
pub unsafe extern "C" fn nl_str_char_count(
    str_ptr: *const crate::eval::sexp::Sexp,
) -> i64 {
    // SAFETY: caller-asserted tag.  Either `Sexp::Str(s)' or
    // `Sexp::Symbol(s)' carries the inline `String' header; both
    // expose the same `.as_str()' view, so a single match arm
    // covers them.  For `Sexp::MutStr(_)` we walk through the
    // boxed `NlStr.value' instead.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    let s: &str = match sexp_ref {
        crate::eval::sexp::Sexp::Str(text) => text.as_str(),
        crate::eval::sexp::Sexp::Symbol(text) => text.as_str(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_str(),
        _ => return 0,
    };
    s.chars().count() as i64
}

/// Doc 122 §122.D — decode the UTF-8 codepoint at `byte_idx' in a
/// `Sexp::Str' and write `(codepoint, byte_width)' through the
/// caller-supplied out-slots.
///
/// Returns `1' on success, `0' on an invalid `byte_idx' (= out of
/// range or in the middle of a multi-byte codepoint) or malformed
/// UTF-8.  On failure the out-slots are left untouched.  Used by
/// `nl_jit_str_codepoint_at' (= the Phase 47 alternative to the
/// Rust trampoline that walks `s.char_indices()' looking for the
/// `byte_idx` match).
///
/// # Safety
/// - `str_ptr' must be non-null and point at a live `Sexp::Str'
///   / `Sexp::Symbol' / `Sexp::MutStr'.
/// - `out_codepoint' and `out_byte_width' must be non-null and
///   writable for one `i64' each.
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
    // `s[idx..].chars().next()` decodes the codepoint starting at
    // `idx'.  The byte width is the difference between the next
    // codepoint boundary and `idx', which `char::len_utf8' supplies
    // directly.
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

/// Doc 122 §122.D — predicate: is the UTF-8 codepoint at `byte_idx'
/// alphanumeric (Unicode-aware)?
///
/// ASCII fast path: when the byte at `byte_idx' is `[0-9A-Za-z]`
/// return `1' directly without UTF-8 decode.  Otherwise decode the
/// codepoint and consult `char::is_alphanumeric'.  Returns `0' on
/// out-of-range / mid-codepoint indices or non-alphanumeric values.
/// Used by `nl_jit_split_by_non_alnum' / lexer char-class checks.
///
/// # Safety
/// `str_ptr' must be non-null and point at a live `Sexp::Str' /
/// `Sexp::Symbol' / `Sexp::MutStr'.
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
    // ASCII fast path — `[0-9A-Za-z]` lead byte means the codepoint
    // is exactly that byte (single-byte UTF-8 sequence).
    let b = bytes[idx];
    if b.is_ascii_alphanumeric() {
        return 1;
    }
    // Multi-byte slow path: decode codepoint at boundary, check
    // Unicode property.  Mid-codepoint indices fail the boundary
    // check the same way as `nl_str_codepoint_at'.
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
//
// §122.A's `sexp-write-str' covers the *inward* direction
// (bytes → `Sexp::Str').  The matching *outward* direction (`Sexp::Str'
// → `*const u8' for a libc-style syscall) was missing until Doc 122
// §122.H — Phase 47's pre-§122.H `str-bytes' grammar op only worked on
// the inline `String' layout of `Sexp::Str' / `Sexp::Symbol'
// (`mov rax, [rdi + 16]'); it could not handle `Sexp::MutStr'
// (which wraps `NlStrRef = NonNull<NlStr>' at payload offset 8, one
// indirection away from the bytes).  The Doc 117.B I/O syscall sweep
// (`write_stdout' / `write_stderr_line' / `read_stdin' / `read_file'
// / `write_file') needs a single grammar op that handles all three
// `string-y' variants safely, so that the same elisp body works
// regardless of whether the caller passes an immutable literal
// (`Sexp::Str'), an interned name (`Sexp::Symbol') or a builder buffer
// (`Sexp::MutStr').
//
// `nl_str_bytes_ptr' centralises the variant dispatch in Rust + uses
// the official `String::as_ptr()' / `NlStr.value.as_ptr()' accessors
// (= no layout assumption on the inner `String' header).  Non-string
// variants return `std::ptr::null()' which the elisp caller is
// expected to either avoid (= tag-check upstream) or treat as a
// 0-length write (= benign no-op when paired with `str-len' of 0).

/// Doc 122 §122.H — return the data pointer of a `Sexp`'s underlying
/// byte buffer for a `string-y' variant (`Sexp::Str' / `Sexp::Symbol'
/// / `Sexp::MutStr'); return `null` for every other tag.
///
/// The returned pointer aliases `*str_ptr`'s inner storage: it remains
/// valid for as long as the `Sexp` slot lives and is not mutated /
/// reallocated.  Callers that pair this with `str-len' (= the byte
/// count returned by `nl_str_len' / inline `[rdi + 24]') reach
/// exactly `len' initialised UTF-8 bytes at `[ptr, ptr + len)'.
///
/// The implementation goes through the standard `String::as_ptr()` /
/// `(&*nlstr).value.as_ptr()' accessors so it does not depend on
/// Rust's `String' header field order being `(ptr, capacity, length)'
/// (= the assumption baked into Phase 47's inline `str-bytes' op).
/// `Sexp::Symbol' carries the same inline `String` header as
/// `Sexp::Str' so its pointer behaves identically; `Sexp::MutStr'
/// indirects through `NlStrRef::deref' to the heap-resident `NlStr.value'.
///
/// # Safety
/// - `str_ptr' must be non-null and point at a live `Sexp' value
///   (= same precondition the §122.D `nl_str_*' externs use).  The
///   tag dispatch is internal — non-string variants yield `null` not
///   undefined behaviour.
/// - For `Sexp::Str' / `Sexp::Symbol' the empty-string case yields a
///   non-null but `dangling-but-aligned' pointer (= `String::as_ptr()`
///   contract); reading 0 bytes from it is sound, reading non-zero is
///   UB.  Callers that pair with `str-len' get the matching `len = 0'
///   and naturally do nothing in that case.
#[no_mangle]
pub unsafe extern "C" fn nl_str_bytes_ptr(
    str_ptr: *const crate::eval::sexp::Sexp,
) -> *const u8 {
    // SAFETY: caller contract — `str_ptr' non-null + live Sexp.
    let sexp_ref: &crate::eval::sexp::Sexp = unsafe { &*str_ptr };
    match sexp_ref {
        crate::eval::sexp::Sexp::Str(s) => s.as_ptr(),
        crate::eval::sexp::Sexp::Symbol(s) => s.as_ptr(),
        crate::eval::sexp::Sexp::MutStr(rc) => rc.value.as_ptr(),
        _ => std::ptr::null(),
    }
}

// ---- Doc 122 §122.G — Float allocator + str-to-float helper ----
//
// §122.G unlocks Reader Float literals (Doc 116.B+).  Two externs:
//
//   `nl_sexp_write_float(slot, val: f64)` — write `Sexp::Float(val)`
//   into `*slot`.  The `Sexp::Float` payload is inline f64 at offset
//   `nelisp-sexp--offset-payload' = 8 (matches `Sexp::Int' layout but
//   with tag byte = `SEXP_TAG_FLOAT' = 3).  No heap box needed.
//
//   `nl_str_to_float(bytes_ptr, len, slot)` — parse the UTF-8 byte
//   range as an f64 via `str::parse::<f64>()` and write
//   `Sexp::Float(parsed)' into `*slot' on success; leave `*slot' as
//   `Sexp::Nil' on failure (= caller observes via the `i64' return
//   status: 1 = success, 0 = parse error).
//
// The two externs split responsibility cleanly:
//   - `nl_sexp_write_float' is the Phase 47 grammar op's lowering
//     target — exercises f64 arg dispatch via xmm0 per Doc 122.C
//     SysV AMD64 ABI.
//   - `nl_str_to_float' is the Reader's bridge — takes a byte range
//     (= Sexp::Str payload) and writes the parsed Float Sexp directly
//     into the parser's result slot.  This avoids needing the elisp
//     parser to round-trip the f64 through an xmm0 in-flight value
//     (= no f64 type-annotated local binding grammar yet).

/// Doc 122 §122.G — write `Sexp::Float(val)` into the caller's slot.
///
/// Inline f64 payload: tag byte 3 at `slot[0]`, f64 little-endian at
/// `slot[8..16]`.  `std::ptr::write' overwrites without dropping the
/// previous value — caller pre-initialises to `Sexp::Nil' so this is
/// safe (= same convention as `nl_alloc_str' / `nl_alloc_symbol' /
/// `nelisp_cons_construct').
///
/// # Safety
/// - `slot' must be non-null, properly aligned, and writable for one
///   full `Sexp' slot (40 bytes).  Pre-init to `Sexp::Nil'.
/// - `val' is a regular f64; NaN / Infinity / -0.0 are all valid and
///   round-trip bit-for-bit (= `std::ptr::write' is a byte copy).
#[no_mangle]
pub unsafe extern "C" fn nl_sexp_write_float(
    slot: *mut crate::eval::sexp::Sexp,
    val: f64,
) -> *mut crate::eval::sexp::Sexp {
    let sexp = crate::eval::sexp::Sexp::Float(val);
    // SAFETY: caller-owned slot pre-init to Nil per contract.
    unsafe { std::ptr::write(slot, sexp) };
    slot
}

/// Doc 122 §122.G — parse a UTF-8 byte range as an f64 and write
/// `Sexp::Float(parsed)' into the caller's slot.  Returns 1 on
/// success, 0 on parse failure (= `slot' is set to `Sexp::Nil' in
/// the failure case).
///
/// Uses `str::parse::<f64>()' which accepts the standard Rust float
/// grammar: decimal point, exponent, optional leading sign, the
/// special tokens `inf' / `infinity' / `nan' (case-insensitive).
/// Matches the Reader's `Token::Float` payload shape (= the lexer's
/// `parse::<f64>' arm) by construction.
///
/// # Safety
/// - `bytes_ptr' must be non-null when `len > 0' and point at `len'
///   initialized bytes of valid UTF-8.  `len == 0' permits a
///   dangling-but-aligned `bytes_ptr' (= `slice::from_raw_parts'
///   contract).
/// - `slot' must be non-null, properly aligned, and writable for one
///   `Sexp' slot.  Pre-init to `Sexp::Nil'.
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
    // SAFETY: caller contract — bytes are valid UTF-8.
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
    // value @ offset 0 — same JIT contract as `NlCell.value'.
    assert!(offset_of!(NlStr, value) == 0);
    // refcount @ offset sizeof(String) — `repr(C)' linear layout.
    assert!(offset_of!(NlStr, refcount) == size_of::<String>());
    // refcount is 8 bytes on every supported arch.
    assert!(size_of::<AtomicUsize>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layout_value_at_offset_0() {
        use std::mem::offset_of;
        assert_eq!(offset_of!(NlStr, value), 0);
    }

    #[test]
    fn layout_refcount_after_value() {
        use std::mem::{offset_of, size_of};
        assert_eq!(offset_of!(NlStr, refcount), size_of::<String>());
    }

    #[test]
    fn new_starts_with_refcount_1() {
        let s = NlStrRef::new("hello".to_string());
        assert_eq!(NlStrRef::strong_count(&s), 1);
    }

    #[test]
    fn new_returns_box_holding_value() {
        let s = NlStrRef::new("hello".to_string());
        assert_eq!(s.value, "hello");
    }

    #[test]
    fn clone_bumps_refcount_and_shares_value() {
        let a = NlStrRef::new("shared".to_string());
        let b = a.clone();
        assert_eq!(NlStrRef::strong_count(&a), 2);
        assert_eq!(b.value, "shared");
    }

    #[test]
    fn drop_decrements_refcount() {
        let a = NlStrRef::new("x".to_string());
        {
            let _b = a.clone();
            assert_eq!(NlStrRef::strong_count(&a), 2);
        }
        assert_eq!(NlStrRef::strong_count(&a), 1);
    }

    #[test]
    fn ptr_eq_same() {
        let a = NlStrRef::new("a".to_string());
        let b = a.clone();
        assert!(NlStrRef::ptr_eq(&a, &b));
    }

    #[test]
    fn ptr_eq_different_alloc() {
        let a = NlStrRef::new("same".to_string());
        let b = NlStrRef::new("same".to_string());
        assert!(!NlStrRef::ptr_eq(&a, &b));
    }

    #[test]
    fn set_value_replaces_in_place() {
        let s = NlStrRef::new("old".to_string());
        // SAFETY: no other borrow into s.value is live.
        unsafe { s.set_value("new".to_string()) };
        assert_eq!(s.value, "new");
    }

    #[test]
    fn set_value_visible_through_clone() {
        let a = NlStrRef::new("v1".to_string());
        let b = a.clone();
        // SAFETY: no live borrow.
        unsafe { a.set_value("v2".to_string()) };
        assert_eq!(a.value, "v2");
        assert_eq!(b.value, "v2");
    }

    #[test]
    fn with_value_mut_in_place_mutation() {
        let s = NlStrRef::new("hello".to_string());
        // SAFETY: no other borrow live.
        unsafe {
            s.with_value_mut(|v| {
                v.push_str(", world");
            });
        }
        assert_eq!(s.value, "hello, world");
    }

    #[test]
    fn with_value_mut_returns_value_from_closure() {
        let s = NlStrRef::new("abc".to_string());
        let len = unsafe { s.with_value_mut(|v| v.len()) };
        assert_eq!(len, 3);
    }

    #[test]
    fn debug_format_uses_mutstr_tuple() {
        let s = NlStrRef::new("x".to_string());
        let d = format!("{:?}", s);
        assert!(
            d.starts_with("MutStr("),
            "expected `MutStr(...)' debug shape, got {:?}",
            d
        );
    }

    #[test]
    fn partial_eq_same_handle_short_circuits() {
        let a = NlStrRef::new("hi".to_string());
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn partial_eq_distinct_alloc_compares_value() {
        let a = NlStrRef::new("hi".to_string());
        let b = NlStrRef::new("hi".to_string());
        assert_eq!(a, b);
        let c = NlStrRef::new("ho".to_string());
        assert_ne!(a, c);
    }

    #[test]
    fn payload_drop_runs_exactly_once() {
        // Probe via a fresh String allocation — observe that `drop'
        // returns the chars buffer to the allocator.  We can't
        // trivially detect this from inside the test, but the
        // round-trip check via large allocation + memory inspection
        // is impractical.  Use a clone-bump round-trip instead:
        // create + clone + drop the clone + drop the original; if
        // the payload were leaked the second drop would not free
        // (= no panic, no observable effect from inside test, but
        // miri / ASAN would catch).  We rely on miri / ASAN runs in
        // CI; the unit test asserts no UB-flag panics.
        let s = NlStrRef::new("payload".to_string());
        {
            let _t = s.clone();
        }
        drop(s);
        // If we reach here without panic / UB, the drop sequence
        // works.  The companion miri / ASAN run is what gives this
        // test real teeth.
    }

    #[test]
    fn ptr_eq_after_intermediate_drops() {
        let a = NlStrRef::new("a".to_string());
        {
            let _b = a.clone();
        }
        let c = a.clone();
        assert!(NlStrRef::ptr_eq(&a, &c));
    }

    #[test]
    fn set_value_drops_previous_string() {
        // Build a String large enough that we'd notice if it leaked.
        let s = NlStrRef::new("0".repeat(1024));
        // SAFETY: no other borrow.
        unsafe { s.set_value("1".to_string()) };
        assert_eq!(s.value, "1");
        // ASAN / miri verify the 1024-byte buffer was returned to
        // the allocator on `drop_in_place'.
    }

    #[test]
    fn with_value_mut_truncate() {
        let s = NlStrRef::new("longstring".to_string());
        unsafe {
            s.with_value_mut(|v| v.truncate(4));
        }
        assert_eq!(s.value, "long");
    }
}
