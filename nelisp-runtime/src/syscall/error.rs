//! Phase 7.0 syscall error wrapper.
//!
//! Thin shim over `errno`. Phase 7.5 will surface this to NeLisp as a
//! tagged Lisp_Object; for now we only need a clean Rust-side type so
//! `cargo test` can assert on syscall failure modes without scattering
//! raw `errno` lookups across the code base.

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SyscallError(pub i32);

impl SyscallError {
    pub fn last_os_error() -> Self {
        let e = unsafe { *libc_errno_location() };
        Self(e)
    }
    pub fn raw(self) -> i32 {
        self.0
    }
}

impl fmt::Display for SyscallError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syscall errno {}", self.0)
    }
}

#[cfg(any(target_os = "linux", target_os = "android"))]
unsafe fn libc_errno_location() -> *mut i32 {
    libc::__errno_location()
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
unsafe fn libc_errno_location() -> *mut i32 {
    libc::__error()
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "ios"
)))]
unsafe fn libc_errno_location() -> *mut i32 {
    // Fallback for *BSD / Solaris / Windows-via-msys.  Phase 7.5 will
    // narrow this; for Phase 7.0 a stub keeps the crate compiling on
    // platforms we don't gate at the test layer yet.
    static mut DUMMY: i32 = 0;
    &raw mut DUMMY
}
