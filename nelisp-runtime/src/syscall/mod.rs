//! Phase 7.0 syscall public surface.
//!
//! Two layers:
//!   * Rust-native wrappers (`unix::*`) — used by `cargo test` and
//!     `main.rs --syscall-smoke` to exercise the path without going
//!     through the FFI boundary.
//!   * `extern "C"` re-exports prefixed `nelisp_syscall_*` — the only
//!     symbols NeLisp will dlsym() in Phase 7.5.  Names are stable.
//!
//! Phase 7.0 ships exactly the 10 calls Doc 27 §3 7.0 enumerated:
//!   read / write / open / close / mmap / munmap / mprotect / exit /
//!   getenv / stat (+ fstat & setenv as cheap bonus, NeLisp file APIs
//!   need them and the cost is one libc passthrough each).
//!
//! 2026-04-25 MAP_JIT update (Doc 28 v2 §6.9): three additional
//! arm64 JIT primitives —
//!   * `nelisp_syscall_mmap_jit` (mmap variant whose `flags` may
//!     include `NELISP_MAP_JIT` on Apple Silicon),
//!   * `nelisp_syscall_clear_icache` (`__clear_cache` wrapper, no-op
//!     on x86_64), and
//!   * `nelisp_syscall_jit_write_protect`
//!     (`pthread_jit_write_protect_np` on Darwin arm64, no-op
//!     elsewhere).
//! Phase 7.1.3 (arm64 native backend) is the consumer; Phase 7.0
//! only proves the symbols link and the no-op paths return cleanly.

pub mod error;
pub mod unix;

pub use error::SyscallError;
pub use unix::NelispStat;

use libc::{c_char, c_int, mode_t, off_t, size_t, ssize_t};

// ---------------------------------------------------------------------------
// Constants — re-exported under the NeLisp prefix so NeLisp side has a
// single source of truth without depending on libc per host.
// ---------------------------------------------------------------------------

#[no_mangle]
pub static NELISP_PROT_NONE: c_int = libc::PROT_NONE;
#[no_mangle]
pub static NELISP_PROT_READ: c_int = libc::PROT_READ;
#[no_mangle]
pub static NELISP_PROT_WRITE: c_int = libc::PROT_WRITE;
#[no_mangle]
pub static NELISP_PROT_EXEC: c_int = libc::PROT_EXEC;

#[no_mangle]
pub static NELISP_MAP_PRIVATE: c_int = libc::MAP_PRIVATE;
#[no_mangle]
pub static NELISP_MAP_ANONYMOUS: c_int = libc::MAP_ANON;

// `NELISP_MAP_JIT` is defined in `unix.rs` so the per-OS `cfg` lives
// next to the rest of the unix mod; we re-export it here so callers
// reach it via the same `syscall::*` path as other constants.
pub use unix::NELISP_MAP_JIT;

#[no_mangle]
pub static NELISP_O_RDONLY: c_int = libc::O_RDONLY;
#[no_mangle]
pub static NELISP_O_WRONLY: c_int = libc::O_WRONLY;
#[no_mangle]
pub static NELISP_O_RDWR: c_int = libc::O_RDWR;
#[no_mangle]
pub static NELISP_O_CREAT: c_int = libc::O_CREAT;
#[no_mangle]
pub static NELISP_O_TRUNC: c_int = libc::O_TRUNC;
#[no_mangle]
pub static NELISP_O_APPEND: c_int = libc::O_APPEND;

// ---------------------------------------------------------------------------
// FFI-stable extern "C" re-exports.  These are the dlsym names NeLisp
// will resolve in Phase 7.5.  Phase 7.0 only requires them to exist
// in the cdylib symbol table; the smoke binary calls them directly.
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_read(
    fd: c_int,
    buf: *mut u8,
    len: size_t,
) -> ssize_t {
    unix::read(fd, buf, len)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_write(
    fd: c_int,
    buf: *const u8,
    len: size_t,
) -> ssize_t {
    unix::write(fd, buf, len)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_open(
    path: *const c_char,
    flags: c_int,
    mode: mode_t,
) -> c_int {
    unix::open(path, flags, mode)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_close(fd: c_int) -> c_int {
    unix::close(fd)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_mmap(
    addr: *mut u8,
    len: size_t,
    prot: c_int,
    flags: c_int,
    fd: c_int,
    off: off_t,
) -> *mut u8 {
    unix::mmap(addr, len, prot, flags, fd, off)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_munmap(addr: *mut u8, len: size_t) -> c_int {
    unix::munmap(addr, len)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_mmap_jit(
    addr: *mut u8,
    len: size_t,
    prot: c_int,
    flags: c_int,
    fd: c_int,
    off: off_t,
) -> *mut u8 {
    unix::mmap_jit(addr, len, prot, flags, fd, off)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_clear_icache(start: *mut u8, end: *mut u8) -> c_int {
    unix::clear_icache(start, end)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_jit_write_protect(enabled: c_int) -> c_int {
    unix::jit_write_protect(enabled)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_mprotect(
    addr: *mut u8,
    len: size_t,
    prot: c_int,
) -> c_int {
    unix::mprotect(addr, len, prot)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_exit(status: c_int) -> ! {
    unix::exit(status)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_getenv(name: *const c_char) -> *const c_char {
    unix::getenv(name)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_setenv(
    name: *const c_char,
    value: *const c_char,
    overwrite: c_int,
) -> c_int {
    unix::setenv(name, value, overwrite)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_stat(
    path: *const c_char,
    out: *mut NelispStat,
) -> c_int {
    unix::stat(path, out)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_syscall_fstat(fd: c_int, out: *mut NelispStat) -> c_int {
    unix::fstat(fd, out)
}
