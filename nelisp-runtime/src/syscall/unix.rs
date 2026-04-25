//! Phase 7.0 unix syscall thin wrappers.
//!
//! Each function is a one-liner over `libc::*` with the same return
//! convention as the underlying syscall (negative value / `MAP_FAILED`
//! pointer = error, errno set).  No bookkeeping, no caching, no
//! retry-on-EINTR — Phase 7.0 explicitly leaves all that to NeLisp.
//!
//! Phase 7.5 will switch the `extern "C"` re-exports in `mod.rs` to
//! call into these functions through the NeLisp FFI boundary.

use libc::{c_char, c_int, c_void, mode_t, off_t, size_t, ssize_t};

// ---------------------------------------------------------------------------
// MAP_JIT — macOS Apple Silicon JIT page flag (sys/mman.h: 0x800).
//
// Doc 28 v2 §6.9 risk: arm64 native code generation must use MAP_JIT
// pages on Apple Silicon and toggle pthread_jit_write_protect_np(0/1)
// across the write→exec boundary, then `__clear_cache` the produced
// instruction range.  Linux / Windows do not have a MAP_JIT flag, so
// the constant degrades to 0 (no-op) and the OR'd `flags` argument
// stays bit-compatible with vanilla `MAP_PRIVATE | MAP_ANON`.  Phase
// 7.0 only wires the primitives — Phase 7.1.3 (arm64 backend) will
// orchestrate the actual write-protect dance.
// ---------------------------------------------------------------------------

#[cfg(target_os = "macos")]
#[no_mangle]
pub static NELISP_MAP_JIT: c_int = 0x800;

#[cfg(not(target_os = "macos"))]
#[no_mangle]
pub static NELISP_MAP_JIT: c_int = 0;

pub unsafe fn read(fd: c_int, buf: *mut u8, len: size_t) -> ssize_t {
    libc::read(fd, buf as *mut c_void, len)
}

pub unsafe fn write(fd: c_int, buf: *const u8, len: size_t) -> ssize_t {
    libc::write(fd, buf as *const c_void, len)
}

pub unsafe fn open(path: *const c_char, flags: c_int, mode: mode_t) -> c_int {
    libc::open(path, flags, mode as c_int)
}

pub unsafe fn close(fd: c_int) -> c_int {
    libc::close(fd)
}

pub unsafe fn mmap(
    addr: *mut u8,
    len: size_t,
    prot: c_int,
    flags: c_int,
    fd: c_int,
    off: off_t,
) -> *mut u8 {
    libc::mmap(addr as *mut c_void, len, prot, flags, fd, off) as *mut u8
}

/// `mmap` with caller-supplied `flags` already including `NELISP_MAP_JIT`
/// when targeting Apple Silicon.  Functionally identical to `mmap` —
/// the Phase 7.1.3 arm64 backend wants a *named* primitive so the call
/// site documents intent and so a future host-specific tweak (e.g.
/// `MAP_HUGETLB` on Linux JIT pools) has one place to live.
pub unsafe fn mmap_jit(
    addr: *mut u8,
    len: size_t,
    prot: c_int,
    flags: c_int,
    fd: c_int,
    off: off_t,
) -> *mut u8 {
    libc::mmap(addr as *mut c_void, len, prot, flags, fd, off) as *mut u8
}

pub unsafe fn munmap(addr: *mut u8, len: size_t) -> c_int {
    libc::munmap(addr as *mut c_void, len)
}

pub unsafe fn mprotect(addr: *mut u8, len: size_t, prot: c_int) -> c_int {
    libc::mprotect(addr as *mut c_void, len, prot)
}

// ---------------------------------------------------------------------------
// I-cache invalidate — `__clear_cache(start, end)`
//
// On x86_64 the data and instruction caches are coherent, so writing
// a buffer and jumping to it is safe with no extra ceremony.  On
// arm64 (Apple Silicon, Linux aarch64) the architecture explicitly
// permits stale prefetched instructions until an `IC IVAU` / `DSB
// ISH` sequence executes, which `__clear_cache` (a libgcc / compiler
// builtin exposed as a C ABI symbol) wraps portably.  Phase 7.1.3
// will call this immediately after writing a code page and before
// flipping the JIT write-protect bit back to "exec".
// ---------------------------------------------------------------------------

#[cfg(target_arch = "aarch64")]
pub unsafe fn clear_icache(start: *mut u8, end: *mut u8) -> c_int {
    extern "C" {
        fn __clear_cache(start: *mut c_char, end: *mut c_char);
    }
    __clear_cache(start as *mut c_char, end as *mut c_char);
    0
}

#[cfg(target_arch = "x86_64")]
pub unsafe fn clear_icache(_start: *mut u8, _end: *mut u8) -> c_int {
    // x86_64 keeps I-cache coherent with stores.  No-op.
    0
}

#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64")))]
pub unsafe fn clear_icache(_start: *mut u8, _end: *mut u8) -> c_int {
    // Phase 7.0 only targets arm64 + x86_64 hosts.  Other architectures
    // get a stub that returns 0 so callers can still wire the FFI; a
    // future port (riscv64, etc.) will fill the real barrier sequence.
    0
}

// ---------------------------------------------------------------------------
// pthread_jit_write_protect_np(enabled)
//
// macOS Apple Silicon (Darwin arm64) requires every thread that
// writes to a `MAP_JIT` page to first call this with `enabled=0`
// (write allowed, exec disallowed) and toggle back to `enabled=1`
// before executing the freshly-written instructions.  The function
// is a no-op on other platforms — Linux / x86_64 macOS / Windows do
// not implement the W^X-on-the-same-page model — but we still expose
// the symbol so NeLisp's arm64 backend can call it unconditionally
// without a per-host `#ifdef` cascade.
// ---------------------------------------------------------------------------

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
pub unsafe fn jit_write_protect(enabled: c_int) -> c_int {
    extern "C" {
        fn pthread_jit_write_protect_np(enabled: c_int);
    }
    pthread_jit_write_protect_np(enabled);
    0
}

#[cfg(not(all(target_os = "macos", target_arch = "aarch64")))]
pub unsafe fn jit_write_protect(_enabled: c_int) -> c_int {
    // No-op on Linux / Windows / x86_64 macOS.  Those platforms either
    // permit RWX pages outright (Linux without SELinux JIT policy) or
    // expect the caller to use plain `mprotect` to flip permissions.
    0
}

pub unsafe fn exit(status: c_int) -> ! {
    libc::exit(status)
}

pub unsafe fn getenv(name: *const c_char) -> *const c_char {
    libc::getenv(name) as *const c_char
}

pub unsafe fn setenv(name: *const c_char, value: *const c_char, overwrite: c_int) -> c_int {
    libc::setenv(name, value, overwrite)
}

/// Phase 7.0 stat snapshot.
///
/// `#[repr(C)]` so NeLisp can later cast a Rust pointer through the
/// FFI boundary with a fixed layout regardless of the host
/// `struct stat` width on Linux vs macOS.  We deliberately copy only
/// the fields NeLisp file APIs need; full `struct stat` exposure is
/// Phase 7.4 (coding) territory.
#[repr(C)]
#[derive(Debug, Default, Clone, Copy)]
pub struct NelispStat {
    pub st_dev: u64,
    pub st_ino: u64,
    pub st_mode: u32,
    pub st_nlink: u64,
    pub st_uid: u32,
    pub st_gid: u32,
    pub st_size: i64,
    pub st_mtime_sec: i64,
    pub st_mtime_nsec: i64,
}

pub unsafe fn stat(path: *const c_char, out: *mut NelispStat) -> c_int {
    let mut buf: libc::stat = std::mem::zeroed();
    let r = libc::stat(path, &mut buf as *mut libc::stat);
    if r == 0 && !out.is_null() {
        copy_stat(&buf, &mut *out);
    }
    r
}

pub unsafe fn fstat(fd: c_int, out: *mut NelispStat) -> c_int {
    let mut buf: libc::stat = std::mem::zeroed();
    let r = libc::fstat(fd, &mut buf as *mut libc::stat);
    if r == 0 && !out.is_null() {
        copy_stat(&buf, &mut *out);
    }
    r
}

#[cfg(any(target_os = "linux", target_os = "android"))]
unsafe fn copy_stat(buf: &libc::stat, out: &mut NelispStat) {
    out.st_dev = buf.st_dev;
    out.st_ino = buf.st_ino;
    out.st_mode = buf.st_mode;
    out.st_nlink = buf.st_nlink as u64;
    out.st_uid = buf.st_uid;
    out.st_gid = buf.st_gid;
    out.st_size = buf.st_size as i64;
    out.st_mtime_sec = buf.st_mtime as i64;
    out.st_mtime_nsec = buf.st_mtime_nsec as i64;
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
unsafe fn copy_stat(buf: &libc::stat, out: &mut NelispStat) {
    out.st_dev = buf.st_dev as u64;
    out.st_ino = buf.st_ino as u64;
    out.st_mode = buf.st_mode as u32;
    out.st_nlink = buf.st_nlink as u64;
    out.st_uid = buf.st_uid;
    out.st_gid = buf.st_gid;
    out.st_size = buf.st_size as i64;
    out.st_mtime_sec = buf.st_mtime as i64;
    out.st_mtime_nsec = buf.st_mtime_nsec as i64;
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "ios"
)))]
unsafe fn copy_stat(_buf: &libc::stat, out: &mut NelispStat) {
    // Phase 7.5 will fill this for *BSD / Solaris.  Until then we
    // leave the struct zeroed so callers can detect the gap.
    *out = NelispStat::default();
}

/// Phase 9d.A1 re-export of `copy_stat` for the `fileio` module.
///
/// `copy_stat` itself stays private (per-OS cfg variants live next to
/// each other).  `fileio.rs` lives in a sibling module so it can't
/// reach the private helper directly; this thin wrapper delegates to
/// whichever `copy_stat` the host cfg gates picked.  Only compiled
/// when the `fileio-syscalls` feature is on, otherwise the function
/// trips dead-code warnings on the mini build.
#[cfg(feature = "fileio-syscalls")]
pub(crate) unsafe fn copy_stat_for_ex(buf: &libc::stat, out: &mut NelispStat) {
    copy_stat(buf, out)
}
