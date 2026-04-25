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

pub unsafe fn munmap(addr: *mut u8, len: size_t) -> c_int {
    libc::munmap(addr as *mut c_void, len)
}

pub unsafe fn mprotect(addr: *mut u8, len: size_t, prot: c_int) -> c_int {
    libc::mprotect(addr as *mut c_void, len, prot)
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
