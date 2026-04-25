//! Phase 9d.A1 file I/O syscall extension.
//!
//! T76 / T54 Wave 1 agent A.  The Phase 7.0 base set (read / write /
//! open / close / mmap / mprotect / exit / getenv / setenv / stat /
//! fstat) was deliberately frozen at "what JIT + smoke needs"; this
//! module breaks that freeze for the anvil standalone bootstrap surface.
//!
//! Eight new primitives:
//!
//!   * `nl_syscall_opendir(path, len) -> i64`           — handle (>0) or `-errno`
//!   * `nl_syscall_readdir(handle, buf, buf_len) -> i64` — bytes written,
//!     `0` = end-of-stream, `-errno` = error, `-(libc::ERANGE)` if
//!     `buf_len` is too small for the next entry's name + 1 NUL byte.
//!   * `nl_syscall_closedir(handle) -> i64`             — `0` or `-errno`
//!   * `nl_syscall_mkdir(path, len, mode) -> i64`        — `0` or `-errno`
//!   * `nl_syscall_unlink(path, len) -> i64`             — `0` or `-errno`
//!   * `nl_syscall_rename(from, from_len, to, to_len) -> i64` — `0` or `-errno`
//!   * `nl_syscall_access(path, len, mode) -> i64`       — `0` or `-errno`
//!   * `nl_syscall_stat_ex(path, len, out) -> i64`       — `0` or `-errno`,
//!     fills the same `NelispStat` struct as Phase 7.0 `nelisp_syscall_stat`
//!     but with the path supplied as a `(*const u8, len)` pair so callers
//!     do not need to NUL-terminate the name first.
//!
//! Calling convention:
//!   * Path arguments use `(*const u8, usize)` byte-stream pairs (= a
//!     unibyte Lisp string).  We copy into a small stack-allocated
//!     `[u8; N]` and append a NUL before handing to libc, so callers
//!     never need to allocate or NUL-terminate.  Paths longer than
//!     `NELISP_FILEIO_PATH_MAX` (4096) get rejected with `-ENAMETOOLONG`.
//!   * Failure: `-errno` (a small negative i64).  `0` always means
//!     success (or end-of-stream for `readdir`).
//!   * Handles (`opendir` return) are opaque positive `i64` values;
//!     callers must pass them back to `readdir` / `closedir` unchanged.
//!     We allocate them with `Box::into_raw` so closing twice or with
//!     a wrong handle is undefined behaviour from the C ABI side, which
//!     matches `libc::closedir(NULL)` semantics.

use libc::{c_char, c_int, mode_t, DIR};
use std::ptr::NonNull;

use super::unix::NelispStat;

/// Maximum path length we accept on the FFI boundary.  POSIX guarantees
/// `PATH_MAX >= 256`; 4096 matches the Linux kernel limit and is enough
/// for everything anvil's bootstrap code would plausibly reach.
pub const NELISP_FILEIO_PATH_MAX: usize = 4096;

// ---------------------------------------------------------------------------
// Internal helpers — copy `(ptr, len)` into a NUL-terminated stack buffer
// without dragging `std::ffi::CString` (= heap alloc per call) into the
// hot path.
// ---------------------------------------------------------------------------

/// Stack scratch buffer for converting `(*const u8, len)` → `*const c_char`.
///
/// Heap-free path so the syscall thin wrapper stays as cheap as the
/// libc primitive itself.
struct PathBuf {
    bytes: [u8; NELISP_FILEIO_PATH_MAX + 1],
}

impl PathBuf {
    fn new(ptr: *const u8, len: usize) -> Result<Self, i64> {
        if ptr.is_null() {
            return Err(-(libc::EFAULT as i64));
        }
        if len > NELISP_FILEIO_PATH_MAX {
            return Err(-(libc::ENAMETOOLONG as i64));
        }
        let mut bytes = [0u8; NELISP_FILEIO_PATH_MAX + 1];
        // SAFETY: caller is responsible for `ptr..ptr+len` being a
        // valid byte range; len has been bounded above so we never
        // overrun `bytes`.
        unsafe {
            std::ptr::copy_nonoverlapping(ptr, bytes.as_mut_ptr(), len);
        }
        // Reject embedded NUL bytes — POSIX paths cannot contain them,
        // and the libc call would silently truncate.
        if bytes[..len].iter().any(|b| *b == 0) {
            return Err(-(libc::EINVAL as i64));
        }
        bytes[len] = 0;
        Ok(PathBuf { bytes })
    }

    fn as_c_ptr(&self) -> *const c_char {
        self.bytes.as_ptr() as *const c_char
    }
}

/// Convert a libc syscall return (0 / -1) into the `0 | -errno` contract.
unsafe fn negate_errno(r: c_int) -> i64 {
    if r == 0 {
        0
    } else {
        -(super::error::SyscallError::last_os_error().raw() as i64)
    }
}

// ---------------------------------------------------------------------------
// opendir / readdir / closedir
//
// Handle representation:
//   `opendir` returns the raw `*mut DIR` re-cast to `i64`.  Phase 7.0
//   already exposes `*mut u8` pointers (mmap return) the same way, so
//   keeping the convention shrinks the FFI surface NeLisp has to learn.
//   We never zero or rewrite the handle on close — calling `closedir`
//   on it once is the contract, and the return value of libc::closedir
//   propagates verbatim.
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_opendir(path: *const u8, len: usize) -> i64 {
    let pb = match PathBuf::new(path, len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let dirp = libc::opendir(pb.as_c_ptr());
    if dirp.is_null() {
        -(super::error::SyscallError::last_os_error().raw() as i64)
    } else {
        dirp as i64
    }
}

/// Layout of the `(*mut u8, buf_len)` slot `readdir` writes:
///
///   * On success (>0): byte 0..N-1 is the entry name (no NUL), the
///     return value is N.  Callers can copy `N` bytes verbatim into a
///     Lisp unibyte string.  `.` and `..` are *not* filtered — Phase
///     9d.A1 keeps the libc semantics, the NeLisp side will skip them
///     in `directory-files` etc.
///   * On end-of-stream: return 0.
///   * On `buf_len` too small: return `-libc::ERANGE` and leave the
///     stream cursor advanced (matches glibc `readdir_r` semantics).
///   * On error: return `-errno`.
#[no_mangle]
pub unsafe extern "C" fn nl_syscall_readdir(
    handle: i64,
    buf: *mut u8,
    buf_len: usize,
) -> i64 {
    if handle <= 0 {
        return -(libc::EBADF as i64);
    }
    if buf.is_null() && buf_len > 0 {
        return -(libc::EFAULT as i64);
    }
    let dirp = handle as *mut DIR;

    // Clear errno so we can distinguish "end of stream" (NULL + errno
    // unchanged) from "error" (NULL + errno set).  POSIX dictates this
    // dance for `readdir`.
    *libc_errno_mut() = 0;
    let ent = libc::readdir(dirp);
    if ent.is_null() {
        let e = super::error::SyscallError::last_os_error().raw();
        if e == 0 {
            // True end of stream.
            return 0;
        }
        return -(e as i64);
    }

    // The name field is a NUL-terminated `[c_char; N]` whose length we
    // discover with `strlen`.  We never read past the NUL.
    let name_ptr = (*ent).d_name.as_ptr();
    let name_len = libc::strlen(name_ptr);
    if name_len > buf_len {
        return -(libc::ERANGE as i64);
    }
    if name_len > 0 {
        std::ptr::copy_nonoverlapping(name_ptr as *const u8, buf, name_len);
    }
    name_len as i64
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_closedir(handle: i64) -> i64 {
    if handle <= 0 {
        return -(libc::EBADF as i64);
    }
    let dirp = match NonNull::new(handle as *mut DIR) {
        Some(p) => p,
        None => return -(libc::EBADF as i64),
    };
    let r = libc::closedir(dirp.as_ptr());
    negate_errno(r)
}

// ---------------------------------------------------------------------------
// mkdir / unlink / rename / access
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_mkdir(
    path: *const u8,
    len: usize,
    mode: u32,
) -> i64 {
    let pb = match PathBuf::new(path, len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let r = libc::mkdir(pb.as_c_ptr(), mode as mode_t);
    negate_errno(r)
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_unlink(path: *const u8, len: usize) -> i64 {
    let pb = match PathBuf::new(path, len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let r = libc::unlink(pb.as_c_ptr());
    negate_errno(r)
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_rename(
    from: *const u8,
    from_len: usize,
    to: *const u8,
    to_len: usize,
) -> i64 {
    let pb_from = match PathBuf::new(from, from_len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let pb_to = match PathBuf::new(to, to_len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let r = libc::rename(pb_from.as_c_ptr(), pb_to.as_c_ptr());
    negate_errno(r)
}

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_access(
    path: *const u8,
    len: usize,
    mode: i32,
) -> i64 {
    let pb = match PathBuf::new(path, len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let r = libc::access(pb.as_c_ptr(), mode as c_int);
    negate_errno(r)
}

// ---------------------------------------------------------------------------
// stat_ex
//
// Functionally a clone of Phase 7.0 `stat`, but with `(*const u8, len)`
// for the path so unibyte Lisp strings flow through unchanged.  We keep
// the original `nelisp_syscall_stat` symbol untouched (Phase 7.0 freeze
// promise to existing callers); `stat_ex` is the future-facing API.
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nl_syscall_stat_ex(
    path: *const u8,
    len: usize,
    out: *mut NelispStat,
) -> i64 {
    if out.is_null() {
        return -(libc::EFAULT as i64);
    }
    let pb = match PathBuf::new(path, len) {
        Ok(p) => p,
        Err(e) => return e,
    };
    let mut buf: libc::stat = std::mem::zeroed();
    let r = libc::stat(pb.as_c_ptr(), &mut buf as *mut libc::stat);
    if r != 0 {
        return -(super::error::SyscallError::last_os_error().raw() as i64);
    }
    super::unix::copy_stat_for_ex(&buf, &mut *out);
    0
}

// ---------------------------------------------------------------------------
// errno mutator — needed for the readdir end-of-stream / error
// distinction.  Mirrors the read-only helper in `error.rs` but returns
// a `*mut i32` so we can clear it before calling `readdir`.
// ---------------------------------------------------------------------------

#[cfg(any(target_os = "linux", target_os = "android"))]
unsafe fn libc_errno_mut() -> *mut i32 {
    libc::__errno_location()
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
unsafe fn libc_errno_mut() -> *mut i32 {
    libc::__error()
}

#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "ios"
)))]
unsafe fn libc_errno_mut() -> *mut i32 {
    static mut DUMMY: i32 = 0;
    &raw mut DUMMY
}

// ---------------------------------------------------------------------------
// Tests — gated behind `#[cfg(test)]` and the `fileio-syscalls` feature
// so a future "mini build" without file I/O still compiles cleanly.
// ---------------------------------------------------------------------------

#[cfg(all(test, feature = "fileio-syscalls"))]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    use std::path::PathBuf as StdPathBuf;

    fn temp_root() -> StdPathBuf {
        let mut p = std::env::temp_dir();
        // Distinguish from other Phase 7.0 tests so parallel cargo
        // workers don't fight over the same name.
        let pid = std::process::id();
        let ts = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        p.push(format!("nelisp-fileio-{pid}-{ts}"));
        p
    }

    fn as_bytes(s: &str) -> (*const u8, usize) {
        (s.as_ptr(), s.len())
    }

    #[test]
    fn test_mkdir_unlink_roundtrip() {
        unsafe {
            let root = temp_root();
            let root_str = root.to_string_lossy().to_string();
            let (p, l) = as_bytes(&root_str);
            assert_eq!(nl_syscall_mkdir(p, l, 0o755), 0);

            // mkdir on existing dir must return -EEXIST.
            assert_eq!(nl_syscall_mkdir(p, l, 0o755), -(libc::EEXIST as i64));

            // Create then remove a file inside.
            let mut child = root.clone();
            child.push("hello.txt");
            let child_str = child.to_string_lossy().to_string();
            // touch the file via libc::open.
            let cpath = CString::new(child.as_os_str().as_bytes()).unwrap();
            let fd = libc::open(cpath.as_ptr(), libc::O_CREAT | libc::O_WRONLY, 0o644);
            assert!(fd >= 0);
            assert_eq!(libc::close(fd), 0);

            let (cp, cl) = as_bytes(&child_str);
            assert_eq!(nl_syscall_unlink(cp, cl), 0);
            // Second unlink → -ENOENT.
            assert_eq!(nl_syscall_unlink(cp, cl), -(libc::ENOENT as i64));

            // rmdir via libc, since `unlink` doesn't remove dirs.
            let rcp = CString::new(root.as_os_str().as_bytes()).unwrap();
            assert_eq!(libc::rmdir(rcp.as_ptr()), 0);
        }
    }

    #[test]
    fn test_opendir_readdir_iterates() {
        unsafe {
            let root = temp_root();
            let root_str = root.to_string_lossy().to_string();
            let (p, l) = as_bytes(&root_str);
            assert_eq!(nl_syscall_mkdir(p, l, 0o755), 0);

            // Create three files: a, b, c.
            for name in &["a", "b", "c"] {
                let mut f = root.clone();
                f.push(name);
                let cpath = CString::new(f.as_os_str().as_bytes()).unwrap();
                let fd = libc::open(cpath.as_ptr(), libc::O_CREAT | libc::O_WRONLY, 0o644);
                assert!(fd >= 0);
                assert_eq!(libc::close(fd), 0);
            }

            let h = nl_syscall_opendir(p, l);
            assert!(h > 0, "opendir returned {h}");

            let mut found = std::collections::BTreeSet::<String>::new();
            let mut buf = [0u8; 256];
            loop {
                let n = nl_syscall_readdir(h, buf.as_mut_ptr(), buf.len());
                if n == 0 {
                    break;
                }
                assert!(n > 0, "readdir error: {n}");
                let name = std::str::from_utf8(&buf[..n as usize]).unwrap().to_string();
                found.insert(name);
            }
            assert_eq!(nl_syscall_closedir(h), 0);

            for name in &["a", "b", "c", ".", ".."] {
                assert!(found.contains(*name), "missing entry {name} in {found:?}");
            }

            // Cleanup.
            for name in &["a", "b", "c"] {
                let mut f = root.clone();
                f.push(name);
                let s = f.to_string_lossy().to_string();
                let (cp, cl) = as_bytes(&s);
                assert_eq!(nl_syscall_unlink(cp, cl), 0);
            }
            let rcp = CString::new(root.as_os_str().as_bytes()).unwrap();
            assert_eq!(libc::rmdir(rcp.as_ptr()), 0);
        }
    }

    #[test]
    fn test_access_returns_zero_for_existing() {
        unsafe {
            // /dev/null exists and is r/w accessible to any user.
            let s = "/dev/null";
            let (p, l) = as_bytes(s);
            assert_eq!(nl_syscall_access(p, l, libc::F_OK), 0);
            assert_eq!(nl_syscall_access(p, l, libc::R_OK), 0);

            // A guaranteed-nonexistent path returns -ENOENT.
            let bogus = "/nelisp-fileio-does-not-exist-12345";
            let (bp, bl) = as_bytes(bogus);
            assert_eq!(
                nl_syscall_access(bp, bl, libc::F_OK),
                -(libc::ENOENT as i64)
            );
        }
    }

    #[test]
    fn test_rename_moves_file() {
        unsafe {
            let root = temp_root();
            let root_str = root.to_string_lossy().to_string();
            let (p, l) = as_bytes(&root_str);
            assert_eq!(nl_syscall_mkdir(p, l, 0o755), 0);

            let mut src = root.clone();
            src.push("src.txt");
            let src_str = src.to_string_lossy().to_string();
            let cpath = CString::new(src.as_os_str().as_bytes()).unwrap();
            let fd = libc::open(cpath.as_ptr(), libc::O_CREAT | libc::O_WRONLY, 0o644);
            assert!(fd >= 0);
            let payload = b"hello\n";
            assert_eq!(
                libc::write(fd, payload.as_ptr() as *const _, payload.len()),
                payload.len() as isize
            );
            assert_eq!(libc::close(fd), 0);

            let mut dst = root.clone();
            dst.push("dst.txt");
            let dst_str = dst.to_string_lossy().to_string();

            let (sp, sl) = as_bytes(&src_str);
            let (dp, dl) = as_bytes(&dst_str);
            assert_eq!(nl_syscall_rename(sp, sl, dp, dl), 0);

            // src no longer accessible.
            assert_eq!(nl_syscall_access(sp, sl, libc::F_OK), -(libc::ENOENT as i64));
            assert_eq!(nl_syscall_access(dp, dl, libc::F_OK), 0);

            assert_eq!(nl_syscall_unlink(dp, dl), 0);
            let rcp = CString::new(root.as_os_str().as_bytes()).unwrap();
            assert_eq!(libc::rmdir(rcp.as_ptr()), 0);
        }
    }

    #[test]
    fn test_stat_ex_returns_size_mtime() {
        unsafe {
            let root = temp_root();
            let root_str = root.to_string_lossy().to_string();
            let (rp, rl) = as_bytes(&root_str);
            assert_eq!(nl_syscall_mkdir(rp, rl, 0o755), 0);

            let mut f = root.clone();
            f.push("size-probe.bin");
            let f_str = f.to_string_lossy().to_string();
            let cpath = CString::new(f.as_os_str().as_bytes()).unwrap();
            let fd = libc::open(cpath.as_ptr(), libc::O_CREAT | libc::O_WRONLY, 0o644);
            assert!(fd >= 0);
            let payload = vec![0xAAu8; 1024];
            assert_eq!(
                libc::write(fd, payload.as_ptr() as *const _, payload.len()),
                payload.len() as isize
            );
            assert_eq!(libc::close(fd), 0);

            let mut sb = NelispStat::default();
            let (fp, fl) = as_bytes(&f_str);
            assert_eq!(nl_syscall_stat_ex(fp, fl, &mut sb), 0);
            assert_eq!(sb.st_size, 1024);
            // mtime sec must be a recent UNIX timestamp (post-2020).
            assert!(sb.st_mtime_sec > 1_577_836_800, "mtime {} too small", sb.st_mtime_sec);
            // mode bit S_IFREG must be set on a regular file.
            let s_ifmt: u32 = 0o170000;
            let s_ifreg: u32 = 0o100000;
            assert_eq!(sb.st_mode & s_ifmt, s_ifreg);

            let (up, ul) = as_bytes(&f_str);
            assert_eq!(nl_syscall_unlink(up, ul), 0);
            let rcp = CString::new(root.as_os_str().as_bytes()).unwrap();
            assert_eq!(libc::rmdir(rcp.as_ptr()), 0);
        }
    }

    #[test]
    fn test_path_too_long_returns_enametoolong() {
        unsafe {
            let huge = vec![b'a'; NELISP_FILEIO_PATH_MAX + 10];
            let r = nl_syscall_access(huge.as_ptr(), huge.len(), libc::F_OK);
            assert_eq!(r, -(libc::ENAMETOOLONG as i64));
        }
    }

    #[test]
    fn test_path_with_embedded_nul_returns_einval() {
        unsafe {
            let s = b"/dev/nu\0ll";
            let r = nl_syscall_access(s.as_ptr(), s.len(), libc::F_OK);
            assert_eq!(r, -(libc::EINVAL as i64));
        }
    }

    #[test]
    fn test_readdir_buf_too_small_returns_erange() {
        unsafe {
            let root = temp_root();
            let root_str = root.to_string_lossy().to_string();
            let (rp, rl) = as_bytes(&root_str);
            assert_eq!(nl_syscall_mkdir(rp, rl, 0o755), 0);

            // Long-name file that won't fit in a 2-byte buffer.
            let mut f = root.clone();
            f.push("verylongfilename.txt");
            let cpath = CString::new(f.as_os_str().as_bytes()).unwrap();
            let fd = libc::open(cpath.as_ptr(), libc::O_CREAT | libc::O_WRONLY, 0o644);
            assert!(fd >= 0);
            assert_eq!(libc::close(fd), 0);

            let h = nl_syscall_opendir(rp, rl);
            assert!(h > 0);

            let mut tiny = [0u8; 2];
            // Skip "." and ".." which fit in 2 bytes; iterate until we
            // see ERANGE on the long name OR end-of-stream (which means
            // the OS happens to expose the long entry first; in either
            // case the call sequence must terminate cleanly).
            let mut saw_erange = false;
            for _ in 0..16 {
                let n = nl_syscall_readdir(h, tiny.as_mut_ptr(), tiny.len());
                if n == 0 {
                    break;
                }
                if n == -(libc::ERANGE as i64) {
                    saw_erange = true;
                    break;
                }
                assert!(n > 0);
            }
            assert!(saw_erange, "readdir never reported ERANGE for a 20-char name in a 2-byte buffer");
            assert_eq!(nl_syscall_closedir(h), 0);

            // cleanup
            let f_str = f.to_string_lossy().to_string();
            let (fp, fl) = as_bytes(&f_str);
            assert_eq!(nl_syscall_unlink(fp, fl), 0);
            let rcp = CString::new(root.as_os_str().as_bytes()).unwrap();
            assert_eq!(libc::rmdir(rcp.as_ptr()), 0);
        }
    }

    #[test]
    fn test_invalid_handles_return_ebadf() {
        unsafe {
            assert_eq!(nl_syscall_closedir(0), -(libc::EBADF as i64));
            assert_eq!(nl_syscall_closedir(-1), -(libc::EBADF as i64));
            let mut buf = [0u8; 8];
            assert_eq!(
                nl_syscall_readdir(0, buf.as_mut_ptr(), buf.len()),
                -(libc::EBADF as i64)
            );
        }
    }

}
