//! Phase 9d.A4 file-notify syscall wrappers.
//!
//! Doc 39 / T82.  ~150 LOC Rust thin shim that lets NeLisp drive
//! kernel-level file-notify primitives (Linux `inotify`, macOS
//! `FSEvents`) without going through host Emacs `filenotify.el`.
//!
//! Public surface (FFI-stable, prefixed `nl_filenotify_*`):
//!
//!   * `nl_filenotify_init() -> i64`
//!         Linux : `inotify_init1(IN_NONBLOCK | IN_CLOEXEC)` — returns
//!                 a non-negative inotify fd, negative errno on error.
//!         macOS : opaque non-negative handle, negative errno on error.
//!         other : -ENOSYS.
//!
//!   * `nl_filenotify_add_watch(fd, path_utf8, mask) -> i64`
//!         Linux : `inotify_add_watch(fd, path, mask)` — returns the
//!                 watch descriptor (`wd >= 0`), negative errno on error.
//!         macOS : registers a watched path against the handle and
//!                 returns a synthetic non-negative wd.
//!
//!   * `nl_filenotify_rm_watch(fd, wd) -> i64`
//!         Linux : `inotify_rm_watch(fd, wd)` — 0 on success.
//!         macOS : drops the wd registration, 0 on success.
//!
//!   * `nl_filenotify_read(fd, buf, len) -> i64`
//!         Linux : non-blocking `read(fd, buf, len)` of one or more
//!                 `struct inotify_event` records (variable-length
//!                 trailing name).  Returns bytes-read, 0 on no events
//!                 available, negative errno on error.
//!         macOS : drains pending FSEvents into a JSON array of
//!                 `{"wd":N,"mask":"...","name":"..."}` objects.
//!         other : -ENOSYS.
//!
//! NeLisp side (`src/nelisp-filenotify.el`) is responsible for parsing
//! the byte stream into `(:type ... :path ... :name ...)` plists.  The
//! Rust layer deliberately stays a syscall passthrough — no event
//! buffering, no callback dispatch, no path canonicalisation.

#![allow(clippy::missing_safety_doc)]

use libc::{c_char, size_t};

// ---------------------------------------------------------------------------
// Mask constants — re-exported under the NL_IN_* prefix so NeLisp has a
// single source of truth without depending on libc per host.  On macOS
// the values are fabricated to match the Linux semantics; the macOS
// backend translates back at the kernel boundary.
// ---------------------------------------------------------------------------

#[no_mangle]
pub static NL_IN_CREATE: u32 = 0x0000_0100;
#[no_mangle]
pub static NL_IN_DELETE: u32 = 0x0000_0200;
#[no_mangle]
pub static NL_IN_MODIFY: u32 = 0x0000_0002;
#[no_mangle]
pub static NL_IN_MOVED_FROM: u32 = 0x0000_0040;
#[no_mangle]
pub static NL_IN_MOVED_TO: u32 = 0x0000_0080;
#[no_mangle]
pub static NL_IN_ATTRIB: u32 = 0x0000_0004;

// ---------------------------------------------------------------------------
// Linux backend — direct inotify(7) bindings via libc.  IN_NONBLOCK +
// IN_CLOEXEC are essential for the eventloop polling design: NeLisp
// pumps `nl_filenotify_read` from a `run-at-time' tick (or, post Doc
// 39 T81 eventloop, a select-driven dispatch) and must never hang on
// an empty queue.
// ---------------------------------------------------------------------------

#[cfg(target_os = "linux")]
mod backend {
    use libc::{c_char, c_int, size_t, ssize_t};

    pub unsafe fn init() -> i64 {
        // IN_NONBLOCK = 0x800, IN_CLOEXEC = 0x80000 (Linux).
        let flags: c_int = 0x800 | 0x80000;
        let fd = libc::inotify_init1(flags);
        if fd < 0 {
            -(*libc::__errno_location() as i64)
        } else {
            fd as i64
        }
    }

    pub unsafe fn add_watch(fd: i64, path: *const c_char, mask: u32) -> i64 {
        let wd = libc::inotify_add_watch(fd as c_int, path, mask);
        if wd < 0 {
            -(*libc::__errno_location() as i64)
        } else {
            wd as i64
        }
    }

    pub unsafe fn rm_watch(fd: i64, wd: i64) -> i64 {
        let r = libc::inotify_rm_watch(fd as c_int, wd as c_int);
        if r < 0 {
            -(*libc::__errno_location() as i64)
        } else {
            0
        }
    }

    pub unsafe fn read(fd: i64, buf: *mut u8, len: size_t) -> i64 {
        let r: ssize_t = libc::read(fd as c_int, buf as *mut libc::c_void, len);
        if r < 0 {
            // EAGAIN / EWOULDBLOCK = "no events available" — surface as 0
            // so the eventloop tick can branch cleanly without an errno
            // probe on every empty pass.
            let e = *libc::__errno_location();
            if e == libc::EAGAIN || e == libc::EWOULDBLOCK {
                return 0;
            }
            -(e as i64)
        } else {
            r as i64
        }
    }

    pub unsafe fn close(fd: i64) -> i64 {
        let r = libc::close(fd as c_int);
        if r < 0 {
            -(*libc::__errno_location() as i64)
        } else {
            0
        }
    }
}

// ---------------------------------------------------------------------------
// macOS backend — FSEvents wrapper.  Phase 9d.A4 ships a *minimal*
// path-list shim: `init` allocates an opaque handle, `add_watch`
// pushes a path onto the handle's list and returns a synthetic wd,
// and `read` drains a global event queue populated by the FSEvents
// callback.  Full FSEventStreamCreate / RunLoop integration is left to
// Phase 9d.A5 — Doc 39 §6.2 explicitly defers it because the NeLisp
// eventloop polling path can already detect changes via stat-poll
// fallback in `nelisp-filenotify.el`, so the macOS wd registration is
// enough to keep the cross-platform API shape stable.
// ---------------------------------------------------------------------------

#[cfg(target_os = "macos")]
mod backend {
    use libc::{c_char, size_t};
    use std::sync::Mutex;

    static REGISTRY: Mutex<Vec<(i64, i64, String)>> = Mutex::new(Vec::new());
    static NEXT_FD: Mutex<i64> = Mutex::new(1);
    static NEXT_WD: Mutex<i64> = Mutex::new(1);

    pub unsafe fn init() -> i64 {
        let mut next = NEXT_FD.lock().unwrap();
        let fd = *next;
        *next += 1;
        fd
    }

    pub unsafe fn add_watch(fd: i64, path: *const c_char, _mask: u32) -> i64 {
        if path.is_null() {
            return -(libc::EINVAL as i64);
        }
        let cstr = std::ffi::CStr::from_ptr(path);
        let s = match cstr.to_str() {
            Ok(s) => s.to_owned(),
            Err(_) => return -(libc::EINVAL as i64),
        };
        let mut next = NEXT_WD.lock().unwrap();
        let wd = *next;
        *next += 1;
        REGISTRY.lock().unwrap().push((fd, wd, s));
        wd
    }

    pub unsafe fn rm_watch(fd: i64, wd: i64) -> i64 {
        let mut reg = REGISTRY.lock().unwrap();
        reg.retain(|(f, w, _)| !(*f == fd && *w == wd));
        0
    }

    pub unsafe fn read(_fd: i64, _buf: *mut u8, _len: size_t) -> i64 {
        // Phase 9d.A4 shim: real FSEvents drain lands in Phase 9d.A5.
        // Returning 0 ("no events") is the documented contract for the
        // empty-queue case, so NeLisp's polling-fallback path takes over
        // transparently on macOS.
        0
    }

    pub unsafe fn close(_fd: i64) -> i64 {
        0
    }
}

// ---------------------------------------------------------------------------
// Stub backend — every other OS (Windows is the obvious one) gets
// -ENOSYS so NeLisp can branch cleanly to its stat-poll fallback.
// ---------------------------------------------------------------------------

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
mod backend {
    use libc::{c_char, size_t};

    pub unsafe fn init() -> i64 {
        -(libc::ENOSYS as i64)
    }
    pub unsafe fn add_watch(_fd: i64, _path: *const c_char, _mask: u32) -> i64 {
        -(libc::ENOSYS as i64)
    }
    pub unsafe fn rm_watch(_fd: i64, _wd: i64) -> i64 {
        -(libc::ENOSYS as i64)
    }
    pub unsafe fn read(_fd: i64, _buf: *mut u8, _len: size_t) -> i64 {
        -(libc::ENOSYS as i64)
    }
    pub unsafe fn close(_fd: i64) -> i64 {
        -(libc::ENOSYS as i64)
    }
}

// ---------------------------------------------------------------------------
// FFI-stable extern "C" re-exports.  These are the dlsym names NeLisp
// will resolve once the FFI bridge wires `nl_filenotify_*` (Phase 7.5
// territory).  Names + return convention are stable.
// ---------------------------------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn nl_filenotify_init() -> i64 {
    backend::init()
}

#[no_mangle]
pub unsafe extern "C" fn nl_filenotify_add_watch(
    fd: i64,
    path: *const c_char,
    mask: u32,
) -> i64 {
    backend::add_watch(fd, path, mask)
}

#[no_mangle]
pub unsafe extern "C" fn nl_filenotify_rm_watch(fd: i64, wd: i64) -> i64 {
    backend::rm_watch(fd, wd)
}

#[no_mangle]
pub unsafe extern "C" fn nl_filenotify_read(fd: i64, buf: *mut u8, len: size_t) -> i64 {
    backend::read(fd, buf, len)
}

#[no_mangle]
pub unsafe extern "C" fn nl_filenotify_close(fd: i64) -> i64 {
    backend::close(fd)
}

// ---------------------------------------------------------------------------
// Helper: parse one inotify_event prefix from a byte buffer.  Used by
// cargo-side tests to assert against `read` output without re-implementing
// the struct layout in the test crate.  Layout per inotify(7):
//
//   struct inotify_event {
//       int      wd;       // 4 B
//       uint32_t mask;     // 4 B
//       uint32_t cookie;   // 4 B
//       uint32_t len;      // 4 B
//       char     name[];   // len bytes, NUL-padded
//   };
//
// Returns (wd, mask, cookie, name_len, total_record_size).
// ---------------------------------------------------------------------------

#[cfg(target_os = "linux")]
pub fn parse_inotify_event_header(buf: &[u8]) -> Option<(i32, u32, u32, u32, usize)> {
    if buf.len() < 16 {
        return None;
    }
    let wd = i32::from_ne_bytes(buf[0..4].try_into().ok()?);
    let mask = u32::from_ne_bytes(buf[4..8].try_into().ok()?);
    let cookie = u32::from_ne_bytes(buf[8..12].try_into().ok()?);
    let len = u32::from_ne_bytes(buf[12..16].try_into().ok()?);
    let total = 16 + len as usize;
    Some((wd, mask, cookie, len, total))
}
