//! Phase 9d.A4 cargo-side tests for `nl_filenotify_*` FFI surface.
#![cfg(feature = "filenotify-syscalls")]
//!
//! Each test exercises the `extern "C"` symbols at the same dlsym
//! boundary NeLisp will hit, so a regression in linkage / signature /
//! per-OS cfg gating shows up here before reaching the ERT smoke
//! layer.  Linux paths are exercised end-to-end; macOS gets the shim
//! contract tests; non-Linux/non-macOS hosts fall through to the
//! `-ENOSYS` stub branch.
//!
//! Phase 7.0 keeps `Cargo.toml` `libc`-only, so the tests roll their
//! own temp-directory helper instead of pulling the `tempfile` crate.

use std::ffi::CString;

use nelisp_runtime::*;

/// Build a temp dir under `$TMPDIR` (or `/tmp`) and return its path.
/// Caller is responsible for `std::fs::remove_dir_all` cleanup.
fn make_tmpdir(tag: &str) -> std::path::PathBuf {
    let base = std::env::temp_dir();
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.subsec_nanos())
        .unwrap_or(0);
    let dir = base.join(format!("nelisp-fn-cargo-{tag}-{pid}-{nanos}"));
    std::fs::create_dir(&dir).unwrap();
    dir
}

#[test]
fn init_returns_non_negative_handle_on_linux_or_macos() {
    if !cfg!(any(target_os = "linux", target_os = "macos")) {
        unsafe {
            let fd = nl_filenotify_init();
            assert!(fd < 0);
        }
        return;
    }
    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0, "nl_filenotify_init returned {fd}");
        let r = nl_filenotify_close(fd);
        assert_eq!(r, 0, "nl_filenotify_close returned {r}");
    }
}

#[test]
fn add_watch_negative_on_bad_path_linux() {
    if !cfg!(target_os = "linux") {
        return;
    }
    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let bogus = CString::new("/this/path/should/not/exist/ever").unwrap();
        let mask = NL_IN_CREATE | NL_IN_MODIFY | NL_IN_DELETE;
        let wd = nl_filenotify_add_watch(fd, bogus.as_ptr(), mask);
        assert!(wd < 0, "add_watch on bogus path returned {wd}");
        nl_filenotify_close(fd);
    }
}

#[cfg(target_os = "linux")]
#[test]
fn linux_watch_create_event_round_trip() {
    use std::fs::File;
    use std::io::Write;
    use std::time::{Duration, Instant};

    let tmpdir = make_tmpdir("create");
    let dir_c = CString::new(tmpdir.to_str().unwrap()).unwrap();

    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let mask = NL_IN_CREATE | NL_IN_MODIFY;
        let wd = nl_filenotify_add_watch(fd, dir_c.as_ptr(), mask);
        assert!(wd >= 0, "add_watch returned {wd}");

        // Trigger a CREATE event.
        let target = tmpdir.join("phase9d-a4-roundtrip.txt");
        {
            let mut f = File::create(&target).unwrap();
            f.write_all(b"hello").unwrap();
        }

        // Drain events with a short retry budget; inotify is
        // asynchronous w.r.t. file syscalls.
        let deadline = Instant::now() + Duration::from_millis(2_000);
        let mut buf = vec![0u8; 4096];
        let mut last_n = 0i64;
        let mut saw_create = false;
        while Instant::now() < deadline {
            let n = nl_filenotify_read(fd, buf.as_mut_ptr(), buf.len());
            if n > 0 {
                last_n = n;
                let mut off = 0usize;
                while off + 16 <= n as usize {
                    let (_w, m, _c, name_len, total) =
                        nelisp_syscall_filenotify::parse_inotify_event_header(
                            &buf[off..n as usize],
                        )
                        .expect("inotify_event header parse");
                    if m & NL_IN_CREATE != 0 {
                        let name_bytes = &buf[off + 16..off + 16 + name_len as usize];
                        let name = std::ffi::CStr::from_bytes_until_nul(name_bytes)
                            .ok()
                            .and_then(|c| c.to_str().ok())
                            .unwrap_or("");
                        if name == "phase9d-a4-roundtrip.txt" {
                            saw_create = true;
                        }
                    }
                    off += total;
                }
                if saw_create {
                    break;
                }
            } else {
                std::thread::sleep(Duration::from_millis(20));
            }
        }
        assert!(
            saw_create,
            "did not observe IN_CREATE for new file (last read returned {last_n} bytes)"
        );

        let r = nl_filenotify_rm_watch(fd, wd);
        assert_eq!(r, 0);
        nl_filenotify_close(fd);
    }

    let _ = std::fs::remove_dir_all(&tmpdir);
}

#[cfg(target_os = "linux")]
#[test]
fn linux_read_on_empty_queue_returns_zero() {
    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let mut buf = vec![0u8; 4096];
        let n = nl_filenotify_read(fd, buf.as_mut_ptr(), buf.len());
        // Nothing watched → no events queued → EAGAIN → mapped to 0.
        assert_eq!(n, 0, "expected 0 (EAGAIN translated), got {n}");
        nl_filenotify_close(fd);
    }
}

#[cfg(target_os = "linux")]
#[test]
fn linux_rm_watch_after_add_returns_zero() {
    let tmpdir = make_tmpdir("rm");
    let dir_c = CString::new(tmpdir.to_str().unwrap()).unwrap();
    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let wd = nl_filenotify_add_watch(fd, dir_c.as_ptr(), NL_IN_CREATE);
        assert!(wd >= 0);
        let r = nl_filenotify_rm_watch(fd, wd);
        assert_eq!(r, 0);
        nl_filenotify_close(fd);
    }
    let _ = std::fs::remove_dir_all(&tmpdir);
}

#[cfg(target_os = "linux")]
#[test]
fn linux_watch_delete_event_round_trip() {
    use std::fs::File;
    use std::time::{Duration, Instant};

    let tmpdir = make_tmpdir("delete");
    let target = tmpdir.join("victim.txt");
    File::create(&target).unwrap();
    let dir_c = CString::new(tmpdir.to_str().unwrap()).unwrap();

    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let wd = nl_filenotify_add_watch(fd, dir_c.as_ptr(), NL_IN_DELETE);
        assert!(wd >= 0);

        std::fs::remove_file(&target).unwrap();

        let deadline = Instant::now() + Duration::from_millis(2_000);
        let mut buf = vec![0u8; 4096];
        let mut saw_delete = false;
        while Instant::now() < deadline {
            let n = nl_filenotify_read(fd, buf.as_mut_ptr(), buf.len());
            if n > 0 {
                let mut off = 0usize;
                while off + 16 <= n as usize {
                    let (_w, m, _c, name_len, total) =
                        nelisp_syscall_filenotify::parse_inotify_event_header(
                            &buf[off..n as usize],
                        )
                        .expect("inotify_event header parse");
                    if m & NL_IN_DELETE != 0 {
                        let name_bytes = &buf[off + 16..off + 16 + name_len as usize];
                        let name = std::ffi::CStr::from_bytes_until_nul(name_bytes)
                            .ok()
                            .and_then(|c| c.to_str().ok())
                            .unwrap_or("");
                        if name == "victim.txt" {
                            saw_delete = true;
                        }
                    }
                    off += total;
                }
                if saw_delete {
                    break;
                }
            } else {
                std::thread::sleep(Duration::from_millis(20));
            }
        }
        assert!(saw_delete, "did not observe IN_DELETE for deleted file");

        nl_filenotify_rm_watch(fd, wd);
        nl_filenotify_close(fd);
    }

    let _ = std::fs::remove_dir_all(&tmpdir);
}

#[test]
fn mask_constants_have_unique_nonzero_values() {
    let masks = [
        NL_IN_CREATE,
        NL_IN_DELETE,
        NL_IN_MODIFY,
        NL_IN_MOVED_FROM,
        NL_IN_MOVED_TO,
        NL_IN_ATTRIB,
    ];
    for m in &masks {
        assert!(*m != 0, "mask constant unexpectedly zero");
    }
    for i in 0..masks.len() {
        for j in i + 1..masks.len() {
            assert_ne!(masks[i], masks[j], "mask collision {i} vs {j}");
        }
    }
}

#[cfg(target_os = "macos")]
#[test]
fn macos_shim_add_remove_returns_zero() {
    let path = CString::new("/tmp").unwrap();
    unsafe {
        let fd = nl_filenotify_init();
        assert!(fd >= 0);
        let wd = nl_filenotify_add_watch(fd, path.as_ptr(), NL_IN_CREATE);
        assert!(wd >= 0);
        let mut buf = vec![0u8; 4096];
        let n = nl_filenotify_read(fd, buf.as_mut_ptr(), buf.len());
        assert_eq!(n, 0);
        let r = nl_filenotify_rm_watch(fd, wd);
        assert_eq!(r, 0);
        nl_filenotify_close(fd);
    }
}
