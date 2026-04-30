//! `nelisp-runtime` CLI smoke binary (Phase 7.0).
//!
//! `--syscall-smoke` exercises a representative slice of the FFI
//! surface end-to-end so the ERT smoke layer (Doc 27 §3 7.0 gate) can
//! prove the cdylib symbols actually link and run, without committing
//! Phase 7.0 to a fully working Elisp ↔ Rust FFI bridge (that lands
//! in Phase 7.5).
//!
//! Doc 49 Phase 49.2 keeps the raw native-code `exec-bytes` developer
//! bridge out of this Rust-min runtime core.  Use the sibling
//! `nelisp-exec-bytes` binary from `nelisp-runtime-cli` for that path.
//!
//! Exit codes:
//!   0 — all probes green
//!   1 — write probe failed
//!   2 — usage / unknown subcommand
//!   3 — mmap / munmap probe failed
//!   4 — getenv probe failed
//!   5 — stat probe failed

use std::env;
use std::ffi::CString;

fn syscall_smoke() -> i32 {
    // 1. write to stdout via the nelisp_syscall_write FFI symbol
    let msg: &[u8] = b"nelisp-runtime syscall smoke OK\n";
    unsafe {
        let n = nelisp_runtime::nelisp_syscall_write(1, msg.as_ptr(), msg.len());
        if n != msg.len() as isize {
            return 1;
        }
    }

    // 2. mmap an anonymous private 4 KiB page, touch it, munmap.  This
    //    is the path Phase 7.2 (allocator) and Phase 7.1 (native code
    //    pages via PROT_EXEC) will both inherit, so we want a hard
    //    failure here if the symbol or constants are wrong.
    unsafe {
        let prot = nelisp_runtime::NELISP_PROT_READ | nelisp_runtime::NELISP_PROT_WRITE;
        let flags = nelisp_runtime::NELISP_MAP_PRIVATE | nelisp_runtime::NELISP_MAP_ANONYMOUS;
        let p = nelisp_runtime::nelisp_syscall_mmap(
            std::ptr::null_mut(),
            4096,
            prot,
            flags,
            -1,
            0,
        );
        if p.is_null() || p as isize == -1 {
            return 3;
        }
        // Touch first byte to prove the page is actually mapped, then
        // release it.
        *p = 0x42;
        if nelisp_runtime::nelisp_syscall_munmap(p, 4096) != 0 {
            return 3;
        }
    }

    // 3. getenv("PATH") — should not be NULL on any reasonable host
    //    that runs `cargo test` or `make test-runtime`.
    unsafe {
        let name = CString::new("PATH").unwrap();
        let v = nelisp_runtime::nelisp_syscall_getenv(name.as_ptr());
        if v.is_null() {
            return 4;
        }
    }

    // 4. stat the binary itself.  argv[0] may not be an absolute path,
    //    but `/proc/self/exe` (Linux) or `current_exe` (cross-platform)
    //    reliably points at a file the kernel just executed.
    unsafe {
        if let Ok(exe) = env::current_exe() {
            let cpath = CString::new(exe.to_string_lossy().as_bytes()).unwrap();
            let mut sb = nelisp_runtime::NelispStat::default();
            let r = nelisp_runtime::nelisp_syscall_stat(cpath.as_ptr(), &mut sb);
            if r != 0 || sb.st_size <= 0 {
                return 5;
            }
        }
    }

    0
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let code = match args.get(1).map(|s| s.as_str()) {
        Some("--syscall-smoke") => syscall_smoke(),
        Some("--version") => {
            println!("nelisp-runtime {}", env!("CARGO_PKG_VERSION"));
            0
        }
        _ => {
            eprintln!("usage: nelisp-runtime --syscall-smoke");
            eprintln!("       nelisp-runtime --version");
            eprintln!("");
            eprintln!("note: image format commands moved to nelisp-build-tool;");
            eprintln!("      exec-bytes moved to nelisp-exec-bytes");
            eprintln!("      (compile-image / eval-image — `nelisp ...').");
            2
        }
    };
    std::process::exit(code);
}
