//! NeLisp Phase 7.0 runtime — Rust syscall stub.
//!
//! Doc 27 §3 7.0: ~300-500 LOC of OS primitive thin wrappers, no
//! logic.  Everything else (allocator / GC / coding / native compiler)
//! lives in NeLisp source per Phase 7.1-7.5.
//!
//! Public surface is intentionally small: 10 syscall thin wrappers
//! re-exported as `nelisp_syscall_*` C ABI symbols, plus a handful
//! of `NELISP_PROT_*` / `NELISP_MAP_*` / `NELISP_O_*` constants so
//! NeLisp does not need a per-OS libc clone.

pub mod anvil_data_registry;
pub mod anvil_host_registry;
pub mod anvil_shell_filter_registry;
pub mod anvil_tools_registry;
pub mod bridge;
pub mod eval;
pub mod mcp;
pub mod reader;
pub mod sqlite;
pub mod syscall;

// Re-export the FFI surface at the crate root for `cargo test` and
// `main.rs` so callers can write `nelisp_runtime::nelisp_syscall_write`
// without having to spell `::syscall::` each time.
pub use syscall::{
    nelisp_syscall_clear_icache, nelisp_syscall_close, nelisp_syscall_exit,
    nelisp_syscall_fstat, nelisp_syscall_getenv, nelisp_syscall_jit_write_protect,
    nelisp_syscall_mmap, nelisp_syscall_mmap_jit, nelisp_syscall_mprotect,
    nelisp_syscall_munmap, nelisp_syscall_open, nelisp_syscall_read,
    nelisp_syscall_setenv, nelisp_syscall_stat, nelisp_syscall_write, NelispStat,
    SyscallError,
};
pub use syscall::{
    NELISP_MAP_ANONYMOUS, NELISP_MAP_JIT, NELISP_MAP_PRIVATE, NELISP_O_APPEND, NELISP_O_CREAT,
    NELISP_O_RDONLY, NELISP_O_RDWR, NELISP_O_TRUNC, NELISP_O_WRONLY, NELISP_PROT_EXEC,
    NELISP_PROT_NONE, NELISP_PROT_READ, NELISP_PROT_WRITE,
};

// Phase 9d.A1 — file I/O syscall extension.  The eight new `nl_syscall_*`
// symbols are wired through here so callers can write
// `nelisp_runtime::nl_syscall_mkdir` without spelling `::syscall::fileio::`.
// Gated behind the `fileio-syscalls` feature (default ON) per Doc 27 §3
// 7.0 freeze contract: a "mini build" without file I/O still compiles.
#[cfg(feature = "fileio-syscalls")]
pub use syscall::{
    nl_syscall_access, nl_syscall_closedir, nl_syscall_mkdir, nl_syscall_opendir,
    nl_syscall_readdir, nl_syscall_rename, nl_syscall_stat_ex, nl_syscall_unlink,
    NELISP_FILEIO_PATH_MAX,
};

// Phase 9d.A4 (T82) file-notify FFI surface.  Re-exported at crate
// root so cargo-side tests in `tests/filenotify_test.rs` can call the
// symbols without spelling `::syscall::filenotify::` each time.
pub use syscall::{
    nl_filenotify_add_watch, nl_filenotify_close, nl_filenotify_init, nl_filenotify_read,
    nl_filenotify_rm_watch, NL_IN_ATTRIB, NL_IN_CREATE, NL_IN_DELETE, NL_IN_MODIFY,
    NL_IN_MOVED_FROM, NL_IN_MOVED_TO,
};

// Phase 9d.J (T100 / Doc 39 LOCKED-2026-04-25-v2 §3.J) subprocess FFI
// surface.  Twelve `nl_syscall_*` symbols wired at the crate root so
// callers can write `nelisp_runtime::nl_syscall_fork` without spelling
// `::syscall::process::` each time.  Gated behind the
// `process-syscalls` feature (default ON) per Doc 39 §3.J — a future
// "mini build" without subprocess support still compiles cleanly.
#[cfg(feature = "process-syscalls")]
pub use syscall::{
    nl_syscall_dup2, nl_syscall_execve, nl_syscall_fork, nl_syscall_getrlimit,
    nl_syscall_kill, nl_syscall_pipe2, nl_syscall_posix_spawn, nl_syscall_prlimit,
    nl_syscall_select, nl_syscall_setrlimit, nl_syscall_setsid, nl_syscall_waitpid,
};

// T77 (Wave 1 agent C) — SQLite FFI surface.  Five public symbols for
// the Emacs 30 `sqlite-*' compat layer in `src/nelisp-sqlite.el', plus
// a `nl_sqlite_alive' liveness probe used by the `nelisp-sqlitep'
// predicate.
pub use sqlite::{
    nl_sqlite_alive, nl_sqlite_close, nl_sqlite_execute, nl_sqlite_open, nl_sqlite_query,
};
