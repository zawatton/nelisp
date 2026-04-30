//! Shared C ABI types for NeLisp syscall extension crates.

/// Phase 7.0 stat snapshot.
///
/// `#[repr(C)]` so NeLisp and extension crates can share one stable
/// layout regardless of host `struct stat` differences.
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
