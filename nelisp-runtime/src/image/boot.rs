//! Doc 47 Stage 3 — boot from a NlImage v1 file.
//!
//! Walking-skeleton path: read header, mmap the code segment into a
//! JIT-capable page, jump to `entry_offset'.  No heap segment, no
//! relocation, no signal handlers — that all lands in Stage 4 once
//! this proves the loader / dumper / mmap / jit-write-protect /
//! clear-icache / transmute chain works on real hardware.
//!
//! See `image/native_assets.rs' for the canned x86_64 / aarch64
//! "return 42" payload that exercises this path without dragging in
//! the Rust evaluator.
//!
//! TODO(stage-4-heap): mmap heap segment + apply relocations.
//! TODO(stage-4-signal): install sigaction handlers before jumping.
//! TODO(stage-3-seed-syscall): swap `File::open` for `nelisp_syscall_open`
//!     once the seed crate is being carved out (per Doc 47 §4.1).

use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

use super::error::ImageError;
use super::format::NL_IMAGE_PAGE_SIZE;
use super::loader::read_header;
use super::native_assets::NlImageEntry;

/// Boot the image at `path`: read header, materialise the code
/// segment into an executable JIT page, jump to entry, return the
/// `i32' the entry produced.
///
/// SAFETY: this transmutes raw bytes from disk into a function
/// pointer and calls it.  The caller is responsible for trusting
/// `path' — typically the build-tool that mints it on the same
/// machine.  Stage 4 will add signal-handler protection so a
/// malformed image faults cleanly instead of corrupting memory.
pub unsafe fn boot_from_image<P: AsRef<Path>>(path: P) -> Result<i32, ImageError> {
    let path = path.as_ref();
    let header = read_header(path)?;

    if header.code_size == 0 {
        return Err(ImageError::NoCodeSegment {
            path: path.to_path_buf(),
        });
    }

    // Pull the code segment off disk.  Total size is bounded by
    // header.code_size which the loader has already validated; for
    // Stage 3 we read the whole arena into memory before touching
    // mmap so we don't hold the file open across the JIT dance.
    let mut file = File::open(path).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    file.seek(SeekFrom::Start(header.code_offset))
        .map_err(|source| ImageError::Io {
            path: path.to_path_buf(),
            source,
        })?;
    let mut code = vec![0u8; header.code_size as usize];
    file.read_exact(&mut code).map_err(|source| ImageError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    drop(file);

    // mmap a page-aligned RWX region — same JIT dance the existing
    // `exec_bytes' CLI subcommand uses (main.rs L115-195).
    let page = NL_IMAGE_PAGE_SIZE as usize;
    let mapped_size = ((code.len() + page - 1) / page) * page;

    let p = crate::nelisp_syscall_mmap_jit(
        std::ptr::null_mut(),
        mapped_size,
        crate::NELISP_PROT_READ | crate::NELISP_PROT_WRITE,
        crate::NELISP_MAP_PRIVATE | crate::NELISP_MAP_ANONYMOUS | crate::NELISP_MAP_JIT,
        -1,
        0,
    );
    if p.is_null() || p as isize == -1 {
        return Err(ImageError::Io {
            path: path.to_path_buf(),
            source: std::io::Error::new(std::io::ErrorKind::Other, "mmap_jit failed"),
        });
    }

    // macOS Apple Silicon: opt the calling thread into write mode.
    // No-op on Linux / x86_64.
    crate::nelisp_syscall_jit_write_protect(0);
    std::ptr::copy_nonoverlapping(code.as_ptr(), p, code.len());
    crate::nelisp_syscall_jit_write_protect(1);

    // Same RWX rationale as `exec_bytes': cells live in the same JIT
    // page as the code, so PROT_READ|PROT_WRITE|PROT_EXEC.
    let prot_rwx = crate::NELISP_PROT_READ | crate::NELISP_PROT_WRITE | crate::NELISP_PROT_EXEC;
    let mp = crate::nelisp_syscall_mprotect(p, mapped_size, prot_rwx);
    if mp != 0 {
        let _ = crate::nelisp_syscall_munmap(p, mapped_size);
        return Err(ImageError::Io {
            path: path.to_path_buf(),
            source: std::io::Error::new(std::io::ErrorKind::Other, "mprotect(RWX) failed"),
        });
    }

    // arm64 demands an explicit I-cache flush across the bytes we just
    // wrote; x86_64 is a no-op.  Pass exact write extent so the host's
    // `__clear_cache' only invalidates what changed.
    let _ = crate::nelisp_syscall_clear_icache(p, p.add(code.len()));

    // Hand the page off to the CPU.
    let entry: NlImageEntry = std::mem::transmute(p.add(header.entry_offset as usize));
    let result = entry(0, std::ptr::null());

    let _ = crate::nelisp_syscall_munmap(p, mapped_size);
    Ok(result)
}
