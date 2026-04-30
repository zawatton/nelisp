//! Doc 49 Phase 49.4 native seed image loader.
//!
//! This module deliberately stops at the Rust boundary: validate a
//! native seed image, map heap/code segments, apply pointer
//! relocations, clear the instruction cache, and jump to the seed
//! entry.  It does not parse or evaluate Elisp.

use libc::{c_char, c_int};
use std::ffi::CStr;
use std::fs;
use std::ptr;

pub const SEED_MAGIC: [u8; 8] = *b"NLSEED\0\x01";
pub const SEED_ABI_VERSION: u32 = 1;
pub const SYSCALL_ABI_VERSION: u32 = 1;
pub const SEED_HEADER_LEN: usize = 96;

type SeedEntry =
    unsafe extern "C" fn(c_int, *const *const c_char, *const SeedSyscallTable) -> c_int;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct SeedSyscallTable {
    pub abi_version: u32,
    pub _reserved0: u32,
    pub read: unsafe extern "C" fn(c_int, *mut u8, usize) -> isize,
    pub write: unsafe extern "C" fn(c_int, *const u8, usize) -> isize,
    pub open: unsafe extern "C" fn(*const c_char, c_int, libc::mode_t) -> c_int,
    pub close: unsafe extern "C" fn(c_int) -> c_int,
    pub mmap: unsafe extern "C" fn(*mut u8, usize, c_int, c_int, c_int, libc::off_t) -> *mut u8,
    pub munmap: unsafe extern "C" fn(*mut u8, usize) -> c_int,
    pub mprotect: unsafe extern "C" fn(*mut u8, usize, c_int) -> c_int,
    pub clear_icache: unsafe extern "C" fn(*mut u8, *mut u8) -> c_int,
    pub getenv: unsafe extern "C" fn(*const c_char) -> *const c_char,
    pub setenv: unsafe extern "C" fn(*const c_char, *const c_char, c_int) -> c_int,
    pub stat: unsafe extern "C" fn(*const c_char, *mut crate::NelispStat) -> c_int,
    pub fstat: unsafe extern "C" fn(c_int, *mut crate::NelispStat) -> c_int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SeedError {
    Io(String),
    BadMagic,
    UnsupportedAbi(u32),
    UnsupportedSyscallAbi(u32),
    BadHeaderLen(u32),
    SegmentOutOfBounds(&'static str),
    SegmentNotPageAligned(&'static str),
    EntryOutOfBounds,
    BadRelocationTable,
    HashMismatch { expected: u64, actual: u64 },
    MmapFailed(&'static str),
    MprotectFailed,
}

impl std::fmt::Display for SeedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SeedError::Io(e) => write!(f, "seed image I/O failed: {e}"),
            SeedError::BadMagic => f.write_str("bad seed image magic"),
            SeedError::UnsupportedAbi(v) => write!(f, "unsupported seed ABI {v}"),
            SeedError::UnsupportedSyscallAbi(v) => write!(f, "seed requires syscall ABI {v}"),
            SeedError::BadHeaderLen(n) => write!(f, "bad seed header length {n}"),
            SeedError::SegmentOutOfBounds(name) => {
                write!(f, "seed {name} segment is out of bounds")
            }
            SeedError::SegmentNotPageAligned(name) => {
                write!(f, "seed {name} segment is not page aligned")
            }
            SeedError::EntryOutOfBounds => {
                f.write_str("seed entry offset is outside the code segment")
            }
            SeedError::BadRelocationTable => f.write_str("bad seed relocation table"),
            SeedError::HashMismatch { expected, actual } => {
                write!(f, "seed payload hash mismatch: expected {expected:016x}, got {actual:016x}")
            }
            SeedError::MmapFailed(name) => write!(f, "mmap failed for seed {name} segment"),
            SeedError::MprotectFailed => f.write_str("mprotect failed for seed code segment"),
        }
    }
}

impl std::error::Error for SeedError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SeedHeader {
    pub abi_version: u32,
    pub header_len: u32,
    pub heap_offset: u64,
    pub heap_size: u64,
    pub code_offset: u64,
    pub code_size: u64,
    pub reloc_offset: u64,
    pub reloc_count: u64,
    pub entry_offset: u64,
    pub required_syscall_abi: u32,
    pub payload_hash: u64,
}

pub fn parse_header(bytes: &[u8]) -> Result<SeedHeader, SeedError> {
    if bytes.len() < SEED_HEADER_LEN {
        return Err(SeedError::SegmentOutOfBounds("header"));
    }
    if bytes[0..8] != SEED_MAGIC {
        return Err(SeedError::BadMagic);
    }
    let h = SeedHeader {
        abi_version: le_u32(bytes, 8),
        header_len: le_u32(bytes, 12),
        heap_offset: le_u64(bytes, 16),
        heap_size: le_u64(bytes, 24),
        code_offset: le_u64(bytes, 32),
        code_size: le_u64(bytes, 40),
        reloc_offset: le_u64(bytes, 48),
        reloc_count: le_u64(bytes, 56),
        entry_offset: le_u64(bytes, 64),
        required_syscall_abi: le_u32(bytes, 72),
        payload_hash: le_u64(bytes, 80),
    };
    if h.abi_version != SEED_ABI_VERSION {
        return Err(SeedError::UnsupportedAbi(h.abi_version));
    }
    if h.header_len as usize != SEED_HEADER_LEN {
        return Err(SeedError::BadHeaderLen(h.header_len));
    }
    if h.required_syscall_abi > SYSCALL_ABI_VERSION {
        return Err(SeedError::UnsupportedSyscallAbi(h.required_syscall_abi));
    }
    Ok(h)
}

pub fn payload_hash(bytes: &[u8], header: &SeedHeader) -> Result<u64, SeedError> {
    let mut ranges = Vec::new();
    push_segment(&mut ranges, "heap", bytes.len(), header.heap_offset, header.heap_size)?;
    push_segment(&mut ranges, "code", bytes.len(), header.code_offset, header.code_size)?;
    let reloc_len = header
        .reloc_count
        .checked_mul(8)
        .ok_or(SeedError::BadRelocationTable)?;
    push_segment(&mut ranges, "reloc", bytes.len(), header.reloc_offset, reloc_len)?;

    let mut hash = 0xcbf29ce484222325u64;
    for (start, end) in ranges {
        for b in &bytes[start..end] {
            hash ^= *b as u64;
            hash = hash.wrapping_mul(0x100000001b3);
        }
    }
    Ok(hash)
}

pub unsafe fn load_and_run(path: &str, argv: &[*const c_char]) -> Result<c_int, SeedError> {
    let bytes = fs::read(path).map_err(|e| SeedError::Io(e.to_string()))?;
    let header = parse_header(&bytes)?;
    validate_image(&bytes, &header)?;

    let heap = map_segment("heap", &bytes, header.heap_offset, header.heap_size, false)?;
    let code = map_segment("code", &bytes, header.code_offset, header.code_size, true)?;
    apply_relocations(heap, code, &bytes, &header)?;

    let code_end = code.add(header.code_size as usize);
    crate::nelisp_syscall_clear_icache(code, code_end);

    let entry_addr = code.add(header.entry_offset as usize);
    let entry: SeedEntry = std::mem::transmute(entry_addr);
    let table = syscall_table();
    let rc = entry(argv.len() as c_int, argv.as_ptr(), &table);
    if !heap.is_null() {
        let _ = crate::nelisp_syscall_munmap(heap, header.heap_size as usize);
    }
    let _ = crate::nelisp_syscall_munmap(code, header.code_size as usize);
    Ok(rc)
}

#[no_mangle]
pub unsafe extern "C" fn nelisp_seed_load_and_run(
    path: *const c_char,
    argc: c_int,
    argv: *const *const c_char,
) -> c_int {
    if path.is_null() || argc < 0 {
        return -libc::EINVAL;
    }
    let path = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return -libc::EINVAL,
    };
    let args = if argc == 0 {
        &[]
    } else if argv.is_null() {
        return -libc::EINVAL;
    } else {
        std::slice::from_raw_parts(argv, argc as usize)
    };
    match load_and_run(path, args) {
        Ok(rc) => rc,
        Err(_) => -libc::EINVAL,
    }
}

fn validate_image(bytes: &[u8], header: &SeedHeader) -> Result<(), SeedError> {
    require_page_aligned("heap", header.heap_offset, header.heap_size)?;
    require_page_aligned("code", header.code_offset, header.code_size)?;
    require_page_aligned("reloc", header.reloc_offset, header.reloc_count.saturating_mul(8))?;
    if header.code_size == 0 || header.entry_offset >= header.code_size {
        return Err(SeedError::EntryOutOfBounds);
    }
    let actual = payload_hash(bytes, header)?;
    if actual != header.payload_hash {
        return Err(SeedError::HashMismatch {
            expected: header.payload_hash,
            actual,
        });
    }
    Ok(())
}

unsafe fn map_segment(
    name: &'static str,
    bytes: &[u8],
    offset: u64,
    size: u64,
    executable: bool,
) -> Result<*mut u8, SeedError> {
    if size == 0 {
        return Ok(ptr::null_mut());
    }
    let (start, end) = segment_range(name, bytes.len(), offset, size)?;
    let prot = crate::NELISP_PROT_READ | crate::NELISP_PROT_WRITE;
    let flags = crate::NELISP_MAP_PRIVATE | crate::NELISP_MAP_ANONYMOUS | crate::NELISP_MAP_JIT;
    let ptr = crate::nelisp_syscall_mmap(ptr::null_mut(), size as usize, prot, flags, -1, 0);
    if ptr.is_null() || ptr as isize == -1 {
        return Err(SeedError::MmapFailed(name));
    }
    ptr::copy_nonoverlapping(bytes[start..end].as_ptr(), ptr, size as usize);
    if executable {
        if crate::nelisp_syscall_mprotect(
            ptr,
            size as usize,
            crate::NELISP_PROT_READ | crate::NELISP_PROT_EXEC,
        ) != 0
        {
            let _ = crate::nelisp_syscall_munmap(ptr, size as usize);
            return Err(SeedError::MprotectFailed);
        }
    }
    Ok(ptr)
}

unsafe fn apply_relocations(
    heap: *mut u8,
    code: *mut u8,
    bytes: &[u8],
    header: &SeedHeader,
) -> Result<(), SeedError> {
    if header.reloc_count == 0 {
        return Ok(());
    }
    if heap.is_null() {
        return Err(SeedError::BadRelocationTable);
    }
    let reloc_len = header
        .reloc_count
        .checked_mul(8)
        .ok_or(SeedError::BadRelocationTable)?;
    let (start, _) = segment_range("reloc", bytes.len(), header.reloc_offset, reloc_len)?;
    for i in 0..header.reloc_count as usize {
        let slot_offset = le_u64(bytes, start + i * 8) as usize;
        if slot_offset + 8 > header.heap_size as usize {
            return Err(SeedError::BadRelocationTable);
        }
        let slot = heap.add(slot_offset) as *mut u64;
        let image_offset = *slot as usize;
        if image_offset > header.code_size as usize {
            return Err(SeedError::BadRelocationTable);
        }
        *slot = code.add(image_offset) as u64;
    }
    Ok(())
}

fn require_page_aligned(name: &'static str, offset: u64, size: u64) -> Result<(), SeedError> {
    if size == 0 {
        return Ok(());
    }
    if offset % 4096 != 0 || size % 4096 != 0 {
        return Err(SeedError::SegmentNotPageAligned(name));
    }
    Ok(())
}

fn push_segment(
    ranges: &mut Vec<(usize, usize)>,
    name: &'static str,
    image_len: usize,
    offset: u64,
    size: u64,
) -> Result<(), SeedError> {
    if size == 0 {
        return Ok(());
    }
    ranges.push(segment_range(name, image_len, offset, size)?);
    Ok(())
}

fn segment_range(
    name: &'static str,
    image_len: usize,
    offset: u64,
    size: u64,
) -> Result<(usize, usize), SeedError> {
    let end = offset
        .checked_add(size)
        .ok_or(SeedError::SegmentOutOfBounds(name))?;
    if end > image_len as u64 {
        return Err(SeedError::SegmentOutOfBounds(name));
    }
    Ok((offset as usize, end as usize))
}

fn le_u32(bytes: &[u8], start: usize) -> u32 {
    u32::from_le_bytes(bytes[start..start + 4].try_into().unwrap())
}

fn le_u64(bytes: &[u8], start: usize) -> u64 {
    u64::from_le_bytes(bytes[start..start + 8].try_into().unwrap())
}

fn syscall_table() -> SeedSyscallTable {
    SeedSyscallTable {
        abi_version: SYSCALL_ABI_VERSION,
        _reserved0: 0,
        read: crate::nelisp_syscall_read,
        write: crate::nelisp_syscall_write,
        open: crate::nelisp_syscall_open,
        close: crate::nelisp_syscall_close,
        mmap: crate::nelisp_syscall_mmap,
        munmap: crate::nelisp_syscall_munmap,
        mprotect: crate::nelisp_syscall_mprotect,
        clear_icache: crate::nelisp_syscall_clear_icache,
        getenv: crate::nelisp_syscall_getenv,
        setenv: crate::nelisp_syscall_setenv,
        stat: crate::nelisp_syscall_stat,
        fstat: crate::nelisp_syscall_fstat,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn put_u32(bytes: &mut [u8], start: usize, v: u32) {
        bytes[start..start + 4].copy_from_slice(&v.to_le_bytes());
    }

    fn put_u64(bytes: &mut [u8], start: usize, v: u64) {
        bytes[start..start + 8].copy_from_slice(&v.to_le_bytes());
    }

    fn test_image(code: &[u8]) -> Vec<u8> {
        let code_offset = 4096usize;
        let code_size = 4096usize;
        let mut bytes = vec![0u8; code_offset + code_size];
        bytes[0..8].copy_from_slice(&SEED_MAGIC);
        put_u32(&mut bytes, 8, SEED_ABI_VERSION);
        put_u32(&mut bytes, 12, SEED_HEADER_LEN as u32);
        put_u64(&mut bytes, 32, code_offset as u64);
        put_u64(&mut bytes, 40, code_size as u64);
        put_u32(&mut bytes, 72, SYSCALL_ABI_VERSION);
        bytes[code_offset..code_offset + code.len()].copy_from_slice(code);
        let header = parse_header(&bytes).unwrap();
        let hash = payload_hash(&bytes, &header).unwrap();
        put_u64(&mut bytes, 80, hash);
        bytes
    }

    #[test]
    fn parses_and_validates_seed_header() {
        let image = test_image(&[0xc3]);
        let header = parse_header(&image).unwrap();
        assert_eq!(header.code_offset, 4096);
        validate_image(&image, &header).unwrap();
    }

    #[test]
    fn rejects_bad_magic() {
        assert!(matches!(
            parse_header(b"not-a-seed"),
            Err(SeedError::SegmentOutOfBounds("header"))
        ));
        let mut image = test_image(&[0xc3]);
        image[0] = b'X';
        assert!(matches!(parse_header(&image), Err(SeedError::BadMagic)));
    }

    #[test]
    fn detects_payload_hash_mismatch() {
        let mut image = test_image(&[0xc3]);
        image[4096] ^= 0xff;
        let header = parse_header(&image).unwrap();
        assert!(matches!(
            validate_image(&image, &header),
            Err(SeedError::HashMismatch { .. })
        ));
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn loads_and_runs_x86_64_seed() {
        let image = test_image(&[0xb8, 0x2a, 0x00, 0x00, 0x00, 0xc3]);
        let path = std::env::temp_dir().join(format!("nelisp-seed-{}.bin", std::process::id()));
        std::fs::write(&path, image).unwrap();
        let rc = unsafe { load_and_run(path.to_str().unwrap(), &[]).unwrap() };
        let _ = std::fs::remove_file(path);
        assert_eq!(rc, 42);
    }
}
