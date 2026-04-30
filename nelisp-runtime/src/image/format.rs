//! NlImage v1 binary format — header struct + magic / version constants.
//!
//! This is the Doc 47 Stage 1 stub: the on-disk layout that the Rust
//! seed reads at boot and that the build-tool writes when it mints a
//! NeLisp image from a source tree.  The header is intentionally fixed
//! at 104 bytes so the seed can `read(fd, &mut header, 104)` in a
//! single syscall and decide whether to continue or abort before any
//! mmap is done.
//!
//! Byte order is **little-endian** for every multi-byte integer field —
//! NlImage v1 targets x86_64 + arm64 LE only.  Big-endian platforms are
//! out of scope for the v1 ABI; a future v2 may add a `byte_order`
//! field if needed.
//!
//! The `#[repr(C)]` layout is the source of truth.  The struct must
//! stay POD (no Drop impls, no padding-sensitive fields).  See Doc 47
//! §2.2 for the field-by-field semantics.
//!
//! See also: `mod.rs` for the loader entry points (Stage 2 will add
//! `read_header`, `verify_magic_and_version`, `apply_relocations`).

/// Magic bytes at offset 0 of every NlImage file.
///
/// The trailing NUL keeps the magic at exactly 8 bytes so the header
/// stays naturally aligned and the Unix `file(1)` magic database can
/// match it without ambiguity.
pub const NL_IMAGE_MAGIC: [u8; 8] = *b"NLIMAGE\0";

/// Current NlImage ABI version produced by the build-tool and accepted
/// by this seed.  Bumped whenever the on-disk layout changes in a way
/// that older seeds cannot interpret.
///
/// Forward/backward compatibility policy (Doc 47 §5.2 推奨):
/// - seed accepts `image.abi_version <= NL_IMAGE_ABI_VERSION_MAX`
/// - seed rejects newer images (= forward-incompatible)
/// - older images stay loadable on newer seeds (= backward-compatible)
pub const NL_IMAGE_ABI_VERSION: u32 = 1;

/// Maximum image ABI version this seed knows how to load.  Equal to
/// `NL_IMAGE_ABI_VERSION` for now; will diverge once v2 ships and seed
/// needs to keep loading v1 images.
pub const NL_IMAGE_ABI_VERSION_MAX: u32 = NL_IMAGE_ABI_VERSION;

/// Compression discriminator for `NlImageHeader::compression`.
///
/// Stage 1 only specifies `NONE`; `ZSTD` and `LZ4` are reserved for
/// future use and must reject `verify_*` until a decoder is wired in.
pub const NL_IMAGE_COMPRESSION_NONE: u32 = 0;
pub const NL_IMAGE_COMPRESSION_ZSTD: u32 = 1;
pub const NL_IMAGE_COMPRESSION_LZ4: u32 = 2;

/// Page size assumption for segment offsets.  All `*_offset` fields
/// in the header that point at mmap-eligible regions (heap, code,
/// reloc) MUST be multiples of this value.
pub const NL_IMAGE_PAGE_SIZE: u64 = 4096;

/// Wire-format on-disk header for a NlImage v1 file.
///
/// Field documentation follows Doc 47 §2.2.  All multi-byte integers
/// are little-endian; the seed and build-tool agree on this without a
/// runtime byte-order check (v1 = LE-only).
///
/// SAFETY contract: this struct is POD.  Callers may copy `&[u8; 104]`
/// into a `NlImageHeader` via `core::mem::transmute` only after
/// asserting that `bytes.len() == NL_IMAGE_HEADER_SIZE` AND that the
/// host architecture is little-endian.  Prefer the safe `from_bytes`
/// helper for production paths.
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct NlImageHeader {
    /// File magic = `NL_IMAGE_MAGIC` (b"NLIMAGE\0").
    pub magic: [u8; 8],

    /// On-disk ABI version (= incremented on layout changes).
    pub abi_version: u32,

    /// Compression discriminator (`NL_IMAGE_COMPRESSION_*`).  Stage 1
    /// requires `NONE`; the field is present so future versions can
    /// add zstd/lz4 without bumping `abi_version`.
    pub compression: u32,

    /// Total payload size on disk after this header.
    pub payload_len: u64,

    /// Heap segment — file offset of the heap snapshot bytes.  The
    /// seed mmaps this with `PROT_READ | PROT_WRITE`.
    pub heap_offset: u64,
    pub heap_size: u64,

    /// Native code arena — file offset of AOT-compiled native bytes.
    /// The seed mmaps this with `PROT_READ | PROT_EXEC` (after the
    /// macOS Apple Silicon `pthread_jit_write_protect_np` dance).
    /// `code_size == 0` means "pure bytecode image, no native arena".
    pub code_offset: u64,
    pub code_size: u64,

    /// Relocation table — array of `u64` image offsets.  At each such
    /// offset the heap segment contains an absolute pointer that the
    /// seed must rewrite from `image_base + image_offset` to
    /// `heap_base + image_offset` (or the equivalent for `code_base`).
    pub reloc_offset: u64,
    pub reloc_count: u64,

    /// Entry point — offset within the *code* segment that the seed
    /// jumps to after relocation.  ABI is
    /// `extern "C" fn(argc: i32, argv: *const *const u8) -> i32`.
    pub entry_offset: u64,

    /// Signal vector — offset within the *heap* segment of an array
    /// of NeLisp closures the seed should dispatch when it catches a
    /// signal.  Stage 1 only defines slot 0 = GC barrier handler.
    pub signal_vector_offset: u64,
    pub signal_vector_count: u32,

    /// Minimum syscall ABI version the image expects.  Seeds advertise
    /// their own syscall ABI version; loading aborts when the image
    /// requires a newer set than the seed exposes.
    pub required_syscall_abi: u32,

    /// Reserved bytes — always zero on write, ignored on read.  Gives
    /// us 8 bytes of slack to add a small field in v2 without bumping
    /// the header size.
    pub _reserved: [u8; 8],
}

/// On-disk size of `NlImageHeader` in bytes.  Asserted at compile time
/// via `static_assert_size_eq` below; if you ever adjust the struct,
/// the build will fail until this constant matches.
pub const NL_IMAGE_HEADER_SIZE: usize = 104;

// Compile-time guard: the wire-format size must stay locked.  If a
// later edit changes the struct layout, this fails to compile and
// forces a deliberate ABI version bump.
const _: [(); NL_IMAGE_HEADER_SIZE] = [(); core::mem::size_of::<NlImageHeader>()];

impl NlImageHeader {
    /// Build a fresh, all-zero header initialised with the v1 magic
    /// and ABI version.  Used by the build-tool when it starts
    /// minting an image and by tests as a baseline value.
    pub const fn new_v1() -> Self {
        Self {
            magic: NL_IMAGE_MAGIC,
            abi_version: NL_IMAGE_ABI_VERSION,
            compression: NL_IMAGE_COMPRESSION_NONE,
            payload_len: 0,
            heap_offset: 0,
            heap_size: 0,
            code_offset: 0,
            code_size: 0,
            reloc_offset: 0,
            reloc_count: 0,
            entry_offset: 0,
            signal_vector_offset: 0,
            signal_vector_count: 0,
            required_syscall_abi: 0,
            _reserved: [0; 8],
        }
    }

    /// Parse a 104-byte slice into a header.  Returns `None` on length
    /// mismatch.  Stage 1 deliberately performs no semantic validation
    /// here — call `verify_magic_and_version` for that.  Stage 2 will
    /// extend this with `read_from(&mut impl Read)` for streaming.
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        if bytes.len() < NL_IMAGE_HEADER_SIZE {
            return None;
        }
        // SAFETY: NlImageHeader is `#[repr(C)]` POD, the slice is at
        // least NL_IMAGE_HEADER_SIZE bytes long, and v1 targets only
        // little-endian hosts so we can copy raw.  The const assertion
        // above guarantees `size_of::<Self>() == NL_IMAGE_HEADER_SIZE`.
        let mut hdr = NlImageHeader::new_v1();
        unsafe {
            core::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                &mut hdr as *mut NlImageHeader as *mut u8,
                NL_IMAGE_HEADER_SIZE,
            );
        }
        Some(hdr)
    }

    /// Serialise the header to its 104-byte on-disk form.  Pairs with
    /// `from_bytes` for round-trip; the byte layout is exactly the
    /// `#[repr(C)]` memory layout because the v1 ABI is little-endian
    /// only.  Build-tool dumpers and runtime tests use this to render
    /// a header without going through `unsafe` directly.
    pub fn to_bytes(&self) -> [u8; NL_IMAGE_HEADER_SIZE] {
        // SAFETY: NlImageHeader is `#[repr(C)]` POD; the const guard
        // below the struct definition asserts the size matches.
        unsafe {
            let mut out = [0u8; NL_IMAGE_HEADER_SIZE];
            core::ptr::copy_nonoverlapping(
                self as *const NlImageHeader as *const u8,
                out.as_mut_ptr(),
                NL_IMAGE_HEADER_SIZE,
            );
            out
        }
    }

    /// Reject malformed or version-incompatible images.  Stage 1
    /// checks magic + ABI version + compression.  Stage 2 will extend
    /// with page-alignment and bounds checks against `payload_len`.
    pub fn verify_magic_and_version(&self) -> Result<(), HeaderError> {
        if self.magic != NL_IMAGE_MAGIC {
            return Err(HeaderError::BadMagic { found: self.magic });
        }
        if self.abi_version > NL_IMAGE_ABI_VERSION_MAX {
            return Err(HeaderError::FutureAbiVersion {
                found: self.abi_version,
                max_supported: NL_IMAGE_ABI_VERSION_MAX,
            });
        }
        if self.compression != NL_IMAGE_COMPRESSION_NONE {
            return Err(HeaderError::UnsupportedCompression {
                code: self.compression,
            });
        }
        Ok(())
    }
}

/// Validation outcome for `NlImageHeader::verify_magic_and_version`.
/// Stage 2 will extend this with relocation/segment errors; for now
/// it covers only the three Stage 1 checks so the build-tool and seed
/// can share a single error vocabulary.
#[derive(Debug, PartialEq, Eq)]
pub enum HeaderError {
    /// First 8 bytes are not `NL_IMAGE_MAGIC`.
    BadMagic { found: [u8; 8] },
    /// `abi_version` is newer than `NL_IMAGE_ABI_VERSION_MAX`.
    FutureAbiVersion {
        found: u32,
        max_supported: u32,
    },
    /// `compression` is not `NONE` (Stage 1 has no decoder wired).
    UnsupportedCompression { code: u32 },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn header_size_is_locked() {
        assert_eq!(core::mem::size_of::<NlImageHeader>(), NL_IMAGE_HEADER_SIZE);
        assert_eq!(NL_IMAGE_HEADER_SIZE, 104);
    }

    #[test]
    fn magic_is_eight_bytes() {
        assert_eq!(NL_IMAGE_MAGIC.len(), 8);
        assert_eq!(&NL_IMAGE_MAGIC[..7], b"NLIMAGE");
        assert_eq!(NL_IMAGE_MAGIC[7], 0);
    }

    #[test]
    fn page_size_is_4k() {
        assert_eq!(NL_IMAGE_PAGE_SIZE, 4096);
        assert!(NL_IMAGE_PAGE_SIZE.is_power_of_two());
    }

    #[test]
    fn fresh_header_passes_verify() {
        let hdr = NlImageHeader::new_v1();
        assert!(hdr.verify_magic_and_version().is_ok());
        assert_eq!(hdr.magic, NL_IMAGE_MAGIC);
        assert_eq!(hdr.abi_version, NL_IMAGE_ABI_VERSION);
        assert_eq!(hdr.compression, NL_IMAGE_COMPRESSION_NONE);
    }

    #[test]
    fn bad_magic_rejected() {
        let mut hdr = NlImageHeader::new_v1();
        hdr.magic = *b"DEADBEEF";
        match hdr.verify_magic_and_version() {
            Err(HeaderError::BadMagic { found }) => assert_eq!(found, *b"DEADBEEF"),
            other => panic!("expected BadMagic, got {:?}", other),
        }
    }

    #[test]
    fn future_abi_rejected() {
        let mut hdr = NlImageHeader::new_v1();
        hdr.abi_version = NL_IMAGE_ABI_VERSION_MAX + 1;
        match hdr.verify_magic_and_version() {
            Err(HeaderError::FutureAbiVersion {
                found,
                max_supported,
            }) => {
                assert_eq!(found, NL_IMAGE_ABI_VERSION_MAX + 1);
                assert_eq!(max_supported, NL_IMAGE_ABI_VERSION_MAX);
            }
            other => panic!("expected FutureAbiVersion, got {:?}", other),
        }
    }

    #[test]
    fn unsupported_compression_rejected() {
        let mut hdr = NlImageHeader::new_v1();
        hdr.compression = NL_IMAGE_COMPRESSION_ZSTD;
        match hdr.verify_magic_and_version() {
            Err(HeaderError::UnsupportedCompression { code }) => {
                assert_eq!(code, NL_IMAGE_COMPRESSION_ZSTD);
            }
            other => panic!("expected UnsupportedCompression, got {:?}", other),
        }
    }

    #[test]
    fn from_bytes_round_trip() {
        let original = NlImageHeader::new_v1();
        // SAFETY: POD struct, fixed size, LE host (v1 contract).
        let bytes: [u8; NL_IMAGE_HEADER_SIZE] =
            unsafe { core::mem::transmute(original) };
        let parsed = NlImageHeader::from_bytes(&bytes).expect("parse failed");
        assert_eq!(parsed.magic, original.magic);
        assert_eq!(parsed.abi_version, original.abi_version);
        assert_eq!(parsed.compression, original.compression);
        assert!(parsed.verify_magic_and_version().is_ok());
    }

    #[test]
    fn from_bytes_short_slice_returns_none() {
        let too_short = [0u8; NL_IMAGE_HEADER_SIZE - 1];
        assert!(NlImageHeader::from_bytes(&too_short).is_none());
    }
}
