//! Doc 122 §122.J probe — struct-by-value grammar helpers.
//!
//! Verifies the Phase 47 `.o' objects compiled from
//! `lisp/nelisp-cc-struct-helpers.el' produce bit-identical layouts
//! to hand-rolled `*const struct winsize` writes when marshalling
//! libc structs by value.  The ship-gate exercise is an `ioctl(0,
//! TIOCGWINSZ, &ws)' round-trip on a real TTY (= the underlying
//! syscall the §122.J spec calls out), but it falls back to a
//! byte-equality check against a `*mut winsize` cast on non-TTY hosts
//! (= `cargo test` redirects stdin, so `isatty(0)' is false).
//!
//! Coverage axes:
//!   1. Width-2 (u16) raw-mem op probes — `ptr-read-u16' /
//!      `ptr-write-u16' round-trip with zero-extension.
//!   2. Width-4 (u32) raw-mem op probes — `ptr-read-u32' /
//!      `ptr-write-u32' round-trip with zero-extension.
//!   3. `struct-make` sugar — produces an 8-byte / align-2 buffer for
//!      a `winsize' tag, same as `(alloc-bytes 8 2)'.
//!   4. `struct-field-set' / `struct-field-get' SIZE = 2 sugar —
//!      reduces to the matching `ptr-{read,write}-u16' at parse time.
//!   5. `struct-field-set' / `struct-field-get' SIZE = 4 sugar — same
//!      for the u32 dispatch arm.
//!   6. Composed `nelisp_winsize_write_full' — drives 4 ×
//!      `struct-field-set' against a Rust-allocated buffer, then
//!      asserts the buffer bytes match a hand-rolled `*const winsize'
//!      view (= layout equivalence).
//!   7. Optional `ioctl(0, TIOCGWINSZ, &ws)' round-trip when stdin is
//!      a TTY (skipped under `cargo test` redirection).
//!
//! Substrate gating role (= Doc 117 §117.D.gaps.3 class 4/5 unblock):
//! the sigaction / termios / pollfd handlers can now build their libc
//! struct arguments entirely in Phase 47 elisp via `struct-make' +
//! `struct-field-set', deleting the Rust `MaybeUninit + as_mut_ptr'
//! shims that currently populate the struct fields one assignment at
//! a time.

#![cfg(all(target_os = "linux", target_arch = "x86_64"))]

// ---- Case 1: width-2 (u16) raw-mem ops ----

#[test]
fn ptr_read_write_u16_round_trip() {
    let mut buf: [u8; 16] = [0; 16];
    let base = buf.as_mut_ptr();
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_write_u16(base, 4, 0xABCD)
    };
    assert_eq!(rc, 1, "ptr-write-u16 must return rax = 1 sentinel");
    let v = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_read_u16(base as *const u8, 4)
    };
    assert_eq!(v, 0xABCD, "u16 read must zero-extend (43981, not -21555)");
    // Little-endian layout: low byte at lower offset.
    assert_eq!(buf[4], 0xCD);
    assert_eq!(buf[5], 0xAB);
}

#[test]
fn ptr_read_write_u16_tolerates_unaligned_offsets() {
    // Writes at odd offsets 1, 3, 5 — not aligned to u16's natural
    // 2-byte boundary.  `read_unaligned' / `write_unaligned' on the
    // Rust side must tolerate.
    let mut buf: [u8; 16] = [0; 16];
    let base = buf.as_mut_ptr();
    unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_write_u16(base, 1, 0x1234);
        nelisp_build_tool::elisp_cc_spike::ptr_write_u16(base, 3, 0x5678);
    }
    assert_eq!(
        unsafe { nelisp_build_tool::elisp_cc_spike::ptr_read_u16(base, 1) },
        0x1234
    );
    assert_eq!(
        unsafe { nelisp_build_tool::elisp_cc_spike::ptr_read_u16(base, 3) },
        0x5678
    );
}

// ---- Case 2: width-4 (u32) raw-mem ops ----

#[test]
fn ptr_read_write_u32_round_trip() {
    let mut buf: [u8; 16] = [0; 16];
    let base = buf.as_mut_ptr();
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_write_u32(
            base,
            4,
            0xDEAD_BEEF_u32 as i64,
        )
    };
    assert_eq!(rc, 1, "ptr-write-u32 must return rax = 1 sentinel");
    let v = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_read_u32(base as *const u8, 4)
    };
    assert_eq!(
        v, 0xDEAD_BEEF_i64,
        "u32 read must zero-extend (3735928559, not -559038737)"
    );
    // Little-endian: 0xEF, 0xBE, 0xAD, 0xDE.
    assert_eq!(buf[4], 0xEF);
    assert_eq!(buf[5], 0xBE);
    assert_eq!(buf[6], 0xAD);
    assert_eq!(buf[7], 0xDE);
}

// ---- Case 3: struct-make sugar ----

#[test]
fn struct_make_winsize_returns_8_byte_buffer() {
    // `struct-make' desugars to `(alloc-bytes 8 2)' at parse time.
    // The returned pointer must be non-null and freshly allocated
    // (= the elisp body just `alloc-bytes`es and returns).
    let ptr = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_make_winsize()
    };
    assert!(!ptr.is_null(), "struct-make 'winsize 8 2 must succeed");
    // Verify the buffer is writable for all 8 bytes by stamping +
    // reading back via the matching width-2 ops.
    for offset in (0..8).step_by(2) {
        let v: i64 = (offset as i64) + 1;
        unsafe {
            nelisp_build_tool::elisp_cc_spike::ptr_write_u16(ptr, offset, v);
        }
    }
    for offset in (0..8).step_by(2) {
        let v: i64 = (offset as i64) + 1;
        let got = unsafe {
            nelisp_build_tool::elisp_cc_spike::ptr_read_u16(
                ptr as *const u8,
                offset,
            )
        };
        assert_eq!(got, v, "byte at offset {} round-tripped", offset);
    }
    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(ptr, 8, 2);
    }
}

// ---- Case 4: struct-field-{set,get} SIZE = 2 sugar ----

#[test]
fn struct_field_set_get_u16_round_trip() {
    // `struct-field-set BUF OFF 2 VAL' desugars to `(ptr-write-u16
    // BUF OFF VAL)'.  Mirror via the `struct_field_*_u16' probe
    // wrappers to verify the dispatch.
    let mut buf: [u8; 16] = [0; 16];
    let base = buf.as_mut_ptr();
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_set_u16(base, 4, 0x1234)
    };
    assert_eq!(rc, 1);
    let v = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_get_u16(
            base as *const u8,
            4,
        )
    };
    assert_eq!(v, 0x1234);
    // Cross-verify against the underlying `ptr-read-u16'.
    let direct = unsafe {
        nelisp_build_tool::elisp_cc_spike::ptr_read_u16(base as *const u8, 4)
    };
    assert_eq!(v, direct, "struct-field-get must agree with ptr-read-u16");
}

// ---- Case 5: struct-field-{set,get} SIZE = 4 sugar ----

#[test]
fn struct_field_set_get_u32_round_trip() {
    let mut buf: [u8; 16] = [0; 16];
    let base = buf.as_mut_ptr();
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_set_u32(
            base,
            4,
            0xCAFEBABE_u32 as i64,
        )
    };
    assert_eq!(rc, 1);
    let v = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_get_u32(
            base as *const u8,
            4,
        )
    };
    assert_eq!(v, 0xCAFEBABE_i64);
}

// ---- Case 6: composed winsize_write_full bytes match struct layout ----

#[repr(C)]
#[derive(Default, Debug, Eq, PartialEq, Clone, Copy)]
struct Winsize {
    ws_row: u16,
    ws_col: u16,
    ws_xpixel: u16,
    ws_ypixel: u16,
}

#[test]
fn winsize_write_full_matches_hand_rolled_struct_layout() {
    // Drive `nelisp_winsize_write_full' on a Rust-allocated buffer +
    // assert the 8 bytes the helper wrote exactly match the bytes a
    // hand-rolled `*const Winsize` cast would produce.  This is the
    // §122.J ship-gate property (modulo the live `ioctl' below):
    // the helper-emitted bytes are layout-equivalent to a C struct
    // populated field-by-field by Rust.
    let mut buf: [u8; 8] = [0; 8];
    let base = buf.as_mut_ptr();
    let row: i64 = 24;
    let col: i64 = 80;
    let xpixel: i64 = 1920;
    let ypixel: i64 = 1080;
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::winsize_write_full(
            base, row, col, xpixel, ypixel,
        )
    };
    assert_eq!(rc, base, "winsize_write_full must return its buf arg");

    // Reinterpret + verify.
    let helper_view = unsafe { *(base as *const Winsize) };
    let expected = Winsize {
        ws_row: row as u16,
        ws_col: col as u16,
        ws_xpixel: xpixel as u16,
        ws_ypixel: ypixel as u16,
    };
    assert_eq!(
        helper_view, expected,
        "winsize_write_full bytes must match field-by-field struct layout"
    );
}

// ---- Case 7: ioctl(0, TIOCGWINSZ, &ws) round-trip (live TTY only) ----

#[test]
fn winsize_buffer_works_with_libc_ioctl() {
    // When stdin is not a TTY (= `cargo test' redirects stdin in
    // most CI environments + when run from another tool), the
    // `ioctl(0, TIOCGWINSZ, _)' call returns -1 ENOTTY.  That's
    // expected and not a §122.J spec violation; the byte-layout
    // equivalence in Case 6 already proves the helper produces the
    // right wire format.  The live ioctl test only adds value when
    // a real TTY is attached, so it skips gracefully otherwise.
    extern "C" {
        fn isatty(fd: i32) -> i32;
        fn ioctl(fd: i32, request: u64, arg: *mut u8) -> i32;
    }
    const TIOCGWINSZ: u64 = 0x5413;

    if unsafe { isatty(0) } == 0 {
        eprintln!(
            "[doc-122-j] skipping live ioctl round-trip: stdin is not a TTY"
        );
        return;
    }

    // Allocate via `struct_make_winsize' (= `alloc-bytes 8 2') so we
    // exercise the whole helper chain end-to-end against a real
    // libc API.
    let buf = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_make_winsize()
    };
    assert!(!buf.is_null());

    // Zero-fill via `winsize_write_full' (= 4 × `struct-field-set
    // _ _ 2 0') so we can detect whether `ioctl' actually wrote
    // anything back.
    let rc = unsafe {
        nelisp_build_tool::elisp_cc_spike::winsize_write_full(
            buf, 0, 0, 0, 0,
        )
    };
    assert_eq!(rc, buf);

    let ioctl_rc = unsafe { ioctl(0, TIOCGWINSZ, buf) };
    assert_eq!(
        ioctl_rc, 0,
        "ioctl(0, TIOCGWINSZ, &ws) on a TTY must return 0; got rc={}",
        ioctl_rc
    );

    // Read back via `struct_field_get_u16' — both row + col must be
    // non-zero on a healthy TTY (kernel only zeroes them when the
    // terminal driver hasn't been told yet, which doesn't happen for
    // an interactive PTY).
    let row = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_get_u16(
            buf as *const u8,
            0,
        )
    };
    let col = unsafe {
        nelisp_build_tool::elisp_cc_spike::struct_field_get_u16(
            buf as *const u8,
            2,
        )
    };
    assert!(row > 0, "TTY winsize ws_row > 0; got {}", row);
    assert!(col > 0, "TTY winsize ws_col > 0; got {}", col);

    unsafe {
        nelisp_build_tool::elisp_cc_spike::dealloc_bytes(buf, 8, 2);
    }
}
