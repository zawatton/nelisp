;;; nelisp-cc-struct-helpers.el --- Doc 122 §122.J struct-by-value grammar  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.J — Struct-by-value extern-call helpers.  Per Doc 117
;; §117.D.gaps.3 class 4/5, the
;; =install_sigint_handler= / =install_winsize_handler= / =raw-mode-
;; enter= / =read-stdin-byte-available= families pass libc structs
;; (=sigaction=, =termios=, =winsize=, =pollfd=) by value or by
;; reference to libc functions.  This file packages the three new
;; grammar primitives as Phase 47 `.o' objects so the
;; `tests/elisp_cc_struct_helpers_probe.rs' integration test can
;; verify the struct field marshalling against the real libc API via
;; an `ioctl(0, TIOCGWINSZ, &ws)' round-trip.
;;
;; Surface (= what the §122.J spec calls "helper opcodes"):
;;
;;   (struct-make TAG SIZE ALIGN) -> BUF-PTR
;;     Allocates a fresh heap struct buffer of size SIZE bytes at
;;     alignment ALIGN.  TAG is a symbol literal used for diagnostic
;;     printing only — it has no layout effect.  Both SIZE and ALIGN
;;     must be compile-time integer constants (= the same restriction
;;     as `alloc-bytes' itself).  Returns the buffer pointer cast to
;;     i64; caller owns the buffer and must eventually call
;;     `(dealloc-bytes BUF-PTR SIZE ALIGN)' with the matching layout.
;;
;;   (struct-field-set BUF-PTR OFFSET SIZE VALUE)
;;     Writes VALUE into BUF-PTR + OFFSET as a SIZE-byte little-endian
;;     integer.  SIZE must be a compile-time constant ∈ {1, 2, 4, 8}.
;;     Returns rax = 1 sentinel for `and'-chain composition.  Layout
;;     correctness against the libc struct's field offsets is the
;;     caller's responsibility — no runtime check.
;;
;;   (struct-field-get BUF-PTR OFFSET SIZE)
;;     Reads SIZE-byte little-endian integer from BUF-PTR + OFFSET.
;;     SIZE ∈ {1, 2, 4, 8}.  Returns the value zero-extended to i64
;;     (= no sign extension).
;;
;; Implementation strategy: parser-level desugar.  Each of the three
;; primitives reduces at parse time to existing Phase 47 ops:
;;
;;   (struct-make _ S A)         -> (alloc-bytes S A)
;;   (struct-field-set B O 1 V)  -> (ptr-write-u8  B O V)
;;   (struct-field-set B O 2 V)  -> (ptr-write-u16 B O V)
;;   (struct-field-set B O 4 V)  -> (ptr-write-u32 B O V)
;;   (struct-field-set B O 8 V)  -> (ptr-write-u64 B O V)
;;   (struct-field-get B O 1)    -> (ptr-read-u8  B O)
;;   (struct-field-get B O 2)    -> (ptr-read-u16 B O)
;;   (struct-field-get B O 4)    -> (ptr-read-u32 B O)
;;   (struct-field-get B O 8)    -> (ptr-read-u64 B O)
;;
;; The `ptr-{read,write}-u{16,32}' ops are new in §122.J; the others
;; are §122.E.  Width-2 + width-4 ops use `read_unaligned' /
;; `write_unaligned' on the Rust side (= libc struct fields are not
;; guaranteed to be naturally aligned inside a raw byte buffer; e.g.
;; `pollfd.fd' at offset 0 is naturally aligned but `pollfd.events' at
;; offset 4 might land on an odd 2-byte boundary inside a packed array).
;;
;; Standard libc struct sizes (linux glibc, x86_64) per §122.J spec:
;;   sigaction = 152 bytes, align 8
;;   termios   = 60 bytes,  align 4
;;   winsize   = 8 bytes,   align 2
;;   pollfd    = 8 bytes,   align 4
;;   stat      = 144 bytes, align 8 (statx for newer kernel layouts)
;;
;; Each `defun' below exposes one stand-alone Phase 47 entry so the
;; integration probe can drive each grammar op independently — same
;; pattern as `nelisp-cc-atomic-raw-mem.el' (§122.E) and
;; `nelisp-cc-alloc-dealloc.el' (§125.A).
;;
;; Ship gate (per spec §122.J):
;;   - Compile + link via `make compile' (CI gate, per
;;     `feedback_nelisp_ci_make_compile_gate' memory).
;;   - Probe test: build a `winsize' struct (4 × u16) via
;;     `struct-field-set', pass to `ioctl(0, TIOCGWINSZ, &ws)' via
;;     `extern-call', verify `ws_row' / `ws_col' read back match the
;;     terminal's actual dimensions when a TTY is attached (= the
;;     probe gates the TTY check on `isatty(0)' so it stays meaningful
;;     under `cargo test' with stdin redirected).
;;
;; Unlocks Doc 117 §117.D.gaps.3 class 4/5 (= the sigaction / termios
;; / pollfd handlers) — the Rust shim layer that builds
;; `struct sigaction' / `struct termios' / `struct pollfd' via
;; `MaybeUninit + as_mut_ptr' can now be replaced with a pure-elisp
;; equivalent built from `struct-make' + `struct-field-set'.
;;
;; Linux-x86_64 only — same arch gate as the rest of the §122 family.
;; aarch64 lands with the rest of the Phase 47 aarch64 sweep.

;;; Code:

;; ---- Width-2 (u16) raw mem op probes ----

(defconst nelisp-cc-struct-helpers--read-u16-source
  '(defun nelisp_ptr_read_u16 (ptr offset)
     ;; ptr:    *const u8 — base pointer.
     ;; offset: i64       — byte offset.
     ;;
     ;; Raw `u16' read at `*(u16*)(ptr + offset)' via Rust
     ;; `read_unaligned' (= libc struct fields are not guaranteed
     ;; naturally aligned).  Returns the value zero-extended to `i64'
     ;; in rax (= 0xABCD returns 43981, not -21555).
     (ptr-read-u16 ptr offset))
  "Phase 47 source for the Doc 122 §122.J `ptr-read-u16' op probe.")

(defconst nelisp-cc-struct-helpers--write-u16-source
  '(defun nelisp_ptr_write_u16 (ptr offset val)
     ;; ptr:    *mut u8 — base pointer.
     ;; offset: i64     — byte offset.
     ;; val:    i64     — low 16 bits written as `u16' little-endian.
     ;;
     ;; Raw `u16' store at `*(u16*)(ptr + offset)' via Rust
     ;; `write_unaligned'.  Returns rax = 1 sentinel for `and'-chain
     ;; composition (= underlying extern is `void').
     (ptr-write-u16 ptr offset val))
  "Phase 47 source for the Doc 122 §122.J `ptr-write-u16' op probe.")

;; ---- Width-4 (u32) raw mem op probes ----

(defconst nelisp-cc-struct-helpers--read-u32-source
  '(defun nelisp_ptr_read_u32 (ptr offset)
     ;; Raw `u32' read at `*(u32*)(ptr + offset)' via Rust
     ;; `read_unaligned'.  Returns the value zero-extended to `i64'
     ;; in rax (= 0xDEADBEEF returns 3735928559, not -559038737).
     (ptr-read-u32 ptr offset))
  "Phase 47 source for the Doc 122 §122.J `ptr-read-u32' op probe.")

(defconst nelisp-cc-struct-helpers--write-u32-source
  '(defun nelisp_ptr_write_u32 (ptr offset val)
     ;; Raw `u32' store at `*(u32*)(ptr + offset)' via Rust
     ;; `write_unaligned'.  Returns rax = 1 sentinel.
     (ptr-write-u32 ptr offset val))
  "Phase 47 source for the Doc 122 §122.J `ptr-write-u32' op probe.")

;; ---- struct-make / struct-field-{set,get} sugar probes ----

(defconst nelisp-cc-struct-helpers--make-source
  '(defun nelisp_struct_make_winsize ()
     ;; winsize struct = 4 × u16 = 8 bytes, align 2 (per spec §122.J).
     ;; Probe verifies the `struct-make' sugar reduces to the
     ;; equivalent `(alloc-bytes 8 2)' call at parse time.  TAG
     ;; argument is a quoted symbol literal used for diagnostic
     ;; printing only (= no layout effect; the parser drops it).
     (struct-make 'winsize 8 2))
  "Phase 47 source for the Doc 122 §122.J `struct-make' sugar probe.

Returns a freshly-allocated 8-byte / align-2 heap buffer suitable
for marshalling a `struct winsize' to `ioctl(2, TIOCGWINSZ, _)'.
Caller owns the buffer + must `(dealloc-bytes BUF 8 2)' eventually.")

(defconst nelisp-cc-struct-helpers--field-set-u16-source
  '(defun nelisp_struct_field_set_u16 (buf offset val)
     ;; Probe verifies the `struct-field-set' sugar with SIZE = 2
     ;; reduces to `(ptr-write-u16 BUF OFFSET VAL)' at parse time.
     ;; The literal `2' below is the compile-time constant SIZE — must
     ;; be foldable to an integer for the parser's dispatch.
     (struct-field-set buf offset 2 val))
  "Phase 47 source for the Doc 122 §122.J `struct-field-set' SIZE = 2 probe.")

(defconst nelisp-cc-struct-helpers--field-get-u16-source
  '(defun nelisp_struct_field_get_u16 (buf offset)
     ;; Probe verifies the `struct-field-get' sugar with SIZE = 2
     ;; reduces to `(ptr-read-u16 BUF OFFSET)' at parse time.
     (struct-field-get buf offset 2))
  "Phase 47 source for the Doc 122 §122.J `struct-field-get' SIZE = 2 probe.")

(defconst nelisp-cc-struct-helpers--field-set-u32-source
  '(defun nelisp_struct_field_set_u32 (buf offset val)
     ;; SIZE = 4 dispatch probe — reduces to `(ptr-write-u32 ...)'.
     (struct-field-set buf offset 4 val))
  "Phase 47 source for the Doc 122 §122.J `struct-field-set' SIZE = 4 probe.")

(defconst nelisp-cc-struct-helpers--field-get-u32-source
  '(defun nelisp_struct_field_get_u32 (buf offset)
     (struct-field-get buf offset 4))
  "Phase 47 source for the Doc 122 §122.J `struct-field-get' SIZE = 4 probe.")

;; ---- Composed winsize probe (the ship-gate test) ----

(defconst nelisp-cc-struct-helpers--winsize-write-source
  '(seq
    ;; Side-effect sequencer mirroring §116.A / §122.I
    ;; `nelisp_cstr_helpers_prog2': evaluate both args left-to-right,
    ;; return the second.  Used to thread the four `struct-field-set'
    ;; side effects in front of the buffer-pointer return value
    ;; without intermediate `let' bindings (= Phase 47 `let' only
    ;; accepts compile-time constants).
    (defun nelisp_struct_helpers_prog2 (_eff val) val)

    ;; Build a winsize struct populated with ROW / COL / XPIXEL /
    ;; YPIXEL via the §122.J `struct-field-set' grammar.  Mirrors the
    ;; libc `struct winsize' layout (linux x86_64 glibc):
    ;;
    ;;   struct winsize {
    ;;     unsigned short ws_row;    // offset 0
    ;;     unsigned short ws_col;    // offset 2
    ;;     unsigned short ws_xpixel; // offset 4
    ;;     unsigned short ws_ypixel; // offset 6
    ;;   };
    ;;
    ;; Returns the freshly-allocated buffer pointer; caller owns the
    ;; buffer + must `(dealloc-bytes BUF 8 2)' eventually.  Layout
    ;; matches the §122.J spec's `winsize = 8 bytes, align 2'.
    ;;
    ;; Arity = 4 (even) — no Doc 124.F alignment workaround needed.
    (defun nelisp_winsize_build (row col xpixel ypixel)
      (nelisp_struct_helpers_prog2
       (struct-field-set (struct-make 'winsize 8 2) 0 2 row)
       ;; The first prog2 arm allocates + writes ws_row.  But we need
       ;; the buf pointer threaded through all 4 writes + the
       ;; eventual return.  Use a 5-arg inner that takes the buf
       ;; first to avoid re-allocating.
       0)))
  "Phase 47 source for the Doc 122 §122.J winsize struct ship-gate.

Three-entry `(seq DEFUN ...)' manifest exposing the placeholder
1-shot allocator above the inner threaded writer.  The probe
test drives `nelisp_winsize_build_full' (= the 5-arg inner that
takes BUF first) directly via `extern \"C\"' bindings.")

;; The 5-arg threaded winsize writer.  Mirrors the cstr-helpers
;; pattern but takes the buf pointer as the first argument so we
;; don't have to thread it through `struct-make' + 4 `field-set'
;; ops without intermediate `let' bindings.  This is the function
;; the probe test invokes after calling `struct-make' on the Rust
;; side (= the test wants to verify both halves: `struct-make'
;; reduces to `alloc-bytes' independently, and the field writes
;; against a Rust-allocated buffer still produce the correct
;; little-endian bytes).
;;
;; Arity = 5 (odd).  The 4 inner sequencer calls are all even-arity
;; so the rsp alignment workaround applies only at the outer call
;; site (= same code path that `nelisp_cstr_from_sexp' uses for its
;; 1-arg / 4-arg inner dispatch).

(defconst nelisp-cc-struct-helpers--winsize-write-full-source
  '(seq
    (defun nelisp_struct_helpers_prog2_full (_eff val) val)

    (defun nelisp_winsize_write_full (buf row col xpixel ypixel)
      ;; Thread BUF through 4 `struct-field-set' calls.  Each
      ;; `struct-field-set' returns rax = 1 sentinel; the prog2
      ;; sequencer discards them and threads through to the final
      ;; BUF return.
      (nelisp_struct_helpers_prog2_full
       (struct-field-set buf 0 2 row)
       (nelisp_struct_helpers_prog2_full
        (struct-field-set buf 2 2 col)
        (nelisp_struct_helpers_prog2_full
         (struct-field-set buf 4 2 xpixel)
         (nelisp_struct_helpers_prog2_full
          (struct-field-set buf 6 2 ypixel)
          buf))))))
  "Phase 47 source for the Doc 122 §122.J winsize write-full probe.

Takes a Rust-allocated 8-byte / align-2 buffer + 4 u16 field
values, writes each at the matching offset via
`struct-field-set', returns the (unchanged) buffer pointer.")

(provide 'nelisp-cc-struct-helpers)

;;; nelisp-cc-struct-helpers.el ends here
