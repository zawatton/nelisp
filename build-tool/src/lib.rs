//! NeLisp build-time tool — Doc 47 §3.1 phase 6 carve-out.
//!
//! Hosts the Doc 44 minimal interpreter (reader + evaluator + Elisp
//! self-host bridge) so the `nelisp-runtime` ship crate stays
//! image-only.  Per Doc 47 §1.1 the long-term split is:
//!
//! ```text
//! nelisp-runtime    = seed loader + image boot + syscall thin-wrappers
//!                     (target ≤ 4,000 LOC, image-only)
//! nelisp-build-tool = reader + minimal evaluator + dumper
//!                     (Doc 44 minimal interpreter lives here)
//! ```
//!
//! Stage 5a created the workspace + empty crate.  Stage 5b moved
//! `bin/nelisp.rs` here.  Stage 5c (this commit) `git mv`-ed
//! `eval/`, `reader/`, `bridge/` across from `nelisp-runtime/src/`
//! and re-wired the `anvil-runtime` consumer to depend on this
//! crate instead.  Future Stage 6+ will add the dumper that bakes a
//! heap evaluated by these modules into a `nelisp.image` v1 file.
//!
//! Doc 114: this crate is **x86_64-linux only**.  Phase 47 emit
//! (`scripts/compile-elisp-objects.el`) produces ELF64 `.o` files
//! linked directly into the crate; the dispatch sites in `eval/` and
//! `jit/` assume those symbols are present.  Cross-arch hosts must
//! build via Docker / Linux VM.  Multi-arch returns later as
//! additional Phase 47 emit branches (e.g.,
//! `lisp/nelisp-asm-aarch64.el`), never as a re-introduced Rust
//! fallback — see `docs/design/114-x86_64-linux-pivot.org`.

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!(
    "Doc 114: nelisp-build-tool requires x86_64-linux \
     (Phase 47 emit is x86_64-linux only).  Build via Docker / Linux VM."
);

pub mod bridge;
pub mod eval;
// Doc 126.C (2026-05-18) — NELIMG v3 frozen-heap image format retired
// from the production boot path (= Doc 126.B switched `Env::new_global'
// to `reader::read_all + eval').
//
// Doc 126.E (2026-05-18) — `pub mod image' (= 1,567 LOC of encoder /
// decoder / round-trip tests) deleted from the lib surface entirely
// and relocated into `src/bin/nelisp-baker.rs' as bin-private items
// (the bin is the sole consumer for the §95.e cross-impl byte-
// identity verifier; `required-features = ["image-baker"]' in
// `Cargo.toml' keeps it opt-in).  Round-trip tests live inline in the
// bin as `#[cfg(test)] mod tests' and run under `cargo test
// --features image-baker'.  Per repo policy (= pure-elisp化, Rust LOC
// 削減 only metric), this is a -1,567 LOC carve-out from the lib.
// Phase 5 Stage 5.0 / Doc 77b Stage b.4 — Cranelift JIT.  Lowered
// primitives flow through elisp wrappers in
// `lisp/nelisp-jit-strategy.el' that call JIT entries via the
// `nl-jit-call-*' bridge primitives; eval-loop dispatches builtins
// directly to `eval::builtins::dispatch' (no `lower_entries' hook).
//
// Phase 7.1.7.a (2026-05-10): narrowed to `pub(crate)' — the only
// crate-external surface ever needed was the `bi_*' re-exports for
// `eval::builtins::dispatch' which are siblings inside the crate.
// Keeping this `pub(crate)' lets `UnifiedJit' field types stay
// `pub(super)' without `private_interfaces' warnings.
pub(crate) mod jit;
// Reader (Doc 73 §2.4 / Doc 98 §98.3 / Doc 126.A §126.A).  Doc 126.A
// (2026-05-18) re-promoted the reader to production: `Env::new_global'
// dispatches to `reader::read_all' + `eval' per top-level STDLIB form
// when `NELISP_EVAL_BOOT=1' is set, as the first step toward retiring
// the entire NELIMG image subsystem (Doc 126.B-D).  The reader's pre-
// 126.A role of supplying `image-baker' / ERT remains intact.
pub mod reader;

// Doc 99 §99.B spike — C-callable function compiled from elisp by the
// Phase 47 chain.  `build.rs' runs `scripts/compile-elisp-objects.el'
// to produce `target/<...>/elisp-objects/nelisp_spike_noop.o' and
// links it into the crate via `cargo:rustc-link-lib=static=...'.
// This module gives the symbol a Rust home so cargo doesn't dead-code-
// eliminate it from the final binary and `cargo test' can probe the
// round-trip end-to-end.  Doc 114: crate-level guard at top of lib.rs
// makes this module-level cfg redundant; non-x86_64-linux builds fail
// at the crate boundary with a clear compile_error!.
pub mod elisp_cc_spike {
    use crate::eval::sexp::Sexp;

    /// Doc 127 §127.A — collapse the trivial-dispatch safe wrapper
    /// pattern to a 1-liner.  Each `nelisp_*' extern has a matching
    /// `pub unsafe fn FOO(args) -> ret { nelisp_FOO(args) }' wrapper;
    /// for those whose docstring is just the Doc § reference (=
    /// derivable from the function name), the wrapper collapses to
    /// `cc_wrap!(FOO: nelisp_FOO, (args) -> ret);'.  Optional 4th arg
    /// is a literal docstring (= for wrappers that carry a `# Safety'
    /// block or other narrative beyond the Doc § reference).
    macro_rules! cc_wrap {
        ($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty) => {
            #[allow(clippy::missing_safety_doc)]
            pub unsafe fn $name($($arg: $aty),*) -> $ret { $extern($($arg),*) }
        };
        ($name:ident : $extern:ident, ($($arg:ident: $aty:ty),* $(,)?) -> $ret:ty, $doc:literal) => {
            #[doc = $doc]
            pub unsafe fn $name($($arg: $aty),*) -> $ret { $extern($($arg),*) }
        };
    }

    // Sexp is `#[repr(C, u8)]` (see `eval/sexp.rs:57' + the assertions
    // in `eval/sexp_abi_assert.rs') so passing it across an extern "C"
    // boundary by raw pointer is sound — the elisp `.o' only touches
    // bytes at the offsets `nelisp-sexp--offset-*' name (= 0 for tag,
    // 8 for the i64 payload of Sexp::Int).  Rust's `improper_ctypes'
    // lint is conservative because Sexp's variants embed a `String'
    // (which is not `#[repr(C)]'), so the lint trips even though we
    // never pass a Sexp by value.
    #[allow(improper_ctypes)]
    extern "C" {
        fn nelisp_spike_noop() -> i64;
        // Doc 99 §99.C — recursive i64 factorial from
        // `lisp/nelisp-cc-fact-i64.el'.  N must satisfy 0 ≤ N ≤ 20
        // (= the fixnum-safe range for i64); the elisp body itself
        // doesn't range-check, so callers in safe Rust must clamp.
        fn nelisp_fact_i64(n: i64) -> i64;
        // Doc 100 §100.C — `(truncate INT)' Int arm.  Reads the i64
        // payload at `*arg0' (must be `Sexp::Int' — caller's
        // precondition, not checked here) and writes a fresh
        // `Sexp::Int' with the same payload into `*result_slot'.
        // Returns `result_slot' for caller ergonomics.  Defined in
        // `lisp/nelisp-cc-truncate-int.el'.
        fn nelisp_truncate_int(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.B — `(length CONS)' proper-list walk compiled
        // from `lisp/nelisp-cc-length-cons.el'.  `arg0' must point at
        // `Sexp::Cons(_)` or `Sexp::Nil`; result is written into
        // `*result_slot` as `Sexp::Int(n)`.
        fn nelisp_length_cons(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 101 §101.C — `(eq SYMBOL SYMBOL)' through
        // `lisp/nelisp-cc-eq-symbol.el'.  Returns `result_slot'
        // after writing the tag byte for `nil' or `t'.
        fn nelisp_eq_symbol(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 101 §101.D — `(cons A B)' constructor compiled from
        // `lisp/nelisp-cc-cons-construct.el'.  Writes `Sexp::Cons(_)'
        // into `*result_slot' and returns that same pointer.
        fn nelisp_cons_construct(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.2 — `(string-bytes STR)' byte-length helper
        // compiled from `lisp/nelisp-cc-bi-string-bytes.el'.  `arg0'
        // must point at a `Sexp::Str' / `Sexp::Symbol' (= the Rust
        // shim in `eval/builtins.rs::bi_string_bytes' unwraps
        // `Sexp::MutStr' into the underlying `Sexp::Str' view before
        // calling).  Writes `Sexp::Int(byte_count)' into `*result_slot'
        // and returns the same pointer for caller ergonomics.
        fn nelisp_bi_string_bytes(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.B — `(recordp X)' predicate compiled from
        // `lisp/nelisp-cc-recordp.el'.  Writes Sexp::T / Sexp::Nil
        // into `*result_slot' and returns that same pointer.
        fn nelisp_recordp(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        // Doc 111 §111.C — `(aref VECTOR IDX)' Vector arm compiled
        // from `lisp/nelisp-cc-aref-vector.el'.  Rust pre-validates
        // that `arg0' is a Vector and `arg1' is an in-range Int.
        fn nelisp_aref_vector(
            arg0: *const Sexp,
            arg1: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 117 §117.A.1 — `(make-vector N INIT)' allocate + fill
        // compiled from `lisp/nelisp-cc-bi-make-vector.el'.  Rust
        // pre-validates that `n_ptr' points at `Sexp::Int' with
        // N >= 0; the elisp body allocates a fresh `Sexp::Vector(N)'
        // via `vector-make' (§115.1) and fills each slot [0, N) with
        // a refcount-aware clone of `*init_ptr' via `vector-slot-set'
        // (§111.E).  Returns i64 = 1 on success (= `and' chain
        // terminator); the caller reads the fresh vector from
        // `*result_slot'.
        fn nelisp_bi_make_vector(
            n_ptr: *const Sexp,
            init_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> i64;
        // Doc 117 §117.B — quit-flag atomic ops compiled from
        // `lisp/nelisp-cc-bi-quit-flag.el'.  Each kernel takes a
        // `*mut i64' pointing at the `QUIT_FLAG' static in
        // `eval/quit.rs' (= surfaced by `nl_quit_flag_ptr') and uses
        // a §122.E `atomic-compare-exchange' (= set / clear) or
        // `ptr-read-u64' (= pending-p) op.  Returns:
        //   set / clear → i64 CAS result (1 on transition, 0 on
        //                 benign no-op); both states satisfy the
        //                 post-condition so the Rust shim discards it.
        //   pending-p   → i64 slot value (0 = clear, non-zero = pending).
        fn nelisp_bi_set_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_clear_quit_flag(flag_ptr: *mut i64) -> i64;
        fn nelisp_bi_quit_flag_pending_p(flag_ptr: *const i64) -> i64;
        // Doc 117 §117.B / Doc 122 §122.H — first I/O syscall swap.
        // Phase 47 elisp body compiled from
        // `lisp/nelisp-cc-bi-write-stderr-line.el'.  Single-arg
        // function: receives a `*const Sexp' caller-validated as
        // `Sexp::Str' / `Sexp::Symbol' / `Sexp::MutStr', dispatches
        // through the §122.H `str-bytes-ptr' grammar op (= Rust
        // `nl_str_bytes_ptr' extern) + the §101.C `str-len' op, and
        // emits a single `write(2, bytes, len)' libc syscall.
        // Returns the libc `write' i64 (= bytes written, or -1 on
        // error).  The Rust shim discards the return — pre-swap
        // `writeln!' suppressed errors the same way (`let _ = ...').
        fn nelisp_bi_write_stderr_line(str_ptr: *const Sexp) -> i64;
        // Doc 117 §117.B (cont) — I/O syscall sweep batch.  Twin of
        // `nelisp_bi_write_stderr_line' modulo (fd=1, no trailing
        // newline).  Compiled from
        // `lisp/nelisp-cc-bi-write-stdout-bytes.el'.  Returns the
        // libc `write(1, ...)' i64 (= bytes written, or -1 on error).
        fn nelisp_bi_write_stdout_bytes(str_ptr: *const Sexp) -> i64;
        // Doc 117 §117.B (cont) — read-side counterpart of the I/O
        // sweep batch.  Compiled from
        // `lisp/nelisp-cc-bi-read-stdin-bytes.el'.  Issues a single
        // `read(0, buf_ptr, limit)' libc syscall against fd 0 (stdin).
        // The destination buffer is Rust-owned (= `Vec<u8>` allocated
        // in the shim before the call).  Returns the libc `read(2)'
        // i64: > 0 = bytes received, 0 = EOF, -1 = errno set.  The
        // Rust shim wraps the bytes into `Sexp::Str' via
        // `String::from_utf8_lossy' (= no Phase 47 grammar equivalent
        // for the lossy UTF-8 path today; future §122.X op would let
        // the wrap migrate too).
        fn nelisp_bi_read_stdin_bytes(buf_ptr: *mut u8, limit: i64) -> i64;
        // Doc 111 §111.D — Cell read+write ops compiled from
        // `lisp/nelisp-cc-cell-ops.el'.  Each op is a separate `.o' in
        // the static archive so the integration test in
        // `tests/phase47_cell.rs' can drive each one independently.
        //
        //   `nelisp_cell_value(arg0, slot)' — inline 32-byte copy of
        //     `NlCell.value' into `*slot'.  No refcount work (MVP,
        //     same contract as `cons-car' / `cons-cdr').
        //   `nelisp_cell_set_value(arg0, val_ptr)' — refcount-aware
        //     overwrite via `nl_cell_set_value' extern.
        //   `nelisp_cell_make(val_ptr, slot)' — allocate a fresh
        //     NlCell via `nl_alloc_cell' and write `Sexp::Cell(box)'
        //     into `*slot'.
        //   `nelisp_cell_null_p(arg0)' — i64 1 iff `NlCell.value's tag
        //     is `Sexp::Nil', else 0.
        fn nelisp_cell_value(arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_set_value(arg0: *const Sexp, val_ptr: *const Sexp);
        fn nelisp_cell_make(val_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp;
        fn nelisp_cell_null_p(arg0: *const Sexp) -> i64;
        // Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' Phase
        // 47 grammar ops compiled from `lisp/nelisp-cc-sexp-write-str.el'.
        // Each op evaluates 3 args (slot, bytes_ptr, len), marshals them
        // to rdi/rsi/rdx, and calls the Rust `nl_alloc_str' /
        // `nl_alloc_symbol' extern (in `build-tool/src/eval/nlstr.rs')
        // which writes a fresh `Sexp::Str' / `Sexp::Symbol' into `*slot'
        // and returns the slot pointer.  Unlike the cell/vector/record
        // allocators these write the full 40-byte `Sexp' value inline
        // (= `Sexp::Str' / `Sexp::Symbol' carry their `String' header
        // inline at payload offset 8..32, not via an `*mut NlXXX'
        // pointer indirection — see comments in `eval/nlstr.rs').
        fn nelisp_sexp_write_str(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        fn nelisp_sexp_write_symbol(
            slot: *mut Sexp,
            bytes_ptr: *const u8,
            len: i64,
        ) -> *mut Sexp;
        // Doc 122 §122.G — `sexp-write-float' Phase 47 grammar op
        // compiled from `lisp/nelisp-cc-sexp-write-float.el'.  Both
        // params are f64-class (= Phase 47 MVP uniform-class restriction);
        // the slot pointer is bit-cast through xmm0 and the elisp emit
        // code unspills it back to GP rdi before calling the Rust extern
        // `nl_sexp_write_float' (in `build-tool/src/eval/nlstr.rs') which
        // writes `Sexp::Float(val)' into `*slot' inline (= tag 3 + f64
        // payload at offset 8, no heap box).
        fn nelisp_sexp_write_float(
            slot_bits: f64,
            val: f64,
        ) -> *mut Sexp;
        // Doc 122 §122.B — Mutable string builder Phase 47 grammar ops
        // compiled from `lisp/nelisp-cc-mut-str.el'.  Each op evaluates
        // its args, marshals them to rdi/rsi per SysV AMD64, and calls
        // the matching `nl_mut_str_*' / `nl_alloc_mut_str' Rust extern
        // (in `build-tool/src/eval/nlstr.rs').  The push/len/finalize
        // ops dereference the `Sexp::MutStr' payload pointer at offset
        // 8 to reach the inner `NlStr.value: String' (= one extra
        // indirection vs. §122.A's inline String layout, because
        // `Sexp::MutStr' wraps an `NlStrRef' / `NonNull<NlStr>').
        fn nelisp_mut_str_make_empty(slot: *mut Sexp, cap: i64) -> *mut Sexp;
        fn nelisp_mut_str_push_byte(ptr: *mut Sexp, byte: i64) -> i64;
        fn nelisp_mut_str_push_codepoint(ptr: *mut Sexp, cp: i64) -> i64;
        fn nelisp_mut_str_len(ptr: *const Sexp) -> i64;
        fn nelisp_mut_str_finalize(
            ptr: *const Sexp,
            slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 122 §122.D — UTF-8 helper Phase 47 grammar ops compiled
        // from `lisp/nelisp-cc-utf8.el'.
        fn nelisp_str_char_count(ptr: *const Sexp) -> i64;
        fn nelisp_str_codepoint_at(
            ptr: *const Sexp,
            idx: i64,
            cp_slot: *mut i64,
            width_slot: *mut i64,
        ) -> i64;
        fn nelisp_str_is_alphanumeric_at(
            ptr: *const Sexp,
            idx: i64,
        ) -> i64;
        // Doc 122 §122.E — Atomic + raw memory primitive Phase 47
        // grammar ops compiled from `lisp/nelisp-cc-atomic-raw-mem.el'.
        // Substrate gate for Doc 123-128 (= refcount elisp化,
        // nl*.rs Clone/Drop elisp化, alloc / dealloc handlers).
        fn nelisp_atomic_fetch_add(ptr: *mut i64, delta: i64) -> i64;
        fn nelisp_atomic_compare_exchange(
            ptr: *mut i64,
            expected: i64,
            new_val: i64,
        ) -> i64;
        fn nelisp_ptr_read_u64(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u64(ptr: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_ptr_read_u8(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u8(ptr: *mut u8, offset: i64, val: i64) -> i64;
        // Doc 122 §122.J — width-{2, 4} raw-mem op probes (= the SIZE
        // gap between `_u8' and `_u64' for libc struct field marshalling).
        // Each compiles to a 1-line `(ptr-{read,write}-u{16,32} ptr off
        // [val])' call from `lisp/nelisp-cc-struct-helpers.el' down to
        // the matching Rust extern in `build-tool/src/eval/raw_mem.rs'.
        // Unaligned access tolerated (= libc struct fields are not
        // guaranteed naturally aligned inside a raw byte buffer).
        fn nelisp_ptr_read_u16(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u16(ptr: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_ptr_read_u32(ptr: *const u8, offset: i64) -> i64;
        fn nelisp_ptr_write_u32(ptr: *mut u8, offset: i64, val: i64) -> i64;
        // Doc 122 §122.J — `struct-make' / `struct-field-{set,get}'
        // sugar probes.  Each is a 1-form parser-level desugar that
        // reduces to existing primitives at parse time (`struct-make'
        // -> `alloc-bytes', `struct-field-set' -> `ptr-write-uN'
        // chosen by compile-time SIZE constant, ditto for get).
        // Exposed here so the integration probe can verify each
        // dispatch arm independently.
        fn nelisp_struct_make_winsize() -> *mut u8;
        fn nelisp_struct_field_set_u16(buf: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_struct_field_get_u16(buf: *const u8, offset: i64) -> i64;
        fn nelisp_struct_field_set_u32(buf: *mut u8, offset: i64, val: i64) -> i64;
        fn nelisp_struct_field_get_u32(buf: *const u8, offset: i64) -> i64;
        // Doc 122 §122.J — composed winsize write-full probe.  Takes a
        // caller-allocated 8-byte / align-2 buffer + 4 u16 field
        // values, writes each at the matching offset via
        // `struct-field-set', returns the (unchanged) buffer pointer.
        // The ship-gate test for the §122.J spec: drive with values
        // (R, C, X, Y), then `ioctl(0, TIOCGWINSZ, buf)' on a real
        // TTY to verify the buffer round-trips through libc + that
        // the helper writes the same bytes a hand-rolled
        // `*const struct winsize' cast would produce.
        fn nelisp_winsize_write_full(
            buf: *mut u8,
            row: i64,
            col: i64,
            xpixel: i64,
            ypixel: i64,
        ) -> *mut u8;
        // Doc 125 §125.A — alloc / dealloc primitive Phase 47 grammar
        // ops compiled from `lisp/nelisp-cc-alloc-dealloc.el'.
        // Substrate gate for Doc 124.G-K (= NlBox Drop kernels'
        // if-zero-refcount free branch) + Doc 126-128 (= bridge GC
        // arena allocator).  Each is a 2-/3-arg extern-call shape
        // matching the §122.E `(ptr-write-u64 …)' / `(atomic-fetch-add
        // …)' family — args go in rdi/rsi(/rdx) per SysV AMD64 and
        // the call lowers to `nl_alloc_bytes' / `nl_dealloc_bytes' in
        // `build-tool/src/eval/raw_mem.rs'.
        fn nelisp_alloc_bytes(size: i64, align: i64) -> *mut u8;
        fn nelisp_dealloc_bytes(ptr: *mut u8, size: i64, align: i64) -> i64;
        // Doc 122 §122.I — pure-elisp CString construction helper
        // compiled from `lisp/nelisp-cc-cstr-helpers.el'.  Takes a
        // `*const Sexp` pointing at `Sexp::Str` / `Sexp::Symbol` /
        // `Sexp::MutStr` and returns a freshly heap-allocated
        // NUL-terminated byte buffer (= what libc `const char *path'
        // APIs expect).  Allocation layout is `(size = str-len + 1,
        // align = 1)' via the §125.A `alloc-bytes' op; caller must
        // free with `nelisp_cstr_drop' (= `dealloc-bytes' with the
        // same `(size, align)' pair) once the libc consumer is done.
        // Substrate gate for Doc 117 §117.D.gaps.3 Tier C (= the
        // bi_open / bi_stat / bi_mkdir file-I/O sweep).
        fn nelisp_cstr_from_sexp(str_ptr: *const Sexp) -> *mut u8;
        fn nelisp_cstr_drop(buf_ptr: *mut u8, size: i64) -> i64;
        // Doc 117 §117.D.gaps.3 — file-I/O syscall body sweeps powered
        // by the §122.I CString helper.  Each kernel takes a `*const
        // Sexp' pointing at a Rust-prepared (= normalised against
        // `default-directory') `Sexp::Str' / `Sexp::MutStr' path
        // argument, builds the libc CString via `nelisp_cstr_from_
        // sexp', issues the relevant libc syscall(s) against a
        // Rust-owned result buffer, frees the CString, and returns
        // the libc rc.  The Rust shim retains arg validation +
        // buffer allocation + Sexp wrap.
        //
        //   `nelisp_bi_syscall_stat(path_ptr, statbuf)' — libc
        //     `stat(2)' against a Rust-owned 144-byte (= sizeof
        //     `struct stat' on Linux x86_64 glibc) buffer.  Returns
        //     i64 = 0 on success / -1 on error.  Rust inspects the
        //     buffer's mode field and maps to `'absent / `'file /
        //     `'directory' symbols.
        //   `nelisp_bi_syscall_canonicalize(path_ptr, result_buf)'
        //     — libc `realpath(3)' against a Rust-owned PATH_MAX
        //     (= 4096-byte) result buffer.  Returns i64 = result_buf
        //     address on success or 0/NULL on error.  Rust does
        //     `CStr::from_ptr' + `Sexp::Str' wrap on success and
        //     `Sexp::Nil' on NULL.
        //   `nelisp_bi_nl_write_file(path_ptr, content_ptr)' —
        //     chained libc `open(2)' + `write(2)' + `close(2)' against
        //     a path string + content string.  Returns i64 = bytes
        //     written (>= 0) on success or the failing syscall's
        //     negative rc on error.  Rust maps < 0 → `EvalError::
        //     Internal' (matching the pre-swap `std::fs::write'
        //     error propagation).
        fn nelisp_bi_syscall_stat(path_ptr: *const Sexp, statbuf: *mut u8) -> i64;
        fn nelisp_bi_syscall_canonicalize(
            path_ptr: *const Sexp,
            result_buf: *mut u8,
        ) -> i64;
        fn nelisp_bi_nl_write_file(
            path_ptr: *const Sexp,
            content_ptr: *const Sexp,
        ) -> i64;
        // Doc 117 §117.D.gaps.3 (cont.) — second file-I/O sweep batch.
        //
        //   `nelisp_bi_nl_make_directory(path_ptr)' — libc `mkdir(2)'
        //     against the path CString with mode 0o755.  Returns i64 =
        //     0 on success / -1 on error (errno set).  Rust maps < 0 →
        //     `EvalError::Internal' (matching the pre-swap
        //     `std::fs::create_dir_all' error propagation; this kernel
        //     is the non-recursive flavour — see the elisp source's
        //     commentary for the semantic narrowing rationale).
        //   `nelisp_bi_syscall_read_file(path_ptr, buf_ptr, read_size)'
        //     — chained libc `open(2)' + `read(2)' + `close(2)'.  Rust
        //     allocates the buffer + sizes it via a separate `stat(2)'
        //     pass, the elisp body does the syscall chain.  Returns
        //     i64 = bytes read on success or negative open/read rc on
        //     error.  Rust maps < 0 → `Sexp::Nil' (matching the pre-
        //     swap `Err(_) => Ok(Sexp::Nil)' arm of `bi_syscall_read_
        //     file').
        fn nelisp_bi_nl_make_directory(path_ptr: *const Sexp) -> i64;
        fn nelisp_bi_syscall_read_file(
            path_ptr: *const Sexp,
            buf_ptr: *mut u8,
            read_size: i64,
        ) -> i64;
        // Doc 122 §122.C — Extended extern-call (f64 args + f64 return) probes.
        fn nelisp_libm_sqrt(x: f64) -> f64;
        fn nelisp_libm_sin(x: f64) -> f64;
        fn nelisp_libm_cos(x: f64) -> f64;
        // Doc 123 §123.A — first substrate elisp化 of rc_primitives.rs.
        // Pure-elisp refcount-inc kernel via §122.E atomic-fetch-add.
        fn nelisp_rc_inc(box_ptr: *mut i64) -> i64;
        // Doc 123 §123.B — second substrate elisp化 of rc_primitives.rs.
        // Pure-elisp refcount-dec kernel via §122.E atomic-fetch-add
        // with delta=-1 (= fetch-sub semantics).  Returns pre-sub i64.
        fn nelisp_rc_dec(box_ptr: *mut i64) -> i64;
        // Doc 123 §123.C — refcount-reader twins.
        fn nelisp_rc_strong_count(box_ptr: *const u8) -> i64;
        fn nelisp_rc_kind(sexp_ptr: *const u8) -> i64;
        // Doc 123 §123.D — payload-ptr reader + walk-children kernel,
        // completing the substrate elisp化 chain of Doc 123.
        // `nelisp_rc_payload_ptr' reads the inner NlBox* payload
        // pointer at offset 8 of the outer `Sexp' (= `SEXP_PAYLOAD_OFFSET');
        // tag-dispatch (the non-Cons-returns-0 branch of
        // `bi_nl_rc_payload_ptr') is the caller's responsibility.
        // `nelisp_gc_walk_children' composes 2 `cons-make' allocations
        // to materialize the 2-list `(car cdr)' for Cons inputs,
        // mirroring the Rust body's `Sexp::list_from(&[car, cdr])'.
        // The `result_slot' parameter receives the head Sexp::Cons;
        // `tail_slot' is caller-owned scratch for the inner
        // `(cdr . nil)' allocation.  Cons-only support; non-Cons
        // tag-dispatch lands in §123.F's sweep stage.
        fn nelisp_rc_payload_ptr(sexp_ptr: *const u8) -> i64;
        fn nelisp_gc_walk_children(
            sexp_ptr: *const Sexp,
            result_slot: *mut Sexp,
            tail_slot: *mut Sexp,
        ) -> *mut Sexp;
        // Doc 124 §124.A — first stage of the `nl*.rs::Clone/Drop'
        // substrate elisp化.  NlConsBox Clone kernel: bumps the
        // refcount at offset 64 via §122.E `atomic-fetch-add', then
        // returns the input pointer (= the cloned-handle's pointer).
        // The Rust `impl Clone for NlConsBoxRef' wraps the return
        // into `Self { ptr, _marker }' in §124.F's sweep stage.
        fn nelisp_nlconsbox_clone(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.G — first Drop-half stage.  NlConsBox Drop
        // kernel: fetch-sub then conditional dealloc.
        fn nelisp_nlconsbox_drop(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.H — NlVector Drop kernel (REFCOUNT_OFFSET=24,
        // SIZE=32, ALIGN=8).  Mechanical port of §124.G modulo the
        // per-type layout literals.
        fn nelisp_nlvector_drop(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.I/J/K — sibling Drop kernels.  Same shape as
        // §124.G/H modulo per-type SIZE / REFCOUNT_OFFSET literals:
        //   §124.I NlCell:   REFCOUNT_OFFSET = 32, SIZE = 40, ALIGN = 8
        //   §124.J NlRecord: REFCOUNT_OFFSET = 56, SIZE = 64, ALIGN = 8
        //   §124.K NlStr:    REFCOUNT_OFFSET = 24, SIZE = 32, ALIGN = 8
        fn nelisp_nlcell_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_drop(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.B-E — mechanical sibling Clone kernels.
        // REFCOUNT_OFFSET = 24/32/56/24 respectively.
        fn nelisp_nlvector_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlcell_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlrecord_clone(box_ptr: *mut i64) -> i64;
        fn nelisp_nlstr_clone(box_ptr: *mut i64) -> i64;
        // Doc 124 §124.L+ — NlBoolVector + NlCharTable Drop kernels
        // (the remaining 2 NlBox types).  Identical shape as §124.G-K
        // modulo per-type SIZE / REFCOUNT_OFFSET literals:
        //   NlBoolVector: REFCOUNT_OFFSET = 24,  SIZE = 32,  ALIGN = 8
        //                 (= Vec<bool> header @ 0; Vec<T> header size
        //                  independent of T).
        //   NlCharTable:  REFCOUNT_OFFSET = 120, SIZE = 128, ALIGN = 8
        //                 (= CharTableInner @ 0; subtype Sexp + default
        //                  Sexp + entries Vec + parent Option<NlChar
        //                  TableRef> + extra Vec = 32+32+24+8+24 = 120).
        fn nelisp_nlboolvector_drop(box_ptr: *mut i64) -> i64;
        fn nelisp_nlchartable_drop(box_ptr: *mut i64) -> i64;
        // Doc 111 §111.E #1 — `mirror_lookup_entry' Phase 47 helper
        // compiled from `lisp/nelisp-cc-mirror-lookup-entry.el'.
        // Returns the `*const Sexp' of the matching symbol-entry
        // Record (= the Sexp slot inside the bucket's (KEY . ENTRY)
        // pair NlConsBox), or 0 on miss / empty mirror.
        //
        // Pre-conditions (= caller / dispatcher responsibility,
        // mirror the Rust impl's early-`return None' arms):
        //   mirror_ptr.tag = Sexp::Record (= globals_record).
        //   mirror_ptr.slots[0].tag = Sexp::Record (= fast-hash-table).
        //   ht.slots[0] = Sexp::Int (= bucket count, power of 2).
        //   ht.slots[1] = Sexp::Vector (= buckets).
        fn nelisp_mirror_lookup_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E #2-6 — Group A compose-on-#1 helpers.  Each
        // is a thin Phase 47 object that calls
        // `nelisp_mirror_lookup_entry' via the `extern-call' grammar
        // form and adds a 1-2 op tail to read the requested
        // symbol-entry slot.  See `lisp/nelisp-cc-mirror-*.el' for
        // the per-helper source body.
        //
        //   `nelisp_mirror_lookup_value(M, S, SLOT)' — copy entry
        //      slot 0 (value cell) into SLOT via record-slot-ref, or
        //      write Sexp::Nil on miss.  Returns SLOT.
        //   `nelisp_mirror_lookup_function(M, S, SLOT)' — slot 1
        //      counterpart of value.
        //   `nelisp_mirror_is_bound(M, S, UNBOUND)' — i64 1 iff entry
        //      exists AND slot 0 != UNBOUND (= the caller-supplied
        //      `Sexp::Symbol("nelisp--unbound-marker")' sentinel).
        //   `nelisp_mirror_is_fbound(M, S, UNBOUND)' — slot 1
        //      counterpart of is_bound.
        //   `nelisp_mirror_is_constant(M, S)' — i64 1 iff entry
        //      exists AND slot 3 has tag `SEXP_TAG_T' (= 1).
        fn nelisp_mirror_lookup_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_lookup_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> *mut Sexp;
        fn nelisp_mirror_is_bound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_fbound(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_is_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
        ) -> i64;
        // Doc 111 §111.E Group B (#7-#12) — env_mirror.rs write path
        // Phase 47 helpers.  Each one composes #1 `mirror_lookup_entry'
        // (via the `extern-call' grammar form) with a §111.B
        // `record-slot-set' on the matched entry's slot N.  Returns 1
        // on hit (entry slot was overwritten in place) or 0 on miss
        // (caller dispatches to the Rust auto-vivify path).
        fn nelisp_mirror_set_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            val_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_value(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_clear_function(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            unbound_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_constant(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            flag_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_install_entry(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            value_ptr: *const Sexp,
            function_ptr: *const Sexp,
            plist_ptr: *const Sexp,
            constant_ptr: *const Sexp,
        ) -> i64;
        // Doc 119 §119.A — auto-vivify fold helpers + wrappers.
        // -------------------------------------------------------------
        // Building blocks:
        //   `nelisp_mirror_alloc_entry' — fresh `symbol-entry' Record
        //      alloc + 4-slot install.  TAG-SYM-PTR points at
        //      `Sexp::Symbol("symbol-entry")' that the safe wrapper
        //      materialises on the call stack.
        //   `nelisp_mirror_bucket_prepend' — hash NAME + cons-make
        //      `(KEY-STR . ENTRY-RECORD)' + `vector-slot-set' install +
        //      count slot bump.  ENTRY-PTR is the freshly-allocated
        //      symbol-entry from `alloc_entry'.  SCRATCH-VEC-PTR is a
        //      `Sexp::Vector' with 5 caller-owned Nil scratch slots
        //      (= Nil-source / inner-pair / outer-cell / count-int /
        //      KEY-Str; see `lisp/nelisp-cc-mirror-bucket-prepend.el'
        //      commentary).
        //
        // Wrappers (= absorb the miss-path of helpers #7/#8/#11/#12):
        //   `nelisp_mirror_set_value_or_insert'    — slot 0 (value).
        //   `nelisp_mirror_set_function_or_insert' — slot 1 (function).
        //   `nelisp_mirror_set_constant_or_insert' — slot 3 (constant flag).
        //   `nelisp_mirror_install_entry_or_insert' — all 4 slots.
        //
        // The 4 wrappers share a uniform 4-arg signature `(mirror,
        // sym, scratch_vec, _pad)' where SCRATCH-VEC-PTR is an
        // 11-slot `Sexp::Vector' (= 5 prepend scratches at 0..4 +
        // tag symbol at 5 + entry result at 6 + 4 caller-supplied
        // value Sexps at 7..10).  The Rust safe wrapper materialises
        // the value Sexps + tag before the call and drops the
        // scratch vector after (= refcount balance on auto-vivify).
        fn nelisp_mirror_alloc_entry(
            tag_sym_ptr: *const Sexp,
            value_ptr: *const Sexp,
            function_ptr: *const Sexp,
            plist_ptr: *const Sexp,
            constant_ptr: *const Sexp,
            result_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_mirror_bucket_prepend(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            entry_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_mirror_set_value_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_set_function_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_set_constant_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_mirror_install_entry_or_insert(
            mirror_ptr: *const Sexp,
            sym_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        // Doc 111 §111.E #19-#26 Group E — env_lexframe.rs Phase 47
        // rewrites.  Each `nelisp_frame_*' below is the Phase 47-
        // compiled pure-elisp implementation in
        // `lisp/nelisp-cc-frame-*.el' (Doc 115 §115.1-7).  The former
        // `nl_frame_*' Rust shims (whole Phase 47 shims module under
        // `eval/') were deleted in Doc 115 §115.8.
        fn nelisp_frame_stack_depth(frames_ptr: *const Sexp) -> i64;
        fn nelisp_frame_stack_ensure_capacity(
            frames_ptr: *const Sexp,
            needed: i64,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_push(
            frames_ptr: *const Sexp,
            scratch_vec_ptr: *const Sexp,
            _pad: i64,
        ) -> i64;
        fn nelisp_frame_pop(
            frames_ptr: *const Sexp,
            scratch_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_bind(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
            cell_ptr: *const Sexp,
            scratch_pair_slot: *mut Sexp,
            scratch_outer_slot: *mut Sexp,
            scratch_count_slot: *mut Sexp,
        ) -> i64;
        fn nelisp_frame_stack_find(
            frames_ptr: *const Sexp,
            name_ptr: *const Sexp,
        ) -> i64;
        fn nelisp_wrap_alist_cells(
            alist_ptr: *const Sexp,
            result_slot: *mut Sexp,
            work_slot: *mut Sexp,
            name_slot: *mut Sexp,
            cell_slot: *mut Sexp,
            inner_slot: *mut Sexp,
        ) -> i64;
        // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash compiled
        // from `lisp/nelisp-cc-fnv1a.el'.  Replaces the deleted
        // Rust `mirror_fnv1a' free fn + `nl_mirror_fnv1a_sexp'
        // extern wrapper in `env_helpers.rs'.  `str_ptr' must
        // point at a `Sexp::Str(_)' or `Sexp::Symbol(_)'; the
        // helper reads bytes via `str-byte-at' (= matches the
        // 24-byte `String' header layout shared by both arms).
        // Returns: i64 — the 32-bit hash zero-extended into the
        // low 32 bits (= `(logand h #xFFFFFFFF)' after every
        // multiply guarantees the high 32 bits are 0).
        fn nelisp_fnv1a(str_ptr: *const Sexp) -> i64;
        // Doc 116 §116.A — pure-elisp Reader lexer compiled from
        // `lisp/nelisp-cc-reader-lexer.el'.  Reads ONE token at
        // `cursor` from the UTF-8 bytes of `*str_ptr' (= must be
        // `Sexp::Str(_)' / `Sexp::Symbol(_)').  Returns an i64
        // token kind code:
        //
        //   0   EOF                  payload untouched
        //   1   LParen   `('         payload untouched
        //   2   RParen   `)'
        //   3   LBracket `['
        //   4   RBracket `]'
        //   5   Quote    `''
        //   6   Backquote `\\`'
        //   7   Comma    `,'
        //   8   CommaAt  `,@'
        //   9   FunctionQuote `#\\''
        //   10  Dot      `.'
        //   11  SharpsParen `#s('
        //   20  Int                   `*payload_slot' = Sexp::Str(text)
        //   21  Float                 `*payload_slot' = Sexp::Str(text)
        //   22  Str                   `*payload_slot' = Sexp::Str(body)
        //   23  Sym                   `*payload_slot' = Sexp::Str(name)
        //   24  Char  `?X'/`?\\X'     `*payload_slot' = Sexp::Str(body
        //                              bytes; parser-side decoder
        //                              materialises the codepoint).
        //   25  RadixInt `#x..'/`#o..'/  `*payload_slot' = Sexp::Str
        //              `#b..'           with first byte = `x'/`o'/`b'
        //                              base marker + remaining digit text.
        //   -1  Error / unexpected EOF
        //
        // Side effects:
        //   `*cursor_out_slot' is written with `Sexp::Int(next-cursor)'
        //     via the §100.B `sexp-int-make' op.
        //   `*payload_slot' is written with `Sexp::Str(_)' via the
        //     §122.B `mut-str-finalize' op only when kind >= 20.
        //   `*scratch_mutstr_slot' is mutated (= bytes pushed) during
        //     atom / string scanning.
        //
        // Caller must:
        //   1. Pre-init `*payload_slot' to `Sexp::Nil' (= the
        //      `mut-str-finalize' / `sexp-int-make' ops do not drop
        //      a pre-existing payload).
        //   2. Pre-init `*cursor_out_slot' to `Sexp::Nil'.
        //   3. Allocate the scratch MutStr via `mut_str_make_empty'
        //      BEFORE the call.  Reset between calls (= a fresh
        //      `mut_str_make_empty' is the safest pattern).
        fn nelisp_reader_lex_one(
            str_ptr: *const Sexp,
            cursor: i64,
            payload_slot: *mut Sexp,
            cursor_out_slot: *mut Sexp,
            scratch_mutstr_slot: *mut Sexp,
        ) -> i64;
        // Doc 116 §116.B — pure-elisp Reader parser compiled from
        // `lisp/nelisp-cc-reader-parser.el'.  Consumes the §116.A
        // token stream (via internal `extern-call nelisp_reader_lex_one'
        // calls) and writes one parsed top-level `Sexp' value into
        // `*result_slot'.  Returns i64 status (1 = success, anything
        // else = parse error).
        //
        // Args:
        //   str_ptr:     `*const Sexp' (Sexp::Str / Sexp::Symbol with
        //                the UTF-8 source bytes).
        //   cursor_slot: `*mut Sexp::Int(_)' — current byte cursor.
        //                Read via `sexp-int-unwrap'; lexer writes the
        //                next cursor back via the same slot.  Must
        //                be pre-initialised to `Sexp::Int(start_cursor)'.
        //   result_slot: `*mut Sexp', pre-init to `Sexp::Nil';
        //                receives the parsed form.
        //   slot_pool:   `*const Sexp::Vector(N)' of pre-Nil slots.
        //                Layout: slot 0 = SCRATCH MutStr, slot 1 =
        //                PAYLOAD Sexp::Str, slot 2 = CONST-NIL,
        //                slots 3+4d..6+4d = per-depth working slots.
        //                Caller must pre-allocate Sexp::MutStr at
        //                slot 0 (via `nl_alloc_mut_str') and pre-Nil
        //                slot 2.
        //   depth:       i64 — initial recursion depth (= 0 for the
        //                top-level call).
        fn nelisp_reader_parse_one(
            str_ptr: *const Sexp,
            cursor_slot: *mut Sexp,
            result_slot: *mut Sexp,
            slot_pool: *const Sexp,
            depth: i64,
        ) -> i64;
        // Doc 100 §100.D Stage 1 — 12 `nl_jit_arith_*' trampoline
        // swaps.  Defined in `lisp/nelisp-cc-jit-arith.el', wired to
        // `unified_fn_ptr' in `jit/bridge.rs::arith_link'.  These
        // declarations also pin the symbols into the test binary's
        // link line (= the rlib's `bridge::arith_link' extern block
        // alone gets DCE'd by `--gc-sections' before integration
        // tests can call them).
        pub fn nelisp_jit_add2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_sub2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_mul2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_eq2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_lt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_gt2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_le2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ge2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logior2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logand2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_logxor2(a: i64, b: i64) -> i64;
        pub fn nelisp_jit_ash(n: i64, c: i64) -> i64;
        // Doc 110 §110.E.2.a — `jit/float.rs' 4 arithmetic trampoline
        // swaps (add / sub / mul / div).  Defined in
        // `lisp/nelisp-cc-jit-float.el'; wired to `unified_fn_ptr' via
        // the `float_link' module in `jit/bridge.rs'.  These decls
        // also pin the symbols into the integration test binary's
        // link line (= the rlib's `float_link' extern block alone
        // gets DCE'd by `--gc-sections' before tests can call them).
        pub fn nl_jit_float_add(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_sub(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_mul(a: f64, b: f64) -> f64;
        pub fn nl_jit_float_div(a: f64, b: f64) -> f64;
        // Doc 110 §110.C.2.a — 4 ordered comparison trampoline swaps.
        // i64 return matches the `extern "C" fn(f64, f64) -> i64'
        // float.rs cmp shape.  NaN semantics: 0 (= matches Rust).
        pub fn nl_jit_float_lt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_gt(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_le(a: f64, b: f64) -> i64;
        pub fn nl_jit_float_ge(a: f64, b: f64) -> i64;
        // Doc 110 §110.C.2.b — EQ-EPS trampoline.  Returns 1 iff
        // `(a - b).abs() < 1e-15' AND both inputs are ordered (=
        // not NaN), matching the Rust float.rs body bit-for-bit.
        pub fn nl_jit_float_eq_eps(a: f64, b: f64) -> i64;
        // Doc 110 §110.F — 3 `jit/math.rs' trampoline swaps.
        // `float' = identity, `exp' / `log' = libm extern call
        // through the new `(f64-call)' grammar form.  Shape is
        // `extern "C" fn(f64) -> f64' (= xmm0/d0 in, xmm0/d0 out).
        pub fn nl_jit_float_float(x: f64) -> f64;
        pub fn nl_jit_float_exp(x: f64) -> f64;
        pub fn nl_jit_float_log(x: f64) -> f64;
        // Doc 120 §120.A — `jit/predicate.rs' 2 of 4 trampoline swaps
        // (`predicate_eq' + `ref_eq'; `sxhash' + `type_of' stay Rust).
        // Defined in `lisp/nelisp-cc-jit-predicate-eq.el' +
        // `lisp/nelisp-cc-jit-ref-eq.el'; wired to `unified_fn_ptr' via
        // the `predicate_link' module in `jit/bridge.rs'.  These decls
        // also pin the symbols into the integration test binary's
        // link line (= the rlib's `predicate_link' extern block alone
        // gets DCE'd by `--gc-sections' before tests can call them).
        pub fn nelisp_jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64;
        pub fn nelisp_jit_ref_eq(
            a: *const Sexp,
            b: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.B — `jit/box_accessor.rs' 4 of 11 record-family
        // trampoline swaps (`record_type' / `record_len' / `record_ref'
        // / `record_set'; `record_alloc' stays Rust + 6 non-record
        // entries SKIP per blocker notes in `jit/box_accessor.rs').
        // Defined in `lisp/nelisp-cc-jit-record.el'; wired to
        // `unified_fn_ptr' via the `box_accessor_link' module in
        // `jit/bridge.rs'.  Same dead-symbol-on-rlib pinning rationale
        // as the §120.A externs above.
        pub fn nelisp_jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_record_ref(
            arg: *const Sexp,
            idx: i64,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_record_set(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        // Doc 120 §120.D — `jit/access.rs' 4 of 4 trampoline swaps
        // (`length' / `aref' / `aset' / `elt').  Defined in
        // `lisp/nelisp-cc-jit-{length,aref,aset,elt}.el'; wired to
        // `unified_fn_ptr' via the `access_link' module in
        // `jit/bridge.rs'.  Same dead-symbol-on-rlib pinning rationale
        // as the §120.A / §120.B externs above.  The Str / BoolVector
        // sub-arms reach narrow Rust externs in `jit/access.rs' via
        // `extern-call' from the elisp bodies (= same shape
        // `nl_sexp_eq' uses for the §120.A predicate-eq slow path).
        pub fn nelisp_jit_length(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_aset(
            arg: *const Sexp,
            idx: i64,
            val: *const Sexp,
            out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64;
        // Doc 120 §120.C — `jit/cons.rs' 4 of 5 trampoline swaps.
        pub fn nelisp_jit_cons_car(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_cons_cdr(arg: *const Sexp, out: *mut Sexp) -> i64;
        pub fn nelisp_jit_cons_setcar(
            arg: *const Sexp, val: *const Sexp, out: *mut Sexp,
        ) -> i64;
        pub fn nelisp_jit_cons_setcdr(
            arg: *const Sexp, val: *const Sexp, out: *mut Sexp,
        ) -> i64;
    }

    /// Doc 99 §99.B probe — call the elisp-compiled function and return
    /// its result.  Used by `tests/elisp_cc_spike_probe.rs' to prove the
    /// build chain (elisp source → Phase 47 compile → ET_REL .o → ar
    /// static archive → cargo link) terminates in a callable symbol.
    pub fn probe() -> i64 {
        unsafe { nelisp_spike_noop() }
    }

    /// Doc 99 §99.C — i64 factorial implemented in elisp.  Wraps the
    /// `nelisp_fact_i64' extern.  The caller is responsible for the
    /// 0..=20 range invariant; the Rust shim in `eval::builtins'
    /// enforces it before calling.
    pub fn fact_i64(n: i64) -> i64 {
        unsafe { nelisp_fact_i64(n) }
    }

    // Doc 100 §100.C — `(truncate INT)' Int arm, elisp-compiled.
    cc_wrap!(truncate_int: nelisp_truncate_int, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.B — `(length CONS)' proper-list walk, elisp-compiled.
    cc_wrap!(length_cons: nelisp_length_cons, (arg0: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.C — `(eq SYMBOL SYMBOL)' via elisp-compiled Symbol/Str ops.
    cc_wrap!(eq_symbol: nelisp_eq_symbol, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 101 §101.D — `(cons A B)' via elisp-compiled Cons construction ops.
    cc_wrap!(cons_construct: nelisp_cons_construct, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 117 §117.A.2 — `(string-bytes STR)' byte-length via `str-len' + `sexp-int-make'.
    cc_wrap!(bi_string_bytes: nelisp_bi_string_bytes, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 111 §111.B — `(recordp X)' via elisp-compiled Record ops.
    cc_wrap!(recordp: nelisp_recordp, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 111 §111.C — `(aref VECTOR IDX)' via elisp-compiled Vector read ops.
    // Caller-provided `slot' must be initialized to `Sexp::Nil' (bit-shape
    // Copy) so the refcount-aware `nl_sexp_clone_into' helper's `ptr::write'
    // does not Drop arbitrary bytes.
    cc_wrap!(aref_vector: nelisp_aref_vector, (arg0: *const Sexp, arg1: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 117 §117.A.1 — `(make-vector N INIT)' via `vector-make' (§115.1) + fill loop.
    cc_wrap!(bi_make_vector: nelisp_bi_make_vector, (n_ptr: *const Sexp, init_ptr: *const Sexp, slot: *mut Sexp) -> i64);

    // Doc 117 §117.B — QUIT_FLAG atomic ops (set / clear / pending-p) via §122.E CAS.
    cc_wrap!(bi_set_quit_flag: nelisp_bi_set_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_clear_quit_flag: nelisp_bi_clear_quit_flag, (flag_ptr: *mut i64) -> i64);
    cc_wrap!(bi_quit_flag_pending_p: nelisp_bi_quit_flag_pending_p, (flag_ptr: *const i64) -> i64);

    // Doc 117 §117.B / Doc 122 §122.H — I/O syscall sweep (stderr/stdout/stdin).
    // Single libc `write' / `read' per call; non-string args become 0-length no-ops.
    cc_wrap!(bi_write_stderr_line: nelisp_bi_write_stderr_line, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_write_stdout_bytes: nelisp_bi_write_stdout_bytes, (str_ptr: *const Sexp) -> i64);
    cc_wrap!(bi_read_stdin_bytes: nelisp_bi_read_stdin_bytes, (buf_ptr: *mut u8, limit: i64) -> i64);

    // Doc 111 §111.D — Cell ops (value / set-value / make / null-p) via Phase 47 grammar.
    cc_wrap!(cell_value: nelisp_cell_value, (arg0: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_set_value: nelisp_cell_set_value, (arg0: *const Sexp, val_ptr: *const Sexp) -> ());
    cc_wrap!(cell_make: nelisp_cell_make, (val_ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(cell_null_p: nelisp_cell_null_p, (arg0: *const Sexp) -> i64);

    // Doc 122 §122.A — `sexp-write-str' / `sexp-write-symbol' Phase 47 grammar ops.
    cc_wrap!(sexp_write_str: nelisp_sexp_write_str, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);
    cc_wrap!(sexp_write_symbol: nelisp_sexp_write_symbol, (slot: *mut Sexp, bytes_ptr: *const u8, len: i64) -> *mut Sexp);

    /// Doc 122 §122.G — `(sexp-write-float SLOT VAL)' Phase 47 grammar op.
    ///
    /// The Phase 47 MVP forbids mixed-class defun params; the probe
    /// declares both slot + val as f64-class so the slot pointer arrives
    /// in xmm0 as a bit-cast f64.  Internally the elisp emit code
    /// MOVQ's xmm0 back into rdi before calling the Rust extern
    /// `nl_sexp_write_float'.  Caller bit-casts `slot' via
    /// `(slot as u64).to_bits()' / `f64::from_bits(slot as u64)' before
    /// invocation.
    ///
    /// # Safety
    /// - `slot' (after bit-cast back to `*mut Sexp') must be non-null,
    ///   properly aligned, and writable for one Sexp slot.  Pre-init
    ///   to `Sexp::Nil`.
    /// - `val' is any f64 (NaN / Infinity / -0.0 all valid).
    pub unsafe fn sexp_write_float_via_grammar(
        slot: *mut Sexp,
        val: f64,
    ) -> *mut Sexp {
        let slot_bits = f64::from_bits(slot as u64);
        nelisp_sexp_write_float(slot_bits, val)
    }

    /// Doc 122 §122.G — direct Rust-extern entry to
    /// [`nl_sexp_write_float`].  Mirrors the safe-wrapper convention
    /// for `sexp_write_str` / `sexp_write_symbol` (= probe tests can
    /// bypass the elisp grammar op and check the Rust side in isolation).
    ///
    /// # Safety
    /// Same as [`sexp_write_float_via_grammar`].
    pub unsafe fn sexp_write_float_extern(slot: *mut Sexp, val: f64) -> *mut Sexp {
        crate::eval::nlstr::nl_sexp_write_float(slot, val)
    }

    /// Doc 122 §122.G — `nl_str_to_float(bytes, len, slot) -> i64` direct
    /// entry.  Parses the UTF-8 byte range as f64 via
    /// `str::parse::<f64>()' and writes `Sexp::Float(parsed)' into
    /// `*slot' on success (returns 1), or `Sexp::Nil` on failure
    /// (returns 0).
    ///
    /// # Safety
    /// - `bytes_ptr' non-null + valid UTF-8 for `len' bytes (or `len == 0`).
    /// - `slot' non-null + writable for one Sexp slot.  Pre-init to Nil.
    pub unsafe fn str_to_float(
        bytes_ptr: *const u8,
        len: i64,
        slot: *mut Sexp,
    ) -> i64 {
        crate::eval::nlstr::nl_str_to_float(bytes_ptr, len, slot)
    }

    // Doc 122 §122.B — MutStr Phase 47 grammar ops (make-empty / push / len / finalize).
    cc_wrap!(mut_str_make_empty: nelisp_mut_str_make_empty, (slot: *mut Sexp, cap: i64) -> *mut Sexp);
    cc_wrap!(mut_str_push_byte: nelisp_mut_str_push_byte, (ptr: *mut Sexp, byte: i64) -> i64);
    cc_wrap!(mut_str_push_codepoint: nelisp_mut_str_push_codepoint, (ptr: *mut Sexp, cp: i64) -> i64);
    cc_wrap!(mut_str_len: nelisp_mut_str_len, (ptr: *const Sexp) -> i64);
    cc_wrap!(mut_str_finalize: nelisp_mut_str_finalize, (ptr: *const Sexp, slot: *mut Sexp) -> *mut Sexp);

    // Doc 122 §122.D — Str Phase 47 grammar ops (char-count / codepoint-at / alphanumeric-at).
    cc_wrap!(str_char_count: nelisp_str_char_count, (ptr: *const Sexp) -> i64);
    cc_wrap!(str_codepoint_at: nelisp_str_codepoint_at, (ptr: *const Sexp, idx: i64, cp_slot: *mut i64, width_slot: *mut i64) -> i64);
    cc_wrap!(str_is_alphanumeric_at: nelisp_str_is_alphanumeric_at, (ptr: *const Sexp, idx: i64) -> i64);

    // Doc 122 §122.E atomic + raw-mem grammar wrappers — collapsed
    // via §127.A `cc_wrap!'.  All SeqCst ordering except as noted
    // in the elisp `.el' source headers.
    cc_wrap!(atomic_fetch_add: nelisp_atomic_fetch_add, (ptr: *mut i64, delta: i64) -> i64);
    cc_wrap!(atomic_compare_exchange: nelisp_atomic_compare_exchange, (ptr: *mut i64, expected: i64, new_val: i64) -> i64);
    cc_wrap!(ptr_read_u64: nelisp_ptr_read_u64, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u64: nelisp_ptr_write_u64, (ptr: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(ptr_read_u8: nelisp_ptr_read_u8, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u8: nelisp_ptr_write_u8, (ptr: *mut u8, offset: i64, val: i64) -> i64);

    // Doc 122 §122.J — width-{2, 4} raw-mem op wrappers.  Same shape
    // as `_u8' / `_u64' modulo width.  All reads zero-extend to i64
    // (= 0xABCD returns 43981, not -21555).  Writes use unaligned
    // store semantics on the Rust side (`std::ptr::write_unaligned')
    // so libc struct fields at odd offsets inside a packed buffer
    // round-trip cleanly.
    cc_wrap!(ptr_read_u16: nelisp_ptr_read_u16, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u16: nelisp_ptr_write_u16, (ptr: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(ptr_read_u32: nelisp_ptr_read_u32, (ptr: *const u8, offset: i64) -> i64);
    cc_wrap!(ptr_write_u32: nelisp_ptr_write_u32, (ptr: *mut u8, offset: i64, val: i64) -> i64);

    // Doc 122 §122.J — `struct-make' / `struct-field-{set,get}'
    // sugar probe wrappers.  Each calls the matching Phase 47-
    // compiled `.o' object that exercises the parser-level desugar
    // path.  Safety contracts mirror the underlying raw-mem ops:
    // the buffer pointer must be non-null + valid for `(offset +
    // size)' bytes of read/write at unaligned access; `struct-make'
    // uses `alloc-bytes' under the hood so its return value
    // inherits the standard "null = OOM / bad-layout" contract.
    cc_wrap!(struct_make_winsize: nelisp_struct_make_winsize, () -> *mut u8);
    cc_wrap!(struct_field_set_u16: nelisp_struct_field_set_u16, (buf: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(struct_field_get_u16: nelisp_struct_field_get_u16, (buf: *const u8, offset: i64) -> i64);
    cc_wrap!(struct_field_set_u32: nelisp_struct_field_set_u32, (buf: *mut u8, offset: i64, val: i64) -> i64);
    cc_wrap!(struct_field_get_u32: nelisp_struct_field_get_u32, (buf: *const u8, offset: i64) -> i64);

    // Doc 122 §122.J — composed winsize struct writer (4 u16 fields).
    cc_wrap!(winsize_write_full: nelisp_winsize_write_full, (buf: *mut u8, row: i64, col: i64, xpixel: i64, ypixel: i64) -> *mut u8);

    // Doc 125 §125.A — generic byte-level alloc / dealloc; Doc 122 §122.I — cstr {from-sexp, drop}.
    cc_wrap!(alloc_bytes: nelisp_alloc_bytes, (size: i64, align: i64) -> *mut u8);
    cc_wrap!(dealloc_bytes: nelisp_dealloc_bytes, (ptr: *mut u8, size: i64, align: i64) -> i64);
    cc_wrap!(cstr_from_sexp: nelisp_cstr_from_sexp, (str_ptr: *const Sexp) -> *mut u8);
    cc_wrap!(cstr_drop: nelisp_cstr_drop, (buf_ptr: *mut u8, size: i64) -> i64);

    // Doc 117 §117.D.gaps.3 — file-I/O syscall body sweeps.  Each
    // wrapper trivially dispatches to its matching `nelisp_*' extern;
    // the safety preconditions are documented per-function (= the
    // shared invariant is that the input Sexp pointer must be valid
    // and point at a string-y variant, and the Rust-owned result
    // buffer must be appropriately sized).
    cc_wrap!(
        bi_syscall_stat: nelisp_bi_syscall_stat,
        (path_ptr: *const Sexp, statbuf: *mut u8) -> i64
    );
    cc_wrap!(
        bi_syscall_canonicalize: nelisp_bi_syscall_canonicalize,
        (path_ptr: *const Sexp, result_buf: *mut u8) -> i64
    );
    cc_wrap!(
        bi_nl_write_file: nelisp_bi_nl_write_file,
        (path_ptr: *const Sexp, content_ptr: *const Sexp) -> i64
    );
    // Doc 117 §117.D.gaps.3 (cont.) — second file-I/O sweep batch.
    // See the extern decl block above for the per-fn contract.
    cc_wrap!(
        bi_nl_make_directory: nelisp_bi_nl_make_directory,
        (path_ptr: *const Sexp) -> i64
    );
    cc_wrap!(
        bi_syscall_read_file: nelisp_bi_syscall_read_file,
        (path_ptr: *const Sexp, buf_ptr: *mut u8, read_size: i64) -> i64
    );

    // Doc 122 §122.C libm probes + Doc 123 §123.A-C rc primitive
    // wrappers — collapsed via §127.A `cc_wrap!'.
    cc_wrap!(libm_sqrt: nelisp_libm_sqrt, (x: f64) -> f64);
    cc_wrap!(libm_sin: nelisp_libm_sin, (x: f64) -> f64);
    cc_wrap!(libm_cos: nelisp_libm_cos, (x: f64) -> f64);
    cc_wrap!(rc_inc: nelisp_rc_inc, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_dec: nelisp_rc_dec, (box_ptr: *mut i64) -> i64);
    cc_wrap!(rc_strong_count: nelisp_rc_strong_count, (box_ptr: *const u8) -> i64);
    cc_wrap!(rc_kind: nelisp_rc_kind, (sexp_ptr: *const u8) -> i64);

    // Doc 123 §123.D — pure-elisp `nelisp_rc_payload_ptr' kernel.
    // Reads inner NlBox* payload pointer at offset 8 (= `SEXP_PAYLOAD_OFFSET').
    // Tag-dispatch is caller's responsibility — this wrapper performs the raw
    // load unconditionally.
    cc_wrap!(rc_payload_ptr: nelisp_rc_payload_ptr, (sexp_ptr: *const u8) -> i64);

    // Doc 123 §123.D — pure-elisp `nelisp_gc_walk_children' kernel.
    // Materializes the 2-list `Sexp::Cons((car . (cdr . nil)))' in
    // `result_slot' using `tail_slot' as scratch.  Cons-only — caller must
    // filter non-Cons inputs.  Returns `result_slot' for caller ergonomics.
    cc_wrap!(gc_walk_children: nelisp_gc_walk_children, (sexp_ptr: *const Sexp, result_slot: *mut Sexp, tail_slot: *mut Sexp) -> *mut Sexp);

    // Doc 124 §124.A — pure-elisp `nelisp_nlconsbox_clone' kernel.
    // Bumps refcount at NlConsBox offset 64 via §122.E `atomic-fetch-add'
    // (delta=+1), then returns `box_ptr' unchanged.
    cc_wrap!(nlconsbox_clone: nelisp_nlconsbox_clone, (box_ptr: *mut i64) -> i64);

    // Doc 127 §127.A — Doc 124 NlBox Clone/Drop kernel wrappers.
    // The 9 wrappers below are trivially-identical dispatches to
    // their `nelisp_nl{type}_{clone,drop}' extern counterparts; the
    // per-type layout literals (REFCOUNT_OFFSET / SIZE / ALIGN) live
    // in the elisp .o source headers (lisp/nelisp-cc-nl*-{clone,drop}.el)
    // and the §124.A-K design doc, not duplicated in Rust docstrings.
    cc_wrap!(nlconsbox_drop: nelisp_nlconsbox_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_drop: nelisp_nlvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_drop: nelisp_nlcell_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_drop: nelisp_nlrecord_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_drop: nelisp_nlstr_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlvector_clone: nelisp_nlvector_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlcell_clone: nelisp_nlcell_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlrecord_clone: nelisp_nlrecord_clone, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlstr_clone: nelisp_nlstr_clone, (box_ptr: *mut i64) -> i64);
    // Doc 124 §124.L+ — Drop kernels for the remaining 2 NlBox types
    // (NlBoolVector + NlCharTable).  Per-type layout literals (SIZE /
    // REFCOUNT_OFFSET / ALIGN) live in `lisp/nelisp-cc-nl{boolvector,
    // chartable}-drop.el' headers, not duplicated here.
    cc_wrap!(nlboolvector_drop: nelisp_nlboolvector_drop, (box_ptr: *mut i64) -> i64);
    cc_wrap!(nlchartable_drop: nelisp_nlchartable_drop, (box_ptr: *mut i64) -> i64);

    /// Doc 111 §111.E #1 — Phase 47 `mirror_lookup_entry' probe wrapper.
    ///
    /// Walks the env-mirror fast-hash-table for `sym_ptr` (= a
    /// `Sexp::Symbol(_)' / `Sexp::Str(_)') and returns the raw
    /// `*const Sexp' of the matching symbol-entry Record, or 0 on
    /// miss / empty mirror.  The returned pointer is *not* refcount-
    /// bumped — it borrows the slot owned by the bucket's `(KEY .
    /// ENTRY)` cons pair, so callers must clone (`nl_sexp_clone_into`)
    /// before storing the result anywhere that outlives the mirror.
    ///
    /// Used by `tests/elisp_cc_mirror_lookup_entry_probe.rs' to drive
    /// the §111.E #1 verification gate.  Production callers (= the
    /// env_mirror.rs Rust impl + its 5 compose-on-it siblings) still
    /// route through `Env::mirror_lookup_entry' — the extern wrapper
    /// dispatch swap lands in a follow-up commit after all Group A/B
    /// helpers ship.
    ///
    /// # Safety
    /// - `mirror_ptr' must be non-null and point at a `Sexp::Record(_)'
    ///   built by `Env::install_empty_mirror_rust_direct' (or its
    ///   `mirror_install_entry' descendants).  The pre-conditions
    ///   listed on the extern decl must hold.
    /// - `sym_ptr' must be non-null and point at a `Sexp::Symbol(_)`
    ///   or `Sexp::Str(_)`.
    /// - The returned pointer is only valid while `*mirror_ptr` and
    ///   its bucket chain remain unchanged.
    pub unsafe fn mirror_lookup_entry(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
    ) -> *const Sexp {
        nelisp_mirror_lookup_entry(mirror_ptr, sym_ptr) as *const Sexp
    }

    // Doc 111 §111.E #2-12 — Phase 47 mirror_* probe wrappers, collapsed
    // via §127.A `cc_wrap!'.
    cc_wrap!(mirror_lookup_value: nelisp_mirror_lookup_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_lookup_function: nelisp_mirror_lookup_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, result_slot: *mut Sexp) -> *mut Sexp);
    cc_wrap!(mirror_is_bound: nelisp_mirror_is_bound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_fbound: nelisp_mirror_is_fbound, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_is_constant: nelisp_mirror_is_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_value: nelisp_mirror_set_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, val_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_function: nelisp_mirror_set_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, val_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_value: nelisp_mirror_clear_value, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_clear_function: nelisp_mirror_clear_function, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, unbound_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_set_constant: nelisp_mirror_set_constant, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, flag_ptr: *const Sexp) -> i64);
    cc_wrap!(mirror_install_entry: nelisp_mirror_install_entry, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp) -> i64);

    // ---- Doc 119 §119.A auto-vivify fold ---------------------------
    //
    // Building-block + wrapper safe wrappers.  Each function manages
    // the per-call scratch `Sexp::Vector' that the elisp helpers
    // consume; callers in `eval/env_helpers.rs' don't need to know the
    // slot layout (see the file commentary of
    // `lisp/nelisp-cc-mirror-set-value-or-insert.el' for the layout
    // reference).
    //
    // The safe wrappers all follow the same pattern:
    //   1. Build a `Sexp::Vector' of 11 slots, pre-filling slots 0..6
    //      with `Sexp::Nil' / tag symbols / scratch and slots 7..10
    //      with the caller-supplied `value' / `function' / `plist' /
    //      `constant' Sexps (= cloned into the vector, refcount-bumped
    //      for box variants).
    //   2. Call the Phase 47 helper which dispatches hit / miss and
    //      writes the final state into the mirror record graph.
    //   3. Drop the scratch vector when the wrapper returns; each
    //      slot's `Sexp' clone refcount-decrements, leaving the mirror
    //      as the sole steady-state owner of the entry record graph.

    // Doc 119 §119.A — Phase 47 `mirror_alloc_entry' probe wrapper.
    cc_wrap!(mirror_alloc_entry: nelisp_mirror_alloc_entry, (tag_sym_ptr: *const Sexp, value_ptr: *const Sexp, function_ptr: *const Sexp, plist_ptr: *const Sexp, constant_ptr: *const Sexp, result_slot: *mut Sexp) -> i64);

    // Doc 119 §119.A — Phase 47 `mirror_bucket_prepend' probe wrapper.
    cc_wrap!(mirror_bucket_prepend: nelisp_mirror_bucket_prepend, (mirror_ptr: *const Sexp, sym_ptr: *const Sexp, entry_ptr: *const Sexp, scratch_vec_ptr: *const Sexp) -> i64);

    /// Doc 119 §119.A — build the 11-slot scratch `Sexp::Vector' used
    /// by the four `_or_insert' wrappers.  Slots 0..4 are pre-filled
    /// with `Sexp::Nil' (= caller-owned scratches for `bucket_prepend');
    /// slot 5 holds the `symbol-entry' tag symbol; slot 6 starts as
    /// `Sexp::Nil' (= filled by `alloc_entry' on miss).  Slots 7..10
    /// hold the caller-supplied value / function / plist / constant
    /// Sexps (= refcount-bumped via Vec clone).
    fn build_or_insert_scratch_vec(
        value: Sexp, function: Sexp, plist: Sexp, constant: Sexp,
    ) -> Sexp {
        Sexp::vector(vec![
            Sexp::Nil,                                  // 0: Nil source
            Sexp::Nil,                                  // 1: inner-pair scratch
            Sexp::Nil,                                  // 2: outer-cell scratch
            Sexp::Nil,                                  // 3: count int scratch
            Sexp::Nil,                                  // 4: KEY str scratch
            Sexp::Symbol("symbol-entry".into()),        // 5: type tag
            Sexp::Nil,                                  // 6: entry result
            value,                                      // 7: value cell
            function,                                   // 8: function cell
            plist,                                      // 9: plist
            constant,                                   // 10: constant flag
        ])
    }

    /// Doc 119 §119.A — `mirror_set_value' counterpart that absorbs
    /// the miss-path.  Auto-vivifies a fresh `symbol-entry' record
    /// with VALUE in slot 0 and UNBOUND in slot 1 (= function cell
    /// default) when NAME is not already in the mirror.
    pub unsafe fn mirror_set_value_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        value_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        // SAFETY: `value_ptr' / `unbound_ptr' borrow stack-local /
        // `Env'-owned `Sexp's that outlive the call.  Clone-into-vector
        // refcount-bumps the box variants; the scratch vector drop on
        // wrapper return rebalances the count.
        let scratch = build_or_insert_scratch_vec(
            (*value_ptr).clone(),
            (*unbound_ptr).clone(),
            Sexp::Nil,
            Sexp::Nil,
        );
        nelisp_mirror_set_value_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    /// Doc 119 §119.A — `mirror_set_function' counterpart that absorbs
    /// the miss-path.  Auto-vivifies a fresh `symbol-entry' record
    /// with UNBOUND in slot 0 and FUNC in slot 1.
    pub unsafe fn mirror_set_function_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        func_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*unbound_ptr).clone(),
            (*func_ptr).clone(),
            Sexp::Nil,
            Sexp::Nil,
        );
        nelisp_mirror_set_function_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    /// Doc 119 §119.A — `mirror_set_constant' counterpart that absorbs
    /// the miss-path.  Auto-vivifies a fresh `symbol-entry' record
    /// with UNBOUND in slots 0/1 (= value + function default) and
    /// FLAG in slot 3.
    pub unsafe fn mirror_set_constant_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        flag_ptr: *const Sexp,
        unbound_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*unbound_ptr).clone(),
            (*unbound_ptr).clone(),
            Sexp::Nil,
            (*flag_ptr).clone(),
        );
        nelisp_mirror_set_constant_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    /// Doc 119 §119.A — `mirror_install_entry' counterpart that
    /// absorbs the miss-path.  All four slot Sexps come from the
    /// caller (= `intern_constant' / image baker).
    pub unsafe fn mirror_install_entry_or_insert(
        mirror_ptr: *const Sexp,
        sym_ptr: *const Sexp,
        value_ptr: *const Sexp,
        function_ptr: *const Sexp,
        plist_ptr: *const Sexp,
        constant_ptr: *const Sexp,
    ) -> i64 {
        let scratch = build_or_insert_scratch_vec(
            (*value_ptr).clone(),
            (*function_ptr).clone(),
            (*plist_ptr).clone(),
            (*constant_ptr).clone(),
        );
        nelisp_mirror_install_entry_or_insert(
            mirror_ptr, sym_ptr,
            &scratch as *const Sexp,
            0,
        )
    }

    // ---- Doc 111 §111.E Group E (env_lexframe.rs) probes -----------

    cc_wrap!(frame_stack_depth: nelisp_frame_stack_depth, (frames_ptr: *const Sexp) -> i64);

    pub unsafe fn frame_stack_ensure_capacity(
        frames_ptr: *const Sexp,
        needed: i64,
    ) -> i64 {
        // Doc 115 §115.1 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-ensure-capacity.el' takes a 3rd
        // `scratch_slot' parameter (= caller-owned `*mut Sexp' to
        // hold the freshly-allocated `Sexp::Vector' before installing
        // it into the frames-record's slot 0).  We allocate a stack-
        // local `Sexp::Nil' here and pass its pointer so the public
        // 2-arg API is preserved for existing callers / probes.
        let mut scratch = Sexp::Nil;
        nelisp_frame_stack_ensure_capacity(
            frames_ptr,
            needed,
            &mut scratch as *mut Sexp,
        )
    }

    pub unsafe fn frame_push(frames_ptr: *const Sexp) -> i64 {
        // Doc 115 §115.3 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-push.el' takes a 7-slot `Sexp::Vector'
        // scratch (= two type-tag symbols + five scratch slots) that
        // the safe wrapper allocates internally so the public 1-arg
        // API is preserved.  See the elisp commentary for the per-slot
        // layout and refcount discipline.  The third `_pad' i64 arg is
        // ignored by the helper but flips outer-defun arity to odd (=
        // 3), which matches the static rsp-alignment assumption baked
        // into `vector-make' / `record-make' / `vector-slot-set'.
        let scratch_vec = Sexp::vector(vec![
            Sexp::Symbol("nelisp-lexframe".into()),
            Sexp::Symbol("fast-hash-table".into()),
            Sexp::Nil, // ensure_capacity scratch
            Sexp::Nil, // bucket vector scratch
            Sexp::Nil, // fast-hash-table record scratch
            Sexp::Nil, // lexframe record scratch
            Sexp::Nil, // reusable Sexp::Int scratch
        ]);
        nelisp_frame_push(
            frames_ptr,
            &scratch_vec as *const Sexp,
            0,
        )
    }

    pub unsafe fn frame_pop(frames_ptr: *const Sexp) -> i64 {
        // Doc 115 §115.2 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-pop.el' takes a 2nd `scratch_slot'
        // parameter (= caller-owned `*mut Sexp' initialised to
        // `Sexp::Nil'; used both as the Nil-source for `vector-slot-
        // set' on the backing element and then overwritten in place
        // with the new depth `Sexp::Int' before `record-slot-set' on
        // slot 1).  We allocate a stack-local `Sexp::Nil' here and
        // pass its pointer so the public 1-arg API is preserved for
        // existing callers / probes.
        let mut scratch = Sexp::Nil;
        nelisp_frame_pop(frames_ptr, &mut scratch as *mut Sexp)
    }

    pub unsafe fn frame_bind(
        frames_ptr: *const Sexp,
        name_ptr: *const Sexp,
        cell_ptr: *const Sexp,
    ) -> i64 {
        // Doc 115 §115.5 — the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-bind.el' takes 3 caller-owned scratch
        // `*mut Sexp' slots (= the freshly-allocated inner pair, outer
        // bucket cell, and `Sexp::Int(new-count)' for the slot-2 bump).
        // We allocate stack-local `Sexp::Nil's here and pass their
        // pointers so the public 3-arg API is preserved for existing
        // callers / probes.
        let mut scratch_pair = Sexp::Nil;
        let mut scratch_outer = Sexp::Nil;
        let mut scratch_count = Sexp::Nil;
        nelisp_frame_bind(
            frames_ptr,
            name_ptr,
            cell_ptr,
            &mut scratch_pair as *mut Sexp,
            &mut scratch_outer as *mut Sexp,
            &mut scratch_count as *mut Sexp,
        )
    }

    pub unsafe fn frame_stack_find(
        frames_ptr: *const Sexp,
        name_ptr: *const Sexp,
    ) -> *const Sexp {
        // Doc 115 §115.6 — the Rust shim `nl_frame_stack_find' has
        // been replaced by the pure-elisp implementation in
        // `lisp/nelisp-cc-frame-stack-find.el'.  The elisp body
        // returns the cell-slot pointer encoded as i64 (or 0 on
        // miss); we widen back to `*const Sexp' for the public API.
        nelisp_frame_stack_find(frames_ptr, name_ptr) as *const Sexp
    }

    pub unsafe fn wrap_alist_cells(
        alist_ptr: *const Sexp,
        result_slot: *mut Sexp,
    ) -> i64 {
        // Doc 115 §115.4 — the pure-elisp implementation in
        // `lisp/nelisp-cc-wrap-alist-cells.el' takes 4 extra scratch-
        // slot pointers for the per-iteration cell-wrap +
        // name-clone-into + single-slot ping-pong outer-cons build.
        // Stack-allocate them here as `Sexp::Nil' so the public 2-arg
        // API is preserved for existing callers.
        let mut work_slot = Sexp::Nil;
        let mut name_slot = Sexp::Nil;
        let mut cell_slot = Sexp::Nil;
        let mut inner_slot = Sexp::Nil;
        let rc = nelisp_wrap_alist_cells(
            alist_ptr,
            result_slot,
            &mut work_slot as *mut Sexp,
            &mut name_slot as *mut Sexp,
            &mut cell_slot as *mut Sexp,
            &mut inner_slot as *mut Sexp,
        );
        // Mem-forget the work slots' contents to prevent the
        // wrapper-local Drops from decrementing refcounts on heap
        // nodes that the result chain already accounts for.  Phase
        // 47's `cons-make' / `cell-make' raw 32-byte copies do not
        // bump refcount on nested boxed payloads (= MVP ownership
        // constraint), so each result-chain reference is matched 1:1
        // with the original `+1' from `nl_alloc_consbox' /
        // `nl_alloc_cell' / `nl_sexp_clone_into'.  The work slots
        // hold "extra" handles whose refcount-claim was never
        // actually counted; Drop on those would underflow.
        core::ptr::write(&mut work_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut name_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut cell_slot as *mut Sexp, Sexp::Nil);
        core::ptr::write(&mut inner_slot as *mut Sexp, Sexp::Nil);
        rc
    }

    // Doc 115 §115.7 — pure-elisp 32-bit FNV-1a hash (Str/Symbol arms).
    cc_wrap!(fnv1a: nelisp_fnv1a, (str_ptr: *const Sexp) -> i64);

    // Doc 116 §116.A / §116.B — pure-elisp Reader lexer + parser.
    cc_wrap!(reader_lex_one: nelisp_reader_lex_one, (str_ptr: *const Sexp, cursor: i64, payload_slot: *mut Sexp, cursor_out_slot: *mut Sexp, scratch_mutstr_slot: *mut Sexp) -> i64);
    cc_wrap!(reader_parse_one: nelisp_reader_parse_one, (str_ptr: *const Sexp, cursor_slot: *mut Sexp, result_slot: *mut Sexp, slot_pool: *const Sexp, depth: i64) -> i64);

    /// Doc 100 §100.D Stage 1 probes — thin safe wrappers around the
    /// 12 elisp-compiled jit/arith trampolines.  Used by
    /// `tests/elisp_cc_jit_arith_probe.rs' to assert every member of
    /// the `.o' archive resolves and computes the expected i64 result.
    /// `unified_fn_ptr` in `jit/bridge.rs` resolves the same symbols
    /// at runtime via the `arith_link' submodule, but those
    /// references alone get DCE'd in test-bin builds; surfacing the
    /// externs through `pub fn' here pins the link.
    pub fn jit_add2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_add2(a, b) } }
    pub fn jit_sub2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_sub2(a, b) } }
    pub fn jit_mul2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_mul2(a, b) } }
    pub fn jit_eq2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_eq2(a, b) } }
    pub fn jit_lt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_lt2(a, b) } }
    pub fn jit_gt2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_gt2(a, b) } }
    pub fn jit_le2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_le2(a, b) } }
    pub fn jit_ge2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_ge2(a, b) } }
    pub fn jit_logior2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logior2(a, b) } }
    pub fn jit_logand2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logand2(a, b) } }
    pub fn jit_logxor2(a: i64, b: i64) -> i64 { unsafe { nelisp_jit_logxor2(a, b) } }
    pub fn jit_ash(n: i64, c: i64) -> i64 { unsafe { nelisp_jit_ash(n, c) } }

    /// Doc 110 §110.E.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled `jit/float.rs' replacements (add / sub / mul /
    /// div).  Used by `tests/elisp_cc_jit_float_probe.rs' to assert
    /// every member of the `.o' archive resolves and computes the
    /// expected f64 result.  The comparison trampolines (eq_eps /
    /// lt / gt / le / ge) keep their Rust impl until §110.E.2.b.
    pub fn jit_float_add(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_add(a, b) } }
    pub fn jit_float_sub(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_sub(a, b) } }
    pub fn jit_float_mul(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_mul(a, b) } }
    pub fn jit_float_div(a: f64, b: f64) -> f64 { unsafe { nl_jit_float_div(a, b) } }

    /// Doc 110 §110.C.2.a probes — thin safe wrappers around the 4
    /// elisp-compiled ordered comparison trampolines (lt / gt / le
    /// / ge).  Result is 0 or 1 as i64.  NaN inputs return 0 to
    /// match Rust's `<' / `>' / `<=' / `>=' semantics.  Used by
    /// `tests/elisp_cc_jit_float_probe.rs' to assert every member
    /// resolves and produces the expected boolean.
    pub fn jit_float_lt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_lt(a, b) } }
    pub fn jit_float_gt(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_gt(a, b) } }
    pub fn jit_float_le(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_le(a, b) } }
    pub fn jit_float_ge(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_ge(a, b) } }

    /// Doc 110 §110.C.2.b probe — thin safe wrapper for EQ-EPS.
    /// Result: 1 iff `(a - b).abs() < 1e-15' AND ordered.
    /// NaN inputs return 0.
    pub fn jit_float_eq_eps(a: f64, b: f64) -> i64 { unsafe { nl_jit_float_eq_eps(a, b) } }

    /// Doc 110 §110.F probes — thin safe wrappers around the 3
    /// elisp-compiled `jit/math.rs' replacements (float identity,
    /// exp, log).  `float' is pure pass-through; `exp' / `log'
    /// delegate to libm via the §110.F `(f64-call)' grammar form.
    pub fn jit_float_float(x: f64) -> f64 { unsafe { nl_jit_float_float(x) } }
    pub fn jit_float_exp(x: f64) -> f64 { unsafe { nl_jit_float_exp(x) } }
    pub fn jit_float_log(x: f64) -> f64 { unsafe { nl_jit_float_log(x) } }

    /// Doc 120 §120.A probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_predicate_eq' trampoline.  ABI matches the
    /// deleted Rust `nl_jit_predicate_eq': `(*const Sexp, *const Sexp)
    /// -> i64' returning 1 iff `(eq a b)' else 0.
    ///
    /// # Safety
    /// - `a' / `b' must be non-null pointers to initialized `Sexp's.
    pub unsafe fn jit_predicate_eq(a: *const Sexp, b: *const Sexp) -> i64 {
        nelisp_jit_predicate_eq(a, b)
    }

    /// Doc 120 §120.A probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_ref_eq' trampoline.  ABI matches the
    /// deleted Rust `nl_jit_ref_eq': `(*const Sexp, *const Sexp, *mut
    /// Sexp) -> i64'; writes `Sexp::T' / `Sexp::Nil' into `*out' and
    /// returns 0 (= always succeeds).
    ///
    /// # Safety
    /// - `a' / `b' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_ref_eq(a: *const Sexp, b: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_ref_eq(a, b, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_type' trampoline.  ABI matches the
    /// pre-§120.B Rust impl in `jit/box_accessor.rs': `(*const Sexp,
    /// *mut Sexp) -> i64' returning 0 on OK (Record arg, `*out =
    /// arg.type_tag'), 1 on ERR (non-Record).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_type(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_record_type(arg, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_len' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, *mut Sexp) -> i64'
    /// returning 0 on OK (Record arg, `*out = Sexp::Int(slots.len)`),
    /// 1 on ERR (non-Record).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_len(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_record_len(arg, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_ref' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, i64, *mut Sexp) -> i64'
    /// returning 0 on OK (Record arg + idx in [0, slots.len), `*out =
    /// arg.slots[idx]'), 1 on ERR (non-Record OR OOR).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_ref(
        arg: *const Sexp,
        idx: i64,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_record_ref(arg, idx, out)
    }

    /// Doc 120 §120.B probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_record_set' trampoline.  ABI matches the
    /// pre-§120.B Rust impl: `(*const Sexp, i64, *const Sexp, *mut
    /// Sexp) -> i64' returning 0 on OK (Record + OOR-valid idx,
    /// `slots[idx] := clone(*val)` and `*out = clone(*val)`), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' / `val' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_record_set(
        arg: *const Sexp,
        idx: i64,
        val: *const Sexp,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_record_set(arg, idx, val, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_length' trampoline.  ABI matches the
    /// pre-§120.D Rust impl in `jit/access.rs': `(*const Sexp, *mut
    /// Sexp) -> i64' returning 0 on OK (Nil / Vector input, `*out =
    /// Sexp::Int(len)'), 1 on ERR (Str / other variant — strategy.el
    /// `condition-case' fallback handles `string' via
    /// `nelisp--mut-str-len').
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_length(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_length(arg, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_aref' trampoline.  ABI: `(*const Sexp,
    /// i64, *mut Sexp) -> i64' returning 0 on OK (Vector / BoolVector
    /// in-range), 1 on ERR (negative idx / OOR / non-array tag).
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_aref(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
        nelisp_jit_aref(arg, idx, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_aset' trampoline.  ABI: `(*const Sexp,
    /// i64, *const Sexp, *mut Sexp) -> i64' returning 0 on OK
    /// (Vector / BoolVector in-range), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' / `val' must be non-null pointers to initialized `Sexp's.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_aset(
        arg: *const Sexp,
        idx: i64,
        val: *const Sexp,
        out: *mut Sexp,
    ) -> i64 {
        nelisp_jit_aset(arg, idx, val, out)
    }

    /// Doc 120 §120.D probe — thin safe wrapper around the Phase 47-
    /// compiled `nelisp_jit_elt' trampoline.  ABI: `(*const Sexp,
    /// i64, *mut Sexp) -> i64' returning 0 on OK (Vector / Cons-list
    /// in-range), 1 on ERR.
    ///
    /// # Safety
    /// - `arg' must be a non-null pointer to an initialized `Sexp'.
    /// - `out' must be non-null, properly aligned, and writable for
    ///   one 32-byte Sexp slot pre-initialised to `Sexp::Nil'.
    pub unsafe fn jit_elt(arg: *const Sexp, idx: i64, out: *mut Sexp) -> i64 {
        nelisp_jit_elt(arg, idx, out)
    }

    /// Doc 120 §120.C safe wrappers — see `jit/cons.rs' for ABI.
    ///
    /// # Safety
    /// - Pointer args must be non-null and point at initialized `Sexp's.
    /// - `out' must be writable for one 32-byte slot pre-initialised to
    ///   `Sexp::Nil'.
    pub unsafe fn jit_cons_car(arg: *const Sexp, out: *mut Sexp) -> i64 {
        nelisp_jit_cons_car(arg, out)
    }
    // See `jit_cons_car`.
    cc_wrap!(jit_cons_cdr: nelisp_jit_cons_cdr, (arg: *const Sexp, out: *mut Sexp) -> i64);
    // See `jit_cons_car`.
    cc_wrap!(jit_cons_setcar: nelisp_jit_cons_setcar, (arg: *const Sexp, val: *const Sexp, out: *mut Sexp) -> i64);
    // See `jit_cons_car`.
    cc_wrap!(jit_cons_setcdr: nelisp_jit_cons_setcdr, (arg: *const Sexp, val: *const Sexp, out: *mut Sexp) -> i64);
}
