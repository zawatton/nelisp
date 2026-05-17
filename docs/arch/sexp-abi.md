# Sexp ABI — frozen byte-layout contract

**Status**: SHIPPED (Doc 100 v2 §100.B, 2026-05-12; Doc 101 §101.A extension, 2026-05-17)
**Scope**: byte-precise layout of `Sexp` and its `Sexp::Int` (Doc 100), `Sexp::Cons` / `Sexp::Symbol` / `Sexp::Str` (Doc 101) variants — sufficient for Doc 100 §100.C plus Doc 101 §101.B-D swaps.

This document is the **single source of truth** for the byte layout of
`Sexp` values that Phase 47-compiled elisp `.o` objects read or write
directly.  Three artifacts encode the same numbers:

1. This file — human-readable spec.
2. `build-tool/src/eval/sexp_abi_assert.rs` — Rust-side `const_assert!`
   block that fails compilation if the underlying enum drifts.
3. `lisp/nelisp-sexp-layout.el` — elisp `defconst` constants the Phase
   47 compiler reads when emitting load / store offsets.

The `make sexp-abi-check` target runs a tiny driver that prints the
Rust-computed values and diffs them against the elisp constants.  CI
fails on drift; the elisp `.o` therefore can never load against a Rust
enum whose layout silently changed.

---

## 1. The `Sexp` enum

```rust
#[derive(Debug, Clone, PartialEq)]
#[repr(C, u8)]
pub enum Sexp {
    Nil,                  // tag 0
    T,                    // tag 1
    Int(i64),             // tag 2 + 8-byte payload
    Float(f64),           // tag 3 + 8-byte payload
    Symbol(String),       // tag 4 + 24-byte String
    Str(String),          // tag 5 + 24-byte String
    MutStr(NlStrRef),     // tag 6 + 8-byte handle
    Cons(NlConsBoxRef),   // tag 7 + 8-byte handle
    Vector(NlVectorRef),  // tag 8 + 8-byte handle
    // CharTable / BoolVector / Cell / Record have tags 9..12 but
    // their payload layouts are not required by Doc 100 / 101.
}
```

`#[repr(C, u8)]` pins the layout as:

```
+----+--------------------------------+
| tag (1 byte) | padding (7 bytes)   |
+----+--------------------------------+
| payload (up to 24 bytes)           |
+----+--------------------------------+
| unused tail (to round up to 32)    |
+----+--------------------------------+
```

Defined in `build-tool/src/eval/sexp.rs:57`.

## 2. Tag byte values

| Variant       | Tag (decimal) | Constant                 |
|---------------|--------------:|--------------------------|
| `Nil`         | 0             | `SEXP_TAG_NIL`           |
| `T`           | 1             | `SEXP_TAG_T`             |
| `Int(i64)`    | 2             | `SEXP_TAG_INT`           |
| `Float(f64)`  | 3             | `SEXP_TAG_FLOAT`         |
| `Symbol(_)`   | 4             | `SEXP_TAG_SYMBOL`        |
| `Str(_)`      | 5             | `SEXP_TAG_STR`           |
| `MutStr(_)`   | 6             | `SEXP_TAG_MUT_STR`       |
| `Cons(_)`     | 7             | `SEXP_TAG_CONS`          |
| `Vector(_)`   | 8             | `SEXP_TAG_VECTOR`        |
| `CharTable`   | 9             | `SEXP_TAG_CHAR_TABLE`    |
| `BoolVector`  | 10            | `SEXP_TAG_BOOL_VECTOR`   |
| `Cell`        | 11            | `SEXP_TAG_CELL`          |
| `Record`      | 12            | `SEXP_TAG_RECORD`        |

Adding a variant **must append** at the end of this table and the
enum.  Re-ordering invalidates every compiled `.o` and the layout
constants in `nelisp-sexp-layout.el`.

## 3. Offsets

| Field            | Offset | Size |
|------------------|-------:|-----:|
| tag byte         | 0      | 1    |
| pad              | 1      | 7    |
| payload start    | 8      | (variant-specific) |
| total slot       | —      | 32   |

The 8-byte payload start is `SEXP_PAYLOAD_OFFSET` in
`build-tool/src/eval/sexp.rs:270`.  32-byte total comes from
`size_of::<Sexp>()` and is verified by the assertion in
`sexp_abi_assert.rs`.

## 4. `Sexp::Int(n)` payload

The payload is an 8-byte signed integer stored little-endian at
offset 8:

```
offset:  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16..31
value:   02 00 00 00 00 00 00 00 <----i64 payload----> <unused>
         ^^                       ^^
         tag = SEXP_TAG_INT       n as i64, little-endian
```

The bytes at offsets `[16, 32)` are not initialized by an `Int` value
and must not be read.  Rust's `Drop` for `Sexp::Int` is a no-op so
leaving them uninitialized is sound.

## 5. Direct-access instruction templates

For x86_64 Linux (SysV AMD64), the Phase 47 compiler emits the
following instruction templates when handling the §100.B grammar
forms.  Phase 47's macro-asm layer encodes these into `.text`
bytes; the linker resolves no relocations because all addressing
is register-indirect with an immediate `imm8` displacement.

### 5.1 `(sexp-tag PTR)` — read tag byte at offset 0

```asm
movzx rax, byte ptr [rdi]    ; 48 0F B6 07   (4 bytes)
```

`PTR` (= the `*const Sexp` to read) must be in `rdi` at the point of
this instruction; the compiler emits the sub-expression that
produces `PTR` before this.  Result is the tag byte zero-extended to
a 64-bit value in `rax`.

### 5.2 `(sexp-int-unwrap PTR)` — read i64 payload at offset 8

```asm
mov rax, qword ptr [rdi + 8] ; 48 8B 47 08   (4 bytes)
```

Pre-condition: the variant tag at `[rdi]` is `SEXP_TAG_INT`.  The
compiler relies on the caller to have checked the tag (or to know
statically that the value is `Sexp::Int`).  Result is the i64
payload in `rax`.

### 5.3 `(sexp-int-make SLOT N)` — write `Sexp::Int(N)` into SLOT

```asm
mov byte ptr [rdi], 2        ; C6 07 02       (3 bytes)
mov qword ptr [rdi + 8], rsi ; 48 89 77 08    (4 bytes)
mov rax, rdi                 ; 48 89 F8       (3 bytes)
```

`SLOT` (= the `*mut Sexp` 32-byte buffer to initialize) is in `rdi`;
`N` (= the i64 payload) is in `rsi`.  The caller-owned-slot is
returned in `rax` for ergonomics (= matches the standard Sexp ABI
return register convention).

The bytes at `[rdi + 1, rdi + 8)` and `[rdi + 16, rdi + 32)` are left
unmodified.  Rust's `Sexp::Int` Drop reads only the tag byte to
dispatch, then drops the i64 payload (= no allocation, no destructor),
so a partially-initialized slot is sound as long as the tag byte is
written.

## 6. `Sexp::Cons(NlConsBoxRef)` payload (Doc 101 §101.A)

`Sexp::Cons` carries an 8-byte `NonNull<NlConsBox>` handle at offset 8 of the Sexp slot.  The pointed-at `NlConsBox` is a `#[repr(C)]` struct with car / cdr / refcount at fixed offsets (defined in `build-tool/src/eval/nlconsbox.rs:56-67`):

```
NlConsBox layout (offsets, sizes in bytes):
    [0, 32)    car         (Sexp, 32 bytes)
    [32, 64)   cdr         (Sexp, 32 bytes)
    [64, 72)   refcount    (AtomicUsize, 8 bytes)
    total:     72 bytes
```

Sexp slot for `Sexp::Cons`:

```
offset:   0  1..7   8  9 10 11 12 13 14 15 16..31
value:    07 <pad>  <NonNull<NlConsBox> ptr>      <unused>
          ^^        ^^^^^^^^^^^^^^^^^^^^^^^^
          tag = 7   8-byte pointer to NlConsBox
```

Phase 47 emit reads / writes through this layout:

- Read box pointer: `mov rax, qword ptr [rdi + 8]` (= 4 bytes encoded).
- Read car (= 32-byte Sexp): two 16-byte SIMD loads from `[box + 0]` + `[box + 16]`, store into caller slot.
- Read cdr: two 16-byte SIMD loads from `[box + 32]` + `[box + 48]`.
- Read raw box pointer for next-cell chain (= `cons-cdr-raw`): tag-check `[box + 32]` == `SEXP_TAG_CONS`, then `mov rax, [box + 32 + 8]` for the next NlConsBox pointer (or NULL if not Cons).

Box allocation requires `nl_alloc_consbox` Rust helper (Doc 101 §101.D) — Phase 47 emits `call nl_alloc_consbox` via the §100.A `extern-call` grammar.

## 7. `Sexp::Symbol(String)` / `Sexp::Str(String)` payload (Doc 101 §101.A)

`Sexp::Symbol` (tag 4) and `Sexp::Str` (tag 5) carry a Rust `String` value inline.  A `String` is a `Vec<u8>` header: 24 bytes consisting of `(ptr, capacity, length)`.  Per the standard Rust stdlib layout (stable since Rust 1.0 in practice; formally not a frozen contract but pinned in this repo by `rust-toolchain.toml`):

```
String layout (offsets within the String header, sizes in bytes):
    [0, 8)     ptr         (NonNull<u8>, 8 bytes)
    [8, 16)    capacity    (usize, 8 bytes)
    [16, 24)   length      (usize, BYTE count not char count)
    total:     24 bytes
```

Sexp slot for `Sexp::Symbol` / `Sexp::Str`:

```
offset:   0  1..7   8 9 10 11 12 13 14 15 16..23 24..31
value:    04 <pad>  <ptr to UTF-8 bytes>  <cap>   <len>
          ^^        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          tag = 4   24-byte String header (ptr/cap/len)
```

Phase 47 emit:

- Read byte length: `mov rax, qword ptr [rdi + 24]` (= length field at offset 24 of Sexp slot).
- Read byte pointer: `mov rax, qword ptr [rdi + 8]` (= ptr field at offset 8 of Sexp slot).
- Read individual byte at index N: load byte pointer + load `[ptr + N]` (= composed op).
- Short-string byte-for-byte equality (= ≤ 16 bytes): SIMD compare two 16-byte loads.
- Long-string equality: `extern-call memcmp` via Doc 100 §100.A grammar.

**Layout drift note**: the `String` layout is the stdlib internal representation.  The Rust stdlib does not formally guarantee this layout but has not changed it since 1.0; the repo pins the toolchain via `rust-toolchain.toml` and `sexp_abi_assert.rs` adds `const_assert!` for each String offset to fail fast on drift.

If the stdlib ever changes the layout, the fix is either to bump the toolchain back, or to migrate `Sexp::Symbol/Str` to hold a NeLisp-internal `NlString` type with explicit `#[repr(C)]` (deferred to a future `NlString` proposal).

## 8. Stability promise

The numbers in §2 (tag bytes) and §3 (offsets) are frozen at the
Doc 100 v2 ship date (2026-05-12).  Changing any of them:

- breaks every Phase 47-compiled `.o` linked into `bin/nelisp`,
- requires updating this doc + the Rust assertion + the elisp
  constant,
- requires a rebuild of every elisp `.o` listed in
  `scripts/compile-elisp-objects.el`.

The CI `make sexp-abi-check` step catches drift before it reaches a
release build.

## 9. Refs

- `build-tool/src/eval/sexp.rs:57` — enum source.
- `build-tool/src/eval/sexp.rs:217..229` — `SEXP_TAG_*` constants.
- `build-tool/src/eval/sexp.rs:270` — `SEXP_PAYLOAD_OFFSET`.
- `build-tool/src/eval/nlconsbox.rs:56-67` — `NlConsBox` struct layout (`car` / `cdr` / `refcount`).
- `build-tool/src/eval/sexp_abi_assert.rs` — Rust-side assertions.
- `lisp/nelisp-sexp-layout.el` — elisp constants.
- `docs/design/100-phase-47-sexp-int-abi-mvp.org` §1.2 / §2.2 — design rationale (Sexp::Int).
- `docs/design/101-phase-47-sexp-cons-symbol-str-abi.org` §1.2 / §2 — design rationale (Sexp::Cons / Symbol / Str).
