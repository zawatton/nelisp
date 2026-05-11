# Sexp ABI — frozen byte-layout contract

**Status**: SHIPPED (Doc 100 v2 §100.B, 2026-05-12)
**Scope**: byte-precise layout of `Sexp` and its `Sexp::Int` variant — sufficient for §100.C and the early Doc 101+ swaps.

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

## 6. Stability promise

The numbers in §2 (tag bytes) and §3 (offsets) are frozen at the
Doc 100 v2 ship date (2026-05-12).  Changing any of them:

- breaks every Phase 47-compiled `.o` linked into `bin/nelisp`,
- requires updating this doc + the Rust assertion + the elisp
  constant,
- requires a rebuild of every elisp `.o` listed in
  `scripts/compile-elisp-objects.el`.

The CI `make sexp-abi-check` step catches drift before it reaches a
release build.

## 7. Refs

- `build-tool/src/eval/sexp.rs:57` — enum source.
- `build-tool/src/eval/sexp.rs:217..229` — `SEXP_TAG_*` constants.
- `build-tool/src/eval/sexp.rs:270` — `SEXP_PAYLOAD_OFFSET`.
- `build-tool/src/eval/sexp_abi_assert.rs` — Rust-side assertions.
- `lisp/nelisp-sexp-layout.el` — elisp constants.
- `docs/design/100-phase-47-sexp-int-abi-mvp.org` §1.2 / §2.2 —
  design rationale.
