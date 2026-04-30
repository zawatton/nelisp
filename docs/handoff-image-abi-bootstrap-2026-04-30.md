# Handoff: Rust image ABI walking skeleton for bootstrap.el

Date: 2026-04-30
Branch observed: `feat/elisp-lsp-stdio-primitives`
Repo root: `/home/madblack-21/Notes/dev/nelisp`

## User Goal

Demonstrate that `dev/nelisp` Rust core can prove Doc 47 §3.1 phase 4
(`bootstrap.el on image`) sub-decomposition 6a-6e as a walking skeleton,
with the image format ABI operating on real Elisp values.

## Implemented Scope

Added a small Rust image-format ABI layer in `nelisp-runtime`:

- `nelisp-runtime/src/image.rs`
  - New image module.
  - Header: `NELIMG\0\x01`
  - ABI version: `1`
  - Encodes/decodes real evaluator `Sexp` values:
    - `nil`
    - `t`
    - integer
    - float
    - symbol
    - string
    - cons
    - vector
  - Public functions:
    - `compile_elisp_to_image(source: &str) -> Result<Vec<u8>, ImageError>`
    - `encode_image(forms: &[Sexp]) -> Result<Vec<u8>, ImageError>`
    - `decode_image(bytes: &[u8]) -> Result<Vec<Sexp>, ImageError>`
    - `eval_image(bytes: &[u8]) -> Result<Sexp, ImageError>`
    - `eval_forms(forms: &[Sexp]) -> Result<Sexp, ImageError>`
  - The image loader returns the same `Sexp` value universe used by the
    Rust evaluator, so this is not a second object model.

- `nelisp-runtime/src/lib.rs`
  - Added `pub mod image;`

- `nelisp-runtime/src/main.rs`
  - Added CLI commands:
    - `compile-image <bootstrap.el> <image-file>`
    - `eval-image <image-file>`
  - `eval-image` prints the loaded/evaluated value via `reader::fmt_sexp`.

- `nelisp-runtime/tests/fixtures/bootstrap.el`
  - Added a small bootstrap fixture:
    - defines `bootstrap-add`
    - defines `bootstrap-main`
    - exercises cons/vector mutation/access
    - evaluates to `(2 "image" 42)`

## 6a-6e Mapping

The unit tests in `nelisp-runtime/src/image.rs` map to the requested
sub-decomposition:

- 6a: `phase4_6a_header_and_version_are_checked`
  - Verifies image magic and ABI version rejection.

- 6b: `phase4_6b_atoms_round_trip_through_image_abi`
  - Round-trips atom values through binary ABI.

- 6c: `phase4_6c_composite_values_reload_as_mutable_elisp_values`
  - Round-trips list/dotted pair/vector and proves loaded cons is a mutable
    real `Sexp::Cons` by mutating `car`.

- 6d: `phase4_6d_bootstrap_el_compiles_to_image_and_evaluates`
  - Compiles bootstrap-like Elisp source to image and evaluates the image.
  - Expected value: `(2 "image" 42)`.

- 6e: `phase4_6e_image_forms_are_real_elisp_values_before_eval`
  - Decodes image forms before evaluation and confirms they are printable /
    evaluable real Elisp values.

## Commands Run

From `/home/madblack-21/Notes/dev/nelisp/nelisp-runtime`:

```sh
cargo test image::tests
```

Result: passed.

```sh
cargo run --quiet --bin nelisp-runtime -- compile-image tests/fixtures/bootstrap.el /tmp/nelisp-bootstrap.img
cargo run --quiet --bin nelisp-runtime -- eval-image /tmp/nelisp-bootstrap.img
```

Result:

```text
(2 "image" 42)
```

```sh
cargo test
```

Result: passed.

Observed full runtime result:

```text
244 lib tests passed
6 src/bin/nelisp.rs tests passed
7 filenotify tests passed
19 syscall tests passed
doc-tests passed
```

Image sanity check:

```sh
wc -c /tmp/nelisp-bootstrap.img
od -An -tx1 -N16 /tmp/nelisp-bootstrap.img
```

Result:

```text
422 /tmp/nelisp-bootstrap.img
4e 45 4c 49 4d 47 00 01 01 00 00 00 03 00 00 00
```

Interpretation:

- `4e 45 4c 49 4d 47 00 01` = `NELIMG\0\x01`
- `01 00 00 00` = ABI version 1
- `03 00 00 00` = 3 top-level forms in `bootstrap.el`

## Formatting Note

`cargo fmt` was attempted but unavailable in this environment:

```text
error: 'cargo-fmt' is not installed for the toolchain 'stable-x86_64-unknown-linux-gnu'.
help: run `rustup component add rustfmt` to install it
```

The edited Rust was manually kept close to rustfmt style, but Claude should
run `cargo fmt` after installing/enabling rustfmt if desired.

## Dirty Worktree Notes

Before this task, the branch already had unrelated modifications:

- `README.org`
- `anvil-runtime/Cargo.lock`
- `anvil-runtime/src/bin/anvil-runtime.rs`
- `nelisp-runtime/Cargo.lock`
- `nelisp-runtime/Cargo.toml`
- untracked `.claude/`
- untracked `.worktrees/`

This task intentionally did not revert or normalize those existing changes.

Files added/modified for this task:

- `nelisp-runtime/src/image.rs`
- `nelisp-runtime/src/lib.rs`
- `nelisp-runtime/src/main.rs`
- `nelisp-runtime/tests/fixtures/bootstrap.el`
- `docs/handoff-image-abi-bootstrap-2026-04-30.md`

## Suggested Next Steps

1. Run `cargo fmt` once rustfmt is installed.
2. Decide whether this walking-skeleton ABI should stay as a Doc 47 phase-4
   skeleton or be renamed/rehomed if Doc 47 has a stricter canonical path.
3. If Doc 47 requires persisted global environment state in the image, extend
   the ABI from "top-level forms image" to "forms plus selected value/function
   cells". Current implementation proves real `Sexp` value encoding/loading,
   not full saved-world dumping.
