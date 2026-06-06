# Doc 142 Native EXEC Findings

Date: 2026-06-04

## Scope

This note records the runtime contract surfaced by AOT object-mode
native defuns that lower ordinary Elisp builtins through the hidden AOT
boundary in `lisp/nelisp-aot-compiler.el`.

The host proof added in this branch deliberately uses the "host-link a
generated harness" option from Doc 142 rather than doing in-process ELF
loading yet. The standalone in-process loader remains the next step and
should consume the pre-digested metadata now embedded in `.neln` files.

## Runtime Symbols

For the current ordinary user-defun builtin lane, the emitted `.text`
relocations reference this exact symbol set:

- `nl_alloc_symbol`
  Used by `sexp-write-symbol-lit` to materialize the builtin name into
  `name_slot` before each dispatcher call.
- `nelisp_aot_builtin_call1`
  Used for direct one-argument builtin delegation such as `1+`, `1-`,
  `car`, `cdr`, `symbol-name`, etc.
- `nelisp_aot_builtin_calln`
  Used for direct vararg builtin delegation such as `cons`, `eq`,
  `list`, `vector`, `plist-get`, etc.

This branch's host proof validates the `nl_alloc_symbol` +
`nelisp_aot_builtin_call1` path end-to-end. `nelisp_aot_builtin_calln`
is serialized into the artifact metadata and documented here, but the
host proof intentionally rejects objects that still require it.

## Hidden Boundary Slots

Ordinary object-mode user defuns synthesize this hidden slot block after
their source parameters:

1. `out`
2. `mirror`
3. `frames`
4. `scratch`
5. `name_slot`
6. `callback-slot-0`
7. `callback-slot-1`
8. `callback-slot-2`
9. `callback-slot-3`
10. `callback-slot-4`
11. `callback-slot-5`
12. `callback-slot-6`
13. `callback-slot-7`
14. `callback-slot-8`
15. `callback-slot-9`
16. `callback-slot-10`
17. `callback-slot-11`

Observed invariants:

- The slots live in the defun's spill frame immediately after the source
  params.
- `:rt-slot-count` in the serialized defun metadata already includes
  this hidden block plus any extra runtime `let-rt` slots.
- The emitted code reads these slots as raw pointers, not by computing
  their addresses itself.

## Slot Population Rules

Before entering a builtin-calling defun body, the loader/trampoline must
populate the slots like this:

- `out`
  Pointer to a caller-owned 32-byte `Sexp` slot. Builtin helpers write
  their result here and compiled code reuses that pointer as the current
  boxed value.
- `mirror`
  Pointer/handle for the runtime mirror environment passed through to
  the AOT builtin bridges.
- `frames`
  Pointer/handle for the runtime lexical-frame state passed through to
  the AOT builtin bridges.
- `scratch`
  Pointer to a caller-owned 32-byte `Sexp` scratch slot. Builtin bridges
  and nested helper lowering reuse it for temporary boxing/materialized
  values.
- `name_slot`
  Pointer to a caller-owned 32-byte `Sexp` slot. `nl_alloc_symbol`
  overwrites this slot with the current builtin symbol before each
  dispatch call.
- `callback-slot-N`
  Pointer to a caller-owned 32-byte `Sexp` slot. These are only needed
  when `nelisp_aot_builtin_calln` lowers function-designator,
  keyword-name, or closure arguments that must survive across the call.
  For builtin1-only defuns they can be preinitialized to `nil` slots and
  otherwise left untouched.

## Host Proof Choice

This branch chose the host-link proof, not the in-process loader:

- The proof function is `nelisp-artifact-native-exec-general`.
- It links the embedded ET_REL object against:
  - a generated assembly trampoline that synthesizes the expected spill
    frame and jumps into the compiled body after its prologue, and
  - a generated C shim runtime that defines the required host symbols.
- The proof is currently scoped to x86_64 Linux and the
  `nl_alloc_symbol` + `nelisp_aot_builtin_call1` subset.

That choice is faithful enough to validate the hidden boundary-slot ABI
without doing runtime ELF parsing in Emacs, and it keeps the future
standalone loader work isolated to metadata consumption plus reloc patching.

## Standalone Loader Plan

`scripts/nelisp-standalone-build.el` is reserved and was not edited
here. The in-process loader that should land there can use this plan:

1. Read the `.neln` artifact and extract the pre-digested native fields:
   `:text-base64`, `:relocs`, `:extern-symbols`, and `:defuns`.
2. Decode `.text` bytes into writable memory.
3. Allocate or reuse one executable page (or page set) large enough for
   the copied `.text`.
4. Build a symbol-address table for every external name in
   `:extern-symbols`.
   For the current builtin lane this table must at least expose
   `nl_alloc_symbol`, `nelisp_aot_builtin_call1`, and eventually
   `nelisp_aot_builtin_calln`.
5. Reuse `nelisp-link-apply-reloc-inplace` from
   `lisp/nelisp-static-linker.el` to patch each serialized reloc entry
   against the copied `.text` base address.
6. Build the per-call boundary block:
   allocate stable `Sexp` slots for `out`, `scratch`, `name_slot`, and
   the callback slots; provide the live `mirror` and `frames` handles.
7. For each exported defun call, synthesize the expected spill frame
   from the serialized defun metadata:
   `:arity`, `:rt-slot-count`, and `:body-offset` are sufficient for the
   current x86_64 SysV gp subset.
8. Jump to `text_base + defun_offset + body_offset`, not to the symbol
   start. The synthetic trampoline owns the prologue work.
9. After return, decode the boxed value from `out`.
10. Extend the same scheme to `nelisp_aot_builtin_calln` by materializing
    callback slots and matching the exact stack-argument ABI used by the
    compiler's `extern-call` lowering.

## What Still Remains

- Prove the `nelisp_aot_builtin_calln` lane end-to-end in the host proof.
- Lift the same metadata-driven trampoline/reloc flow into the standalone
  in-process loader.
- Decide whether the standalone runtime will keep a trampoline-per-call
  design or synthesize one reusable entry stub per exported defun.
