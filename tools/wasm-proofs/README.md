# wasm-proofs — Doc 164 P2 ground-truth fixtures

Hand-assembled `.wasm` modules (built byte-by-byte in `wasm-build.mjs`, the JS
mirror of `lisp/nelisp-asm-wasm.el`) that PROVE the binary encodings Doc 164 P2
depends on, by running `WebAssembly.validate` + `instantiate` + execute under
the pinned harness **Node v24.14.1**.  These are reference fixtures: the proven
byte sequences transfer 1:1 into the Elisp encoder/writer.  See
`docs/design/164-wasm-backend-p2-blueprint.org` for how each maps onto code.

Run everything (spawns the try_table proof with the required V8 flag):

    node tools/wasm-proofs/run-all.mjs

| file | proves | flag |
|------|--------|------|
| `proof-01-eh-trytable.mjs` | standardized EH: `try_table` 0x1f, `throw` 0x08, catch clause 0x00, catch label depth, Tag placement | needs `--experimental-wasm-exnref` |
| `proof-01b-tag-order.mjs` | Tag section (id 13) MUST sit after Memory(5)/before Global(6) | none |
| `proof-02-eh-legacy.mjs` | legacy EH: `try` 0x06 / `catch` 0x07 / `throw` 0x08; nested catch + tag-match + rethrow (proves `throw_ref`/exnref NOT needed) | none (flag-free) |
| `proof-03-unwind.mjs` | unwind-protect via `catch_all` 0x19 + `rethrow` 0x09; cleanup runs on normal AND throw paths | none |
| `proof-04-import-env.mjs` | Import section (id 2); imports occupy low func indices; `env.ext_add` provision | none |
| `proof-05-wasi-fdwrite.mjs` | `node:wasi` `fd_write` to stdout via fixed iovec; reactor `_initialize` path | none |
| `proof-06-wasi-clock.mjs` | `clock_time_get` returns nonzero ns timestamp | none |
| `proof-07-wasi-file.mjs` | hello-file: `path_open` + `fd_write` + `fd_close` in a preopen dir; file-fd rights = 0x46 | none |
| `proof-08-wasi-start-exit.mjs` | WASI COMMAND module (`_start`) + `proc_exit` exit code via `wasi.start` | none |
| `proof-09-wasi-random.mjs` | `random_get` fills a fixed buffer | none |

**Key V8 finding:** on Node v24.14.1 the *standardized* EH opcodes
(`try_table` 0x1f, `throw_ref` 0x0a, `exnref`) are gated behind
`--experimental-wasm-exnref`, but the *legacy* EH opcodes (`try` 0x06 /
`catch` 0x07 / `catch_all` 0x19 / `delegate` 0x18 / `rethrow` 0x09 / `throw`
0x08) and the Tag section run **flag-free**.  P2 therefore defaults to legacy
EH so `make wasm-eh-smoke` needs no special Node flag.
