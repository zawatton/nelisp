# newDTW wasm site bundle

This directory is the canonical static-host bundle for the Doc 164 P4b/P4c map-walk slice.

- `index.html` boots the browser demo.
- `dtw.js` is the minimal JS shim: it fetches `dtw.wasm`, forwards arrow-key state, drains wasm-emitted draw ops, and paints the canvas.
- `dtw.wasm` is compiled from `target/wasm-dtw/dtw-p4b.nlri`, which is produced by the P4b transpiler from the reduced newDTW Elisp slice.

Pipeline summary: Elisp corpus slice -> `tools/wasm-dtw-p4b/transpile-slice.mjs` -> `target/wasm-dtw/dtw-p4b.nlri` -> `compile-runtime-image --target wasm32-wasi` -> `site/dtw/dtw.wasm`.

Placeholder art note: the public bundle intentionally synthesizes placeholder map/player imagery in JS from the wasm manifest. It does not ship fan-game PNGs or the 103 MB source sprite set.

Rights note: if an owner later swaps in real game assets, that deployment decision and rights review happen outside this repo change.

Deployment options:

1. Serve `site/dtw/` from any static host as-is.
2. Publish the same files through GitHub Pages by either serving a repo `docs/` directory or publishing this directory as a Pages artifact.

No deployment workflow was added here.
