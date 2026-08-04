# P4b Build Notes

This slice stays on the public-safe asset plan from Doc 164 P4: `tools/wasm-proofs/p4-www/dtw.js` synthesizes placeholder `map` and `player` buffers from the `LOAD_IMAGE` manifest and does not ship fan-game PNGs.

## 1. Deterministic transpile

Run from the `nelisp` repo root:

```powershell
node tools/wasm-dtw-p4b/transpile-slice.mjs
```

Expected artifacts:

```text
target/wasm-dtw/dtw-p4b.nlri
target/wasm-dtw/dtw-p4b-report.json
```

Expected report highlights:

```json
{
  "sliceFunctions": ["init", "step", "gr-update-input", "gr-draw-map", "gr-draw-player"],
  "runtime": { "slotCapacity": 1270, "ringRecordCapacity": 128 },
  "croppedFrom": { "x": 13, "y": 0, "startX": 23, "startY": 9 }
}
```

Equivalent make target:

```powershell
make wasm-dtw-transpile
```

## 2. Compile to wasm

Single `emacs -Q` invocation, explicit `-L`, via the runtime-image CLI path:

```powershell
$env:HOME=(Get-Location).Path
$env:XDG_CONFIG_HOME=(Get-Location).Path
emacs --batch -Q -L lisp -L src `
  --eval '(setq load-prefer-newer t)' `
  --eval "(progn (require 'nelisp-artifact) (compile-runtime-image '(\"compile-runtime-image\" \"--kind\" \"neln\" \"--target\" \"wasm32-wasi\" \"--input\" \"target/wasm-dtw/dtw-p4b.nlri\" \"--output\" \"target/wasm-dtw/dtw.wasm\")))"
```

Equivalent make target:

```powershell
make wasm-dtw-compile
```

Expected artifact:

```text
target/wasm-dtw/dtw.wasm
```

## 3. Headless smoke

Run after compile:

```powershell
node tools/wasm-dtw-p4b/smoke.mjs target/wasm-dtw/dtw.wasm
```

Equivalent make target:

```powershell
make wasm-dtw-smoke
```

Expected smoke output:

```text
manifest=["map","player"]
frame0_ops=<non-zero>
player_x=10->11
map0_dxdy=<present>
player0_dxdy=160,150
player1_dxdy=160,150
result=OK
```

Smoke meaning:

- `manifest=["map","player"]`: `init()` emitted the real slice manifest.
- `frame0_ops=<non-zero>`: the frame was non-blank.
- `player_x=10->11`: one rising-edge `ArrowRight` press moved slot `66` by one tile.
- `result=OK`: manifest, non-blank frame, text draw, map draw, and movement all passed.

## 3.1 Logic regression probe (`or`)

If wasm compile succeeds but V8 rejects or misexecutes short-circuit logic, run this
minimal source through the same wasm compile path:

```lisp
(seq
  (defun f (a b)
    (if (or (= a 1) (= b 2)) 7 9))
  (list (f 1 0) (f 0 2) (f 0 0)))
```

Expected result:

```text
(7 7 9)
```

This specifically exercises the wasm `or` emitter path where the first truthy value
must still be preserved as the short-circuit result while the surrounding `if`
receives a real wasm `i32` condition.

## 4. Browser eyeball

First verify the proof skeleton is still green:

```powershell
node tools/wasm-proofs/p4-run-all.mjs
```

Or:

```powershell
make wasm-dtw-skeleton-smoke
```

For the canonical static-host bundle, build `site/dtw/` and serve that directory instead:

```powershell
make wasm-dtw-site
cd site/dtw
python -m http.server 8000
```

Expected browser behavior:

- a non-blank 340x340 frame appears immediately;
- placeholder `map` and `player` buffers are created from the wasm manifest;
- one `ArrowRight` press advances the player one tile;
- holding the key does not repeat until release and press again.

The older `tools/wasm-proofs/p4-www/` directory remains a proof fixture only. Its `p4-06` rebuild is intentionally isolated from the deployable `site/dtw/` bundle.

## 5. Scope notes

- The transpiler reads only the read-only game corpus files:
  - `newDTW-nelisp/nelisp_runtime/game-runner.el`
  - `newDTW-nelisp/nelisp_runtime/gamedata-state-dungeon.el`
- This P4b slice uses real cropped map data from slots `71` and `87`, plus real seeded slots around the player start state.
- `now_ms` is intentionally not used in this slice because the current wasm extern lane still blocks f64-return imports.
