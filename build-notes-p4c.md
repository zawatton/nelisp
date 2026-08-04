# P4c build notes: site bundle, smoke, and deploy handoff

This note closes the Doc 164 P4c work: the proof fixture stays under `tools/wasm-proofs/p4-www/`, while the canonical static-host bundle now lives under `site/dtw/`.

## 1. Build the canonical site bundle

`make wasm-dtw-site` runs the full pipeline in order:

```powershell
make wasm-dtw-site
```

That does three things deterministically:

1. transpiles the reduced P4b slice into `target/wasm-dtw/dtw-p4b.nlri`;
2. compiles that runtime image to `target/wasm-dtw/dtw.wasm` with one `emacs --batch -Q` invocation;
3. copies `index.html`, `dtw.js`, `README.md`, and the compiled `dtw.wasm` into `site/dtw/`.

Expected bundle files:

- `site/dtw/index.html`
- `site/dtw/dtw.js`
- `site/dtw/dtw.wasm`
- `site/dtw/README.md`

## 2. Site smoke

The site smoke executes the real `site/dtw/dtw.js` against a fake DOM/canvas in Node, loads `site/dtw/dtw.wasm`, and checks the P4 exit conditions:

- init manifest is `["map","player"]`;
- the first rendered frame is visibly non-blank;
- an `ArrowRight` key press changes the rendered canvas.

Run it directly:

```powershell
node tools/wasm-dtw-p4b/site-smoke.mjs site/dtw
```

Or through make:

```powershell
make wasm-dtw-site-smoke
```

## 3. Proof fixture split

`tools/wasm-proofs/p4-www/` is now fixture-only.

- `node tools/wasm-proofs/p4-run-all.mjs`
- `make wasm-dtw-skeleton-smoke`

still rebuild and validate the proof skeleton bundle there, but they do **not** touch `site/dtw/`.

## 4. Browser serve

Any static server is sufficient because `dtw.js` uses `fetch(...).arrayBuffer()` instead of `instantiateStreaming()`:

```powershell
cd site/dtw
python -m http.server 8000
```

Then open `http://127.0.0.1:8000/` and verify the map-walk renders and responds to arrow keys.

## 5. Deployment options (docs only)

Option A: serve `site/dtw/` from any static host by uploading the four bundle files as-is.

Option B: publish the same directory via GitHub Pages from a repository. Common patterns are:

- copy the bundle under a repo `docs/` directory and configure Pages to serve `/docs`;
- or use a repository-owned Actions workflow that publishes `site/dtw/` as the Pages artifact.

No workflow was added here. The owner still chooses the repo, URL, and whether the Pages site is public or private/unlisted.
