# NeLisp v1.0.0

**Release date**: 2026-04-27
**Tag**: `v1.0.0`
**Tier matrix**: Linux x86_64 = blocker (CI gate); macOS arm64 / Linux arm64 = `v1.0 限定` best-effort 95%+ (Doc 32 v2 §11).

> ⚠ Non-native English author note: phrasing edited with LLM
> assistance, technical claims verified against repo state on tag
> commit. File issues with anything unclear.

## Highlights

- **Stage D v2.0 — bundled-Emacs tarball**. `bin/anvil` resolves a
  stripped Emacs binary from `$ANVIL_HOME/emacs/bin/emacs` first, so
  hosts without `apt install emacs` can run NeLisp. Existing dev
  checkouts and earlier Stage D tarballs fall through to the system
  PATH unchanged. (Doc 32 v2 §3.3 LOCKED.)
- **Phase 8.0 — Rust-side minimal Elisp interpreter + MCP server**. A
  ~421 KB `anvil-runtime` Rust binary implements the reader, evaluator,
  MCP stdio JSON-RPC server, and `anvil-host-*` tool registry.
  `bin/anvil mcp serve --no-emacs` starts an MCP server with **zero
  Emacs process spawn**. (Doc 44 LOCKED 2026-04-27.)
- **`v1.0` 完遂条件 #4 ACHIEVED** — true standalone binary distribution
  available, removing the Emacs-install gate that previously blocked
  NeLisp public announcement
  (`feedback_nelisp_announcement_standalone_gate.md` cleared).

## Phase ship roll-up

| Phase | Topic                                          | Status   |
|-------|------------------------------------------------|----------|
| 7.0   | Rust syscall surface trimmed to ~819 LOC       | SHIPPED  |
| 7.1   | NeLisp native compiler scaffold                | SHIPPED  |
| 7.2   | Allocator (3-tier ratio bench harness)         | SHIPPED  |
| 7.3   | GC inner (bench harness, native fast-path TBD) | SHIPPED  |
| 7.4   | Coding (UTF-8 / SJIS / EUC-JP tier-A bench)    | SHIPPED  |
| 7.5   | Integration + standalone E2E                   | SHIPPED  |
| 7.5.3 | `stage-d-v2.0` bundled-Emacs tarball           | SHIPPED  |
| 8.0.1 | Rust Elisp reader (47 tests, 1456 LOC)         | SHIPPED  |
| 8.0.2 | Rust Elisp evaluator (24 special forms, ~60 builtins, 3417 LOC) | SHIPPED  |
| 8.0.3 | NeLisp self-host bridge (559 LOC)              | SHIPPED  |
| 8.0.4 | Rust MCP stdio JSON-RPC server (598 LOC)       | SHIPPED  |
| 8.0.5 | `bin/anvil --no-emacs` Rust binary dispatch    | SHIPPED  |
| 8.x   | Reader parity (backquote / char-lit / `#'`)    | SHIPPED  |
| 8.x   | Macro/define extension (require / pcase / cl-defun) | SHIPPED |
| 8.0.5 | anvil-host MCP wire (4 tools end-to-end)       | SHIPPED  |

## Feature highlights

### `bin/anvil mcp serve --no-emacs` — MCP server with zero Emacs spawn

```
$ bin/anvil mcp serve --no-emacs
{"jsonrpc":"2.0","id":1,"method":"initialize", ...}
{"jsonrpc":"2.0","id":2,"method":"tools/list", ...}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"anvil-host-info","arguments":{}}}
```

The `--no-emacs` flag dispatches to `target/release/anvil-runtime`
when present, otherwise falls back to `emacs --batch` (3-year safety
window per Doc 44 §3.6 LOCKED). `--strict-no-emacs` refuses the
fallback.

### Stage D v2.0 — install on a host without Emacs

```bash
# Build the bundled tarball (~25 MB compressed)
make stage-d-v2-tarball

# Install (extracts to ~/.local/share/anvil-stage-d-v2.0/)
./release/stage-d-v2.0/install.sh --from $(pwd)/dist

# Verify bundled Emacs is the active binary
$HOME/.local/share/anvil-stage-d-v2.0/bin/anvil version
# anvil stage-d-v0.2-pre
#   emacs            GNU Emacs 30.1 (bundled)
```

The `(bundled)` marker confirms the self-contained path. `(system)`
means the launcher fell through to host PATH (= dev checkout).

## Soak gate result (2026-04-27)

| Tier             | Duration | RSS growth | Result               |
|------------------|----------|------------|----------------------|
| blocker (CI)     | 1h       | < 5 MB     | PASS (linux-x86_64)  |
| post-ship audit  | 24h      | < 10 MB    | scheduled (weekly)   |

## Test status

- `cargo test --lib`: **236 pass / 0 failed**
- ERT suite: wired via `make test`
- `tools/soak-test.sh` 1h soak: PASS on linux-x86_64

## Install instructions

### Path A — bundled tarball (no Emacs install required)

```bash
# Pre-v1.0 (build locally)
make stage-d-v2-tarball
./release/stage-d-v2.0/install.sh --from $(pwd)/dist

# Post-v1.0 (curl from GitHub Release; pending upload of v1.0.0 artifact)
curl -fsSL https://github.com/zawatton/nelisp/releases/download/v1.0.0/install.sh | bash
```

Add to `PATH`:

```bash
export PATH="$HOME/.local/share/anvil-stage-d-v2.0/bin:$PATH"
```

### Path B — Rust binary only (smallest deployment)

```bash
git clone https://github.com/zawatton/nelisp.git
cd nelisp
cargo build --release --manifest-path nelisp-runtime/Cargo.toml
./bin/anvil mcp serve --no-emacs
```

The `anvil-runtime` binary (target/release) is ~421 KB.

### Path C — clone + dev checkout

```bash
git clone https://github.com/zawatton/nelisp.git
cd nelisp
make
make test
```

This requires a host Emacs install (Phase 8.0 evaluator does not yet
cover the full surface required for self-bootstrap; default
`--no-emacs` will fall back to `emacs --batch` if needed).

## Verifying a downloaded artifact

```bash
sha256sum --check stage-d-v2.0-linux-x86_64.tar.gz.sha256
```

GPG signing lands in v2.1+ (Doc 32 v2 §2.5 + §8); v2.0 ships an
ad-hoc signature placeholder only.

## Known limitations (carried over from `RELEASE_NOTES.md`)

- *macOS notarization*: out of v2.0 scope. Ad-hoc signature only.
- *Windows native `--no-emacs`*: out of v2.0 scope. Stage A path
  (Doc 18) still handles Windows via msys2 mingw64.
- *ARM 32-bit*: out of v2.0 scope.
- *Self-update*: no `bin/anvil --self-update` in v2.0.
- *Phase 8.0 deferred features*: GC bridge, bignum, full backquote
  semantics, full `save-excursion`. Most existing Elisp packages
  will not run unmodified through `--no-emacs` yet.

## Doc references

- `docs/design/27-phase7-c-runtime-self-impl.org` — Phase 7+ syscall
  surface (LOCKED 2026-04-25 v2).
- `docs/design/28-phase7.1-native-compiler.org` — NeLisp native
  compiler (LOCKED 2026-04-25 v2).
- `docs/design/32-phase7.5-integration.org` — Phase 7.5 integration
  + standalone E2E (LOCKED 2026-04-25 v2, includes tier matrix +
  soak gate).
- `docs/design/44-phase8.0-rust-elisp-interpreter.org` — Phase 8.0
  Rust Elisp interpreter (LOCKED 2026-04-27).
- `docs/design/18-stage-d-standalone.org` — Stage D launcher
  (LOCKED).
- `RELEASE_NOTES.md` — release-by-release Stage D v2.0 changelog.
- `README-stage-d-v2.0.org` — stage-d-v2.0 quickstart.

## Acknowledgments

NeLisp builds on the methodology established by:

- SBCL (Rhodes 2008) — Lisp implemented in itself, on a small non-Lisp
  runtime.
- Emacs native-comp (Corallo 2020) — AOT compilation reference for
  Phase 7.1 design.
- PyPy / RPython — self-hosting interpreter precedent.

Sister project `anvil.el` (https://github.com/zawatton/anvil.el)
provides the MCP tool surface that `anvil-host-*` tools port to the
NeLisp Rust binary.

## License

GPL-3.0+ (matches Emacs).

---

🤖 Release notes drafted with assistance from Claude.
