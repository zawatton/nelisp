# CLAUDE.md

This project is shifting to a library-first architecture.  Claude Code work
in this repository must optimize for reusable pure-Elisp Emacs-core
libraries before application-specific shortcuts.

Read first:

- `README.org`
- `docs/design/17-library-first-reuse-and-gui-reintegration.org`
- `docs/design/18-library-package-ownership-inventory.org`
- `docs/design/19-buffer-display-text-property-boundary.org`
- `docs/design/20-files-runtime-boundary.org`
- `docs/design/21-app-gui-semantic-definition-inventory.org`
- `docs/design/24-public-api-inventory.org`
- `docs/design/25-library-consumer-contract.org`
- `docs/design/26-library-residual-api-audit.org`
- `docs/design/27-package-extraction-readiness.org`
- `docs/design/12-development-gates.org`

## Operating Principle

`nelisp-emacs` should become a reusable library stack.  `nemacs`, TUI, and
GUI code are consumers of that stack.

When implementing behavior, put it in the lowest reusable owner:

- foundation primitives,
- buffer/edit substrate,
- editor core,
- IO/runtime adapter,
- display model/backend,
- app or GUI glue only when the behavior is truly adapter-specific.

Do not implement Emacs command semantics directly in `nemacs-main.el`,
GUI bridge code, or TUI shortcut code if a shared runtime module can own
the behavior.

## API Rules

- Public reusable APIs should be prefixed.
- Unprefixed Emacs-compatible functions belong in explicit shim/builtins
  modules.
- Prefixed helpers defined in `emacs-*-builtins.el` are shim helper
  surface by default, not stable external API, unless the facade stable API
  manifest explicitly promotes them.
- Prefixed guard helpers such as `*-check-live` are internal validation
  surface unless an external consumer use case justifies facade stable API
  promotion.
- `--` helpers are private.  Do not spread dependencies on them across
  modules without promoting the API intentionally.
- Use `make nemacs-public-api-inventory` to review package-group public
  API before treating a symbol as reusable/stable.
- Keep public API inventory `UNOWNED` rows at zero under
  `make nemacs-library-gate`; new reusable files need Doc 18 ownership.
- Keep file-level ownership coverage `unowned` and `stale` rows at zero
  with `make nemacs-ownership-coverage`.
- Reusable `*-features` manifests must be duplicate-free, resolvable on
  `load-path`, and must not include app/bootstrap or concrete TUI/GUI
  features.
- Keep `nelisp-emacs-library-package-manifest` aligned with facade and
  group loader manifests when package membership changes.
- Run `make nemacs-library-contract` when facade query APIs or
  `nelisp-emacs-library-contract-version` change; contract symbol failures
  must stay zero.
- When changing `nelisp-emacs-library-stable-package-api` or its query
  helpers, run `make nemacs-library-contract`,
  `make nemacs-library-api-promotion-queue`, and
  `make nemacs-library-package-verify`; stable package API symbols must have
  test/doc evidence and classify as `stable-contract`.
- Run `make nemacs-library-package-manifest` when package membership
  changes; the generated TSV/Org artifacts are the reviewable package view.
- Run `make nemacs-library-package-deps` when package dependencies change;
  `app-or-frontend` dependency rows must remain zero under the library gate,
  and so must `unmanifested-reusable`, `lazy-unmanifested-reusable`, and
  unknown `external-or-host` rows.  `build/nemacs-library-package-migration-queue.*`
  is the review queue for eager hidden dependencies.  Declare intended lazy
  companions through the facade manifest; classify host/vendor dependencies
  in the deps tool; do not promote lazy rows to eager package membership
  without a deliberate load-time decision.
- Run `make nemacs-library-package-descriptors` when package membership or
  dependencies change; the generated descriptor drafts are the review input
  for package archive and repository extraction work.
- Run `make nemacs-library-package-guide` when descriptor or external
  consumer package docs change; the generated guide is the reviewable
  package loader/dependency/lazy-feature view for consumers.
- Run `make nemacs-library-package-api` when descriptor membership or
  public API surface changes; the generated artifact is the package-scoped
  candidate API view.
- Run `make nemacs-library-package-catalog` when descriptor, guide, or
  package API surface changes; the generated catalog is the consumer-facing
  package loader/dependency/API summary.
- Run `make nemacs-library-api-promotion-queue` when package API surface,
  package docs, or package tests change; the generated queue is the review
  input for deciding which public-prefixed and compat-global symbols can be
  promoted into consumer-facing package docs.
- Run `make nemacs-library-package-layout` when physical package extraction
  shape changes; the generated TSV/Org artifacts are the reviewable
  `packages/` move plan and must not be hand-maintained.
- Run `make nemacs-library-package-scaffold` to generate the experimental
  `packages/` tree from the layout plan without moving or deleting `src/`.
  Treat copied scaffold files as generated artifacts; edit `src/` and
  regenerate instead of hand-editing package copies.
- Run `make nemacs-library-app-scaffold` when GUI bridge app/frontend glue
  staged outside the reusable package count changes.  This target generates
  `packages/nelisp-emacs-app-gui/` as an extraction staging artifact, not
  as one of the reusable library packages.
- Run `make verify-production-runtime-path` when `nemacs-main`,
  bootstrap/runtime-image inputs, or production GUI/runtime adapter requires
  change.  This gate must prove production modules are present in the
  bootstrap bundle and mapped through either the reusable package scaffold or
  the app scaffold.
- Run `make nemacs-runtime-image-input-inventory` when runtime-image preload
  scripts, bake recipes, or lazy runtime feature inputs change.  Unknown and
  missing rows must stay zero; temporary `src/` rows are allowed only when
  they are explicit compatibility debt.
- Use `make nemacs-library-package-load-path` or
  `scripts/nemacs-library-package-load-path.sh` for package scaffold
  load-path arguments.  Do not open-code `find packages ... -L` fragments
  in new smoke targets.  The helper intentionally excludes
  `packages/nelisp-emacs-app-*` staging trees; pass app scaffold roots
  explicitly when a test needs them.
- Run `make nemacs-library-package-path-smoke` after scaffold generation to
  prove package loaders can require from `packages/*/lisp` and
  `packages/*/lazy` load paths.
  Keep this loader smoke ERT-free via
  `scripts/nemacs-library-package-smoke.el` so the scaffolded
  `cl-lib.el` is exercised instead of being shadowed by host ERT setup.
- Run `make nemacs-library-package-consumer-smoke` after scaffold
  generation to prove the top-level `nelisp-emacs` facade can load from
  package paths without `src/`.
- Run `make nemacs-library-package-lazy-smoke` after scaffold generation
  to prove package-owned lazy companion features can load from package
  paths.
- Run `make nemacs-library-package-frontend-smoke` after scaffold
  generation to prove selected frontend glue, currently GTK view/menu
  smoke, can run with reusable libraries and package-owned lazy helpers
  loaded from package paths, plus staged app/frontend glue loaded from
  `packages/nelisp-emacs-app-gui/` instead of `src/`.
- Run `make nemacs-library-package-gui-bridge-smoke` after scaffold
  generation to prove selected GUI bridge host/source-shape checks can run
  with reusable libraries loaded from package paths and staged app/frontend
  bridge glue resolved through the app scaffold where available.
- Run `make nemacs-library-package-gui-bridge-standalone-smoke` when a
  standalone reader is available to prove selected GUI bridge runtime
  checks can run with package-path host setup and package scaffold image
  source resolution.  Mapped reusable sources should come from
  `packages/`; mapped app/frontend bridge glue should come from
  `packages/nelisp-emacs-app-gui/`; only unmapped inputs may fall back to
  `src/`.  The current bridge image input set must have zero `src/`
  fallback under package-backed smoke.  This target is intentionally smaller than
  `test-nemacs-gui-bridge-gate` and must keep avoiding the slow tail.
- ERT-backed package-path smoke targets must load host ERT through
  `NEMACS_LIBRARY_PACKAGE_HOST_ERT_PRELUDE` before package load paths; do
  not let host ERT discover the scaffolded `cl-lib.el`.
- Run `make nemacs-library-package-verify` when descriptor, guide, package
  API, catalog, promotion queue, or layout shape changes;
  descriptor/guide/API/catalog/promotion/layout verification failures must
  stay zero.
- Prefer facade query helpers (`nelisp-emacs-library-package-names`,
  `nelisp-emacs-library-package`,
  `nelisp-emacs-library-package-features`, and
  `nelisp-emacs-library-package-lazy-features`) over open-coded manifest
  traversal in consumers.
- Treat `nelisp-emacs-library-contract-version` as the external consumer
  compatibility marker; bump it for incompatible facade contract changes.
- Keep `make nemacs-library-consumer-smoke` green when changing facade
  membership; consumers must be able to require `nelisp-emacs` from
  `emacs -Q -L src` without loading app/bootstrap or frontends.
- Keep `make nemacs-library-package-smoke` green when changing group
  loaders; package groups must remain directly requireable without the
  top-level facade or application bootstrap.
- Host Emacs should remain safe to load.  Avoid global overrides except in
  guarded compatibility layers.

## GUI Reintegration Rule

If `nelisp-gui` is brought back under this repository, treat it as a
consumer test for the libraries.  GUI owns transport and rendering.  It
does not own buffer state, command dispatch, keymap lookup, minibuffer
semantics, undo, file command behavior, or window layout semantics.

Missing GUI behavior should usually be added to the shared runtime first,
then invoked from the GUI adapter.

## Workflow

Use the anvil definition index and call graph before large edits:

- locate definitions through `defs-index` / `defs-search`,
- inspect callers/references before changing hotspots,
- use architecture/clusters to understand subsystem boundaries.

Default verification starts with focused ERTs and `make test-fast`.
Library-surface work should also run `make nemacs-library-gate`; for
facade-only changes, use `make nemacs-library-contract` and
`make nemacs-library-consumer-smoke` as the focused preflights.  For group loader changes, run
`make nemacs-library-package-smoke`.  Review
`make nemacs-public-api-inventory` and
`make nemacs-library-package-manifest`; for dependency shape changes, also
run `make nemacs-library-package-deps` and review its migration queue.  For
package extraction shape changes, run
`make nemacs-library-package-descriptors` and
`make nemacs-library-package-guide` and
`make nemacs-library-package-api` and
`make nemacs-library-package-catalog` and
`make nemacs-library-api-promotion-queue` and
`make nemacs-library-package-layout` and
`make nemacs-library-package-scaffold` and
`make nemacs-library-package-load-path` and
`make nemacs-library-package-path-smoke` and
`make nemacs-library-package-consumer-smoke` and
`make nemacs-library-package-lazy-smoke` and
`make nemacs-library-package-frontend-smoke` and
`make nemacs-library-package-gui-bridge-smoke` and
`make nemacs-library-package-gui-bridge-standalone-smoke` and
`make nemacs-library-package-verify`.  Use `make verify-production-runtime-path`
for bootstrap/runtime-image changes and the Doc 12 gates for TUI/GUI bridge work.

## Completion Standard

A change is not complete merely because the app path works.  It should
also preserve or improve library reuse:

- app glue stays thin,
- shared behavior has focused tests,
- fallback paths are documented or inventoried,
- external consumers would not need private internals.
