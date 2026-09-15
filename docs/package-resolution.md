# Dependency resolution contract

`tools/nelisp_packages.py` implements the data-only resolution and lock layer
for strategy sections 6.4 and 7. The project CLI connects it through `update`,
`fetch`, `add`, `remove`, and shared dependency loading for run/test/REPL/build.
An explicit HTTPS registry endpoint is supported. Official registry service,
ownership/publication APIs, signed index metadata, and package
publication remain to be implemented. The existing `packages/nelisp-pkg` library still
checks source-level feature dependencies; this resolver does not replace it.
This Python implementation remains part of the temporary host frontend, with
self-hosted execution still required by the strategy.

## Index input

Select an HTTPS index endpoint once for a development shell, or pass it for
one command. The URL below is a placeholder for an operator's registry:

```sh
export NELISP_REGISTRY="https://registry.example.invalid/index.json"
nelisp search json --json
nelisp add json
nelisp update
nelisp update --offline
# Per-command selection overrides the environment:
nelisp search --registry https://registry.example.invalid/index.json
```

`--index FILE` selects a local snapshot instead and takes precedence over the
environment. `--index` and `--registry` cannot be passed together. There is no
hardcoded public registry, implicit hostname discovery, credential store, or
service deployment in this milestone. `search` matches a package-name substring
case-insensitively and emits each matching package's latest non-yanked stable
version. `--json` emits an array of name/version objects; no matches returns
an empty array successfully. Search does not need a project or runtime.

The registry read API is one GET of the selected URL, returning the JSON index
schema below. Artifact URLs in that index must be HTTPS. Both use certificate-
validated transport with redirects rejected. This can consume an operator's
static HTTPS snapshot; it does not implement package ownership, registration,
publication, advisory, metrics, or documentation hosting APIs.

Online `search`, `add`, and `update` always retrieve the index, validate its
entire schema (including duplicate JSON fields), and atomically cache an envelope
containing the exact URL, raw index, and SHA-256. A rejected response preserves
the previous snapshot. `--offline` requires the cache for that exact URL and
verifies its digest and schema without network access; it does not silently
fall back to another registry. Index responses are limited to 16 MiB with a
30-second socket timeout. Local `--index` files use the same size/schema checks.

TLS authenticates the selected server, not the package authors. The envelope's
digest detects accidental corruption; someone able to replace both local data
and its digest can replace this cache. Signed indexes, publisher identity,
rollback protection, and expiry policy remain open. Offline snapshots may be
stale; online failures are reported rather than silently using old data.

For an ordinary `[dependencies]` table, the CLI can edit requirements:

```sh
nelisp add json --index ../registry-index.json
nelisp add json --version 2.1 --index ../registry-index.json --offline
nelisp remove json
```

Without `--version`, `add` selects the highest non-yanked stable version and
writes its full caret requirement. Explicit requirements use the grammar below.
Existing pins are preferred when they satisfy the resulting graph; `update`
is the separate command that requests fresh resolution for all requirements.
`remove` removes only direct requirements, prunes unreachable transitive
dependencies, and retains the exact versions and metadata of surviving pins.
It operates offline using only manifest/lock data, without requiring cached
artifacts, Emacs, or the runtime. Surviving missing artifacts still require
`fetch` before runtime execution.

The editor preserves unrelated bytes, comments, quoted ordinary keys, and
LF/CRLF line endings in regular tables with single-line string requirements.
It compares the entire parsed result with the intended semantic change before
writing. Inline/dotted dependency tables, escaped keys, and multiline dependency
values remain readable but are not automatically rewritten: unsupported edits
fail without modifying files. General TOML-preserving editing remains open.

Resolution, acquisition, and source validation finish before `add` writes either
project file. Both outputs are staged; a failed lock replacement restores the
original manifest, including its mode. If restoration itself fails, the error
names the retained original in a `.package-edit-*` recovery directory. These
two file replacements are not a crash-atomic transaction. Readers reject
mismatched requirements; concurrent writers and process-crash recovery still
need a stronger transaction protocol. Do not run simultaneous package edits.

Add requirements to the project's `nelisp.toml`:

```toml
[dependencies]
json = "2.1"
```

Then select a trusted local index snapshot explicitly:

```sh
nelisp update --index ../registry-index.json
nelisp fetch --offline
nelisp run
nelisp test
nelisp build
```

`update` resolves fresh versions, obtains verified artifacts, validates their
Lisp syntax as data in one host Emacs process, and atomically replaces
`nelisp.lock`. A failed operation preserves the previous lock; successful
artifact downloads can remain cached. `--offline` prevents network access.
`fetch` obtains exactly the existing lock's artifacts without resolving versions
or changing the lock. `fetch --offline` verifies cache contents and source syntax.
An empty dependency graph is valid and creates an empty lock on update.
The selected index is an explicit trust input, not a claim that an official
registry exists. Do not run simultaneous project updates;
input changes are checked before publication, but there is no interprocess
project transaction/lock yet.

Run/test/REPL/build always use the existing lock and cache without network
access or implicit resolution. Missing locks, stale root requirements, absent
cache entries, and changed artifact bytes fail before package code executes.
Packages are single UTF-8 Lisp source artifacts, named by SHA-256 in
`NELISP_CACHE` or `${XDG_CACHE_HOME:-~/.cache}/nelisp/sources-v1`.
The consumer verifies actual bytes on every use, then combines sources in
dependency order before the application. Packages can use `provide`/`require`
for already-loaded dependencies. Arbitrary dynamic imports and sidecar files
are not discovered or embedded automatically. `fmt` and syntax-only `check`
continue to operate on project files and never rewrite cached packages.

`tools/nelisp_package_store.py` fetches missing artifacts with certificate-
validated HTTPS, a 30-second socket timeout, and a 16 MiB artifact limit.
Redirects are rejected, including HTTPS-to-HTTP changes. A verified UTF-8,
NUL-free artifact is published by atomic file replacement with no executable
permissions. Corrupt cache entries fail explicitly, without a silent repair.
There is no archive extraction, host evaluation, or package build hook.
The socket timeout is not a whole-download deadline or resource sandbox.

The resolver takes a root requirements mapping and a registry index snapshot:

```json
{
  "schema_version": 1,
  "packages": {
    "json": [{
      "version": "2.1.0",
      "dependencies": {},
      "url": "https://example.invalid/json-2.1.0.nl",
      "sha256": "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef",
      "yanked": false
    }]
  }
}
```

The example URL and digest are placeholders, not an available package.
All fields are required. Unknown fields, duplicate releases, invalid names,
noncanonical versions, malformed hashes, and non-HTTPS artifact URLs fail
validation. HTTPS authenticates the selected server; an artifact hash supplied
by an untrusted index establishes no publisher authenticity.
No network request or package code execution occurs in this module.

Stable versions have three decimal components without leading zeroes.
Requirements accept a bare or `^`-prefixed one-, two-, or three-component
version, or `=MAJOR.MINOR.PATCH`. Bare and caret forms share these bounds:

| Requirement | Accepted versions |
|---|---|
| `1`, `1.3`, `1.3.2` | From the specified version through, but excluding, `2.0.0` |
| `0` | `>=0.0.0`, `<1.0.0` |
| `0.2`, `0.2.3` | From the specified version through, but excluding, `0.3.0` |
| `0.0` | `>=0.0.0`, `<0.1.0` |
| `0.0.3` | Exactly `0.0.3` |
| `=1.3.2` | Exactly `1.3.2` |

Prereleases, build metadata, wildcards, disjunctions, comparator ranges,
multiple simultaneous versions, and optional/platform dependencies are not
supported. Unsupported syntax is rejected rather than partially interpreted.

## Selection and integrity

`resolve(requirements, index, previous=None)` selects one version per name.
Names are considered alphabetically and versions in descending numeric order;
backtracking resolves conflicts between direct and transitive requirements.
Cycles are rejected because this initial source-loading model requires an
acyclic dependency order. A cyclic candidate may fall back to another version.
The search is exhaustive for this finite model, but has no large-registry
performance guarantee or resource budget yet.

A previous lock prefers pinned versions that still meet all constraints.
Yanked releases cannot enter a new resolution; an existing matching pin may
retain one. Missing releases cannot be reconstructed from the lock alone.
Changed URL, digest, or dependency metadata for an indexed pinned version is
an integrity error. Calling without a previous lock requests fresh resolution;
it does not imply that fetched bytes have been checked or published.

`lock_bytes` writes deterministic TOML, with exact versions, URLs, SHA-256
digests, direct root requirements, and transitive edges. Packages are ordered
before their dependents. Index ordering and timestamps do not affect bytes.
`read_lock` checks the whole reachable graph, every constraint, duplicates,
cycles, and unsupported schema fields. Both functions validate graph content;
they do not perform filesystem writes. The CLI must publish a new lock only
after successful resolution and artifact validation, preserving the previous
lock on failure. `verify_artifact` checks actual artifact bytes against a
locked hash before a consumer may parse or execute them.

## Fast validation

```sh
python3 test/nelisp-package-resolver-test.py
python3 test/nelisp-package-store-test.py  # local HTTPS; needs OpenSSL CLI
python3 test/nelisp-project-manifest-test.py  # no runtime or host Emacs
python3 test/nelisp-registry-test.py  # local HTTPS index and CLI integration
python3 test/nelisp-project-packages-test.py  # runtime and native ELF integration
```

This suite needs only Python 3.11. It tests backtracking, deterministic bytes,
exact and zero-major constraints, pin retention, updates, yanked releases,
metadata tampering, cyclic/missing/unreachable dependencies, failed resolution
without input mutation, and actual byte integrity. Run it during resolver
edits; runtime rebuilding cannot answer these data-contract questions.
The store suite uses an ephemeral local HTTPS server and CA to exercise
certificate validation, actual downloads, cache reuse, corruption, rejected
redirects, and bounded response size. Project integration covers transitive
`require` ordering in run/test/REPL, isolated native deployment after cache
removal, stale requirements, source syntax failures, and lock preservation.
Manifest tests also inject second-file and rollback failures, and package
integration verifies pin preservation, automatic pruning, and removal with no
cache or host executable available.
