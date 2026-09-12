# NeLisp v1.3.0

Release implementation and qualification notes, 2026-09-12.

A minor release: it adds public API and required gates, and changes no
existing contract. v1.2.2 was prepared but never tagged or published; its
notes remain at [`release/v1.2.2/RELEASE.md`](../v1.2.2/RELEASE.md) and
everything they describe is included here.

## Changes since v1.2.1

### Native unit replacement, and the protocol that carries it

- `reload.plan` / `reload.apply` now carry general user native units under
  `unit: "native-unit"`, not only the fixed allocator/GC unit. A plan binds the
  live session, the target unit's expected generation, the source and staged
  artifact hashes, the compiler input set, the build-options digest and the
  running binary hash; applying revalidates every one of them and **discards
  the staged candidate** when any has moved, so a refused plan leaves nothing
  publishable behind. `session.clear` and TTL expiry revoke candidates with
  their plans.
- `nelisp-native-unit-code-info` answers which source and artifact produced the
  code a unit is running now. `:source-current` is recomputed against the file
  on disk at call time, and a bounded history of past publications gives a new
  process what it needs to reproduce the state. The record is written only
  after a successful CAS: a refused publish leaves the previous identity
  untouched.
- Candidate resources are reclaimed. A generation table that was never
  CAS-installed is unmapped on discard, TTL expiry, staging failure and refused
  publish. A superseded **published** generation is deliberately kept mapped —
  this runtime exposes no evidence that in-flight calls through a stable gate
  have returned — so `nelisp-native-unit-reclaim` reports it refused with that
  reason rather than guessing, and `nelisp-native-unit-resources` accounts for
  what is held.
- Rebuilds are bounded: a timeout (default 120s, validated 1..600), a cancel
  predicate, C-g, and a combined stdout/stderr byte budget (default 64 KiB).
  Every rejection path deletes its temporary artifact and says so. The
  timeout verdict comes from the clock and the worker's terminal record, never
  from re-sampling `process-live-p` — the racy form misreported an exhausted
  deadline on Windows.
- Doc 202 adds **declared replaceable call sites**: names listed in
  `tools/nelisp-replaceable-entries.txt` are built with the original renamed
  and the public name behind a control-word check, so their EXISTING direct
  callers reach a replacement without being recompiled.
  `nelisp-native-callsite-reachability` answers `:build-declared`,
  `:gate-only` or `:not-replaceable` per name, which is what stops this from
  being over-claimed.

**The boundary, stated plainly.** A name not declared at build time can never
have its existing `call rel32` sites redirected in that process: the
displacement is baked in at link time and the text is mapped read-execute.
What a live REPL can change without any rebuild is the BODY of a declared
entry, and any call site written from the start to go through
`nelisp-native-unit-address`.

### Memory

- **The arena's boundary reclaim zeroes what it rewinds.** `nl_boundary_reclaim`
  rewound a bump cursor back over a span the reader had already written to and
  handed it out again without zeroing it — unlike the free-list reuse path,
  which zero-fills. A constructor expecting fresh memory then inherited a stale
  `Sexp::Cons` with a garbage payload pointer and crashed on the first clone.
  Deterministic, reproducible from 152 bytes, and present in every standalone
  binary built that day: it was never a recent regression, nothing had driven a
  shape that hit it. It is what stopped the real-user-init audit at form 4.
- The 1-hour soak's RSS growth on some hosts is **transparent huge pages, not a
  leak**. Same binary, same host, one variable: at `[always]` it fails at
  1,135.7s (61,844 → 70,688 KiB); with THP disabled for that process alone
  (`tools/nelisp-nothp.c`) it passes 1,500.6s with peak RSS equal to start. The
  5,120 KiB ceiling is smaller than three 2 MiB huge pages.
- `tools/nelisp-native-reload-memory-audit.py` separates retained native
  mappings from GC heap by summing the resident bytes of exactly the unit's own
  `/proc/PID/smaps` regions. Over 10 republications: RSS +4,164 KiB, of which
  retained native mappings are +64 KiB resident — under 1.5%.

### Gates

Three new required Linux gates, each with a mutation row verified red:

- `native-unit-repl-smoke` — 88 checks in one live process: multi-export
  switching, a caller already in the old generation, the six-argument boundary,
  ten consecutive publications, state retention after a refused publish and
  after a failed build, and the reclamation accounting.
- `lisp-byte-compile` — `make compile` covers `src/` and `packages/*/src/`
  only, so `lisp/` (253 files: the AOT compiler, the artifact loader, the whole
  DEV protocol) had **never been byte-compiled by CI**. One process per file,
  because a single batch was measured to hide a real defect entirely.
- `nelisp-sexp-clone-bind-smoke` — pins the reclaim-zeroing mechanism, sweeping
  the variable-name length across the boundary that decides whether the
  reclaimed span is reused, with both the `'x` shorthand and `(quote x)`.

`tools/ai/nelisp-ai.sh` gained `-L scripts`, without which
`test/nelisp-native-runtime-dispatch-test.el` failed to load and ran **zero
cases** while appearing to pass.

## Qualification

| Check | Result |
|---|---|
| Branch CI, every lane | PASS — run 34626792979: ubuntu 30.1/29.4, windows 30.1/29.4, macOS 30.1/29.4, four gate-mutation shards, gates, tier perf/smokes/extras, final unscoped `verify` |
| Full ERT | 5,921 tests, 5,760 as expected, **0 unexpected**, 161 skipped |
| Check tier | 23/23 PASS (`NELISP_CHECK_SKIP=gate-mutation`; gate-mutation runs as CI's four shards) |
| Full real-init audit (930 forms) | PASS — 930/930 boundaries, `AUDIT_DONE 930`, exit 0, no signal, 36s, binary `b9cb89afd25c`. 322 `FORM_ERROR`s remain: 238 `void-function`, 54 `file-missing`, 29 `void-variable`, 1 `error` — unimplemented Emacs APIs and absent files, not memory faults |
| Native unit replacement, live | PASS: publish, a caller compiled once observing a later generation, CAS rejection of a stale candidate, arity refusal, preserved Lisp state |
| Version consistency | 9/9 sites say v1.3.0 |
| Linux 1-hour soak, release runner | PASS — 2,140 batches, 3,600.000s, RSS 74,744 → peak 76,252 KiB (+1,508 against a 5,120 ceiling). The THP behaviour described above is a property of hosts with `transparent_hugepage/enabled` at `[always]`; the release runner is not one |
| Semver release pipeline, `linux-x86_64` | PASS (run 34662576736) |
| Semver release pipeline, `linux-aarch64` | PASS (run 34662576736, 4m34s) |
| macOS ARM64 on real hardware | **Deferred to v1.3.1** — not a v1.3.0 release target; see [`MACOS-QUALIFICATION.md`](MACOS-QUALIFICATION.md) |

## Release qualification

The Linux blockers are qualified. Run 34662576736 exercised the semver release
pipeline against this tree by `workflow_dispatch` -- the same jobs a tag push
runs, without creating a tag or publishing anything -- so the result was known
before the tag existed rather than after.

macOS ARM64 is **deferred to v1.3.1** and is deliberately not a v1.3.0 release
target. It has never been qualified on real hardware; CI's macOS smoke lanes
(30.1 and 29.4) are green, and that is a materially weaker claim, since CI does
not build the macOS release artifact, does not verify its tarball, and does not
exercise the native replacement work this release is mostly about. The run
sheet for that qualification is [`MACOS-QUALIFICATION.md`](MACOS-QUALIFICATION.md);
its result ships as v1.3.1. Until then the macOS artifact must not be reported
as PASS.
