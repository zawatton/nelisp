# NeLisp v1.0.0

**Release date**: 2026-08-26
**Tag**: `v1.0.0`
**Previous stable**: `v0.6.0` (2026-06-26)

> Non-native English author note: phrasing was edited with LLM assistance.
> The technical claims are mine and each figure here was measured on the
> release tree — see *How these numbers were obtained* at the end.

## Why this is 1.0

Two things, and they are the whole reason for the version number.

**The runtime collects garbage by default.** Before this release, NeLisp's
precise-root collector existed and was correct, but it only ran if you flipped
a debug switch. A default build grew without bound. On a 200,000-iteration
allocating loop, peak RSS falls from **669,936 KiB to 339,920 KiB (-49.3%)**,
and RSS is now flat from 500k to 1M iterations (352,624 → 352,620 KiB) while
**256 MiB is returned to the OS**. It is also *faster* — 5,204 ms collected
versus 8,125 ms uncollected — because a small heap keeps locality and avoids
further growth `mmap`s. Memory and speed were not a trade-off here.

**Ordinary allocating Elisp runs on real OS threads.** "Parallelism" used to
mean cooperative scheduling on one thread. It now means `clone(2)` workers
evaluating unrestricted allocating Elisp, with per-thread precise roots, a
park barrier, atomic cache publication, and a catchable refusal of worker
writes to the shared globals mirror.

## What is in it

### Garbage collection (Doc 152, all stages)

The collector is on by default, with mid-form collection at `while` backedges.
Enabling it surfaced a latent defect worth naming: the mark phase asserted that
char-tables and bool-vectors "do not occur in the reader graph" and so marked
such a box without walking its children. They do occur — the reader's own
char-table smoke keeps them in the global mirror — so those children were
reachable-but-unwalked. Nothing had collected mid-form by default, so nothing
had ever noticed. Precise child walkers for both now ship.

Doc 152's planned Stage 6, a write barrier, was **retired rather than
implemented**: the park barrier below stops every mutator before marking, so
the tri-colour invariant holds by construction instead of by instrumenting
every store.

### True multicore (Doc 199, Tiers 1–3b)

- **Tier 1** — `nl-clj-future`, `nl-clj-pcalls`, `nl-clj-pmap`, cooperative.
- **Tier 2** — GC-free `clone(2)` workers with a publish-happens-after-join
  protocol.
- **Tier 3a** — bounded allocating tasks sharing the bump arena, safe because
  the cursor bump is already lock-free CAS and no mark/sweep runs mid-section.
- **Tier 3b** — unrestricted allocating Elisp: a 64-entry per-thread root
  registry, the park barrier, claim-fill-publish for the macro and
  function-value caches, and `nelisp-worker-mirror-mutation` raised (catchably)
  when a worker tries to write the shared globals mirror.

The gate asserts the barrier actually engaged rather than skipping an empty
registry: `PARK-DIAG parked=3 current=0 missed=0 collections=1` — a real
collection with three live workers parked.

### Standard Emacs names, not a NeLisp dialect

An early NeLisp exposed its own vocabulary, so code written for it was code
written for NeLisp. Docs 184, 188 and 194 wired standard Emacs names over
those models. All of these are `fboundp` in a default `target/nelisp` with no
`--load`: `current-buffer`, `set-buffer`, `generate-new-buffer`, `insert`,
`point`, `goto-char`, `buffer-string`, `buffer-substring`, `erase-buffer`,
`make-process`, `process-send-string`, `process-filter`, `set-process-filter`,
`accept-process-output`, `process-status`, `process-live-p`,
`make-network-process`, `open-network-stream`, `run-at-time`, `cancel-timer`,
`timerp`, `add-hook`, `run-hooks`. The `nelisp-`-prefixed functions remain as
the layer underneath; they are the implementation, not the interface.

### Opt-in language extensions

Every one follows the same rule, and the rule is what makes them usable:
**loading one changes nothing about plain Elisp semantics.** You pay only
where you annotate.

- **`nl-safe`, `nl-static`, `nl-check`, `nl-contract`** — Rust-style
  discipline for the *runtime implementation layer* (raw pointers,
  `syscall-direct`, arenas, mmap), which is where the danger actually lives;
  ordinary Lisp is already memory-safe. Borrow cells enforcing sharing XOR
  mutation, fat pointers with generation-based use-after-free detection, an
  `nl-unsafe` boundary under a CI ratchet, macroexpansion-time totality and
  type annotations, and Racket-style boundary contracts with blame.
- **`nl-ns`** — Emacs Lisp has one obarray, so a second `defun` of a name
  silently wins. `nl-ns` rewrites nothing and adds nothing at run time; it
  reads files and reports crossings, under a CI ratchet.
- **`nl-clj`** — persistent vector, hash-map and hash-set, `atom`, and an
  eager *and* lazy seq API. The lazy seq uses `nl-safe`'s borrow cell for
  realize-once semantics, so the libraries compose rather than coexist.
- Also: `nl-num` (numeric tower), `nl-hygiene` (hygienic macros), `nl-prelude`,
  `nl-condition`, `nl-parens` — 39 packages in total.

### Networking, buffers, bignums

Doc 184 processes and event loop; Doc 194 `make-network-process`,
`open-network-stream`, `/etc/hosts` lookup, a DNS-over-TCP resolver,
nonblocking sockets with `:nowait`/`:server`, and IPv6 — all in the default
binary. Doc 188 buffers, Doc 190 bignums, and full backquote all ship; the
v0.6.0 README still listed all three as deferred.

## Verification

| Gate | Result |
|------|--------|
| `nelisp-ai.sh test` | 5,466 tests / 5,309 expected / **0 unexpected** / 157 skipped |
| `make emacs-parity` | **19,961 checks, 0 findings** — differential against real stock Emacs |
| `nelisp-ai.sh verify` | VERDICT PASS (66 gates) |
| `nelisp-ai.sh check` | PASS (22 gates) |
| `make bench-aot-tco` | 0.997x against a 0.95 floor |
| `make standalone-midform-gc-bounded` | at 1M: `FIRED=5 RECLAIMED=4 RECLAIMED_BYTES=268435456 RELEASE_FAILURES=0` |
| CI | six lanes (Linux/macOS/Windows × Emacs 29.4/30.1) plus a fast Linux `gates` job |

`emacs-parity` is the number that matters most: it diffs behaviour against a
real Emacs rather than against NeLisp's own idea of correct.

## Scale

`lisp/` 234 files / 88,447 lines · `src/` 42 / 57,859 · `scripts/` 20 / 39,799
· `packages/` 212 / 55,338. The standalone binary is 7.66 MB (1.77 MB
gzipped). **Zero `.rs` files remain** — the evaluator, reader, compiler,
allocator, GC, object writers, native emitters and syscall surface are Elisp.

## Known limits

Stated plainly, and each one checked against the binary rather than recalled:

- No windows or frames (`selected-window`, `make-frame` absent by design —
  display is Layer 3, `../nelisp-gui`).
- Buffers yes; `make-marker`, `overlay-start` and `save-excursion` no.
- A Tier 3b worker's arena share returns at process exit, not task end.
- Workers cannot write the shared globals mirror — such a write signals rather
  than serialising. The read path is unrestricted.
- Sockets are Linux-only. Windows (Doc 194 P6) is blocked on PE import-table
  emission, which does not exist yet; other targets get a catchable
  `nelisp-unsupported-primitive`.
- Mid-form collection safepoints are `while` backedges only.
- Linux arm64 is best-effort; Linux x86_64, macOS arm64 and Windows x86_64 are
  the gated native standalone targets.

## How these numbers were obtained

RSS figures come from `wait4(2)`'s own `ru_maxrss` for each child process
(not `RUSAGE_CHILDREN`, which would carry a maximum over from an earlier
case). Timings are best-of-three on one machine, comparing the same binary
with the collector armed and disarmed, so the delta is not a build
difference. Test, gate and parity counts are the gates' own `GATE-COUNT`
lines. Line counts are `wc -l`. Binary size is `stat -c %s`; note that a
local build measures larger than CI's for the same commit, so size should be
judged from CI's own report or from a BASE-vs-HEAD delta.
