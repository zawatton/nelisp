# NeLisp v1.0.1

**Release date**: 2026-08-26
**Tag**: `v1.0.1`
**Previous**: `v1.0.0` (same day)

> Non-native English author note: phrasing edited with LLM assistance; the
> technical claims are mine and every figure was measured on this tree.

A same-day follow-up to v1.0.0. Three defects that v1.0.0's own change --
turning the mid-form collector on by default -- brought into the open, plus
the in-process `.neln` loader that the collector change was blocking.

Then the release verification found more than it was meant to. Fixing the
one workflow that had been red for twelve consecutive runs let the macOS
lane reach its own tests for the first time, and it stopped at the first
one. Fixing that reached the second. Nine in all -- including every
float-returning operation dying with SIGILL on arm64, and `boundp`
answering `t` from an uninitialised register on x86_64. None is a
regression; each had simply never been executed. Two of the nine were
gates that had been reporting success while examining nothing.

CI itself went from 108 minutes to 16 along the way, without testing any
less.

## The loader now runs any artifact, read at run time

v1.0.0 shipped a loader, but as a demo: the artifact's bytes and its externs'
addresses were baked into the binary at build time, so it could run exactly
one function. Everything that had to vary was fixed before the reader was
built.

`lisp/nelisp-native-load.el` drives the same mechanism from data read at run
time — mmap a page, write the text into it, patch relocations, build stubs for
the externs, enter through `ptr-call`. No linker, no `cc`, no subprocess.

## Defects fixed

**`pcase` refused `(cl-type TYPE)`.** `(pcase 5 ((cl-type integer) :int))`
signalled `Unknown cl-type pattern` where Emacs answers `:int`. Notably
`emacs-parity` did not catch this: it compares 19,995 behaviours and this was
not among them, so the divergence sat behind a green gate.

**`base64-decode-string` returned characters, not bytes.** Decoding `"yMnK"`
gave `(195 136 195 137 195 138)` where every other base64 gives
`(200 201 202)` — each byte ≥ 128 came back as its two-byte UTF-8 form, so the
pair did not round-trip for any binary input. The encoder had already been
fixed; only half the pair had been. Found while verifying a compiled
artifact's own recorded sha256 against the bytes it was computed from: the
file was intact and only this runtime's decode disagreed.

**The loader's digest buffer was not a GC root.** `alloc-bytes` returns a raw
pointer the collector knows nothing about, and the digest helper filled it one
byte at a time across 1152 iterations of a `while` — whose backedge is a
mid-form safepoint. A collection partway through reclaimed the buffer, and the
same intact artifact digested to `27c76247`, `a88c7b03`, `5a66d5e0` on
successive runs where its real digest is `42d6a0bf`. Every-run-different is
what reading reissued storage looks like. This could not happen before
v1.0.0, because the collector did not run by default.

**The native harness's slot registry was a fixed 64.** Its own comment said
"about 46 usable after the boundary and callback slots" and "a loop that boxes
a value per iteration outruns it in tens of iterations" — which is every tight
arithmetic loop. Grown on demand instead.


### macOS arm64: the lane had never run to its end

v1.0.0 shipped with `stage-d-v3.0 standalone parity` red, and the run
before this one was the first time it reached its own tests. It stopped
at the `boxed` parity case on a `set -e`, so nothing after
`macos-selfhost-test.sh` had ever executed on macOS: not the standalone
eval smoke, not cache identity, not the reader, not the tarball, not the
installer. Fixing `boxed` made the next failure reachable, and so on.
Nine in all. None is a regression — each reproduces on a pristine v1.0.1
worktree, or sits on a path that had never run.

1. **`boxed` exited 139.** The smoke's own NlCell stubs were written
   before Doc 147 Phase 1, which shrank NlCell 40B→16B and moved the
   value to an 8-byte tagged WORD at box+0. The vector and record stubs
   were updated; the cell trio was not, so `nl_alloc_cell` copied a
   32-byte Sexp into a 16-byte box while `nl_cell_get_value` read box+0
   as a WORD — dereferencing a tag byte. lldb: `EXC_BAD_ACCESS` at
   address `0x2`.

2. **The eval and cache-identity smokes expected a filename the builder
   has never written.** Only cross-built targets get an arch suffix, and
   aarch64 is the host arch there.

3. **The macOS reader could not be built at all.**
   `(:extern-call-too-many-gp-args nl_eval_source_all 10)` — AArch64
   passes eight arguments in registers and the extern-call emitter had
   no stack path, though the direct-call emitter had carried one all
   along. Verified by disassembly, and by a test that fails with the old
   emitter and passes with the new.

4. **`bf_boundp` passed two arguments to a three-parameter function.** On
   x86_64 the missing third register usually held a non-null address
   whose tag was not Symbol, so `boundp` answered `t` and the defect sat
   silent. On aarch64 that register arrives zero and the tag load faults:
   SIGSEGV on the first `boundp` of every `--eval`, `--load` or bare-file
   run.

5. **The REPL's blank-Enter idle pump still called the pre-Doc-180
   shape**, leaving two outgoing stack slots uninitialised.

6. **Six files still pinned v0.6.0.** The Windows set was self-consistent
   at v0.6.0 while the POSIX side had moved to v1.0.1, so CI shipped
   v0.6.0-labelled tarballs for a v1.0.1 release and nothing complained.

7. **`default-directory` was nil on macOS**, so `(expand-file-name "a")`
   answered `"/a"`. Darwin has no getcwd syscall; it is
   `fcntl(fd, F_GETPATH, buf)` on a descriptor for `"."`.

8. **Every operation returning a float died with SIGILL on arm64.**
   `nl_sexp_write_float` and `nl_os_float_time` were hand-written x86_64
   machine code linked into every target, and the arm64 image executed
   `mov qword [rdi], 3`. Both now have aarch64 bodies generated through
   the assembler rather than hand-encoded, compared word-for-word against
   Xcode's `as`.

9. **`gate-selfcheck` reported `emacs-parity` as a gate that examined
   nothing — on macOS only.** BSD `wc` right-pads its count, so
   `checked=   19900` matched no parser. A gate that had just compared
   19,900 behaviours against stock Emacs was recorded as one that found
   no inputs: precisely the failure gate-selfcheck exists to catch,
   produced by gate-selfcheck.

The lane now ends with `=== Cross-platform verify PASS ===` — 79 checks,
zero failures — and the gate battery is 27/27.

### CI ran for 108 minutes and now runs for 16

Not by testing less: the CPU time is unchanged at ~104 minutes. The
Linux lane carried the entire gate suite alone and serially — 39 of its
46 minutes was work no other lane does at all, while on the steps every
OS shares Linux is *faster* than Windows. That work is now split across
parallel jobs (`gate-mutation` in four shards, the subsystem tiers in
three), and `verify` runs last over the union of every job's gate
reports, so "did every required gate actually run" survives the split.
Three other things fell out of measuring it: the reader smokes were
rebuilding the same binary 41 times (1269s → 209s), `gate-mutation` now
narrows to the rows a change can affect on branches, and a wall-clock
benchmark that sampled once now takes the best of three.

## Also

- `stage-d-v3.0 standalone parity` had failed 12 runs consecutively without
  testing anything: it demanded a `main` branch from a sibling repository
  that is on `master`, so it died before cloning. It now probes for a usable
  default branch — which is how the nine macOS defects above became
  reachable at all. The Windows half of that step wanted the same fix and
  did not get it until this release.
- The release version now has one source, `./VERSION`, and a
  `version-consistency` gate holds the copies that cannot read it — the
  standalone installers, which users fetch on their own with curl — equal to
  it. Nine sites, with a `gate-mutation` row proving the gate goes red when
  one of them is left behind.
- `macos-x86_64` could no longer install Emacs at all: Nixpkgs 26.11 dropped
  x86_64-darwin. That job builds from Homebrew now.
- Doc 200 records a defect this release does NOT fix; see below.

## Known issues

**Unibyte strings are not a distinct representation.** `(unibyte-string 227
129 130)` and `"あ"` are `equal` on this runtime; a real Emacs answers `nil`.
The visible symptom is `append`, which answers `(521 640 0)` for
`(unibyte-string 200 201 202)` where Emacs answers `(200 201 202)`. Fixing
`append` alone is the wrong move — the same root reaches `concat`,
`substring`, `aref`, `equal`, `sxhash` and the printer, and 59 code lines
across ~24 files test the string tag today. `docs/design/200-unibyte-string-
representation.org` records the measurement, quotes Emacs 31.1's own NEWS on
the invariant it wants to keep, and states why this was not folded into a
release that shipped the same day.

## Verification

`tools/ai/preflight.sh --full`, all 16 gates clear on the release commit.
`gate-mutation` 45/45 rows caught their injected defect. `emacs-parity`
19,994 checks, 0 findings.

CI: 15 jobs green on Linux/macOS/Windows x Emacs 29.4/30.1, plus the fast
`gates` job, the four `gate-mutation` shards, the three subsystem tiers,
and `verify` — which reads the union of every job's gate reports and
answered `VERDICT: PASS (68 gates)`. 16.2 minutes wall clock.

macOS arm64: `=== Cross-platform verify PASS ===`, 79 checks, zero
failures; gate battery 27/27, `make test` included. Verified on hardware,
not in CI — the nine defects above are the reason that distinction
matters.
