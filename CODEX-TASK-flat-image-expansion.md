# Task: the flat runtime image writes ~1.5 MB per top-level form

Repository: `/home/madblack-21/Cowork/Notes/dev/nelisp` (pure Elisp — no Rust).
Read `AGENTS.md` first, as that repo requires.

This is diagnosis-and-fix. The defect is quantified; the code path that causes
it is not identified, and finding it is the work.

---

## 1. What is wrong

`nelisp compile-runtime-image --flat-artifact-cache` produces an output
grotesquely larger than its input, and on the real bootstrap it dies:

```
Segmentation fault
rc=139  elapsed=6210s (103 min)
output: 1009194760 bytes (1.0 GB) from a 14 MB input artifact
```

The compile stage before it is not implicated: `compile-elisp-artifact` takes
4–5 seconds regardless of input size. Everything below is the flat-image stage.

## 2. Measured, on 2026-08-07/08 — do not re-derive

Inputs generated as N `defun`s each paired with a `defalias`, compiled with
`--rewrite-defalias-late`, then fed to the flat-image stage:

| N | source | artifact | flat image | image/source | artifact stage | image stage |
|---|---|---|---|---|---|---|
| 50 | 3.2 KB | 37.7 KB | **266 MB** | ~82000x | 4 s | 560 s |
| 200 | 13.8 KB | 151 KB | **706 MB** | ~51000x | 5 s | 588 s |
| 800 | 56 KB | 604 KB | — (900 s timeout) | — | 5 s | — |

From those two complete points:

* **image ≈ 120 MB + 3872 × artifact_bytes.** About 3900 bytes of image per
  byte of artifact, plus a ~120 MB fixed baseline.
* **~1.47 MB of image per top-level form.** N=50→200 adds 300 forms and
  440 MB: `(706384672 - 266158816) / 300 = 1467419`. The forms are two lines
  long. **The cost is not proportional to the form.**
* Extrapolating to the real 14.3 MB artifact gives roughly **55 GB**, which is
  why the 103-minute run died at 1.0 GB — it was nowhere near finished.

What the output contains:

* 266 MB gzips to 7.9 MB (**33.7x**, i.e. 3% of original).
* Of the first 2 MB, **50.7% is zero bytes**.
* Header: `LULF` followed by 8-byte-aligned fields whose high bytes are zero.

So: a sparse array of fixed-width slots holding small values, and something
per-form that is O(whole state) rather than O(form). A per-form snapshot of a
heap or a table would produce exactly this signature. That is a hypothesis,
not a finding — confirm or refute it from the code.

Note the shape also means the fix is probably **not** "compress the output".
Compression would hide a 55 GB logical size behind a smaller file; the amount
of work per form is the actual problem.

## 3. Where to start

`--flat-artifact-cache` is parsed at `lisp/nelisp-artifact.el:9623` and
dispatched at `:9682`. The runtime-side argv handling is in
`scripts/nelisp-standalone-build.el` around `:16927-16941`
(`nl_argv_has_flat_artifact_cache`), and that file carries several comments
about `--flat-artifact-cache` replay at `:13906`, `:13960`, `:14406` that are
worth reading for intent.

The `LULF` magic does not appear in any `.el` source, so the writer emits it
some other way — find it.

## 4. Reproduction — 9 minutes, not 103

Use the harness; do not hand-roll this and do not run the real bootstrap.

```sh
PROBE_SCALE_UNITS_PER_N=2 \
PROBE_SCALE_GEN='<generator writing @N@ defun+defalias pairs to @OUT@>' \
PROBE_SCALE_STAGE1="$NELISP compile-elisp-artifact --kind neln --input @IN@ --output @OUT@ --rewrite-defalias-late --native-policy opportunistic" \
PROBE_SCALE_STAGE2="$NELISP compile-runtime-image --flat-artifact-cache --runtime $NELISP --input @IN@ --output @OUT@" \
  bash /home/madblack-21/Cowork/Notes/.claude/skills/nelisp-probe-driver/probe-scale.sh 50,200
```

with `NELISP=/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/vendor/nelisp/target/nelisp`.

**Read the slope, not the exit codes.** A stage that times out still leaves a
partial output carrying the same slope. `N=50,200` is enough; 800 only times
out and adds nothing.

`probe-watch.sh` in the same directory samples an output file's growth so you
can see unbounded expansion in 60 seconds instead of waiting for a crash. Use
it rather than launching anything you intend to sit through.

## 5. Acceptance

**Per-form slope from 1,467,419 bytes to under a few thousand.**

Report the before and after slope from the same `probe-scale.sh` invocation.
Do not report "it completes now" — completion is not the criterion, and a run
that completes because the output merely got smaller by a constant factor has
not fixed this.

Also state what the remaining per-form cost consists of, so the number is
explained rather than merely reduced.

## 6. Constraints

- Do not add Rust; there is none here.
- Do not build the real bootstrap artifact or run `bin/nemacs`; both are
  35–100 minute operations and neither is needed to see this defect.
- Do not "fix" it by compressing output, by raising a limit, or by skipping
  the flat cache path.
- The validating build is `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/vendor/nelisp/`,
  a full source checkout of the same project carrying two uncommitted local
  patches (`lisp/nelisp-heap-image.el`, `lisp/nelisp-stdlib-hash.el`). Apply
  your change there too so it can be measured, and do not touch those two.
- Do not commit. Report diffs.

## 7. Report

The diff for both trees, the mechanism (what was being written per form and
why), before/after slopes from the harness, and anything that contradicts §2.
If you cannot find the cause, a complete inventory of what writes into the flat
image, each with a line on why it is or is not per-form O(state), is a good
outcome and is preferred over a speculative change.
