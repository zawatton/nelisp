# Task: the flat image dumps dead arena — write only live data

Repository: `/home/madblack-21/Cowork/Notes/dev/nelisp` (pure Elisp — no Rust).
Read `AGENTS.md` first, as that repo requires.

This is fix #1 of two. It is expected to be sufficient on its own for the
acceptance number below. Fix #2 (a quadratic replay cost in the native
evaluator) is a separate task and is **out of scope here** — do not attempt it.

---

## 1. The defect

`compile-runtime-image --flat-artifact-cache` writes the arena's **bump
high-water mark**, not its live data. Free blocks are copied verbatim.

Measured in-process, before the output file is even opened:

```
live 34,605,000   free 89,952,136
```

**72% of the image is already-dead arena.** That is what makes the output ~50%
zero bytes and 34x-gzippable, and it is most of why a 14 MB input extrapolates
to tens of GB and segfaults at 1.0 GB after 103 minutes.

## 2. Why the cursor never drops (measured — do not re-derive)

All in `scripts/nelisp-standalone-build.el`:

- the mid-form collector `nl_gc_collect_from_recorded_roots` (`:3382`) always
  sweeps and **never compacts**, by design (`:3300-3305`, `:3376-3381`);
- the form-boundary collector is gated on `depth == 0`
  (`nl_gc_form_boundary_due_p`, `:3406`), and the whole CLI command is a single
  top-level form, so it never fires;
- `(garbage-collect)` only sets a request flag (`nl_gc_request`, `:3404`);
- compaction is off by default (`:17532`).

So sweeping returns memory to free lists but never to the cursor.

## 3. What writes the image

`bf_arena_dump_image_stream` (`:5038`), reached from
`lisp/nelisp-artifact.el:9521`. It emits four regions:

| region | source | size | scaling |
|---|---|---|---|
| header | `hdr` | 64 B | constant |
| relocation table | one entry per live pointer field | `tlen × 8` | ~89 entries = **712 B per form** |
| chunk regions | `nl_mc_write_chunks` (`:4488`), `[data_start, cursor)` per chunk | `nl_mc_total()` | **the problem** |
| intern region | `nl_fa_write_all fd ib isz` | `isz` ≥ 16 MiB | constant |

`LULF` in the header is not a string: it is the u64 `1179407692` written
little-endian at `:5087` (the comment there says `"FLAT"`; the constant is
byte-reversed relative to it).

## 4. Two ways in — pick one and say why

1. **Compact immediately before the dump.** Nothing in Elisp can lower the
   bump cursor, so this means enabling/《invoking》 the compacting path that is
   currently off by default, at the one point where it is safe: after the
   module has finished replaying and before `bf_arena_dump_image_stream` runs.
2. **Skip free blocks while writing** and renumber offsets. The relocation
   table must stay consistent with the new layout — that is the whole risk of
   this approach, and a table that disagrees with the payload produces an image
   that loads and then misbehaves, which is worse than a big image.

Whichever you choose, state the reasoning, and be explicit about what keeps
the relocation entries correct.

## 5. Acceptance — two conditions, both required

**(a) The slope.** Currently **0.47–0.70 MB per top-level form** (it varies
~±22% run to run, so quote the runs you took). Target: **under a few thousand
bytes per form**, which is the relocation-table rate of ~712 B/form. Measure
with:

```sh
NELISP=/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/vendor/nelisp/target/nelisp
PROBE_SCALE_UNITS_PER_N=2 \
PROBE_SCALE_GEN='<generator writing @N@ defun+defalias pairs to @OUT@>' \
PROBE_SCALE_STAGE1="$NELISP compile-elisp-artifact --kind neln --input @IN@ --output @OUT@ --rewrite-defalias-late --native-policy opportunistic" \
PROBE_SCALE_STAGE2="$NELISP compile-runtime-image --flat-artifact-cache --runtime $NELISP --input @IN@ --output @OUT@" \
  bash /home/madblack-21/Cowork/Notes/.claude/skills/nelisp-probe-driver/probe-scale.sh 50,200
```

Report before and after from the same invocation. Note the CLI enforces
`.neln` / `.flat.nlri` output suffixes, so the stages need wrapping; the
harness follows symlinks correctly as of `e5fc8993f`, but check you are sizing
the real file.

**(b) The image must still work.** A smaller image that does not load is not a
fix, and this is the failure mode the approach in §4.2 invites. Establish a
cheap validity check — cheaper than the 35–100 minute `bin/nemacs` cold path —
and state what it is and what it proves. If you cannot find one, say so
explicitly rather than reporting (a) alone as success.

Expected magnitude if this lands: baseline image 139 MB → roughly the live
35 MB, with the per-form term falling to the relocation-table rate.

## 6. Constraints

- Do not add Rust; there is none here.
- Do not attempt the O(N²) replay cost. Separate task.
- Do not "fix" this by compressing the output, raising a limit, or skipping the
  flat cache path.
- Do not build the real bootstrap artifact and do not run `bin/nemacs`.
- The validating build is
  `/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/vendor/nelisp/`, a full
  source checkout carrying two uncommitted local patches
  (`lisp/nelisp-heap-image.el`, `lisp/nelisp-stdlib-hash.el`). Apply your change
  there too so it can be measured, and do not touch those two files.
  `make standalone-reader` in that tree rebuilds the binary in well under a
  minute.
- Do not commit. Report diffs for both trees.

## 7. Report

The diff for both trees, which approach from §4 and why, how relocation
entries are kept correct, before/after slopes from §5(a), the validity check
from §5(b) and its result, and anything that contradicts §1–§3.

A well-scoped negative result is acceptable: if compaction cannot be made safe
at that point, or the relocation table cannot be renumbered without more
context than exists, say what blocks it. Do not ship a change whose
correctness you cannot argue.
