# Task: a symbol's name slice reads past its terminator under memory contention

Repository: `/home/madblack-21/Cowork/Notes/dev/nelisp` (branch `fix/gc-retention-edge-magit`)

This is a **diagnosis-and-fix** task. The reproduction is exact and the
byte-level symptom is known; the location in the source is not, and finding it
is part of the work.

---

## 1. Symptom

Loading a large Elisp file under the standalone runtime intermittently makes a
function that the file *does* define come back `void-function`:

```
nelisp: uncaught error: void-function: (nelisp-emacs-magit-bridge--ensure-symbol-runtime<NUL>nel)
```

`od -c` on that log line:

```
... -  r  u  n  t  i  m  e  \0  n  e  l  )
```

The symbol's name should be `nelisp-emacs-magit-bridge--ensure-symbol-runtime`
(47 characters). Instead the name carries **a NUL and then three more bytes**.
Those three bytes are the first three characters of an adjacent string in
memory — here `nel`, i.e. the start of some `nelisp-…`. A name that differs
from the interned one no longer matches, so the lookup fails and the definition
appears never to have happened.

Every instance observed on 2026-08-06 has the same shape — real name, NUL,
three bytes of the neighbouring string:

| observed                       | actual name                  | spliced |
| `--ensure-symbol-runtime<NUL>nel`  | `…--ensure-symbol-runtime`   | `nel` |
| `eieio--class-class-slots<NUL>eie` | `eieio--class-class-slots`   | `eie` |
| `--precond-trace<NUL>cl-`          | `…--precond-trace`           | `cl-` |

The overrun is three bytes every time, and the spliced text is always the
head of a neighbouring string. That regularity is the main lead.

**Read the raw bytes, not shell output.** Bash's `$(...)` silently drops NUL
and only warns, so through a shell the same string looks like two list
elements, or like one concatenated symbol, depending on the capture path. Use
`od -c`. Mis-reading this cost the previous session several hours of chasing a
"reader token boundary" theory that does not exist.

---

## 2. Reproduction — exact, 100%

```sh
cd /home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib
PROBE_LOAD_JOBS=3 bash /home/madblack-21/Cowork/Notes/.claude/skills/nelisp-probe-driver/probe-repeat.sh \
  15 /tmp/miniflat/v2.el
```

Measured 2026-08-06, same driver and same image, varying only the load knob:

| `PROBE_LOAD_JOBS=0` (quiet)      | 15/15 PASS | **0.0%**   |
| `PROBE_LOAD_JOBS=3` (contention) | 15/15 FAIL | **100.0%** |

`PROBE_LOAD_JOBS=K` runs K competing `exec-runtime-image` boots alongside the
measurement. Under contention all fifteen runs failed **on the same victim**.

**A quiet run proves nothing.** The unfixed runtime passes fifteen times in a
row with `PROBE_LOAD_JOBS=0`. Never validate anything on a quiet machine.

If `/tmp/miniflat/v2.el` is gone (it is a scratch file), any driver that loads
`dev/nelisp-emacs-lib/src/nelisp-emacs-magit-bridge.el` (4812 lines, 230720
bytes) and then calls
`nelisp-emacs-magit-bridge--ensure-preconditions` reproduces it; see
`dev/nelisp-emacs-lib/scripts/magit-bridge-probes/` for working examples and
`.claude/skills/nelisp-probe-driver/SKILL.md` for how to build one.

---

## 3. Where to look

`dev/nelisp` is **pure Elisp** — there is no Rust in this repository. The
runtime binary at `target/nelisp` is produced from this Elisp by the project's
own toolchain.

Start from how a symbol's name is stored and how its length is computed:

- `lisp/nelisp-cc-jit-make-symbol.el`, `lisp/nelisp-cc-jit-symbol-name.el`
- `lisp/nelisp-cc-nlstr-*.el` (string representation, clone, drop, direct ops,
  utf-8 direct)
- `lisp/nelisp-allocator.el`, `lisp/nelisp-cc-alloc-dealloc.el`

The shape to look for is a length or slice bound that is computed once and
then used after the underlying storage has moved or been reused — the classic
form being a stale length surviving an arena reallocation. Contention is what
makes the reallocation happen at the wrong moment, which is why the defect is
invisible on a quiet machine.

The constant three-byte overrun is worth explaining rather than papering over.
If a fix makes the symptom disappear without accounting for *why it was three*,
say so explicitly in the report — a fix that merely moves the race is worse
than no fix, because the gate will then read green.

---

## 4. Constraints

1. **Do not add Rust.** There is none here today; a solution that introduces
   any is out of scope regardless of how small.
2. **Do not touch `dev/nelisp-emacs-lib`.** The defect is in the runtime, not
   in the file that exposes it. That repository has a large uncommitted working
   tree belonging to other work; leave it alone.
3. **Do not weaken the reproduction.** Lowering `PROBE_LOAD_JOBS`, raising a
   timeout, or retrying a failed load are not fixes.
4. **Do not commit.** The supervising session verifies independently and owns
   the commit.
5. Keep the change minimal and explain the mechanism. A large refactor that
   happens to make the gate green will be rejected.

---

## 5. Acceptance

Build the runtime, then measure the candidate binary directly:

```sh
cd /home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib
PROBE_BIN=/home/madblack-21/Cowork/Notes/dev/nelisp/target/nelisp \
PROBE_LOAD_JOBS=3 \
  bash /home/madblack-21/Cowork/Notes/.claude/skills/nelisp-probe-driver/probe-repeat.sh \
    15 /tmp/miniflat/v2.el
```

Required: **`pass=15 fail=0` with `load_jobs=3`.**

Also report, from the same harness:

- the before numbers on the unfixed binary (expected 0/15 pass under load), and
- `PROBE_LOAD_JOBS=0` after the fix (must stay 15/15 — the fix must not break
  the quiet path).

Report the diff, the mechanism, both sets of numbers, and anything you found
that contradicts section 1 or 2.
