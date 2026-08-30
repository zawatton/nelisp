# A top-level form whose result is immediate could rewind the arena over a fresh mirror entry

Status: **fixed 2026-08-21.**  This file previously described the same defect
with the wrong cause; the correction is the point of this rewrite, so the
original claim is kept below rather than quietly replaced.

## What was wrong with the first diagnosis

The first write-up said the deciding factor was the VALUE BEING STORED: that
`(fset 'FRESH 5)` corrupted the table because `5` is an immediate, while
`(fset 'FRESH "s")` was clean because a string is heap-tagged.  It came with a
table of measurements that all agreed with it.

They agreed because every case in that table was a bare `fset`, and a bare
`fset` RETURNS the value it stores.  The table could not tell the two apart.
Separating them takes four cases, not two:

| stored value  | form's result | bindings lost |
|---------------|---------------|---------------|
| `5`  immediate| immediate     | 2             |
| `5`  immediate| `"heap"`      | 0             |
| `"s"` heap    | `7` immediate | **2**         |
| `"s"` heap    | heap          | 0             |

Row 3 is the falsifier: storing a heap string still loses bindings when the
form returns an integer.  **The stored value is irrelevant.  What matters is
the tag of the value the top-level form returns.**

## The actual cause

`nl_boundary_maybe_reclaim` (`scripts/nelisp-standalone-build.el`) rewinds the
per-form bump arena to a mark when all of:

1. boundary reclamation is enabled,
2. the form's result is an immediate (`nl_boundary_immediate_result_p`, tag <= 3),
3. the mutation epoch at `268435544` is unchanged since the form began,
4. no signal is pending.

Condition 3 is the guard that should have stopped it.  Installing an entry in
the env mirror IS a mutation -- it puts an arena-allocated record, and the
bucket cons cells linking it, into a table that outlives the form.  But the
mirror install path did not bump the epoch, so the guard saw a form that had
allegedly escaped nothing, and the arena was rewound out from under the new
entry.  The bucket then pointed into reclaimed space; the entries behind the
new head went with it; a later allocation landing on that space is what ended
the process.

Confirmed by disabling reclamation and rebuilding: all four cases above go to
zero losses, and the interleaved walk that used to end the process runs clean.

## Why the guard had this hole

This is the third instance of one pattern, and the source already documented
the other two:

- the macro-expansion cache store (`nl_mxcache_store`), fixed by bumping the
  epoch, with a root-cause comment describing exactly this failure;
- `nelisp_frame_push` / `nelisp_frame_pop_inner` installing the depth Int into
  the persistent frames record, same fix, same comment.

A 2026-06-01 note in the same file is blunter still: this reclamation was
prototyped, measured to be a large win, and **left out as incorrect**, naming
`(fset 'g ..) (g 1) (g 10)` -> 0 among its failures, because "the eval
machinery escapes boxes above the mark ... that the build-glue cannot
enumerate".  Doc 140 Stage 5 later shipped it behind the epoch gate.  The gate
is only as good as the list of sites that bump it, and the list is maintained
by hand -- the comment above it claims `fset` bumps, and it did not.

## The fix

`mirror-prepend.o` and `mirror-setfn.o` now go through the same
`--reader-extra-unit-epoch` wrapper the frame and setq sites use:

- `nelisp_mirror_bucket_prepend` -- every insert, whichever `_or_insert`
  wrapper called it;
- `nelisp_mirror_set_function_or_insert` -- additionally the hit path, which
  stores a box into an existing record.

Over-bumping is safe by design here ("under-bumping would corrupt,
over-bumping only leaks").

## Verified

- the four cases above: 0 losses each;
- the interleaved insert-and-walk that used to end the process: 4 rounds, no
  losses, exit 0;
- neighbouring escape sites probed for the same hazard and clean: hit-path
  `fset` with a heap value and an immediate result, `set` on a fresh global,
  `defvar`, `defun`, `puthash`;
- `make test` 5036 tests, 0 unexpected; emacs-parity byte-identical; eleven
  gates PASS; ten fuzz seeds x 1500 cases, 0 disagreements.

## Pre-existing failures this did NOT touch

`tools/ai/nelisp-ai.sh check` is red for two reasons that are older than this
work, both measured at `e368da033` without the fix applied:

- `parens-check`: 6 findings.  Fixed separately, and the severity I first
  reported was wrong -- see the note at the end of this file.
- `standalone-reader-test`: fails at "generated reader literal smoke".  A
  control build at `e368da033` fails identically, so it is not from this
  change.  (The stale report `verify` was reading named a different sub-smoke,
  `compile-runtime-image` -- worth knowing that the two differ.)


## Correction: the `parens-check` findings were indentation, not structure

I first reported these as an unclosed `unless` in the prelude that swallowed
the 183 top-level forms after it, so that if `string-version-lessp` ever became
fbound natively all 183 definitions would vanish.  That was wrong, and reading
the checker's message instead of measuring the structure is how I got there.

Measured, the form has exactly two body forms:

    form #231: unless with 2 body form(s)
      body heads: ((defun nelisp--version-rank) (defun string-version-lessp))

The other two sites are the same shape -- a guard helper and the function it
guards, both intended to be inside the `unless`.  All three parse as intended
and always did.

The real defect is that the inner `defun` was written at column 0, so the
nesting is invisible to a reader: the code looks like a top-level definition
that a missing paren has accidentally swallowed.  `parens-check` flags exactly
that, because a genuine missing-paren bug is indistinguishable from it on
sight.  "absorbed the 183 top-level forms" is the checker's heuristic phrasing
for a column-0 form found nested, not a claim about the parse.

Fixed by re-indenting the three forms, whitespace only.  Verified by reading
every form in each file before and after and comparing: 641/641, 66/66 and 4/4
forms, `equal` in all three.
