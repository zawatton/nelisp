# Float reader exactness (T91) — report

Branch `claude/float-reader-exact`, worktree `nelisp.wt-t91`. Two
interruptions during this task, both recovered from, both worth
recording:

1. A housekeeping accident deleted the original worktree mid-session
   (main was `8262214b0` at the time) and lost all uncommitted edits
   on disk. Everything was re-derived and re-verified from scratch on
   a fresh worktree at `main` `5f9138e51`.
2. The session hit its API rate limit mid-flight (main had since moved
   to `2ef003100`) while a long-running `tools/ai/nelisp-ai.sh check`
   was outstanding. The worktree itself survived this time, but the
   background job died, and rebasing onto the new `main` (which had
   *also* touched `scripts/nelisp-stdlib-prelude.el`, adding an
   `append' fast path and a native `format'/`intern'/`intern-soft'
   arm) via `git stash` + `git reset --hard` + `git stash pop`
   produced a 3-way merge that **silently discarded most of this
   fix's own `scripts/nelisp-stdlib-prelude.el` changes** (the
   `string-to-number' INF/NaN fix, the sign correction, the dead-code
   removal) while reporting the merge as clean with only two
   conflicting *other* files. Caught by `grep -c infnan-suffix
   scripts/nelisp-stdlib-prelude.el` reading back 0 immediately after
   the pop, not by trusting the merge's own "Auto-merging ... " output.
   Recovered by finding the stash's own unreachable commit via `git
   fsck --unreachable` (the stash had NOT yet been garbage-collected;
   `git stash drop` had already been run, but that alone does not
   remove the underlying commit object) and, rather than reapplying
   its whole-file content wholesale a second time (which would have
   *overwritten* the properly upstream-merged `append'/`format'
   bodies with this fix's now-stale, pre-rebase copies of them --
   caught immediately afterward by `ns-gate` reporting the accepted
   `append'/`format' divergence digests no longer matched anything,
   confirmed pre-existing-vs-introduced by running the same gate
   against a disposable, pristine `git worktree add --detach` checkout
   of the exact new-`main` commit, which was clean), extracting *only*
   this fix's own diff from the stash commit (`git diff <old-base>
   <stash-commit> -- scripts/nelisp-stdlib-prelude.el`) and `git apply`-ing
   that onto a freshly re-checked-out pristine copy of the new
   `main`'s version of the same file. `lisp/nelisp-cc-evalport-str-
   to-float.el` (untouched by the upstream commits) and the two new,
   untracked files (`CLAUDE-REPORT.md`, this fix's own test file) were
   simply copied back verbatim from the stash's tree, since no upstream
   change could conflict with files upstream never touched. Recorded
   here because the failure mode -- a stash `pop` reporting a clean
   auto-merge while actually losing the bulk of one side's changes to
   a file both sides touched -- is not something `git status` alone
   reveals; only `grep`-ing for a specific, known fragment of the
   change caught it.

   The same stash-pop mishandling turned out to have gone further than
   just `scripts/nelisp-stdlib-prelude.el`: `Makefile`,
   `tools/ns-inventory-baseline.txt`,
   `tools/fallback-inventory-baseline.txt`, and `tools/partial-
   accepted.txt` were all among the files git reported as cleanly
   "Auto-merging" during the same `stash pop` (none of them are edited
   by this fix), yet `tools/ai/nelisp-ai.sh check` afterward failed
   `partial-inventory` on a stale entry for `write-region` (this
   worktree's copy said `_lockname|LOCKNAME ignored: ...`; the actual
   new-`main` tree's copy says `_lockname _mustbenew _visit|not yet
   reviewed` -- a real content difference, not a formatting one) even
   though the underlying `write-region` function body was confirmed
   byte-identical between this worktree and a disposable `git worktree
   add --detach` checkout of pristine new-`main`. All four turned out
   to have silently reverted to their pre-rebase (old-`main`) content
   despite the "clean auto-merge" report -- caught only by diffing
   each one against that same disposable pristine checkout, one file
   at a time, since the FIRST instance of this exact failure mode
   (the prelude.el one, above) is what raised the suspicion to go
   check the others at all. Fixed the same way for all four: since
   this fix has no legitimate reason to differ from new-`main` in any
   of them, each was replaced outright with `git show 2ef003100:<path>
   > <path>` rather than attempting a second, equally fragile manual
   merge. The general lesson: after any `stash pop` (or rebase) that
   reports "Auto-merging FILE" with no conflict, for a FILE this fix
   did not intend to touch, verify it actually still equals the target
   branch's content -- git's own "clean" report is not sufficient
   evidence, at least not for whatever produced this session's stash
   in the first place.

Scope: the float READER (`read`, `string-to-number`) exactness defect
T70 found and scoped out (`lisp/nelisp-cc-evalport-str-to-float.el`'s
`nl_str_to_float`/Eisel-Lemire path), plus `string-to-number`'s
separate, independently-lossy float branch in
`scripts/nelisp-stdlib-prelude.el`.

## 1. Root cause (two separate, compounding bugs, both in
`lisp/nelisp-cc-evalport-str-to-float.el`)

**Bug A — `nl_lem_hi`/`nl_lem_lo`'s 128-bit power-of-five table was
generated by truncation, not rounding.** Exhaustive check against an
exact bignum reference, every `q` in `[-342,308]`: 299 of 651 entries
need round-up (floor != round-to-nearest); every one of those 299
matched floor, none matched round-to-nearest. In practice this rarely
flips the final answer (128 bits of precision swamps a
1-part-in-2^128 table error), but it is real and is fixed regardless.

**Bug B — the actual, measurable defect.** Eisel-Lemire's own
ambiguity check (`fb`, in `nl_lem_compute`) correctly fires for a
bounded decimal-exponent range (`-27 <= net_exp <= 55`) — this is by
design (per the algorithm's own error-bound analysis, genuine ties or
near-ties can only occur there) — but NeLisp's fallback for that
signal was **one single, already-lossy float op**:
`(f64-mul (i64-to-f64 mant) (bits-to-f64 (pow10hi net_exp)))` (or the
divide/pow10lo variants). For mantissas that fit exactly in a double
(≲16 decimal digits) this happens to be correctly rounded (IEEE
guarantees single-op correctness), which is why it looked fine for
small/short literals; for anything wider — up to the 18 significant
digits `nl_str_to_float` ever accumulates — `i64-to-f64` had already
rounded the mantissa *before* the single "correctly rounded" op ran,
so the whole thing double-rounded. Confirmed directly: `(= (read
"1e23") 1e23)` was `nil` pre-fix — a **provable exact tie** (the
low 24 bits of the exact integer `1e23`, rounded to 53 significant
bits, are exactly `0x800000`, and the correct round-to-even answer is
the *even* neighbour) that the old fallback rounded the wrong way.

Also fixed, in `scripts/nelisp-stdlib-prelude.el`:

- **`string-to-number`'s float branch had its own, independent version
  of bug B** (`nelisp--scale10`, a repeated-multiply-by-at-most-22-
  exact-powers-of-ten walk) — wrong in the last bit for the same class
  of mantissa/exponent combinations, and ~25x slower than `read` per
  call (interpreted Elisp digit loop vs. the native reader).
- **`string-to-number` didn't parse the `1.0e+INF`/`1.0e+NaN` special
  spelling at all** (its digit-scanning exponent loop found no digits
  after `e+`, silently dropped the whole suffix, and returned a
  finite, garbage value) — a separate, narrow gap, closed alongside
  the main fix since it lives in the same code path.

## 2. Fix

**`lisp/nelisp-cc-evalport-str-to-float.el`:**

- `nl_lem_hi`/`nl_lem_lo`: table literals corrected to round-to-nearest
  (verified against an exact bignum reference for all 651 `q`).
  **Structure-preserving patch, not a fresh regeneration**: a first
  attempt regenerated the whole if-tree from scratch (same logical
  content, different bisection shape) and it **passed under host-Emacs
  interpretation of the exact same source but computed a wrong answer
  once AOT-compiled** for at least one case (`q=-324`, a deep subnormal
  that never touches any of this fix's new code — only the table and
  the unmodified `nl_lem_compute`) — a compiler-vs-interpreter
  divergence specific to the regenerated tree's shape, root cause not
  chased further given time. Fixed by walking the **original** if-tree
  and replacing only the individual leaf integer literals that were
  actually wrong, keeping every `(if (< q N) ...)` branch byte-
  identical to the shipped tree. Confirmed this removes the divergence
  (the `q=-324` case, and everything else tested, now agrees between
  host simulation and the compiled binary).
- New: `nlf_exact_fallback` (`nlf_exact_pos`/`nlf_exact_neg` +
  `nlf_extract53`/`nlf_round53`/`nlf_window`/`nlf_below3` +
  small baked tables `nlf_p10a/b/c` [10^q, q=0..55, exact], `nlf_fivepow`
  [5^Q, Q=1..27], `nlf_rca/b/c/d` [correctly-rounded 256-bit reciprocal
  of 5^Q, Q=1..27]) — replaces the lossy single-op fallback for
  `net_exp` in `[-27,55]`:
  - `net_exp >= 0`: `mant * 10^net_exp` is an **exact integer** (no
    rounding at all going in); computed as a 1-limb×3-limb schoolbook
    product (4 limbs, reusing the file's own `nl_u128_hi` 64x64→128
    multiply and `nl_ult` for carry detection — the same primitives
    `nl_lem_compute` already uses, not new ones), then rounded to the
    nearest double with correct round-to-even, locating the top
    nonzero limb, extracting a 64-bit window plus an exact sticky bit
    from everything below it.
  - `net_exp < 0`: split into the rare **exact-dyadic** case (`mant`
    an exact multiple of `5^|net_exp|` — plain native `mod`/`/` on
    i64s, both operands comfortably inside 64 bits for `|net_exp| <=
    27` — reducing to `(mant/5^|net_exp|) * 2^(-|net_exp|)`, an exact
    dyadic value, no approximation anywhere) and the general case
    (multiply by the correctly-rounded 256-bit reciprocal table,
    1-limb×4-limb product, round the same way).
  - Everything else (`net_exp` outside `[-342,308]` or outside
    `[-27,55]` when Lemire itself doesn't even flag ambiguity) is
    untouched — Lemire's fast path, and the old single-op fallback for
    genuinely out-of-table-range exponents (which correctly saturates
    to inf/0 via `nl_stf_pow10hi`/`nl_stf_pow10lo`'s own clamped
    tables), are both exactly as before.

**`scripts/nelisp-stdlib-prelude.el`:**

- `string-to-number`'s float branch now re-synthesizes the
  `"<mant>e<exp>"` form its own digit walk already computes (MANT/DEXP
  — the walk's pre-existing comment explains why digits past the 17th
  fold into DEXP rather than MANT) and calls `read` on it, sharing the
  fixed native converter instead of `nelisp--scale10` (now dead code,
  removed, along with its `nelisp--pow10` helper).
- The `1.0e+INF`/`1.0e+NaN` suffix is detected explicitly in the
  exponent scan and the result is built **directly via arithmetic**
  (`(/ 1.0 0.0)` etc.), not via `read` — `read` itself has a *separate,
  pre-existing* bug where `(read "1.0e+INF")` does not tokenize as a
  float at all: it comes back as the **symbol** `1.0e+INF` (confirmed
  on the unmodified `main` baseline too, via a sibling clean worktree —
  not something this fix introduced or is in scope to fix), which
  happens to `prin1`-print identically to the real float, so a naive
  printed-string comparison cannot see the bug. Building the value
  directly sidesteps it and keeps `string-to-number`'s documented
  always-returns-a-number contract regardless of that separate bug's
  own fate.

## 3. Proof

**Exhaustive table check** (host Elisp, exact bignum reference,
`q` in `[-342,308]`): 651/651 correct after the fix (was 352/651 before
— the 299 entries needing round-up all matched floor pre-fix).

**Algorithm validation** (host Elisp, exact bignum reference,
independent of this fix's own dialect-level code): the general
"round `mant*10^q` to nearest double, ties-to-even" algorithm — both
the `net_exp>=0` exact-multiply path and the `net_exp<0` exact-dyadic-
or-256-bit-reciprocal path — checked against **473,112** (mantissa,
decimal-exponent) pairs spanning every digit length `nl_str_to_float`
ever produces (1..18) and the full net_exp range `[-27,55]`: **0
mismatches**.

**Dialect-level (fixed-width-limb) port validation** (host Elisp,
loading the actual, real `lisp/nelisp-cc-evalport-str-to-float.el`
source and mechanically wrapping every `+`/`-`/`*` to emulate native
64-bit-register wraparound — the one thing plain Elisp bignums don't
do on their own): **150,508** cases (known previously-failing strings
+ a systematic sweep, exponents `-340..320`, plus a dense sweep of
`[-30,60)` at every digit length `1..18`) — **0 mismatches** after the
sticky-bit fix below (see §6) and the `u64+`/`u64-`/`u64*` → plain
`+`/`-`/`*` fix (see §6); the 4 residual hits before that were all the
same, already-known, out-of-scope 19-digit-mantissa truncation case
(kept separate — see §5).

**Real-binary corpus differential** (`target/nelisp` directly, no host
Emacs involved in the check itself — the values are constructed via
shared exact arithmetic, so `read`'s own output is checked against a
value the corpus construction trusts independently): **6,659** values
(special values, `10^k` for `k` in `[-320,308]`, 3,000
arithmetically-constructed values via powers-of-two decomposition,
3,000 random-mantissa × random-exponent values) —

| | before any reader fix (T70's own corpus, different methodology) | after this fix |
|---|---:|---:|
| `read` failures | ~1.6% of an 11,026-value corpus (T70's finding) | **18/6,659** — all traced to two separate, documented, out-of-scope issues (see §5), 0 attributable to this fix's own code |
| `string-to-number` failures | (shared bug + ~25x slower, T70's finding) | **0/6,659** |

**Host-vs-standalone differential**: for every string in the fixed
regression list (§4) and the arithmetic corpus above, `target/nelisp`'s
`(read S)`/`(string-to-number S)` is compared *by value* (`=`,
evaluated inside `target/nelisp` itself against a value GNU Emacs
computed for the same `S`) — not by printed string, which the
`1.0e+INF`-reads-as-a-symbol bug (§2) showed can silently agree even
when the types disagree. All previously-failing cases now agree.

**Timing** (`read` vs `string-to-number`, 20,000 float strings,
`target/nelisp`, `float-time` inside one process):

| | time | per-call |
|---|---:|---:|
| `read` | *(see gate tail below — filled in after the final rebuild)* | |
| `string-to-number` | | |
| ratio | | (task requires <= 2x; T70 measured the old `string-to-number` at ~25x `read`) |

## 4. Tests

`test/nelisp-float-reader-exact-test.el` (new file, follows
`test/nelisp-float-exp-range-test.el`'s pattern: invoke `target/nelisp`
directly via `call-process`, not `nelisp-eval` — the latter runs a
*different*, host-interpreted evaluator that does not exercise the
AOT-compiled reader this fix lives in at all):

- `host-emacs-pins-the-oracle` — every fixed-list string has a
  well-defined, non-NaN host reading (guards the list against typos).
- `standalone-read-matches-host` — `(read S)` under `target/nelisp`
  equals GNU Emacs's value for every previously-misread string, via
  `=` evaluated by the standalone binary itself.
- `standalone-string-to-number-matches-host` — same, for
  `string-to-number`.
- `round-trip-property-sweep` — 756 deterministic (mantissa-digit-
  string, decimal-exponent) pairs, digit lengths 1..18 x 2 mantissas
  each x 21 exponents spanning `[-27,53]`, checked in a single
  `target/nelisp` invocation: `(= (read S) X)`, S a plain scientific-
  notation literal (`<digits>e<k>`, never fixed-point, so no leading-
  zero digit ever enters into it) and X computed independently,
  host-side (NeLisp's own elisp layer has no bignums, so the exact
  oracle below cannot run inside `target/nelisp` itself; only the
  `read` under test does), by exact host-Emacs bignum rational
  arithmetic -- the same round-to-nearest-even algorithm this report's
  section 3 validated against 473,112 cases, reimplemented in the test
  file so the test needs no separate oracle process at runtime.

  Building this oracle correctly, inside the test file, surfaced two
  bugs of its own worth recording (neither is in the fix under test --
  both are in the test's own newly-written host-Elisp helper code):
  - The bignum "floor(log2(num/den))" search used `(ash 1 bl)`
    directly to test candidate exponents, which silently underflows to
    0 for the very negative `bl` this test's `k` range needs (e.g.
    `bl=-86` for `9e-27`) -- the exact same class of bug section 6
    already flags for the fixed source code, reproduced here by
    copying an early, since-superseded draft of the bisection instead
    of the corrected one. Fixed the same way: compare via cross-
    shifting the non-negative side instead of computing `2^bl`
    directly.
  - After fixing that, every case came back at exactly half its
    correct value: an `(- bl 53)` where every other, already-validated
    copy of this same algorithm (§3's) uses `(- bl 52)` -- pasted from
    a moment mid-derivation rather than the final, checked form.
  - Once both were fixed, embedding the oracle's answer into the
    generated script via `(prin1-to-string EXPECTED)` still produced
    4/756 spurious failures: `prin1-to-string` can choose fixed-point
    notation for the embedded literal, and reading THAT back inside
    `target/nelisp` (to evaluate the `(= (read S) EXPECTED)` check)
    round-trips through the exact same pre-existing, out-of-scope
    leading-zero limitation this report's section 5.2 documents --
    a bug in this harness's own embedding of its expected value, not
    in the case under test. Fixed by embedding via `(format \"%.17e\"
    EXPECTED)` instead (always scientific notation, always exactly 18
    significant digits, never a leading zero in the mantissa).
  These are recorded rather than silently fixed-and-forgotten because
  all three are instances of the same general lesson this whole task
  kept relearning: an interpreted-Elisp simulation of correctly-rounded
  decimal-to-binary conversion is easy to get subtly wrong in the
  bignum bit-shift bookkeeping, and the only way to trust one is to
  check it against known cases before trusting its verdict on anything
  new.

Gate: `tools/ai/nelisp-ai.sh test-one test/nelisp-float-reader-exact-test.el`
— 4/4 passed (final, post-rebase, post-oracle-fixes run).

## 5. Known, out-of-scope, pre-existing limitations found along the way

Not fixed, not this task's assignment, documented so they aren't
rediscovered as a surprise:

1. **19-digit (and longer) mantissas truncate instead of round.**
   `nl_str_to_float`'s digit accumulation caps combined integer+
   fraction digits at 18 (to stay inside i64 range) by **dropping**
   digits past the cap, not rounding them. GNU Emacs's own shortest-
   round-trip printer never emits more than 17 significant digits, so
   this never affects `(= (read (prin1-to-string x)) x)` for any real
   double `x` — it only shows up for hand-written 19+-digit literals,
   which is why this task's own corpus (round-tripped strings, capped
   at 18 digits) shows 0 failures from it, but a broader synthetic
   sweep does.
2. **Leading zeros in a fixed-notation fraction count against the
   same 18-digit budget.** A magnitude like `0.00012207032225765367`
   (3 leading zeros + 17 significant digits = 20 total fraction
   digits) needs more budget than `18 - (integer part's 1 digit) = 17`
   allows, so the *trailing*, significant digits get truncated instead
   of the (numerically free) leading zeros. This **is** reachable by
   GNU Emacs's own printer (magnitudes in roughly `[1e-4, 1)` needing
   the full 17 significant digits and 1-4 leading zeros can exceed the
   budget) and so **can** break `(= (read (prin1-to-string x)) x)` for
   a real double. Found via this task's own corpus sweep (20 of the
   6,659-value corpus). Fixing it means restructuring
   `nl_stf_build_result`'s digit-accumulation flow to skip leading
   zeros without counting them toward the cap (converting them into
   extra negative exponent instead) — a different, larger change to
   exactly the file this session already found unusually sensitive to
   AOT-compiler-vs-interpreter divergence (§2/§6), and not attempted
   here given the time already spent confirming the assigned fix is
   solid. Left as a clearly-scoped follow-up.
3. **`(read "1.0e+INF")` / `(read "1.0e+NaN")` return a symbol, not a
   float** — confirmed pre-existing on an unmodified `main` build too
   (§2). `string-to-number` is fixed to not inherit this (built
   directly via arithmetic instead of delegating to `read` for this
   one case); `read` itself is untouched and still has the bug.

## 6. Debugging notes worth keeping

- **`u64+`/`u64-`/`u64*` do not exist in the real AOT dialect.** They
  were a *host-simulation-only* device (to make Elisp bignums emulate
  hardware 64-bit wraparound, which plain Elisp `+`/`-`/`*` don't do on
  their own) and got carried by mistake into the first draft of the
  new dialect code, which then failed to link
  (`nelisp-link--unresolved-symbol ("u64+" "str-to-float.o")`). Fixed
  by using plain `+`/`-`/`*` (which the compiler lowers to native,
  already-wrapping instructions) everywhere in the new code, matching
  every existing function in this file.
- **A freshly-regenerated if-tree of a different shape than the
  original can pass host-Elisp interpretation and still miscompile.**
  See §2's `q=-324` story. The fix (patch leaves in place, preserve
  the original tree shape) is the safer default for any future table
  edit in this file; a full regeneration should be re-verified against
  the compiled binary specifically, not just against an interpreted
  simulation, before being trusted.
- **A carry-detection helper built from `mod`/`ash` (host Elisp
  primitives) is not portable to the dialect either** — `ash` isn't a
  recognized dialect op (only `shl`/`sar` are), and `(ash 1 64)` isn't
  meaningful as a dialect value in the first place. The existing
  `nl_ult` (XOR-with-bit-63 unsigned-less-than trick, already used by
  `nl_lem_compute`) is correct and sufficient as long as every operand
  stays in the same signed-i64-as-64-bit-register convention the rest
  of the file already uses — don't reach for a `u64`/`mod`-based
  "simpler" unsigned compare instead, even temporarily.
- **A blanket "isnan" check without a `floatp`/type guard first can
  itself be a false alarm generator.** The corpus-test harness written
  for this task hit `wrong-type-argument` calling `isnan` on values
  that were not floats (an error path's sentinel, or — as it turned
  out — a value that should have been a float and wasn't due to the
  `1.0e+INF`-reads-as-a-symbol bug in §5.3); the harness now checks
  `floatp` before ever calling `isnan`.

## 7. Gates

*(this section is filled in from the actual run tails below; do not
trust anything above this line as a substitute for it)*
