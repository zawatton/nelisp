<!-- Template for a coordinator-dispatched codex/subagent brief.
     Launch with: tools/ai/codex-track.sh NAME BASE_SHA THIS_FILE
     Delete these comments and every unused section before dispatching. -->

# STARTING STATE

Read `BRIEF-CONTEXT.txt` at the root of your work tree FIRST and quote its
three values (`base-sha`, `base-subject`, `tree-digest`) verbatim in your final
report under a heading "STARTING STATE".

The tree is already prepared. **git is not available to you** -- do not check
anything out, do not run git at all. Leave your edits in the work tree; the
coordinator commits and pushes.

<!-- NEVER write "check out X" here.  A brief that says both "check out X" and
     "do not run git" is self-contradictory: the agent cannot do the first, so
     it silently reads whatever tree it was given.  That happened once and
     produced a confident report that four existing mechanisms "do not exist".
     The coordinator prepares the tree; this section only DESCRIBES it. -->

# TASK

<!-- One paragraph: what to build or fix, and the ONE sentence of why it
     matters.  If there is a measurement that makes the case, put the numbers
     here, not an adjective. -->

# WHAT IS ALREADY KNOWN

<!-- Everything the agent must NOT re-derive.  Cite file:line or a measured
     number for each claim so the agent can check rather than trust.  If a
     prior session narrowed a bug, give the narrowing as a map: what
     reproduces, what does NOT reproduce, and on which substrate.  Mark
     anything you are NOT certain of as "verify this holds" rather than
     stating it flatly -- an agent that inherits a wrong premise confidently
     will spend its whole run inside it. -->

# CONSTRAINTS

- Rust LOC must never increase (pure Elisp).
- Opt-in discipline: unannotated Elisp stays byte-identical to stock Emacs.
- Do not weaken, skip, or delete an existing assertion to make something pass.
- Do not raise a ratchet baseline to make room; if one legitimately moves, say
  so and justify it with the measurement.
- `make compile` clean (warnings are errors) and `make parens-check` clean.

# DEFINITION OF DONE

<!-- Enumerate the artifacts.  A gate is only wired if it is wired in ALL
     THREE places -- Makefile target, tools/ai/gates.expected, and the right
     tier in tools/ai/nelisp-ai.sh -- a gate missing one of them fails
     verification with "no report". -->

# MUTATION ROW (if this adds or changes a gate)

Add a `tools/gate-mutations.txt` row and PROVE IT IS REACHED: inject it,
observe the gate go RED, restore, observe GREEN. Record that evidence in the
row's comment. Prefer a row whose RED-ness is **structural** (the check itself
is gone) over one that depends on timing or load -- an environment-sensitive
row can be RED locally on every attempt and still green in CI. If you cannot
construct a genuinely reachable row, add none and say why.

# REPORT

State, in this order:

1. **STARTING STATE** -- the three values from `BRIEF-CONTEXT.txt`.
2. What you did, and the measurement that proves each load-bearing claim.
3. **Anything in this brief that turned out to be WRONG**, called out
   explicitly. This is the most valuable line in your report; a brief is a
   hypothesis, not an authority.
4. Anything you could not do, and why -- rather than narrowing the task
   silently to what did work.
