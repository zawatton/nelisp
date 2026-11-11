# Windows CI hang: host-Emacs C stack overflow in the standalone build

## Step 1: why the job hangs (premise corrected)
After "Re-entering top level after C stack overflow" batch Emacs sits in its idle
command loop (strace: pselect on the internal eventfd, timeout 100000 s). It is NOT
waiting on stdin: with `</dev/null` and with `<&-` the repro still hangs
(`timeout 60` exit 124 both ways; see tools/ai/ci-speedup/stdin-hang-repro.sh).
So a stdin redirect is not a fix and was not applied.
No in-build fail-fast is merged.  A watchdog timer that detected the idle loop
(exit 70 after 11-14 s under `ulimit -s 1024`, same spot as CI:
`nelisp-tco: 0 rewrite(s) in unit val-load.obj`) was prototyped but dropped in
review: it relied on an unspecified Emacs 31.1 detail (timers run from the idle
loop lack a `timer-event-handler` frame).  The root cause is fixed below, and any
future hang is bounded by the CI step/job timeouts (CHECKED steps 30 min).

## Step 2: deep walkers (Linux, windows-x86_64 target, depth limit lowered to 1600)
Method: ulimit -s 8192, max-lisp-eval-depth 1600, debug-on-error, read the backtrace.
1. `--cl-walk--` in `nelisp-standalone--rebase-arena-source` (scripts/nelisp-standalone-build.el
   ~796; same pattern in `nelisp-standalone--windows-rebase-arena-source` ~766):
   `(cons (walk (car f)) (walk (cdr f)))` recurses once per list ELEMENT. Input: the
   unit source `(seq ...)` with thousands of forms (called at ~3580).
2. `nelisp-standalone--inline-clone-p` (~2405): car/cdr recursion, same shape; driven by
   the long `nl_sym_init_one`/`ptr-write-u64` sequence (~500 elements deep).
Both depths grew with list length, not with form nesting.

## Step 3: fix
Both walkers now iterate along the cdr spine (new helper
`nelisp-standalone--map-list-spine`; `inline-clone-p` uses a while loop, still testing
every tail like the old recursion). Depth now follows nesting only. Full Windows-target
build then completes at max-lisp-eval-depth 1600; the binding at ~3590 is lowered
from 40000 to 5000 (margin). Output: same size (60450467 B) as the base build at an equal-length path; the
remaining differing bytes are the embedded build-script digests (the script
changed) and base/fixd path strings.  Fixed build: rc=0 at `ulimit -s 1024`, 214-244 s.
