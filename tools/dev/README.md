# nl-dev-session: resident NeLisp image for build-free probes

One long-lived `exec-runtime-image` process; probes are sent as files and cost milliseconds.
No binary rebuild, no image re-bake. The base binary/image are used read-only.

    export NLDS_BASE=<dir with target/nelisp and build/nemacs-runtime.nlri>   # read-only base
    export NLDS_CPUS=9,25 NLDS_TIMEOUT=120          # taskset list (SMT pair), per-eval timeout (s)
    tools/dev/nl-dev-session.sh start               # ~65 s once (image replay of the 8 MB bootstrap)
    tools/dev/nl-dev-session.sh eval-string '(+ 1 2)'
    tools/dev/nl-dev-session.sh eval FORM-FILE      # forms are read+eval'd one by one; last value printed
    tools/dev/nl-dev-session.sh load-el FILE.el [real]   # (re)load a changed .el; `real` = image's own load
    tools/dev/nl-dev-session.sh load-neln FILE.neln SYM...   # needs (require 'nelisp-native-load) first
    tools/dev/nl-dev-session.sh status | log | stop

Properties
- Process runs under `ulimit -v 8388608`, stdin /dev/null, optional taskset. State dir NLDS_DIR.
- Per-eval timeout: on expiry the client kills the session (own pid only) and says so; restart costs 65 s.
- Fast loader (default, NLDS_FAST=0 to disable): `load`/`require` of .el files under the lane's `lisp/`
  (plus NLDS_LOAD_PATH dirs) read+eval each form instead of the image's `load`. 0.14 s vs 50 s for
  lisp/nelisp-native-load.el; `(require 'nelisp-native-load)` 0.24 s vs 36-97 s. Differences: no reader
  rewrites, no eager macroexpansion, `load-in-progress` stays nil. Vendor/baked files keep the real loader.
  Use `load-el FILE real` when you need the real loader semantics.
- In-load trap (state-perf F95): requests are evaluated by the loop, not inside a `(load ...)`; the client
  refuses a request containing both `(load ` and timing code (NLDS_FORCE=1 overrides). Send the load and the
  timed form as separate requests. `(nl-dev-time-call FN)` errors if `load-in-progress` is set.
- Output (princ) and `NLDS=> VALUE` / `NLDS-ERR ...` come back on the client's stdout.

Verdicts from the session license "correct and worth batching" only, never perf numbers or "integrated"
(see review-fable-buildfree-strategy.md section 2). Pin probes to CPUs that no benchmark uses.
