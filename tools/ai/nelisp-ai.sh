#!/bin/sh
# nelisp-ai.sh --- one entry point for working in this repository.
#
# The root Makefile has 76 targets and no `help'.  This script is not a
# replacement for it; it is the short list of things worth running on
# every change, wired to emit machine-checkable gate reports instead of
# bare exit codes.  See AI.md for the workflow and tools/ai/README.md for
# the report contract.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
cd "$root"

EMACS=${EMACS:-emacs}
GATE_DIR=${NELISP_GATE_DIR:-target/gates}
export NELISP_GATE_DIR="$GATE_DIR"

# Mirrors the load path the root Makefile builds for `test'.
load_path_args() {
    printf -- '-L lisp -L src -L test -L bench'
    for d in packages/*/src packages/*/test; do
        [ -d "$d" ] && printf -- ' -L %s' "$d"
    done
}

test_files() {
    # Same shape as the Makefile's TESTS: `*-test.el' only, soak excluded.
    # The `-test.el' suffix is load-bearing — a plain `test/*.el' glob in a
    # sibling repository swept in driver scripts, the batch run died before
    # defining any test, and the suite reported an exit code instead of
    # "0 tests".
    #
    # NELISP_GATE_MUTATION_TEST_FILES, when set, replaces the glob below
    # with an explicit list.  It exists only for
    # `tools/nelisp-gate-mutation.sh': proving the `ert-full' gate goes red
    # on a corrupted widely-used helper has to run tests through this same
    # function and `run_ert_gate', but the full suite costs about 75s per
    # injection — too slow to carry inside `nelisp-ai.sh check''s fast
    # lane.  Scoping to the one file that calls the corrupted helper
    # exercises the identical mechanism at a fraction of the cost; see
    # `tools/gate-mutations.txt' and the `ert-full' note in
    # `tools/ai/gates.expected'.  Every ordinary invocation of
    # `nelisp-ai.sh test' / `check' / `verify' leaves this unset and gets
    # the real, unscoped glob below.
    if [ -n "${NELISP_GATE_MUTATION_TEST_FILES:-}" ]; then
        printf '%s\n' "$NELISP_GATE_MUTATION_TEST_FILES"
        return 0
    fi
    ls test/nelisp*-test.el packages/*/test/nelisp*-test.el \
       packages/*/test/nl-*-test.el 2>/dev/null \
        | grep -v 'nelisp-worker-soak-test\.el$' || true
}

nelisp_binary() {
    if [ -n "${NELISP_BIN:-}" ]; then printf '%s' "$NELISP_BIN"; return 0; fi
    for candidate in target/nelisp.exe target/nelisp; do
        [ -x "$candidate" ] || [ -f "$candidate" ] && { printf '%s' "$candidate"; return 0; }
    done
    return 1
}

binary_identity() {
    # Which binary produced a number matters as much as the number.  This
    # repository keeps a dozen experimental builds in target/ at once.
    bin=$1
    if command -v sha256sum >/dev/null 2>&1; then
        sha=$(sha256sum "$bin" | cut -c1-16)
    else
        sha="nosha"
    fi
    size=$(wc -c < "$bin" | tr -d ' ')
    printf '%s sha=%s size=%s' "$bin" "$sha" "$size"
}

cmd_help() {
    cat <<'EOF'
usage: tools/ai/nelisp-ai.sh <command> [args]

  status              regenerate target/ai/STATUS.{json,md} — read this first
  check               run the fast gates, then verify — the pre-commit command
  verify              aggregate gate reports and print one verdict
  test                run the full ERT suite as gate "ert-full"
  standalone          build the binary and run "standalone-reader-test",
                      "binary-size-ratchet" and "emacs-parity"
  native-artifact     build the binary and run gate "nelisp-native-artifact-gate"
  selfhost            build the binary and run gate "standalone-selfhost-test"
  perf                build the binary and run gate "nelisp-performance-gate"
  smokes              run the 28 individual reader smokes as one gate
  extras              run the remaining binary-building gates, one report each
  presence            build the binary and run gate "substrate-presence-sweep"
                      (~354-name fboundp sweep, own tier: too slow for extras)
  test-one FILE...    run selected test files as gate "ert-focus"
  compile             byte-compile with error-on-warn as gate "compile"
  ns [FILE...]        namespace check (defaults to the recipe skeletons)
  recipes             run every recipe smoke against the standalone binary
  runtime-probe       report what the standalone binary can actually do
  gate NAME -- CMD    run CMD and report it, reading its GATE-COUNT line
  probe EXPR          evaluate EXPR in the standalone runtime, output to files
  build-probe [-- CMD] build, then probe -- and refuse to probe if the build failed
  doctor              print the toolchain and binary identity of this checkout
  gates-clean         delete every gate report (they are per-machine)

Every command that checks something writes a report into target/gates/.
`verify' is the only command whose exit code you should trust for "is the
tree good", because it is the only one that also knows which gates are
missing entirely.
EOF
}

cmd_status() {
    "$EMACS" --batch -Q -l "$here/nelisp-status.el" -f nelisp-status-run
    printf '\n'
    cat target/ai/STATUS.md
}

cmd_check() {
    # The same set the CI fast lane runs, in one command, so that
    # "what will CI say" is answerable before pushing rather than after.
    #
    # The full ERT suite is deliberately NOT here: it takes minutes, and
    # `verify' already holds you to the last ert-full report and prints
    # its age.  Run `test' when the age column says the evidence is old.
    check_steps="compile parens-check generated-source-parse doc200-census \
prelude-toplevel-check partial-inventory gate-selfcheck version-consistency \
gate-mutation \
ns-gate nl-check-gate fallback-inventory-selftest \
fallback-inventory bootstrap-contract \
wasm-dtw-skeleton-smoke \
unsafe-inventory ns-inventory \
reader-surface-audit pkg-graph pkg-load-lists \
parity-coverage substrate-presence-corpus-check \
doc-claims ns"
    # NELISP_CHECK_SKIP lets CI hand a step to a job of its own rather than
    # drop it.  `gate-mutation' is the only user today: it is 1257 of the
    # Linux lane's 2784 seconds, in one serial stretch, and it now runs as
    # four parallel shards elsewhere.  A skipped step leaves no gate report,
    # so `verify' still counts it MISSING unless that other job's report is
    # merged in -- which is exactly the property that must not be lost here.
    for skip in ${NELISP_CHECK_SKIP:-}; do
        check_steps=$(printf '%s\n' $check_steps | grep -vx "$skip" | tr '\n' ' ')
        printf 'check: skipping %s (NELISP_CHECK_SKIP; it must run elsewhere)\n' "$skip"
    done
    failures=0
    for step in $check_steps; do
        printf '\n=== %s ===\n' "$step"
        case "$step" in
            compile) ( cmd_compile ) || failures=$((failures + 1)) ;;
            ns)      ( cmd_ns ) || failures=$((failures + 1)) ;;
            *)       ( cmd_gate "$step" -- make "$step" ) || failures=$((failures + 1)) ;;
        esac
    done
    # `check' only ran the steps above, so holding its trailing verify to
    # every gate in `gates.expected' -- including the binary-tier gates
    # `standalone'/`native-artifact'/`perf'/`extras'/`test' produce -- means
    # it fails on gates that have not had a chance to run yet, not on gates
    # this command actually exercised.  Measured on GitHub Actions run
    # 32604739757 (2026-08-23): every check-tier gate passed
    # (GATE-COUNT findings=0, gate-mutation 13/13) and the step still exited
    # nonzero, because the CI lane runs the binary-tier steps AFTER `check',
    # so their reports could not exist yet -- verify read that as
    # "ert-full: no report (the gate never ran)" etc. and failed the job
    # before it ever reached the steps that produce them.
    #
    # Scope the trailing verify to a manifest filtered to check's own gate
    # names.  Each kept line is copied verbatim (including a trailing `?')
    # from the real manifest, so a check-tier gate that IS missing or red
    # still fails verify -- only the gates outside this tier drop out of
    # scope, not the requiredness of the ones inside it.  Plain `verify',
    # run directly with no `NELISP_GATE_MANIFEST' override, is unaffected:
    # this only changes what `check' itself asks for at the end.
    scoped_manifest=$(check_tier_manifest "$check_steps")
    printf '\n=== verify (check-tier scope) ===\n'
    set +e
    NELISP_GATE_MANIFEST="$scoped_manifest" cmd_verify
    verify_code=$?
    set -e
    rm -f "$scoped_manifest"
    return "$verify_code"
}

check_tier_manifest() {
    # Build a `gates.expected'-shaped file containing only the gate names
    # `cmd_check' produces reports for, each copied verbatim (trailing `?'
    # and all) from the real manifest -- see `cmd_check' above for why.
    # Reads from `NELISP_GATE_MANIFEST' when the caller already pointed it
    # somewhere else, so a caller's own manifest stays the source of truth
    # for which of check's gates are required vs. optional.
    #
    # `ns' is the one step whose reported gate name differs from the step
    # name: `cmd_ns' called with no FILE arguments (as `check' calls it)
    # reports as `ns-recipes', not `ns' -- see `cmd_ns' below.
    #
    # A step name with no matching manifest line is not silently dropped:
    # that is `check' and `gates.expected' having drifted apart, the exact
    # shape of silent failure this whole harness exists to catch, so it is
    # a loud error here instead.
    steps=$1
    src=${NELISP_GATE_MANIFEST:-$here/gates.expected}
    out=$(mktemp)
    missing=""
    for s in $steps; do
        case "$s" in
            ns) name=ns-recipes ;;
            *)  name=$s ;;
        esac
        line=$(grep -E "^${name}\\??\$" "$src" | head -1 || true)
        if [ -z "$line" ]; then
            missing="$missing $name"
        else
            printf '%s\n' "$line" >> "$out"
        fi
    done
    if [ -n "$missing" ]; then
        rm -f "$out"
        printf 'nelisp-ai.sh check: gate(s)%s run by `check'"'"' are not in %s -- the check step list and gates.expected have drifted\n' "$missing" "$src" >&2
        exit 1
    fi
    printf '%s' "$out"
}

cmd_verify() {
    "$EMACS" --batch -Q -l "$here/nelisp-verify.el" -f nelisp-verify-run
}

run_ert_gate() {
    gate_name=$1
    shift
    files=$*
    if [ -z "$(printf '%s' "$files" | tr -d ' \n')" ]; then
        "$here/gate-report.sh" --name "$gate_name" --kind ert --ran 0 \
            --failed 1 --reason "no test file matched; nothing was run" \
            --command "nelisp-ai.sh $gate_name"
        exit 1
    fi
    loads=""
    for f in $files; do loads="$loads -l $f"; done
    started=$(date +%s)
    set +e
    # shellcheck disable=SC2046,SC2086
    NELISP_GATE_NAME="$gate_name" \
    NELISP_GATE_COMMAND="tools/ai/nelisp-ai.sh $gate_name" \
        "$EMACS" --batch -Q $(load_path_args) \
            --eval '(setq load-prefer-newer t)' \
            -l ert $loads \
            -l "$here/nelisp-ert-gate.el" -f nelisp-ert-gate-run
    code=$?
    set -e
    printf 'elapsed: %ss\n' "$(( $(date +%s) - started ))"
    return $code
}

cmd_test() {
    run_ert_gate ert-full "$(test_files)"
}

cmd_native_artifact() {
    # The native artifact contract: bulk .neln builds, `--native-policy
    # required' proving every top-level defun entered native code, the audit
    # reporting coverage gaps, and the load/eval/exec commands preferring the
    # adjacent artifact.  Builds a binary, so it is out of `check' alongside
    # `standalone'.
    cmd_gate nelisp-native-artifact-gate -- make nelisp-native-artifact-gate
}

cmd_selfhost() {
    # The standalone interpreter loading its own compiler toolchain as source
    # and emitting a native ELF, with no Emacs in the loop.  Builds a binary,
    # so it is out of `check' with the other standalone gates.
    cmd_gate standalone-selfhost-test -- make standalone-selfhost-test
}

cmd_perf() {
    # Artifact runtime cache vs source fallback: both paths must load and eval
    # the same module, and the build must restore the cache enable marker it
    # removed.  Builds a binary; out of `check'.
    #
    # Doc 187's checked-arithmetic AOT comparison belongs in this perf tier:
    # ten independent compile + five-round measurements took 9.4s total on
    # the wiring host (~0.94s each), well inside this tier's cost, and it is a
    # performance ceiling rather than one of `extras'' subsystem smokes.
    failures=0
    cmd_gate nelisp-performance-gate -- make nelisp-performance-gate \
        || failures=$((failures + 1))
    cmd_gate bench-aot-checked-arith -- make bench-aot-checked-arith \
        || failures=$((failures + 1))
    [ "$failures" -eq 0 ]
}

cmd_smokes() {
    # The 28 `standalone-reader-*' targets, as one gate.  They test things
    # `standalone-reader-test' does not -- format directives, getenv, TLS,
    # processes, match-data, intern-soft, the FFI bridge -- and until the
    # 2026-08-21 sweep nothing ran them.
    cmd_gate standalone-reader-smokes -- make standalone-reader-smokes
}

cmd_presence() {
    # Task A: the definable-name surface (~354 names), one `fboundp' probe
    # each, across every substrate.  Its own command rather than folded
    # into `extras': measured at ~2400 process launches (vs. ~250 for
    # `substrate-parity-smoke'), too slow for that tier's per-gate budget
    # -- see tools/ai/gates.expected's substrate-presence-sweep entry for
    # the measurement.  Builds a binary, so it is out of `check' with the
    # other standalone gates.
    cmd_gate substrate-presence-sweep -- make substrate-presence-sweep
}

cmd_extras() {
    # The green gates that build a binary and were not wired.  One report each,
    # because unlike the reader smokes these are independent gates over
    # different subsystems: collapsing them would hide which one went red.
    failures=0
    for g in nelisp-runtime-image-cache-gate \
             nelisp-source-command-substrate-gate \
             standalone-chunk-growth-test \
             standalone-midform-gc-bounded \
             standalone-parallel-compile-test \
             standalone-selfhost-mt-test \
             standalone-eval-test \
             wasm-runtime-image-smoke \
             nelisp-nelix-native-hot-gate \
             substrate-parity-smoke \
             nl-condition-standalone-smoke \
             nl-safe-standalone-smoke \
             nl-resource-standalone-smoke \
             nl-num-standalone-smoke \
             nelisp-thread-standalone-smoke \
             nelisp-thread-allocating-standalone-smoke \
             nelisp-thread-mirror-guard-standalone-smoke \
             nelisp-thread-percpu-roots-smoke \
             standalone-reader-buffer-smoke \
             precise-root-coverage \
             nl-actor-standalone-smoke; do
        printf '\n=== %s ===\n' "$g"
        ( cmd_gate "$g" -- make "$g" ) || failures=$((failures + 1))
    done
    [ "$failures" -eq 0 ]
}

cmd_standalone() {
    # Not in `check' for the same reason `test' is not: it builds a binary
    # from the tree, which is minutes rather than seconds.  `verify' holds
    # you to the last report and prints its age, so this is the command to
    # run when that column says the evidence is old.  On a host that cannot
    # run the target it reports a reasoned skip, not a pass.
    failures=0
    cmd_gate standalone-reader-test -- make standalone-reader-test \
        || failures=$((failures + 1))
    # binary-size-ratchet consumes the binary standalone-reader-test just
    # built (its own prerequisite is conditional -- it does not build
    # again).  Wired here rather than as its own command: every other
    # binary-tier gate already has a `cmd_X' of its own, but this one has
    # nothing to measure without a fresh build having just happened, and
    # `nelisp-ai.sh standalone' is the command that makes that happen.
    cmd_gate binary-size-ratchet -- make binary-size-ratchet \
        || failures=$((failures + 1))
    # emacs-parity: differential vs the host Emacs already on this
    # machine, over `test/nelisp-shadow-differential-cases.el'.
    # Mutation-proven (tools/gate-mutations.txt) since 2026-08-19 but
    # never wired into `nelisp-ai.sh' at all until this pass -- it needs
    # the same binary this command already built, and its own Makefile
    # prerequisite is the identical conditional-build check
    # `binary-size-ratchet' uses, so it costs nothing extra here.
    cmd_gate emacs-parity -- make emacs-parity \
        || failures=$((failures + 1))
    [ "$failures" -eq 0 ]
}

cmd_test_one() {
    [ $# -gt 0 ] || { echo "usage: nelisp-ai.sh test-one test/nelisp-FOO-test.el" >&2; exit 2; }
    run_ert_gate ert-focus "$*"
}

cmd_compile() {
    # Mirrors the root Makefile's SRCS + PACKAGE_SRCS, including the
    # nl-* packages.  Keep the two in step: a glob that quietly stops
    # matching is the same failure this harness exists to catch, and it
    # is easiest to introduce right here.
    srcs=$(ls src/nelisp*.el packages/*/src/nelisp*.el \
              packages/*/src/nl-*.el 2>/dev/null || true)
    if [ -z "$srcs" ]; then
        "$here/gate-report.sh" --name compile --kind lint --ran 0 --failed 1 \
            --reason "no source file matched" --command "nelisp-ai.sh compile"
        exit 1
    fi
    started=$(date +%s)
    set +e
    # shellcheck disable=SC2046,SC2086
    "$EMACS" --batch -Q -L src $(load_path_args) \
        --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile $srcs
    code=$?
    set -e
    produced=$(ls src/nelisp*.elc packages/*/src/nelisp*.elc \
                  packages/*/src/nl-*.elc 2>/dev/null | wc -l | tr -d ' ')
    # Byte-compiled artifacts are removed again unless asked for.  This
    # is a lint check, and the root Makefile's `test' target deletes
    # them before every run for a documented reason: byte-compiled
    # `nelisp-eval.el' adds host stack frames that trip
    # `max-lisp-eval-depth' inside the self-host probes.  Leaving .elc
    # behind would make a later test run depend on whether someone had
    # linted first.  NELISP_KEEP_ELC=1 opts out.
    if [ -z "${NELISP_KEEP_ELC:-}" ]; then
        rm -f src/nelisp*.elc packages/*/src/nelisp*.elc packages/*/src/nl-*.elc
        printf 'removed %s .elc (set NELISP_KEEP_ELC=1 to keep them)\n' "$produced"
    fi
    "$here/gate-report.sh" --name compile --kind lint \
        --ran "$produced" --passed "$produced" \
        --failed "$([ $code -eq 0 ] && echo 0 || echo 1)" \
        --duration-ms "$(( ($(date +%s) - started) * 1000 ))" \
        --command "nelisp-ai.sh compile"
}

cmd_recipes() {
    # The recipes are the answer to "can I build something on this?", so
    # they are the surface most expensive to let rot: nobody notices a
    # stale recipe until they follow it.  One command runs them all.
    #
    # Each smoke reports its own gate and skips with a reason when the
    # binary is missing, so this loop does not need to know which shapes
    # are viable on this host.
    found=0
    failures=0
    for script in recipes/*/verify.sh; do
        [ -f "$script" ] || continue
        found=$((found + 1))
        printf '\n=== %s ===\n' "$(dirname "$script" | sed 's|recipes/||')"
        ( sh "$script" ) || failures=$((failures + 1))
    done
    if [ "$found" -eq 0 ]; then
        printf 'no recipe smoke found under recipes/*/verify.sh\n' >&2
        exit 1
    fi
    printf '\n%d recipe(s), %d failing\n' "$found" "$failures"
    [ "$failures" -eq 0 ]
}

cmd_runtime_probe() {
    # Asks the binary what it can do, instead of consulting a table
    # somebody wrote down once.  The recipes used to carry these facts by
    # hand and two of them were already wrong.
    gate_name=${NELISP_GATE_NAME:-runtime-probe}
    if ! bin=$(nelisp_binary); then
        "$here/gate-report.sh" --name "$gate_name" --kind probe             --reason "no nelisp binary in target/; build one or set NELISP_BIN"             --command "nelisp-ai.sh runtime-probe"
        return 0
    fi
    mkdir -p target/ai
    log=$(mktemp)
    set +e
    "$bin" --load tools/ai/runtime-probe.el > "$log" 2>&1
    code=$?
    set -e
    # The identity goes into the OUTPUT, not only into the report.  A probe
    # table is read as a transcript -- pasted into a message, scrolled back
    # to, compared against an earlier run -- and on its own it cannot say
    # which binary produced it.  Measured 2026-08-19: a build failed, the
    # next command probed the binary that was already in target/, and the
    # table it printed was indistinguishable from a passing run.  That
    # happened three times in one day, once immediately after building the
    # command meant to prevent it, because `build-probe' can only guard
    # measurements it starts.  A line that travels with the output guards
    # every reader of it.
    printf 'PROBE-BINARY %s\n' "$(binary_identity "$bin")"
    grep -E '^PROBE ' "$log" || true
    checked=$(grep -cE '^PROBE ' "$log" || true)
    rm -f "$log"
    if [ "$code" -ne 0 ] || [ "${checked:-0}" -eq 0 ]; then
        "$here/gate-report.sh" --name "$gate_name" --kind probe             --ran "${checked:-0}" --failed 1             --reason "the probe did not finish (exit $code)"             --command "nelisp-ai.sh runtime-probe"
        exit 1
    fi
    "$here/gate-report.sh" --name "$gate_name" --kind probe         --ran "$checked" --passed "$checked" --failed 0         --command "nelisp-ai.sh runtime-probe ($(binary_identity "$bin"))"
}

cmd_gate() {
    # Wrap a command that already knows what it checked.  The command
    # must print a line of the form
    #
    #   GATE-COUNT checked=<files> findings=<n>
    #
    # and its absence is itself a failure: a gate that will not say what
    # it examined cannot be believed when it says nothing is wrong.
    # `make parens-check' prints nothing at all on a clean run, so an
    # empty file list and a clean tree looked identical from outside.
    #
    # A gate that cannot run on this host prints instead
    #
    #   GATE-SKIP <reason>
    #
    # which is recorded as a reasoned skip.  The standalone gates used to
    # exit 0 on the wrong platform, which reads exactly like a pass.
    [ $# -ge 3 ] || { echo 'usage: nelisp-ai.sh gate NAME -- COMMAND...' >&2; exit 2; }
    gate_name=$1
    shift
    [ "$1" = "--" ] || { echo 'nelisp-ai.sh gate: expected -- after NAME' >&2; exit 2; }
    shift
    log=$(mktemp)
    started=$(date +%s)
    set +e
    "$@" > "$log" 2>&1
    code=$?
    set -e
    cat "$log"
    skip=$(grep -E '^GATE-SKIP ' "$log" | tail -1 | sed 's/^GATE-SKIP //' || true)
    if [ -n "$skip" ]; then
        duration=$(( ($(date +%s) - started) * 1000 ))
        rm -f "$log"
        "$here/gate-report.sh" --name "$gate_name" --kind wrapped \
            --duration-ms "$duration" --reason "$skip" --command "$*"
        return 0
    fi
    line=$(grep -E '^GATE-COUNT ' "$log" | tail -1 || true)
    checked=$(printf '%s' "$line" | sed -n 's/.*checked=\([0-9][0-9]*\).*/\1/p')
    findings=$(printf '%s' "$line" | sed -n 's/.*findings=\([0-9][0-9]*\).*/\1/p')
    # The command's own verdict line, when it has one.  A wrapped gate
    # that reports only a count says "1704 findings" where the tool
    # itself said which kind went over its baseline -- keep that half.
    detail=$(grep -E 'FAIL' "$log" | tail -1 | cut -c1-200 || true)
    duration=$(( ($(date +%s) - started) * 1000 ))
    rm -f "$log"
    if [ -z "$checked" ]; then
        "$here/gate-report.sh" --name "$gate_name" --kind wrapped --ran 0 \
            --failed 1 --duration-ms "$duration" \
            --reason "no GATE-COUNT line; the command did not report what it checked (exit $code)" \
            --command "$*"
        exit 1
    fi
    if [ "$code" -eq 0 ]; then
        "$here/gate-report.sh" --name "$gate_name" --kind wrapped \
            --ran "$checked" --passed "$checked" --failed 0 \
            --duration-ms "$duration" --command "$*"
    else
        "$here/gate-report.sh" --name "$gate_name" --kind wrapped \
            --ran "$checked" --passed 0 --failed 1 \
            --duration-ms "$duration" \
            --reason "${detail:-exit $code with ${findings:-?} finding(s)}" \
            --command "$*"
    fi
}

cmd_ns() {
    # Elisp has one obarray, so a second definition of a name silently
    # wins.  `nl-ns' reports the boundaries the language cannot enforce.
    # Whole-tree checking is `make ns-inventory', which ratchets against
    # a baseline; this is for a small set that should simply be clean.
    if [ $# -gt 0 ]; then
        files=$*
        gate_name=${NELISP_GATE_NAME:-ns}
    else
        files=$(ls recipes/*/skeleton/*.el 2>/dev/null || true)
        gate_name=${NELISP_GATE_NAME:-ns-recipes}
    fi
    if [ -z "$(printf '%s' "$files" | tr -d ' \n')" ]; then
        "$here/gate-report.sh" --name "$gate_name" --kind ns --ran 0 \
            --failed 1 --reason "no file matched; nothing was checked" \
            --command "nelisp-ai.sh ns"
        exit 1
    fi
    # shellcheck disable=SC2086
    NELISP_GATE_NAME="$gate_name" \
        "$EMACS" --batch -Q -L packages/nl-prelude/src -L packages/nl-ns/src \
            -l "$here/nelisp-ns-check.el" -f nelisp-ns-check-run $files
}

cmd_probe() {
    [ $# -gt 0 ] || { echo "usage: nelisp-ai.sh probe '(+ 40 2)'" >&2; exit 2; }
    expr=$1
    bin=$(nelisp_binary) || { echo "no nelisp binary in target/; set NELISP_BIN" >&2; exit 2; }
    dir="target/ai/probe/$(date +%Y%m%d-%H%M%S)-$$"
    mkdir -p "$dir"
    printf '%s' "$expr" > "$dir/expr.el"
    set +e
    "$bin" --eval "$expr" > "$dir/stdout.txt" 2> "$dir/stderr.txt"
    code=$?
    set -e
    {
        printf 'expr_file: %s\n' "$dir/expr.el"
        printf 'exit: %s\n' "$code"
        printf 'binary: %s\n' "$(binary_identity "$bin")"
        printf 'finished: %s\n' "$(date +%Y-%m-%dT%H:%M:%S%z)"
    } > "$dir/meta.txt"
    # Only the path goes to stdout.  Reading the value out of a terminal
    # transcript is how `tail -1' ends up returning the shell's own echo,
    # and how oversized frames get truncated on the way back.
    printf '%s\n' "$dir"
}

cmd_build_probe() {
    # Build, then measure -- and refuse to measure when the build failed.
    #
    # The event this prevents, measured 2026-08-19: a reader build exited
    # 255 and the next steps in the same shell command ran the binary that
    # was already there, printing a clean `42' and a full probe table.
    # Read without the exit code, that is indistinguishable from "built
    # fine, nothing changed" -- a failure wearing a pass, which is the
    # same shape as a gate that exits 0 without running.
    #
    # Noticing it took a habit (print BUILD_EXIT first), and a habit is a
    # memory dependency.  This is the same rule as an exit code, moved
    # somewhere it cannot be skipped.
    #
    # Everything after `--' replaces the default build command.
    if [ "${1:-}" = "--" ]; then shift; fi
    [ $# -gt 0 ] || set -- make standalone-reader
    printf '=== build: %s ===\n' "$*"
    set +e
    "$@"
    code=$?
    set -e
    if [ "$code" -ne 0 ]; then
        printf '\n=== NOT MEASURING ===\n'
        printf 'build failed (exit %s).  Any measurement now would run the\n' "$code"
        printf 'binary that was already in target/, and report it as this\n'
        printf "build's result.\n"
        # `|| true': a failing gate report must not become the exit
        # status.  Without it `set -e' ends the script here and the
        # build's own code never propagates -- a build that died 255
        # reported 1, which is the report's status wearing the build's
        # name.
        "$here/gate-report.sh" --name build-probe --kind smoke \
            --ran 1 --passed 0 --failed 1 \
            --reason "build failed (exit $code); no measurement taken" \
            --command "nelisp-ai.sh build-probe -- $*" || true
        exit "$code"
    fi
    if ! bin=$(nelisp_binary); then
        printf '\n=== NOT MEASURING ===\n'
        printf 'build reported success but left no binary in target/.\n'
        "$here/gate-report.sh" --name build-probe --kind smoke \
            --ran 1 --passed 0 --failed 1 \
            --reason "build exited 0 but produced no binary" \
            --command "nelisp-ai.sh build-probe -- $*" || true
        exit 1
    fi
    # Which binary produced the numbers is part of the numbers.
    printf '\n=== measuring: %s ===\n' "$(binary_identity "$bin")"
    NELISP_GATE_NAME=build-probe cmd_runtime_probe
}

cmd_doctor() {
    printf 'repo:        %s\n' "$root"
    printf 'branch:      %s\n' "$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo '?')"
    printf 'head:        %s\n' "$(git rev-parse --short HEAD 2>/dev/null || echo '?')"
    printf 'worktrees:   %s\n' "$(git worktree list 2>/dev/null | wc -l | tr -d ' ')"
    printf 'emacs:       %s\n' "$("$EMACS" --version 2>&1 | head -1)"
    printf 'make:        %s\n' "$(make --version 2>&1 | head -1)"
    if bin=$(nelisp_binary); then
        printf 'nelisp:      %s\n' "$(binary_identity "$bin")"
        printf 'nelisp mtime:%s\n' " $(date -r "$bin" +%Y-%m-%dT%H:%M:%S 2>/dev/null || echo '?')"
    else
        printf 'nelisp:      (none in target/)\n'
    fi
    printf 'gate dir:    %s (%s report(s))\n' "$GATE_DIR" \
        "$(ls "$GATE_DIR"/*.json 2>/dev/null | wc -l | tr -d ' ')"
}

cmd_gates_clean() {
    rm -f "$GATE_DIR"/*.json
    printf 'cleared %s\n' "$GATE_DIR"
}

command=${1:-help}
[ $# -gt 0 ] && shift || true
case "$command" in
    help|-h|--help) cmd_help ;;
    status)         cmd_status ;;
    check)          cmd_check ;;
    verify)         cmd_verify ;;
    test)           cmd_test ;;
    standalone)     cmd_standalone ;;
    native-artifact) cmd_native_artifact ;;
    selfhost)       cmd_selfhost ;;
    perf)           cmd_perf ;;
    smokes)         cmd_smokes ;;
    extras)         cmd_extras ;;
    presence)       cmd_presence ;;
    test-one)       cmd_test_one "$@" ;;
    compile)        cmd_compile ;;
    gate)           cmd_gate "$@" ;;
    ns)             cmd_ns "$@" ;;
    recipes)        cmd_recipes ;;
    runtime-probe)  cmd_runtime_probe ;;
    probe)          cmd_probe "$@" ;;
    build-probe)    cmd_build_probe "$@" ;;
    doctor)         cmd_doctor ;;
    gates-clean)    cmd_gates_clean ;;
    *)              printf 'unknown command: %s\n\n' "$command" >&2; cmd_help >&2; exit 2 ;;
esac
