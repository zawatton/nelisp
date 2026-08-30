#!/usr/bin/env bash
# Run the standalone-reader smoke targets, several at a time.
#
# Why parallel.  Measured on the ubuntu/30.1 lane of run 32931012524,
# `nelisp-ai.sh smokes' took 1269 seconds -- 21.2 of that lane's 60.6
# minutes, and the single largest step once gate-mutation is narrowed.
#
# They all read ./target/nelisp, and most of them rebuild it first.  The
# `standalone-reader' rule has no prerequisites and its target name is not
# the file it produces, so make can never see it as up to date: every
# invocation runs the full build.  Measured here, one smoke takes 26
# seconds of which 25 are that rebuild -- so the sequential gate spent the
# bulk of its 1269 seconds building the same binary 28 times over, from
# sources that do not change while it runs.
#
# Build it once, then pass `-o standalone-reader' so no worker rebuilds
# it (26s -> 1s per smoke, measured).  That is also what makes the
# parallel run safe: with every worker building, they collided on the
# output file --
#
#   Opening output file: Text file busy, .../target/nelisp
#   make[1]: *** [Makefile:421: standalone-reader] Error 255
#
# -- 13 of 28 targets, twice, and a serial pre-build did NOT fix it
# because the rule reruns regardless of what is already on disk.
#
# The mutation harness is unaffected: it invokes the individual smoke
# targets itself and relies on their rebuild, and never goes through here.
#
# Output is deliberately NOT interleaved: each target's log is captured
# whole and only printed if that target fails.  A parallel run that
# shuffles 28 streams together is unreadable exactly when you need to
# read it.
set -u
cd "$(dirname "$0")/.." || exit 1

jobs="${NELISP_SMOKE_JOBS:-}"
if [ -z "$jobs" ]; then
  cpus="$( (nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 2) )"
  jobs=$(( cpus > 4 ? 4 : cpus ))
  [ "$jobs" -lt 1 ] && jobs=1
fi

MAKE_BIN="${MAKE:-make}"
mkdir -p target/tmp
status_dir="$(mktemp -d)"
trap 'rm -rf "$status_dir"' EXIT

run_one() {
  local t="$1" log="target/tmp/nelisp-smoke-$1.log"
  if "$MAKE_BIN" --no-print-directory -o standalone-reader "$t" > "$log" 2>&1; then
    : > "$status_dir/$t.ok"
  else
    # Keep the tail with the marker: the aggregator prints it later, in
    # target order, instead of whenever this worker happened to finish.
    tail -3 "$log" > "$status_dir/$t.fail"
  fi
  rm -f "$log"
}
export -f run_one
export status_dir MAKE_BIN

# Three of these targets do NOT just read the binary: they rebuild it
# themselves, each with a different configuration --
# `standalone-reader-ffi-smoke' dynamic, `standalone-reader-tls-smoke'
# dynamic, `standalone-reader-ffi-unsupported-smoke' explicitly static
# (env -u NELISP_READER_DYNAMIC).  Each needs ./target/nelisp to BE its
# own variant while it runs, so they cannot share it with anything,
# including each other.  Measured: with all 41 in parallel these three
# were the only failures, `Text file busy' on the binary, while the other
# 38 passed against the default build.  They run serially, after the
# fan-out, so nobody is executing the binary while one of them replaces it.
SERIAL_SMOKES="standalone-reader-ffi-smoke standalone-reader-ffi-unsupported-smoke standalone-reader-tls-smoke"

# That list is a copy of a fact about the Makefile, so check it still
# holds.  If a new smoke starts building the reader and is not listed, it
# joins the parallel group and reintroduces exactly the race this split
# exists to remove -- intermittently, which is the worst way to find out.
detected=""
for t in "$@"; do
  if awk "/^$t:/,/^\$/" Makefile 2>/dev/null | grep -q 'nelisp-standalone-build-reader'; then
    detected="$detected $t"
  fi
done
for t in $detected; do
  case " $SERIAL_SMOKES " in
    *" $t "*) ;;
    *) echo "[reader-smokes] FAIL: $t rebuilds the reader but is not in SERIAL_SMOKES"
       echo "GATE-COUNT checked=0 findings=1"; exit 1 ;;
  esac
done

par_list=""
for t in "$@"; do
  case " $SERIAL_SMOKES " in
    *" $t "*) ;;
    *) par_list="$par_list $t" ;;
  esac
done

# Build the binary once, before the fan-out.  A failure here is a build
# error, not a smoke result, and must not be reported as 28 failing smokes.
if ! "$MAKE_BIN" --no-print-directory standalone-reader > target/tmp/reader-build.log 2>&1; then
  echo "[reader-smokes] FAIL: standalone-reader did not build"
  tail -5 target/tmp/reader-build.log | sed 's/^/    /'
  echo "GATE-COUNT checked=0 findings=1"
  exit 1
fi

printf '%s\n' $par_list | xargs -P "$jobs" -I{} bash -c 'run_one "$@"' _ {}

# Serial tail: these replace ./target/nelisp, so nothing else may be
# running.  They rebuild it themselves -- no -o here.
for t in $SERIAL_SMOKES; do
  case " $* " in *" $t "*) ;; *) continue ;; esac
  log="target/tmp/nelisp-smoke-$t.log"
  if "$MAKE_BIN" --no-print-directory "$t" > "$log" 2>&1; then
    : > "$status_dir/$t.ok"
  else
    tail -3 "$log" > "$status_dir/$t.fail"
  fi
  rm -f "$log"
done

ran=0; failed=0; failed_names=""
for t in "$@"; do
  if [ -e "$status_dir/$t.ok" ]; then
    ran=$((ran + 1))
  elif [ -e "$status_dir/$t.fail" ]; then
    ran=$((ran + 1)); failed=$((failed + 1)); failed_names="$failed_names $t"
    echo "[reader-smokes] FAIL: $t"
    sed 's/^/    /' "$status_dir/$t.fail"
  else
    # Neither marker: the worker died before it could write one (OOM, a
    # kill, xargs giving up).  That is not a pass, and counting it as one
    # would be the "gate that never ran read as green" failure this
    # repository has already been bitten by.
    ran=$((ran + 1)); failed=$((failed + 1)); failed_names="$failed_names $t(no-result)"
    echo "[reader-smokes] FAIL: $t produced no result (worker died before reporting)"
  fi
done

echo "GATE-COUNT checked=$ran findings=$failed"
if [ "$failed" -eq 0 ]; then
  echo "[reader-smokes] PASS: $ran smokes (-P $jobs)"
else
  echo "[reader-smokes] FAIL:$failed_names"
  exit 1
fi
