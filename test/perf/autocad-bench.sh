#!/bin/sh
# Run one autocad bench (test/perf/autocad-bench.el) on $AUTOCAD_PERF_BIN.
# Usage: AUTOCAD_PERF_BIN=path/to/nelisp test/perf/autocad-bench.sh NAME
# The fixture is regenerated into target/perf/ whenever the generator is
# newer than it.  Bounded: 120 s per bench, low priority.
cd "$(dirname "$0")/../.." || exit 2
: "${AUTOCAD_PERF_BIN:?set AUTOCAD_PERF_BIN to a built nelisp binary}"
fixture=target/perf/inkscape-fixture.svg
fixture4x=target/perf/inkscape-fixture-4x.svg
if [ ! -f "$fixture4x" ] || [ test/perf/gen-inkscape-svg.py -nt "$fixture4x" ]; then
  mkdir -p target/perf || exit 2
  python3 test/perf/gen-inkscape-svg.py "$fixture" >/dev/null || exit 2
  python3 test/perf/gen-inkscape-svg.py "$fixture4x" 8000 2200 >/dev/null || exit 2
fi
out=target/perf/bench-$1.out
BENCH="$1" AUTOCAD_FIXTURE="$fixture" AUTOCAD_FIXTURE_4X="$fixture4x" timeout 120 nice -n 10 \
  "$AUTOCAD_PERF_BIN" --load test/perf/autocad-bench.el >"$out" 2>&1
rc=$?
grep '^AUTOCAD-BENCH' "$out" || echo "AUTOCAD-BENCH $1 no result (rc=$rc)"
exit $rc
