#!/bin/sh
# S2.6: 7 fresh-process sub-phased startup runs on build13, every run passes the phase checker,
# and the recomputed summary equals the recorded one.
cd "$(dirname "$0")" || exit 1
n=0
for w in phase-out/*.wrapper.json; do
  id=$(basename "$w" .wrapper.json)
  python3 -I check_phase_events.py --wrapper "$w" --driver "phase-out/$id.events.sexp" >/dev/null || exit 1
  grep -q PREP_INSTALL_DONE "phase-out/$id.events.sexp" || exit 1
  n=$((n+1))
done
test "$n" -eq 7 || exit 1
mkdir -p .tmp && ln -sfn ../phase-out .tmp/phase-out && python3 -I summarize.py .tmp > .tmp/s.txt && cmp -s .tmp/s.txt summary.txt; r=$?; rm -rf .tmp; test $r -eq 0 || exit 1
echo PASS
