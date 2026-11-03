#!/bin/sh
# S2.7: (1) structural tests pin the escape-variable relaxation and the tick budget;
# (2) the 320-case print differential on build20 adds no divergence beyond build18's pre-existing set
#     (delegated to s2-printer/check.sh);
# (3) producer preflight median dropped from build13 (1642 ms) to build18 (7 fresh runs) by >= 50%.
cd "$(dirname "$0")" || exit 1
R=$(cd ../../../.. && pwd)
(cd "$R" && ulimit -v 8388608 && emacs --batch -Q -l test/nelisp-printer-fastpath-test.el -f ert-run-tests-batch < /dev/null 2>&1 | grep -q '^Ran 4 tests, 4 results as expected, 0 unexpected') || exit 1
sh ../s2-printer/check.sh >/dev/null || exit 1
python3 -I - <<'P' || exit 1
import glob, re, statistics, sys
v = []
for f in glob.glob('phase-out-b18/*.events.sexp'):
    e = {m.group(1): int(m.group(2)) for m in re.finditer(r"\(([A-Z_]+) (\d+) nil\)", open(f).read())}
    v.append((e['PREP_PREFLIGHT_DONE'] - e['PREP_PRODUCER_LOADED']) / 1e6)
b13 = float(re.search(r"^\s*([\d.]+) ms .*PREP_PRODUCER_LOADED -> PREP_PREFLIGHT_DONE", open('summary-b13.txt').read(), re.M).group(1))
m = statistics.median(v)
print(f"preflight b13 {b13} ms -> b18 {m:.1f} ms ({len(v)} runs)")
sys.exit(0 if len(v) == 7 and m <= 0.5 * b13 else 1)
P
echo PASS
