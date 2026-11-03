#!/bin/bash
# Re-verifies S3 guard check from saved outputs. Exit 0 iff all hold. Optional arg: dir (default: this dir).
d=${1:-$(cd "$(dirname "$0")" && pwd)}; fail=0
f(){ echo "FAIL: $*"; fail=1; }
g=$d/out/valid.guard
[ "$(cat $d/valid.rc)" = 0 ] || f "valid rc"
grep -q 'route=named-fx-default' $d/valid.out || f "route"
grep -q '^BENCH-CHECK n=1048576 result=1048576:' $d/valid.out || f "BENCH-CHECK"
[ ! -s $d/valid.err ] || f "stderr non-empty"
grep -q '^(guard-summary overall pass broken nil wrong nil' $g || f "overall not pass"
for p in "fset pass n 64 count 64" "restore pass n 64 count 0" "restore-cell-eq pass" "advice pass n 64 count 64" "advice-removed pass n 64 count 0" "error pass k 10 count 10" "arity pass count 0 res (err wrong-number-of-arguments)" "final-restore pass n 64 count 0"; do
  grep -qF "($p" $g || f "phase missing: $p"; done
grep -q 'log-ok nil\|result-ok nil' $g && f "log/result mismatch recorded"
# GNU control must agree phase-for-phase
python3 - "$g" "$d/gnu-ctl.out" <<'PY' || f "GNU/NeLisp phase mismatch"
import re,sys
a=open(sys.argv[1]).read(); b=open(sys.argv[2]).read()
def heads(s): return re.findall(r'\((fset|restore|restore-cell-eq|advice|advice-removed|error|arity|final-restore) (pass|fail)', s)
ha,hb=heads(a),heads(b)
sys.exit(0 if ha==hb and len(ha)==8 and all(x[1]=='pass' for x in ha) else 1)
PY
# negative controls
[ "$(cat $d/neg-broken.rc)" != 0 ] && ! grep -q BENCH-CHECK $d/neg-broken.out && [ ! -e $d/out/neg-broken.guard ] || f "neg-broken accepted"
grep -q 'overall fail' $d/out/neg-wrong.guard && ! grep -q 'overall pass' $d/out/neg-wrong.guard || f "neg-wrong did not fail"
[ $fail = 0 ] && echo "S3 GUARD PASS" || echo "S3 GUARD FAIL"; exit $fail
