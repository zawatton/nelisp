#!/bin/sh
# S1.2: adopted count rises by >= 200 over the seed build (61 -> report), buffer opcodes admitted to both lists, every generated fixture re-run on the built binary.
cd "$(dirname "$0")/../../.." || exit 1
NB=${NOTES_BIN:-$(d=$PWD; while [ "$d" != / ]; do [ -x "$d/bin/heavy-run.sh" ] && { echo "$d/bin"; break; }; d=$(dirname "$d"); done)}
python3 - <<'PY' || exit 1
import re, json
rep = [l.rstrip('\n').split('\t') for l in open('target/bcadopt-build/dynamic/src/target/nelisp-prelude-bytecode-report.tsv') if not l.startswith('#')][1:]
base = [l.rstrip('\n').split('\t') for l in open('../resume-4943-20261024/target/integ-1033-build/dynamic/src/target/nelisp-prelude-bytecode-report.tsv') if not l.startswith('#')][1:]
a = sum(r[3] == 'adopt' for r in rep); b = sum(r[3] == 'adopt' for r in base)
assert a - b >= 200, (a, b)
vm = set(map(int, re.search(r"nelisp-standalone--prelude-bytecode-opcodes\n  '\(([^)]*)\)", open('scripts/nelisp-standalone-build.el').read()).group(1).split()))
sel = set(map(int, re.search(r"nelisp-prelude-bytecode--opcodes\n  '\(([^)]*)\)", open('scripts/nelisp-prelude-bytecode.el').read()).group(1).split()))
for o in (96, 98, 100, 101, 114, 123, 164): assert o in vm and o in sel, o
assert not ({112, 113} & (vm | sel))
assert not (sel - vm)
used = set()
for r in rep:
    if r[3] == 'adopt': used |= set(map(int, re.findall(r'\d+', r[6])))
assert not (used - vm), used - vm
v = json.load(open('target/bcadopt/verify2.json')); bad = {k for k, x in v.items() if x != 'ok'}
ex = set(json.load(open('target/bcadopt/work/exclude2.json')))
assert bad == ex and len(v) - len(bad) >= 440, (bad, len(v))
print('adopted', a, 'baseline', b, 'delta', a - b, 'verified-on-binary', len(v) - len(bad))
PY
V16=$PWD/../resume-4943-20261024/target/integ-1033-build/dynamic/nelisp-dynamic; B=$PWD/target/bcadopt-build/dynamic/nelisp-dynamic
(ulimit -v 8388608; $NB/nelisp-bytecode-adoption-check.sh --repo $PWD --report $PWD/target/bcadopt-build/dynamic/src/target/nelisp-prelude-bytecode-report.tsv --floor 509 --baseline-bin $V16 --candidate-bin $B --smoke $PWD/test/bcadopt-opcodes-smoke.el --baseline-must-match 'redefined nconc|-99') > target/bcadopt/logs/adoption-check.log 2>&1
# The tool's "adopted rows use opcodes absent from the VM list" check sorts numerically but compares with comm (lexicographic):
# it prints "comm: file 1 is not in sorted order" and a false failure. The python check above is the authoritative subset proof.
grep -q 'comm: file 1 is not in sorted order' target/bcadopt/logs/adoption-check.log || grep -q 'RESULT: ' target/bcadopt/logs/adoption-check.log && true
fails=$(grep -c '^\[adoption\] FAIL' target/bcadopt/logs/adoption-check.log)
other=$(grep '^\[adoption\] FAIL' target/bcadopt/logs/adoption-check.log | grep -vc 'adopted rows use opcodes absent')
[ "$other" = 0 ] || { echo "adoption check failed beyond the comm defect"; exit 1; }
grep -q 'PASS  rebinding smoke: red on the baseline binary, green on the candidate' target/bcadopt/logs/adoption-check.log || exit 1
grep -q 'PASS  adoption floor' target/bcadopt/logs/adoption-check.log || exit 1
echo "S1.2 ok (adoption-check fails=$fails, only the comm defect)"
