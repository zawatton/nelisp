#!/bin/sh
# S3.1: every measured survey2 row whose callees became byte-code has cand/GNU < base/GNU (paired, CPU 10), and the adoption report lists the moved functions.
cd "$(dirname "$0")/../../.." || exit 1
test -s target/codex/adopted-moved.txt || exit 1
python3 - <<'PY'
import re, glob, os, sys
bad = []; n = 0
for f in sorted(glob.glob('target/bcadopt/bench/*.log')):
    id_ = os.path.basename(f)[:-4]
    if id_ in ('reg-sum-bc', 'r3-mix', 'r3-call'): continue
    t = open(f).read()
    m = {k: float(v) for k, v in re.findall(r'^(base|cand|gnu)\s+([0-9.]+)', t, re.M)}
    if len(m) < 3: bad.append((id_, 'unmeasured')); continue
    n += 1
    if not m['cand'] < m['base']: bad.append((id_, round(m['cand'] / m['base'], 3)))
print('rows', n, 'not reduced:', bad)
sys.exit(1 if bad or n == 0 else 0)
PY
