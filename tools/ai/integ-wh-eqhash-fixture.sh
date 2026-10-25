#!/usr/bin/env bash
# integ-wh-eqhash-fixture.sh [BIN]: the hashtab line's 56-case GNU fixture (tools/ai/eqhash-semantics.el against its
# GNU 31.1 expected output) on the merged dynamic binary.  Prints "EQHASH-FIXTURE n/56" and the names of the cases
# that differ; exit 0 only when all match.  Measured: hashtab lane tip 41/56, F-final stage 39, gc-port stage 37,
# callpath stage 37 (the unrooted-weak cases wait on the conservative stack scan; scan off, debug switch 28, gives 39).
# EQHASH_FIXTURE_OUT selects the log directory; TIMEOUT and KILL_AFTER (same prefix) are positive seconds.
# NeLisp's normal CLI epilogue is one optional final "t" line, observed after (eqhash-complete 56).
set -u
[[ $# -le 1 ]] || { echo 'usage: integ-wh-eqhash-fixture.sh [BIN]' >&2; exit 2; }
cd "$(dirname "$0")/../.." || exit 2
for dependency in realpath timeout python3; do
    command -v "$dependency" >/dev/null || { echo "missing command: $dependency" >&2; exit 2; }
done
BIN=$(realpath -- "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
[[ -f "$BIN" && -x "$BIN" ]] || { echo "runtime is not executable: $BIN" >&2; exit 2; }
for input in tools/ai/eqhash-semantics.el tools/ai/eqhash-semantics.expected; do
    [[ -f "$input" && -r "$input" ]] || { echo "unreadable fixture input: $input" >&2; exit 2; }
done
LIMIT=${EQHASH_FIXTURE_TIMEOUT:-600}
KILL_AFTER=${EQHASH_FIXTURE_KILL_AFTER:-5}
for seconds in "$LIMIT" "$KILL_AFTER"; do
    [[ "$seconds" =~ ^[1-9][0-9]*$ ]] || { echo 'timeouts must be positive integer seconds' >&2; exit 2; }
done
OUT=${EQHASH_FIXTURE_OUT:-target/integ-wh}
mkdir -p -- "$OUT" || exit 2
OUT=$(realpath -- "$OUT") || exit 2
: > "$OUT/eqhash-fixture.out" && : > "$OUT/eqhash-fixture.err" || exit 2
runtime_rc=0
(ulimit -v 8388608 || exit 2
 LC_ALL=C.UTF-8 timeout --kill-after="$KILL_AFTER" "$LIMIT" "$BIN" --load tools/ai/eqhash-semantics.el -- \
     > "$OUT/eqhash-fixture.out" 2> "$OUT/eqhash-fixture.err") || runtime_rc=$?
python3 - "$OUT/eqhash-fixture.out" "$OUT/eqhash-fixture.err" "$runtime_rc" <<'PY'
from collections import Counter
from pathlib import Path
import re
import sys

row = re.compile(rb'\(eqhash ([a-z][a-z0-9-]*) .+\)')
completion = b'(eqhash-complete 56)'
expected = Path('tools/ai/eqhash-semantics.expected').read_bytes()
expected_lines = expected.split(b'\n')
if not expected.endswith(b'\n') or len(expected_lines) != 58 or expected_lines[-2:] != [completion, b'']:
    sys.exit('EQHASH-FIXTURE invalid canonical oracle framing')
want = {}
for line in expected_lines[:-2]:
    match = row.fullmatch(line)
    if not match or match[1] in want:
        sys.exit('EQHASH-FIXTURE malformed or duplicate canonical oracle row')
    want[match[1]] = line
issues = []
if int(sys.argv[3]):
    issues.append(f'runtime exit {sys.argv[3]}')
if Path(sys.argv[2]).read_bytes():
    issues.append('runtime stderr is not empty')
output = Path(sys.argv[1]).read_bytes()
if not output.endswith(b'\n'):
    issues.append('stdout lacks final newline')
lines = output.split(b'\n')
if lines[-1] == b'':
    lines.pop()
got, labels, completions = {}, [], []
for index, line in enumerate(lines):
    match = row.fullmatch(line)
    if match:
        label = match[1]
        labels.append(label)
        got[label] = line
        if label not in want:
            issues.append(f'unexpected label {label.decode("ascii")}')
        if completions:
            issues.append('row follows completion')
    elif line.startswith(b'(eqhash-complete'):
        completions.append(line)
    elif line == b't' and index == len(lines) - 1 and completions == [completion]:
        pass  # Only the documented CLI epilogue, after the complete protocol.
    else:
        issues.append(f'unexpected or malformed stdout line {index + 1}')
if completions != [completion]:
    issues.append('expected exactly one (eqhash-complete 56)')
counts = Counter(labels)
if labels != list(want):
    issues.append('case labels differ in count or canonical order')
bad = [label.decode('ascii') for label in want if counts[label] != 1 or got.get(label) != want[label]]
print('EQHASH-FIXTURE %d/56' % (56 - len(bad))
      + (' differ: ' + ' '.join(bad) if bad else '')
      + (' invalid: ' + '; '.join(issues[:6]) if issues else ''))
sys.exit(1 if bad or issues else 0)
PY
