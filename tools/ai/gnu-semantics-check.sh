#!/usr/bin/env bash
# Machine checks for gnu-semantics-progress.org; evidence stays under target/.
set -euo pipefail
cd "$(dirname "$0")/../.."
ulimit -v 8388608
root=$PWD
out=$root/target/gnu-semantics
export TMPDIR=$root/target/tmp XDG_CACHE_HOME=$root/target/xdg-cache
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME"
dyn=${GNU_SEMANTICS_DYN:-$out/nelisp-dyn}
static=${GNU_SEMANTICS_STATIC:-$out/nelisp-static}
gnu=${EMACS:-emacs}
mkdir -p "$out/checks"
fail() { echo "GNU-SEMANTICS FAIL: $*"; exit 1; }
fixture() {
  local item=$1 locale=$2 source=test/fixtures/gnu-semantics-$1.el
  local expected=test/fixtures/gnu-semantics-$1.$2.gnu-31.1.expected
  [[ -f $expected ]] || expected=test/fixtures/gnu-semantics-$1.gnu-31.1.expected
  [[ -f $source && -f $expected ]] || fail "missing $item GNU fixture"
  LC_ALL=$locale "$gnu" -Q --batch -l "$source" >"$out/checks/$item.$locale.gnu.out" 2>"$out/checks/$item.$locale.gnu.err"
  cmp "$expected" "$out/checks/$item.$locale.gnu.out"
  [[ ! -s $out/checks/$item.$locale.gnu.err ]] || fail "$item GNU unexpected stderr"
  for binary in "$dyn" "$static"; do
    [[ -x $binary ]] || fail "missing build $binary (not yet verified)"
    local name; name=$(basename "$binary")
    LC_ALL=$locale "$binary" --load "$source" >"$out/checks/$item.$locale.$name.out" 2>"$out/checks/$item.$locale.$name.err"
    cmp "$expected" "$out/checks/$item.$locale.$name.out"
    [[ ! -s $out/checks/$item.$locale.$name.err ]] || fail "$item unexpected stderr"
  done
}
case ${1:-} in
  buffer|load|printer|syntax) fixture "$1" C.UTF-8 ;;
  error) fixture error C; fixture error C.UTF-8 ;;
  shadow)
    fixture shadow C.UTF-8
    ;;
  variables)
    fixture variables C.UTF-8
    for binary in "$dyn" "$static"; do
      [[ -x $binary ]] || fail "missing build $binary (not yet verified)"
      NELISP_BIN=$binary EMACS=$gnu python3 test/nelisp-special-variables-test.py >"$out/checks/variables.$(basename "$binary").log" 2>&1
    done
    ;;
  final)
    [[ -x $dyn && -x $static ]] || fail "both final builds are required"
    grep -qx 'GATE-COUNT: 11 cases, 0 failures' "$out/final-checks.log"
    grep -qx 'STANDALONE-CHECKS PASS' "$out/standalone-checks.log"
    python3 - "$out/mcp/result.json" "$out/perf/results-dyn" <<'PY'
import json, sys
from pathlib import Path
mcp = json.load(open(sys.argv[1]))
assert mcp['smoke_passed'] and mcp['count'] == 40 and mcp['returncode'] == 0
directory = Path(sys.argv[2])
manifest = json.load(open(directory / 'manifest.json'))
assert manifest['cpu'] == 15 and manifest['rounds'] == 7
data = json.load(open(directory / 'summary.json'))['results']
for name in ('buffer', 'load', 'syntax'):
    item = data[name]
    assert len(item['rounds']) == 7
    assert item['median_per_round_ratios']['workload_seconds']['candidate/base'] <= 1.03
PY
    ;;
  *) fail "usage: $0 buffer|error|shadow|variables|load|printer|syntax|final" ;;
esac
echo "GNU-SEMANTICS PASS: $1"
