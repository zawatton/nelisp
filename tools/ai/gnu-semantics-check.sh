#!/usr/bin/env bash
# Machine checks for gnu-semantics-progress.org; evidence stays under target/.
set -euo pipefail
cd "$(dirname "$0")/../.."
ulimit -v 8388608
root=$PWD
out=$root/target/gnu-semantics
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
    fixture error C.UTF-8
    "$gnu" -Q --batch -l tools/nelisp-emacs-compat.el >"$out/checks/shadow.log" 2>&1
    ;;
  variables)
    for binary in "$dyn" "$static"; do
      [[ -x $binary ]] || fail "missing build $binary (not yet verified)"
      NELISP_BIN=$binary EMACS=$gnu python3 test/nelisp-special-variables-test.py >"$out/checks/variables.$(basename "$binary").log" 2>&1
    done
    ;;
  final)
    [[ -x $dyn && -x $static ]] || fail "both final builds are required"
    grep -qx 'GATE-COUNT: 11 cases, 0 failures' "$out/final-checks.log"
    grep -qx 'STANDALONE-CHECKS PASS' "$out/standalone-checks.log"
    grep -qx 'MCP tools=40' "$out/mcp.log"
    python3 - "$out/timing.json" <<'PY'
import json, sys
data = json.load(open(sys.argv[1]))
assert set(data) == {'buffer', 'load', 'syntax'}
for item in data.values():
    assert len(item['paired_candidate_base']) == 7
    assert item['cpu'] == 15
    assert sorted(item['paired_candidate_base'])[3] <= 1.03
PY
    ;;
  *) fail "usage: $0 buffer|error|shadow|variables|load|printer|syntax|final" ;;
esac
echo "GNU-SEMANTICS PASS: $1"
