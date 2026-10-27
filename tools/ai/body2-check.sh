#!/bin/sh
# body2-check.sh BINARY CHECK: criteria checks of the body2 lane (exit 0 = pass).
#   ir-prin1   per-trip Ir of the prin1-strs-esc driver <= 50% of the v25 baseline
#   len-o1     first `length' of a fresh 90k / 180k multibyte string costs O(1) Ir (callgrind difference)
#   ir-efn     per-trip Ir of the expand-file-name-tilde driver below the v25 baseline
#   print-parity | len-parity | efn-parity   corpus outputs equal GNU Emacs 31.1 (print: known v25 gaps frozen)
#   selftest   the parity harness fails on a corrupted copy of a GNU output (negative control)
BIN=$1; CHK=$2; HERE=$(cd "$(dirname "$0")" && pwd)/body2-checks
case $BIN in /*) ;; *) BIN=$(pwd)/$BIN;; esac
[ -x "$BIN" ] || { echo "no binary $BIN"; exit 2; }
base() { awk -v k="$1" '$1==k{print $2}' "$HERE/baseline-v25.txt"; }
case $CHK in
ir-prin1) v=$(python3 "$HERE/cgrun.py" "$BIN" "$HERE/drv-prin1.el" 4 10) || exit 2; b=$(base prin1-strs-esc)
  echo "prin1-strs-esc Ir/trip=$v baseline=$b"; python3 -c "import sys;sys.exit(not($v*2<=$b))";;
ir-efn) v=$(python3 "$HERE/cgrun.py" "$BIN" "$HERE/drv-efn.el" 200 600) || exit 2; b=$(base expand-file-name-tilde)
  echo "expand-file-name-tilde Ir/trip=$v baseline=$b"; python3 -c "import sys;sys.exit(not($v<$b))";;
len-o1) python3 "$HERE/lenO1.py" "$BIN" "$HERE/drv-len.el";;
print-parity) "$HERE/parity.sh" "$BIN" print-corpus.el "" known-print.txt;;
len-parity) "$HERE/parity.sh" "$BIN" len-corpus.el;;
efn-parity) "$HERE/parity.sh" "$BIN" efn-corpus.el;;
selftest) T=$(mktemp -d); printf '3 corrupted\n' > "$T/known.txt"   # a frozen row that no engine produces
  ! "$HERE/parity.sh" "$BIN" len-corpus.el "$T/o" "$T/known.txt" >/dev/null;;
*) echo "unknown check $CHK"; exit 2;;
esac
