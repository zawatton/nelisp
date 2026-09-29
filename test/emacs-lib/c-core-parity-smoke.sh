#!/usr/bin/env bash
# c-core-parity-smoke.sh --- C-core probes: NeLisp (after the bundle) vs host GNU.
#
#   test/c-core-parity-smoke.sh run [--unit NAME] [--extra FILE]
#   test/c-core-parity-smoke.sh check AREA
#
# `run' evaluates test/c-core-probes/*.el (or only NAME.el) with
# test/c-core-parity-driver.el on host `emacs -Q --batch' (GNU 31.1) and on
# $NELISP_BIN after build/nemacs-bootstrap.el loads (plus --extra FILE, for
# trying a unit before the bundle is rebuilt), then compares the P| lines.
# A full run (no --unit) records build/c-core-parity/AREA.result per area of
# tools/c-core-areas.tsv: PASS only when every name of the area has at least
# one probe line and none of its lines differ.  Loading the bundle takes most
# of the budget, so the ledger checks that recorded result with `check',
# which fails when it is older than the bundle, the binary, the driver, the
# area table or any probe file.
set -u
here=$(cd "$(dirname "$0")/.." && pwd) || exit 1
cd "$here" || exit 1
BIN=${NELISP_BIN:-$here/vendor/nelisp/target/nelisp}
HOST=${EMACS:-emacs}
OUT=build/c-core-parity
AREAS="x-gui display process buffer chars files other"

run() {
  local unit="" extra=""
  while [ $# -gt 0 ]; do
    case "$1" in
      --unit) unit=$2; shift 2 ;;
      --extra) extra=$2; shift 2 ;;
      *) echo "c-core-parity: unknown option $1" >&2; return 2 ;;
    esac
  done
  mkdir -p "$OUT" || return 1
  [ -f build/nemacs-bootstrap.el ] || { echo "c-core-parity: build/nemacs-bootstrap.el missing" >&2; return 1; }
  C_CORE_UNIT=$unit timeout 120 "$HOST" -Q --batch -l test/c-core-parity-driver.el \
    > "$OUT/host.raw" 2> "$OUT/host.err"
  {
    echo '(load (expand-file-name "build/nemacs-bootstrap.el") nil t)'
    [ -n "$extra" ] && printf '(load (expand-file-name "%s") nil t)\n' "$extra"
    echo '(load (expand-file-name "test/c-core-parity-driver.el") nil t)'
  } > "$OUT/run.el"
  C_CORE_UNIT=$unit timeout 300 "$BIN" --load "$here/$OUT/run.el" \
    > "$OUT/nelisp.raw" 2> "$OUT/nelisp.err"
  grep -E '^P(\||-DONE)' "$OUT/host.raw" > "$OUT/host.out"
  grep -E '^P(\||-DONE)' "$OUT/nelisp.raw" > "$OUT/nelisp.out"
  grep -q '^P-DONE' "$OUT/host.out" || { echo "c-core-parity: host run did not finish ($OUT/host.err)" >&2; return 1; }
  if ! grep -q '^P-DONE' "$OUT/nelisp.out"; then
    echo "c-core-parity: NeLisp run did not finish ($OUT/nelisp.err):" >&2
    tail -n 5 "$OUT/nelisp.err" >&2
  fi
  local ndiff
  ndiff=$(diff "$OUT/host.out" "$OUT/nelisp.out" | grep -c '^[<>]')
  diff "$OUT/host.out" "$OUT/nelisp.out" > "$OUT/diff.txt"
  echo "c-core-parity: $(grep -c '^P|' "$OUT/host.out") host lines, $ndiff differing lines ($OUT/diff.txt)"
  if [ -z "$unit" ]; then
    local a
    for a in $AREAS; do
      awk -F'\t' -v a="$a" '$2==a {print $1}' tools/c-core-areas.tsv > "$OUT/names-$a"
      paste -d '\001' "$OUT/host.out" "$OUT/nelisp.out" | awk -F '\001' -v a="$a" -v names="$OUT/names-$a" '
        BEGIN { while ((getline n < names) > 0) { want[n]=1; total++ } }
        $1 ~ /^P\| / { split($1, h, " \\| "); nm=h[2]; if (nm in want) { seen[nm]=1; if ($1 != $2) bad[nm]=1 } }
        END {
          covered=0; nbad=0
          for (n in want) { if (n in seen) covered++; if (n in bad) nbad++ }
          status = (total > 0 && covered == total && nbad == 0) ? "PASS" : "FAIL"
          printf "%s covered=%d/%d differing=%d\n", status, covered, total, nbad
        }' > "$OUT/$a.result"
      echo "  $a: $(cat "$OUT/$a.result")"
    done
  fi
  [ "$ndiff" -eq 0 ] && grep -q '^P-DONE' "$OUT/nelisp.out"
}

check() {
  local a=${1:-} f
  case " $AREAS " in *" $a "*) ;; *) echo "usage: $0 check {$AREAS}" >&2; return 2 ;; esac
  f="$OUT/$a.result"
  [ -f "$f" ] || { echo "c-core-parity: no result for $a; run: $0 run" >&2; return 1; }
  local dep
  for dep in build/nemacs-bootstrap.el "$BIN" test/c-core-parity-driver.el tools/c-core-areas.tsv test/c-core-probes/*.el; do
    [ -e "$dep" ] || continue
    [ "$f" -nt "$dep" ] || { echo "c-core-parity: $f is older than $dep; rerun: $0 run" >&2; return 1; }
  done
  echo "c-core-parity: $a $(cat "$f")"
  grep -q '^PASS ' "$f"
}

case "${1:-}" in
  run) shift; run "$@" ;;
  check) check "${2:-}" ;;
  *) echo "usage: $0 {run [--unit NAME] [--extra FILE]|check AREA}" >&2; exit 2 ;;
esac
