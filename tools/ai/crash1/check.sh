#!/bin/sh
# crash1 checks.  usage: check.sh BINARY MODE   (run from the repository root)
# MODE: replace | sort | sortgc | ifc | literal | callproc | stdin | small | all
# Every run is under the 8 GiB address-space cap with a timeout.  The corpus is
# run on GNU Emacs and on BINARY and the outputs must be identical (sortgc and
# the data cases are self-checked: see the corpus headers).
BIN=$1; MODE=$2
D=tools/ai/crash1
W=${CRASH1_WORK:-target/crash1/w}
DATA=${CRASH1_DATA:-$W/data}
GNU=${CRASH1_GNU:-/usr/local/bin/emacs}
mkdir -p "$W"
[ -f "$DATA/expected.txt" ] || python3 -I $D/mkdata.py "$DATA" || exit 2
run_gnu() { ( ulimit -v 8388608; timeout 600 "$GNU" -Q --batch "$@" 2>&1 ); }
run_nl()  { ( ulimit -v 8388608; timeout 600 "$BIN" --batch "$@" 2>&1 ); }
same() { # NAME FILE...: GNU and BINARY output of the corpus must match
  name=$1; shift
  run_gnu -l "$1" > "$W/$name.gnu" ; run_nl --load "$1" --eval '(ignore)' > "$W/$name.nl"; rc=$?
  cmp -s "$W/$name.gnu" "$W/$name.nl" || { diff "$W/$name.gnu" "$W/$name.nl" | head -5; exit 1; }
  [ "$(wc -l < "$W/$name.gnu")" -ge "${MINLINES:-1}" ] || { echo "too few lines"; exit 1; }
}
case $MODE in
  all) for m in small replace sort sortgc ifc literal callproc stdin; do sh "$0" "$BIN" $m || exit 1; done; exit 0 ;;
  replace) MINLINES=25 same replace $D/replace.el ;;
  sort)    MINLINES=57 same sort $D/sort.el ;;
  sortgc)  run_nl --load $D/sortgc.el --eval '(ignore)' > "$W/sortgc.nl"
           [ "$(wc -l < "$W/sortgc.nl")" -eq 6 ] && [ "$(grep -c ' t$' "$W/sortgc.nl")" -eq 6 ] || { cat "$W/sortgc.nl" | head; exit 1; } ;;
  ifc|literal|callproc)
           CRASH1_DATA=$PWD/$DATA CRASH1_KIND=$MODE run_nl --load $D/file.el --eval '(ignore)' > "$W/file-$MODE.nl"
           python3 -I - "$DATA/expected.txt" "$W/file-$MODE.nl" "$MODE" <<'PY' || exit 1
import sys
exp = {l.split()[0]: l.split() for l in open(sys.argv[1])}
n = 0
for l in open(sys.argv[2]):
    e = exp[l.split()[0]]
    if f'({e[1]} "{e[2]}")' not in l:
        print("MISMATCH", l.strip()[:120]); sys.exit(1)
    n += 1
want = 2 if sys.argv[3] == "literal" else 14
if n != want: print("lines", n, "want", want); sys.exit(1)
PY
           ;;
  stdin)   # st_size 0 (pipe): forces the grow path past 8 MiB
           for n in 8388607 8388608 8388609 20000000 50000000; do
             got=$(head -c $n "$DATA/ascii-50000000.txt" | run_nl --eval '(with-temp-buffer (insert-file-contents "/dev/stdin") (message "%d" (buffer-size)))' --eval '(ignore)')
             [ "$got" = "$n" ] || { echo "stdin $n -> $got"; exit 1; }
           done ;;
  small)   # a change in plain behaviour must not show: tiny and mid-size files, a missing file, a directory
           MINLINES=1 same small $D/small.el ;;
  *) echo "unknown mode $MODE"; exit 2 ;;
esac
echo "crash1 $MODE PASS"
