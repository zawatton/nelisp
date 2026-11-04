#!/bin/sh
# S2 evidence check: re-validate every phased run and the stdout/stderr contract.
cd "$(dirname "$0")" || exit 1
m() { sed -E 's/elapsed=[0-9.e-]+/elapsed=X/; s/resolution=[0-9.e-]+/resolution=X/' "$1"; }
for i in 1 2 3; do
  m plain$i.out > .a; m phased$i.out > .b
  cmp -s .a .b || { rm -f .a .b; echo "stdout differs pair $i"; exit 1; }
  cmp -s plain$i.err phased$i.err || { rm -f .a .b; echo "stderr differs pair $i"; exit 1; }
  grep -q 'route=named-fx-default' plain$i.out || { rm -f .a .b; echo "no native route $i"; exit 1; }
done
rm -f .a .b
n=0
for w in phase-out/*.wrapper.json; do
  id=$(basename "$w" .wrapper.json)
  python3 -I check_phase_events.py --wrapper "$w" --driver "phase-out/$id.events.sexp" >/dev/null || exit 1
  n=$((n+1))
done
test "$n" -eq 3
