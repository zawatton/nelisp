#!/bin/sh
# applyfn2 GNU-parity corpus (target/applyfn2/corpus/applyfn2-parity.el; gnu.out = GNU Emacs
# 31.1 -Q --batch, v35.out = the v35 baseline build): the wordroute rows plus GNU Fapply/Ffuncall
# of builtins from compiled call sites and the builtins this lane converted, with wrong counts, wrong
# types, &rest edge cases, fset redefinition and advice.
# usage: wordroute-corpus.sh BIN gnu|mutant|summary
#   gnu:     no row v35 got right regressed, and every row differing from GNU already differed
#            on v35 (pre-existing divergences, listed); prints CORPUS ...
#   mutant:  the gnu comparison rejects an output with one row changed
#   summary: compare v35.out itself (the S1 record)
D=$(cd "$(dirname "$0")/../.." && pwd); C=$D/target/applyfn2/corpus
FIXTURE=$D/test/nelisp-applyfn2-parity-corpus.el
BIN=$1; MODE=$2; ulimit -v 8388608
case $BIN in /*) ;; *) BIN=$PWD/$BIN ;; esac
O=${TMPDIR:-/tmp}/applyfn2corpus.$$; mkdir -p $O
cmp() { # $1 output under test, $2 its label
  python3 - "$C/gnu.out" "$C/v35.out" "$1" "$2" <<'PY'
import sys
g,v,t=[[l for l in open(f, errors='replace').read().splitlines() if ' => ' in l] for f in sys.argv[1:4]]
key=lambda l:l.split(' => ')[0]
G={key(l):l for l in g}; V={key(l):l for l in v}; T={key(l):l for l in t}
reg=[k for k in G if V.get(k)==G[k] and T.get(k)!=G[k]]
diff=[k for k in G if T.get(k)!=G[k]]
old=[k for k in diff if V.get(k)!=G[k]]
fixed=[k for k in G if V.get(k)!=G[k] and T.get(k)==G[k]]
print(f'CORPUS {sys.argv[4]} rows={len(G)} agree_gnu={len(G)-len(diff)} v35_agree={sum(1 for k in G if V.get(k)==G[k])} regressed={len(reg)} differ={len(diff)} preexisting={len(old)} fixed={len(fixed)}')
for k in reg: print('REGRESSED', T.get(k, k+' => MISSING')[:200]); print('  GNU', G[k][:200])
for k in old: print('PREEXISTING', T.get(k, k+' => MISSING')[:160])
sys.exit(1 if (reg or len(diff) != len(old) or len(G) < 220 or len(T) < len(G)) else 0)
PY
}
case $MODE in
  gnu) (cd $O && timeout 150 "$BIN" --load "$FIXTURE" > $O/out 2>/dev/null)
    cp $O/out $C/last.out
    cmp $O/out "$(basename $(dirname $(dirname $BIN)))"; rc=$?; grep -q "CORPUS-END" $O/out || { echo "NO CORPUS-END"; rc=1; } ;;
  summary) cmp $C/v35.out v35; rc=$? ;;
  exact) python3 - "$C/gnu.out" "$C/last.out" <<'PY'
import sys
rows = [[line for line in open(path).read().splitlines() if ' => ' in line] for path in sys.argv[1:]]
expected, actual = rows
diff = sum(a != b for a, b in zip(expected, actual)) + abs(len(expected) - len(actual))
print(f'EXACT-GNU rows={len(expected)} candidate_rows={len(actual)} differences={diff}')
sys.exit(0 if len(expected) >= 225 and expected == actual else 1)
PY
    rc=$? ;;
  mutant) sed 's/^append3 => .*/append3 => (1 2 3)/' $C/gnu.out > $O/mut
    if cmp $O/mut mutant > /dev/null; then echo "MUTANT NOT CAUGHT"; rc=1; else echo "MUTANT caught"; rc=0; fi ;;
  *) echo "usage: $0 BIN gnu|mutant|summary"; rc=2 ;;
esac
rm -rf $O; exit $rc
