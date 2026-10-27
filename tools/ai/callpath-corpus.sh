#!/bin/sh
# callpath GNU-parity corpus for builtin calls from interpreted, byte-code and
# funcall/apply call sites (target/callpath/corpus/callpath-parity.el;
# gnu.out = GNU Emacs 31.1 -Q --batch, v25.out = the v25 baseline build).
# usage: callpath-corpus.sh BIN gnu|same|mutant
#   gnu:    every row on which v25 agreed with GNU still agrees, and at least as
#           many rows agree with GNU as on v25
#   same:   every row equals v25's or GNU's (changes only toward GNU, listed)
#   mutant: the gnu comparison rejects an output with one row changed
D=$(cd "$(dirname "$0")/../.." && pwd); C=$D/target/callpath/corpus
BIN=$1; MODE=$2; ulimit -v 8388608
case $BIN in /*) ;; *) BIN=$PWD/$BIN ;; esac
O=${TMPDIR:-/tmp}/callpathcorpus.$$; mkdir -p $O
cmp() { # $1 output under test, $2 mode
  python3 - "$C/gnu.out" "$C/v25.out" "$1" "$2" <<'PY'
import sys
g,v,t=[[l for l in open(f, errors='replace').read().splitlines() if ' => ' in l] for f in sys.argv[1:4]]
mode=sys.argv[4]
key=lambda l:l.split(' => ')[0]
G={key(l):l for l in g}; V={key(l):l for l in v}; T={key(l):l for l in t}
reg=[k for k in G if V.get(k)==G[k] and T.get(k)!=G[k]]
agree=sum(1 for k in G if T.get(k)==G[k]); vagree=sum(1 for k in G if V.get(k)==G[k])
changed=[k for k in G if T.get(k)!=V.get(k) and T.get(k)!=G[k]]
toward=[k for k in G if T.get(k)!=V.get(k) and T.get(k)==G[k]]
print(f'CORPUS rows={len(G)} agree_gnu={agree} v25_agree={vagree} regressed={len(reg)} changed={len(changed)} toward_gnu={len(toward)}')
for k in reg[:10]: print('REGRESSED', T.get(k))
for k in toward[:20]: print('TOWARD-GNU', T.get(k))
for k in changed[:10]: print('CHANGED', T.get(k))
bad = (reg or agree < vagree or len(G) < 100) if mode=='gnu' else (changed or len(G) < 100)
sys.exit(1 if bad else 0)
PY
}
case $MODE in
  gnu|same) (cd $O && timeout 150 "$BIN" --load $C/callpath-parity.el > $O/out 2>&1)
    grep -q "^CORPUS-END" $O/out || echo "NO CORPUS-END"
    cmp $O/out $MODE; rc=$?; grep -q "^CORPUS-END" $O/out || rc=1 ;;
  mutant) sed 's/^fx memq improper hit => .*/fx memq improper hit => nil/' $C/gnu.out > $O/mut; echo CORPUS-END >> $O/mut
    if cmp $O/mut gnu > /dev/null; then echo "MUTANT NOT CAUGHT"; rc=1; else echo "MUTANT caught"; rc=0; fi ;;
  *) echo "usage: $0 BIN gnu|same|mutant"; rc=2 ;;
esac
rm -rf $O; exit $rc
