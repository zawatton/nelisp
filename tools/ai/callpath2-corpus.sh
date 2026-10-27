#!/bin/sh
# callpath2 GNU-parity corpus (target/callpath2/corpus/callpath2-parity.el;
# gnu.out = GNU Emacs 31.1 -Q --batch, v28.out = the v28 baseline build):
# subr-arity/func-arity of every builtin in names.txt, symbol-function on
# aliases/autoloads/nil, specials under let and buffer-local, closures with
# &optional/&rest, wrong-number-of-arguments, intern.
# usage: callpath2-corpus.sh BIN gnu|mutant
#   gnu:    every row equals GNU's, except the `arity NAME' rows of the names
#           NeLisp defines in Lisp (not a Subr object; known divergence, listed
#           in lisp-names.txt), and no row v28 got right regressed
#   mutant: the gnu comparison rejects an output with one row changed
D=$(cd "$(dirname "$0")/../.." && pwd); C=$D/target/callpath2/corpus
BIN=$1; MODE=$2; ulimit -v 8388608
case $BIN in /*) ;; *) BIN=$PWD/$BIN ;; esac
O=${TMPDIR:-/tmp}/callpath2corpus.$$; mkdir -p $O
cmp() { # $1 output under test
  python3 - "$C/gnu.out" "$C/v28.out" "$1" "$C/lisp-names.txt" <<'PY'
import sys
g,v,t=[[l for l in open(f, errors='replace').read().splitlines() if ' => ' in l] for f in sys.argv[1:4]]
allow={'arity '+n.strip() for n in open(sys.argv[4]) if n.strip()}
key=lambda l:l.split(' => ')[0]
G={key(l):l for l in g}; V={key(l):l for l in v}; T={key(l):l for l in t}
reg=[k for k in G if V.get(k)==G[k] and T.get(k)!=G[k]]
diff=[k for k in G if T.get(k)!=G[k]]
bad=[k for k in diff if k not in allow]
agree=len(G)-len(diff); vagree=sum(1 for k in G if V.get(k)==G[k])
print(f'CORPUS rows={len(G)} agree_gnu={agree} v28_agree={vagree} regressed={len(reg)} differ={len(diff)} allowed={len(diff)-len(bad)} bad={len(bad)}')
for k in reg[:10]: print('REGRESSED', T.get(k))
for k in bad[:20]: print('DIFF', (T.get(k) or k+' => MISSING')[:160]); print('  GNU', G[k][:160])
sys.exit(1 if (reg or bad or len(G) < 290) else 0)
PY
}
case $MODE in
  gnu) (cd $O && timeout 150 "$BIN" --load $C/callpath2-parity.el > $O/out 2>/dev/null)
    grep -q "^CORPUS-END" $O/out || echo "NO CORPUS-END"
    cmp $O/out; rc=$?; grep -q "^CORPUS-END" $O/out || rc=1 ;;
  mutant) sed 's/^intern existing => .*/intern existing => nil/' $C/gnu.out > $O/mut
    if cmp $O/mut > /dev/null; then echo "MUTANT NOT CAUGHT"; rc=1; else echo "MUTANT caught"; rc=0; fi ;;
  *) echo "usage: $0 BIN gnu|mutant"; rc=2 ;;
esac
rm -rf $O; exit $rc
