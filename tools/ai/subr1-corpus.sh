#!/bin/sh
# subr1 GNU-parity corpus for subr introspection and calling
# (target/subr1/corpus/subr-parity.el; gnu.out = GNU Emacs 31.1 -Q --batch,
# nc5.out = the nc5 build, the closest to this lane's seed).
# usage: subr1-corpus.sh BIN same|gnu|mutant
#   same:   every row of BIN's output equals nc5.out's, or equals GNU's where nc5
#           differed from GNU (behaviour-equal except changes toward GNU; those
#           rows are listed as TOWARD-GNU)
#   gnu:    every row on which nc5 agreed with GNU still agrees, and the
#           calling rows (wrong-number-of-arguments included) all agree
#   mutant: the gnu comparison rejects an output with one row changed
D=$(cd "$(dirname "$0")/../.." && pwd); C=$D/target/subr1/corpus
BIN=$1; MODE=$2; ulimit -v 8388608
case $BIN in /*) ;; *) BIN=$PWD/$BIN ;; esac
O=${TMPDIR:-/tmp}/subr1corpus.$$; mkdir -p $O
cmpgnu() { # $1 = output under test
  python3 - "$C/gnu.out" "$C/nc5.out" "$1" <<'PY'
import sys
g,n,t=[open(f).read().splitlines() for f in sys.argv[1:4]]
key=lambda l:l.split(' => ')[0]
G={key(l):l for l in g}; N={key(l):l for l in n}; T={key(l):l for l in t}
calls=[k for k in G if k.split(' ')[0] in ('car','cons','aref','substring','make-string','integerp','fixnump','symbol-function','length','1+','eq','funcall','apply','mapcar','eval','defun','loop','fset','after','defalias','wrapper')]
bad=[k for k in G if N.get(k)==G[k] and T.get(k)!=G[k]]
badc=[k for k in calls if N.get(k)==G[k] and T.get(k)!=G[k]]
agree=sum(1 for k in G if T.get(k)==G[k])
print(f'CORPUS rows={len(G)} agree_gnu={agree} nc5_agree={sum(1 for k in G if N.get(k)==G[k])} regressed={len(bad)} call_rows={len(calls)}')
for k in bad[:10]: print('REGRESSED', T.get(k))
sys.exit(1 if bad or badc or len(calls)<40 else 0)
PY
}
case $MODE in
  same|gnu) (cd $O && timeout 120 "$BIN" --load $C/subr-parity.el > $O/out 2>&1)
    if [ $MODE = same ]; then python3 - "$C/gnu.out" "$C/nc5.out" "$O/out" <<'PY'
import sys
g,n,t=[open(f).read().splitlines() for f in sys.argv[1:4]]
key=lambda l:l.split(' => ')[0]
G={key(l):l for l in g}; T={key(l):l for l in t}
bad=0; toward=0
if len(t)!=len(n): print('ROWCOUNT', len(n), len(t)); bad+=1
for l in n:
    k=key(l)
    if T.get(k)==l: continue
    if T.get(k) is not None and T.get(k)==G.get(k): toward+=1; print('TOWARD-GNU', T[k]); continue
    bad+=1; print('CHANGED', T.get(k))
print(f'CORPUS same rows={len(n)} toward_gnu={toward} changed={bad}')
sys.exit(1 if bad else 0)
PY
      rc=$?
    else cmpgnu $O/out; rc=$?; fi ;;
  mutant) sed 's/^car 0 => .*/car 0 => (ERR wrong-number-of-arguments (car 0))/' $C/gnu.out > $O/mut
    if cmpgnu $O/mut > /dev/null; then echo "MUTANT NOT CAUGHT"; rc=1; else echo "MUTANT caught"; rc=0; fi ;;
  *) echo "usage: $0 BIN same|gnu|mutant"; rc=2 ;;
esac
rm -rf $O; exit $rc
