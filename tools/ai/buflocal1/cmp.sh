#!/bin/bash
# usage: cmp.sh NL_BINARY [local|narrow...]   -- diff NeLisp vs GNU on each corpus
HERE=$(cd "$(dirname "$0")" && pwd); B=$1; shift; O=${OUT:-${TMPDIR:-/tmp}/buflocal1-cmp}; mkdir -p $O
for c in "${@:-local narrow}"; do for c1 in $c; do
CORPUS=$HERE/corpus-$c1.el timeout 120 /usr/local/bin/emacs -Q --batch -l $HERE/run-corpus.el > $O/gnu-$c1.txt 2>/dev/null
(ulimit -v 8388608; CORPUS=$HERE/corpus-$c1.el timeout 600 $B --load ${RUNNER:-$HERE/run-corpus.el} -- > $O/nl-$c1.txt 2>$O/nl-$c1.err)
sed -i '${/^\(nil\|t\)$/d}' $O/nl-$c1.txt; n=$(wc -l < $O/gnu-$c1.txt); d=$(diff $O/gnu-$c1.txt $O/nl-$c1.txt | grep -c '^>')
echo "$c1: gnu-lines=$n diverging=$d"; done; done
