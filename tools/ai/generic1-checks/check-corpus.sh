#!/bin/bash
# usage: check-corpus.sh BINARY CORPUS  -- PASS iff BINARY's output equals GNU Emacs' for the corpus
# (a final `t' line printed by the standalone loader is ignored).
BIN=$1; C=$2; H=$(cd "$(dirname "$0")" && pwd)
D=$(mktemp -d); trap 'rm -rf $D' EXIT; cd $D
(ulimit -v 8388608; timeout 120 ${GNU_EMACS:-/usr/local/bin/emacs} -Q --batch -l $H/$C.el >g.out 2>g.err)
(ulimit -v 8388608; timeout 120 $BIN --load $H/$C.el >n.out 2>n.err)
sed -i '${/^t$/d}' n.out
if diff g.out n.out >d.txt; then echo "corpus $C: $(wc -l <g.out) forms identical"; exit 0; fi
echo "corpus $C: $(grep -c '^<' d.txt) diverging lines"; head -6 d.txt; exit 1
