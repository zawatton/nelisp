#!/bin/sh
# usage: check.sh BINARY [scaling|corpus]   (run from repo root)
BIN=$1; MODE=${2:-scaling}; D=tools/ai/replmatch
ulimit -v 8388608; export LC_ALL=C.UTF-8
if [ "$MODE" = scaling ]; then
  timeout 200 "$BIN" --load $D/scaling.el; exit $?
fi
# corpus: equal to GNU except known pre-existing divergences (cases 15,17,39,40,42)
timeout 100 "$BIN" --load $D/corpus.el 2>&1 | grep -E '^[0-9]+:' | grep -vE '^(15|17|39|40|42):' > /tmp/replmatch-nl.$$ || exit 1
timeout 100 /usr/local/bin/emacs -Q --batch -l $D/corpus.el 2>&1 | grep -E '^[0-9]+:' | grep -vE '^(15|17|39|40|42):' > /tmp/replmatch-gnu.$$
diff /tmp/replmatch-nl.$$ /tmp/replmatch-gnu.$$ >/dev/null; rc=$?; rm -f /tmp/replmatch-nl.$$ /tmp/replmatch-gnu.$$; exit $rc
