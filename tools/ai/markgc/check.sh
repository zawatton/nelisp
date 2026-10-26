#!/bin/sh
# usage: check.sh BINARY [probe|live|corpus]   (run from repo root)
BIN=$1; MODE=${2:-probe}; D=tools/ai/markgc
ulimit -v 8388608; export LC_ALL=C.UTF-8
case $MODE in
probe) timeout 100 "$BIN" --load $D/probe.el; exit $? ;;
live) timeout 100 "$BIN" --load $D/live.el; exit $? ;;
esac
timeout 100 "$BIN" --load $D/corpus.el 2>&1 | grep -E '^[0-9]+:' > /tmp/markgc-nl.$$ || exit 1
timeout 100 /usr/local/bin/emacs -Q --batch -l $D/corpus.el 2>&1 | grep -E '^[0-9]+:' > /tmp/markgc-gnu.$$
diff /tmp/markgc-nl.$$ /tmp/markgc-gnu.$$; rc=$?; rm -f /tmp/markgc-nl.$$ /tmp/markgc-gnu.$$; exit $rc
