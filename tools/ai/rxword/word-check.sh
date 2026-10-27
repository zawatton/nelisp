#!/bin/sh
# usage: word-check.sh BINARY   exit 0 iff every word-boundary / symbol-boundary search costs < WORD_LIMIT x
# (default 3) the plain search of the same buffer (tools/ai/rxword/wordbench.el).
here=$(cd "$(dirname "$0")" && pwd); bin=$1; lim=${WORD_LIMIT:-3}
out=$( ulimit -v 8388608; WB_REPS=${WB_REPS:-3} timeout 300 ${RXWORD_CPU:+taskset -c $RXWORD_CPU} "$bin" -Q --batch --load "$here/wordbench.el" 2>&1 < /dev/null )
echo "$out" | grep '^WB'
echo "$out" | grep '^WBRATIO' | awk -v lim="$lim" 'BEGIN{n=0;bad=0} {n++; if ($3+0 >= lim+0) bad=1} END{ exit (n==4 && !bad) ? 0 : 1 }'
