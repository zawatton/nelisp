#!/bin/sh
# usage: startoff-check.sh BINARY   exit 0 iff string-match on a 1.26M-char multibyte string costs the same
# at start 900000 as at start 1000 (ratio <= 1.5) and under 500 us per call at both, for the ASCII,
# non-ASCII and word-boundary patterns of startoff.el.
here=$(cd "$(dirname "$0")" && pwd); bin=$1
out=$( ulimit -v 8388608; timeout 300 ${RXWORD_CPU:+taskset -c $RXWORD_CPU} "$bin" -Q --batch --load "$here/startoff.el" 2>&1 < /dev/null )
echo "$out" | grep '^SO'
echo "$out" | grep '^SO' | awk 'BEGIN{n=0;bad=0} {n++; if ($5+0 > 1.5 || $3+0 > 500 || $4+0 > 500) bad=1} END{ exit (n==4 && !bad) ? 0 : 1 }'
