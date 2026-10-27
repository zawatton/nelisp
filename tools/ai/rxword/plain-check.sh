#!/bin/sh
# usage: plain-check.sh BINARY   exit 0 iff plain ASCII searches did not get slower than v25 (+10%):
# re-search-forward <= PLAIN_BUF_US (default 10.6), string-match <= PLAIN_SM_US (default 5.5) us per match.
here=$(cd "$(dirname "$0")" && pwd); bin=$1
out=$( ulimit -v 8388608; timeout 300 ${RXWORD_CPU:+taskset -c $RXWORD_CPU} "$bin" -Q --batch --load "$here/plainbench.el" 2>&1 < /dev/null )
echo "$out" | grep '^PB'
echo "$out" | awk -v b="${PLAIN_BUF_US:-10.6}" -v s="${PLAIN_SM_US:-5.5}" '/^PB re-search-forward/ {if ($3+0 > b+0) bad=1; n++} /^PB string-match/ {if ($3+0 > s+0) bad=1; n++} END{ exit (n==2 && !bad) ? 0 : 1 }'
