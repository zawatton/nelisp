#!/bin/sh
# usage: rows-check.sh BINARY   End-to-end: the two survey rows (hl-re-search-count-buf, mb-mbstr-string-match-far)
# give the same result under BINARY as under GNU Emacs 31.1.  Exit 0 iff both agree.
here=$(cd "$(dirname "$0")" && pwd); bin=$1; rc=0
for row in hl-re-search-count-buf mb-mbstr-string-match-far; do
  d=$here/far/drv/$row.el
  a=$( ulimit -v 8388608; BENCH_N=3 timeout 300 "$bin" --load "$d" 2>&1 < /dev/null | sed -n 's/.*result=\(.*\) elapsed=.*/\1/p' )
  b=$( BENCH_N=3 timeout 300 /usr/local/bin/emacs -Q --batch -l "$d" 2>&1 < /dev/null | sed -n 's/.*result=\(.*\) elapsed=.*/\1/p' )
  echo "$row nelisp=[$a] gnu=[$b]"
  [ -n "$a" ] && [ "$a" = "$b" ] || rc=1
done
exit $rc
