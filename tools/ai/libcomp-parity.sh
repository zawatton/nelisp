#!/bin/sh
# libcomp-parity.sh BINARY MODE   (run from the repository root)
#   nl | gnu-out  : the transcript of test/nelisp-libcomp-parity-corpus.el on BINARY / GNU Emacs
#   gnu           : BINARY's transcript equals GNU Emacs 31.1's (>= 100 cases, ends with the END line)
#   mutant-caught : with LIBCOMP_MUTANT=1 the BINARY transcript differs from GNU's
set -u
BIN=${1:?binary}; MODE=${2:?mode}
C=test/nelisp-libcomp-parity-corpus.el
GNU=${GNU_EMACS:-/usr/local/bin/emacs}
T=${TMPDIR:-/tmp}/libcomp-parity.$$
ulimit -v 8388608
nl() { timeout 300 "$BIN" --load $C 2>/dev/null; }
gnu() { timeout 300 "$GNU" -Q --batch -l $C 2>/dev/null; }
case $MODE in
  nl) nl ;;
  gnu-out) gnu ;;
  gnu) nl > $T.nl; gnu > $T.gnu
       n=$(grep -c ' => ' $T.gnu)
       test "$n" -ge 100 && grep -q '^LIBCOMP-PARITY-END$' $T.gnu && cmp -s $T.nl $T.gnu; rc=$?
       echo "cases=$n rc=$rc"; [ $rc = 0 ] || diff $T.gnu $T.nl | head -40
       rm -f $T.*; exit $rc ;;
  mutant-caught) LIBCOMP_MUTANT=1 nl > $T.mut; gnu > $T.gnu
       grep -q '^LIBCOMP-PARITY-END$' $T.mut && ! cmp -s $T.mut $T.gnu; rc=$?
       diff $T.gnu $T.mut | head -4; rm -f $T.*; exit $rc ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
