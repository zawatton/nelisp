#!/usr/bin/env bash
# Negative controls for nl-elcheck: the clean file passes, every defective file is caught with
# its expected finding kind.  Exit 0 only if all hold.
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd); t=$here/nl-elcheck-tests; rc=0
chk() { # file expected-kind expected-rc
  out=$("$here/nl-elcheck" --strict "$t/$1" 2>&1); r=$?
  if [ "$3" = 0 ]; then [ $r = 0 ] && ! echo "$out" | grep -q 'error:\|warning:' && { echo "PASS $1 clean"; return; }
  else [ $r = 1 ] && echo "$out" | grep -q "\[$2\]" && { echo "PASS $1 -> $2"; return; }; fi
  echo "FAIL $1 (want $2 rc=$3, got rc=$r)"; echo "$out" | head -5; rc=1; }
chk ok.el - 0
chk unclosed-paren.el unclosed-paren 1
chk extra-close.el read-error 1
chk unterminated-string.el unterminated-string 1
chk swallow-balanced.el swallowed-form 1
chk docstring-quote.el docstring-quote 1
chk docstring-col0-paren.el string-col0-paren 1
chk bytecomp-warn.el bytecomp 1
exit $rc
