#!/usr/bin/env bash
# editor-core-probes-test.sh [LANE_ROOT]
# Run packages/emacs-lib-core/test/editor-core-probes-test.el on the lane's
# baked runtime image (build/nemacs-runtime.nlri) and on host Emacs 31.1.
# Exit 0 only when both print PROBE-DONE and no FAIL row.
set -uo pipefail
root=$(cd "${1:-.}" && pwd) || exit 2
t="$root/packages/emacs-lib-core/test/editor-core-probes-test.el"
host_emacs=${HOST_EMACS:-/usr/local/bin/emacs-31.1}
rc=0
check() { # NAME OUTPUT
  if grep -q '^FAIL' <<<"$2" || ! grep -q '^PROBE-DONE' <<<"$2"; then
    echo "== $1: FAILED"; grep -E '^(FAIL|nelisp:)' <<<"$2"; rc=1
  else echo "== $1: ok ($(grep -c '^PASS' <<<"$2") checks)"; fi
}
if [[ -x $host_emacs ]]; then
  out=$(cd "$root" && ulimit -v 8388608 && "$host_emacs" -Q --batch -l "$t" 2>&1)
  check host "$out"
fi
out=$(cd "$root" && ulimit -v 8388608 && NELISP_HOME="$root" timeout 600 \
  target/nelisp exec-runtime-image build/nemacs-runtime.nlri "(load \"$t\" nil t t)" 2>&1 | grep -v '^ *[0-9]*: ')
check nelisp "$out"
exit $rc
