#!/bin/bash
# integ-wh-build.sh [REV]: build the dynamic linkage (LINKAGE=static: the static one) of REV (default HEAD) from a
# git-archive snapshot, so edits in flight in the worktree cannot leak into the build.
# Adapted from batch F's target/integ/build.sh.  The Windows binary is tools/ai/win-smoke.sh's.
#
# One heavy unit (heavy-run.sh) under ulimit -v 8 GiB.  The runtime version is git
# describe of REV (NELISP_VERSION), because the snapshot has no Git metadata.
# Outputs: target/integ/nelisp-dyn (with its companion files beside it),
# target/integ/build-dyn.log, target/integ/build.rev, target/integ/build.snapshot.
#
# BUILD_SEED=DIR seeds the unit cache (target/standalone-units, keyed by content and
# toolchain digest) from an earlier snapshot's, so only changed units compile.
# HEAVY_RUN and GNU_EMACS override the heavy-run.sh and Emacs paths.
# BUILD_OUT=PATH writes the binary to PATH (relative to the worktree root) and its log to PATH.log,
# so an intermediate build does not replace target/integ/nelisp-dyn.
cd "$(dirname "$0")/../.." || exit 1
root=$PWD
H=${HEAVY_RUN:-}
if [ -z "$H" ]; then   # the nearest ancestor that has bin/heavy-run.sh
  d=$root
  while [ "$d" != / ]; do
    [ -x "$d/bin/heavy-run.sh" ] && { H=$d/bin/heavy-run.sh; break; }
    d=$(dirname "$d")
  done
fi
[ -x "$H" ] || { echo "heavy-run.sh not found; set HEAVY_RUN"; exit 2; }
EMACS_BIN=${GNU_EMACS:-/usr/local/bin/emacs}
rev=$(git rev-parse --verify "${1:-HEAD}^{commit}") || exit 2
linkage=${LINKAGE:-dynamic}
case $linkage in
  dynamic) linkenv=(NELISP_READER_DYNAMIC=1); defout=target/integ/nelisp-dyn ;;
  static)  linkenv=(-u NELISP_READER_DYNAMIC); defout=target/integ/nelisp-static ;;
  *) echo "LINKAGE must be dynamic or static"; exit 2 ;;
esac
out=${BUILD_OUT:-$defout}
case $out in /*) ;; *) out=$root/$out ;; esac
[ -z "$(git status --porcelain --untracked-files=no)" ] || { echo "tracked changes in the worktree: commit first"; exit 2; }
avail=$(df --output=avail -BG / | tail -1 | tr -dc 0-9)
[ "$avail" -ge 50 ] || { echo "DISK LOW: ${avail}G"; exit 3; }
ver=$(git describe --tags --long --always --abbrev=9 "$rev")
snap=$root/target/integ/src-${rev:0:9}${LINKAGE:+-$LINKAGE}
mkdir -p target/integ
rm -rf "$snap" && mkdir -p "$snap" && git archive --format=tar "$rev" | tar -x -C "$snap" || exit 2
mkdir -p "$snap/target/tmp" "$snap/target/xdg-cache"
[ -n "${BUILD_SEED:-}" ] && [ -d "$BUILD_SEED/target/standalone-units" ] && cp -a "$BUILD_SEED/target/standalone-units" "$snap/target/"
echo "$rev" > target/integ/build.rev
echo "$snap" > target/integ/build.snapshot
echo "build $ver rev=${rev:0:9} snapshot=$snap"
s=$(date +%s)
( cd "$snap" || exit 2
  export TMPDIR=$snap/target/tmp XDG_CACHE_HOME=$snap/target/xdg-cache LC_ALL=C
  exec "$H" bash -c 'ulimit -v 8388608; exec timeout 3000 env "$@"' _ \
    "${linkenv[@]}" NELISP_VERSION="$ver" NELISP_SOURCE_REVISION="$rev" \
    NELISP_STANDALONE_READER_OUTPUT="$out" \
    nice -n 10 "$EMACS_BIN" --batch -Q -L lisp -L src -L scripts \
    --eval '(setq load-prefer-newer t)' -l nelisp-standalone-build -f nelisp-standalone-build-reader
) > "${out}.log" 2>&1
rc=$?
echo "${linkage:0:3} rc=$rc $(( $(date +%s) - s ))s $(stat -c %s "$out" 2>/dev/null) $( (ulimit -v 8388608; "$out" --version 2>/dev/null) | head -1)"
[ "$rc" = 0 ] || exit 1
sha256sum "$out" | cut -c1-16,65-
