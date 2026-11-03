# Shared setup for the a240-s1 checks (sourced).
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)
BIN=${STARTUP_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on28/nelisp-dynamic}
RUNDIR=${STARTUP_DRIVER_DIR:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/run/b28}
PRODUCE=$ROOT/tools/nelisp-application-image-produce.sh
SCRATCH=${STARTUP_SCRATCH:-$HOME/.cache/tmp/a240-s1}/scratch
mkdir -p "$SCRATCH" || exit 1
T=$(mktemp -d "$SCRATCH/chk.XXXXXX") || exit 1
trap 'rm -rf "$T"' EXIT
BSHA=$(sha256sum "$BIN" | cut -d' ' -f1)
# run_nelisp ENVASSIGNMENTS... -- ARGS... : clean env, 8 GiB cap
run_nelisp() {
  local e=(); while [ "$1" != -- ]; do e+=("$1"); shift; done; shift
  ( ulimit -v 8388608; env -i HOME="$HOME" PATH="$PATH" BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256="$BSHA" "${e[@]}" timeout 600 "$BIN" "$@" )
}
