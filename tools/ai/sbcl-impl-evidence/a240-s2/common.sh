# Shared setup for the a240-s2 checks (sourced).
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../../.." && pwd)
BIN=${STARTUP_BIN:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on28/nelisp-dynamic}
BIN27=${STARTUP_BIN_OTHER:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on27/nelisp-dynamic}
RUNDIR=${STARTUP_DRIVER_DIR:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds/run/b28}
PRODUCE=$ROOT/tools/nelisp-application-image-produce.sh
LAUNCH=${LAUNCHER:-$ROOT/tools/nelisp-application-image-launch.sh}
case "$LAUNCH" in *.py) LCMD=(python3 -I "$LAUNCH") ;; *) LCMD=("$LAUNCH") ;; esac
SCRATCH=${STARTUP_SCRATCH:-$HOME/.cache/tmp/a240-s2}/scratch
mkdir -p "$SCRATCH" || exit 1
T=$(mktemp -d "$SCRATCH/chk.XXXXXX") || exit 1
trap 'rm -rf "$T"' EXIT
BSHA=$(sha256sum "$BIN" | cut -d' ' -f1)
bad() { echo "FAIL: $*"; exit 1; }
[ -r "$LAUNCH" ] || bad "launcher absent: $LAUNCH"
# launch IMAGE_DIR BINARY BINARY_SHA N -> sets OUT (stdout+stderr), RC
launch() {
  OUT=$(NELISP_APP_IMAGE_DIR=$1 NELISP_LANE_IMAGE=$2 NELISP_LANE_IMAGE_SHA256=$3 \
        NELISP_SCRATCH_ROOT=$T/lane-scratch BENCH_ROW=hash-get1024 BENCH_N=$4 BENCH_HIGH=$4 BENCH_WARMUPS=0 \
        timeout 700 "${LCMD[@]}" --load PLACEHOLDER 2>&1); RC=$?
}
