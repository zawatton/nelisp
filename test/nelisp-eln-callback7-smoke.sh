#!/bin/sh
set -eu

# Build/run the dynamic reader (the static reader omits dynamic-loader FFI imports):
# NELISP_READER_DYNAMIC=1 NELISP_STANDALONE_READER_OUTPUT="$PWD/target/nelisp-callbacks" make standalone-reader
# NELISP_BIN="$PWD/target/nelisp-callbacks" sh test/nelisp-eln-callback7-smoke.sh

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=${NELISP_ROOT:-$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)}
NELISP_BIN=${NELISP_BIN:-"$ROOT/target/nelisp-p5-callback7-luna"}
CC=${CC:-cc}
CACHE_ROOT=${XDG_CACHE_HOME:-$HOME/.cache}
OUTDIR=$(mktemp -d "$CACHE_ROOT/nelisp-eln-callback7.XXXXXX")

[ -x "$NELISP_BIN" ] || { echo "NELISP_BIN is not executable" >&2; exit 2; }
"$CC" -O2 -fPIC -shared "$SCRIPT_DIR/fixtures/nelisp-eln-callback7-caller.c" \
  -o "$OUTDIR/callback7-caller.so"

BIN_SHA_BEFORE=$(sha256sum "$NELISP_BIN" | awk '{print $1}')
set +e
(cd "$ROOT" && NELISP_ROOT="$ROOT" NELISP_BIN="$NELISP_BIN" OUTDIR="$OUTDIR" \
  "$NELISP_BIN" -L "$ROOT/lisp" -L "$ROOT/packages/nl-ffi/src" \
  --load "$SCRIPT_DIR/nelisp-eln-callback7-driver.el") \
  >"$OUTDIR/standalone.stdout" 2>"$OUTDIR/standalone.stderr"
RUN_STATUS=$?
set -e

[ "$RUN_STATUS" -eq 0 ] || {
  cat "$OUTDIR/standalone.stderr" >&2
  echo "callback7 driver exited $RUN_STATUS (artifacts: $OUTDIR)" >&2
  exit 1
}
[ ! -s "$OUTDIR/standalone.stderr" ] || {
  cat "$OUTDIR/standalone.stderr" >&2
  echo "unexpected callback7 stderr (artifacts: $OUTDIR)" >&2
  exit 1
}
grep -Fxq 'ELN_CALLBACK7_PASS full_width=2 nested=1 error_status=1 inactive=1' \
  "$OUTDIR/standalone.stdout" || {
  cat "$OUTDIR/standalone.stdout" >&2
  echo "callback7 success marker missing (artifacts: $OUTDIR)" >&2
  exit 1
}
grep -Fxq 'ELN_CALLBACK1_PASS argc=1 zero_tail=6 error_status=1' \
  "$OUTDIR/standalone.stdout" || {
  cat "$OUTDIR/standalone.stdout" >&2
  echo "callback1 success marker missing (artifacts: $OUTDIR)" >&2
  exit 1
}
BIN_SHA_AFTER=$(sha256sum "$NELISP_BIN" | awk '{print $1}')
[ "$BIN_SHA_BEFORE" = "$BIN_SHA_AFTER" ] || {
  echo "candidate changed during callback7 smoke" >&2
  exit 1
}
printf 'CALLBACK7_BIN_SHA256=%s\nARTIFACT_DIR=%s\n' \
  "$BIN_SHA_AFTER" "$OUTDIR"
