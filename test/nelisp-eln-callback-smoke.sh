#!/bin/sh
# Build one genuine GNU .eln function that tail-calls its `random' helper,
# install a test-only NeLisp callback in that artifact's verified freloc slot,
# and exercise the existing checked-root gateway.
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=${NELISP_ROOT:-$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)}
EMACS_BIN=${EMACS_BIN:-emacs}
CC=${CC:-cc}
NELISP_BIN=${NELISP_BIN:-"$ROOT/target/nelisp"}
CACHE_ROOT=${XDG_CACHE_HOME:-$HOME/.cache}
OUTDIR=$(mktemp -d "$CACHE_ROOT/nelisp-eln-callback.XXXXXX")
export OUTDIR

case "$EMACS_BIN" in
  */*) [ -x "$EMACS_BIN" ] || { echo "EMACS_BIN is not executable: $EMACS_BIN" >&2; exit 2; } ;;
  *) command -v "$EMACS_BIN" >/dev/null 2>&1 || { echo "EMACS_BIN was not found: $EMACS_BIN" >&2; exit 2; } ;;
esac
[ -x "$NELISP_BIN" ] || { echo "NELISP_BIN is not executable: $NELISP_BIN" >&2; exit 2; }
command -v "$CC" >/dev/null 2>&1 || { echo "CC was not found: $CC" >&2; exit 2; }

cat > "$OUTDIR/compile.el" <<'EL'
;; -*- lexical-binding: t; -*-
(require 'comp)
(require 'cl-lib)
(let* ((dir (getenv "OUTDIR"))
       (source (expand-file-name "callback-random.el" dir))
       (output (expand-file-name "callback-random.eln" dir))
       (index (cl-position (symbol-function 'random) comp-subr-list)))
  (unless (and index (= (length comp-subr-list) 1479))
    (error "Unexpected GNU comp-subr-list metadata"))
  (native-compile source output)
  (load output nil nil t)
  (unless (and (native-comp-function-p
                (symbol-function 'nelisp-eln-callback-random))
               (= (nelisp-eln-callback-random 1) 0))
    (error "Host GNU .eln random(1) did not return zero"))
  (with-temp-file (expand-file-name "host-metadata" dir)
    (insert (format "%s\n%s\n" comp-abi-hash index))))
EL

cp "$SCRIPT_DIR/fixtures/nelisp-eln-callback-random.el" "$OUTDIR/callback-random.el"
"$EMACS_BIN" --batch -Q -l "$OUTDIR/compile.el" > "$OUTDIR/host.log" 2>&1
cat "$OUTDIR/host.log"
[ -s "$OUTDIR/callback-random.eln" ] || { echo "GNU .eln was not produced" >&2; exit 1; }

HOST_ABI_HASH=$(sed -n '1p' "$OUTDIR/host-metadata")
RANDOM_SUBR_INDEX=$(sed -n '2p' "$OUTDIR/host-metadata")
ELN_SYMBOL=$(nm -D -S --defined-only "$OUTDIR/callback-random.eln" |
  awk '$3 == "T" && $4 ~ /_nelisp_eln_callback_random_0$/ { print $4 }')
[ -n "$ELN_SYMBOL" ] || { echo "GNU .eln entry symbol was not exported" >&2; exit 1; }
DISP_HEX=$(objdump -d "$OUTDIR/callback-random.eln" | awk -v symbol="$ELN_SYMBOL" '
  $0 ~ "<" symbol ">:" { in_function=1; next }
  in_function && /jmp[[:space:]]+\*0x/ {
    line=$0; sub(/^.*\*0x/, "", line); sub(/\(%rax\).*$/, "", line)
    print line; exit
  }
  in_function && /^[[:xdigit:]]+[[:space:]]+<.*>:/ { exit }
')
[ -n "$DISP_HEX" ] || { echo "GNU .eln does not tail-call a freloc helper" >&2; exit 1; }
DISP_BYTES=$((0x$DISP_HEX))
[ $((DISP_BYTES % 8)) -eq 0 ] || { echo "freloc displacement is not pointer-aligned" >&2; exit 1; }
RANDOM_HELPER_SLOT=$((DISP_BYTES / 8))
[ "$RANDOM_HELPER_SLOT" -eq $((15 + RANDOM_SUBR_INDEX)) ] || {
  echo "GNU .eln helper slot disagrees with producer metadata" >&2
  exit 1
}
export HOST_ABI_HASH RANDOM_SUBR_INDEX RANDOM_HELPER_SLOT ELN_SYMBOL

ELN_SHA_BEFORE=$(sha256sum "$OUTDIR/callback-random.eln" | awk '{print $1}')
CANDIDATE_SHA_BEFORE=$(sha256sum "$NELISP_BIN" | awk '{print $1}')
cat > "$OUTDIR/run-manifest" <<EOF
NELISP_BIN=$NELISP_BIN
NELISP_SHA256=$CANDIDATE_SHA_BEFORE
ELN_SHA256=$ELN_SHA_BEFORE
COMMAND=cd $ROOT && NELISP_ROOT=$ROOT NELISP_BIN=$NELISP_BIN OUTDIR=$OUTDIR HOST_ABI_HASH=$HOST_ABI_HASH RANDOM_SUBR_INDEX=$RANDOM_SUBR_INDEX RANDOM_HELPER_SLOT=$RANDOM_HELPER_SLOT ELN_SYMBOL=$ELN_SYMBOL $NELISP_BIN -L $ROOT/packages/nl-ffi/src --load $SCRIPT_DIR/nelisp-eln-callback-driver.el
EOF

"$CC" -shared -fPIC -fno-stack-protector -fno-common -nostdlib \
  -Wl,-z,defs -o "$OUTDIR/callback-adapter.so" \
  "$SCRIPT_DIR/../packages/nl-ffi/test/fixtures/nelisp-eln-callback-context.c"

set +e
(cd "$ROOT" && NELISP_ROOT="$ROOT" NELISP_BIN="$NELISP_BIN" \
   OUTDIR="$OUTDIR" HOST_ABI_HASH="$HOST_ABI_HASH" \
   RANDOM_SUBR_INDEX="$RANDOM_SUBR_INDEX" \
   RANDOM_HELPER_SLOT="$RANDOM_HELPER_SLOT" ELN_SYMBOL="$ELN_SYMBOL" \
   "$NELISP_BIN" -L "$ROOT/packages/nl-ffi/src" \
     --load "$SCRIPT_DIR/nelisp-eln-callback-driver.el") \
  > "$OUTDIR/standalone.stdout" 2> "$OUTDIR/standalone.stderr"
RUN_STATUS=$?
set -e
cat "$OUTDIR/standalone.stdout"
[ "$RUN_STATUS" -eq 0 ] || {
  cat "$OUTDIR/standalone.stderr" >&2
  echo "standalone callback driver exited $RUN_STATUS" >&2
  exit 1
}
[ ! -s "$OUTDIR/standalone.stderr" ] || {
  cat "$OUTDIR/standalone.stderr" >&2
  echo "standalone callback driver wrote unexpected stderr" >&2
  exit 1
}
MARKER="ELN_CALLBACK_PASS slot=$RANDOM_HELPER_SLOT subr-index=$RANDOM_SUBR_INDEX success_status=0 negative_arity_status=1 calls=2"
[ "$(grep -Fxc "$MARKER" "$OUTDIR/standalone.stdout")" -eq 1 ] || {
  echo "standalone callback driver did not emit its exact success marker" >&2
  exit 1
}
ELN_SHA_AFTER=$(sha256sum "$OUTDIR/callback-random.eln" | awk '{print $1}')
CANDIDATE_SHA_AFTER=$(sha256sum "$NELISP_BIN" | awk '{print $1}')
[ "$ELN_SHA_BEFORE" = "$ELN_SHA_AFTER" ] || {
  echo "GNU .eln artifact changed during callback run" >&2
  exit 1
}
[ "$CANDIDATE_SHA_BEFORE" = "$CANDIDATE_SHA_AFTER" ] || {
  echo "NeLisp candidate changed during callback run" >&2
  exit 1
}
printf 'ELN_SHA256=%s\nNELISP_SHA256=%s\nARTIFACT_DIR=%s\n' \
  "$ELN_SHA_AFTER" "$CANDIDATE_SHA_AFTER" "$OUTDIR" >> "$OUTDIR/run-manifest"
