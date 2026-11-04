#!/bin/bash
# Doc 240 D2/S3: launch a saved NeLisp application image inside the lane sandbox.
#
# Bash twin of tools/nelisp-application-image-launch.py (same contract, same
# trust, no Python start-up cost).  Drop-in engine for the paired bench:
#   nelisp-application-image-launch.sh --load DRIVER_PLACEHOLDER
# The placeholder is ignored; the row's saved image, its main file and manifest
# come from NELISP_APP_IMAGE_DIR (as written by nelisp-application-image-produce.sh):
#   DIR/<row>.img  DIR/<row>.img.main.el  DIR/<row>.img.manifest
#
# Environment: NELISP_APP_IMAGE_DIR, NELISP_LANE_IMAGE (binary),
# NELISP_LANE_IMAGE_SHA256, BENCH_ROW, BENCH_* (passed through),
# NELISP_SCRATCH_ROOT (optional, default $TMPDIR or /tmp),
# NELISP_APP_LAUNCH_TIMING=1 (per-part breakdown on stderr).
#
# The child is spawned before the hashes are compared but waits on a start
# gate (fifo in the scratch dir) until they matched; see below.
#
# Trust: the manifest pins the binary and the image and main-file sha256.  The
# image and main file are opened once on descriptors, hashed from those
# descriptors (/dev/fd/N), and the SAME inodes are handed to the binary as
# /dev/fd/N (inherited descriptors, new procfs in the sandbox).  A rename or
# replacement of the path after the open cannot change what the binary reads.
# A writer could still modify an inode in place while the child runs; that is
# detected afterwards (dev/ino/size/mtime/ctime change -> exit 2), not prevented.
# Exit codes: 2 refusal (manifest/hash/env/change detected), 124 timeout,
# 125 launcher failure, otherwise the child's code.  No `set -e': every step
# checks its own status.
TIMEOUT=600
FORMAT=nelisp-app-image-v1
ROWS=" integer-sum vector-read vector-write vector-allocate64 string-concat16 string-slice16 hash-get1024 function-call2 nested-function-call2 "
KEYS=" format image-sha256 binary-sha256 driver-sha256 row main-sha256 rows prep-sha256 bootstrap-sha256 "
ALLOWED_ENV="HOME USER LOGNAME PATH LANG LC_ALL LC_CTYPE NELISP_V8_ROOT NELISP_SCRATCH_ROOT NELISP_PRODUCER_IMAGE_SHA256 NELISP_OWNED_SCRATCH NELISP_FXIR_ROOT"
HEX64='^[0-9a-f]{64}$'

TIMING=0; [ "${NELISP_APP_LAUNCH_TIMING:-}" = 1 ] && TIMING=1
MARKS=(); NAMES=()
mark() { [ $TIMING = 1 ] && { NAMES+=("$1"); MARKS+=("${EPOCHREALTIME//[.,]/}"); }; return 0; }
mark start

SCRATCH=""; PID=""; TPID=""
cleanup() {
  if [ -n "$PID" ] && kill -0 "$PID" 2>/dev/null; then
    kill -KILL -- "-$PID" 2>/dev/null; wait "$PID" 2>/dev/null
  fi
  [ -n "$TPID" ] && { kill "$TPID" 2>/dev/null; wait "$TPID" 2>/dev/null; }
  [ -n "$SCRATCH" ] && rm -rf "$SCRATCH"
  return 0
}
report_timing() {
  [ $TIMING = 1 ] || return 0
  mark cleanup
  local i out="LAUNCH-TIMING"
  for ((i = 1; i < ${#MARKS[@]}; i++)); do
    local d=$(( MARKS[i] - MARKS[i-1] ))
    out+=" ${NAMES[i]}=$(( d / 1000 )).$(printf %02d $(( d % 1000 / 10 )))ms"
  done
  echo "$out" >&2
}
finish() { cleanup; report_timing; exit "$1"; }
refuse() { echo "launch: refused: $*" >&2; finish 2; }
fail() { echo "launch: failed: $*" >&2; finish 125; }

[ $# -eq 2 ] && [ "$1" = "--load" ] || { echo "usage: launch.sh --load DRIVER_PLACEHOLDER" >&2; exit 2; }
ROW=${BENCH_ROW-}
case "$ROWS" in *" $ROW "*) [ -n "$ROW" ] || { echo "launch: invalid BENCH_ROW: ''" >&2; exit 2; } ;;
  *) echo "launch: invalid BENCH_ROW: '$ROW'" >&2; exit 2 ;; esac

for k in NELISP_APP_IMAGE_DIR NELISP_LANE_IMAGE NELISP_LANE_IMAGE_SHA256; do
  [ -n "${!k:-}" ] || refuse "$k is required"
done
[[ $NELISP_LANE_IMAGE_SHA256 =~ $HEX64 ]] || refuse "NELISP_LANE_IMAGE_SHA256 is not a lowercase sha256"
DIR=$NELISP_APP_IMAGE_DIR
IMG_PATH=$DIR/$ROW.img; MAIN_PATH=$DIR/$ROW.img.main.el; MAN_PATH=$DIR/$ROW.img.manifest

# --- open once, hash from the descriptor, compare ---
# The files are opened and the two hashes started before the manifest is
# parsed so that the (dominant) image hash overlaps the rest of the
# preparation; open failures are reported in the same order as launch.py
# (after the manifest checks) and nothing is compared or spawned before the
# hashes are read below.
OPEN_ERR=""
{ exec {MAIN_FD}<"$MAIN_PATH"; } 2>/dev/null || OPEN_ERR="main file $MAIN_PATH"
if [ -z "$OPEN_ERR" ]; then
  { exec {IMG_FD}<"$IMG_PATH"; } 2>/dev/null || OPEN_ERR="image $IMG_PATH"
fi
if [ -z "$OPEN_ERR" ]; then
  exec {HM}< <(sha256sum "/dev/fd/$MAIN_FD") || fail "cannot start sha256sum"
  exec {HI}< <(sha256sum "/dev/fd/$IMG_FD") || fail "cannot start sha256sum"
  STAT_BEFORE=$(stat -L -c '%d:%i:%s:%.9Y:%.9Z' "/dev/fd/$MAIN_FD" "/dev/fd/$IMG_FD") || fail "stat of opened files"
fi

# --- strict manifest parse (same rules as launch.py parse_manifest) ---
declare -A M
[ -r "$MAN_PATH" ] && [ -f "$MAN_PATH" ] || refuse "cannot read manifest $MAN_PATH: not a readable file"
n=0
while IFS= read -r line || [ -n "$line" ]; do
  n=$((n + 1))
  [ -z "$line" ] && continue
  case "$line" in *" "*) key=${line%% *}; val=${line#* } ;; *) key=$line; val="" ;; esac
  if [ -z "$val" ] || [[ $val =~ ^[[:space:]] ]] || [[ $val =~ [[:space:]]$ ]]; then
    refuse "manifest line $n malformed: '$line'"
  fi
  case "$KEYS" in *" $key "*) [ -n "$key" ] || refuse "manifest has unknown key ''" ;;
    *) refuse "manifest has unknown key '$key'" ;; esac
  [ -z "${M[$key]+x}" ] || refuse "manifest has duplicate key '$key'"
  M[$key]=$val
done <"$MAN_PATH" || refuse "cannot read manifest $MAN_PATH"
missing=""
for k in $KEYS; do [ -n "${M[$k]+x}" ] || missing+=" $k"; done
[ -z "$missing" ] || refuse "manifest is missing keys:$missing"
[ "${M[format]}" = "$FORMAT" ] || refuse "manifest format '${M[format]}', expected '$FORMAT'"
for k in image-sha256 binary-sha256 driver-sha256 main-sha256 prep-sha256; do
  [[ ${M[$k]} =~ $HEX64 ]] || refuse "manifest $k is not a sha256: '${M[$k]}'"
done
if [ "${M[bootstrap-sha256]}" != none ] && ! [[ ${M[bootstrap-sha256]} =~ $HEX64 ]]; then
  refuse "manifest bootstrap-sha256 is neither a sha256 nor 'none'"
fi
rows_ok=1
[[ ${M[rows]} =~ ^[^\ ]+(\ [^\ ]+)*$ ]] || rows_ok=0
if [ $rows_ok = 1 ]; then
  for r in ${M[rows]}; do case "$ROWS" in *" $r "*) ;; *) rows_ok=0 ;; esac; done
  case "$ROWS" in *" ${M[row]} "*) ;; *) rows_ok=0 ;; esac
  case " ${M[rows]} " in *" ${M[row]} "*) ;; *) rows_ok=0 ;; esac
fi
[ $rows_ok = 1 ] || refuse "manifest rows/row invalid: row='${M[row]}' rows='${M[rows]}'"
[ "${M[row]}" = "$ROW" ] || refuse "manifest row '${M[row]}' differs from BENCH_ROW '$ROW'"
[ "${M[binary-sha256]}" = "$NELISP_LANE_IMAGE_SHA256" ] || refuse "manifest binary-sha256 differs from NELISP_LANE_IMAGE_SHA256"
BINARY=$NELISP_LANE_IMAGE
[ -f "$BINARY" ] || refuse "binary not found: $BINARY"
case "$BINARY" in /*) ;; *) BINARY=$PWD/$BINARY ;; esac
mark manifest
[ -z "$OPEN_ERR" ] || refuse "cannot open $OPEN_ERR"

# --- scratch and child environment ---
SROOT=${NELISP_SCRATCH_ROOT:-${TMPDIR:-/tmp}}
mkdir -p "$SROOT" || fail "cannot create scratch root $SROOT"
SROOT=$(cd "$SROOT" && pwd -P) || fail "cannot resolve scratch root"
SCRATCH=$(mktemp -d "$SROOT/nelisp-exp-XXXXXXXX") || fail "cannot create scratch dir"
mkdir "$SCRATCH/cache" || fail "cannot create scratch cache"
CENV=()
declare -A EXPORTED
while read -r v; do EXPORTED[$v]=1; done < <(compgen -e)
for v in $ALLOWED_ENV; do [ -n "${EXPORTED[$v]+x}" ] && CENV+=("$v=${!v}"); done
for v in "${!EXPORTED[@]}"; do
  case "$v" in BENCH_ENGINE) ;; BENCH_*) CENV+=("$v=${!v}") ;; esac
done
CENV+=("NELISP_OWNED_SCRATCH=$SCRATCH" "TMPDIR=$SCRATCH" "XDG_CACHE_HOME=$SCRATCH/cache"
       "BENCH_ENGINE=nelisp" "NELISP_PRODUCER_IMAGE_SHA256=$NELISP_LANE_IMAGE_SHA256")
HOME_FOR_CHILD=${HOME-~}
mkfifo -m 600 "$SCRATCH/gate" || fail "cannot create start gate"
exec {GATE}<>"$SCRATCH/gate" || fail "cannot open start gate"
mark prepare
trap 'echo "launch: terminated by signal" >&2; [ -n "$PID" ] && kill -TERM -- "-$PID" 2>/dev/null; finish 143' TERM
trap 'echo "launch: interrupted" >&2; [ -n "$PID" ] && kill -TERM -- "-$PID" 2>/dev/null; finish 130' INT
set -m
env -i "${CENV[@]}" bwrap --ro-bind / / --bind "$SCRATCH" "$SCRATCH" \
  --proc /proc --dev /dev --unshare-user --unshare-pid --unshare-net --die-with-parent --new-session \
  --setenv HOME "$HOME_FOR_CHILD" --setenv TMPDIR "$SCRATCH" --setenv XDG_CACHE_HOME "$SCRATCH/cache" \
  -- /bin/bash -c 'ulimit -v 8388608; ulimit -c 0; read -r _ <"$1" || exit 126; shift; exec "$@"' lane \
  "$SCRATCH/gate" "$BINARY" --cold-load-from "/dev/fd/$IMG_FD" --load "/dev/fd/$MAIN_FD" </dev/null &
PID=$!
sleep "$TIMEOUT" & TPID=$!
mark spawn
# The sandbox (bwrap, bash, ulimit) is set up while the hashes finish, but the
# binary is not exec'd, and so cannot open the image, until the hashes of the
# opened descriptors matched the manifest.  A refusal kills the process group.
read -r -u "$HM" got _ || fail "sha256sum of main file produced no output"
[ "$got" = "${M[main-sha256]}" ] || refuse "main file sha256 $got differs from manifest ${M[main-sha256]}"
mark hash-main
read -r -u "$HI" got _ || fail "sha256sum of image produced no output"
[ "$got" = "${M[image-sha256]}" ] || refuse "image sha256 $got differs from manifest ${M[image-sha256]}"
mark hash-image

echo go >&"$GATE" || fail "cannot release start gate"
mark release
wait -n -p DONE "$PID" "$TPID"; RC=$?
if [ "${DONE:-}" = "$TPID" ]; then
  TPID=""
  kill -TERM -- "-$PID" 2>/dev/null
  sleep 2 & GPID=$!
  wait -n -p DONE2 "$PID" "$GPID"
  if [ "${DONE2:-}" = "$GPID" ]; then kill -KILL -- "-$PID" 2>/dev/null; wait "$PID" 2>/dev/null; else kill "$GPID" 2>/dev/null; fi
  wait "$GPID" 2>/dev/null
  PID=""
  echo "launch: lane process timed out; process group terminated" >&2
  finish 124
fi
PID=""
mark child
STAT_AFTER=$(stat -L -c '%d:%i:%s:%.9Y:%.9Z' "/dev/fd/$MAIN_FD" "/dev/fd/$IMG_FD") || fail "stat after run"
if [ "$STAT_AFTER" != "$STAT_BEFORE" ]; then
  echo "launch: image or main file was modified in place while the child ran" >&2
  finish 2
fi
finish "$RC"
