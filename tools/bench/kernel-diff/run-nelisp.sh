#!/bin/sh
# Usage: run-nelisp.sh [native|interp] [OUTNAME]
#  native: admitted native kernels installed by the driver (drv.orig minus its final (kill-emacs 0)) then kernel-cases.el.
#  interp: same, but kernels are first redefined from kernels.el (best-effort interpreted reference; no native mapping guaranteed).
# Restores the driver from drv.orig afterwards.
MODE=${1:-native}; K=$(cd "$(dirname "$0")" && pwd); OUT=${2:-nelisp.out}
# KD_RUN/KD_IMAGE/KD_SHA select another repinned run dir and image (defaults: build5).
B=${NELISP_BUILDS:-$HOME/.cache/tmp/nelisp-sbcl-impl-builds}; D=${KD_RUN:-$B/run/b5}
IMG=${KD_IMAGE:-$B/build-on5/nelisp-dynamic}
SHA=${KD_SHA:-72281d97d8844d020da97110e5898ab23558d3b35aed3c00e83aeabd91d9f771}
[ "$(sha256sum "$IMG" | cut -d' ' -f1)" = "$SHA" ] || { echo "image sha mismatch"; exit 3; }
L=$(grep -n '^(kill-emacs 0)' "$D/drv.orig" | cut -d: -f1)
{ head -n $((L-1)) "$D/drv.orig"
  if [ "$MODE" = interp ]; then grep '^(defun' "$K/kernels.el"; fi
  if [ "$MODE" = native ]; then echo '(dolist (n (quote (bench-vector-read bench-vector-write bench-vector-allocate64 bench-string-concat16 bench-string-slice16 bench-hash-get1024))) (princ (format "NATIVE-PROBE %s identity-eq=%s row=%s\n" n (eq (symbol-function n) (cdr (assq n nelisp-runtime-driver-preparation--installed-identities))) (nth 1 (assq n nelisp-nc--table)))))'; fi
  grep -v '^;;; ' "$K/kernel-cases.el"; echo '(kill-emacs 0)'; } > "$D/benchmark-driver-native-on.el"
(ulimit -v 8388608; ulimit -c 0; cd "$D" && env NELISP_LANE=on NELISP_LANE_IMAGE="$IMG" NELISP_LANE_IMAGE_SHA256=$SHA NELISP_SCRATCH_ROOT="$D/scratch" BENCH_ROW=integer-sum BENCH_N=1000 BENCH_HIGH=1000 BENCH_WARMUPS=0 timeout 300 python3 -I "$D/lane_runner_phased.py" --load x >"$K/$OUT" 2>"$K/$OUT.err")
echo "exit=$?"
cp "$D/drv.orig" "$D/benchmark-driver-native-on.el"
