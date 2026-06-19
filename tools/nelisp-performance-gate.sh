#!/usr/bin/env bash
# Repeatable NeLisp performance gate for Doc 154.
#
# Covers:
#   - source-command normal load/eval
#   - source-command marker-cache load/eval
#   - opt-in arena stats output
#   - focused artifact ERT
#   - marker cleanup
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

EMACS="${EMACS:-emacs}"
BUILD=1
RUN_ERT=1

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-build) BUILD=0; shift ;;
    --no-ert) RUN_ERT=0; shift ;;
    --emacs) EMACS="$2"; shift 2 ;;
    *)
      echo "usage: $0 [--no-build] [--no-ert] [--emacs EMACS]" >&2
      exit 2
      ;;
  esac
done

ENABLE_MARKER="target/nelisp-artifact-runtime.el.nelc.enable"
STATS_MARKER="target/nelisp-source-command-cache.stats"
TRACE_FILE="target/nelisp-artifact-runtime.trace"
TMP_DIR="$(mktemp -d)"

cleanup() {
  rm -f "$ENABLE_MARKER" "$STATS_MARKER" "$TRACE_FILE"
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

rm -f "$STATS_MARKER" "$TRACE_FILE"
if [ "$BUILD" -eq 1 ]; then
  rm -f "$ENABLE_MARKER"
fi

if [ "$BUILD" -eq 1 ]; then
  make standalone-reader
fi

EXE="$REPO_ROOT/target/nelisp"
if [ ! -x "$EXE" ]; then
  echo "gate_fail reason=missing-executable path=$EXE" >&2
  exit 1
fi

MOD="$TMP_DIR/mod.el"
cat >"$MOD" <<'EOF'
(defun bench-native-smoke-add (x) (+ x 1))
(provide 'bench-native-smoke)
EOF

FALLBACK_MOD="$TMP_DIR/source-fallback.el"
cat >"$FALLBACK_MOD" <<'EOF'
(defvar source-fallback-value 42)
(provide 'source-fallback)
EOF

"$EXE" compile-elisp-artifact --kind neln --input "$MOD" --output "$MOD.neln" \
  >"$TMP_DIR/compile.out" 2>"$TMP_DIR/compile.err"

if [ ! -f "$ENABLE_MARKER" ]; then
  echo "gate_fail reason=missing-default-runtime-cache-enable-marker path=$ENABLE_MARKER" >&2
  exit 1
fi

if ! grep -Eq '^enabled [0-9a-f]{64}$' "$ENABLE_MARKER"; then
  echo "gate_fail reason=invalid-runtime-cache-enable-marker" >&2
  exit 1
fi

run_timed() {
  local label="$1"; shift
  local out_file="$TMP_DIR/$label.out"
  local err_file="$TMP_DIR/$label.err"
  local start end rc
  start="$(date +%s%3N)"
  set +e
  "$@" >"$out_file" 2>"$err_file"
  rc=$?
  set -e
  end="$(date +%s%3N)"
  printf 'gate_result label=%s rc=%s ms=%s out=%s\n' \
    "$label" "$rc" "$((end - start))" "$(tr '\n' ' ' <"$out_file" | sed 's/[[:space:]]*$//')"
  return "$rc"
}

expect_out() {
  local label="$1" expected="$2"
  local actual
  actual="$(cat "$TMP_DIR/$label.out")"
  if [ "$actual" != "$expected" ]; then
    printf 'gate_fail label=%s reason=output-mismatch expected=%s actual=%s\n' \
      "$label" "$expected" "$actual" >&2
    exit 1
  fi
}

run_timed cache_default_load "$EXE" load-elisp-source "$MOD"
expect_out cache_default_load "bench-native-smoke"

run_timed cache_default_eval "$EXE" eval-elisp-source "$MOD" '(bench-native-smoke-add 41)'
expect_out cache_default_eval "42"

rm -f "$ENABLE_MARKER"
run_timed fallback_load "$EXE" load-elisp-source "$MOD"
expect_out fallback_load "bench-native-smoke"

run_timed fallback_eval "$EXE" eval-elisp-source "$MOD" '(bench-native-smoke-add 41)'
expect_out fallback_eval "42"

make standalone-reader >/dev/null
if [ ! -f "$ENABLE_MARKER" ]; then
  echo "gate_fail reason=runtime-cache-enable-marker-not-restored-by-build" >&2
  exit 1
fi

run_timed cache_source_fallback_load "$EXE" load-elisp-source "$FALLBACK_MOD"
expect_out cache_source_fallback_load "source-fallback"

run_timed cache_source_fallback_eval "$EXE" eval-elisp-source "$FALLBACK_MOD" 'source-fallback-value'
expect_out cache_source_fallback_eval "42"

touch "$STATS_MARKER"
run_timed marker_stats "$EXE" eval-elisp-source "$MOD" '(bench-native-smoke-add 41)'
expect_out marker_stats "42"
if ! grep -q '^source-cache stats stage=artifact-after-module ' "$TMP_DIR/marker_stats.err"; then
  echo "gate_fail label=marker_stats reason=missing-artifact-after-module-stats" >&2
  exit 1
fi
grep '^source-cache stats stage=' "$TMP_DIR/marker_stats.err" | sed 's/^/gate_stats /'

rm -f "$ENABLE_MARKER" "$STATS_MARKER" "$TRACE_FILE"
if [ -e "$ENABLE_MARKER" ] || [ -e "$STATS_MARKER" ] || [ -e "$TRACE_FILE" ]; then
  echo "gate_fail reason=marker-cleanup" >&2
  exit 1
fi
echo "gate_result label=marker_absent rc=0"

if [ "$RUN_ERT" -eq 1 ]; then
  "$EMACS" -Q --batch \
    -L src -L lisp \
    -L packages/nelisp-json/src \
    -L packages/nelisp-process/src \
    -l test/nelisp-artifact-test.el \
    --eval "(ert-run-tests-batch-and-exit '(or nelisp-artifact/neln-generic-allows-bytecode-only-module nelisp-artifact/source-loader-installs-adjacent-neln-native-wrapper nelisp-artifact/eval-elisp-source-prefers-adjacent-neln nelisp-artifact/load-elisp-source-auto-compiles-neln))"
  echo "gate_result label=focused_artifact_ert rc=0"
fi

echo "gate_result label=nelisp_performance_gate rc=0"
