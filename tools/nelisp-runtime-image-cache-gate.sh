#!/usr/bin/env bash
# Runtime-image artifact cache gate for Doc 154 Stage B.
#
# This is the repeatable comparison harness for moving source replay work
# toward a precompiled cache.  The fixture is deliberately small; the gate
# proves the command contract and freshness behavior before the same path is
# widened to the evaluator substrate.
set -euo pipefail

# A gate whose only output is its own result lines cannot be told apart from a
# gate that ran nothing.  CASES counts the checks that finished; the trap
# reports it however the script exits, so a failure says how far it got.
# Findings comes from the exit status: this script stops at the first failure.
CASES=0
gate_report_count() {
  gate_rc=$?
  if [ "$gate_rc" -eq 0 ]; then
    printf 'GATE-COUNT checked=%s findings=0\n' "$CASES"
  else
    printf 'GATE-COUNT checked=%s findings=1\n' "$CASES"
  fi
}


REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

BUILD=1
NELISP="${NELISP:-$REPO_ROOT/target/nelisp}"
TMP_DIR="$(mktemp -d)"

cleanup() {
  gate_report_count
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-build) BUILD=0; shift ;;
    --nelisp) NELISP="$2"; shift 2 ;;
    *)
      echo "usage: $0 [--no-build] [--nelisp PATH]" >&2
      exit 2
      ;;
  esac
done

# Needs a host that can run the configured target; ask the build script's
# own predicate rather than a bare `[ -x ]' test on $NELISP.  2026-08-23
# Windows inventory: the build produced target/nelisp (a linux-x86_64 ELF),
# but `[ -x ]' read false for it there, so this gate reported
# "missing-nelisp" (GATE-COUNT checked=0) for what is really an unrunnable
# target rather than a truly absent binary.  Same convention as
# tools/selfhost-test.sh.
set +e
"${EMACS:-emacs}" --batch -Q -L lisp -L src -L scripts -l nelisp-standalone-build \
  --eval '(kill-emacs (if (nelisp-standalone--target-runnable-on-host-p) 0 3))' \
  >/dev/null 2>&1
host_rc=$?
set -e
case "$host_rc" in
  0) ;;
  3)
    echo "GATE-SKIP target ${NELISP_STANDALONE_TARGET:-linux-x86_64} cannot run on host $(uname -s)/$(uname -m)"
    echo "runtime_image_cache_gate_result label=runtime_image_cache_gate rc=0 skipped=1"
    exit 0
    ;;
  *)
    echo "runtime_image_cache_gate_fail reason=cannot-ask-host-runnability rc=$host_rc" >&2
    exit 1
    ;;
esac

if [ "$BUILD" -eq 1 ]; then
  make standalone-reader
fi

if [ ! -x "$NELISP" ]; then
  echo "runtime_image_cache_gate_fail reason=missing-nelisp path=$NELISP" >&2
  exit 1
fi

IMAGE="$TMP_DIR/substrate-fixture.nlri"

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
  printf 'runtime_image_cache_gate_result label=%s rc=%s ms=%s out=%s\n' \
    "$label" "$rc" "$((end - start))" \
    "$(tr '\n' ' ' <"$out_file" | sed 's/[[:space:]]*$//')"
  if [ "$rc" -ne 0 ]; then
    sed 's/^/runtime_image_cache_gate_stderr /' "$err_file" >&2
    exit "$rc"
  fi
  CASES=$((CASES + 1))
}

expect_out() {
  local label="$1" expected="$2"
  local actual
  actual="$(cat "$TMP_DIR/$label.out")"
  if [ "$actual" != "$expected" ]; then
    printf 'runtime_image_cache_gate_fail label=%s reason=output-mismatch expected=%s actual=%s\n' \
      "$label" "$expected" "$actual" >&2
    exit 1
  fi
}

expect_file() {
  local path="$1"
  if [ ! -f "$path" ]; then
    echo "runtime_image_cache_gate_fail reason=missing-file path=$path" >&2
    exit 1
  fi
}

run_timed dump_image_v1 \
  "$NELISP" dump-runtime-image "$IMAGE" \
  '(defun substrate-cache-inc (x) (+ x 1))' \
  '(defvar substrate-cache-base 41)'
expect_out dump_image_v1 ""

run_timed source_replay_v1 \
  "$NELISP" eval-runtime-image "$IMAGE" '(substrate-cache-inc substrate-cache-base)'
expect_out source_replay_v1 "42"

run_timed cache_cold_v1 \
  "$NELISP" eval-runtime-image "$IMAGE" --cache-kind nelc \
  '(substrate-cache-inc substrate-cache-base)'
expect_out cache_cold_v1 "42"
expect_file "$IMAGE.nelc"
expect_file "$IMAGE.nelc.manifest.el"

run_timed cache_warm_v1 \
  "$NELISP" eval-runtime-image "$IMAGE" --cache-kind nelc \
  '(substrate-cache-inc substrate-cache-base)'
expect_out cache_warm_v1 "42"

run_timed dump_image_v2 \
  "$NELISP" dump-runtime-image "$IMAGE" \
  '(defun substrate-cache-inc (x) (+ x 3))' \
  '(defvar substrate-cache-base 40)'
expect_out dump_image_v2 ""

run_timed cache_refresh_v2 \
  "$NELISP" eval-runtime-image "$IMAGE" --cache-kind nelc \
  '(substrate-cache-inc substrate-cache-base)'
expect_out cache_refresh_v2 "43"

if ! grep -q ':runtime-image' "$IMAGE.nelc.manifest.el"; then
  echo "runtime_image_cache_gate_fail reason=missing-runtime-image-manifest-record" >&2
  exit 1
fi

CASES=$((CASES + 1))
echo "runtime_image_cache_gate_result label=runtime_image_cache_gate rc=0"
