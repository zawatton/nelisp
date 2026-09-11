#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$script_dir/.." && pwd)
run_parent=${TMPDIR:-"$root/target/tmp"}
mkdir -p "$run_parent" "$root/target/ai"
run_dir=$(mktemp -d "$run_parent/nelisp-repl-gc-smoke.XXXXXX")
stdout_file=$run_dir/stdout
stderr_file=$run_dir/stderr
log_file=$root/target/ai/nelisp-repl-gc-smoke.log
binary=${NELISP_BIN:-"$root/target/nelisp-runtime-reload"}
fixture=${NELISP_REPL_GC_FIXTURE:-"$script_dir/fixtures/nelisp-repl-gc-smoke.repl"}

hash_file() {
  if command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$1" | awk '{print $1}'
  elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "$1" | awk '{print $1}'
  else
    printf '%s' unavailable
  fi
}

record_log() {
  {
    printf 'binary=%s\n' "$binary"
    printf 'fixture=%s\n' "$fixture"
    printf 'binary_sha256=%s\n' "$(hash_file "$binary")"
    printf 'run_dir=%s\n' "$run_dir"
    printf 'exit=%s\n' "$1"
    printf '%s\n' '--- stdout ---'
    cat "$stdout_file"
    printf '%s\n' '--- stderr ---'
    cat "$stderr_file"
  } >"$log_file"
}

if [ ! -x "$binary" ]; then
  printf 'FAIL: executable binary not found: %s\n' "$binary" >&2
  exit 1
fi
if [ ! -r "$fixture" ]; then
  printf 'FAIL: fixture not readable: %s\n' "$fixture" >&2
  exit 1
fi

actual_hash=$(hash_file "$binary")
if [ -n "${NELISP_REPL_GC_EXPECTED_SHA256-}" ] &&
   [ "$actual_hash" != "$NELISP_REPL_GC_EXPECTED_SHA256" ]; then
  printf 'FAIL: binary hash mismatch: got %s expected %s\n' \
    "$actual_hash" "$NELISP_REPL_GC_EXPECTED_SHA256" >&2
  exit 1
fi

set +e
NELISP_BIN="$binary" \
NELISP_REPL_GC_FILE="$root/lisp/nelisp-repl-gc.el" \
NELISP_NATIVE_LOAD_FILE="$root/lisp/nelisp-native-load.el" \
NELISP_ABI_FILE="$root/lisp/nelisp-runtime-reload-abi.el" \
TMPDIR="$run_dir" \
bash "$root/tools/ai/nelisp-ai.sh" repl --no-prompt --no-print \
  <"$fixture" >"$stdout_file" 2>"$stderr_file"
rc=$?
set -e
record_log "$rc"

if [ "$rc" -ne 0 ]; then
  printf 'FAIL: REPL exit=%s (see %s)\n' "$rc" "$log_file" >&2
  exit 1
fi
if [ -s "$stderr_file" ]; then
  printf 'FAIL: REPL stderr is not empty (see %s)\n' "$log_file" >&2
  exit 1
fi
marker_count=$(grep -Ec '^REPL_GC_SMOKE=PASS ATTEMPTS=[1-9][0-9]* SCANNED=[0-9]+ PINNED=[0-9]+ FLAGS=(t|nil) STATUS=:available$' "$stdout_file" || true)
if [ "$marker_count" -ne 1 ]; then
  printf 'FAIL: expected one validated final marker, got %s (see %s)\n' \
    "$marker_count" "$log_file" >&2
  exit 1
fi

printf 'PASS: REPL GC smoke (binary_sha256=%s, log=%s)\n' \
  "$actual_hash" "$log_file"
