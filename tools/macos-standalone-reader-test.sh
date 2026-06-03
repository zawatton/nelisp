#!/usr/bin/env bash
# macOS arm64 standalone reader smoke.
#
# Builds target/nelisp as a pure-elisp Mach-O executable.  On macOS arm64 it
# exercises embedded source, file-argument source, eval/load commands, and REPL
# stdin/stdout.  No Rust toolchain is used.
set -euo pipefail

EMACS="${EMACS:-emacs}"
SOURCE="(+ 40 2)"
EXPECTED=42
BUILD_ONLY=0
EMBEDDED_ONLY=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --source) SOURCE="$2"; shift 2 ;;
    --expected) EXPECTED="$2"; shift 2 ;;
    --build-only|--emit-only) BUILD_ONLY=1; shift ;;
    --embedded-only) EMBEDDED_ONLY=1; shift ;;
    *) echo "usage: $0 [--source FORM] [--expected N] [--build-only] [--embedded-only]" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "--- macOS standalone reader smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET=macos-aarch64
export NELISP_SRC="$SOURCE"

"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  -f nelisp-standalone-build-reader

EXE="$REPO_ROOT/target/nelisp"
if [ ! -f "$EXE" ]; then
  echo "[macos-standalone-reader] FAIL: missing $EXE"
  exit 1
fi

if [ "$(uname -s)" != "Darwin" ] || [ "$(uname -m)" != "arm64" ] || [ "$BUILD_ONLY" -eq 1 ]; then
  file "$EXE"
  echo "[macos-standalone-reader] build-only PASS: $EXE"
  exit 0
fi

codesign -f -s - "$EXE" >/dev/null

run_expect_code() {
  local label="$1" want="$2"; shift 2
  set +e
  "$@"
  local code=$?
  set -e
  if [ "$code" -ne "$want" ]; then
    echo "[macos-standalone-reader] FAIL: $label -> exit $code (expected $want)"
    if command -v lldb >/dev/null 2>&1; then
      echo "[macos-standalone-reader] lldb backtrace for: $*"
      set +e
      lldb --batch -o run -o "bt all" -- "$@"
      set -e
    fi
    exit 1
  fi
  echo "[macos-standalone-reader] PASS: $label -> exit $want"
}

run_expect_output() {
  local label="$1" expected="$2"; shift 2
  local output code
  set +e
  output="$("$@")"
  code=$?
  set -e
  if [ "$code" -ne 0 ]; then
    echo "[macos-standalone-reader] FAIL: $label exited $code"
    echo "$output"
    exit 1
  fi
  if [ "$output" != "$expected" ]; then
    echo "[macos-standalone-reader] FAIL: $label output mismatch"
    printf 'expected: %s\nactual  : %s\n' "$expected" "$output"
    exit 1
  fi
  echo "[macos-standalone-reader] PASS: $label"
}

run_expect_code "embedded src=$SOURCE" "$EXPECTED" "$EXE"

if [ "$EMBEDDED_ONLY" -eq 1 ]; then
  exit 0
fi

SMOKE_DIR="$REPO_ROOT/target/macos-standalone-reader"
mkdir -p "$SMOKE_DIR"

FILE_SMOKE="$SMOKE_DIR/file-smoke.el"
SPACED_FILE_SMOKE="$SMOKE_DIR/file smoke spaced.el"
UNICODE_FILE_SMOKE="$SMOKE_DIR/unicode-あ.el"
printf '%s\n' "(+ 39 3)" >"$FILE_SMOKE"
printf '%s\n' "(* 6 7)" >"$SPACED_FILE_SMOKE"
printf '%s\n' "(- 50 8)" >"$UNICODE_FILE_SMOKE"

run_expect_code "file arg" 42 "$EXE" "$FILE_SMOKE"
run_expect_code "file arg with spaces" 42 "$EXE" "$SPACED_FILE_SMOKE"
run_expect_code "unicode file arg" 42 "$EXE" "$UNICODE_FILE_SMOKE"

HELP_OUTPUT="$("$EXE" --help)"
if ! printf '%s\n' "$HELP_OUTPUT" | grep -q "Usage: nelisp"; then
  echo "[macos-standalone-reader] FAIL: --help"
  printf '%s\n' "$HELP_OUTPUT"
  exit 1
fi
echo "[macos-standalone-reader] PASS: --help"

run_expect_output "eval" "42" "$EXE" eval "(+ 40 2)"
run_expect_output "-e" '[1 "a" nil t]' "$EXE" -e '(vector 1 "a" nil t)'
run_expect_output "load" "42" "$EXE" load "$FILE_SMOKE"

RUNTIME_IMAGE="$SMOKE_DIR/runtime-smoke.nlri"
run_expect_output "dump-runtime-image" "" \
  "$EXE" dump-runtime-image "$RUNTIME_IMAGE" "(setq base 40)"
run_expect_output "eval-runtime-image" "42" \
  "$EXE" eval-runtime-image "$RUNTIME_IMAGE" "(setq add 2)" "(+ base add)"
run_expect_output "exec-runtime-image" "" \
  "$EXE" exec-runtime-image "$RUNTIME_IMAGE" "(setq add 2)" "(+ base add)"
run_expect_code "exec-runtime-image missing form" 1 \
  "$EXE" exec-runtime-image "$RUNTIME_IMAGE"

REPL_OUTPUT="$(printf '%s\n' \
  "(+ 40 2)" \
  "nil" \
  "t" \
  "(quote (1 2 3))" \
  '(vector 1 "a" nil t)' \
  "(exit)" | "$EXE" repl --no-prompt)"
EXPECTED_REPL=$'42\nnil\nt\n(1 2 3)\n[1 "a" nil t]'
if [ "$REPL_OUTPUT" != "$EXPECTED_REPL" ]; then
  echo "[macos-standalone-reader] FAIL: repl stdin/stdout output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "$EXPECTED_REPL" "$REPL_OUTPUT"
  exit 1
fi
echo "[macos-standalone-reader] PASS: repl stdin/stdout -> 42"

QUIET_REPL_OUTPUT="$(printf '%s\n' \
  "(defun hot () 1)" \
  "(hot)" \
  "(condition-case e (signal 'quit nil) (quit 42))" \
  '(nelisp--write-stdout-bytes "explicit\n")' \
  "(hot)" \
  "(exit)" | "$EXE" repl --no-prompt --no-print)"
if [ "$QUIET_REPL_OUTPUT" != "explicit" ]; then
  echo "[macos-standalone-reader] FAIL: repl --no-print output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "explicit" "$QUIET_REPL_OUTPUT"
  exit 1
fi
echo "[macos-standalone-reader] PASS: repl --no-print"

run_expect_code "repl bad option" 2 "$EXE" repl --bad
echo "[macos-standalone-reader] all PASS - macOS-native standalone reader OK"
