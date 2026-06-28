#!/usr/bin/env bash
# macOS standalone reader smoke.
#
# Builds a pure-elisp Mach-O executable.  On native macOS it
# exercises embedded source, file-argument source, --eval/--load commands, and REPL
# stdin/stdout.  No Rust toolchain is used.
set -euo pipefail

EMACS="${EMACS:-emacs}"
SOURCE="(+ 40 2)"
EXPECTED=42
BUILD_ONLY=0
EMBEDDED_ONLY=0
TARGET=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --source) SOURCE="$2"; shift 2 ;;
    --expected) EXPECTED="$2"; shift 2 ;;
    --target) TARGET="$2"; shift 2 ;;
    --build-only|--emit-only) BUILD_ONLY=1; shift ;;
    --embedded-only) EMBEDDED_ONLY=1; shift ;;
    *) echo "usage: $0 [--source FORM] [--expected N] [--target macos-aarch64|macos-x86_64] [--build-only] [--embedded-only]" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

if [ -z "$TARGET" ]; then
  case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
    Darwin-arm64) TARGET="macos-aarch64" ;;
    Darwin-x86_64) TARGET="macos-x86_64" ;;
    *) TARGET="macos-aarch64" ;;
  esac
fi

echo "--- macOS standalone reader smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET="$TARGET"
export NELISP_SRC="$SOURCE"

"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  -f nelisp-standalone-build-reader

case "$TARGET" in
  macos-aarch64) EXE="$REPO_ROOT/target/nelisp-macos-aarch64" ;;
  macos-x86_64) EXE="$REPO_ROOT/target/nelisp-macos-x86_64" ;;
  *) echo "[macos-standalone-reader] FAIL: unsupported target $TARGET" >&2; exit 2 ;;
esac
if [ ! -f "$EXE" ]; then
  echo "[macos-standalone-reader] FAIL: missing $EXE"
  exit 1
fi

HOST_TARGET=""
case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
  Darwin-arm64) HOST_TARGET="macos-aarch64" ;;
  Darwin-x86_64) HOST_TARGET="macos-x86_64" ;;
esac
if [ "$HOST_TARGET" != "$TARGET" ] || [ "$BUILD_ONLY" -eq 1 ]; then
  file "$EXE"
  echo "[macos-standalone-reader] build-only PASS: $EXE"
  exit 0
fi

if command -v codesign >/dev/null 2>&1; then
  codesign -f -s - "$EXE" >/dev/null
fi

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

run_expect_code_output_contains() {
  local label="$1" want="$2" needle="$3"; shift 3
  local output code
  set +e
  output="$("$@")"
  code=$?
  set -e
  if [ "$code" -ne "$want" ]; then
    echo "[macos-standalone-reader] FAIL: $label -> exit $code (expected $want)"
    echo "$output"
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -q "$needle"; then
    echo "[macos-standalone-reader] FAIL: $label output missing $needle"
    echo "$output"
    exit 1
  fi
  echo "[macos-standalone-reader] PASS: $label -> exit $want"
}

run_lldb_with_stdin() {
  local input="$1"; shift
  if command -v lldb >/dev/null 2>&1; then
    echo "[macos-standalone-reader] lldb backtrace for stdin case: $* < $input"
    set +e
    lldb --batch \
      -o "process launch --stdin '$input'" \
      -o "bt all" \
      -- "$@"
    set -e
  fi
}

run_expect_pipe_output() {
  local label="$1" expected="$2" input="$3"; shift 3
  local output code
  set +e
  output="$(cat "$input" | "$@")"
  code=$?
  set -e
  if [ "$code" -ne 0 ]; then
    echo "[macos-standalone-reader] FAIL: $label exited $code"
    printf 'stdout:\n%s\n' "$output"
    run_lldb_with_stdin "$input" "$@"
    exit 1
  fi
  if [ "$output" != "$expected" ]; then
    echo "[macos-standalone-reader] FAIL: $label output mismatch"
    printf 'expected:\n%s\nactual:\n%s\n' "$expected" "$output"
    exit 1
  fi
  echo "[macos-standalone-reader] PASS: $label"
}

run_expect_code "embedded src=$SOURCE" "$EXPECTED" "$EXE" --embedded

if [ "$EMBEDDED_ONLY" -eq 1 ]; then
  exit 0
fi

SMOKE_DIR="$REPO_ROOT/target/macos-standalone-reader"
mkdir -p "$SMOKE_DIR"

FILE_SMOKE="$SMOKE_DIR/file-smoke.el"
SPACED_FILE_SMOKE="$SMOKE_DIR/file smoke spaced.el"
UNICODE_FILE_SMOKE="$SMOKE_DIR/unicode-あ.el"
printf '%s\n' "(+ 40 3)" >"$FILE_SMOKE"
printf '%s\n' "(+ 40 4)" >"$SPACED_FILE_SMOKE"
printf '%s\n' "(+ 40 5)" >"$UNICODE_FILE_SMOKE"

run_expect_code "file arg" 43 "$EXE" "$FILE_SMOKE"
run_expect_code "file arg with spaces" 44 "$EXE" "$SPACED_FILE_SMOKE"
run_expect_code "unicode file arg" 45 "$EXE" "$UNICODE_FILE_SMOKE"

set +e
HELP_OUTPUT="$("$EXE" --help)"
HELP_CODE=$?
set -e
if [ "$HELP_CODE" -ne 0 ] || ! printf '%s\n' "$HELP_OUTPUT" | grep -q "Usage: nelisp"; then
  echo "[macos-standalone-reader] FAIL: --help -> exit $HELP_CODE"
  printf '%s\n' "$HELP_OUTPUT"
  exit 1
fi
echo "[macos-standalone-reader] PASS: --help"

run_expect_output "--eval" "42" "$EXE" --eval "(+ 40 2)"
run_expect_output "--load" "43" "$EXE" --load "$FILE_SMOKE"
run_expect_code_output_contains "bare eval rejected" 2 "Arguments:" \
  "$EXE" eval "(+ 40 2)"
run_expect_code_output_contains "unknown option rejected" 2 "Arguments:" \
  "$EXE" --bad-option
run_expect_code_output_contains "--help extra arg rejected" 2 "Arguments:" \
  "$EXE" --help extra
run_expect_code_output_contains "--eval missing expr rejected" 2 "Arguments:" \
  "$EXE" --eval
run_expect_code_output_contains "--load missing file rejected" 2 "Arguments:" \
  "$EXE" --load

RUNTIME_IMAGE="$SMOKE_DIR/runtime-smoke.nlri"
run_expect_output "dump-runtime-image" "" \
  "$EXE" dump-runtime-image "$RUNTIME_IMAGE" "(setq base 40)"
run_expect_output "eval-runtime-image" "42" \
  "$EXE" eval-runtime-image "$RUNTIME_IMAGE" "(setq add 2)" "(+ base add)"
RUNTIME_FN_IMAGE="$SMOKE_DIR/runtime-smoke-fn.nlri"
run_expect_output "dump-runtime-image defun" "" \
  "$EXE" dump-runtime-image "$RUNTIME_FN_IMAGE" "(defun image-hot () 99)"
run_expect_output "eval-runtime-image defun" "99" \
  "$EXE" eval-runtime-image "$RUNTIME_FN_IMAGE" "(image-hot)"
RUNTIME_LOAD_SRC="$SMOKE_DIR/runtime-load-src.el"
RUNTIME_LOAD_IMAGE="$SMOKE_DIR/runtime-load-smoke.nlri"
printf '%s\n' '(setq loaded-base 39)' '(defun loaded-hot () 3)' >"$RUNTIME_LOAD_SRC"
run_expect_output "dump-runtime-image --load" "" \
  "$EXE" dump-runtime-image "$RUNTIME_LOAD_IMAGE" --load "$RUNTIME_LOAD_SRC" "(setq loaded-add 0)"
run_expect_output "eval-runtime-image --load" "42" \
  "$EXE" eval-runtime-image "$RUNTIME_LOAD_IMAGE" "(+ loaded-base loaded-add (loaded-hot))"
run_expect_output "exec-runtime-image" "" \
  "$EXE" exec-runtime-image "$RUNTIME_IMAGE" "(setq add 2)" "(+ base add)"
run_expect_code "exec-runtime-image missing form" 1 \
  "$EXE" exec-runtime-image "$RUNTIME_IMAGE"

REPL_INPUT="$SMOKE_DIR/repl-input.el"
cat >"$REPL_INPUT" <<'EOF'
(+ 40 2)
nil
t
(quote (1 2 3))
(vector 1 "a" nil t)
(exit)
EOF
EXPECTED_REPL=$'42\nnil\nt\n(1 2 3)\n[1 "a" nil t]'
run_expect_pipe_output "repl stdin/stdout" "$EXPECTED_REPL" "$REPL_INPUT" \
  "$EXE" --repl --no-prompt

QUIET_REPL_INPUT="$SMOKE_DIR/repl-quiet-input.el"
cat >"$QUIET_REPL_INPUT" <<'EOF'
(defun hot () 1)
(hot)
(condition-case e (signal 'quit nil) (quit 42))
(nelisp--write-stdout-bytes "explicit\n")
(hot)
(exit)
EOF
run_expect_pipe_output "repl --no-print" "explicit" "$QUIET_REPL_INPUT" \
  "$EXE" --repl --no-prompt --no-print

NO_ARGS_REPL_INPUT="$SMOKE_DIR/repl-no-args-input.el"
cat >"$NO_ARGS_REPL_INPUT" <<'EOF'
(exit)
EOF
run_expect_pipe_output "no-args repl" "nelisp> " "$NO_ARGS_REPL_INPUT" "$EXE"

run_expect_code "--repl bad option" 2 "$EXE" --repl --bad
echo "[macos-standalone-reader] all PASS - macOS-native standalone reader OK"
