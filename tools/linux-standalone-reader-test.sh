#!/usr/bin/env bash
# Linux x86_64 standalone reader smoke.
#
# Builds target/nelisp as a pure-elisp ELF executable and exercises embedded
# source, file-argument source, --eval/--load commands, and REPL stdin/stdout.
# No Rust toolchain is used.
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

echo "--- Linux standalone reader smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET=linux-x86_64
export NELISP_SRC="$SOURCE"

"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  -f nelisp-standalone-build-reader

EXE="$REPO_ROOT/target/nelisp"
if [ ! -f "$EXE" ]; then
  echo "[linux-standalone-reader] FAIL: missing $EXE"
  exit 1
fi

if [ "$(uname -s)" != "Linux" ] || [ "$(uname -m)" != "x86_64" ] || [ "$BUILD_ONLY" -eq 1 ]; then
  file "$EXE"
  echo "[linux-standalone-reader] build-only PASS: $EXE"
  exit 0
fi

chmod +x "$EXE"

run_expect_code() {
  local label="$1" want="$2"; shift 2
  set +e
  "$@"
  local code=$?
  set -e
  if [ "$code" -ne "$want" ]; then
    echo "[linux-standalone-reader] FAIL: $label -> exit $code (expected $want)"
    exit 1
  fi
  echo "[linux-standalone-reader] PASS: $label -> exit $want"
}

run_expect_output() {
  local label="$1" expected="$2"; shift 2
  local output code
  set +e
  output="$("$@")"
  code=$?
  set -e
  if [ "$code" -ne 0 ]; then
    echo "[linux-standalone-reader] FAIL: $label exited $code"
    echo "$output"
    exit 1
  fi
  if [ "$output" != "$expected" ]; then
    echo "[linux-standalone-reader] FAIL: $label output mismatch"
    printf 'expected: %s\nactual  : %s\n' "$expected" "$output"
    exit 1
  fi
  echo "[linux-standalone-reader] PASS: $label"
}

run_expect_code_output_contains() {
  local label="$1" want="$2" needle="$3"; shift 3
  local output code
  set +e
  output="$("$@")"
  code=$?
  set -e
  if [ "$code" -ne "$want" ]; then
    echo "[linux-standalone-reader] FAIL: $label -> exit $code (expected $want)"
    echo "$output"
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -q "$needle"; then
    echo "[linux-standalone-reader] FAIL: $label output missing $needle"
    echo "$output"
    exit 1
  fi
  echo "[linux-standalone-reader] PASS: $label -> exit $want"
}

run_expect_code "embedded src=$SOURCE" "$EXPECTED" "$EXE" --embedded

if [ "$EMBEDDED_ONLY" -eq 1 ]; then
  exit 0
fi

SMOKE_DIR="$REPO_ROOT/target/linux-standalone-reader"
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
  echo "[linux-standalone-reader] FAIL: --help -> exit $HELP_CODE"
  printf '%s\n' "$HELP_OUTPUT"
  exit 1
fi
echo "[linux-standalone-reader] PASS: --help"

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

REPL_OUTPUT="$(printf '%s\n' \
  "(+ 40 2)" \
  "nil" \
  "t" \
  "(quote (1 2 3))" \
  '(vector 1 "a" nil t)' \
  "(exit)" | "$EXE" --repl --no-prompt)"
EXPECTED_REPL=$'42\nnil\nt\n(1 2 3)\n[1 "a" nil t]'
if [ "$REPL_OUTPUT" != "$EXPECTED_REPL" ]; then
  echo "[linux-standalone-reader] FAIL: repl stdin/stdout output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "$EXPECTED_REPL" "$REPL_OUTPUT"
  exit 1
fi
echo "[linux-standalone-reader] PASS: repl stdin/stdout -> 42"

QUIET_REPL_OUTPUT="$(printf '%s\n' \
  "(defun hot () 1)" \
  "(hot)" \
  "(condition-case e (signal 'quit nil) (quit 42))" \
  '(nelisp--write-stdout-bytes "explicit\n")' \
  "(hot)" \
  "(exit)" | "$EXE" --repl --no-prompt --no-print)"
if [ "$QUIET_REPL_OUTPUT" != "explicit" ]; then
  echo "[linux-standalone-reader] FAIL: repl --no-print output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "explicit" "$QUIET_REPL_OUTPUT"
  exit 1
fi
echo "[linux-standalone-reader] PASS: repl --no-print"

NO_ARGS_REPL_OUTPUT="$(printf '%s\n' "(exit)" | "$EXE")"
if [ "$NO_ARGS_REPL_OUTPUT" != "nelisp> " ]; then
  echo "[linux-standalone-reader] FAIL: no-args repl output mismatch"
  printf 'expected:\n%s\nactual:\n%s\n' "nelisp> " "$NO_ARGS_REPL_OUTPUT"
  exit 1
fi
echo "[linux-standalone-reader] PASS: no-args repl"

run_expect_code "--repl bad option" 2 "$EXE" --repl --bad
echo "[linux-standalone-reader] all PASS - Linux-native standalone reader OK"
