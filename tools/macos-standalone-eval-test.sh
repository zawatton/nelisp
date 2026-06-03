#!/usr/bin/env bash
# macOS arm64 standalone eval smoke.
#
# Builds target/nelisp-standalone-eval as a pure-elisp Mach-O executable.
# On macOS arm64 it also executes the binary and checks the exit code.
set -euo pipefail

EMACS="${EMACS:-emacs}"
OP="+"
A=1
B=2
BUILD_ONLY=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --op) OP="$2"; shift 2 ;;
    --a) A="$2"; shift 2 ;;
    --b) B="$2"; shift 2 ;;
    --build-only|--emit-only) BUILD_ONLY=1; shift ;;
    *) echo "usage: $0 [--op +|-|*] [--a N] [--b N] [--build-only]" >&2; exit 2 ;;
  esac
done

case "$OP" in
  +) EXPECTED=$((A + B)) ;;
  -) EXPECTED=$((A - B)) ;;
  '*') EXPECTED=$((A * B)) ;;
  *) echo "[macos-standalone-eval] FAIL: unsupported op $OP" >&2; exit 2 ;;
esac

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

echo "--- macOS standalone eval smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET=macos-aarch64
export NELISP_FORM_OP="$OP"
export NELISP_FORM_A="$A"
export NELISP_FORM_B="$B"

"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  -f nelisp-standalone-build

EXE="$REPO_ROOT/target/nelisp-standalone-eval"
if [ ! -f "$EXE" ]; then
  echo "[macos-standalone-eval] FAIL: missing $EXE"
  exit 1
fi

if [ "$(uname -s)" != "Darwin" ] || [ "$(uname -m)" != "arm64" ] || [ "$BUILD_ONLY" -eq 1 ]; then
  file "$EXE"
  echo "[macos-standalone-eval] build-only PASS: $EXE"
  exit 0
fi

codesign -f -s - "$EXE" >/dev/null
set +e
"$EXE"
CODE=$?
set -e

if [ "$CODE" -eq "$EXPECTED" ]; then
  echo "[macos-standalone-eval] PASS: $EXE -> exit $CODE (expected $EXPECTED)"
  exit 0
fi

echo "[macos-standalone-eval] FAIL: $EXE -> exit $CODE (expected $EXPECTED)"
exit 1
