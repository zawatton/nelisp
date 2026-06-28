#!/usr/bin/env bash
# macOS standalone eval smoke.
#
# Builds target/nelisp-standalone-eval-macos-* as a pure-elisp Mach-O executable.
# On native macOS it also executes the binary and checks the exit code.
set -euo pipefail

EMACS="${EMACS:-emacs}"
OP="+"
A=1
B=2
BUILD_ONLY=0
TARGET=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --op) OP="$2"; shift 2 ;;
    --a) A="$2"; shift 2 ;;
    --b) B="$2"; shift 2 ;;
    --target) TARGET="$2"; shift 2 ;;
    --build-only|--emit-only) BUILD_ONLY=1; shift ;;
    *) echo "usage: $0 [--op +|-|*] [--a N] [--b N] [--target macos-aarch64|macos-x86_64] [--build-only]" >&2; exit 2 ;;
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

if [ -z "$TARGET" ]; then
  case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
    Darwin-arm64) TARGET="macos-aarch64" ;;
    Darwin-x86_64) TARGET="macos-x86_64" ;;
    *) TARGET="macos-aarch64" ;;
  esac
fi

echo "--- macOS standalone eval smoke ---"
uname -a
"$EMACS" --version | head -1

export NELISP_STANDALONE_TARGET="$TARGET"
export NELISP_FORM_OP="$OP"
export NELISP_FORM_A="$A"
export NELISP_FORM_B="$B"

"$EMACS" --batch -Q -L lisp -L src -L scripts \
  --eval '(setq load-prefer-newer t)' \
  -l nelisp-standalone-build \
  -f nelisp-standalone-build

case "$TARGET" in
  macos-aarch64) EXE="$REPO_ROOT/target/nelisp-standalone-eval-macos-aarch64" ;;
  macos-x86_64) EXE="$REPO_ROOT/target/nelisp-standalone-eval-macos-x86_64" ;;
  *) echo "[macos-standalone-eval] FAIL: unsupported target $TARGET" >&2; exit 2 ;;
esac
if [ ! -f "$EXE" ]; then
  echo "[macos-standalone-eval] FAIL: missing $EXE"
  exit 1
fi

HOST_TARGET=""
case "$(uname -s 2>/dev/null || echo)-$(uname -m 2>/dev/null || echo)" in
  Darwin-arm64) HOST_TARGET="macos-aarch64" ;;
  Darwin-x86_64) HOST_TARGET="macos-x86_64" ;;
esac
if [ "$HOST_TARGET" != "$TARGET" ] || [ "$BUILD_ONLY" -eq 1 ]; then
  file "$EXE"
  echo "[macos-standalone-eval] build-only PASS: $EXE"
  exit 0
fi

if command -v codesign >/dev/null 2>&1; then
  codesign -f -s - "$EXE" >/dev/null
fi
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
