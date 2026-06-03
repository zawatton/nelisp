#!/usr/bin/env bash
# macOS OS compatibility ERT smoke.
#
# This script mirrors the Windows OS compatibility runner at a smaller Darwin
# surface: it executes the Darwin/POSIX selector in test/nelisp-stdlib-os-test.el
# and verifies the selector did not silently skip tests.
set -euo pipefail

EMACS="${EMACS:-emacs}"
SUITE="all"
SELECTOR=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --suite) SUITE="$2"; shift 2 ;;
    --selector) SELECTOR="$2"; shift 2 ;;
    --list)
      printf '%s\n' all fds vm process sockets
      exit 0
      ;;
    -h|--help)
      echo "usage: $0 [--suite all|fds|vm|process|sockets] [--selector REGEXP] [--emacs EMACS]"
      exit 0
      ;;
    *) echo "usage: $0 [--suite all|fds|vm|process|sockets] [--selector REGEXP] [--emacs EMACS]" >&2; exit 2 ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

case "$SUITE" in
  all) DEFAULT_SELECTOR="darwin"; EXPECTED_COUNT=30 ;;
  fds) DEFAULT_SELECTOR="open-darwin\\|close-darwin\\|read-darwin\\|write-darwin\\|pipe-darwin\\|poll-darwin\\|dup2-darwin\\|fcntl-darwin\\|fstat-darwin"; EXPECTED_COUNT=9 ;;
  vm) DEFAULT_SELECTOR="vm-darwin"; EXPECTED_COUNT=1 ;;
  process) DEFAULT_SELECTOR="exit-darwin\\|execve-darwin\\|wait-darwin\\|kill-darwin\\|getppid-darwin"; EXPECTED_COUNT=5 ;;
  sockets) DEFAULT_SELECTOR="network-byte-order-darwin\\|sockopts-darwin\\|shutdown-darwin\\|basic-syscalls-darwin\\|socketpair-darwin\\|sendto-inet-darwin\\|recvfrom-inet-darwin\\|inet-darwin\\|unix-darwin\\|inet6-darwin\\|getname-darwin\\|sendmsg-fds-darwin\\|recvmsg-fds-darwin\\|getsockopt-peercred-darwin"; EXPECTED_COUNT=14 ;;
  *) echo "[macos-os] FAIL: unknown suite $SUITE" >&2; exit 2 ;;
esac

if [ -z "$SELECTOR" ]; then
  SELECTOR="$DEFAULT_SELECTOR"
  EXPECTED="$EXPECTED_COUNT"
else
  EXPECTED=0
fi

OUT_DIR="$REPO_ROOT/target/macos-os-compat"
mkdir -p "$OUT_DIR"
LOG="$OUT_DIR/nelisp-macos-os-$SUITE.log"

echo "--- macOS OS compatibility ERT smoke ---"
uname -a
"$EMACS" --version | head -1
echo "logs: $OUT_DIR"
echo "suite: $SUITE"

set +e
NELISP_MACOS_OS_SELECTOR="$SELECTOR" "$EMACS" --batch -Q -L lisp -L src -L test \
  --eval '(setq load-prefer-newer t)' \
  -l ert \
  -l test/nelisp-stdlib-os-test.el \
  --eval '(ert-run-tests-batch-and-exit (getenv "NELISP_MACOS_OS_SELECTOR"))' \
  >"$LOG" 2>&1
CODE=$?
set -e

if [ "$CODE" -ne 0 ]; then
  echo "[macos-os] FAIL: $SUITE selector '$SELECTOR'"
  tail -80 "$LOG"
  exit "$CODE"
fi

if [ "$EXPECTED" -gt 0 ]; then
  RAN="$(sed -n 's/.*Ran \([0-9][0-9]*\) tests.*/\1/p' "$LOG" | tail -1)"
  if [ -z "$RAN" ]; then
    echo "[macos-os] FAIL: $SUITE missing ERT test count"
    tail -80 "$LOG"
    exit 1
  fi
  if [ "$RAN" -lt "$EXPECTED" ]; then
    echo "[macos-os] FAIL: $SUITE ran $RAN tests, expected at least $EXPECTED"
    tail -80 "$LOG"
    exit 1
  fi
fi

echo "[macos-os] PASS: $SUITE selector '$SELECTOR'"
tail -8 "$LOG"
echo "[macos-os] all PASS - macOS OS compatibility ERT smoke OK"
