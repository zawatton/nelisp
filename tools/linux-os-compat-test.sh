#!/usr/bin/env bash
# Linux OS compatibility ERT smoke.
#
# Linux is the historical default path, so this smoke currently gates the
# explicit non-Windows stdlib selectors plus the full stdlib OS test file on
# native Linux hosts.  No Rust toolchain is used.
set -euo pipefail

EMACS="${EMACS:-emacs}"
SUITE="all"
SELECTOR=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --emacs) EMACS="$2"; shift 2 ;;
    --suite|-s) SUITE="$2"; shift 2 ;;
    --selector) SELECTOR="$2"; shift 2 ;;
    --list)
      printf '%s\n' all non-windows fds sockets linux-only full
      exit 0
      ;;
    -h|--help)
      echo "usage: $0 [--suite all|non-windows|fds|sockets|linux-only|full] [--selector REGEXP] [--emacs EMACS]"
      exit 0
      ;;
    *)
      echo "usage: $0 [--suite all|non-windows|fds|sockets|linux-only|full] [--selector REGEXP] [--emacs EMACS]" >&2
      exit 2
      ;;
  esac
done

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

case "$SUITE" in
  all)
    DEFAULT_SELECTOR="non-windows\\|open-linux\\|fcntl-linux\\|socket-linux\\|sockopts-linux\\|sockaddr-in6-linux\\|socketpair-linux\\|linux-only-apis-use-linux-syscalls\\|sendmsg-fds-linux\\|recvmsg-fds-linux"
    EXPECTED_COUNT=12
    ;;
  non-windows)
    DEFAULT_SELECTOR="non-windows"
    EXPECTED_COUNT=3
    ;;
  fds)
    DEFAULT_SELECTOR="open-linux\\|fcntl-linux"
    EXPECTED_COUNT=2
    ;;
  sockets)
    DEFAULT_SELECTOR="socket-linux\\|sockopts-linux\\|sockaddr-in6-linux\\|socketpair-linux\\|sendmsg-fds-linux\\|recvmsg-fds-linux"
    EXPECTED_COUNT=6
    ;;
  linux-only)
    DEFAULT_SELECTOR="linux-only-apis-use-linux-syscalls"
    EXPECTED_COUNT=1
    ;;
  full)
    DEFAULT_SELECTOR="t"
    EXPECTED_COUNT=318
    ;;
  *) echo "[linux-os] FAIL: unknown suite $SUITE" >&2; exit 2 ;;
esac

if [ -z "$SELECTOR" ]; then
  SELECTOR="$DEFAULT_SELECTOR"
  EXPECTED="$EXPECTED_COUNT"
else
  EXPECTED=0
fi

LOG_DIR="$REPO_ROOT/target/linux-os-compat"
mkdir -p "$LOG_DIR"
LOG="$LOG_DIR/$SUITE.log"

echo "--- Linux OS compatibility ERT smoke ---"
uname -a
"$EMACS" --version | head -1
echo "logs: $LOG_DIR"
echo "suite: $SUITE"

NELISP_LINUX_OS_SELECTOR="$SELECTOR" "$EMACS" --batch -Q -L lisp -L src -L test \
  --eval '(setq load-prefer-newer t)' \
  -l ert -l test/nelisp-stdlib-os-test.el \
  --eval '(ert-run-tests-batch-and-exit
           (let ((selector (getenv "NELISP_LINUX_OS_SELECTOR")))
             (if (string= selector "t") t selector)))' >"$LOG" 2>&1

if [ "$EXPECTED" -gt 0 ]; then
  RAN="$(sed -n 's/.*Ran \([0-9][0-9]*\) tests.*/\1/p' "$LOG" | tail -1)"
  if [ -z "$RAN" ]; then
    echo "[linux-os] FAIL: $SUITE missing ERT test count"
    tail -80 "$LOG"
    exit 1
  fi
  if [ "$RAN" -lt "$EXPECTED" ]; then
    echo "[linux-os] FAIL: $SUITE ran $RAN tests, expected at least $EXPECTED"
    tail -80 "$LOG"
    exit 1
  fi
  if grep -q "unexpected results:" "$LOG"; then
    echo "[linux-os] FAIL: $SUITE had unexpected results"
    tail -80 "$LOG"
    exit 1
  fi
fi

echo "[linux-os] PASS: $SUITE selector '$SELECTOR'"
tail -12 "$LOG"
echo "[linux-os] $SUITE PASS - Linux OS compatibility ERT smoke OK"
