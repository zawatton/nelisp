#!/usr/bin/env bash
# test-anvil-no-emacs.sh --- Phase 8.0.5 Rust launcher smoke
#
# Proves `bin/anvil mcp serve --no-emacs` dispatches to the Rust binary
# directly, without needing a working `$EMACS` at runtime.  The script
# performs a minimal MCP stdio handshake:
#   1. initialize  -> assert protocolVersion response
#   2. shutdown    -> assert result:null
#   3. close stdin -> assert clean exit

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$SCRIPT_DIR/.." && pwd)
ANVIL="$REPO/bin/anvil"
RUNTIME_BIN="$REPO/nelisp-runtime/target/release/anvil-runtime"

if [[ ! -x "$RUNTIME_BIN" ]]; then
  echo "SKIP: rust runtime binary not built: $RUNTIME_BIN"
  exit 0
fi

ERR=$(mktemp)
IN_FIFO=$(mktemp -u)
OUT_FIFO=$(mktemp -u)
mkfifo "$IN_FIFO" "$OUT_FIFO"
cleanup() {
  local status=$?
  if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  rm -f "$ERR" "$IN_FIFO" "$OUT_FIFO"
  exit "$status"
}
trap cleanup EXIT

EMACS=/definitely-missing-emacs-path "$ANVIL" mcp serve --no-emacs \
  <"$IN_FIFO" >"$OUT_FIFO" 2>"$ERR" &
SERVER_PID=$!
exec {SERVER_IN}>"$IN_FIFO"
exec {SERVER_OUT}<"$OUT_FIFO"

printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' >&"$SERVER_IN"
if ! IFS= read -r -t 5 INIT_LINE <&"$SERVER_OUT"; then
  echo "FAIL: initialize reply not observed" >&2
  cat "$ERR" >&2 || true
  exit 1
fi
if [[ "$INIT_LINE" != *'"id":1'* ]] || [[ "$INIT_LINE" != *'"protocolVersion":"2024-11-05"'* ]]; then
  echo "FAIL: initialize reply missing protocolVersion" >&2
  printf '%s\n' "$INIT_LINE" >&2
  exit 1
fi

printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"shutdown"}' >&"$SERVER_IN"
if ! IFS= read -r -t 5 SHUTDOWN_LINE <&"$SERVER_OUT"; then
  echo "FAIL: shutdown reply not observed" >&2
  cat "$ERR" >&2 || true
  exit 1
fi
if [[ "$SHUTDOWN_LINE" != *'"id":2,"result":null'* ]]; then
  echo "FAIL: shutdown reply missing or malformed" >&2
  printf '%s\n' "$SHUTDOWN_LINE" >&2
  exit 1
fi
exec {SERVER_IN}>&-
if IFS= read -r -t 5 EXTRA_LINE <&"$SERVER_OUT"; then
  echo "FAIL: unexpected extra output after shutdown" >&2
  printf '%s\n' "$EXTRA_LINE" >&2
  exit 1
fi
for _ in $(seq 1 50); do
  state=$(ps -o stat= -p "$SERVER_PID" 2>/dev/null | tr -d '[:space:]' || true)
  if [[ -z "$state" ]] || [[ "$state" == Z* ]]; then
    break
  fi
  sleep 0.1
done
state=$(ps -o stat= -p "$SERVER_PID" 2>/dev/null | tr -d '[:space:]' || true)
if [[ -n "$state" ]] && [[ "$state" != Z* ]]; then
  echo "FAIL: server did not exit after shutdown + stdin close" >&2
  cat "$ERR" >&2 || true
  exit 1
fi
wait "$SERVER_PID"

echo "PASS: bin/anvil mcp serve --no-emacs dispatches to anvil-runtime"
