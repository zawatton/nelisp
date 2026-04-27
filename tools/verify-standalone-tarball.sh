#!/usr/bin/env bash
# tools/verify-standalone-tarball.sh — stage-d-v3.0 e2e smoke

set -euo pipefail

VERSION="${1:-stage-d-v3.0}"
PLATFORM="${2:-linux-x86_64}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

log() { printf "  \033[1;34m==>\033[0m %s\n" "$*"; }
err() { printf "  \033[1;31merror:\033[0m %s\n" "$*" >&2; }
ok()  { printf "  \033[1;32m✓\033[0m %s\n" "$*"; }

ARTIFACT_NAME="anvil-${VERSION}-${PLATFORM}"
TAR_FILE="dist/${ARTIFACT_NAME}.tar.gz"
SHA_FILE="dist/${ARTIFACT_NAME}.tar.gz.sha256"

[[ -f "$TAR_FILE" ]] || { err "tarball missing — run 'make stage-d-v3-tarball' first."; exit 1; }
[[ -f "$SHA_FILE" ]] || { err "checksum missing: $SHA_FILE"; exit 1; }

log "verifying SHA-256"
RECORDED=$(awk '{print $1}' "$SHA_FILE")
if command -v sha256sum >/dev/null 2>&1; then
  RECOMPUTED=$(sha256sum "$TAR_FILE" | awk '{print $1}')
elif command -v shasum >/dev/null 2>&1; then
  RECOMPUTED=$(shasum -a 256 "$TAR_FILE" | awk '{print $1}')
else
  err "neither sha256sum nor shasum found"
  exit 1
fi
[[ "$RECORDED" == "$RECOMPUTED" ]] || { err "SHA mismatch"; exit 2; }
ok "SHA-256 matches ($RECORDED)"

TEST_ROOT="$(mktemp -d -t anvil-v3-verify-XXXXXX)"
trap 'rm -rf "$TEST_ROOT"' EXIT
tar -xzf "$TAR_FILE" -C "$TEST_ROOT"
INSTALL_DIR="$TEST_ROOT/$ARTIFACT_NAME"

[[ -x "$INSTALL_DIR/bin/anvil" ]] || { err "bin/anvil missing"; exit 2; }
[[ -x "$INSTALL_DIR/bin/anvil-runtime" ]] || { err "bin/anvil-runtime missing"; exit 2; }
find "$INSTALL_DIR/lib" -maxdepth 1 -type f | grep -q . || { err "runtime cdylib missing"; exit 2; }
ok "tarball layout OK"

log "running bin/anvil version"
VERSION_OUT=$(env -u EMACS ANVIL_HOME="$INSTALL_DIR" "$INSTALL_DIR/bin/anvil" version 2>&1)
echo "$VERSION_OUT" | sed 's/^/      /'
echo "$VERSION_OUT" | grep -q "v3.0 standalone (= no emacs)" || { err "version output missing standalone marker"; exit 2; }
ok "version reports standalone mode"

ERR=$(mktemp)
IN_FIFO="$TEST_ROOT/in.fifo"
OUT_FIFO="$TEST_ROOT/out.fifo"
mkfifo "$IN_FIFO" "$OUT_FIFO"
env -u EMACS ANVIL_HOME="$INSTALL_DIR" "$INSTALL_DIR/bin/anvil" mcp serve \
  <"$IN_FIFO" >"$OUT_FIFO" 2>"$ERR" &
SERVER_PID=$!
cleanup_server() {
  if kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
}
trap 'cleanup_server; rm -rf "$TEST_ROOT"; rm -f "$ERR"' EXIT
exec {SERVER_IN}>"$IN_FIFO"
exec {SERVER_OUT}<"$OUT_FIFO"

printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' >&"$SERVER_IN"
IFS= read -r -t 5 INIT_LINE <&"$SERVER_OUT" || { err "initialize reply not observed"; cat "$ERR" >&2 || true; exit 2; }
echo "$INIT_LINE" | grep -q '"protocolVersion":"2024-11-05"' || { err "initialize protocol mismatch"; exit 2; }
ok "initialize OK"

SERVER_CMD=$(ps -p "$SERVER_PID" -o args= 2>/dev/null || true)
echo "$SERVER_CMD" | grep -q "anvil-runtime" || {
  err "server process is not anvil-runtime: $SERVER_CMD"
  exit 2
}
ok "server process = anvil-runtime (no emacs)"

printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' >&"$SERVER_IN"
IFS= read -r -t 5 TOOLS_LINE <&"$SERVER_OUT" || { err "tools/list reply not observed"; cat "$ERR" >&2 || true; exit 2; }
echo "$TOOLS_LINE" | grep -q 'anvil-host-info' || { err "tools/list missing anvil-host-info"; exit 2; }
echo "$TOOLS_LINE" | grep -q 'anvil-host-helpers-list' || { err "tools/list missing anvil-host-helpers-list"; exit 2; }
ok "tools/list exposes anvil-host tools"

printf '%s\n' '{"jsonrpc":"2.0","id":3,"method":"shutdown"}' >&"$SERVER_IN"
IFS= read -r -t 5 SHUTDOWN_LINE <&"$SERVER_OUT" || { err "shutdown reply not observed"; exit 2; }
echo "$SHUTDOWN_LINE" | grep -q '"id":3,"result":null' || { err "shutdown reply malformed"; exit 2; }
exec {SERVER_IN}>&-
wait "$SERVER_PID"
ok "shutdown clean"

echo ""
ok "stage-d-v3.0 standalone tarball smoke PASS"
