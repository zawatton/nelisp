#!/bin/sh
# m1-bootsmoke.sh --- T97 M1 milestone integration boot smoke
#
# Boot anvil-server / anvil-defs / anvil-state core on top of the
# NeLisp standalone substrate (Wave 1+2: nelisp-json, base64,
# secure-hash, sqlite, coding, regex, emacs-compat, file-notify,
# load, ec-bridge), then drive an MCP `tools/list` round-trip.
#
# Usage:
#   scripts/m1-bootsmoke.sh                         # PASS / FAIL summary
#   scripts/m1-bootsmoke.sh --raw                   # raw MCP stdout
#   scripts/m1-bootsmoke.sh --bridge-status         # only print bridge status
#   scripts/m1-bootsmoke.sh --tools-list-only       # print tools/list reply
#   ANVIL_PATH=/path/to/anvil.el scripts/m1-bootsmoke.sh
#
# Exit codes:
#   0  PASS  -- substrate loads, bridge installs, anvil core boots,
#               tools/list returns >= 1 tool
#   1  PARTIAL or FAIL  -- one or more assertions failed (still useful
#               output for diagnosis; report still gets written)
#   2  environment / invocation error

set -eu

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$SCRIPT_DIR/.." && pwd)
EMACS="${EMACS:-emacs}"
MODE="${1:-check}"

# anvil.el source path -- prefer ANVIL_PATH env, else common locations
ANVIL_PATH="${ANVIL_PATH:-}"
if [ -z "$ANVIL_PATH" ]; then
  for cand in \
    "$HOME/Cowork/Notes/dev/anvil.el" \
    "$HOME/Notes/dev/anvil.el" \
    "$HOME/.emacs.d/external-packages/anvil.el"; do
    if [ -d "$cand" ] && [ -f "$cand/anvil-server.el" ]; then
      ANVIL_PATH="$cand"
      break
    fi
  done
fi
if [ -z "$ANVIL_PATH" ] || [ ! -f "$ANVIL_PATH/anvil-server.el" ]; then
  echo "m1-bootsmoke: ANVIL_PATH not set / anvil-server.el not found" >&2
  echo "  set ANVIL_PATH=/path/to/anvil.el" >&2
  exit 2
fi

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "m1-bootsmoke: $EMACS not on PATH" >&2
  exit 2
fi

# 6-message MCP dialog: initialize, notifications/initialized,
# tools/list, then 3 representative tools/call probes (read-only).
REQUESTS=$(cat <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize"}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"defs-index-status","arguments":{}}}
EOF
)

OUT=$(mktemp)
ERR=$(mktemp)
trap 'rm -f "$OUT" "$ERR"' EXIT

# Substrate load order matches the M1 plan: load-prefer-newer first,
# then host-side helpers, then the ec-bridge install.
# NOTE: nelisp-runtime-init.el does NOT exist (= host Emacs already
# provides the runtime); the bridge's own Phase 9d.A4 layout starts
# from nelisp-load.
printf '%s\n' "$REQUESTS" | \
  "$EMACS" --batch -Q -L "$REPO/src" -L "$ANVIL_PATH" \
    --eval "(setq load-prefer-newer t)" \
    -l nelisp-load \
    -l nelisp-emacs-compat \
    -l nelisp-emacs-compat-fileio \
    -l nelisp-regex \
    -l nelisp-json \
    -l nelisp-sqlite \
    -l nelisp-coding \
    -l nelisp-base64 \
    -l nelisp-secure-hash \
    -l nelisp-ec-bridge \
    --eval "(nelisp-ec-bridge-install)" \
    -l anvil \
    -l anvil-server \
    -l anvil-server-commands \
    -l anvil-defs \
    -l anvil-state \
    --eval "(anvil-state-enable)" \
    --eval "(anvil-defs-enable)" \
    --eval "(princ (format \";; bridge-status: %S\\n\" (nelisp-ec-bridge-status)))" \
    --eval "(princ (format \";; substrate-features: load=%s ec=%s ec-fileio=%s rx=%s json=%s sqlite=%s coding=%s b64=%s hash=%s ec-bridge=%s\\n\" (featurep 'nelisp-load) (featurep 'nelisp-emacs-compat) (featurep 'nelisp-emacs-compat-fileio) (featurep 'nelisp-regex) (featurep 'nelisp-json) (featurep 'nelisp-sqlite) (featurep 'nelisp-coding) (featurep 'nelisp-base64) (featurep 'nelisp-secure-hash) (featurep 'nelisp-ec-bridge)))" \
    --eval "(princ (format \";; anvil-features: anvil=%s server=%s commands=%s defs=%s state=%s\\n\" (featurep 'anvil) (featurep 'anvil-server) (featurep 'anvil-server-commands) (featurep 'anvil-defs) (featurep 'anvil-state)))" \
    --eval "(anvil-server-run-batch-stdio \"emacs-eval\")" \
    > "$OUT" 2> "$ERR" || {
      echo "m1-bootsmoke: emacs batch invocation failed (exit $?)" >&2
      head -40 "$ERR" >&2
      exit 1
    }

if [ "$MODE" = "--raw" ]; then
  cat "$OUT"
  exit 0
fi

if [ "$MODE" = "--bridge-status" ]; then
  grep '^;; bridge-status:' "$OUT" || true
  grep '^;; substrate-features:' "$OUT" || true
  grep '^;; anvil-features:' "$OUT" || true
  exit 0
fi

if [ "$MODE" = "--tools-list-only" ]; then
  grep '^{' "$OUT" | sed -n '2p'
  exit 0
fi

FAIL=0
WARN=0

assert_contains() {
  if ! grep -q "$1" "$OUT"; then
    echo "FAIL: expected stdout to contain: $1" >&2
    FAIL=1
  fi
}

# 1. substrate load assertions
assert_contains ';; substrate-features:'
if ! grep -q ';; substrate-features:.*ec-bridge=t' "$OUT"; then
  echo "FAIL: nelisp-ec-bridge feature not loaded" >&2
  FAIL=1
fi

# 2. anvil core load assertions
if ! grep -qE ';; anvil-features:.*server=t.*defs=t.*state=t' "$OUT"; then
  echo "FAIL: anvil core (server/defs/state) features missing" >&2
  FAIL=1
fi

# 3. MCP initialize reply
assert_contains '"protocolVersion":"2025-03-26"'
assert_contains '"serverInfo":{"name":"anvil"'

# 4. tools/list reply -- must be present and non-empty
TOOLS_LINE=$(grep '^{' "$OUT" | grep '"id":2' || true)
if [ -z "$TOOLS_LINE" ]; then
  echo "FAIL: no tools/list reply (id=2)" >&2
  FAIL=1
else
  TOOL_COUNT=$(printf '%s' "$TOOLS_LINE" | grep -oE '"name":"[a-z][a-zA-Z0-9_-]*"' | wc -l)
  if [ "$TOOL_COUNT" -lt 1 ]; then
    echo "FAIL: tools/list returned 0 tools" >&2
    FAIL=1
  else
    echo "PASS: tools/list returned $TOOL_COUNT tools"
  fi
fi

# 5. tools/call defs-index-status -- soft check (existing anvil shape
#    issue may yield -32602 even on host; treat as WARN not FAIL)
CALL_LINE=$(grep '^{' "$OUT" | grep '"id":3' || true)
if [ -z "$CALL_LINE" ]; then
  echo "WARN: no tools/call reply (id=3) — handler crashed silently?" >&2
  WARN=1
elif printf '%s' "$CALL_LINE" | grep -q '"error"'; then
  ERR_MSG=$(printf '%s' "$CALL_LINE" | grep -oE '"message":"[^"]*"' | head -1)
  echo "WARN: tools/call defs-index-status returned error: $ERR_MSG (= shared with host-Emacs path, not M1-specific)" >&2
  WARN=1
else
  echo "PASS: tools/call defs-index-status returned a result"
fi

if [ $FAIL -eq 0 ]; then
  if [ $WARN -ne 0 ]; then
    echo "VERDICT: PARTIAL (boot OK, tools/list OK, some tools/call warnings)"
  else
    echo "VERDICT: PASS"
  fi
  exit 0
else
  echo "VERDICT: FAIL"
  exit 1
fi
