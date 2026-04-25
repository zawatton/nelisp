#!/bin/sh
# nelisp-server-smoke.sh --- Phase 5-E §3.4 MCP smoke test
#
# Pipe a scripted MCP dialog into `emacs --batch' running
# `nelisp-server-run-stdio', capture stdout + verify JSON replies.
# Intended as both a CI-style integration check and the source of
# the reference dialog transcript saved under
# `docs/design/16-claude-code-dialog.org'.
#
# Usage:
#   ./test/nelisp-server-smoke.sh              # PASS/FAIL to stdout
#   ./test/nelisp-server-smoke.sh --raw        # print raw stdout
#   ./test/nelisp-server-smoke.sh --transcript # machine-readable
#                                                request/response log
#
# Exit codes:
#   0  all assertions pass
#   1  stdio handshake or JSON assertions failed
#   2  environment / invocation error

set -eu

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$SCRIPT_DIR/.." && pwd)
EMACS="${EMACS:-emacs}"
MODE="${1:-check}"

if ! command -v "$EMACS" >/dev/null 2>&1; then
  echo "smoke: $EMACS not on PATH" >&2
  exit 2
fi

# The dialog is a stable 5-request sequence exercising every
# built-in method and three representative tools:
#   1. initialize
#   2. notifications/initialized  (no reply expected)
#   3. tools/list
#   4. tools/call git-log limit 2
#   5. tools/call data-set-path foo=42
#   6. tools/call data-get-path foo
REQUESTS=$(cat <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize"}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"git-log","arguments":{"limit":2}}}
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"data-set-path","arguments":{"path":"foo","value":42}}}
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"data-get-path","arguments":{"path":"foo"}}}
EOF
)

OUT=$(mktemp)
ERR=$(mktemp)
trap 'rm -f "$OUT" "$ERR"' EXIT

# Run server, feeding REQUESTS as stdin.  `read-from-minibuffer'
# signals `end-of-file' when stdin closes; the runner catches
# this and cleanly exits.
printf '%s\n' "$REQUESTS" | \
  "$EMACS" --batch -Q -L "$REPO/src" \
    --eval "(setq load-prefer-newer t)" \
    -l nelisp-server -l nelisp-tools \
    -f nelisp-server-run-stdio \
    > "$OUT" 2> "$ERR" || {
      echo "smoke: emacs batch invocation failed (exit $?)" >&2
      cat "$ERR" >&2
      exit 2
    }

if [ "$MODE" = "--raw" ]; then
  cat "$OUT"
  exit 0
fi

if [ "$MODE" = "--transcript" ]; then
  awk -v reqs="$REQUESTS" '
    BEGIN {
      n = split(reqs, req_lines, "\n")
      for (i = 1; i <= n; i++) {
        if (length(req_lines[i]) > 0) print "> " req_lines[i]
      }
      print ""
    }
    /^{/ { print "< " $0 }
  ' "$OUT"
  exit 0
fi

FAIL=0
assert_contains() {
  if ! grep -q "$1" "$OUT"; then
    echo "FAIL: expected stdout to contain: $1" >&2
    FAIL=1
  fi
}
assert_line_count() {
  actual=$(grep -c '^{' "$OUT" || true)
  if [ "$actual" -ne "$1" ]; then
    echo "FAIL: expected $1 JSON reply lines, got $actual" >&2
    FAIL=1
  fi
}
assert_absent() {
  if grep -q "$1" "$OUT"; then
    echo "FAIL: expected stdout NOT to contain: $1" >&2
    FAIL=1
  fi
}

# notifications/initialized has no reply -> 5 JSON lines for 6 reqs
assert_line_count 5

# Response shape assertions
assert_contains '"protocolVersion":"2024-11-05"'
assert_contains '"serverInfo":{"name":"nelisp-anvil"'
assert_contains '"listChanged":false'
assert_absent   '"listChanged":"false"'
assert_contains '"name":"file-read"'
assert_contains '"name":"git-log"'
assert_contains '"name":"data-set-path"'
assert_contains '"name":"data-get-path"'
# Phase 5-F.1.2 state-* (Doc 17 §2.7 A)
assert_contains '"name":"state-set"'
assert_contains '"name":"state-get"'
assert_contains '"name":"state-delete"'
assert_contains '"name":"state-list-ns"'
assert_contains '"name":"state-list-keys"'
assert_contains '"name":"state-count"'
# Phase 5-F.2 file/git/data tool surface (Doc 19)
assert_contains '"name":"file-replace-string"'
assert_contains '"name":"file-insert-at-line"'
assert_contains '"name":"file-delete-lines"'
assert_contains '"name":"file-append"'
assert_contains '"name":"file-read-snippet"'
assert_contains '"name":"git-diff-names"'
assert_contains '"name":"git-head-sha"'
assert_contains '"name":"git-branch-current"'
assert_contains '"name":"git-repo-root"'
assert_contains '"name":"data-list-keys"'
assert_contains '"name":"data-delete-path"'
# Phase 5-F.3.1 org tool (Doc 20)
assert_contains '"name":"org-read-outline-tree"'
# Phase 6.2.3 anvil-http MVP MCP surface (Doc 24)
assert_contains '"name":"http-fetch"'
assert_contains '"name":"http-head"'
assert_contains '"name":"http-cache-get"'
assert_contains '"name":"http-cache-clear"'
assert_contains '"name":"http-cache-status"'
# Phase 6.2.8 anvil-http POST + auth port (Doc 24 §3 6.2.8 SHIPPED)
assert_contains '"name":"http-post"'
# Phase 6.2.5 + 6.2.6 robots.txt + batch fetch (Doc 24 完全クローズ)
assert_contains '"name":"http-robots-check"'
assert_contains '"name":"http-fetch-batch"'
# git-status has no params — inputSchema.properties must be `{}` not `null`
# (Claude Code zod schema rejects `null`; surfaced as "Failed to fetch tools"
# invalid_type at tools[*].inputSchema.properties.)
assert_contains '"name":"git-status"'
assert_contains '"inputSchema":{"type":"object","properties":{}}'
assert_absent   '"properties":null'
assert_contains '"id":3'
assert_contains '"id":4'
assert_contains '"id":5'

if [ "$FAIL" -eq 0 ]; then
  echo "PASS: nelisp-server MCP stdio smoke (6 requests, 5 replies)"
else
  echo "--- raw stdout ---" >&2
  cat "$OUT" >&2
  echo "--- raw stderr ---" >&2
  cat "$ERR" >&2
  exit 1
fi
