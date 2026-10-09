#!/bin/sh
# Doc 213 S5: two MCP sessions served by ONE anvil daemon through
# `anvil-runtime mcp shared'.  Usage: tools/anvil-shared-daemon-smoke.sh
# with ANVIL_EL_DIR set (and NELISP_BIN if the default reader is not the
# one to test).  Uses the real anvil state directory, so a running shared
# daemon is reused rather than replaced.  Exit 0 = SHARED-PASS.
[ -n "$ANVIL_EL_DIR" ] || { echo "ANVIL_EL_DIR is not set" >&2; exit 2; }
export ANVIL_TOOL_MODULES="${ANVIL_TOOL_MODULES:-anvil-discovery,anvil-sqlite,anvil-bench,anvil-state,anvil-memory,anvil-worklog}"
tmp=$(mktemp -d)
cat > "$tmp/in.txt" <<'JSON'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"smoke","version":"1"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"worklog-list","arguments":{"limit":"1"}}}
JSON
for i in 1 2; do
    timeout 900 "$ANVIL_EL_DIR/bin/anvil-runtime" mcp shared < "$tmp/in.txt" > "$tmp/out$i.txt" 2>"$tmp/err$i.txt" &
done
wait
state="$HOME/.anvil-runtime/service/anvil.state"
fail=0
for i in 1 2; do
    if grep -q '"id":3,"result"' "$tmp/out$i.txt"; then echo "session $i ok"; else
        echo "session $i FAIL"; tail -3 "$tmp/err$i.txt"; fail=1; fi
done
[ -f "$state" ] && echo "daemon $(cat "$state" | cut -c1-60)" || { echo "no daemon state"; fail=1; }
rm -rf "$tmp"
[ $fail -eq 0 ] && echo SHARED-PASS || echo SHARED-FAIL
exit $fail
