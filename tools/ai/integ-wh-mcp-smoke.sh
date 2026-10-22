#!/bin/bash
# integ-wh-mcp-smoke.sh NELISP_BIN: start anvil-standalone on NELISP_BIN, send read-only MCP
# calls (NDJSON) and print a summary, one line per request.  Line 2 must read "2 tools=40".
# Adapted from batch F's target/integ/anvil-mcp-smoke.sh.  Raw output goes to
# target/integ/anvil-smoke.{out,err}.  ANVIL_MCP_LAUNCHER overrides the launcher path.
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "$1") || exit 2
S=$PWD/target/integ
mkdir -p "$S"
L=${ANVIL_MCP_LAUNCHER:-$HOME/.emacs.d/external-packages/anvil.el/tools/anvil-standalone-mcp.sh}
{
  echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"smoke","version":"0"}}}'
  sleep 2
  echo '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  echo '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'
  echo '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"memory-search","arguments":{"query":"shallow binding","limit":"2"}}}'
  echo '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"worklog-search","arguments":{"query":"cl-threads","limit":"2"}}}'
  sleep 60
} | (ulimit -v 8388608; NELISP_BIN=$BIN exec timeout 180 bash "$L") > "$S/anvil-smoke.out" 2> "$S/anvil-smoke.err"
python3 - "$S/anvil-smoke.out" <<'PY'
import json,sys
ok={}
for line in open(sys.argv[1],errors='replace'):
    line=line.strip()
    if not line.startswith('{'): continue
    try: m=json.loads(line)
    except Exception: continue
    i=m.get('id')
    if i is None: continue
    if 'error' in m: ok[i]='ERROR '+str(m['error'])[:120]
    else:
        r=m.get('result',{})
        if i==2: ok[i]='tools=%d'%len(r.get('tools',[]))
        elif i in (3,4): ok[i]=('isError ' if r.get('isError') else 'ok ')+str(r.get('content',[{}])[0].get('text',''))[:100]
        else: ok[i]='ok'
for i in (1,2,3,4): print(i, ok.get(i,'NO RESPONSE'))
PY
