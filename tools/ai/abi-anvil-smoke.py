#!/usr/bin/env python3
"""Read-only Anvil MCP smoke, with all artifacts inside the selected directory."""
import json
import os
from pathlib import Path
import subprocess
import sys

binary, directory = map(Path, sys.argv[1:])
directory.mkdir(parents=True, exist_ok=True)
launcher = Path(os.environ.get("ANVIL_EL_DIR", Path.home() / ".emacs.d/external-packages/anvil.el")) / "tools/anvil-standalone-mcp.sh"
messages = [
    {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
        "protocolVersion": "2025-06-18", "capabilities": {},
        "clientInfo": {"name": "abi-smoke", "version": "1"}}},
    {"jsonrpc": "2.0", "method": "notifications/initialized"},
    {"jsonrpc": "2.0", "id": 2, "method": "tools/list"},
    {"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {
        "name": "memory-search", "arguments": {"query": "shallow binding", "limit": "2"}}},
    {"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {
        "name": "worklog-search", "arguments": {"query": "cl-threads", "limit": "2"}}},
]
with (directory / "stdout").open("w") as out, (directory / "stderr").open("w") as err:
    result = subprocess.run(["bash", str(launcher)], env=dict(os.environ, NELISP_BIN=str(binary.resolve())),
                            input="\n".join(map(json.dumps, messages)) + "\n", text=True,
                            stdout=out, stderr=err, timeout=180)
responses = {}
for line in (directory / "stdout").read_text(errors="replace").splitlines():
    try:
        m = json.loads(line)
    except ValueError:
        continue
    if isinstance(m, dict) and m.get("id") in (1, 2, 3, 4):
        responses[m["id"]] = m
ok = result.returncode == 0
for i in (1, 2, 3, 4):
    m = responses.get(i, {})
    r = m.get("result", {})
    passed = bool(r) and "error" not in m
    if i == 2:
        passed &= len(r.get("tools", [])) == 40
        detail = "tools=" + str(len(r.get("tools", [])))
    else:
        detail = str(r.get("content", [{}])[0].get("text", ""))[:200]
        if r.get("isError"):
            # Keep the baseline's known-red worklog schema failure visible.
            known = i == 4 and "threads" in detail and "column" in detail
            passed &= known
            detail = ("KNOWN-RED " if known else "ERROR ") + detail
    ok &= passed
    print(i, "PASS" if passed else "FAIL", detail)
sys.exit(0 if ok else 1)
