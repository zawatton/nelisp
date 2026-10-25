#!/bin/bash
# Run four read-only private MCP requests; summary line 2 is "2 tools=40".
set -euo pipefail
cd "$(dirname "$0")/../.."
BIN=$(realpath "${1:?usage: integ-wh-mcp-smoke.sh NELISP_BIN}")
S=${ANVIL_MCP_SMOKE_DIR:-$PWD/target/integ}
L=${ANVIL_MCP_LAUNCHER:-$HOME/.emacs.d/external-packages/anvil.el/tools/anvil-standalone-mcp.sh}
mkdir -p "$S"
ulimit -v 8388608
exec python3 - "$BIN" "$L" "$S" <<'PY'
import json
import os
from pathlib import Path
import selectors
import signal
import subprocess
import sys
import time

binary, launcher, directory = sys.argv[1:]
directory = Path(directory)
summary = {i: "NO RESPONSE" for i in range(1, 5)}
seen = set()
sent = set()
pending = bytearray()
failure = None
budget = float(os.environ.get("ANVIL_MCP_SMOKE_TIMEOUT", "180"))
cleanup_budget = float(os.environ.get("ANVIL_MCP_SMOKE_CLEANUP_TIMEOUT", "5"))
if not (0 < budget <= 180 and 0 < cleanup_budget <= 5):
    sys.exit("Smoke timeout overrides may only reduce the 180s/5s bounds")
deadline = time.monotonic() + budget

def require(condition, message):
    if not condition:
        raise ValueError(message)

def send(identifier, method, params=None):
    request = dict(jsonrpc="2.0", method=method)
    if identifier is not None:
        request["id"] = identifier
    if params is not None:
        request["params"] = params
    child.stdin.write((json.dumps(request) + "\n").encode())
    child.stdin.flush()
    if identifier is not None:
        sent.add(identifier)

def validate(message):
    require(isinstance(message, dict) and message.get("jsonrpc") == "2.0", "invalid JSON-RPC envelope")
    identifier = message.get("id")
    if identifier is None and isinstance(message.get("method"), str):
        require("id" not in message and "error" not in message and "result" not in message,
                "invalid notification envelope")
        return
    require(type(identifier) is int and identifier in summary, "unexpected response id")
    require(identifier in sent, f"unsolicited response {identifier}")
    require(identifier not in seen, f"duplicate response {identifier}")
    require("error" not in message and isinstance(message.get("result"), dict), f"response {identifier} has JSON-RPC error or invalid result")
    result = message["result"]
    if identifier == 1:
        require(isinstance(result.get("protocolVersion"), str) and bool(result["protocolVersion"]), "invalid initialize protocol")
        info = result.get("serverInfo", {})
        require(isinstance(result.get("capabilities"), dict) and isinstance(info, dict) and
                all(isinstance(info.get(k), str) and info[k] for k in ("name", "version")), "invalid initialize result")
        summary[1] = "ok initialize"
    elif identifier == 2:
        tools = result.get("tools")
        require(isinstance(tools, list) and len(tools) == 40, "expected exactly 40 tools")
        names = [tool.get("name") if isinstance(tool, dict) else None for tool in tools]
        require(all(isinstance(name, str) and name for name in names) and len(set(names)) == 40, "expected 40 distinct tool names")
        summary[2] = "tools=40"
    else:
        require("isError" not in result or result["isError"] is False, f"tool response {identifier} isError")
        content = result.get("content")
        require(isinstance(content, list) and len(content) == 1 and isinstance(content[0], dict) and
                content[0].get("type") == "text" and isinstance(content[0].get("text"), str), "invalid search content")
        payload = json.loads(content[0]["text"])
        require(isinstance(payload, dict) and isinstance(payload.get("rows"), list) and
                all(isinstance(row, dict) for row in payload["rows"]), "invalid search result rows")
        summary[identifier] = "ok rows=" + str(len(payload["rows"]))
    seen.add(identifier)

def receive_until(identifier):
    global pending
    while identifier not in seen:
        remaining = deadline - time.monotonic()
        require(remaining > 0, "response deadline exceeded")
        require(selector.select(remaining), "response deadline exceeded")
        chunk = os.read(child.stdout.fileno(), 65536)
        require(bool(chunk), f"missing response {identifier}")
        raw.write(chunk)
        raw.flush()
        pending.extend(chunk)
        require(len(pending) <= 1048576, "response line exceeds 1 MiB")
        while b"\n" in pending:
            line, _, rest = pending.partition(b"\n")
            pending = bytearray(rest)
            if line.strip():
                validate(json.loads(line))

with (directory / "anvil-smoke.out").open("wb") as raw, (directory / "anvil-smoke.err").open("wb") as errors:
    child = subprocess.Popen(["timeout", "--kill-after=5", "180", "bash", launcher],
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=errors,
                             env=dict(os.environ, NELISP_BIN=binary), start_new_session=True)
    selector = selectors.DefaultSelector()
    selector.register(child.stdout, selectors.EVENT_READ)
    try:
        send(1, "initialize", dict(protocolVersion="2025-06-18", capabilities={}, clientInfo=dict(name="smoke", version="1")))
        receive_until(1)
        send(None, "notifications/initialized")
        send(2, "tools/list")
        receive_until(2)
        for identifier, name, query in ((3, "memory-search", "shallow binding"), (4, "worklog-search", '"cl-threads"')):
            send(identifier, "tools/call", dict(name=name, arguments=dict(query=query, limit="2")))
            receive_until(identifier)
        child.stdin.close()
        # Drain after EOF as well: late duplicates and failures must not pass.
        while True:
            remaining = deadline - time.monotonic()
            require(remaining > 0, "response deadline exceeded during final drain")
            require(selector.select(min(cleanup_budget, remaining)), "child did not close after stdin EOF")
            chunk = os.read(child.stdout.fileno(), 65536)
            if not chunk:
                break
            raw.write(chunk)
            pending.extend(chunk)
            require(len(pending) <= 1048576, "response line exceeds 1 MiB during final drain")
            while b"\n" in pending:
                line, _, rest = pending.partition(b"\n")
                pending = bytearray(rest)
                if line.strip():
                    validate(json.loads(line))
        require(not pending.strip(), "unterminated response data")
        require(child.wait(timeout=cleanup_budget) == 0, "MCP child exited nonzero")
        require(seen == {1, 2, 3, 4}, "missing responses")
    except (ValueError, OSError, subprocess.TimeoutExpired) as error:
        failure = str(error)
    finally:
        if not child.stdin.closed:
            try:
                child.stdin.close()
            except OSError:
                pass
        # The timeout/launcher leader may have exited while descendants
        # still hold the pipe. The process group is private to this run.
        cleanup_deadline = time.monotonic() + cleanup_budget
        try:
            os.killpg(child.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        while time.monotonic() < cleanup_deadline:
            child.poll()
            try:
                os.killpg(child.pid, 0)
            except ProcessLookupError:
                break
            time.sleep(0.01)
        try:
            os.killpg(child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        child.wait(timeout=5)
        selector.close()
        child.stdout.close()
for identifier in range(1, 5):
    print(identifier, summary[identifier])
if failure:
    print("MCP smoke failed: " + failure, file=sys.stderr)
sys.exit(1 if failure else 0)
PY
