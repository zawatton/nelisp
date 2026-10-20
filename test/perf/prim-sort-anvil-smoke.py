#!/usr/bin/env python3
"""Exercise the public Anvil launcher with an open stdin and 40-tool list."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import select
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]


def response(process, captured):
    deadline = time.monotonic() + 90
    data = b""
    size = None
    while size is None or len(data) < size:
        if not select.select([process.stdout], [], [], max(0, deadline - time.monotonic()))[0]:
            raise RuntimeError("MCP response timed out")
        chunk = os.read(process.stdout.fileno(), 1)
        if not chunk:
            raise RuntimeError(f"MCP stdout closed early: {data[:120]!r}")
        captured.extend(chunk)
        data += chunk
        if data.startswith(b"{") and data.endswith(b"\n"):
            return json.loads(data)
        if size is None and b"\r\n\r\n" in data:
            header, data = data.split(b"\r\n\r\n", 1)
            size = int(header.removeprefix(b"Content-Length: "))
    return json.loads(data)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    parser.add_argument("--anvil-root", type=Path, required=True)
    args = parser.parse_args()
    out = ROOT / "target/prim-sort/anvil"
    out.mkdir(parents=True, exist_ok=True)
    binary = args.binary.resolve()
    anvil = args.anvil_root.resolve()
    env = dict(os.environ, NELISP_BIN=str(binary), ANVIL_EL_DIR=str(anvil),
               TMPDIR=str(ROOT / "target/tmp"),
               ANVIL_MEMORY_DB=str(out / "memory.db"),
               ANVIL_WORKLOG_DB=str(out / "worklog.db"))
    captured = bytearray()
    replies = {}
    requests = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
            "protocolVersion": "2024-11-05", "capabilities": {},
            "clientInfo": {"name": "prim-sort-smoke", "version": "1"}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
        {"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {
            "name": "code-context", "arguments": {
                "path": str(ROOT / "test/perf/prim-sort-bench.el"),
                "query": "ps-operation", "budget-chars": "1200"}}},
    ]
    with (out / "stderr").open("wb") as stderr:
        process = subprocess.Popen([str(anvil / "tools/anvil-standalone-mcp.sh")],
                                   cwd=ROOT, env=env, stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE, stderr=stderr)
        try:
            # The public server must see a live stream while booting.
            time.sleep(25)
            for request in requests:
                process.stdin.write(json.dumps(request).encode() + b"\n")
                process.stdin.flush()
                if "id" in request:
                    reply = response(process, captured)
                    replies[reply["id"]] = reply
            process.stdin.close()
            process.wait(timeout=30)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait()
            (out / "stdout").write_bytes(captured)
    if process.returncode or (out / "stderr").stat().st_size:
        raise RuntimeError(f"MCP exit={process.returncode}; see {out}")
    if any("error" in reply for reply in replies.values()):
        raise RuntimeError("MCP protocol error")
    count = len(replies[2]["result"]["tools"])
    call = replies[3]["result"]
    if count != 40 or call.get("isError"):
        raise RuntimeError(f"MCP tools={count}, context isError={call.get('isError')}")
    if '"status":"ok"' not in json.dumps(call).replace(" ", "").replace('\\"', '"'):
        raise RuntimeError("Context result did not report status ok")
    report = {"tools": count, "context_call": "PASS", "stdin_open_s": 25,
              "binary_sha256": hashlib.sha256(binary.read_bytes()).hexdigest()}
    (out / "result.json").write_text(json.dumps(report, indent=2) + "\n")
    print("ANVIL-SMOKE PASS: 40 tools, real context call, clean stderr")


if __name__ == "__main__":
    main()
