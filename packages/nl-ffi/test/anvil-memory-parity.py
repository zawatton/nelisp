#!/usr/bin/env python3
"""Run real Anvil MCP searches against private SQLite snapshots.

Requires ANVIL_NOTES_DIR (or --notes). Live databases are opened read-only
solely for SQLite's backup API; the child runtime receives only private copies.
Use --db-dir from a prior run to compare platforms on the exact same snapshot.
"""

import argparse
from contextlib import closing
import hashlib
import json
import os
from pathlib import Path
import queue
import shutil
import sqlite3
import subprocess
import threading
import tempfile
import time


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", required=True, type=Path)
    parser.add_argument("--wine", action="store_true")
    parser.add_argument("--notes", default=os.environ.get("ANVIL_NOTES_DIR"), type=Path)
    parser.add_argument("--anvil", default=os.environ.get("ANVIL_EL_DIR"), type=Path)
    parser.add_argument("--db-dir", type=Path)
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--reference", type=Path)
    args = parser.parse_args()
    if args.notes is None:
        parser.error("set ANVIL_NOTES_DIR or --notes")
    notes = args.notes.resolve()
    anvil = (args.anvil or notes / "dev/anvil.el").resolve()
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)
    snapshots = args.db_dir or out / "snapshot"
    if args.db_dir is None:
        snapshots.mkdir(exist_ok=True)
        for kind in ("memory", "worklog"):
            source = notes / f".anvil-{kind}/anvil-{kind}-index.db"
            with closing(sqlite3.connect(source.as_uri() + "?mode=ro", uri=True)) as src:
                with closing(sqlite3.connect(snapshots / f"{kind}.db")) as dst:
                    src.backup(dst)
    private = Path(tempfile.mkdtemp(prefix="db-", dir=out))
    for kind in ("memory", "worklog"):
        shutil.copy2(snapshots / f"{kind}.db", private / f"{kind}.db")

    env = os.environ.copy()

    def target_path(path):
        path = str(path.resolve())
        if args.wine:
            return subprocess.check_output(["winepath", "-w", path], env=env,
                                           text=True, stderr=subprocess.DEVNULL).strip().replace("\\", "/")
        return path.replace("\\", "/") if os.name == "nt" else path

    binary = target_path(args.binary)
    runtime_dir = Path(tempfile.mkdtemp(prefix="runtime-", dir=out))
    temporary = target_path(runtime_dir)
    env.update(ANVIL_NOTES_DIR=target_path(notes), ANVIL_EL_DIR=target_path(anvil),
               ANVIL_MEMORY_DB=target_path(private / "memory.db"),
               ANVIL_WORKLOG_DB=target_path(private / "worklog.db"),
               NELISP_EMACS_LIB_DIR=target_path(notes / "dev/nelisp-emacs-lib/src"),
               NELISP_BIN=binary, TMPDIR=temporary, TEMP=temporary, TMP=temporary)
    # The POSIX launcher is the normal Linux entry point. Wine and native
    # Windows use its identical --load entry with Windows-form environment.
    if args.wine:
        command = ["wine", binary, "--load", target_path(anvil / "tools/anvil-standalone-mcp.el")]
    elif os.name == "nt":
        command = [binary, "--load", target_path(anvil / "tools/anvil-standalone-mcp.el")]
    else:
        command = [str(anvil / "tools/anvil-standalone-mcp.sh")]
    invocation = dict(binary=str(args.binary.resolve()),
                      binary_sha256=hashlib.sha256(args.binary.read_bytes()).hexdigest(),
                      command=command,
                      temporary_directory=temporary,
                      working_directory=str(runtime_dir),
                      memory_db=env["ANVIL_MEMORY_DB"], worklog_db=env["ANVIL_WORKLOG_DB"],
                      snapshot_sha256={kind: hashlib.sha256(
                          (snapshots / f"{kind}.db").read_bytes()).hexdigest()
                          for kind in ("memory", "worklog")})
    (out / "invocation.json").write_text(json.dumps(invocation, indent=2) + "\n", encoding="utf-8")
    responses = queue.Queue()
    with (out / "stderr.log").open("wb") as errors, (out / "stdout.log").open("wb") as output:
        process = subprocess.Popen(command, env=env, cwd=runtime_dir, stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE, stderr=errors)

        def reader():
            for line in process.stdout:
                output.write(line)
                output.flush()
                try:
                    value = json.loads(line)
                except (ValueError, UnicodeDecodeError):
                    continue
                responses.put(value)

        thread = threading.Thread(target=reader, daemon=True)
        thread.start()

        def send(value):
            process.stdin.write((json.dumps(value, ensure_ascii=False) + "\n").encode("utf-8"))
            process.stdin.flush()

        def receive(identifier):
            deadline = time.monotonic() + 180
            while time.monotonic() < deadline:
                try:
                    value = responses.get(timeout=1)
                except queue.Empty:
                    if process.poll() is not None:
                        raise RuntimeError(f"Anvil exited {process.returncode}; see {out}")
                    continue
                if value.get("id") == identifier:
                    if "error" in value or value.get("result", {}).get("isError"):
                        raise RuntimeError(f"MCP request {identifier} failed: {value}")
                    return value["result"]
            raise TimeoutError(f"MCP request {identifier}; see {out}")

        result = {}
        try:
            send(dict(jsonrpc="2.0", id=1, method="initialize",
                      params=dict(protocolVersion="2024-11-05", capabilities={},
                                  clientInfo=dict(name="ffi-memory-parity", version="1"))))
            result["initialize"] = receive(1)
            send(dict(jsonrpc="2.0", method="notifications/initialized"))
            time.sleep(25)  # Keep stdin open through the normal profile startup.
            send(dict(jsonrpc="2.0", id=2, method="tools/list", params={}))
            tools = receive(2)["tools"]
            assert len(tools) == 40, len(tools)
            result["tools"] = sorted(tool["name"] for tool in tools)
            identifier = 3
            for tool in ("memory-search", "worklog-search"):
                for query in ("nelisp", "日本語"):
                    send(dict(jsonrpc="2.0", id=identifier, method="tools/call",
                              params=dict(name=tool, arguments=dict(query=query, limit="3"))))
                    reply = receive(identifier)
                    text = "\n".join(item.get("text", "") for item in reply.get("content", []))
                    payload = json.loads(text)
                    assert isinstance(payload.get("rows"), list) and payload["rows"], (
                        f"missing or empty rows: {tool} {query}")
                    result[f"{tool}:{query}"] = payload
                    identifier += 1
        finally:
            process.stdin.close()
            try:
                process.wait(timeout=15)
            except subprocess.TimeoutExpired:
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
            thread.join(timeout=5)
        assert process.returncode == 0, process.returncode
    (out / "result.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    if args.reference:
        reference = json.loads(args.reference.read_text(encoding="utf-8"))
        # Server build/version metadata may differ; compare the actual tool
        # inventory and all four complete search results without row filtering.
        assert {k: v for k, v in result.items() if k != "initialize"} == {
            k: v for k, v in reference.items() if k != "initialize"}, "platform results differ"
    print("ANVIL-MEMORY-PARITY: PASS 40 tools, 4 searches")


if __name__ == "__main__":
    main()
