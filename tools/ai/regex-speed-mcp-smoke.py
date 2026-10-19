#!/usr/bin/env python3
"""Run the Anvil MCP initialize/tools-list smoke with isolated databases.

Usage: regex-speed-mcp-smoke.py BINARY OUTPUTDIR
"""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fail(message):
    print(f"REGEX-SPEED-MCP FAIL: {message}", file=sys.stderr)
    return 1


def main():
    if len(sys.argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    binary = Path(sys.argv[1]).resolve()
    output = Path(sys.argv[2]).resolve()
    if not binary.is_file() or not os.access(binary, os.X_OK):
        return fail(f"binary is not executable: {binary}")
    binary_hash = sha256(binary)
    output.mkdir(parents=True, exist_ok=True)
    if any(output.iterdir()):
        return fail(f"output directory must be empty: {output}")

    tool_root = Path(__file__).resolve().parents[2]
    common_text = subprocess.check_output(
        ["git", "-C", str(tool_root), "rev-parse", "--git-common-dir"],
        text=True).strip()
    common = Path(common_text)
    if not common.is_absolute():
        common = (tool_root / common).resolve()
    repository = common.parent
    notes = repository.parent.parent
    anvil = repository.parent / "anvil.el"
    launcher = anvil / "tools/anvil-standalone-mcp.sh"
    if not launcher.is_file():
        return fail(f"MCP launcher not found: {launcher}")

    tmp = tool_root / "target/tmp"
    tmp.mkdir(parents=True, exist_ok=True)
    environment = dict(
        os.environ,
        ANVIL_NOTES_DIR=str(notes),
        ANVIL_EL_DIR=str(anvil),
        NELISP_BIN=str(binary),
        NELISP_EMACS_LIB_DIR=str(repository.parent / "nelisp-emacs-lib/src"),
        ANVIL_MEMORY_DB=str(output / "memory.db"),
        ANVIL_WORKLOG_DB=str(output / "worklog.db"),
        TMPDIR=str(tmp),
    )
    requests = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
            "protocolVersion": "2024-11-05", "capabilities": {},
            "clientInfo": {"name": "regex-speed", "version": "1"}}},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}},
    ]
    try:
        with (output / "stderr").open("w") as error_stream:
            process = subprocess.run(
                [str(launcher)],
                input="".join(json.dumps(row) + "\n" for row in requests),
                text=True,
                stdout=subprocess.PIPE,
                stderr=error_stream,
                env=environment,
                cwd=tool_root,
                timeout=180,
            )
    except subprocess.TimeoutExpired:
        return fail("MCP process timed out after 180 seconds")
    (output / "stdout").write_text(process.stdout)

    rows = []
    for line in process.stdout.splitlines():
        try:
            row = json.loads(line)
        except ValueError:
            continue
        if isinstance(row, dict):
            rows.append(row)
    initialize_rows = [row for row in rows if row.get("id") == 1]
    listing_rows = [row for row in rows if row.get("id") == 2]
    initialize = initialize_rows[0] if len(initialize_rows) == 1 else {}
    listing = listing_rows[0] if len(listing_rows) == 1 else {}
    initialized = (len(initialize_rows) == 1
                   and isinstance(initialize.get("result"), dict)
                   and "error" not in initialize)
    tools = listing.get("result", {}).get("tools", [])
    names = ([row.get("name") for row in tools]
             if isinstance(tools, list) and all(isinstance(row, dict) for row in tools)
             else [])
    listed = (len(listing_rows) == 1 and "error" not in listing
              and isinstance(listing.get("result"), dict)
              and isinstance(tools, list))
    try:
        unchanged = binary.is_file() and sha256(binary) == binary_hash
    except OSError:
        unchanged = False
    passed = (process.returncode == 0 and initialized and listed
              and len(names) == 40 and unchanged
              and all(isinstance(name, str) and name for name in names)
              and len(set(names)) == 40)
    record = {
        "returncode": process.returncode,
        "initialized": initialized,
        "tools_listed": listed,
        "count": len(names),
        "names": names,
        "binary_sha256": binary_hash,
        "binary_unchanged": unchanged,
        "smoke_passed": passed,
    }
    (output / "result.json").write_text(json.dumps(record, indent=2) + "\n")
    print(f"REGEX-SPEED-MCP tools={len(names)} initialized={int(initialized)} "
          f"exit={process.returncode} unchanged={int(unchanged)} "
          f"{'PASS' if passed else 'FAIL'}")
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())
