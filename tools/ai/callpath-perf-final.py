#!/usr/bin/env python3
"""COMMON.md final verification for S3.1, once, on fresh dynamic and static builds.

  callpath-perf-final.py DYNAMIC STATIC

1. target/final-verify/checks.sh DYNAMIC STATIC callpath (11 groups; maphash-sparse
   is the known-red standalone check);
2. test/perf/standalone-checks.sh on DYNAMIC;
3. the anvil MCP smoke on DYNAMIC: initialize, keep stdin open about 25 s,
   then tools/list, expecting 40 tools.
Writes target/callpath-perf/final/report.json, pinning every output, for
`callpath-perf-check.py final`.  Run it under the 8 GiB cap through heavy-run.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf/final"


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def source_identity():
    spec = importlib.util.spec_from_file_location("cpp_measure", ROOT / "tools/ai/callpath-perf-measure.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.source_identity()


def run(name, argv, env=None, timeout=7200):
    out, err = OUT / f"{name}.out", OUT / f"{name}.err"
    with out.open("wb") as o, err.open("wb") as e:
        proc = subprocess.run(argv, cwd=ROOT, env=env, stdout=o, stderr=e, timeout=timeout)
    return {"command": argv, "returncode": proc.returncode, "stdout": pin(out), "stderr": pin(err)}


def anvil(binary):
    launcher = Path(os.environ.get("ANVIL_EL_DIR", Path.home() / ".emacs.d/external-packages/anvil.el")) \
        / "tools/anvil-standalone-mcp.sh"
    out, err = OUT / "anvil.out", OUT / "anvil.err"
    first = [{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
                 "protocolVersion": "2025-06-18", "capabilities": {},
                 "clientInfo": {"name": "callpath-final", "version": "1"}}},
             {"jsonrpc": "2.0", "method": "notifications/initialized"}]
    with out.open("wb") as o, err.open("wb") as e:
        proc = subprocess.Popen(["bash", str(launcher)], cwd=ROOT, stdin=subprocess.PIPE, stdout=o, stderr=e,
                                env=dict(os.environ, NELISP_BIN=str(Path(binary).resolve())))
        for m in first:
            proc.stdin.write((json.dumps(m) + "\n").encode())
        proc.stdin.flush()
        time.sleep(25)
        proc.stdin.write((json.dumps({"jsonrpc": "2.0", "id": 2, "method": "tools/list"}) + "\n").encode())
        proc.stdin.flush()
        deadline = time.time() + 120
        tools = None
        while time.time() < deadline and tools is None:
            time.sleep(2)
            for line in out.read_text(errors="replace").splitlines():
                try:
                    m = json.loads(line)
                except ValueError:
                    continue
                if isinstance(m, dict) and m.get("id") == 2 and "result" in m:
                    tools = len(m["result"].get("tools", []))
        proc.stdin.close()
        try:
            rc = proc.wait(timeout=60)
        except subprocess.TimeoutExpired:
            proc.kill()
            rc = proc.wait()
    return {"command": ["bash", str(launcher)], "returncode": 0 if tools is not None else (rc or 1),
            "launcher_returncode": rc, "tools": tools, "stdout": pin(out), "stderr": pin(err)}


def main():
    dynamic, static = map(lambda p: Path(p).resolve(), sys.argv[1:3])
    OUT.mkdir(parents=True, exist_ok=True)
    report = {"schema": 1, "source": source_identity(), "binaries": {"dynamic": pin(dynamic), "static": pin(static)},
              "producer": pin(__file__), "checks": {}}
    row = run("checks", ["bash", "target/final-verify/checks.sh", str(dynamic), str(static), "callpath"])
    text = Path(row["stdout"]["path"]).read_text(errors="replace")
    match = re.search(r"GATE-COUNT: (\d+) cases, (\d+) failures", text)
    row["groups"], row["failures"] = (int(match.group(1)), int(match.group(2))) if match else (0, -1)
    report["checks"] = {"dynamic": row, "static": row}
    print(f"checks groups={row['groups']} failures={row['failures']} rc={row['returncode']}", flush=True)
    env = dict(os.environ, EVAL_BIN=str(dynamic), STANDALONE_CHECK_LOGDIR=str(OUT / "standalone-logs"))
    report["standalone"] = run("standalone", ["sh", "test/perf/standalone-checks.sh"], env=env)
    print(f"standalone rc={report['standalone']['returncode']}", flush=True)
    report["anvil"] = anvil(dynamic)
    print(f"anvil tools={report['anvil']['tools']}", flush=True)
    if report["source"] != source_identity():
        raise SystemExit("source changed during the final verification")
    (OUT / "report.json").write_text(json.dumps(report, indent=1) + "\n")


if __name__ == "__main__":
    main()
