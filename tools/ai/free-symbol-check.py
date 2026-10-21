#!/usr/bin/env python3
"""Receipt-backed checks for the free-symbol ledger.

reject runs small production object-compiler fixtures.  census and final only
consume evidence; they never trigger builds or repeat the final suites.
Record one actual run with:
  free-symbol-check.py record build-dynamic --artifact BIN -- COMMAND ...
  free-symbol-check.py record build-static --artifact BIN -- COMMAND ...
  free-symbol-check.py record common --artifact DYN --artifact STATIC -- COMMAND ...
  free-symbol-check.py record perf --artifact DYN -- COMMAND ...
  free-symbol-check.py record mcp --artifact DYN
The recorder supplies heavy-run and the 8 GiB limit.  Static builds must unset
NELISP_READER_DYNAMIC; the string "0" enables dynamic mode in the Lisp builder.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
EVIDENCE = ROOT / "target/free-symbol"
RECEIPTS = EVIDENCE / "receipts"
FIXTURE = ROOT / "test/nelisp-native-free-symbol-fixture.el"
COMPILER = ROOT / "lisp/nelisp-aot-compiler.el"
NOTES = next((parent for parent in ROOT.parents if (parent / "bin/heavy-run.sh").is_file()), None)
HEAVY = os.environ.get("HEAVY_RUN", str(NOTES / "bin/heavy-run.sh") if NOTES else "heavy-run.sh")
ANVIL = Path(os.environ.get("ANVIL_EL_DIR", Path.home() / ".emacs.d/external-packages/anvil.el"))
MCP = os.environ.get("MCP_SCRIPT", str(ANVIL / "tools/anvil-standalone-mcp.sh"))
LIMIT = 8388608 * 1024
KINDS = ("build-dynamic", "build-static", "common", "perf", "mcp")


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def limited():
    resource.setrlimit(resource.RLIMIT_AS, (LIMIT, LIMIT))


def source_hash():
    """Hash actual build inputs, including uncommitted and untracked sources."""
    paths = set()
    for options in (("--cached",), ("--others", "--exclude-standard")):
        output = subprocess.check_output(
            ["git", "ls-files", "-z", *options, "--", "src", "lisp", "scripts",
             "packages", "Makefile"], cwd=ROOT)
        paths.update(Path(p.decode()) for p in output.split(b"\0") if p)
    sha = hashlib.sha256()
    for path in sorted(paths):
        if path.suffix not in (".el", ".c", ".h", ".py", ".sh") and path.name != "Makefile":
            continue
        sha.update(str(path).encode() + b"\0")
        sha.update((ROOT / path).read_bytes() if (ROOT / path).is_file() else b"DELETED")
    return sha.hexdigest()


def harnesses(kind):
    if kind == "common":
        return [ROOT / "target/final-verify/checks.sh",
                ROOT / "target/final-verify/word-heap-final-checks.sh",
                *harnesses("perf"), *sorted(ROOT.glob("test/perf/fx-*-check.*")),
                ROOT / "test/perf/fx-corpus.el",
                *sorted(ROOT.glob("tools/nelisp-thread-*-smoke.el")),
                ROOT / "tools/ai/native-free-symbol-census.py"]
    if kind == "perf":
        return [ROOT / "test/perf/standalone-checks.sh",
                ROOT / "test/perf/standalone-checks-known-red.txt",
                *sorted(ROOT.glob("test/standalone-*-check.el")),
                *sorted(ROOT.glob("test/standalone-*-check.expected"))]
    if kind == "mcp":
        return [Path(MCP), Path(MCP).with_suffix(".el"), Path(__file__)]
    return []


def identities(paths):
    return {str(Path(p).resolve()): digest(p) for p in paths}


def receipt(kind):
    path = RECEIPTS / f"{kind}.json"
    data = json.loads(path.read_text())
    assert data["kind"] == kind and data["returncode"] == 0 and data["checks_passed"], f"{kind}: unsuccessful run"
    assert data["source_before"] == data["source_after"] == source_hash(), f"{kind}: stale source"
    assert data["log_sha256"] == digest(data["log"]), f"{kind}: changed log"
    assert data["artifacts"] and data["artifacts"] == identities(data["artifacts"]), f"{kind}: changed artifact"
    assert data["harnesses"] == identities(harnesses(kind)), f"{kind}: changed harness"
    return data, Path(data["log"]).read_text(errors="replace")


def baseline():
    data = json.loads((EVIDENCE / "base-reject.json").read_text())
    assert data["returncode"] == 0, "baseline did not accept bad fixture"
    for field, filename in (("compiler_sha256", "base-compiler.el"),
                            ("fixture_sha256", "base-fixture.el"),
                            ("artifact_sha256", "base-bad.o"),
                            ("log_sha256", "base-bad.log")):
        assert data[field] == digest(EVIDENCE / filename), f"baseline changed: {filename}"
    assert "FREE-SYMBOL-FIXTURE compiled bad" in (EVIDENCE / "base-bad.log").read_text()


def reject(compiler=None):
    baseline()
    good = ("defvar", "defconst", "declaration", "global", "lexical", "parameter")
    cases = ("bad", "bad-let", *good, "plain-bad")
    failed = 0
    for case in cases:
        output = EVIDENCE / f"fixture-{case}.o"
        output.unlink(missing_ok=True)
        env = dict(os.environ, FREE_SYMBOL_CASE=case, FREE_SYMBOL_OUTPUT=str(output),
                   TMPDIR=str(ROOT / "target/tmp"))
        env["FREE_SYMBOL_COMPILER"] = str(Path(compiler or COMPILER).resolve())
        command = ["emacs", "-Q", "--batch", "-L", "src", "-L", "lisp", "-L", "scripts", "-l", str(FIXTURE)]
        run = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                             text=True, timeout=90, preexec_fn=limited)
        log = run.stdout + run.stderr
        (EVIDENCE / f"fixture-{case}.log").write_text(log)
        if case in good:
            ok = run.returncode == 0 and output.is_file() and output.stat().st_size > 0
            ok = ok and f"FREE-SYMBOL-FIXTURE compiled {case}" in log
        else:
            ok = run.returncode != 0 and not output.exists() and ":free-symbol free_symbol_absent" in log
            if case in ("bad", "bad-let"):
                name = "free_symbol_bad_let" if case == "bad-let" else "free_symbol_bad"
                ok = ok and name in log
        failed += not ok
        print(f"FREE-SYMBOL {case} {'PASS' if ok else 'FAIL'} rc={run.returncode}")
    print(f"FREE-SYMBOL reject checked={len(cases)} failures={failed} baseline-accepted=1")
    return int(failed != 0)


def census():
    artifacts = []
    for kind in ("build-dynamic", "build-static"):
        data, _ = receipt(kind)
        assert len(data["artifacts"]) == 1, f"{kind}: expected one binary"
        artifacts.extend(data["artifacts"])
    assert artifacts[0] != artifacts[1], "dynamic and static paths must differ"
    assert digest(artifacts[0]) != digest(artifacts[1]), "dynamic and static artifacts are identical"
    for binary in artifacts:
        run = subprocess.run([sys.executable, str(ROOT / "tools/ai/native-free-symbol-census.py"),
                              "check", binary, "--max", "0"], cwd=ROOT,
                             text=True, capture_output=True, timeout=180, preexec_fn=limited)
        log = run.stdout + run.stderr
        print(log.strip())
        assert run.returncode == 0 and "sites=0 max=0" in log, "nonzero census or missing coverage"
    print("FREE-SYMBOL census checked=2 failures=0")
    return 0


def final():
    dyn, _ = receipt("build-dynamic")
    static, _ = receipt("build-static")
    binaries = {**dyn["artifacts"], **static["artifacts"]}
    common, common_log = receipt("common")
    perf, perf_log = receipt("perf")
    mcp, mcp_log = receipt("mcp")
    assert common["artifacts"] == binaries, "COMMON did not test both final builds"
    dynamic = dyn["artifacts"]
    assert perf["artifacts"] == mcp["artifacts"] == dynamic, "perf/MCP did not test dynamic build"
    assert re.search(r"^GATE-COUNT: 11 cases, 0 failures$", common_log, re.M), "COMMON summary missing"
    groups = re.findall(r"^([\w-]+) rc=(\d+) \d+s$", common_log, re.M)
    expected = {"standalone-checks", "standalone-shallow-binding-check",
                "standalone-shallow-binding-dyn-check", "standalone-fx-call-shape-check",
                "standalone-bytecode-word-check", "fx-spike", "fx-corpus",
                "thread-standalone", "thread-allocating", "thread-percpu", "thread-mirror-guard"}
    assert len(groups) == 11 and {name for name, _ in groups} == expected and all(rc == "0" for _, rc in groups), "COMMON group coverage"
    assert "STANDALONE-CHECKS PASS" in perf_log, "perf summary missing"
    count = len(list(ROOT.glob("test/standalone-*-check.el")))
    assert re.search(rf"^GATE-COUNT: {count} cases, 0 failures$", perf_log, re.M), "incomplete perf corpus"
    assert "STANDALONE-CHECK FAIL" not in perf_log, "unexpected perf failure"
    assert "FREE-SYMBOL MCP tools=40" in mcp_log, "MCP tool count missing"
    assert mcp["argv"][1:2] == [str(Path(__file__).resolve())], "MCP runner mismatch"
    print(f"FREE-SYMBOL final COMMON=11/11 standalone-checks={count}/{count} MCP=40 failures=0")
    return 0


def run_mcp(binary):
    """Keep the server's stdin open through startup, then request its tools."""
    env = dict(os.environ, NELISP_BIN=str(Path(binary).resolve()),
               ANVIL_STANDALONE_NO_IMAGE="1")
    process = subprocess.Popen([MCP], cwd=ROOT, env=env, stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    time.sleep(25)
    request = json.dumps({"jsonrpc": "2.0", "id": 71, "method": "tools/list", "params": {}})
    try:
        out, err = process.communicate(request + "\n", timeout=240)
    except subprocess.TimeoutExpired:
        process.kill()
        process.communicate()
        raise AssertionError("MCP startup/request timed out")
    print(out, end="")
    print(err, end="", file=sys.stderr)
    responses = []
    for line in out.splitlines():
        try:
            item = json.loads(line)
            if isinstance(item, dict) and item.get("id") == 71:
                responses.append(item)
        except json.JSONDecodeError:
            pass
    assert process.returncode == 0 and len(responses) == 1, "MCP missing unique response"
    tools = responses[0].get("result", {}).get("tools", [])
    assert len(tools) == 40 and len({tool["name"] for tool in tools}) == 40, "MCP expected 40 unique tools"
    print("FREE-SYMBOL MCP tools=40")
    return 0


def record(kind, artifacts, argv):
    RECEIPTS.mkdir(parents=True, exist_ok=True)
    assert artifacts, "record needs --artifact"
    artifacts = [str(Path(path).resolve()) for path in artifacts]
    if kind == "mcp":
        assert not argv and len(artifacts) == 1, "MCP takes one artifact and supplies its runner"
        argv = [sys.executable, str(Path(__file__).resolve()), "_mcp", artifacts[0]]
    else:
        assert argv, "record needs a command after --"
    joined = " ".join(argv)
    if kind.startswith("build-"):
        assert len(artifacts) == 1, "build needs exactly one output artifact"
        assert "nelisp-standalone-build.el" in joined or "standalone-reader" in joined, "not a standalone build command"
        assert f"NELISP_STANDALONE_READER_OUTPUT={artifacts[0]}" in joined, "build output must name the recorded artifact"
        if kind == "build-dynamic":
            assert "NELISP_READER_DYNAMIC=1" in joined, "dynamic build must set NELISP_READER_DYNAMIC=1"
        else:
            assert "-u NELISP_READER_DYNAMIC" in joined and "NELISP_READER_DYNAMIC=" not in joined, "static build must unset NELISP_READER_DYNAMIC"
    elif kind == "common":
        assert "target/final-verify/checks.sh" in joined, "not the COMMON harness"
        assert len(artifacts) == 2 and all(path in joined for path in artifacts), "COMMON must use both artifacts"
    elif kind == "perf":
        assert "test/perf/standalone-checks.sh" in joined, "not the standalone-checks harness"
        assert len(artifacts) == 1 and f"EVAL_BIN={artifacts[0]}" in joined, "perf must use recorded artifact"
        assert not any("standalone-" in arg and arg.endswith("-check.el") for arg in argv), "perf must run entire corpus"
    before = source_hash()
    before_artifacts = identities(artifacts) if not kind.startswith("build-") else None
    harness_before = identities(harnesses(kind))
    log = RECEIPTS / f"{kind}.log"
    started = time.time()
    env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"))
    Path(env["TMPDIR"]).mkdir(parents=True, exist_ok=True)
    with log.open("w") as stream:
        run = subprocess.run([HEAVY, *argv], cwd=ROOT, stdout=stream,
                             stderr=subprocess.STDOUT, env=env, preexec_fn=limited)
    after_artifacts = identities(artifacts)
    stable_artifacts = before_artifacts is None or before_artifacts == after_artifacts
    stable_harnesses = harness_before == identities(harnesses(kind))
    source_after = source_hash()
    fresh = not kind.startswith("build-") or run.returncode != 0 or all(
        Path(path).stat().st_mtime >= started for path in artifacts)
    checks_passed = stable_artifacts and stable_harnesses and before == source_after and fresh
    data = {"kind": kind, "argv": argv, "returncode": run.returncode, "checks_passed": checks_passed,
            "started": started, "finished": time.time(), "source_before": before,
            "source_after": source_after, "artifacts": after_artifacts,
            "harnesses": harness_before, "log": str(log), "log_sha256": digest(log)}
    (RECEIPTS / f"{kind}.json").write_text(json.dumps(data, indent=2) + "\n")
    assert checks_passed, "run changed source/harness/artifact or did not produce fresh build"
    print(f"FREE-SYMBOL recorded {kind} rc={run.returncode} log={log.relative_to(ROOT)}")
    return run.returncode


def main():
    os.chdir(ROOT)
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="mode", required=True)
    reject_parser = commands.add_parser("reject")
    reject_parser.add_argument("--compiler")
    commands.add_parser("census")
    commands.add_parser("final")
    rec = commands.add_parser("record")
    rec.add_argument("kind", choices=KINDS)
    rec.add_argument("--artifact", action="append", required=True)
    internal = commands.add_parser("_mcp")
    internal.add_argument("binary")
    raw = sys.argv[1:]
    if "--" in raw:
        index = raw.index("--")
        arguments, argv = raw[:index], raw[index + 1:]
    else:
        arguments, argv = raw, []
    args = parser.parse_args(arguments)
    try:
        if args.mode == "record":
            return record(args.kind, args.artifact, argv)
        if args.mode == "reject":
            return reject(args.compiler)
        if args.mode == "_mcp":
            return run_mcp(args.binary)
        return {"census": census, "final": final}[args.mode]()
    except (AssertionError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"FREE-SYMBOL {args.mode} FAIL: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
