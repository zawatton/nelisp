#!/usr/bin/env python3
"""Reproducible standalone boot measurements, retaining raw evidence in target/."""
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import json
import os
from pathlib import Path
import re
import selectors
import shutil
import signal
import statistics
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/boot-image"
OUT.mkdir(parents=True, exist_ok=True)
ENV = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"), PYTHONDONTWRITEBYTECODE="1")
Path(ENV["TMPDIR"]).mkdir(parents=True, exist_ok=True)


def execute(argv, name, *, env=None, timeout=600):
    started = time.monotonic()
    with (OUT / (name + ".out")).open("wb") as out, (OUT / (name + ".err")).open("wb") as err:
        child = subprocess.Popen(list(map(str, argv)), cwd=ROOT, env=env or ENV,
                                 stdin=subprocess.DEVNULL, stdout=out, stderr=err,
                                 start_new_session=True)
        try:
            rc = child.wait(timeout=timeout)
        finally:
            if child.poll() is None:
                os.killpg(child.pid, signal.SIGKILL)
                child.wait()
    return dict(rc=rc, seconds=time.monotonic() - started,
                stdout=(OUT / (name + ".out")).read_text(errors="replace"),
                stderr=(OUT / (name + ".err")).read_text(errors="replace"))


def ir(binary, name, env=None):
    profile = OUT / (name + ".callgrind")
    result = execute(["valgrind", "--tool=callgrind", "--callgrind-out-file=" + str(profile),
                      binary, "--eval", "nil"], name, env=env)
    assert result["rc"] == 0, result
    assert "nelisp: uncaught" not in result["stderr"], "Invalid measurement: Lisp startup error"
    counts = re.findall(r"^summary: (\d+)$", profile.read_text(), re.M)
    assert len(counts) == 1, "Missing or ambiguous instruction count"
    result["ir"] = int(counts[0])
    return result


def smoke(binary, name):
    anvil = Path(os.environ.get("ANVIL_EL_DIR", str(Path.home() / ".emacs.d/external-packages/anvil.el")))
    env = dict(ENV, NELISP_BIN=str(binary))
    # Independent DBs keep this protocol smoke from writing the live indexes.
    dbdir = OUT / (name + "-db")
    dbdir.mkdir(exist_ok=True)
    env.update(ANVIL_MEMORY_DB=str(dbdir / "memory.db"),
               ANVIL_WORKLOG_DB=str(dbdir / "worklog.db"))
    started = time.monotonic()
    with (OUT / (name + ".err")).open("wb") as err:
        child = subprocess.Popen([str(anvil / "tools/anvil-standalone-mcp.sh")],
                                 cwd=ROOT, env=env, stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE, stderr=err, start_new_session=True)
        selector = selectors.DefaultSelector()
        selector.register(child.stdout, selectors.EVENT_READ)
        received = bytearray()
        messages = []
        times = {}

        def send(value):
            child.stdin.write((json.dumps(value) + "\n").encode())
            child.stdin.flush()

        try:
            send(dict(jsonrpc="2.0", id=1, method="initialize", params=dict(
                protocolVersion="2024-11-05", capabilities={},
                clientInfo=dict(name="boot-image-smoke", version="1"))))
            listed = False
            while time.monotonic() - started < 60:
                if not listed and time.monotonic() - started >= 25:
                    send(dict(jsonrpc="2.0", method="notifications/initialized"))
                    send(dict(jsonrpc="2.0", id=2, method="tools/list", params={}))
                    listed = True
                for key, _ in selector.select(0.1):
                    chunk = os.read(key.fileobj.fileno(), 65536)
                    if not chunk:
                        raise RuntimeError("Anvil exited before both protocol responses")
                    received.extend(chunk)
                    # The Anvil batch transport selects newline JSON from our first request.
                    for line in received.splitlines():
                        try:
                            message = json.loads(line)
                        except (ValueError, UnicodeDecodeError):
                            continue
                        if message.get("id") not in times:
                            messages.append(message)
                            times[message.get("id")] = time.monotonic() - started
                if 1 in times and 2 in times:
                    init = next(m for m in messages if m.get("id") == 1)
                    listing = next(m for m in messages if m.get("id") == 2)
                    assert "result" in init, init
                    assert len(listing["result"]["tools"]) == 40, listing
                    return dict(startup_seconds=times[1], tools=40)
            raise RuntimeError("Anvil protocol timeout")
        finally:
            (OUT / (name + ".out")).write_bytes(received)
            selector.close()
            os.killpg(child.pid, signal.SIGKILL) if child.poll() is None else None
            child.wait()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=["ir", "wall", "opens", "isolated", "smoke", "e2e", "apportion"])
    parser.add_argument("--name", default="candidate")
    parser.add_argument("--resume", action="store_true", help="Reuse completed apportionment pairs for this binary hash")
    parser.add_argument("--jobs", type=int, default=2, choices=range(1, 5))
    args = parser.parse_args()
    binary = Path(os.environ["BOOT_IMAGE_BIN"]).resolve(strict=True)
    result = dict(binary=str(binary), sha256=hashlib.sha256(binary.read_bytes()).hexdigest())
    if args.mode == "ir":
        result.update(ir(binary, args.name))
        assert result["ir"] <= int(os.environ.get("BOOT_IMAGE_IR_CAP", "14500000000")), result
    elif args.mode == "wall":
        samples = [execute([binary, "--eval", "nil"], args.name + "-wall") for _ in range(5)]
        assert all(s["rc"] == 0 and s["stdout"] == "nil\n" and not s["stderr"] for s in samples), samples
        result.update(seconds=statistics.median(s["seconds"] for s in samples), samples=samples)
    elif args.mode == "opens":
        trace = OUT / (args.name + ".strace")
        run = execute(["strace", "-f", "-e", "trace=open,openat", "-o", trace,
                       binary, "--eval", "nil"], args.name + "-opens")
        if run["rc"] != 0 and "Operation not permitted" in run["stderr"]:
            # Valgrind observes the actual syscalls in-process and requires no
            # ptrace permission; retain the exact strace denial beside it.
            trace = OUT / (args.name + "-syscalls.log")
            run = execute(["valgrind", "--tool=none", "--trace-syscalls=yes",
                           "--log-file=" + str(trace), binary, "--eval", "nil"], args.name + "-syscalls")
            assert run["rc"] == 0 and run["stdout"] == "nil\n" and not run["stderr"], run
            opened = re.findall(r'sys_open(?:at)?\s*\([^\n]*?\(([^\n]*?\.elc?)\)', trace.read_text())
            assert "sys_open" in trace.read_text(), "Empty syscall observation"
            result["mechanism"] = "Valgrind syscall trace (strace ptrace denied)"
        else:
            assert run["rc"] == 0 and run["stdout"] == "nil\n" and not run["stderr"], run
            opened = re.findall(r'"([^"\n]+\.elc?)"', trace.read_text())
        assert not opened, opened
        result.update(opens=opened)
    elif args.mode == "isolated":
        with tempfile.TemporaryDirectory(prefix="boot-image-", dir="/tmp") as directory:
            deployed = Path(directory) / "nelisp"
            for suffix in ("", ".cold", ".native-startup.el"):
                source = Path(str(binary) + suffix)
                if source.exists():
                    shutil.copy2(source, str(deployed) + suffix)
            for expression, expected in [("nil", "nil\n"), ("(progn (require 'rx) (rx \"a\"))", '"a"\n')]:
                run = execute(["bwrap", "--ro-bind", "/", "/", "--tmpfs", ROOT,
                               "--chdir", directory, deployed, "--eval", expression], args.name + "-isolated")
                assert run["rc"] == 0 and run["stdout"] == expected and not run["stderr"], run
            script = Path(directory) / "boot-libraries-check.el"
            shutil.copy2(ROOT / "test/standalone-boot-libraries-check.el", script)
            run = execute(["bwrap", "--ro-bind", "/", "/", "--tmpfs", ROOT,
                           "--chdir", directory, deployed, "--load", script], args.name + "-isolated-libraries")
            assert run["rc"] == 0 and "checks=23 failures=0 PASS" in run["stdout"] and not run["stderr"], run
        result.update(mechanism="bwrap --tmpfs over build worktree; executable and sidecars copied to /tmp")
    elif args.mode == "smoke":
        result.update(smoke(binary, args.name + "-anvil"))
    elif args.mode == "apportion":
        rows = []
        receipt = OUT / "apportion.json"
        baseline = None
        if args.resume and receipt.exists():
            previous = json.loads(receipt.read_text())
            assert previous["binary_sha256"] == result["sha256"], "Apportionment binary changed"
            rows = previous["rows"]
            baseline = previous["baseline"]
        if baseline is None:
            baseline = ir(binary, "apportion-full", dict(ENV, NELISP_BOOT_SKIP="none"))
            assert baseline["stdout"].endswith("nil\n") and "BOOT-LOAD-HISTORY:" in baseline["stdout"], baseline

        def measure(feature):
            # Stop immediately after the selected call on BOTH sides. Required
            # providers cannot be omitted from a complete, semantically valid
            # boot; matching prefixes isolate their load without later errors.
            stop = "inline" if feature == "macroexp" else feature
            loaded = ir(binary, "apportion-" + feature + "-loaded", dict(ENV, NELISP_BOOT_STOP=stop, NELISP_BOOT_SKIP="none"))
            stubbed = ir(binary, "apportion-" + feature + "-stubbed", dict(ENV, NELISP_BOOT_STOP=stop, NELISP_BOOT_SKIP=feature))
            assert loaded["stdout"] == stubbed["stdout"] == "nil\n", (loaded, stubbed)
            return dict(feature=feature, ir=loaded["ir"]-stubbed["ir"], loaded=loaded, stubbed=stubbed)

        features = ["backquote", "byte-run", "macroexp", "inline", "json", "easy-mmode", "rx", "cl-seq", "nelisp-process-buffer"]
        remaining = [f for f in features if f not in {row["feature"] for row in rows}]
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            for future in as_completed([pool.submit(measure, f) for f in remaining]):
                row = future.result()
                rows.append(row)
                rows.sort(key=lambda r: features.index(r["feature"]))
                receipt.write_text(json.dumps(dict(binary_sha256=result["sha256"], baseline=baseline, rows=rows), indent=2) + "\n")
                print(row["feature"], row["ir"], flush=True)
        result.update(rows=rows, baseline=baseline)
    else:
        result["standalone"] = execute(["sh", "test/perf/standalone-checks.sh"], args.name + "-standalone",
                                       env=dict(ENV, EVAL_BIN=str(binary)), timeout=540)
        result["verify"] = execute(["sh", "tools/ai/nelisp-ai.sh", "verify"], args.name + "-verify")
        result["anvil"] = smoke(binary, args.name + "-anvil")
        (OUT / (args.name + "-e2e.json")).write_text(json.dumps(result, indent=2) + "\n")
        if result["standalone"]["rc"] or result["verify"]["rc"]:
            raise RuntimeError("S5 failed: standalone rc=%s, verify rc=%s; see target/boot-image/%s-e2e.json" %
                               (result["standalone"]["rc"], result["verify"]["rc"], args.name))
    (OUT / (args.name + "-" + args.mode + ".json")).write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({k: v for k, v in result.items() if k not in ("rows", "baseline", "samples", "standalone", "verify", "stdout", "stderr")}))


if __name__ == "__main__":
    main()
