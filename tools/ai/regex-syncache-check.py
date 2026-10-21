#!/usr/bin/env python3
"""Scoped syntax-cache checks; raw, hash-bound evidence lives in target/.

record-speed --label base --binary target/regex-syncache/base-dynamic
record-speed --label final --binary target/regex-syncache/dynamic
speed and final validate retained evidence without rerunning measurements or
the final suites.  --binary selects a negative control for semantics/api.
"""
import argparse
import ast
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import shutil
import signal
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/regex-syncache"
FIXTURE = "test/fixtures/regex-syncache.el"
ORACLE = "test/fixtures/regex-syncache.gnu-31.1.expected"
IDENTITY = "test/fixtures/regex-syncache-identity.el"
API = "test/fixtures/regex-syncache-api.el"
SPEED = "test/fixtures/regex-syncache-speed.el"
RUN_FIXTURE = "test/fixtures/regex-syntax-run.el"
RUN_ORACLE = "test/fixtures/regex-syntax-run.gnu-31.1.expected"
LAYOUTS = (0, 700, 1400, 2100)
PATTERNS = {"syntax": r"\`\(\sw\|\s_\)*\'", "plain": r"\`[a-z_]*\'"}
SOURCES = ("lisp/nelisp-stdlib-regexp.el", "tools/ai/regex-syncache-check.py",
           FIXTURE, ORACLE, IDENTITY, API, SPEED,
           "test/nelisp-regexp-syncache-test.el",
           "test/nelisp-shadow-differential-cases.el",
           RUN_FIXTURE, RUN_ORACLE, "test/nelisp-regexp-syntax-run-test.el",
           "test/standalone-regexp-syncache-check.el",
           "test/standalone-regexp-syncache-check.expected")
ENV = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"), PYTHONDONTWRITEBYTECODE="1")
Path(ENV["TMPDIR"]).mkdir(parents=True, exist_ok=True)


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def digest(path):
    h = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def rooted(path):
    path = Path(path)
    return path if path.is_absolute() else ROOT / path


def identity(path):
    path = rooted(path).resolve()
    require(path.is_file(), f"missing artifact: {path}")
    return {"path": str(path), "sha256": digest(path)}


def check_identity(record):
    require(digest(rooted(record["path"])) == record["sha256"],
            f"artifact changed: {record['path']}")


def save(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")


def run(argv, stem, *, timeout=300, env=None):
    stem.parent.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    with stem.with_suffix(".out").open("wb") as out, stem.with_suffix(".err").open("wb") as err:
        child = subprocess.Popen(list(map(str, argv)), cwd=ROOT, env=env or ENV,
                                 stdin=subprocess.DEVNULL, stdout=out, stderr=err,
                                 start_new_session=True)
        try:
            rc = child.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(child.pid, signal.SIGKILL)
            child.wait()
            raise RuntimeError(f"timeout after {timeout}s: {stem}") from None
        finally:
            if child.poll() is None:
                os.killpg(child.pid, signal.SIGKILL)
                child.wait()
    result = {"command": list(map(str, argv)), "returncode": rc,
              "seconds": time.monotonic() - started,
              "stdout": identity(stem.with_suffix(".out")),
              "stderr": identity(stem.with_suffix(".err"))}
    save(stem.with_suffix(".json"), result)
    return result


def heavy_runner():
    """Find the workspace scheduler without changing PATH or pinning a host."""
    candidates = [parent / "bin/heavy-run.sh" for parent in (ROOT, *ROOT.parents)]
    path = next((path.resolve() for path in candidates if path.is_file()), None)
    require(path is not None, "workspace bin/heavy-run.sh was not found")
    return path


def stop_process_tree(child):
    """Also stop the profiler, which owns a separate process group."""
    rows = subprocess.check_output(["ps", "-eo", "pid=,ppid="], text=True)
    parents = {int(pid): int(parent) for pid, parent in
               (row.split() for row in rows.splitlines())}
    descendants = {child.pid}
    while True:
        expanded = descendants | {pid for pid, parent in parents.items() if parent in descendants}
        if expanded == descendants:
            break
        descendants = expanded
    for pid in sorted(descendants, reverse=True):
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    child.wait()


def profile_one(args):
    """Private entry point: heavy-run has acquired one profile slot already."""
    require(args.job is not None, "_profile-one requires --job")
    job = json.loads(rooted(args.job).read_text())
    require(job["schema"] == 1, "invalid private profile job")
    limits = list(resource.getrlimit(resource.RLIMIT_AS))
    require(limits[0] == 8388608 * 1024 and limits == job["address_space_limit_bytes"],
            "private profiler must inherit the campaign's 8 GiB cap")
    require(ENV.get("ABI_LAYOUT_PAD") == job["abi_layout_pad"],
            "private profiler did not inherit its requested ABI layout")
    require(shutil.disk_usage(ROOT).free >= 50 * 1000 ** 3, "less than 50 GB free")
    stem = rooted(job["stem"])
    acquired = time.monotonic()
    started_wall = time.time()
    try:
        result = run(job["command"], stem, timeout=job["timeout_seconds"], env=ENV)
    except RuntimeError as error:
        result = {"command": job["command"], "returncode": 124,
                  "seconds": time.monotonic() - acquired, "error": str(error),
                  "stdout": identity(stem.with_suffix(".out")),
                  "stderr": identity(stem.with_suffix(".err"))}
    result.update(address_space_limit_bytes=limits,
                  acquired_helper_monotonic=acquired,
                  started_wall_clock=started_wall,
                  timeout_seconds=job["timeout_seconds"],
                  queue_and_launcher_seconds=acquired - job["scheduled_monotonic"],
                  timing_scope="actual child execution inside acquired heavy slot")
    save(stem.with_suffix(".execution.json"), result)
    return result["returncode"]


def scheduled_profile(argv, stem, *, timeout, env):
    """Schedule ONE profile; the queue has no execution timeout."""
    stem.parent.mkdir(parents=True, exist_ok=True)
    scheduler = identity(heavy_runner())
    job_path = stem.with_suffix(".job.json")
    queued = time.monotonic()
    save(job_path, {"schema": 1, "command": list(map(str, argv)), "stem": str(stem),
                    "timeout_seconds": timeout, "scheduled_monotonic": queued,
                    "address_space_limit_bytes": list(resource.getrlimit(resource.RLIMIT_AS)),
                    "abi_layout_pad": env.get("ABI_LAYOUT_PAD"), "scheduler": scheduler})
    command = [scheduler["path"], sys.executable, str(Path(__file__).resolve()),
               "_profile-one", "--job", str(job_path)]
    stdout = stem.with_suffix(".scheduler.out")
    stderr = stem.with_suffix(".scheduler.err")
    with stdout.open("wb") as out, stderr.open("wb") as err:
        child = subprocess.Popen(command, cwd=ROOT, env=env, stdin=subprocess.DEVNULL,
                                 stdout=out, stderr=err, start_new_session=True)
        try:
            rc = child.wait()  # Scheduler queue time is deliberately unbounded.
        finally:
            if child.poll() is None:
                stop_process_tree(child)
    execution = stem.with_suffix(".execution.json")
    require(execution.is_file(), f"private profile did not produce metadata: {stderr}")
    result = json.loads(execution.read_text())
    require(rc == result["returncode"], f"scheduler/helper exit mismatch: {stderr}")
    require(stdout.read_bytes() == b"" and stderr.read_bytes() == b"",
            f"unexpected scheduler diagnostics: {stderr}")
    check_identity(scheduler)
    result.update(scheduler=scheduler, scheduler_command=command,
                  scheduler_stdout=identity(stdout), scheduler_stderr=identity(stderr),
                  scheduler_elapsed_seconds=time.monotonic() - queued,
                  execution=identity(execution), job=identity(job_path))
    save(stem.with_suffix(".json"), result)
    return result


def clean_output(result, expected):
    require(result["returncode"] == 0,
            f"exit {result['returncode']}; see {result['stderr']['path']}")
    require(rooted(result["stderr"]["path"]).read_bytes() == b"",
            f"unexpected stderr: {result['stderr']['path']}")
    actual = rooted(result["stdout"]["path"]).read_text()
    require(actual == expected, f"output mismatch: {result['stdout']['path']}")


def binaries(args):
    if args.binary:
        return [("override", rooted(args.binary).resolve())]
    return [(name, OUT / name) for name in ("dynamic", "static")]


def semantics(args):
    """GNU-visible parity plus a mechanism check that fails on the old cache."""
    oracle_dir = OUT / f"semantics-{args.label}"
    version = run([args.emacs, "--version"], oracle_dir / "gnu-version")
    require(version["returncode"] == 0, "cannot query GNU Emacs")
    require(rooted(version["stdout"]["path"]).read_text().splitlines()[0] == "GNU Emacs 31.1",
            "oracle must be GNU Emacs 31.1")
    expected = rooted(ORACLE).read_text()
    gnu = run([args.emacs, "-Q", "--batch", "-l", FIXTURE], oracle_dir / "gnu")
    clean_output(gnu, expected)
    run_expected = rooted(RUN_ORACLE).read_text()
    require(run_expected.strip(), "syntax-run GNU oracle is empty")
    gnu_run = run([args.emacs, "-Q", "--batch", "-l", RUN_FIXTURE], oracle_dir / "gnu-syntax-run")
    clean_output(gnu_run, run_expected)
    results = {"gnu": gnu, "gnu_syntax_run": gnu_run}
    failures = []
    for name, binary in binaries(args):
        # A sole --load prints its final Lisp value.  Two actions suppress
        # that CLI echo, keeping the portable fixture output identical to GNU.
        parity = run([binary, "--eval", "nil", "--load", FIXTURE], oracle_dir / (name + "-parity"))
        mechanism = run([binary, "--eval", "nil", "--load", IDENTITY], oracle_dir / (name + "-identity"))
        run_parity = run([binary, "--eval", "nil", "--load", RUN_FIXTURE],
                         oracle_dir / (name + "-syntax-run"))
        for check, want in ((parity, expected), (mechanism, "REGEXP-SYNCACHE identity PASS\n"),
                            (run_parity, run_expected)):
            try:
                clean_output(check, want)
            except RuntimeError as error:
                failures.append(str(error))
        results[name] = {"binary": identity(binary), "parity": parity, "identity": mechanism,
                         "syntax_run": run_parity}
    save(oracle_dir / "result.json", {"sources": {p: digest(rooted(p)) for p in SOURCES},
                                      "results": results, "failures": failures})
    require(not failures, "; ".join(failures))
    print(f"PASS semantics: GNU 31.1 parity and table identity ({len(binaries(args))} binaries)")


def api(args):
    results = {}
    for name, binary in binaries(args):
        result = run([binary, "--eval", "nil", "--load", API], OUT / f"api-{args.label}" / name)
        clean_output(result, "REGEXP-SYNCACHE api PASS\n")
        results[name] = {"binary": identity(binary), "result": result}
    # Verify the actual host-visible docstring, not a source substring.
    doc = run([args.emacs, "-Q", "--batch", "-L", "lisp", "-l",
               "test/nelisp-regexp-syncache-test.el", "--eval",
               "(ert-run-tests-batch-and-exit 'nelisp-regexp-syncache-public-api-documentation)"],
              OUT / f"api-{args.label}" / "documentation")
    require(doc["returncode"] == 0, f"public documentation check failed: {doc['stderr']['path']}")
    save(OUT / f"api-{args.label}" / "result.json",
         {"sources": {p: digest(rooted(p)) for p in SOURCES}, "results": results,
          "documentation": doc})
    print(f"PASS api: generation, repeated clear, mutation hook and plain reuse ({len(results)} binaries)")


def lisp_string(value):
    return '"' + value.replace('\\', '\\\\').replace('"', '\\"') + '"'


def measurement_protocol(source=None):
    """Bind performance-affecting code while allowing final-validator fixes."""
    source = source if source is not None else Path(__file__).read_text()
    functions = {node.name: ast.get_source_segment(source, node) + "\n"
                 for node in ast.parse(source).body if isinstance(node, ast.FunctionDef)}
    text = "\n".join(functions[name] for name in
                     ("run", "heavy_runner", "stop_process_tree", "profile_one",
                      "scheduled_profile", "lisp_string", "record_speed"))
    text += json.dumps({"layouts": LAYOUTS, "patterns": PATTERNS, "fixture": SPEED,
                        "environment": {k: ENV[k] for k in ("TMPDIR", "PYTHONDONTWRITEBYTECODE")}},
                       sort_keys=True)
    return hashlib.sha256(text.encode()).hexdigest()


def record_speed(args):
    require(args.binary is not None, "record-speed requires --binary")
    require(shutil.which("valgrind"), "valgrind is required")
    require(shutil.disk_usage(ROOT).free >= 50 * 1000 ** 3, "less than 50 GB free")
    require(0 <= args.low_iterations < args.iterations, "iterations must exceed low-iterations")
    binary = rooted(args.binary).resolve()
    before = identity(binary)
    speed_dir = OUT / f"speed-{args.label}"
    speed_dir.mkdir(parents=True, exist_ok=True)
    checker = speed_dir / "checker.py"
    shutil.copyfile(Path(__file__), checker)
    protocol = measurement_protocol()
    fixture_before = identity(rooted(SPEED))
    scheduler_before = identity(heavy_runner())
    rows = []
    for pad in LAYOUTS:
        for kind, pattern in PATTERNS.items():
            runs = []
            for count in (args.low_iterations, args.iterations):
                stem = speed_dir / f"pad-{pad}-{kind}-n{count}"
                profile = stem.with_suffix(".callgrind")
                expression = (f"(progn (setq regex-syncache-count {count} "
                              f"regex-syncache-pattern {lisp_string(pattern)}) nil)")
                env = dict(ENV, ABI_LAYOUT_PAD=str(pad))
                result = scheduled_profile(["valgrind", "--tool=callgrind", "--error-exitcode=99",
                              "--callgrind-out-file=" + str(profile), binary,
                              "--eval", expression, "--load", SPEED],
                             stem, env=env, timeout=args.timeout)
                require(result["returncode"] == 0,
                        f"Callgrind failed: {result['stderr']['path']}")
                require(rooted(result["stdout"]["path"]).read_text() ==
                        f"REGEXP-SYNCACHE speed {count} PASS\n",
                        f"measurement did not complete: {result['stdout']['path']}")
                summaries = re.findall(r"^summary: (\d+)$", profile.read_text(), re.M)
                require(len(summaries) == 1, f"expected one total Ir summary: {profile}")
                result.update(count=count, ir=int(summaries[0]), profile=identity(profile),
                              abi_layout_pad=pad)
                runs.append(result)
            calls = args.iterations - args.low_iterations
            delta = runs[1]["ir"] - runs[0]["ir"]
            require(delta > 0, "non-positive full-process Ir difference")
            rows.append({"pad": pad, "kind": kind, "runs": runs,
                         "calls": calls, "ir_per_call": delta / calls,
                         "seconds_per_call": (runs[1]["seconds"] - runs[0]["seconds"]) / calls})
        print(f"record-speed {args.label}: layout {pad} complete", flush=True)
    require(identity(binary) == before, "binary changed during measurement")
    require(identity(rooted(SPEED)) == fixture_before, "speed fixture changed during measurement")
    require(measurement_protocol() == protocol, "measurement protocol changed during measurement")
    require(identity(heavy_runner()) == scheduler_before, "heavy scheduler changed during measurement")
    means = {kind: statistics.mean(r["ir_per_call"] for r in rows if r["kind"] == kind)
             for kind in PATTERNS}
    data = {"schema": 1, "label": args.label, "binary": before,
            "fixture": fixture_before, "checker": identity(checker),
            "measurement_protocol_sha256": protocol,
            "scheduler": scheduler_before,
            "scheduling_policy": "one heavy-run slot per profile; queue time excluded from execution timeout and wall information",
            "metric": "full-process Callgrind Ir difference, GC included",
            "layouts": list(LAYOUTS), "patterns": PATTERNS, "rows": rows,
            "mean_ir_per_call": means, "ratio": means["syntax"] / means["plain"]}
    save(speed_dir / "measurements.json", data)
    print(f"RECORDED {args.label}: syntax={means['syntax']:.1f} plain={means['plain']:.1f} "
          f"Ir/call ratio={data['ratio']:.3f}")


def validate_speed(label):
    path = OUT / f"speed-{label}/measurements.json"
    data = json.loads(path.read_text())
    require(data["schema"] == 1 and data["label"] == label, f"invalid speed manifest: {path}")
    check_identity(data["binary"])
    check_identity(data["cold_image"])
    require(rooted(data["cold_image"]["path"]).resolve() ==
            Path(data["binary"]["path"] + ".cold").resolve(),
            "speed cold image is not the measured binary's sidecar")
    check_identity(data["fixture"])
    check_identity(data["checker"])
    check_identity(data["scheduler"])
    require(data["measurement_protocol_sha256"] == measurement_protocol(),
            "current measurement protocol differs from retained measurement")
    require(data["layouts"] == list(LAYOUTS) and data["patterns"] == PATTERNS,
            "speed layout/pattern mismatch")
    require(len(data["rows"]) == 8, "speed evidence must cover both patterns in all four layouts")
    observed = set()
    means = {}
    for row in data["rows"]:
        key = (row["pad"], row["kind"])
        require(key not in observed and row["pad"] in LAYOUTS and row["kind"] in PATTERNS,
                "duplicate or unknown speed row")
        observed.add(key)
        low, high = row["runs"]
        for result in (low, high):
            require(result["returncode"] == 0 and result["abi_layout_pad"] == row["pad"],
                    "failed or wrong-layout measurement")
            require(result["scheduler"] == data["scheduler"], "profile used a different scheduler")
            for key_name in ("stdout", "stderr", "profile", "execution", "job",
                             "scheduler_stdout", "scheduler_stderr"):
                check_identity(result[key_name])
            execution = json.loads(rooted(result["execution"]["path"]).read_text())
            job = json.loads(rooted(result["job"]["path"]).read_text())
            require(all(result[key] == value for key, value in execution.items()),
                    "profile timing/command differs from inside-slot execution record")
            require(job["command"] == result["command"] and
                    job["scheduler"] == data["scheduler"] and
                    job["timeout_seconds"] == execution["timeout_seconds"] and
                    execution["address_space_limit_bytes"] == data["address_space_limit_bytes"] and
                    job["address_space_limit_bytes"] == data["address_space_limit_bytes"] and
                    job["abi_layout_pad"] == str(row["pad"]) and
                    execution["queue_and_launcher_seconds"] >= 0 and
                    result["scheduler_elapsed_seconds"] >=
                    execution["queue_and_launcher_seconds"] + execution["seconds"],
                    "private profile job/resource/timing context mismatch")
            require(rooted(result["scheduler_stdout"]["path"]).read_bytes() == b"" and
                    rooted(result["scheduler_stderr"]["path"]).read_bytes() == b"",
                    "scheduler emitted unexpected diagnostics")
            diagnostics = rooted(result["stderr"]["path"]).read_text()
            unexpected = [line for line in diagnostics.splitlines() if line.strip() and
                          not re.match(r"^(?:==\d+==|--\d+--)", line)]
            require(not unexpected, f"unexpected CLI diagnostics: {result['stderr']['path']}")
            summaries = re.findall(r"^summary: (\d+)$",
                                   rooted(result["profile"]["path"]).read_text(), re.M)
            require(summaries == [str(result["ir"])], "profile Ir differs from manifest")
        calls = high["count"] - low["count"]
        require(calls == row["calls"] and calls > 0, "invalid call-count subtraction")
        require(row["ir_per_call"] == (high["ir"] - low["ir"]) / calls,
                "Ir difference differs from manifest")
    for kind in PATTERNS:
        means[kind] = statistics.mean(r["ir_per_call"] for r in data["rows"] if r["kind"] == kind)
    require(means == data["mean_ir_per_call"], "Ir means differ from raw profiles")
    require(data["ratio"] == means["syntax"] / means["plain"], "speed ratio differs from raw profiles")
    return data


def run_speed_with_cold_identity(args):
    """Bind the sidecar outside the unchanged measured campaign functions."""
    require(args.binary is not None, "record-speed requires --binary")
    binary = rooted(args.binary).resolve()
    cold = identity(Path(str(binary) + ".cold"))
    limits = resource.getrlimit(resource.RLIMIT_AS)
    require(limits[0] == 8388608 * 1024,
            "run record-speed under 'ulimit -v 8388608' (8 GiB)")
    record_speed(args)
    require(identity(Path(cold["path"])) == cold, "cold image changed during measurement")
    path = OUT / f"speed-{args.label}/measurements.json"
    data = json.loads(path.read_text())
    data["cold_image"] = cold
    data["address_space_limit_bytes"] = list(limits)
    save(path, data)
    validate_speed(args.label)


def speed(_args):
    base, final = validate_speed("base"), validate_speed("final")
    require(base["binary"]["sha256"] != final["binary"]["sha256"], "base/final binaries must differ")
    require(base["measurement_protocol_sha256"] == final["measurement_protocol_sha256"],
            "base/final measurements used different protocols")
    require(base["scheduler"] == final["scheduler"], "base/final used different heavy schedulers")
    require(base["address_space_limit_bytes"] == final["address_space_limit_bytes"] and
            base["address_space_limit_bytes"][0] == 8388608 * 1024,
            "base/final measurements must use the same 8 GiB address-space cap")
    require(final["binary"] == identity(OUT / "dynamic"), "speed result is not the final dynamic binary")
    require(base["ratio"] > 2, "baseline did not fail the unchanged 2x cap")
    require(final["ratio"] <= 2, f"syntax/plain ratio {final['ratio']:.3f} exceeds 2x")
    print(f"PASS speed: base {base['ratio']:.3f}x -> final {final['ratio']:.3f}x (cap 2x; 4-layout mean)")


def final(_args):
    """Validate the coordinator's already-run suites, never rerun them."""
    data = json.loads((OUT / "final-evidence.json").read_text())
    require(data["schema"] == 1, "invalid final evidence schema")
    limits = [8388608 * 1024] * 2
    require(data["address_space_limit_bytes"] == limits, "final evidence must record both 8 GiB limits")
    current = {"binaries": {name: identity(OUT / name) for name in ("dynamic", "static")},
               "cold_images": {name: identity(OUT / (name + ".cold")) for name in ("dynamic", "static")},
               "sources": {source: digest(rooted(source)) for source in SOURCES}}
    initial = {key: data[key] for key in current}
    require(initial == current, "final ELF, cold image, or source identity mismatch")
    require(data["artifacts_unchanged"] is True and data["after"] == initial,
            "final before/after snapshots differ or artifacts_unchanged is not true")
    text = {}
    for name in ("final_verify", "host_regexp", "standalone", "mcp"):
        suite = data["suites"][name]
        require(suite["returncode"] == 0, f"final suite failed: {name}")
        require(digest(rooted(suite["log"])) == suite["log_sha256"], f"changed suite log: {name}")
        if name != "mcp":
            unit_path = rooted(suite["unit_state"])
            require(digest(unit_path) == suite["unit_sha256"], f"changed suite unit state: {name}")
            unit = json.loads(unit_path.read_text())
            require(unit["status"] == "done" and unit["returncode"] == 0 and
                    unit["address_space_limit_bytes"] == limits and
                    not unit.get("timed_out") and not unit.get("error"),
                    f"suite unit did not complete successfully within the 8 GiB cap: {name}")
            require(type(unit["seconds"]) in (int, float) and unit["seconds"] >= 0 and
                    unit["seconds"] == suite["seconds"], f"suite/unit actual elapsed mismatch: {name}")
        text[name] = rooted(suite["log"]).read_text(errors="replace")
    require(re.search(r"^GATE-COUNT: 11 cases, 0 failures$", text["final_verify"], re.M),
            "final verification must report 11 groups and 0 failures")
    require(re.search(r"^STANDALONE-CHECKS PASS$", text["standalone"], re.M) and
            re.search(r"^GATE-COUNT: [1-9]\d* cases, 0 failures$", text["standalone"], re.M),
            "standalone checks did not pass")
    host_count = re.search(r"Ran ([1-9]\d*) tests, ([1-9]\d*) results as expected, 0 unexpected",
                           text["host_regexp"])
    require(host_count and host_count[1] == host_count[2],
            "host regexp ERT did not run a positive count with zero unexpected results")
    check = data["suites"]["standalone"]["syncache"]
    check_identity(check["stdout"])
    check_identity(check["stderr"])
    require(rooted(check["stdout"]["path"]).read_bytes() ==
            rooted("test/standalone-regexp-syncache-check.expected").read_bytes(),
            "standalone syntax-cache fixture differs from pinned output")
    require(rooted(check["stderr"]["path"]).read_bytes() == b"",
            "standalone syntax-cache fixture has unexpected stderr")
    require(data["suites"]["mcp"]["tools"] == 40, "MCP manifest must report 40 tools")
    tool_counts = []
    for line in text["mcp"].splitlines():
        try:
            response = json.loads(line)
        except (ValueError, TypeError):
            continue
        if isinstance(response, dict) and isinstance(response.get("result"), dict):
            tools = response["result"].get("tools")
            if isinstance(tools, list):
                tool_counts.append(len(tools))
    require(40 in tool_counts, "MCP raw tools/list response must contain 40 tools")
    print("PASS final: retained 11/0 verification, host regexp ERT, standalone checks and MCP 40")


def main():
    def terminated(_signum, _frame):
        raise InterruptedError("terminated by SIGTERM")
    signal.signal(signal.SIGTERM, terminated)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("semantics", "api", "record-speed", "speed", "final", "_profile-one"))
    parser.add_argument("--job", help=argparse.SUPPRESS)
    parser.add_argument("--binary", help="binary override; preserves negative-control logs separately")
    parser.add_argument("--label", default="final", choices=("base", "final"))
    parser.add_argument("--emacs", default=os.environ.get("EMACS") or shutil.which("emacs") or "emacs")
    parser.add_argument("--iterations", type=int, default=8)
    parser.add_argument("--low-iterations", type=int, default=0)
    parser.add_argument("--timeout", type=int, default=600)
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    try:
        if args.mode == "_profile-one":
            return profile_one(args)
        {"semantics": semantics, "api": api, "record-speed": run_speed_with_cold_identity,
         "speed": speed, "final": final}[args.mode](args)
    except (RuntimeError, OSError, ValueError, KeyError) as error:
        print(f"FAIL {args.mode}: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
