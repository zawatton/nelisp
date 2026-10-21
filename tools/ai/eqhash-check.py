#!/usr/bin/env python3
"""Record/replay the four eqhash ledger gates; config: EQHASH_CONFIG or target/eqhash/config.json.

record-wall --role base|candidate --variant dynamic|static --layout PAD [--suite flat|speed]
record-perf --role base|candidate|gnu --suite flat|speed [diagnostic only]
record-semantics --role base|candidate|gnu
record-final --common LOG --standalone LOG --mcp LOG --common-rc 0 --standalone-rc 0 --mcp-rc 0
flat|semantics|speed|final; baseline-control; report --role base|candidate|gnu
Each profile owns one heavy-run slot. Re-running resumes unchanged completed rows.
"""
import argparse
from functools import lru_cache
import hashlib
import itertools
import json
import os
from pathlib import Path
import re
import resource
import shutil
import subprocess
import time
import statistics

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / os.environ.get("EQHASH_DIR", "target/eqhash")
LAYOUTS, VARIANTS = (0, 700, 1400, 2100), ("dynamic", "static")
KINDS = ("string", "cons", "symbol", "vector", "float", "bignum", "marker")
PROBE, FIXTURE, GOLDEN = [ROOT / ("tools/ai/" + p) for p in
                         ("eqhash-perf.el", "eqhash-semantics.el", "eqhash-semantics.expected")]
WALL_PROBE = ROOT / "tools/ai/eqhash-wall.el"
WALL_PROTOCOL = "eqhash-wall-GC-median7-v2"
PROTOCOL = "eqhash-loop-plus-GC-Ir-v3"
GROUPS = ("standalone-checks", "standalone-shallow-binding-check", "standalone-shallow-binding-dyn-check",
          "standalone-fx-call-shape-check", "standalone-bytecode-word-check", "fx-spike", "fx-corpus",
          "thread-standalone", "thread-allocating", "thread-percpu", "thread-mirror-guard")

def require(ok, message):
    if not ok:
        raise ValueError(message)

def path(p):
    return ROOT / p

def name(p):
    p = Path(p).resolve()
    return str(p.relative_to(ROOT)) if p.is_relative_to(ROOT) else str(p)

@lru_cache(maxsize=256)
def digest(p, size, mtime, ctime):
    h = hashlib.sha256()
    with Path(p).open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            h.update(block)
    return h.hexdigest()

def identity(p):
    p = path(p).resolve()
    s = p.stat()
    return dict(path=name(p), sha256=digest(str(p), s.st_size, s.st_mtime_ns, s.st_ctime_ns))

def checked(item):
    require(identity(item["path"]) == item, "changed artifact: " + item["path"])
    return path(item["path"])

def read(p):
    return json.loads(path(p).read_text())

def write(p, row):
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(".tmp")
    tmp.write_text(json.dumps(row, indent=2) + "\n")
    tmp.replace(p)

@lru_cache(maxsize=1)
def config():
    return read(os.environ.get("EQHASH_CONFIG", "target/eqhash/config.json"))

def binary(role, variant="dynamic", layout=0):
    b = config()["binaries"][role]
    if role != "gnu":
        b = b[variant]
        if isinstance(b, dict):
            b = b[str(layout)]
    return path(b).resolve()

def inputs(role, variant, layout, probe):
    items = [identity(binary(role, variant, layout)), identity(probe)]
    if probe == PROBE:
        items.append(identity(OUT / ("instrument.so" if role == "gnu" else "instrument.bin")))
    return items

def memory_limit():
    resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024, 8388608 * 1024))

def execute(command, env, stem, timeout, heavy_unit=False):
    require(shutil.disk_usage(ROOT).free >= 50_000_000_000, "less than 50 GB free")
    heavy = os.environ.get("HEAVY_RUN") or config().get("heavy_run")
    if not heavy:
        heavy = next((p / "bin/heavy-run.sh" for p in ROOT.parents if (p / "bin/heavy-run.sh").is_file()), None)
    require(heavy and path(heavy).is_file(), "configure heavy_run or HEAVY_RUN")
    stem.parent.mkdir(parents=True, exist_ok=True)
    tmp = ROOT / "target/tmp"
    tmp.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    with stem.with_suffix(".out").open("wb") as out, stem.with_suffix(".err").open("wb") as err:
        # timeout starts inside heavy-run, after acquisition; queue time is unbounded.
        run = ["timeout", "--kill-after=5", str(timeout), *command]
        # Focused semantic probes and subsecond wall trials need no heavy slot.
        if command[0] == "valgrind" or heavy_unit:
            run = [str(path(heavy)), *run]
        rc = subprocess.run(run,
                            cwd=ROOT, env=dict(env, TMPDIR=str(tmp)), stdout=out, stderr=err,
                            preexec_fn=memory_limit).returncode
    return rc, time.monotonic() - started

def invocation(b, role, probe):
    return [str(b), *(["-Q", "--batch", "-l"] if role == "gnu" else ["--load"]), str(probe)]

def gnu_version():
    r = subprocess.run([str(binary("gnu")), "-Q", "--batch", "--eval", "(princ emacs-version)"],
                       capture_output=True, text=True, timeout=30, preexec_fn=memory_limit)
    require(r.returncode == 0 and r.stdout == "31.1" and not r.stderr, "GNU 31.1 required")

@lru_cache(maxsize=16)
def gc_address(b, binary_hash):
    r = subprocess.run(["nm", "--defined-only", str(b)], capture_output=True, text=True,
                       timeout=30, preexec_fn=memory_limit)
    matches = re.findall(r"^([0-9a-fA-F]+) \w nl_gc_stats$", r.stdout, re.M)
    require(r.returncode == 0 and len(matches) == 1, "nl_gc_stats telemetry symbol required")
    return str(int(matches[0], 16))

def stem_for(p):
    suffix = "-wall" if p["wall"] else ""
    return OUT / "perf" / p["role"] / p["variant"] / (f'{p["kind"]}-{p["test"]}-{p["weak"]}-n{p["n"]}-'
           f'{p["op"]}-pad{p["layout"]}-' + ("control" if p["control"] else "target") + suffix)

def dimensions(suite):
    if suite == "flat":
        return itertools.product(KINDS, ("eq", "eql"), ("nil", "key"), (10, 8000), ("miss",))
    return [("chartable" if op == "put" else "string", "eq", "key", n, op)
            for n, op in itertools.product((10, 8000), ("hit", "miss", "put"))]

def read_perf(p):
    row = read(p)
    q = row["parameters"]
    require(row["protocol"] == PROTOCOL and row["rc"] == 0 and q["calls"] > 0, "failed/empty profile: " + name(p))
    require(row["inputs"] == inputs(q["role"], q["variant"], q["layout"], PROBE), "changed binary/probe")
    out, err = [checked(row[k]).read_text() for k in ("stdout", "stderr")]
    require(out.count("EQHASH-PERF-COMPLETE\n") == 1 and
            not re.search(r"uncaught|Error:|Fatal|Invalid read|Invalid write|ERROR SUMMARY: [1-9]", err), "incomplete/error profile")
    if q["wall"]:
        metric = re.findall(r"^EQHASH-WALL ([0-9.]+) ([0-9]+)$", out, re.M)
        require(len(metric) == 1 and int(metric[0][1]) == q["calls"], "invalid wall metric")
        row["value"] = float(metric[0][0]) * 1e6
    else:
        require(out == "EQHASH-PERF-COMPLETE\n", "unexpected profile stdout")
        with checked(row["callgrind"]).open() as stream:
            header = "".join(itertools.islice(stream, 80))
        metric = re.findall(r"^summary: ([0-9]+)$", header, re.M)
        require("events: Ir\n" in header and len(metric) == 1 and int(metric[0]) > 0, "missing complete-process Ir")
        row["value"] = int(metric[0])
    return row

def record_perf(a):
    require(a.calls > 0 and a.timeout > 0 and (a.max_runs is None or a.max_runs > 0), "positive calls/timeout/max-runs required")
    if a.role == "gnu":
        gnu_version()
    count = 0
    for variant, dim, layout, control in itertools.product(("dynamic",) if a.role == "gnu" else VARIANTS,
                                                          dimensions(a.suite), LAYOUTS, (False, True)):
        kind, test, weak, n, op = dim
        params = dict(role=a.role, variant=variant, kind=kind, test=test, weak=weak, n=n,
                      op=op, layout=layout, control=control, calls=a.calls, wall=a.wall)
        if any(getattr(a, k) is not None and getattr(a, k) != params[k]
               for k in ("variant", "kind", "test", "weak", "n", "layout", "op")):
            continue
        stem = stem_for(params)
        if stem.with_suffix(".json").exists() and not a.replace:
            require(read_perf(stem.with_suffix(".json"))["parameters"] == params, "different parameters; use --replace")
            continue
        b = binary(a.role, variant, layout)
        before = inputs(a.role, variant, layout, PROBE)
        env = dict(os.environ, ABI_LAYOUT_PAD="x" * layout, EQHASH_KIND=kind, EQHASH_TEST=test,
                   EQHASH_WEAK=weak, EQHASH_N=str(n), EQHASH_OP=op, EQHASH_CONTROL=str(int(control)),
                   EQHASH_CALLS=str(a.calls), EQHASH_WALL=str(int(a.wall)))
        env["EQHASH_INSTRUMENT"] = str(OUT / ("instrument.so" if a.role == "gnu" else "instrument.bin"))
        env.pop("GC_STATS_ADDR", None)
        if a.role != "gnu":
            env["GC_STATS_ADDR"] = gc_address(b, before[0]["sha256"])
        command = invocation(b, a.role, PROBE)
        if not a.wall:
            command = ["valgrind", "--tool=callgrind", "--vgdb=no", "--instr-atstart=no", "--error-exitcode=97", "--dump-instr=no",
                       "--collect-jumps=no", "--trace-children=no", "--callgrind-out-file=" + str(stem.with_suffix(".cg")), *command]
        rc, seconds = execute(command, env, stem, a.timeout)
        require(before == inputs(a.role, variant, layout, PROBE), "binary/probe changed during profile")
        row = dict(protocol=PROTOCOL, parameters=params, inputs=before, rc=rc, seconds=seconds,
                   stdout=identity(stem.with_suffix(".out")), stderr=identity(stem.with_suffix(".err")))
        if not a.wall and stem.with_suffix(".cg").exists():
            row["callgrind"] = identity(stem.with_suffix(".cg"))
        write(stem.with_suffix(".json"), row)
        read_perf(stem.with_suffix(".json"))
        count += 1
        print(f"recorded {name(stem)} {seconds:.1f}s", flush=True)
        if a.max_runs and count >= a.max_runs:
            break
        if not a.wall:
            time.sleep(3)  # Let other queued units acquire a released heavy slot.
    print(f"EQHASH record-perf checked={count}")

def cost(role, variant, kind, test, weak, n, op, wall=False):
    values, counts = [], set()
    for layout in LAYOUTS:
        params = dict(role=role, variant=variant, kind=kind, test=test, weak=weak, n=n, op=op, layout=layout, wall=wall)
        rows = [read_perf(stem_for(dict(params, control=c)).with_suffix(".json")) for c in (False, True)]
        for row, control in zip(rows, (False, True)):
            require(all(row["parameters"][k] == v for k, v in dict(params, control=control).items()), "wrong profile dimensions")
            counts.add(row["parameters"]["calls"])
        delta = rows[0]["value"] - rows[1]["value"]
        require(delta > 0, "nonpositive target-control own cost")
        values.append(delta / rows[0]["parameters"]["calls"])
    require(len(counts) == 1, "different call counts across layouts/controls")
    return sum(values) / len(values), counts.pop()

def baseline_control():
    low, high = [cost("base", "dynamic", "string", "eq", "key", n, "miss") for n in (10, 8000)]
    require(low[1] == high[1], "base call counts differ")
    print(f"EQHASH-BASE dynamic N10={low[0]:.3f} N8000={high[0]:.3f} ratio={high[0]/low[0]:.4f}")
    return 1, ["base string slope exceeds 1.5"] if high[0] > 1.5 * low[0] else []

def flat(role, metric="wall"):
    return wall_flat(role)

def speed(role, metric="wall"):
    return wall_speed(role)

def record_semantics(a):
    if a.role == "gnu":
        gnu_version()
    for variant in ("dynamic",) if a.role == "gnu" else VARIANTS:
        if a.variant and a.variant != variant:
            continue
        stem = OUT / "semantics" / f"{a.role}-{variant}"
        before = inputs(a.role, variant, 0, FIXTURE)
        rc, seconds = execute(invocation(binary(a.role, variant), a.role, FIXTURE), os.environ, stem, a.timeout)
        require(before == inputs(a.role, variant, 0, FIXTURE), "semantics inputs changed")
        write(stem.with_suffix(".json"), dict(inputs=before, expected=identity(GOLDEN), rc=rc, seconds=seconds,
              stdout=identity(stem.with_suffix(".out")), stderr=identity(stem.with_suffix(".err"))))
        print(f"recorded semantics {a.role}/{variant}")

def semantics(role):
    expected = GOLDEN.read_bytes()
    count = len(re.findall(rb"^\(eqhash .+\)$", expected, re.M))
    require(count > 0 and expected.endswith(f"(eqhash-complete {count})\n".encode()), "incomplete pinned fixture")
    failures = []
    for r, v in [("gnu", "dynamic"), *[(role, v) for v in VARIANTS]]:
        row = read(OUT / "semantics" / f"{r}-{v}.json")
        require(row["inputs"] == inputs(r, v, 0, FIXTURE) and row["expected"] == identity(GOLDEN), "stale semantic inputs")
        ok = row["rc"] == 0 and not checked(row["stderr"]).read_bytes() and checked(row["stdout"]).read_bytes() == expected
        if not ok:
            failures.append(f"{r}/{v}: semantic output/exit mismatch")
    return count * 2, failures

def final():
    row = read(OUT / "final.json")
    require(row["binaries"] == [identity(binary("candidate", v)) for v in VARIANTS], "changed final binaries")
    logs = {}
    for run, item in row["logs"].items():
        require(item["rc"] == 0, "failed final run: " + run)
        logs[run] = checked(item["file"]).read_text()
    groups = re.findall(r"^([\w-]+) rc=(-?[0-9]+) [0-9]+s$", logs["common"], re.M)
    require(len(groups) == 11 and {n for n, _ in groups} == set(GROUPS) and all(rc == "0" for _, rc in groups), "COMMON needs all 11 successful groups")
    require(re.findall(r"^GATE-COUNT: (\d+) cases, (\d+) failures$", logs["common"], re.M) == [("11", "0")], "invalid COMMON summary")
    counts = re.findall(r"^GATE-COUNT: (\d+) cases, (\d+) failures$", logs["standalone"], re.M)
    require(len(counts) == 1 and int(counts[0][0]) > 0 and counts[0][1] == "0" and
            "STANDALONE-CHECKS PASS\n" in logs["standalone"] and "STANDALONE-CHECK FAIL " not in logs["standalone"], "standalone must finish positive cases with zero failures")
    replies = []
    for line in logs["mcp"].splitlines():
        try:
            obj = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict) and obj.get("jsonrpc") == "2.0" and obj.get("id") is not None and "error" not in obj:
            result = obj.get("result")
            if isinstance(result, dict) and isinstance(result.get("tools"), list):
                replies.append(result["tools"])
    require(len(replies) == 1 and len(replies[0]) == 40, "one raw MCP tools/list response with 40 tools required")
    names = [t.get("name") if isinstance(t, dict) else None for t in replies[0]]
    require(all(isinstance(n, str) and n for n in names) and len(set(names)) == 40, "MCP tools must have 40 unique names")
    return 11 + int(counts[0][0]) + 40, []

def wall_dimensions(role, suite="flat"):
    if suite == "speed":
        return list(dimensions("speed"))
    if role == "base":
        return [(k, "eq", "key", n, "miss") for k in KINDS for n in (10, 8000)]
    return list(dimensions("flat"))

def wall_stem(role, variant, layout, sample, runtime, suite="flat"):
    return OUT / "wall" / role / variant / suite / f"pad{layout}-run{sample}-{runtime}"

def wall_inputs(role, variant, layout):
    return [identity(binary(role, variant, layout)), identity(WALL_PROBE)]

def wall_read(stem):
    row = read(stem.with_suffix(".json"))
    p = row["parameters"]
    require(row["protocol"] == WALL_PROTOCOL and row["rc"] == 0, "incomplete wall run")
    require(row["inputs"] == wall_inputs(p["runtime"], p["variant"], p["layout"]), "stale wall inputs")
    out, err = [checked(row[k]).read_text() for k in ("stdout", "stderr")]
    require(out.count("EQHASH-WALL-COMPLETE\n") == 1 and not err, "wall probe failed")
    values = {}
    for fields in re.findall(r"^EQHASH-WALL (\w+) (\w+) (\w+) (\d+) (\w+) ([01]) ([0-9.]+) (\d+)$", out, re.M):
        kind, test, weak, n, op, control, seconds, calls = fields
        key = (kind, test, weak, int(n), op, bool(int(control)))
        require(key not in values and int(calls) == p["calls"] and float(seconds) > 0, "invalid wall row")
        values[key] = float(seconds) * 1e6 / int(calls)
    expected = {(*tuple(d), c) for d in p["dimensions"] for c in ((False, True) if p["suite"] == "speed" else (False,))}
    require(set(values) == expected, "incomplete wall dimensions")
    row["values"] = values
    return row

def record_wall(a):
    require(a.calls > 0 and 24 <= a.cpu <= 31, "positive calls and idle CPU 24-31 required")
    gnu_version()
    dims = [d for d in wall_dimensions(a.role, a.suite) if (not a.kind or d[0] == a.kind) and (not a.op or d[4] == a.op)]
    if a.probe:
        dims = [d for d in dims if d[0] == ("chartable" if a.op == "put" else "string") and d[1:3] == ("eq", "key")]
    require(dims, "empty wall workload")
    cases = "(" + " ".join("(" + " ".join(map(str, d)) + ")" for d in dims) + ")"
    for sample in range(1 if a.probe else 7):
        for runtime in (a.role, "gnu"):
            stem = wall_stem(a.role, a.variant, a.layout, sample, runtime, a.suite)
            if a.probe:
                stem = stem.parent / (stem.name + "-probe")
            params = dict(runtime=runtime, role=a.role, variant=a.variant, layout=a.layout,
                          sample=sample, calls=a.calls, cpu=a.cpu, dimensions=dims, probe=a.probe, suite=a.suite)
            if stem.with_suffix(".json").exists() and not a.probe:
                old = wall_read(stem)
                require(old["parameters"] == json.loads(json.dumps(params)), "wall parameters changed")
                print(f"reused {name(stem)}", flush=True)
                continue
            before = wall_inputs(runtime, a.variant, a.layout)
            env = dict(os.environ, ABI_LAYOUT_PAD="x" * a.layout, EQHASH_CASES=cases, EQHASH_CALLS=str(a.calls), EQHASH_OWN=str(int(a.suite == "speed")))
            command = ["taskset", "-c", str(a.cpu), *invocation(binary(runtime, a.variant, a.layout), runtime, WALL_PROBE)]
            rc, seconds = execute(command, env, stem, a.timeout, heavy_unit=not a.probe and runtime != "gnu")
            require(before == wall_inputs(runtime, a.variant, a.layout), "wall inputs changed during run")
            write(stem.with_suffix(".json"), dict(protocol=WALL_PROTOCOL, parameters=params, inputs=before,
                  rc=rc, seconds=seconds, stdout=identity(stem.with_suffix(".out")), stderr=identity(stem.with_suffix(".err"))))
            row = wall_read(stem)
            print(f"recorded {name(stem)} {seconds:.1f}s rows={len(row['values'])}", flush=True)
    print("EQHASH-WALL-RECORDED", flush=True)

@lru_cache(maxsize=256)
def wall_batch(role, variant, layout, sample, runtime, suite):
    return wall_read(wall_stem(role, variant, layout, sample, runtime, suite))

def wall_cost(role, variant, dim, own=False, reference=False):
    values = []
    for layout in LAYOUTS:
        samples = []
        for sample in range(7):
            row = wall_batch(role, variant, layout, sample, "gnu" if reference else role, "speed" if own else "flat")
            p = row["parameters"]
            require(not p["probe"] and p["sample"] == sample and p["role"] == role and
                    p["variant"] == variant and p["layout"] == layout, "wrong wall batch")
            target = row["values"][(*dim, False)]
            samples.append(target - row["values"][(*dim, True)] if own else target)
        values.append(statistics.median(samples))
    return statistics.mean(values)

def wall_flat(role):
    # The before/after ledger requires all baseline key types, plus GNU.
    for kind, n, reference in itertools.product(KINDS, (10, 8000), (False, True)):
        wall_cost("base", "dynamic", (kind, "eq", "key", n, "miss"), reference=reference)
    base_low, base_high = [wall_cost("base", "dynamic", ("string", "eq", "key", n, "miss")) for n in (10, 8000)]
    require(base_high > 1.5 * base_low, "base string negative control must fail flatness")
    print(f"EQHASH-BASE wall N10={base_low:.6f} N8000={base_high:.6f} us/call ratio={base_high/base_low:.4f}")
    failures = []
    for variant, kind, test, weak in itertools.product(VARIANTS, KINDS, ("eq", "eql"), ("nil", "key")):
        low, high = [wall_cost(role, variant, (kind, test, weak, n, "miss")) for n in (10, 8000)]
        for n in (10, 8000):
            wall_cost(role, variant, (kind, test, weak, n, "miss"), reference=True)
        if high > 1.5 * low:
            failures.append(f"{variant}/{kind}/{test}/{weak}: wall ratio={high/low:.4f}")
    return 56, failures

def wall_speed(role):
    # One actual base witness. GNU total bounds its own cost from above,
    # avoiding a noisy tiny GNU subtraction in this short negative control.
    dim = ("string", "eq", "key", 8000, "miss")
    base_own, gnu_total = [], []
    for sample in range(7):
        b = wall_batch("base", "dynamic", 0, sample, "base", "speed")["values"]
        g = wall_batch("base", "dynamic", 0, sample, "gnu", "speed")["values"]
        base_own.append(b[(*dim, False)] - b[(*dim, True)])
        gnu_total.append(g[(*dim, False)])
    require(statistics.median(base_own) > statistics.median(gnu_total), "base own-cost witness must fail GNU bar")
    failures = []
    for variant, dim in itertools.product(VARIANTS, dimensions("speed")):
        own = wall_cost(role, variant, dim, own=True)
        gnu = wall_cost(role, variant, dim, own=True, reference=True)
        print(f"EQHASH-OWN {variant}/{dim} candidate={own:.6f} GNU={gnu:.6f} us/call")
        if own <= 0 or gnu <= 0 or own > gnu:
            failures.append(f"{variant}/{dim}: own wall {own:.6f} > GNU {gnu:.6f} (positive costs required)")
    return 12, failures

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    for gate in ("flat", "semantics", "speed", "report"):
        p = sub.add_parser(gate)
        if gate in ("flat", "speed"):
            p.add_argument("--metric", choices=("wall",), default="wall")
        p.add_argument("--role", choices=("base", "candidate", "gnu"), default="candidate")
        if gate == "report":
            p.add_argument("--variant", choices=VARIANTS, default="dynamic")
    for gate in ("final", "baseline-control"):
        sub.add_parser(gate)
    p = sub.add_parser("record-wall")
    p.add_argument("--suite", choices=("flat", "speed"), default="flat")
    p.add_argument("--role", choices=("base", "candidate"), default="candidate")
    p.add_argument("--variant", choices=VARIANTS, default="dynamic")
    p.add_argument("--layout", choices=LAYOUTS, type=int, default=0)
    p.add_argument("--calls", type=int, default=10000)
    p.add_argument("--cpu", type=int, default=24)
    p.add_argument("--timeout", type=int, default=600)
    p.add_argument("--kind", choices=(*KINDS, "chartable"))
    p.add_argument("--operation", dest="op", choices=("hit", "miss", "put"))
    p.add_argument("--probe", action="store_true", help="one fast pair; not gate evidence")
    p = sub.add_parser("record-perf")
    p.add_argument("--role", choices=("base", "candidate", "gnu"), required=True)
    p.add_argument("--suite", choices=("flat", "speed"), required=True)
    for key, choices, kind in (("variant", VARIANTS, str), ("layout", LAYOUTS, int), ("kind", (*KINDS, "chartable"), str),
                               ("test", ("eq", "eql"), str), ("weak", ("nil", "key"), str), ("n", (10, 8000), int)):
        p.add_argument("--" + key, choices=choices, type=kind)
    p.add_argument("--operation", dest="op", choices=("hit", "miss", "put"))
    p.add_argument("--calls", type=int, default=2000)
    p.add_argument("--max-runs", type=int)
    p.add_argument("--timeout", type=int, default=900)
    p.add_argument("--replace", action="store_true")
    p.add_argument("--wall", action="store_true")
    p = sub.add_parser("record-semantics")
    p.add_argument("--role", choices=("base", "candidate", "gnu"), required=True)
    p.add_argument("--variant", choices=VARIANTS)
    p.add_argument("--timeout", type=int, default=300)
    p = sub.add_parser("record-final")
    for run in ("common", "standalone", "mcp"):
        p.add_argument("--" + run, required=True)
        p.add_argument("--" + run + "-rc", type=int, required=True)
    a = parser.parse_args()
    if a.command == "record-wall":
        record_wall(a)
    elif a.command == "record-perf":
        record_perf(a)
    elif a.command == "record-semantics":
        record_semantics(a)
    elif a.command == "record-final":
        write(OUT / "final.json", dict(binaries=[identity(binary("candidate", v)) for v in VARIANTS],
              logs={r: dict(file=identity(getattr(a, r)), rc=getattr(a, r + "_rc")) for r in ("common", "standalone", "mcp")}))
        print("EQHASH record-final", final()[0], "cases")
    elif a.command == "report":
        for n in (10, 8000):
            dim = ("string", "eq", "key", n, "miss")
            total = wall_cost(a.role, a.variant, dim)
            gnu = wall_cost(a.role, a.variant, dim, reference=True)
            print(f"EQHASH-REPORT {a.role}/{a.variant} N{n} total={total:.6f} GNU={gnu:.6f} us/call (GC included)")
    else:
        cases, failures = (baseline_control() if a.command == "baseline-control" else final() if a.command == "final"
                           else semantics(a.role) if a.command == "semantics" else {"flat": flat, "speed": speed}[a.command](a.role, a.metric))
        for failure in failures:
            print(failure)
        print(f"GATE-COUNT: {cases} cases, {len(failures)} failures")
        return bool(failures)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, KeyError, subprocess.SubprocessError) as error:
        print("EQHASH ERROR:", error)
        raise SystemExit(1)
