#!/usr/bin/env python3
"""Measure fixed sort workloads and verify the lane's recorded evidence.

All instruction counts include GC. Four identical environment layouts are
measured with low/high iteration subtraction, retaining the complete profiles.
Use ``retime TAG --cases strings,integers`` to repeat informative wall timings
without rerunning or altering the authenticated instruction measurements.
"""
import argparse
import concurrent.futures
import difflib
import hashlib
import json
import os
from pathlib import Path
import re
import runpy
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-sort"
BENCH = ROOT / "test/perf/prim-sort-bench.el"
LAYOUTS = (0, 700, 1400, 2100)
CAPS = {"strings": 200, "integers": 120}


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def assets(binary):
    paths = [Path(binary)]
    if not paths[0].name.startswith("emacs"):
        paths += [Path(str(binary) + suffix) for suffix in (".cold", ".native-startup.el")]
    return {str(path): sha(path) for path in paths}


def run(command, name, env=None, marker=b"PS-COMPLETE\n", timeout=1800):
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                            timeout=timeout)
    (OUT / f"{name}.out").write_bytes(result.stdout)
    (OUT / f"{name}.err").write_bytes(result.stderr)
    if result.returncode or marker not in result.stdout:
        raise RuntimeError(f"{name}: exit {result.returncode}, see raw artifacts")
    if result.stderr and command[0] != "valgrind":
        raise RuntimeError(f"{name}: unexpected stderr")
    return result.stdout.decode()


def measure(binary, tag, cases):
    binary = Path(binary).resolve()
    command = [str(binary)]
    if binary.name.startswith("emacs"):
        command += ["-Q", "--batch"]
    command += ["--load", str(BENCH)]
    record = {"binary": str(binary), "binary_sha256": sha(binary),
              "assets": assets(binary),
              "benchmark_sha256": sha(BENCH), "cases": {}}
    for case in cases:
        env = dict(os.environ, PS_CASE=case, TMPDIR=str(ROOT / "target/tmp"))
        trials = []
        for trial in range(3):
            count = "1000" if binary.name.startswith("emacs") else "3"
            output = run(command, f"{tag}-{case}-time-{trial}", dict(env, PS_N=count))
            trials.append(float(re.search(r"PS-TIME \S+ ([\d.]+)", output)[1]))

        def profile(pad):
            totals = []
            for count in (1, 3):
                name = f"{tag}-{case}-{pad}-{count}"
                path = OUT / f"{name}.callgrind"
                output = run(["valgrind", "--tool=callgrind",
                              f"--callgrind-out-file={path}"] + command, name,
                             dict(env, PS_N=str(count), ABI_LAYOUT_PAD="x" * pad))
                matches = re.findall(r"^summary: (\d+)$", path.read_text(), re.M)
                if len(matches) != 1:
                    raise RuntimeError(f"{name}: missing instruction total")
                totals.append(int(matches[0]))
            ir = (totals[1] - totals[0]) / 2
            if ir <= 0:
                raise RuntimeError(f"{tag} {case}: invalid subtraction {totals}")
            return {"pad": pad, "totals": totals, "Ir": ir}

        # Independent layouts only: never change sources while these run.
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            profiles = list(pool.map(profile, LAYOUTS))
        record["cases"][case] = {"us": min(trials), "layouts": profiles,
                                 "Ir": sum(x["Ir"] for x in profiles) / 4}
        print(tag, case, record["cases"][case]["us"],
              record["cases"][case]["Ir"], flush=True)
        (OUT / f"{tag}.json").write_text(json.dumps(record, indent=2) + "\n")


def read(tag):
    record = json.loads((OUT / f"{tag}.json").read_text())
    if sha(record["binary"]) != record["binary_sha256"] or sha(BENCH) != record["benchmark_sha256"]:
        raise RuntimeError(f"{tag}: evidence identity changed")
    require(record["assets"] == assets(record["binary"]), f"{tag}: runtime assets changed")
    for case in record["cases"].values():
        if [x["pad"] for x in case["layouts"]] != list(LAYOUTS):
            raise RuntimeError(f"{tag}: incomplete layouts")
        if any(x["Ir"] <= 0 or x["totals"][1] <= x["totals"][0] for x in case["layouts"]):
            raise RuntimeError(f"{tag}: nonpositive instruction delta")
        if case["Ir"] != sum(x["Ir"] for x in case["layouts"]) / 4:
            raise RuntimeError(f"{tag}: instruction mean disagrees with layouts")
    return record


def measure_load(binary, tag):
    binary = Path(binary).resolve()
    profile_counts = runpy.run_path(str(ROOT / "tools/ai/abi-ir.py"))["profile_counts"]

    def profile(pad):
        name = f"{tag}-load-{pad}"
        cache = OUT / f"{name}-cache"
        cache.mkdir(exist_ok=False)
        path = OUT / f"{name}.callgrind"
        output = run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                      f"--callgrind-out-file={path}", str(binary), "--load",
                      "test/perf/prim-speed-load-five.el"], name,
                     dict(os.environ, ABI_LAYOUT_PAD="x" * pad,
                          XDG_CACHE_HOME=str(cache), TMPDIR=str(ROOT / "target/tmp")),
                     marker=b"FIVE-LIBRARIES PASS: 5 loaded\n")
        ir, gc = profile_counts(path, binary)
        return {"pad": pad, "Ir": ir, "gc": gc, "output": output}

    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        profiles = list(pool.map(profile, LAYOUTS))
    record = {"binary": str(binary), "binary_sha256": sha(binary), "layouts": profiles,
              "assets": assets(binary),
              "benchmark_sha256": sha(ROOT / "test/perf/prim-speed-load-five.el"),
              "Ir": sum(x["Ir"] for x in profiles) / 4}
    (OUT / f"{tag}-load.json").write_text(json.dumps(record, indent=2) + "\n")
    print(tag, "load", record["Ir"], flush=True)


def retime(tag, cases):
    record = read(tag)
    binary = Path(record["binary"])
    command = [str(binary)]
    gnu = binary.name.startswith("emacs")
    if gnu:
        command += ["-Q", "--batch"]
    command += ["--load", str(BENCH)]
    start = time.monotonic()
    for case in cases or list(record["cases"]):
        count = (10000 if case == "strings" else 1000) if gnu else {"strings": 100, "integers": 10}.get(case, 1000)
        samples = []
        for trial in range(3):
            output = run(command, f"{tag}-{case}-retime-{trial}",
                         dict(os.environ, PS_CASE=case, PS_N=str(count),
                              TMPDIR=str(ROOT / "target/tmp")))
            samples.append(float(re.search(r"PS-TIME \S+ ([\d.]+)", output)[1]))
        row = record["cases"][case]
        row["previous_us"] = row["us"]
        row["us"] = min(samples)
        row["timing_trials"] = {"count": count, "samples_us": samples}
        print(tag, case, row["us"], "us", flush=True)
    record["retime_elapsed_s"] = time.monotonic() - start
    (OUT / f"{tag}.json").write_text(json.dumps(record, indent=2) + "\n")


def check(action, binary=None):
    if action == "profile":
        record = read("base")
        for name in ("strings", "integers", "machinery", "predicate", "comparison", "floor"):
            require(record["cases"][name]["Ir"] > 0, f"profile missing {name}")
        profile = json.loads((OUT / "profile.json").read_text())
        require(profile["baseline_commit"] == "692dc8a09", "wrong baseline commit")
        require(profile["baseline_record_sha256"] == sha(OUT / "base.json"), "profile identity changed")
        for name in ("machinery_Ir_estimate", "predicate_dispatch_Ir_estimate", "comparison_Ir_estimate"):
            require(profile[name] > 0, f"invalid profile estimate {name}")
        print("baseline profile: 6 workloads, 4 layouts each, 121 string comparisons")
    elif action == "parity":
        binary = str(Path(binary).resolve()) if binary else read("after")["binary"]
        source = "test/standalone-prim-sort-check.el"
        reference = run([os.environ.get("GNU_EMACS", "emacs"), "-Q", "--batch", "--load", source],
                        "gnu-parity", marker=b"PRIM-SORT COMPLETE\n")
        output = run([binary, "--load", source], "parity-" + sha(binary)[:12],
                     marker=b"PRIM-SORT COMPLETE\n")
        count = len(re.findall(r"^PS-CASE \d+ ", reference, re.M))
        if count != 76:
            raise RuntimeError(f"Incomplete parity corpus: {count} cases")
        mixed = len(re.findall(r"^PS-MIXED \d+ ", reference, re.M))
        if mixed != 512:
            raise RuntimeError(f"Incomplete mixed-type corpus: {mixed} cases")
        if output != reference:
            difference = list(difflib.unified_diff(reference.splitlines(), output.splitlines()))
            raise RuntimeError("GNU parity differs:\n" + "\n".join(difference[:14]))
        print(f"GNU parity: {count} cases + {mixed} mixed triples")
    elif action == "cost":
        candidate, gnu = read("after"), read("gnu")
        require(candidate["binary_sha256"] != gnu["binary_sha256"], "self-comparison")
        for name, cap in CAPS.items():
            ratio = candidate["cases"][name]["Ir"] / gnu["cases"][name]["Ir"]
            print(name, f"{ratio:.3f}x <= {cap}x")
            require(ratio <= cap, f"{name}: {ratio:.3f}x exceeds {cap}x")
    elif action == "final":
        report = json.loads((OUT / "final.json").read_text())
        require(report["passed"], "final verification did not pass")
        for path, digest in report["evidence"].items():
            require(sha(ROOT / path) == digest, f"final evidence changed: {path}")
        for path, digest in report["static_assets"].items():
            require(sha(ROOT / path) == digest, f"static asset changed: {path}")
        require(report["binary_sha256"] == read("after")["binary_sha256"], "final binary changed")
        checks = (ROOT / report["checks"]).read_text()
        require("GATE-COUNT: 11 cases, 0 failures" in checks, "missing 11-group final success")
        require("STANDALONE-CHECKS PASS" in checks, "standalone checks did not complete")
        anvil = json.loads((OUT / "anvil/result.json").read_text())
        require(anvil["tools"] == 40 and anvil["context_call"] == "PASS", "Anvil smoke failed")
        require(anvil["binary_sha256"] == report["binary_sha256"], "Anvil used another binary")
        for tag in ("base", "after"):
            load = json.loads((OUT / f"{tag}-load.json").read_text())
            require(load["assets"] == assets(load["binary"]), f"{tag} load assets changed")
            require(load["benchmark_sha256"] == sha(ROOT / "test/perf/prim-speed-load-five.el"),
                    f"{tag} load benchmark changed")
            require([x["pad"] for x in load["layouts"]] == list(LAYOUTS), "load layouts incomplete")
            require(all(x["Ir"] > 0 for x in load["layouts"]), "load instruction count missing")
            require(load["Ir"] == sum(x["Ir"] for x in load["layouts"]) / 4, "load mean changed")
            require(all(x["output"] == "FIVE-LIBRARIES PASS: 5 loaded\n" for x in load["layouts"]),
                    "load output differs")
        base_load = json.loads((OUT / "base-load.json").read_text())
        after_load = json.loads((OUT / "after-load.json").read_text())
        require(after_load["Ir"] <= base_load["Ir"], "five-library load regressed")
        census = runpy.run_path(str(ROOT / "tools/ai/box-census.py"))
        totals = {}
        for tag, path in (("base", OUT / "base/census.tsv"),
                          ("after", ROOT / report["census"])):
            units = census["load"](path)
            require(len(units) == 114, f"{tag}: incomplete census units")
            version = census["CONVERSION_WALK_VERSION"]
            require(all(any(idiom == version for _, idiom, _ in rows) for rows in units.values()),
                    f"{tag}: missing conversion walker certificates")
            summary, _ = census["tally"](units)
            conversions = sum(summary["idioms"].get(x, 0) for x in
                              (*census["CONVERSION_CALLS"], "inline-box-to-word"))
            require(conversions == 0, f"{tag}: S6.2 conversions={conversions}")
            totals[tag] = sum(summary["idioms"].values())
        require(totals["after"] <= totals["base"], f"box census regressed: {totals}")
        print(f"box census {totals}; S6.2=0")
    print(action, "PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("measure", "measure-load", "retime", "profile", "parity", "cost", "final"))
    parser.add_argument("binary", nargs="?")
    parser.add_argument("tag", nargs="?")
    parser.add_argument("--cases")
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    if args.action == "retime":
        if not args.binary:
            parser.error("retime needs TAG")
        retime(args.binary, args.cases.split(",") if args.cases else None)
    elif args.action in ("measure", "measure-load"):
        if not args.binary or not args.tag:
            parser.error("measure needs binary and tag")
        if args.action == "measure-load":
            measure_load(args.binary, args.tag)
        else:
            measure(args.binary, args.tag, (args.cases or "strings,integers,machinery,predicate,comparison,floor").split(","))
    else:
        check(args.action, args.binary)


if __name__ == "__main__":
    try:
        main()
    except (AssertionError, OSError, ValueError, RuntimeError, subprocess.TimeoutExpired) as exc:
        print(f"FAIL: {exc}", file=sys.stderr)
        sys.exit(1)
