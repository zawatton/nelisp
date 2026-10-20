#!/usr/bin/env python3
"""Measure retained WORD objects in one invocation; replay only pinned inputs.

Run `measure BASE BEFORE CAND` once, then select live/objects/reproduce/parity
from the progress meter. Logs and input hashes stay in ABI_CONS_DIR (default
target/abi-cons). No rebuild is needed to diagnose a retained-size regression.
"""
import hashlib
import concurrent.futures
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / os.environ.get("ABI_CONS_DIR", "target/abi-cons")
TYPES = ("cons", "marker", "record1", "string8")
PROBES = ("test/perf/word-cons-live.el", "test/perf/word-object-size.el",
          "test/standalone-word-allocation-check.el")


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run(binary, probe, stem, **env):
    start = time.monotonic()
    r = subprocess.run([str(binary), "--load", str(ROOT / probe)], cwd=ROOT,
                       env=dict(os.environ, **env), capture_output=True, timeout=180)
    for suffix, data in (("out", r.stdout), ("err", r.stderr)):
        (OUT / f"{stem}.{suffix}").write_bytes(data)
    return {"rc": r.returncode, "seconds": time.monotonic() - start,
            "out": r.stdout.decode(), "err": r.stderr.decode()}


def measure(binaries):
    started = time.monotonic()
    OUT.mkdir(parents=True, exist_ok=True)
    record = {"inputs": {}, "runs": {}}
    paths = [ROOT / p for p in PROBES] + [Path(__file__)]
    for binary in binaries:
        paths.extend(Path(str(Path(binary).resolve()) + s)
                     for s in ("", ".cold", ".native-startup.el"))
    record["inputs"] = {str(p): sha(p) for p in paths}
    def population(who, binary):
        binary = Path(binary).resolve()
        inputs = [Path(str(binary) + s) for s in ("", ".cold", ".native-startup.el")]
        runs = {}
        for kind in TYPES:
            for n in (4000, 8000):
                key = f"{who}-{kind}-{n}"
                runs[key] = run(binary, PROBES[1], key, WH_TYPE=kind, WH_N=str(n))
        for kind, probe in (("live", PROBES[0]), ("parity", PROBES[2])):
            key = f"{who}-{kind}"
            runs[key] = run(binary, probe, key)
        return inputs, runs
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        jobs = [pool.submit(population, who, binary)
                for who, binary in zip(("base", "before", "cand"), binaries, strict=True)]
        for job in jobs:
            inputs, runs = job.result()
            record["runs"].update(runs)
    assert record["inputs"] == {str(p): sha(p) for p in paths}, "inputs changed during measurement"
    record["seconds"] = time.monotonic() - started
    (OUT / "objects.json").write_text(json.dumps(record, indent=2) + "\n")


def load():
    record = json.loads((OUT / "objects.json").read_text())
    assert all(sha(p) == h for p, h in record["inputs"].items()), "stale object inputs"
    return record["runs"]


def live(row):
    assert row["rc"] == 0, row
    return int(re.search(r"per-cons-live=(\d+)", row["out"])[1])


def obj(row, kind, n):
    assert row["rc"] == 0 and "WH-OBJECT-DONE" in row["out"], row
    return int(re.search(rf"WH-OBJECT {kind} {n} (\d+)", row["out"])[1])


def main():
    mode = sys.argv[1]
    if mode == "inspect":
        started = time.monotonic()
        query = '(prin1 (mapcar (lambda (n) (cl-position n nelisp-standalone--reader-neln-bridgeable-symbols :test (function equal))) (quote ("nl_root_pin_begin" "nl_root_pin_end"))))'
        indices = subprocess.check_output(
            [os.environ.get("EMACS", "emacs"), "-Q", "--batch",
             "-L", str(ROOT / "lisp"), "-L", str(ROOT / "src"), "-L", str(ROOT / "scripts"),
             "-l", "nelisp-standalone-build", "--eval", query], cwd=ROOT, text=True)
        match = re.fullmatch(r"\((\d+) (\d+)\)", indices)
        assert match, indices
        OUT.mkdir(parents=True, exist_ok=True)
        binary = Path(sys.argv[2]).resolve()
        probe = "test/perf/word-object-fields.el"
        row = run(binary, probe, "fields", ABI_PIN_BEGIN=match[1], ABI_PIN_END=match[2])
        row.update(seconds=time.monotonic() - started, binary_sha256=sha(binary), probe_sha256=sha(ROOT / probe))
        (OUT / "fields.json").write_text(json.dumps(row, indent=2) + "\n")
        assert row["rc"] == 0 and "WORD-FIELDS-DONE" in row["out"], row
        print(row["out"], end="")
        print(f"inspection elapsed {row['seconds']:.3f}s")
        return
    if mode == "measure":
        measure(sys.argv[2:])
        return
    runs = load()
    if mode in ("baseline", "reproduce", "live", "objects", "parity"):
        if mode == "baseline":
            assert live(runs["base-live"]) == 80
        elif mode == "reproduce":
            assert live(runs["before-live"]) > 80
            assert runs["before-parity"]["rc"] != 0
        elif mode == "live":
            assert live(runs["cand-live"]) <= 80
        elif mode == "objects":
            for kind in TYPES:
                for n in (4000, 8000):
                    b, c = (obj(runs[f"{w}-{kind}-{n}"], kind, n) for w in ("base", "cand"))
                    print(f"{kind} n={n}: base={b/n:.3f} candidate={c/n:.3f} bytes/object")
                    assert c <= b, (kind, n, b, c)
                    values = [re.search(r"^WH-VALUES .*$", runs[f"{w}-{kind}-{n}"]["out"], re.M)[0]
                              for w in ("base", "cand")]
                    golden = re.search(r"^WH-VALUES .*$", (OUT / f"gnu-{kind}.out").read_text(), re.M)[0]
                    assert values[0] == values[1] == golden, (values, golden)
        else:
            for who in ("base", "cand"):
                row = runs[f"{who}-parity"]
                assert row["rc"] == 0 and "WORD-ALLOCATION PASS" in row["out"]
            assert "WORD-ALLOCATION PASS" in (OUT / "gnu.out").read_text()
    elif mode == "census":
        census = Path(os.environ.get("ABI_CENSUS", OUT / "final-dynamic/census.tsv"))
        subprocess.run(["tools/ai/abi-merge-gate.sh", "census", str(census), "1363"], cwd=ROOT, check=True)
        subprocess.run(["tools/ai/abi-merge-gate.sh", "store-word", str(census), "0"], cwd=ROOT, check=True)
    else:
        raise ValueError(mode)
    print(f"ABI-CONS {mode} PASS")


if __name__ == "__main__":
    main()
