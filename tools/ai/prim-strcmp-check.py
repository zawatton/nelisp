#!/usr/bin/env python3
"""Measure string comparisons and check authenticated lane evidence.

measure BIN TAG runs four layouts, preserving complete Callgrind profiles.
parity-run BIN TAG records results alongside the GNU oracle.
Ledger checks consume recorded evidence without rerunning expensive builds.
"""
import argparse
import bisect
import concurrent.futures
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-strcmp"
BENCH = ROOT / "test/perf/prim-strcmp-bench.el"
PARITY = ROOT / "test/perf/prim-strcmp-parity.el"
LAYOUTS = (0, 700, 1400, 2100)
NAMES = ("string-lessp", "string<", "string>", "string-greaterp",
         "compare-strings", "string=", "string-equal", "string-prefix-p")
IR_CAP = 40  # Measurement-based provisional revision; see the S3.2 ledger NOTE.
SOURCE_PATHS = tuple(ROOT / name for name in (
    "lisp/nelisp-cc-string-compare.el", "lisp/nelisp-stdlib-plist-str.el",
    "scripts/nelisp-standalone-build.el", "scripts/nelisp-stdlib-prelude.el",
    "scripts/nelisp-string-compare-compiler.el", "test/standalone-string-compare-check.el",
    "test/perf/prim-strcmp-bench.el", "test/perf/prim-strcmp-parity.el",
    "test/perf/prim-strcmp-library-parity.el", "test/nelisp-shadow-differential-cases.el"))
CANDIDATE_RECORDS = {"after", "after-parity", "after-no-fx-parity", "after-load-five",
                     "semantic-controls", "final-builds", "final-smokes", "final-census"}


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(name, record):
    (OUT / (name + ".json")).write_text(json.dumps(record, indent=2) + "\n")


def authenticated(name):
    r = json.loads((OUT / (name + ".json")).read_text())
    if name in CANDIDATE_RECORDS and r.get("sources") != source_snapshot():
        raise ValueError(f"{name}: candidate runtime or test source changed or was not pinned")
    for path, digest in r["files"].items():
        if sha(ROOT / path) != digest:
            raise ValueError(f"changed evidence: {path}")
    if name in CANDIDATE_RECORDS:
        binary = Path(r["binary"])
        key = str(binary.relative_to(ROOT)) if binary.is_relative_to(ROOT) else str(binary)
        if r["files"].get(key) != r["binary_sha256"] or sha(binary) != r["binary_sha256"]:
            raise ValueError(f"{name}: primary binary was not authenticated")
        if name != "after":
            after = authenticated("after")
            if any(r[k] != after[k] for k in ("binary", "binary_sha256")):
                raise ValueError(f"{name}: primary binary differs from measured candidate")
    return r


def source_snapshot():
    return files_record(SOURCE_PATHS)


def require_unchanged(sources):
    if sources and sources != source_snapshot():
        raise ValueError("candidate sources changed during verification")


def run(command, stem, env=None, marker=None, timeout=580):
    with Path(str(stem) + ".out").open("wb") as out, Path(str(stem) + ".err").open("wb") as err:
        rc = subprocess.run(command, cwd=ROOT, env=env, stdout=out, stderr=err,
                            timeout=timeout).returncode
    if rc:
        raise ValueError(f"exit {rc}: {stem}.err")
    text = Path(str(stem) + ".out").read_text()
    if marker and marker not in text:
        raise ValueError(f"missing {marker}: {stem}.out")
    return text


def environment(layout=0, **kwargs):
    return dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
                ABI_LAYOUT_PAD="x" * layout, **kwargs)


def invocation(binary, source):
    return [str(binary), *(["-Q", "--batch"] if "emacs" in binary.name else []),
            "--load", str(source)]


def files_record(paths):
    return {str(p.relative_to(ROOT)) if p.is_relative_to(ROOT) else str(p): sha(p)
            for p in paths}


def binary_identity(binary):
    binary = Path(binary).resolve()
    return {"binary": str(binary), "binary_sha256": sha(binary)}


def measure(binary, tag):
    sources = source_snapshot() if tag == "after" else {}
    binary = Path(shutil.which(binary) or binary).resolve()
    cmd = invocation(binary, BENCH)
    time_output = run(cmd, OUT / (tag + "-time"), environment(), "PC-COMPLETE")
    values = {n: {"us": float(v)} for n, v in
              re.findall(r"^PC-TIME (\S+) ([\d.]+)$", time_output, re.M)}
    if set(values) != set(NAMES):
        raise ValueError("incomplete timing output")

    def sample(item):
        name, layout, count = item
        label = name.replace("<", "lt").replace(">", "gt").replace("=", "eq")
        stem = OUT / f"{tag}-{label}-{layout}-{count}"
        cg = Path(str(stem) + ".cg")
        run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
             f"--callgrind-out-file={cg}", *cmd], stem,
            environment(layout, PC_CASE=name, PC_N=str(count)), "PC-COMPLETE")
        totals = re.findall(r"^summary: (\d+)$", cg.read_text(), re.M)
        if len(totals) != 1:
            raise ValueError(f"missing Ir: {cg}")
        return name, layout, count, int(totals[0]), cg

    jobs = [(name, layout, n) for name in NAMES for layout in LAYOUTS for n in (10, 110)]
    samples, artifacts = {}, [binary, BENCH]
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        for name, layout, n, total, cg in pool.map(sample, jobs):
            samples[name, layout, n] = total
            artifacts.extend([cg, Path(str(cg)[:-3] + ".out"), Path(str(cg)[:-3] + ".err")])
            if n == 110:
                print(f"{tag} {name} layout {layout} recorded", flush=True)
    for name in NAMES:
        values[name]["layouts"] = {str(layout):
            (samples[name, layout, 110] - samples[name, layout, 10]) / 100
            for layout in LAYOUTS}
        values[name]["Ir"] = sum(values[name]["layouts"].values()) / 4
        if min(values[name]["layouts"].values()) <= 0:
            raise ValueError(f"nonpositive operation count: {name}")
    artifacts.extend(p for p in binary.parent.glob(binary.name + ".*") if p.is_file())
    require_unchanged(sources)
    save(tag, {"files": files_record(artifacts), "operations": values, "sources": sources,
               "layouts": list(LAYOUTS), **binary_identity(binary)})


def retime(tag):
    """Warm the measured loop before timing, without repeating Ir profiles."""
    record = authenticated(tag)
    # The initial PC_N=1 run compiles pc-run and each measured lambda. Keep
    # compilation outside all three reported wall-clock trials.
    form = """(progn
      (dolist (row pc-cases)
        (let ((fn (cadr row)) (best nil))
          (pc-run fn 1)
          (dotimes (_ 3)
            (let ((start (float-time)))
              (pc-run fn 1000)
              (let ((us (* 1000.0 (- (float-time) start))))
                (when (or (null best) (< us best)) (setq best us)))))
          (princ (format "PC-WARM-TIME %s %.6f\\n" (car row) best))))
      (kill-emacs 0))"""
    stem = OUT / (tag + "-warm-time")
    output = run(invocation(Path(record["binary"]), BENCH) + ["--eval", form],
                 stem, environment(PC_N="1"), "PC-WARM-TIME string-prefix-p")
    values = dict(re.findall(r"^PC-WARM-TIME (\S+) ([\d.]+)$", output, re.M))
    assert set(values) == set(NAMES)
    for name, value in values.items():
        record["operations"][name]["us"] = float(value)
    record["files"].update(files_record([Path(str(stem) + ext) for ext in (".out", ".err")]))
    record["wall_recipe"] = form
    save(tag, record)


def profile():
    """Resolve differential self costs to native symbols, for each function."""
    record = authenticated("base")
    symbols = []
    for line in subprocess.check_output(["nm", "-n", record["binary"]], text=True).splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[1] in ("t", "T"):
            symbols.append((int(parts[0], 16), parts[2]))
    addresses = [x[0] for x in symbols]
    report, artifacts = {}, [OUT / "base-layers.out", OUT / "base-aliases.out"]
    for name in NAMES:
        label = name.replace("<", "lt").replace(">", "gt").replace("=", "eq")
        delta = {}
        for n, sign in ((10, -1), (110, 1)):
            cg = OUT / f"base-{label}-0-{n}.cg"
            annotated = subprocess.check_output(["callgrind_annotate", "--auto=no",
                                                 "--threshold=100", str(cg)], text=True)
            path = OUT / f"base-{label}-{n}-annotated.out"
            path.write_text(annotated)
            artifacts.extend([cg, path])
            for count, address in re.findall(r"^\s*([\d,]+)\s+\(\s*[\d.]+%\)\s+\?\?\?:0x([\da-f]+)", annotated, re.M):
                index = bisect.bisect_right(addresses, int(address, 16)) - 1
                symbol = symbols[index][1] if index >= 0 else address
                delta[symbol] = delta.get(symbol, 0) + sign * int(count.replace(",", "")) / 100
        if not delta:
            raise ValueError(f"no resolved profile rows: {name}")
        report[name] = sorted(delta.items(), key=lambda p: -p[1])[:20]
    save("baseline-profile", {"files": files_record(artifacts), "passed": True,
                              "base_commit": "692dc8a09", "self_Ir_per_operation": report})


def measure_load(binary, tag):
    sources = source_snapshot() if tag == "after" else {}
    binary = Path(binary).resolve()
    source = ROOT / "test/perf/prim-speed-load-five.el"
    artifacts, samples, outputs = [binary, source], [], []
    for layout in LAYOUTS:
        stem = OUT / f"{tag}-load-five-{layout}"
        output = run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                      f"--callgrind-out-file={stem}.cg", *invocation(binary, source)],
                     stem, environment(layout), "FIVE-LIBRARIES PASS: 5 loaded")
        profile_path = Path(str(stem) + ".cg")
        totals = re.findall(r"^summary: (\d+)$", profile_path.read_text(), re.M)
        assert len(totals) == 1 and int(totals[0]) > 0
        samples.append(int(totals[0]))
        outputs.append(output)
        artifacts.extend(Path(str(stem) + ext) for ext in (".cg", ".out", ".err"))
        print(f"{tag} load-five layout {layout}: {totals[0]} Ir", flush=True)
    require_unchanged(sources)
    save(tag + "-load-five", {"files": files_record(artifacts), "layouts": list(LAYOUTS), "sources": sources,
                              "samples": samples, "outputs": outputs, **binary_identity(binary)})


def measure_floor(binary, tag):
    """Measure the supplied anonymous-callback harness with an empty body."""
    binary = Path(shutil.which(binary) or binary).resolve()
    form = """(let ((pc-fn (lambda () nil)))
      (pc-run pc-fn 1)
      (pc-run pc-fn (string-to-number (getenv "PC_FLOOR_N")))
      (princ "PC-FLOOR-COMPLETE\\n") (kill-emacs 0))"""
    artifacts, samples = [binary, BENCH], []
    for layout in LAYOUTS:
        totals = []
        for n in (10, 110):
            stem = OUT / f"{tag}-floor-{layout}-{n}"
            run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                 f"--callgrind-out-file={stem}.cg", *invocation(binary, BENCH), "--eval", form],
                stem, environment(layout, PC_CASE="floor", PC_FLOOR_N=str(n)), "PC-FLOOR-COMPLETE")
            totals.append(int(re.findall(r"^summary: (\d+)$", Path(str(stem) + ".cg").read_text(), re.M)[0]))
            artifacts.extend(Path(str(stem) + ext) for ext in (".cg", ".out", ".err"))
        samples.append((totals[1] - totals[0]) / 100)
    assert min(samples) > 0
    save(tag + "-floor", {"files": files_record(artifacts), "samples": samples,
                          "Ir": sum(samples) / 4, "recipe": form})
    print(f"{tag} empty anonymous callback floor: {sum(samples) / 4:.2f} Ir")


def parity_run(binary, tag):
    sources = source_snapshot() if tag.startswith("after") else {}
    binary = Path(binary).resolve()
    oracle = run(invocation(Path("/usr/local/bin/emacs"), PARITY) + ["--eval", "(kill-emacs 0)"],
                 OUT / "gnu-parity", environment(), "PC-PARITY-COMPLETE")
    result = run(invocation(binary, PARITY) + ["--eval", "(kill-emacs 0)"], OUT / (tag + "-parity"),
                 environment(), "PC-PARITY-COMPLETE")
    artifacts = [binary, PARITY, OUT / "gnu-parity.out", OUT / "gnu-parity.err",
                 OUT / (tag + "-parity.out"), OUT / (tag + "-parity.err")]
    require_unchanged(sources)
    save(tag + "-parity", {"files": files_record(artifacts), "passed": oracle == result, "sources": sources,
                          "cases": int(re.findall(r"PC-PARITY-COMPLETE (\d+)", oracle)[0]),
                          **binary_identity(binary)})
    if oracle != result:
        different = [(i + 1, a, b) for i, (a, b) in
                     enumerate(zip(oracle.splitlines(), result.splitlines())) if a != b]
        (OUT / (tag + "-parity-diff.json")).write_text(json.dumps(different, indent=2) + "\n")
        raise ValueError(f"{len(different)} parity differences; see {tag}-parity-diff.json")


def check(action):
    if action in ("baseline-metrics", "candidate-metrics"):
        r = authenticated("base" if action.startswith("baseline") else "after")
        if action == "baseline-metrics":
            gnu = authenticated("gnu")
            assert gnu["layouts"] == list(LAYOUTS) and set(gnu["operations"]) == set(NAMES)
        assert r["layouts"] == list(LAYOUTS) and set(r["operations"]) == set(NAMES)
        for value in r["operations"].values():
            assert set(value["layouts"]) == {str(n) for n in LAYOUTS}
            assert min(value["layouts"].values()) > 0 and value["Ir"] > 0
    elif action == "ratios":
        a, g = authenticated("after"), authenticated("gnu")
        authenticated("base-floor")
        authenticated("gnu-floor")
        for name in NAMES:
            ratio = a["operations"][name]["Ir"] / g["operations"][name]["Ir"]
            print(f"{name}: {ratio:.3f}x GNU Ir")
            assert ratio <= IR_CAP, name
    elif action == "load-five":
        a, b = authenticated("after-load-five"), authenticated("base-load-five")
        assert a["layouts"] == b["layouts"] == list(LAYOUTS)
        assert len(set(a["outputs"] + b["outputs"])) == 1
        aa, bb = sum(a["samples"]) / 4, sum(b["samples"]) / 4
        print(f"five-file load: base {bb:.2f}, candidate {aa:.2f} Ir ({(aa / bb - 1) * 100:+.3f}%)")
        assert aa <= bb
    elif action.startswith("parity"):
        r = authenticated("after-parity")
        assert r["passed"] and r["cases"] >= 1392
        assert not (OUT / "after-parity.err").stat().st_size
        if action == "parity":
            assert authenticated("semantic-controls")["passed"]
            assert authenticated("after-no-fx-parity")["passed"]
            assert authenticated("selftest")["passed"]
    else:
        r = authenticated(action)
        assert r["passed"], action
        if action == "baseline-profile":
            assert r["base_commit"] == "692dc8a09"
            assert set(r["self_Ir_per_operation"]) == set(NAMES)
            assert all(r["self_Ir_per_operation"].values())
    print(action + ": PASS")


def controls(binary):
    """Run assertions normally and disabled, plus the broken-base control."""
    sources = source_snapshot()
    binary = Path(binary).resolve()
    source = ROOT / "test/standalone-string-compare-check.el"
    artifacts = [binary, OUT / "base", OUT / "candidate1", source]
    for tag, flags in (("normal", {}), ("disabled", {"NELISP_FX_DISABLE": "1"})):
        stem = OUT / ("controls-" + tag)
        run(invocation(binary, source), stem, environment(**flags), "STRING-COMPARE PASS: 39 assertions")
        assert not Path(str(stem) + ".err").stat().st_size
        artifacts.extend(Path(str(stem) + ext) for ext in (".out", ".err"))
    alias_form = """(let ((saved (symbol-function 'string-equal)))
      (unwind-protect
          (progn
            (fset 'string-equal (lambda (&rest _args) 'replacement))
            (unless (and (eq (string= "a" "b") 'replacement)
                         (eq (string-equal "a" "b") 'replacement))
              (error "string= did not follow its redefined target"))
            (princ "STRING-EQUAL-ALIAS PASS\\n"))
        (fset 'string-equal saved))
      (kill-emacs 0))"""
    stem = OUT / "controls-equality-alias"
    run([str(binary), "--eval", alias_form], stem, environment(), "STRING-EQUAL-ALIAS PASS")
    artifacts.extend(Path(str(stem) + ext) for ext in (".out", ".err"))
    for tag, name in (("broken-base", "base"), ("broken-wrapper-guard", "candidate1")):
        stem = OUT / ("controls-" + tag)
        with Path(str(stem) + ".out").open("wb") as out, Path(str(stem) + ".err").open("wb") as err:
            result = subprocess.run(invocation(OUT / name, source), cwd=ROOT,
                                    env=environment(), stdout=out, stderr=err, timeout=55)
        diagnostic = Path(str(stem) + ".err").read_text()
        expected = ("wrong-type-argument: (number-or-marker-p t)" if name == "base"
                    else "assertion 17 failed")
        assert result.returncode != 0 and expected in diagnostic, diagnostic[:400]
        artifacts.extend(Path(str(stem) + ext) for ext in (".out", ".err"))
    library_source = ROOT / "test/perf/prim-strcmp-library-parity.el"
    run(invocation(Path("/usr/local/bin/emacs"), library_source), OUT / "controls-library",
        environment(), "STRING-LIBRARY PASS: 675 comparisons")
    artifacts.extend([library_source, OUT / "controls-library.out", OUT / "controls-library.err"])
    old_source = OUT / "library-before.el"
    with (OUT / "controls-library-broken.out").open("wb") as out, (OUT / "controls-library-broken.err").open("wb") as err:
        old = subprocess.run(invocation(Path("/usr/local/bin/emacs"), library_source), cwd=ROOT,
                             env=environment(PC_LIBRARY_SOURCE=str(old_source)), stdout=out,
                             stderr=err, timeout=55)
    assert old.returncode != 0
    assert "Library parity case 4" in (OUT / "controls-library-broken.err").read_text()
    artifacts.extend([old_source, OUT / "controls-library-broken.out", OUT / "controls-library-broken.err"])
    require_unchanged(sources)
    save("semantic-controls", {"files": files_record(artifacts), "passed": True, "sources": sources,
                               **binary_identity(binary)})


def census(path, binary):
    spec = importlib.util.spec_from_file_location("box_census", ROOT / "tools/ai/box-census.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    base = OUT / "base-census.tsv"
    candidate = Path(path).resolve()
    binary = Path(binary).resolve()
    values = []
    for p in (base, candidate):
        rows = module.load(p)
        summary, _ = module.tally(rows)
        total = sum(summary["idioms"].values())
        conversion = sum(summary["idioms"].get(k, 0) for k in
                         ("nl_val_store_word", "nl_abi_rc_to_word", "nl_abi_rc2_to_word", "inline-box-to-word"))
        values.append((total, conversion))
        print(f"{p.name}: total {total}; S6.2 {conversion}")
    assert values[1][0] <= values[0][0] and values[1][1] == 0
    save("final-census", {"files": files_record([base, candidate, binary]), "passed": True,
                          "values": values, "sources": source_snapshot(), **binary_identity(binary)})


def selftest():
    """Show actual ledger checks rejecting missing, stale and broken input."""
    global OUT
    actual_out = OUT
    count = 0
    with tempfile.TemporaryDirectory(dir=ROOT / "target/tmp") as directory:
        OUT = Path(directory)
        try:
            try:
                authenticated("absent")
            except FileNotFoundError:
                count += 1
            artifact = OUT / "fixture"
            artifact.write_text("original")
            save("fixture", {"files": files_record([artifact])})
            authenticated("fixture")
            artifact.write_text("mutated")
            try:
                authenticated("fixture")
            except ValueError:
                count += 1
            operations = {n: {"Ir": 100, "us": 1, "layouts": {str(l): 100 for l in LAYOUTS}} for n in NAMES}
            identity = binary_identity(artifact)
            fixture_files = files_record([artifact])
            for tag in ("gnu", "after"):
                save(tag, {"files": fixture_files, "operations": operations, "layouts": list(LAYOUTS),
                           "sources": source_snapshot(), **identity})
            for tag in ("base-floor", "gnu-floor"):
                save(tag, {"files": {}, "Ir": 1})
            check("ratios")
            operations["string-lessp"]["Ir"] = IR_CAP * 100 + 1
            save("after", {"files": fixture_files, "operations": operations, "layouts": list(LAYOUTS),
                           "sources": source_snapshot(), **identity})
            try:
                check("ratios")
            except AssertionError:
                count += 1
            operations.pop("compare-strings")
            save("after", {"files": fixture_files, "operations": operations, "layouts": list(LAYOUTS),
                           "sources": source_snapshot(), **identity})
            try:
                check("candidate-metrics")
            except AssertionError:
                count += 1
            stale = source_snapshot()
            stale[next(iter(stale))] = "0" * 64
            save("after", {"files": {}, "sources": stale})
            try:
                authenticated("after")
            except ValueError:
                count += 1
            save("after", {"files": fixture_files, "sources": source_snapshot(), **identity})
            other = OUT / "other-binary"
            other.write_text("another executable")
            save("after-load-five", {"files": files_record([other]), "sources": source_snapshot(),
                                     **binary_identity(other)})
            try:
                authenticated("after-load-five")
            except ValueError as error:
                assert "differs from measured candidate" in str(error)
                count += 1
            assert count == 6
        finally:
            OUT = actual_out
    save("selftest", {"files": files_record([Path(__file__).resolve()]), "passed": True,
                      "negative_controls": count})
    print(f"VERIFIER PASS: {count} negative controls plus positive ratio control")


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    (ROOT / "target/tmp").mkdir(parents=True, exist_ok=True)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action")
    parser.add_argument("binary", nargs="?")
    parser.add_argument("tag", nargs="?")
    args = parser.parse_args()
    if args.action == "measure":
        measure(args.binary, args.tag)
    elif args.action == "retime":
        retime(args.binary)
    elif args.action == "profile":
        profile()
    elif args.action == "measure-load":
        measure_load(args.binary, args.tag)
    elif args.action == "measure-floor":
        measure_floor(args.binary, args.tag)
    elif args.action == "controls":
        controls(args.binary)
    elif args.action == "census":
        census(args.binary, args.tag)
    elif args.action == "selftest":
        selftest()
    elif args.action == "parity-run":
        parity_run(args.binary, args.tag)
    else:
        check(args.action)


if __name__ == "__main__":
    try:
        main()
    except (AssertionError, OSError, ValueError, KeyError, subprocess.TimeoutExpired) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
