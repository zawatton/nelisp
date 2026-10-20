#!/usr/bin/env python3
"""Strict Doc 216 S4 instruction gate; the historical producer is immutable.

capture LABEL BINARY [--output DIR] uses the exact original capture program.
capture-source --candidate-prefix LABEL binds both measured linkages to the live
build guard. check --candidate-prefix LABEL (default final) replays all 48 raw
profiles and that source receipt. controls exercises the real merged regression
and corrupted copies, without editing any original evidence. No verdict is cached.
Source coverage is the existing guard: direct src/lisp/scripts *.el plus staged
subr.el, not a claim to cover every compiler or vendored input.
"""
from __future__ import annotations

import argparse
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
import time

ROOT = Path(__file__).resolve().parents[2]
PRODUCER = "target/wh-s4f/ir-investigation/capture-ir.py"
PRODUCER_SHA = "adf06625508cbccd28e68fe6f56b102aa248a1c5b29ef5bc4cf42975ce17024b"
SOURCE_CHECKER = "tools/ai/word-heap-vectorlike-source.py"
SOURCE_CHECKER_SHA = "4e312d693942f0682416ceb428d1008a69b0183af8afa438460cbfdae7a0bdc4"
DEFAULT_OUTPUT = "target/wh-s4f/ir"
BASE_MANIFESTS = {
    "dynamic": "014335687ae04d8dc38e5de2c40df3fd879571dbdb05a38268b6b2e8f41eda09",
    "static": "64c7d353d0ee3b9654385a735cc0d2a92e4b37d0785a26a31f8a48e6b201c7f8",
}
MODES = ("boot", "files", "regex")
LAYOUTS = (0, 700, 1400, 2100)
LINKAGES = ("dynamic", "static")


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def relative(path):
    return Path(path).resolve().relative_to(ROOT).as_posix()


def local(path):
    value = Path(path)
    need(not value.is_absolute() and ".." not in value.parts,
         "evidence path must be repository-relative")
    result = (ROOT / value).resolve()
    need(result.is_relative_to(ROOT), "evidence path escaped repository")
    return result


def label(value):
    need(re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]*", value), "invalid measurement label")
    return value


def load_pinned(name, expected, module):
    path = ROOT / name
    need(sha(path) == expected, "changed pinned program: " + name)
    spec = importlib.util.spec_from_file_location(module, path)
    result = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(result)
    return result


def programs():
    # Hash before import: importing the producer also loads the ABI harness.
    producer = ROOT / PRODUCER
    need(sha(producer) == PRODUCER_SHA, "changed pinned producer")
    baseline = json.loads((ROOT / DEFAULT_OUTPUT / "base-dynamic/measurement.json").read_text())
    need(sha(ROOT / DEFAULT_OUTPUT / "base-dynamic/measurement.json") == BASE_MANIFESTS["dynamic"],
         "changed pinned baseline manifest: dynamic")
    for name, digest in baseline["common_inputs"]["files"].items():
        need(sha(local(name)) == digest, "changed pinned capture input: " + name)
    return load_pinned(PRODUCER, PRODUCER_SHA, "s4f_capture"), load_pinned(
        SOURCE_CHECKER, SOURCE_CHECKER_SHA, "s4f_source")


def scrub_environment():
    # Exactly the original producer's main(): do not silently normalize LANG,
    # LD_* or other LC_* variables that its provenance intentionally measures.
    os.environ["LC_ALL"] = "C.UTF-8"
    for key in tuple(os.environ):
        if key.startswith(("NELISP_", "ABI_", "M5_")):
            del os.environ[key]


def profile_integrity(path, binary, mode, directory):
    """Check raw exclusive costs independently of the producer's summary reader."""
    summaries, totals, commands, events, positions = [], [], [], [], []
    exclusive = 0
    costs = 0
    arc = False
    with path.open() as stream:
        for line in stream:
            if line.startswith("summary:"):
                summaries.append(int(line.split(":", 1)[1]))
            elif line.startswith("totals:"):
                totals.append(int(line.split(":", 1)[1]))
            elif line.startswith("cmd:"):
                commands.append(line.split(":", 1)[1].strip())
            elif line.startswith("events:"):
                events.append(line.split(":", 1)[1].strip())
            elif line.startswith("positions:"):
                positions.append(line.split(":", 1)[1].strip())
            elif line.startswith("calls="):
                need(not arc, "call arc has no cost: " + path.name)
                arc = True
            elif line[:1].isdigit() or line[:1] in ("+", "-", "*"):
                fields = line.split()
                need(len(fields) == 3 and fields[-1].isdigit(), "bad Ir cost: " + path.name)
                if arc:
                    arc = False
                else:
                    exclusive += int(fields[-1])
                    costs += 1
    need(not arc and costs > 0 and events == ["Ir"] and positions == ["instr line"],
         "incomplete raw instruction profile: " + path.name)
    need(summaries == totals == [exclusive] and exclusive > 0,
         "summary/totals differ from raw exclusive Ir: " + path.name)
    args = "--eval nil" if mode == "boot" else "--load " + str(
        ROOT / "tools/ai/abi-file-exec.el" if mode == "files" else directory / "regex.el")
    need(commands == [str(binary) + " " + args], "wrong raw profile command: " + path.name)


def replay(producer, output, name, *, control_original=None):
    name = label(name)
    directory = output / name
    command_directory = directory
    if control_original is not None:
        # Only controls supply this immutable snapshot. Copied profiles still
        # name the original capture's regex path; authenticate that origin
        # rather than treating any arbitrary alternate command path as valid.
        original, manifest_sha, original_binary = control_original
        need(original.name == name and original.resolve().is_relative_to(ROOT)
             and original.resolve() != directory.resolve(), "invalid control origin directory")
        need(sha(original / "measurement.json") == manifest_sha, "changed control origin manifest")
        origin = json.loads((original / "measurement.json").read_text())
        need(origin["label"] == name and origin["binary"] == original_binary
             and producer.binary_identity(Path(origin["binary_path"])) == original_binary,
             "changed control origin binary trio")
        need(origin["common_inputs"] == producer.common_identity()
             and all(sha(original / filename) == digest
                     for filename, digest in origin["artifacts"].items()),
             "changed control origin capture inputs")
        command_directory = original
    record, rows = producer.replay(output, name)
    if control_original is not None:
        need(record["binary"] == original_binary and record["binary_path"] == origin["binary_path"],
             "control copy changed original binary association")
    need(type(record["gate_cap"]) in (int, float) and record["gate_cap"] == 1.0
         and record["total_includes_GC"] is True, "wrong original cap/GC policy: " + name)
    need(all(type(row["Ir"]) is int and row["Ir"] > 0
             and type(row["GC"]) is int and row["GC"] >= 0
             and type(row["returncode"]) is int and row["returncode"] == 0
             for row in record["completed_jobs"].values()), "invalid profile counters: " + name)
    need(set(record["completed_jobs"]) == {
        f"{mode}-{name}-{layout}" for mode in MODES for layout in LAYOUTS},
        "wrong completed-job keys: " + name)
    binary = Path(record["binary_path"])
    need(binary.is_absolute() and binary.resolve().is_relative_to(ROOT), "binary outside repository")
    for mode in MODES:
        for layout in LAYOUTS:
            profile_integrity(directory / f"{mode}-{name}-{layout}.cg", binary, mode, command_directory)
    return record, rows


def replay_all(producer, output, prefix):
    records, results = {}, {}
    for linkage in LINKAGES:
        base, candidate = "base-" + linkage, prefix + "-" + linkage
        manifest = output / base / "measurement.json"
        need(sha(manifest) == BASE_MANIFESTS[linkage], "changed pinned baseline manifest: " + linkage)
        records[base], baseline = replay(producer, output, base)
        records[candidate], measured = replay(producer, output, candidate)
        need(records[base]["binary"]["executable"]["sha256"] !=
             records[candidate]["binary"]["executable"]["sha256"],
             "same executable compared with itself: " + linkage)
        results[linkage] = {}
        for mode in MODES:
            b, c = baseline[mode], measured[mode]
            # Complete stdout must agree across both binaries AND every layout.
            same = len({row[2] for row in (*b, *c)}) == 1
            bb, cc = sum(row[0] for row in b), sum(row[0] for row in c)
            passed = same and cc <= bb  # Exact original 1.0 cap; GC is never subtracted.
            results[linkage][mode] = {"base_mean_Ir": bb / 4, "candidate_mean_Ir": cc / 4,
                "delta_percent": (cc / bb - 1) * 100, "stdout_identical": same,
                "base_GC": [row[1] for row in b], "candidate_GC": [row[1] for row in c],
                "pass_original_cap": passed}
            print(f"Ir {linkage} {mode} base={bb / 4:.2f} candidate={cc / 4:.2f} "
                  f"delta={(cc / bb - 1) * 100:+.3f}% stdout={'identical' if same else 'DIFF'} "
                  f"{'PASS' if passed else 'FAIL'}", flush=True)
    need(len({record["binary"]["executable"]["sha256"] for record in records.values()}) == 4,
         "all four baseline/candidate executables must have distinct identities")
    return records, results


def measurement_items(source, output, prefix):
    return {linkage: source.item(output / (prefix + "-" + linkage) / "measurement.json")
            for linkage in LINKAGES}


def source_query(source, binary, directory, name, timeout):
    query = directory / "query-source-identity.el"
    if not query.exists():
        query.write_bytes(source.QUERY_SOURCE)
    need(query.read_bytes() == source.QUERY_SOURCE, "changed identity query")
    cache = directory / (name + "-cache")
    cache.mkdir()
    env = {"PATH": os.environ.get("PATH", os.defpath), "LANG": "C.UTF-8", "LC_ALL": "C.UTF-8",
           "TMPDIR": str(ROOT / "target/tmp"), "XDG_CACHE_HOME": str(cache)}
    raw = source.run_raw(directory, name + "-identity", [relative(binary), "--load", relative(query),
                          "--eval", "nil"], env=env, timeout=timeout)
    return source.parse_candidate(source.replay_raw(raw)), raw


def capture_source(producer, source, output, prefix, emacs, timeout):
    receipt = output / (prefix + "-source.json")
    directory = output / (prefix + "-source")
    need(not receipt.exists() and not directory.exists(), "source receipt already exists; preserve it")
    directory.mkdir(parents=True)
    rows = source.source_rows(ROOT)
    identity, host = source.host_identity(ROOT, rows, directory, emacs)
    measured = measurement_items(source, output, prefix)
    record = {"schema": 1, "scope": source.SCOPE, "prefix": prefix,
              "producer_sha256": PRODUCER_SHA, "source_checker_sha256": SOURCE_CHECKER_SHA,
              "source_rows": rows, "source_identity": identity, "host": host,
              "measurements": measured, "candidates": {}}
    for linkage in LINKAGES:
        manifest = json.loads((output / (prefix + "-" + linkage) / "measurement.json").read_text())
        binary = Path(manifest["binary_path"])
        trio = producer.binary_identity(binary)
        need(trio == manifest["binary"], "measured binary trio changed: " + linkage)
        embedded, raw = source_query(source, binary, directory, linkage, timeout)
        need(embedded == identity, "candidate build guard differs from current sources: " + linkage)
        record["candidates"][linkage] = {"binary": trio, "query": raw}
    need(source.source_rows(ROOT) == rows and measured == measurement_items(source, output, prefix),
         "source or measurement changed during source capture")
    with receipt.open("x") as stream:
        stream.write(json.dumps(record, indent=2) + "\n")
    print(f"SOURCE CAPTURE PASS prefix={prefix} files={len(rows)} receipt={relative(receipt)}")


def check_source(producer, source, output, prefix, records, emacs, timeout):
    receipt = output / (prefix + "-source.json")
    value = json.loads(receipt.read_text())
    need(value["schema"] == 1 and value["scope"] == source.SCOPE and value["prefix"] == prefix
         and value["producer_sha256"] == PRODUCER_SHA
         and value["source_checker_sha256"] == SOURCE_CHECKER_SHA, "bad source receipt schema")
    need(value["measurements"] == measurement_items(source, output, prefix), "changed measured manifests")
    rows = source.source_rows(ROOT)
    need(rows == value["source_rows"], "current build-guard source bytes/set changed")
    host = value["host"]
    need(source.verify_item(host["source"]).read_bytes() == source.HOST_SOURCE,
         "changed captured host identity query")
    need(json.loads(source.verify_item(host["rows"]).read_text()) == rows, "changed captured source rows")
    need(source.parse_host(source.replay_raw(host["raw"])) == value["source_identity"],
         "captured host identity mismatch")
    need(set(value["candidates"]) == set(LINKAGES), "source receipt needs both linkages")
    with tempfile.TemporaryDirectory(prefix="wh-s4f-source-check-", dir=ROOT / "target/tmp") as temp:
        directory = Path(temp)
        identity, _ = source.host_identity(ROOT, rows, directory, emacs)
        need(identity == value["source_identity"], "current build guard changed")
        for linkage in LINKAGES:
            record = records[prefix + "-" + linkage]
            saved = value["candidates"][linkage]
            binary = Path(record["binary_path"])
            need(saved["binary"] == record["binary"] == producer.binary_identity(binary),
                 "source receipt and measured binary trio differ: " + linkage)
            need(source.parse_candidate(source.replay_raw(saved["query"])) == identity,
                 "captured candidate build guard mismatch: " + linkage)
            captured_query = saved["query"]["command"]
            need(captured_query == [relative(binary), "--load",
                 relative(output / (prefix + "-source") / "query-source-identity.el"), "--eval", "nil"],
                 "wrong captured candidate identity command")
            need((output / (prefix + "-source") / "query-source-identity.el").read_bytes()
                 == source.QUERY_SOURCE, "changed captured candidate query")
            embedded, _ = source_query(source, binary, directory, linkage, timeout)
            need(embedded == identity, "live candidate build guard mismatch: " + linkage)
    need(source.source_rows(ROOT) == rows, "sources changed during source check")
    print(f"SOURCE PASS files={len(rows)} candidates=2 identity={value['source_identity']}")
    return rows


def write_report(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2) + "\n")


def check(producer, source, args):
    started = time.monotonic()
    output, prefix = local(args.output), label(args.candidate_prefix)
    before = producer.common_identity()
    records, results = replay_all(producer, output, prefix)
    rows = check_source(producer, source, output, prefix, records, args.emacs, args.timeout)
    # Detect mutations over the whole check, not just while each profile was read.
    for name, record in records.items():
        need(json.loads((output / name / "measurement.json").read_text()) == record,
             "measurement changed during check: " + name)
        need(producer.binary_identity(Path(record["binary_path"])) == record["binary"],
             "binary changed during check: " + name)
        need(all(sha(output / name / filename) == digest
                 for filename, digest in record["artifacts"].items()), "evidence changed during check: " + name)
    need(source.source_rows(ROOT) == rows and producer.common_identity() == before,
         "capture inputs changed during check")
    passed = all(row["pass_original_cap"] for group in results.values() for row in group.values())
    report = {"schema": 1, "candidate_prefix": prefix, "profiles": 48, "gate_cap": 1.0,
              "total_includes_GC": True, "results": results, "pass": passed,
              "seconds": time.monotonic() - started}
    write_report(output / (prefix + "-gate-report.json"), report)
    print(f"WORD-HEAP S4F IR {'PASS' if passed else 'FAIL'} profiles=48 cap=1.0 total-includes-GC "
          f"seconds={report['seconds']:.3f}")
    return 0 if passed else 1


def controls(producer, args):
    output, prefix = local(args.output), label(args.candidate_prefix)
    records, results = replay_all(producer, output, prefix)
    need(all(not row["pass_original_cap"] for group in results.values() for row in group.values()),
         "real merged regression must be red in all six workload/linkage cells")
    rejected, accepted = {}, []
    # Copies retain original labels, binary paths and raw command paths. Only the
    # nominated corrupted input is replaced; originals are never written.
    for linkage in LINKAGES:
        name = prefix + "-" + linkage
        original = output / name
        need(json.loads((original / "measurement.json").read_text()) == records[name],
             "control origin changed after authenticated replay")
        origin = (original, sha(original / "measurement.json"), records[name]["binary"])
        for mutation in ("profile", "identity", "count", "summary"):
            with tempfile.TemporaryDirectory(prefix="wh-s4f-control-", dir=ROOT / "target/tmp") as temp:
                shadow = Path(temp)
                directory = shadow / name
                directory.mkdir()
                for artifact in original.iterdir():
                    if artifact.is_file():
                        (directory / artifact.name).symlink_to(artifact)
                manifest = directory / "measurement.json"
                record, _ = replay(producer, shadow, name, control_original=origin)
                need(record == records[name], "unmodified shadow differs from authenticated origin")
                accepted.append(linkage + "-" + mutation)
                print(f"CONTROL {linkage}-{mutation} UNMODIFIED SHADOW PASS profiles=12", flush=True)
                manifest.unlink()
                stem = "boot-" + name + "-0"
                if mutation in ("profile", "summary"):
                    target = directory / (stem + ".cg")
                    content = target.read_text()
                    target.unlink()
                    if mutation == "profile":
                        content += "\n# corrupted profile control\n"
                    else:
                        old = record["completed_jobs"][stem]["Ir"]
                        content = content.replace("summary: " + str(old), "summary: " + str(old - 1), 1)
                        # Even a forged manifest count/hash must not make a lower
                        # summary authentic: raw exclusive Ir still disagrees.
                        record["completed_jobs"][stem]["Ir"] = old - 1
                    target.write_text(content)
                    if mutation == "summary":
                        record["artifacts"][target.name] = sha(target)
                elif mutation == "identity":
                    record["binary"]["executable"]["sha256"] = "0" * 64
                else:
                    record["completed_jobs"][stem]["Ir"] += 1
                manifest.write_text(json.dumps(record))
                expected = {"profile": "changed evidence", "identity": "changed binary trio",
                            "count": "changed profile counts",
                            "summary": "summary/totals differ from raw exclusive Ir: " + stem + ".cg"}[mutation]
                try:
                    replay(producer, shadow, name, control_original=origin)
                except ValueError as error:
                    need(str(error) == expected,
                         f"negative control {linkage}-{mutation} rejected for wrong reason: {error}")
                    rejected[linkage + "-" + mutation] = str(error)
                else:
                    raise ValueError("negative control escaped: " + linkage + "-" + mutation)
                print(f"CONTROL {linkage}-{mutation} REJECT {rejected[linkage + '-' + mutation]}")
    need(len(accepted) == len(rejected) == 8, "incomplete control pairs")
    write_report(output / (prefix + "-gate-controls.json"),
                 {"real_regression": results, "unmodified_shadows": accepted, "rejected": rejected})
    print("CONTROLS PASS real-red-profiles=24 unmodified-shadows=8 corruptions=8 intended-reasons=8")
    return 0


def main():
    scrub_environment()
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("capture", "capture-source", "check", "controls"))
    parser.add_argument("label", nargs="?")
    parser.add_argument("binary", nargs="?")
    parser.add_argument("--candidate-prefix", default="final")
    parser.add_argument("--output", default=DEFAULT_OUTPUT)
    parser.add_argument("--workers", type=int, choices=(1, 2, 3, 4), default=4)
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--timeout", type=int, default=180)
    args = parser.parse_args()
    (ROOT / "target/tmp").mkdir(parents=True, exist_ok=True)
    producer, source = programs()
    if args.action == "capture":
        need(args.label and args.binary, "capture requires LABEL BINARY")
        producer.capture(local(args.output), label(args.label), local(args.binary), args.workers)
        return 0
    need(args.label is None and args.binary is None, "unexpected positional arguments")
    if args.action == "capture-source":
        capture_source(producer, source, local(args.output), label(args.candidate_prefix), args.emacs, args.timeout)
        return 0
    if args.action == "controls":
        return controls(producer, args)
    return check(producer, source, args)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (ValueError, OSError, KeyError, TypeError, subprocess.SubprocessError) as error:
        print(f"WORD-HEAP S4F IR FAIL: {error}", file=sys.stderr)
        raise SystemExit(1)
