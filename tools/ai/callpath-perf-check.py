#!/usr/bin/env python3
"""Fail-closed meter for call-path safety, measured Ir and final evidence.

The A1 brief intentionally defers C2/C3 and the full final verification; missing
measurements or the deferred final receipt remain red under the original caps.
All performance values are independently recomputed from authenticated profiles.
"""
import argparse
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import re
import statistics
import sys

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf"
LAYOUTS = {0, 700, 1400, 2100}
MODES = ("interpreter", "vm", "bytecode")
LINKAGES = ("dynamic", "static")
REFERENCE = ROOT.parent / "wh-s4f-20261021/target/wh-s4f/session3a"
SESSION_SHA = "cd63566e38c603c96ae27766108f15b6c84b10440d25a296f3c5a375ed315622"
CAPS = {"interpreter": 15823.0, "vm": 13568.0, "bytecode": 2538.0}
MAX_ADDRESS_SPACE = 8 * 1024 ** 3


def need(ok, message):
    if not ok:
        raise ValueError(message)


def read(path):
    return json.loads(Path(path).read_text())


def sha(path):
    with Path(path).open("rb") as f:
        return hashlib.file_digest(f, "sha256").hexdigest()


def auth(pin):
    path = Path(pin["path"])
    if not path.is_absolute():
        path = ROOT / path
    need(path.is_file() and sha(path) == pin["sha256"], "changed/missing evidence: " + str(path))
    return path


def source_identity():
    spec = importlib.util.spec_from_file_location("cpp_measure", ROOT / "tools/ai/callpath-perf-measure.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.source_identity()



def check_memory_policy(policy):
    need(policy["jobs"] == 1 and policy["maximum_bytes"] == MAX_ADDRESS_SPACE and
         0 < policy["rlimit_as_soft_bytes"] <= MAX_ADDRESS_SPACE and
         policy["required_heavy_lock"] == "/tmp/notes-heavy.lock",
         "measurement violates the 8 GiB/jobs1 memory and load policy")

def profile(pin, expected, isolated=False):
    path = auth(pin)
    summary, triggered, termination = None, False, False
    # Streaming avoids retaining or printing function/address tables.
    with path.open() as stream:
        for line in stream:
            triggered |= "Trigger: " in line
            termination |= "Program termination" in line
            if line.startswith("summary: "):
                need(summary is None, "multiple profile summaries")
                summary = int(line.split()[1])
    need(summary == expected and summary > 0, "profile summary/record mismatch")
    if isolated:
        need(triggered and not termination, "profile is not the isolated measured GC interval")


def perf(suite, tag):
    directory = OUT / (suite + "-queued") / tag
    campaign = read(directory / "campaign.json")
    report = read(directory / "perf.json")
    need(campaign["protocol"] == "queued-3-gc-v1", "unqualified measurement protocol")
    auth(campaign["script"])
    check_memory_policy(campaign["memory_policy"])
    need(campaign["profile_includes_following_GC"] is True and report["isolated"] is True,
         "measured interval must include GC in an isolated process")
    need(campaign["n"] == report["n"] == (100 if suite == "study" else 300), "iteration mismatch")
    selected = report["qualified_binary"]
    auth(selected)
    need(selected["sha256"] == campaign["binary"]["sha256"] == report["binary"]["sha256"], "binary identity mismatch")
    auth(report["binary"])
    for pin in report.get("binary_companions", []) + report["inputs"]:
        auth(pin)
    need(len(report["runs"]) == 12 and {(r["mode"], r["layout"]) for r in report["runs"]} ==
         {(m, l) for m in MODES for l in LAYOUTS}, "incomplete/duplicate mode/layout campaign")
    means = {}
    required = {"control", "while", "funcall"} if suite == "study" else {"control", "while", "builtin", "let"}
    for run in report["runs"]:
        rows = {r["op"]: r for r in run["intervals"]}
        need(set(rows) == required and len(run["intervals"]) == len(required), "operation coverage differs")
        for op, row in rows.items():
            need(row["returncode"] == row["wall_returncode"] == 0, "failed profile invocation")
            for key in ("stdout", "stderr", "wall_stdout", "wall_stderr", "control_stdout", "control_stderr"):
                auth(row[key])
            need(b"PCF-COMPLETE 1\n" in auth(row["stdout"]).read_bytes(), "incomplete measured interval")
            need(row["profile"]["path"].endswith(".cg.3"), "measured interval must be third explicit GC dump")
            profile(row["profile"], row["ir"], isolated=True)
            if op == "control":
                continue
            baseline = rows["control"] if op == "while" else rows["while"]
            value = (row["ir"] - baseline["ir"]) / report["n"]
            need(math.isfinite(value) and (value > 0 or suite == "study" and op == "while") and abs(value - row["ir_per_op"]) < 1e-9, "Ir delta mismatch")
            means.setdefault(run["mode"], {}).setdefault(op, []).append(value)
    means = {mode: {op: statistics.mean(values) for op, values in ops.items()} for mode, ops in means.items()}
    if tag.startswith("result-"):
        need(campaign["source"] == source_identity(), "candidate build source is stale")
    return means, report


def auth_tree(value):
    if isinstance(value, dict):
        if "path" in value and "sha256" in value:
            auth(value)
        else:
            for item in value.values():
                auth_tree(item)
    elif isinstance(value, list):
        for item in value:
            auth_tree(item)


def arity_report(report, tag, *, link_build=True):
    auth_tree(report)
    names, table = read(auth(report["contracts"]))
    by_name = {name: (minimum, maximum) for name, minimum, maximum in table}
    need(len(names) == len(set(names)) > 0 and len(by_name) == len(table) and set(names) <= by_name.keys(),
         "empty/duplicate/incomplete builtin contracts")
    c = report["counts"]
    need(c["builtins"] == len(names), "builtin count differs from inventory")
    expected, boundary_specs, low, high, number = [], [], 0, 0, 0
    for name in names:
        minimum, maximum = by_name[name]
        boundaries = []
        if minimum:
            low += 1
            boundaries.append(minimum - 1)
        if maximum != "many":
            high += 1
            boundaries.append(maximum + 1)
        for argc in boundaries:
            boundary_specs.append((name, argc))
            for callable_ in (name, "(builtin " + name + ")", "(builtin " + name + ")"):
                expected.append(f"({number} (wrong-number-of-arguments {callable_} {argc}))")
                number += 1
    need((c["min"], c["max"]) == (low, high) and low > 0 and high > 0, "arity boundary counts differ")
    need(report["extract"]["exit"] == 0, "contract extraction failed")
    regression = report["regression"]
    regression_lines = auth(regression["stdout"]).read_text().splitlines()
    need(regression["readiness_confirmed"] is True and len(regression_lines) >= 3 and
         regression["public_error"] == regression_lines[0] and
         regression_lines[0].startswith("(callpath-public-error (wrong-number-of-arguments ") and
         regression_lines[0].endswith(" 1))") and
         regression_lines[1:3] == ["CALLPATH-PUBLIC-ARITY-READY", "CALLPATH-PRIVATE-ENTERED"],
         "base/control crashed or stopped before public arity readiness and private entry")
    if tag == "base":
        need(len(regression_lines) == 3 and regression["exit"] in (-11, 139) and
             regression["defect_present"], "missing isolated private-builtin crash control")
        return number
    check_memory_policy(report["memory_policy"])
    need(regression["exit"] == 0 and not auth(regression["stderr"]).read_bytes() and
         len(regression_lines) == 4 and regression_lines[-1] == "(wrong-number-of-arguments nelisp--string-search 1)",
         "string-search regression control failed")
    expected.append("(callpath-arity-done 42)")
    actual = auth(report["native"]["stdout"]).read_text().splitlines()
    need(report["native"]["exit"] == 0 and not auth(report["native"]["stderr"]).read_bytes(), "native arity process failed")
    need(actual == expected and c["passed"] == number and c["failed"] == 0 and not report["failures"],
         "native arity output/counts do not match complete contract sweep")
    gnu = report["gnu_run"]
    need(gnu["exit"] == 0 and not auth(gnu["stderr"]).read_bytes(), "GNU arity control failed")
    gnu_cases = json.loads(auth(gnu["stdout"]).read_text())
    need(len(gnu_cases) == c["gnu_cases"] == c["gnu_passed"] > 0, "GNU arity case count differs")
    need(len(gnu_cases) % 3 == 0, "GNU arity routes incomplete")
    seen = set()
    for offset in range(0, len(gnu_cases), 3):
        group = gnu_cases[offset:offset + 3]
        index = group[0][0]
        need(isinstance(index, int) and 0 <= index < len(boundary_specs) and index not in seen,
             "GNU arity boundary missing/duplicate")
        seen.add(index)
        need([r[0] for r in group] == [index] * 3 and [r[1] for r in group] == ["direct", "funcall", "apply"],
             "GNU arity direct/funcall/apply routes incomplete")
        name, argc = boundary_specs[index]
        need(group[0][2] == f"(wrong-number-of-arguments {name} {argc})", "GNU direct arity data differs")
        need(group[1][2] == group[2][2] and group[1][2].startswith("(wrong-number-of-arguments ") and
             group[1][2].endswith(f" {argc})"), "GNU indirect arity data differs")

    valid = report["valid"]
    cases = read(auth(valid["cases"]))
    expected_valid = [f"({index} {case['expected']})" for index, case in enumerate(cases)] + ["callpath-valid-done"]
    need(auth(valid["expected"]).read_text().splitlines() == expected_valid,
         "valid-boundary expected output differs from its case definitions")
    required_valid = {"rdf", "nelisp--syscall-read-file", "nelisp--readdir-scan-raw",
                      "nelisp--syscall-readdir-names", "nelisp--read-all-from-string-native",
                      "nelisp--debug-switch", "nelisp-portable-syscall", "syscall-direct"}
    need(all(sum(case["name"] == name for case in cases) >= 2 for name in required_valid),
         "missing private optional min/max valid-boundary controls")
    need(sum(case["name"] == "nelisp--gc-diag" for case in cases) >= 2 and
         all(any(case["name"] == name for case in cases) for name in
             ("nelisp--string-search", "nelisp--string-search-chars")),
         "missing GC diagnostic/private string-search valid-boundary controls")
    need(valid["exit"] == 0 and not auth(valid["stderr"]).read_bytes() and
         auth(valid["stdout"]).read_text().splitlines() == expected_valid and
         c["valid_passed"] == len(cases) and c["valid_failed"] == 0,
         "private optional valid-boundary controls failed/incomplete")
    need(report["passed"] is True, "arity reporter failed")
    if link_build:
        campaign = read(OUT / "floors-queued" / ("result-" + tag) / "campaign.json")
        auth(campaign["binary"])
        check_memory_policy(campaign["memory_policy"])
        need(campaign["binary"]["sha256"] == report["binary"]["sha256"] and
             campaign["source"] == source_identity(), "arity report is not the fresh current-source result build")
    return number


def check_arity():
    arity_report(read(OUT / "arity/base/report.json"), "base")
    counts = []
    for tag in LINKAGES:
        n = arity_report(read(OUT / "arity" / tag / "report.json"), tag)
        counts.append((tag, n))
    print("arity PASS " + " ".join(f"{tag}={n}" for tag, n in counts))


def check_funcall(mode, cap):
    mode = {"interp": "interpreter", "byte": "bytecode"}.get(mode, mode)
    need(mode in CAPS and cap == CAPS[mode], "threshold must equal the ledger cap")
    means, result = perf("study", "result-dynamic")
    baseline, base = perf("study", "base02-dynamic")
    need(result["qualified_binary"]["sha256"] != base["qualified_binary"]["sha256"], "baseline and result are the same binary")
    value = means[mode]["funcall"]
    print(f"{mode} funcall {value:.3f} Ir cap={cap:g} base={baseline[mode]['funcall']:.3f}")
    need(value <= cap, "original funcall milestone not reached")


def ir(tag):
    report = read(OUT / "ir" / tag / "report.json")
    auth(report["binary"])
    for pin in report["inputs"]:
        auth(pin)
    need(report["total_includes_GC"] is True, "ABI interval excludes GC")
    rows = report["runs"]
    need(len(rows) == 12 and {(r["mode"], r["layout"]) for r in rows} ==
         {(m, l) for m in ("boot", "files", "regex") for l in LAYOUTS}, "incomplete ABI layouts")
    result = {}
    for row in rows:
        profile(row["profile"], row["ir"])
        auth(row["stdout"])
        auth(row["stderr"])
        result.setdefault(row["mode"], []).append(row)
    if tag.startswith("result-"):
        check_memory_policy(report["memory_policy"])
        need(report["source"] == source_identity(), "ABI result source is stale")
    return result


def check_noregress():
    session = REFERENCE / "performance-summary.json"
    need(sha(session) == SESSION_SHA, "session3a reference changed")
    reference = read(session)
    bars = {(g["linkage"], g["mode"], g["operation"]): g["candidate"]["ir_per_op"]
            for g in reference["gates"] if g["operation"] in ("builtin", "let", "while")}
    gnu, _ = perf("floors", "gnu-capped")
    failures, checked = [], 0
    for linkage in LINKAGES:
        values, report = perf("floors", "result-" + linkage)
        _, base = perf("floors", "base02-" + linkage)
        need(base["qualified_binary"]["sha256"] != report["qualified_binary"]["sha256"], "same floor baseline/result binary")
        for mode in MODES:
            for op in ("builtin", "let", "while"):
                key = linkage, mode, op
                if key not in bars:
                    continue
                actual = values[mode][op]
                checked += 1
                if actual > bars[key]:
                    failures.append(f"{linkage}/{mode}/{op} {actual:.3f}>{bars[key]:.3f}")
        # Report the required GNU ratios without using a changing denominator to
        # loosen the pinned session3a instruction-count bars.
        for mode in ("vm", "bytecode"):
            for op in ("let", "while"):
                need(gnu[mode][op] > 0, "missing GNU denominator")
    baseline, candidate = ir("base00-dynamic"), ir("result-dynamic")
    for mode in ("boot", "files", "regex"):
        checked += 1
        a = statistics.mean(r["ir"] for r in baseline[mode])
        b = statistics.mean(r["ir"] for r in candidate[mode])
        for left, right in zip(sorted(baseline[mode], key=lambda r: r["layout"]), sorted(candidate[mode], key=lambda r: r["layout"])):
            need(auth(left["stdout"]).read_bytes() == auth(right["stdout"]).read_bytes(), "ABI output changed")
        if b > a:
            failures.append(f"dynamic/{mode} {b:.3f}>{a:.3f}")
    print(f"noregress checked={checked} failures={len(failures)}")
    need(not failures, "; ".join(failures))


def check_focused():
    report = read(OUT / "semantics/result/report.json")
    need(report["pass"] and report["source"] == source_identity(), "focused semantics failed/stale")
    check_memory_policy(report["memory_policy"])
    auth(report["gnu"])
    for pin in report["inputs"]:
        auth(pin)
    for linkage in LINKAGES:
        auth(report["binaries"][linkage])
        parity = read(auth(report["callfloor"][linkage]))
        for pin in parity["inputs"]:
            auth(pin)
        for mode in MODES:
            case = parity["modes"][mode]
            need(case["differences"] == 0, "callfloor differs from GNU")
            for engine in ("gnu", "nelisp"):
                row = case[engine]
                need(row["returncode"] == 0 and row["complete"] and row["valid"], "incomplete callfloor")
                raw = auth(row["stdout"]).read_bytes()
                need(not auth(row["stderr"]).read_bytes(), "callfloor stderr")
                if mode == "vm" and engine == "nelisp":
                    need(b"(pcf-nil-body . t)" in raw, "nil-formal target did not execute in VM")
        row = report["env_capture"][linkage]
        need(row["returncode"] == 0 and not auth(row["stderr"]).read_bytes(), "env capture failed")
        need(re.search(rb"ENV-CAPTURE-ROOTS checked=[1-9][0-9]* PASS\n", auth(row["stdout"]).read_bytes()), "env capture marker missing")
    need(set(report["shadow_arity"]) == {e + "/" + m for e in ("gnu", *LINKAGES) for m in ("interpreter", "vm")}, "missing focused shadow cases")
    for mode in ("interpreter", "vm"):
        expected = None
        for engine in ("gnu", *LINKAGES):
            row = report["shadow_arity"][engine + "/" + mode]
            need(row["returncode"] == 0 and not auth(row["stderr"]).read_bytes(), "focused shadow arity failed")
            raw = auth(row["stdout"]).read_bytes()
            lines = [line for line in raw.splitlines() if line.startswith(b"SHADOW-ARITY ")]
            need(len(lines) == 1, "focused shadow marker missing/duplicate")
            if engine == "gnu":
                expected = lines[0]
            else:
                need(lines[0] == expected, "focused shadow error data differs from GNU")
                if mode == "vm":
                    need(b"SHADOW-VM t\n" in raw or b"SHADOW-VM nil\n" in raw,
                         "shadow VM registration coverage receipt missing")
    need(len(report["captures"]) >= 24, "missing capture corpus")
    for key, row in report["captures"].items():
        auth(row["stdout"])
        auth(row["stderr"])
        if not key.startswith("gnu/"):
            need(row["returncode"] == 0 and not auth(row["stderr"]).read_bytes(), "capture failed")
            ref = report["captures"]["gnu/" + key.split("/", 1)[1]]
            if ref["returncode"] not in (124,) and ref["returncode"] >= 0:
                need(auth(row["stdout"]).read_bytes() == auth(ref["stdout"]).read_bytes(), "capture differs from GNU")
    print("focused semantics PASS callfloor=48 capture>=16 env-capture=2")


def check_final():
    check_focused()
    final = read(OUT / "final/report.json")
    need(final["source"] == source_identity(), "full final source is stale")
    for linkage in LINKAGES:
        row = final["checks"][linkage]
        auth(row["stdout"])
        auth(row["stderr"])
        need(row["returncode"] == 0 and row["groups"] == 11 and row["failures"] == 0, "full final checks not green")
    for key in ("standalone", "anvil"):
        row = final[key]
        auth(row["stdout"])
        auth(row["stderr"])
        need(row["returncode"] == 0, key + " failed")
    need(final["anvil"]["tools"] == 40, "anvil tool count differs")
    print("final PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="check", required=True)
    for name in ("arity", "noregress", "focused", "final"):
        sub.add_parser(name)
    p = sub.add_parser("funcall")
    p.add_argument("mode", choices=("interp", "vm", "byte"))
    p.add_argument("cap", type=float)
    args = parser.parse_args()
    try:
        if args.check == "funcall":
            check_funcall(args.mode, args.cap)
        else:
            {"arity": check_arity, "noregress": check_noregress, "focused": check_focused, "final": check_final}[args.check]()
    except (OSError, ValueError, KeyError, TypeError) as error:
        print("FAIL " + str(error))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
