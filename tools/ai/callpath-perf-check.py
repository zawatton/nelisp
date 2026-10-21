#!/usr/bin/env python3
"""Fail-closed meter for call-path safety, measured Ir and final evidence.

The GNU structural-port brief keeps the original S4.2 caps. Missing or stale
measurements and final receipts remain red; a plan is not an implementation.
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
CAPS = {"interpreter": 10230.0, "vm": 5115.0, "bytecode": 1035.0}
GNU_FACTORS = {"interpreter": 10, "vm": 5, "bytecode": 5}
# Original full Doc 219 study / session3a floor fixtures. bytecode.el is
# GNU 31.1's normalized forms, without a source-path-dependent .elc header.
INTENDED_FIXTURES = {
    "study": ("0bb2af9272cb4c3e4a297d3b36c5aff5b90e2bbd27a6de53e3309ee814826370",
              "135e8597a877451058dc4e6f858c33ad665c6a2ce3edbb57bd42868dac11c225"),
    "floors": ("590f1429b281105f6127f8dfd8107105a6dd8e9cc1447e873347bfe06513fe4d",
               "1f90701eadbcf9e59d17953e5f6b96503155d56b58b50540a36355ae39eb5a7d"),
}
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


def fixture_identity(suite, report):
    sources = [pin for pin in report["inputs"] if Path(pin["path"]).name == "functions.el"]
    need(len(sources) == 1, "measurement must pin exactly one functions.el fixture")
    source, compiled = sources[0], report["compiled_program"]
    need(Path(compiled["path"]).name == "bytecode.el" and compiled in report["inputs"],
         "compiled fixture must be the pinned bytecode.el input")
    auth(source)
    auth(compiled)
    identity = source["sha256"], compiled["sha256"]
    need(identity == INTENDED_FIXTURES[suite], "measurement differs from the intended full " + suite + " corpus")
    return report["gnu"]["sha256"], *identity


def same_fixtures(suite, *reports):
    identities = [fixture_identity(suite, report) for report in reports]
    need(len(identities) >= 2 and all(identity == identities[0] for identity in identities[1:]),
         "compared campaigns use different GNU compilers or source/compiled fixtures")


def check_portplan(path=None):
    path = Path(path) if path is not None else ROOT / "docs/design/221-gnu-call-path-port.org"
    text = path.read_text()
    expected = {f"M{number:02d}" for number in range(1, 36)}
    rows = [tuple(cell.strip() for cell in line.strip().strip("|").split("|"))
            for line in text.splitlines() if re.match(r"^\|\s*M\d+\s*\|", line)]
    need(len(rows) == 35 and all(len(row) == 4 for row in rows) and
         {row[0] for row in rows} == expected, "port plan must classify M01-M35 exactly once")
    methods = {"ported": 0, "adapted": 0, "impossible": 0}
    for identity, method, anchor, transfer in rows:
        need(method in methods, identity + ": unknown transfer method")
        methods[method] += 1
        need(re.search(r"\b(?:eval|data|bytecode|alloc)\.c:[1-9]\d*|\blisp\.h:[1-9]\d*", anchor),
             identity + ": missing GNU file:line")
        need(len(transfer) >= 32, identity + ": missing concrete transfer or architectural reason")
        if method == "adapted":
            need(re.search(r"=\w+|WORD|mirror|reserve|arena|specpdl|backtrace|worker|allocation-debt|"
                           r"native-entry metadata|allocator|Executor", transfer),
                 identity + ": adapted transfer must name the NeLisp structure")
        if method == "impossible":
            need(re.search(r"needs|lacks|depends|has no|requires|cannot", transfer, re.I),
                 identity + ": impossible transfer must explain the blocking layer")
    order = text.split("* Implementation order\n", 1)
    need(len(order) == 2, "port plan lacks GNU implementation order")
    steps = re.findall(r"^([1-6])\. (.*?)(?=^[1-6]\. |\nAfter EACH|\n\* |\Z)",
                       order[1], re.M | re.S)
    need([number for number, _ in steps] == list("123456"), "port plan must contain six ordered steps")
    required = (("=Ffuncall=", "=funcall_general="), ("=funcall_subr=", "=apply_lambda="),
                ("=funcall_lambda=",), ("=specbind=", "=unbind_to="),
                ("=exec_byte_code=",), ("Conservative roots", "register", "stack"))
    for (number, body), mechanisms in zip(steps, required):
        need(all(mechanism in body for mechanism in mechanisms), "incorrect GNU mechanism in step " + number)
    need("classifies the transfer method, not completion" in text and
         "remains OPEN" in text, "port plan must distinguish planned transfers from completed ports")
    print("portplan PASS mechanisms=35 steps=6 " + " ".join(f"{key}={value}" for key, value in methods.items()))


def perf(suite, tag, *, current_source=False):
    directory = OUT / (suite + "-queued") / tag
    campaign = read(directory / "campaign.json")
    report = read(directory / "perf.json")
    required = {"control", "while", "funcall"} if suite == "study" else {"control", "while", "builtin", "let"}
    need(campaign["protocol"] == "queued-3-gc-v1", "unqualified measurement protocol")
    need(campaign["suite"] == suite and set(campaign["modes"]) == set(MODES) and
         len(campaign["modes"]) == len(MODES) and set(campaign["layouts"]) == LAYOUTS and
         len(campaign["layouts"]) == len(LAYOUTS) and set(campaign["operations"]) == required and
         len(campaign["operations"]) == len(required), "campaign recipe coverage differs")
    auth(campaign["script"])
    auth(campaign["binary"])
    check_memory_policy(campaign["memory_policy"])
    need(campaign["profile_includes_following_GC"] is True and report["isolated"] is True,
         "measured interval must include GC in an isolated process")
    need(campaign["n"] == report["n"] == (100 if suite == "study" else 300), "iteration mismatch")
    selected = report["qualified_binary"]
    auth(selected)
    need(selected["sha256"] == campaign["binary"]["sha256"] == report["binary"]["sha256"], "binary identity mismatch")
    auth(report["binary"])
    auth(report["gnu"])
    auth(report["compiled_program"])
    for pin in report.get("binary_companions", []) + report["inputs"]:
        auth(pin)
    fixture_identity(suite, report)
    need(len(report["runs"]) == 12 and {(r["mode"], r["layout"]) for r in report["runs"]} ==
         {(m, l) for m in MODES for l in LAYOUTS}, "incomplete/duplicate mode/layout campaign")
    means = {}
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
    if current_source or tag.startswith("result-"):
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
            # The reader prints a builtin callable as GNU prints a subr.
            for callable_ in (name, "#<subr " + name + ">", "#<subr " + name + ">"):
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
    # A fixed-arity builtin (syscall-direct takes exactly 7) has one valid
    # boundary; the others need both their min and their max case.
    fixed = {row[0] for row in read(auth(report["contracts"]))[1] if row[1] == row[2]}
    need(all(sum(case["name"] == name for case in cases) >= (1 if name in fixed else 2)
             for name in required_valid),
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
        # Since the switch to wall-clock (2026-10-21) the measured result
        # build is the wall report's candidate, not an Ir campaign's binary.
        floors = read(WALL / ("result-" + tag) / "floors.json")
        auth(floors["engines"]["candidate"])
        need(floors["engines"]["candidate"]["sha256"] == report["binary"]["sha256"] and
             floors["source"] == source_identity(), "arity report is not the fresh current-source result build")
    return number


def check_arity():
    arity_report(read(OUT / "arity/base/report.json"), "base")
    counts = []
    for tag in LINKAGES:
        n = arity_report(read(OUT / "arity" / tag / "report.json"), tag)
        counts.append((tag, n))
    print("arity PASS " + " ".join(f"{tag}={n}" for tag, n in counts))


def check_funcall(mode, cap, *, result_tag="result-dynamic", baseline_tag="base02-dynamic"):
    mode = {"interp": "interpreter", "byte": "bytecode"}.get(mode, mode)
    need(mode in CAPS and cap == CAPS[mode], "threshold must equal the ledger cap")
    means, result = perf("study", result_tag, current_source=True)
    baseline, base = perf("study", baseline_tag)
    same_fixtures("study", result, base)
    need(result["qualified_binary"]["sha256"] != base["qualified_binary"]["sha256"], "baseline and result are the same binary")
    value = means[mode]["funcall"]
    print(f"{result_tag}/{mode} funcall {value:.3f} Ir cap={cap:g} base={baseline[mode]['funcall']:.3f}")
    need(value <= cap, "original S4.2 funcall cap not reached")


def check_builtin(*, result_prefix="result", baseline_prefix="base02", gnu_tag="gnu-capped",
                  linkages=LINKAGES):
    gnu, reference = perf("floors", gnu_tag)
    need(reference["qualified_binary"]["sha256"] == reference["gnu"]["sha256"],
         "builtin reference is not the GNU executable")
    failures, checked = [], 0
    for linkage in linkages:
        tag = result_prefix + "-" + linkage
        means, result = perf("floors", tag, current_source=True)
        baseline, base = perf("floors", baseline_prefix + "-" + linkage)
        same_fixtures("floors", reference, base, result)
        need(result["qualified_binary"]["sha256"] != base["qualified_binary"]["sha256"],
             "baseline and result are the same binary")
        for mode in MODES:
            value, denominator = means[mode]["builtin"], gnu[mode]["builtin"]
            cap = GNU_FACTORS[mode] * denominator
            need(denominator > 0 and math.isfinite(cap), "missing GNU builtin denominator")
            checked += 1
            print(f"{tag}/{mode} builtin {value:.3f} Ir cap={cap:.3f} "
                  f"GNU={denominator:.3f} factor={GNU_FACTORS[mode]} "
                  f"base={baseline[mode]['builtin']:.3f}")
            if value > cap:
                failures.append(f"{tag}/{mode} {value:.3f}>{cap:.3f}")
    need(checked == len(linkages) * len(MODES) > 0, "empty builtin linkage coverage")
    print(f"builtin checked={checked} failures={len(failures)} linkages={','.join(linkages)}")
    need(not failures, "original S4.2 builtin caps not reached: " + "; ".join(failures))


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
    gnu, gnu_report = perf("floors", "gnu-capped")
    failures, checked = [], 0
    for linkage in LINKAGES:
        values, report = perf("floors", "result-" + linkage)
        _, base = perf("floors", "base02-" + linkage)
        same_fixtures("floors", gnu_report, base, report)
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


def check_focused(tag="result"):
    report = read(OUT / "semantics" / tag / "report.json")
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


WALL = ROOT / "target/callpath-perf/wall"
BASE_BINARIES = {"dynamic": ROOT / "target/callpath-perf/base/nelisp",
                 "static": ROOT / "target/callpath-perf/base/nelisp-static"}
WALL_OPS = {"funcall": "pcf-funcall", "builtin": "pcf-builtin", "let": "pcf-let", "while": "pcf-while"}


def wall(tag, kind="floors"):
    """A wall-clock report (COMMON.md Measurement): 7 alternating pinned rounds of
    candidate, GNU 31.1 and the base02 binary of the same linkage, all runs pinned."""
    report = read(WALL / tag / (kind + ".json"))
    need(report["protocol"] == ("wall-7-alternating-v1" if kind == "floors" else "wall-7-alternating-procs-v1")
         and report["rounds"] == 7, "unqualified wall-clock protocol")
    for pin in report["engines"].values():
        auth(pin)
    auth(report["producer"])
    linkage = "static" if tag.endswith("-static") else "dynamic"
    need(report["engines"]["base"]["sha256"] == sha(BASE_BINARIES[linkage]), "wall baseline is not base02 " + linkage)
    need(report["engines"]["candidate"]["sha256"] != report["engines"]["base"]["sha256"], "candidate is the base binary")
    if kind == "floors":
        # /usr/local/bin/emacs resolves to its versioned executable.
        need(Path(report["engines"]["gnu"]["path"]).name in ("emacs", "emacs-31.1"),
             "GNU reference is not emacs 31.1")
        for pins in report["fixtures"].values():
            for pin in pins:
                auth(pin)
        rows = {(r["engine"], r["suite"], r["mode"], r["round"]) for r in report["runs"]}
        need(len(rows) == len(report["runs"]) == 3 * 2 * 3 * 7, "incomplete wall campaign")
        need(report["medians_us"] == wall_medians(report["runs"]), "wall medians differ from the runs")
        need(report.get("paired_ratios") == wall_paired(report["runs"]), "wall paired ratios differ from the runs")
    if tag.startswith("result-"):
        need(report["source"] == source_identity(), "wall candidate source is stale")
    return report


def wall_medians(runs):
    table = {}
    for row in runs:
        for op, value in row["own_s"].items():
            table.setdefault(row["engine"], {}).setdefault(row["mode"], {}).setdefault(op, []).append(value)
    return {e: {m: {op: statistics.median(v) * 1e6 for op, v in ops.items()} for m, ops in modes.items()}
            for e, modes in table.items()}


def wall_paired(runs):
    """Median over rounds of the per-round candidate/GNU and candidate/base
    ratios (COMMON.md Measurement: clocks vary second to second)."""
    own = {}
    for row in runs:
        for op, value in row["own_s"].items():
            own.setdefault((row["mode"], op, row["round"]), {})[row["engine"]] = value
    ratios = {}
    for (mode, op, _), engines in sorted(own.items()):
        if {"candidate", "gnu", "base"} <= set(engines):
            c, g, b = engines["candidate"], engines["gnu"], engines["base"]
            slot = ratios.setdefault(mode, {}).setdefault(op, {"gnu": [], "base": []})
            slot["gnu"].append(c / g if g > 0 else float("inf"))
            slot["base"].append(c / b if b > 0 else float("inf"))
    return {mode: {op: {"cand_over_gnu": statistics.median(v["gnu"]),
                        "cand_over_base": statistics.median(v["base"]),
                        "rounds": len(v["gnu"])} for op, v in ops.items()}
            for mode, ops in ratios.items()}


def check_wallfuncall(mode, factor, tag="result-dynamic"):
    mode = {"interp": "interpreter", "byte": "bytecode"}.get(mode, mode)
    need(factor == GNU_FACTORS[mode], "factor must equal the ledger multiplier")
    report = wall(tag)
    med, pr = report["medians_us"], report["paired_ratios"][mode]["pcf-funcall"]
    print(f"wall {tag}/{mode} funcall paired cand/GNU={pr['cand_over_gnu']:.2f} cap={factor}x "
          f"cand/base={pr['cand_over_base']:.3f} (medians {med['candidate'][mode]['pcf-funcall']:.4f}us, "
          f"GNU {med['gnu'][mode]['pcf-funcall']:.4f}us)")
    need(pr["cand_over_gnu"] <= factor, "wall-clock funcall cap not reached")


def check_wallbuiltin(prefix="result"):
    failures = []
    for linkage in LINKAGES:
        ratios = wall(prefix + "-" + linkage)["paired_ratios"]
        for mode in MODES:
            ratio = ratios[mode]["pcf-builtin"]["cand_over_gnu"]
            print(f"wall {linkage}/{mode} builtin paired cand/GNU={ratio:.2f} cap={GNU_FACTORS[mode]}x")
            if ratio > GNU_FACTORS[mode]:
                failures.append(f"{linkage}/{mode} {ratio:.2f}x")
    need(not failures, "wall-clock builtin caps not reached: " + "; ".join(failures))


def check_wallnoregress(prefix="result"):
    failures, checked = [], 0
    for linkage in LINKAGES:
        ratios = wall(prefix + "-" + linkage)["paired_ratios"]
        for mode in MODES:
            for op in ("builtin", "let", "while"):
                pr = ratios[mode][WALL_OPS[op]]
                checked += 1
                print(f"wall {linkage}/{mode}/{op} paired cand/base={pr['cand_over_base']:.3f} "
                      f"cand/GNU={pr['cand_over_gnu']:.2f}")
                if pr["cand_over_base"] > 1.03:
                    failures.append(f"{linkage}/{mode}/{op} {pr['cand_over_base']:.3f}")
    procs = wall(prefix + "-dynamic", "procs")
    rounds = {}
    for row in procs["runs"]:
        rounds.setdefault((row["mode"], row["round"]), {})[row["engine"]] = row["seconds"]
    for mode in ("boot", "files", "regex"):
        ratios = [e["candidate"] / e["base"] for (m, _), e in rounds.items()
                  if m == mode and {"candidate", "base"} <= set(e)]
        need(len(ratios) == 7, "incomplete process rounds: " + mode)
        ratio = statistics.median(ratios)
        need(abs(ratio - procs["paired_cand_over_base"][mode]) < 1e-12, "procs paired ratio differs from runs")
        checked += 1
        print(f"wall dynamic/{mode} paired cand/base={ratio:.3f}")
        if ratio > 1.03:
            failures.append(f"dynamic/{mode} {ratio:.3f}")
    print(f"wallnoregress checked={checked} failures={len(failures)}")
    need(not failures, "; ".join(failures))


S63 = ROOT / "target/callpath-perf/s63"
S63_SLACK = 0.005  # a constant few allocations (warm-up, a collection) over the probe's 20000 calls


def s63_tool():
    spec = importlib.util.spec_from_file_location("callpath_perf_s63", ROOT / "tools/ai/callpath-perf-s63.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def s63_result_build():
    """Every stage-S4 report must come from the current source's wall-clock result build."""
    floors = read(WALL / "result-dynamic" / "floors.json")
    need(floors["source"] == source_identity(), "the wall-clock result build is not the current source")
    return auth(floors["engines"]["candidate"]) and floors["engines"]["candidate"]["sha256"]


def check_s63census(kind):
    """S4.1/S4.2: box-census debt on the call path (SCOPE in callpath-perf-s63.py)."""
    tool = s63_tool()
    report = read(S63 / "census.json")
    need(report["source"] == source_identity() and report["stage_source"] == report["source"],
         "the census is not a build of the current source")
    for key in ("binary", "producer"):
        auth(report[key])
    need(report["binary"]["sha256"] == s63_result_build(), "the census build is not the wall-clock result build")
    need(report["scope"] == tool.SCOPE.pattern, "the census scope differs from the checker's")
    counts = tool.census_counts(auth(report["census"]))
    need(counts["units"] >= 100 and counts["scope_defuns"] >= 200, "the census does not cover the call path")
    top = sorted(counts["per_defun"].items(),
                 key=lambda kv: -sum(n for i, n in kv[1].items() if (i in tool.ROOTS) == (kind == "roots")))
    print(f"s63census {kind}={counts[kind]} over {counts['scope_defuns']} call-path defuns; most: " +
          ", ".join(f"{fn}={sum(n for i, n in row.items() if (i in tool.ROOTS) == (kind == 'roots'))}"
                    for fn, row in top[:5]))
    need(counts[kind] == 0, f"{counts[kind]} {kind} sites remain on the call path")
    print(f"s63census {kind} PASS")


def check_s63alloc():
    """S4.3: allocations per call at most GNU 31.1's objects per call, every mode."""
    tool = s63_tool()
    report = read(S63 / "alloc.json")
    need(report["source"] == source_identity(), "the allocation probe is stale")
    for key in ("binary", "gnu", "producer"):
        auth(report[key])
    for row in report["fixture"]:
        auth(row)
    need(report["binary"]["sha256"] == s63_result_build(), "the probe did not run the wall-clock result build")
    need(Path(report["gnu"]["path"]).name in ("emacs", "emacs-31.1"), "GNU reference is not emacs 31.1")
    fails = []
    for mode in tool.MODES:
        row = report["modes"][mode]
        for engine in ("nelisp", "gnu"):
            need(row[engine]["returncode"] == 0, f"{engine} {mode} probe failed")
        mine = tool.per_call(row["nelisp"]["raw"], "nelisp", report["n"])
        gnu = tool.per_call(row["gnu"]["raw"], "gnu", report["n"])
        need(mine["control"] >= 1 and gnu["control"] >= 1, f"{mode}: a counter missed the control's allocations")
        need(not any(gnu["vector_cells"].values()), f"{mode}: GNU allocated vectors, which the object count omits")
        for op in tool.OPS:
            ok = mine[op] <= gnu[op] + S63_SLACK
            print(f"s63alloc {mode} {op} nelisp={mine[op]:.3f} gnu={gnu[op]:.3f}{'' if ok else ' OVER'}")
            if not ok:
                fails.append(f"{mode}/{op}")
    need(not fails, "allocations per call above GNU: " + ", ".join(fails))
    print("s63alloc PASS")


def check_s63e2e():
    """S4.4: the final verification and the wall-clock regression guard on the current source."""
    check_final()
    check_wallnoregress()
    print("s63e2e PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="check", required=True)
    for name in ("portplan", "arity", "noregress", "final", "wallbuiltin", "wallnoregress", "s63alloc", "s63e2e"):
        sub.add_parser(name)
    p = sub.add_parser("s63census")
    p.add_argument("kind", choices=("roots", "materialisers"))
    p = sub.add_parser("wallfuncall")
    p.add_argument("mode", choices=("interp", "vm", "byte"))
    p.add_argument("factor", type=float)
    p = sub.add_parser("focused")
    p.add_argument("--tag", default="result", help="focused receipt tag; current-source checks remain mandatory")
    p = sub.add_parser("funcall")
    p.add_argument("mode", choices=("interp", "vm", "byte"))
    p.add_argument("cap", type=float)
    p.add_argument("--result-tag", default="result-dynamic", help="current-source intermediate or final receipt tag")
    p.add_argument("--baseline-tag", default="base02-dynamic")
    p = sub.add_parser("builtin")
    p.add_argument("--result-prefix", default="result", help="current-source receipt prefix, before -dynamic/-static")
    p.add_argument("--baseline-prefix", default="base02")
    p.add_argument("--gnu-tag", default="gnu-capped")
    p.add_argument("--linkage", choices=LINKAGES, action="append", help="intermediate coverage; default checks both linkages")
    args = parser.parse_args()
    try:
        if args.check == "wallfuncall":
            check_wallfuncall(args.mode, args.factor)
        elif args.check == "wallbuiltin":
            check_wallbuiltin()
        elif args.check == "wallnoregress":
            check_wallnoregress()
        elif args.check == "funcall":
            check_funcall(args.mode, args.cap, result_tag=args.result_tag, baseline_tag=args.baseline_tag)
        elif args.check == "builtin":
            need(args.linkage is None or len(args.linkage) == len(set(args.linkage)), "duplicate builtin linkages")
            check_builtin(result_prefix=args.result_prefix, baseline_prefix=args.baseline_prefix,
                          gnu_tag=args.gnu_tag, linkages=args.linkage or LINKAGES)
        elif args.check == "focused":
            check_focused(args.tag)
        elif args.check == "s63census":
            check_s63census(args.kind)
        else:
            {"portplan": check_portplan, "arity": check_arity,
             "noregress": check_noregress, "final": check_final,
             "s63alloc": check_s63alloc, "s63e2e": check_s63e2e}[args.check]()
    except (OSError, ValueError, KeyError, TypeError) as error:
        print("FAIL " + str(error))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
