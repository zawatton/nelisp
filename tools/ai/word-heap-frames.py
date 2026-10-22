#!/usr/bin/env python3
"""Doc 216 S4.2: replay exact parity, actual frame counters and callfloor Ir.

capture --tag TAG --dynamic BIN --static BIN [--source-root DIR]
performance --tag TAG --dynamic BIN --static BIN [--jobs 4]
check [--tag candidate] [--group provenance|source|parity|witness|floor]
controls [--tag candidate]
performance-controls [--tag candidate]

All generated inputs, process outputs and receipts stay under
target/wh-s4f/frames. Binary paths and GNU are arguments or environment values.
GNU crashes/hangs are retained as defects; successful cases are never excluded.
No cached PASS field is accepted. Performance uses GC-isolated in-process
callfloor intervals and independently replays its authenticated raw profiles.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

ROOT = next(p for p in Path(__file__).resolve().parents
            if (p / "AI.md").is_file() and (p / "tools/ai").is_dir())
OUT = ROOT / "target/wh-s4f/frames"
PROBE = Path(__file__).with_name("word-heap-frames-parity.el")
CASES = ("loop-capture", "capture-sharing", "backtrace-frame",
         "backtrace-locals", "nonlocal", "recursion", "threads", "public-threads", "legacy-threads")
THREAD_PROVIDER = ROOT / "packages/nelisp-emacs-foundation/src"
THREAD_FILES = ("emacs-cc-thread-1.el", "emacs-cc-thread-predicates-1.el", "emacs-cc-thread-2.el")
LINKAGES = ("dynamic", "static")
GROUPS = ("provenance", "source", "parity", "witness", "floor")
WITNESS_FIELDS = ("pushes", "stack_pushes", "heap_frames", "promotions",
                  "live_depth", "highwater")


def need(condition, message):
    if not condition:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": sha(path), "bytes": path.stat().st_size}


def verify(row):
    need(pin(row["path"]) == row, "changed/missing input: " + row["path"])
    return Path(row["path"])


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2) + "\n")


RECEIPT_ENV = ("WH_FRAME_CASE", "WH_FRAME_THREAD_PROVIDER", "NELISP_FX_DISABLE",
               "WHVS_SOURCE_ROOT", "WHVS_ROWS")


def selected_env(**updates):
    result = dict(os.environ)
    for name in RECEIPT_ENV:
        result.pop(name, None)
    result.update(updates)
    return result


def run_raw(directory, name, command, env=None, timeout=60):
    directory.mkdir(parents=True, exist_ok=True)
    env = selected_env() if env is None else env
    started = time.monotonic()
    try:
        process = subprocess.run(command, cwd=ROOT, env=env,
                                 capture_output=True, timeout=timeout)
        stdout, stderr, code = process.stdout, process.stderr, process.returncode
    except subprocess.TimeoutExpired as error:
        stdout, stderr, code = error.stdout or b"", error.stderr or b"", 124
    paths = {key: directory / (name + suffix) for key, suffix in
             (("stdout", ".out"), ("stderr", ".err"), ("status", ".rc"))}
    paths["stdout"].write_bytes(stdout)
    paths["stderr"].write_bytes(stderr)
    paths["status"].write_text(str(code) + "\n")
    return {"command": command, "environment": {name: env[name] for name in RECEIPT_ENV if name in env},
            "returncode": code,
            "seconds": time.monotonic() - started,
            **{key: pin(value) for key, value in paths.items()}}


def replay_run(row, allow_failure=False):
    output = verify(row["stdout"]).read_bytes()
    stderr = verify(row["stderr"]).read_bytes()
    code = int(verify(row["status"]).read_text())
    need(code == row["returncode"], "process status disagrees with raw receipt")
    if not allow_failure:
        need(code == 0 and not stderr,
             f"process failed: rc={code}, stderr={stderr[:180]!r}")
    return output, stderr, code


def load_tool(name, filename):
    spec = importlib.util.spec_from_file_location(name, ROOT / "tools/ai" / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def source_rows(root):
    files = [p for directory in ("src", "lisp", "scripts")
             for p in sorted((root / directory).glob("*.el"))]
    files.append(root / "vendor/staged-emacs-lisp/subr.el")
    need(len(files) > 400, "incomplete build-source tree")
    return [[str(path.relative_to(root)), sha(path)] for path in files]


def capture(args):
    directory = OUT / args.tag
    need(not (directory / "evidence.json").exists(), "use a fresh capture tag")
    directory.mkdir(parents=True, exist_ok=True)
    source_root = Path(args.source_root).resolve()
    rows = source_rows(source_root)
    write_json(directory / "source-rows.json", rows)
    helper = load_tool("whf_source", "word-heap-vectorlike-source.py")
    host_source = directory / "guard.el"
    host_source.write_bytes(helper.HOST_SOURCE)
    gnu = str(Path(args.gnu).resolve())
    env = selected_env(WHVS_SOURCE_ROOT=str(source_root),
               WHVS_ROWS=str(directory / "source-rows.json"))
    host_guard = run_raw(directory, "host-guard", [gnu, "-Q", "--batch", "--load", str(host_source)], env)
    report = {"tag": args.tag, "performance_tag": args.performance_tag or args.tag,
              "gnu": pin(gnu), "probe": pin(PROBE),
              "checker": pin(__file__), "thread_providers": [pin(THREAD_PROVIDER / name) for name in THREAD_FILES],
              "source_root": str(source_root),
              "source_rows": pin(directory / "source-rows.json"),
              "guard_source": pin(host_source), "host_guard": host_guard,
              "tools": [pin(helper.__file__),
                        *[pin(ROOT / "tools/ai" / name) for name in
                          ("prim-callfloor-check.py", "prim-callfloor-perf.py", "word-heap-frames-ir.py",
                           "word-heap-frames-ir-probe.el", "word-heap-frames-ir-client.c")]],
              "binaries": {}, "runs": {}}
    report["gnu_version"] = run_raw(directory, "gnu-version", [gnu, "-Q", "--batch", "--eval", "(princ emacs-version)"])
    for case in CASES:
        result = run_raw(directory / "gnu", case,
                         [gnu, "-Q", "--batch", "--load", str(PROBE)],
                         selected_env(WH_FRAME_CASE=case, WH_FRAME_THREAD_PROVIDER=str(THREAD_PROVIDER)))
        report["runs"]["gnu/" + case] = result
        if result["returncode"] < 0 or result["returncode"] == 124:
            defect = OUT / "gnu-defects" / args.tag / case
            defect.mkdir(parents=True, exist_ok=True)
            shutil.copy2(PROBE, defect / "input.el")
            write_json(defect / "invocation.json", {"case": case, **result})
    query = directory / "query-guard.el"
    query.write_text('(princ "WH-GUARD ")\n(princ nelisp-native-cache--build-source-identity)\n(terpri)\n')
    report["query_guard"] = pin(query)
    for linkage in LINKAGES:
        binary = Path(getattr(args, linkage)).resolve()
        companions = [Path(str(binary) + suffix) for suffix in (".cold", ".native-startup.el")]
        report["binaries"][linkage] = {"binary": pin(binary), "companions": [pin(p) for p in companions]}
        report["runs"][linkage + "/guard"] = run_raw(directory / linkage, "guard", [str(binary), "--load", str(query), "--eval", "nil"])
        for case in (*CASES, "witness"):
            report["runs"][linkage + "/" + case] = run_raw(
                directory / linkage, case, [str(binary), "--load", str(PROBE)],
                selected_env(WH_FRAME_CASE=case, WH_FRAME_THREAD_PROVIDER=str(THREAD_PROVIDER), NELISP_FX_DISABLE="1"))
    write_json(directory / "evidence.json", report)
    print(str(directory / "evidence.json"))


def performance(args):
    need(shutil.disk_usage(ROOT).free >= 50_000_000_000, "less than 50 GB free")
    module = load_tool("whf_isolated", "word-heap-frames-ir.py")
    module.OUT = OUT / args.tag / "performance"
    module.OUT.mkdir(parents=True, exist_ok=True)
    for linkage, binary in (("gnu", args.gnu), ("dynamic", args.dynamic), ("static", args.static)):
        module.measure(binary, linkage, args.gnu, n=args.n, jobs=args.jobs, emacs_include=args.emacs_include)


def authenticate_runs(report):
    """Bind every raw receipt to its selected executable, input and case."""
    gnu, probe = report["gnu"]["path"], report["probe"]["path"]
    directory = OUT / report["tag"]
    provider = str(THREAD_PROVIDER)
    def require_run(row, command, environment):
        need(row.get("command") == command, "receipt command is not the selected executable/input")
        need(row.get("environment") == environment, "receipt environment/case is not the selected input")
    require_run(report["gnu_version"], [gnu, "-Q", "--batch", "--eval", "(princ emacs-version)"], {})
    require_run(report["host_guard"], [gnu, "-Q", "--batch", "--load", report["guard_source"]["path"]],
                {"WHVS_SOURCE_ROOT": report["source_root"], "WHVS_ROWS": report["source_rows"]["path"]})
    for case in CASES:
        require_run(report["runs"]["gnu/" + case], [gnu, "-Q", "--batch", "--load", probe],
                    {"WH_FRAME_CASE": case, "WH_FRAME_THREAD_PROVIDER": provider})
    for linkage in LINKAGES:
        binary = report["binaries"][linkage]["binary"]["path"]
        require_run(report["runs"][linkage + "/guard"],
                    [binary, "--load", report["query_guard"]["path"], "--eval", "nil"], {})
        for case in (*CASES, "witness"):
            require_run(report["runs"][linkage + "/" + case], [binary, "--load", probe],
                        {"WH_FRAME_CASE": case, "WH_FRAME_THREAD_PROVIDER": provider, "NELISP_FX_DISABLE": "1"})


def provenance(report):
    authenticate_runs(report)
    verify(report["checker"])
    verify(report["probe"])
    need([verify(row) for row in report["thread_providers"]] ==
         [path.resolve() for path in (THREAD_PROVIDER / name for name in THREAD_FILES)],
         "production thread provider set changed")
    for tool in report["tools"]:
        verify(tool)
    version, _, _ = replay_run(report["gnu_version"])
    need(version == b"31.1", "GNU reference must be 31.1")
    verify(report["gnu"])
    rows = json.loads(verify(report["source_rows"]).read_text())
    need(rows == source_rows(Path(report["source_root"])), "build sources changed")
    verify(report["guard_source"])
    verify(report["query_guard"])
    host = json.loads(replay_run(report["host_guard"])[0])
    need(host["guard"] == host["listed"], "source list differs from build guard")
    for linkage in LINKAGES:
        info = report["binaries"][linkage]
        verify(info["binary"])
        for item in info["companions"]:
            verify(item)
        output, _, _ = replay_run(report["runs"][linkage + "/guard"])
        need(output.decode().splitlines() == ["WH-GUARD " + host["guard"]],
             linkage + " binary does not contain this build-source identity")
    need(report["binaries"]["dynamic"]["binary"]["sha256"] !=
         report["binaries"]["static"]["binary"]["sha256"], "linkages use the same executable")


def clean_elisp(text):
    # Retain identifiers/parens while removing comments and string contents.
    return re.sub(r'"(?:\\.|[^"\\])*"|;[^\n]*', ' ', text)


def function_body(text, name):
    source = clean_elisp(text)
    start = re.search(r"\(defun\s+" + re.escape(name) + r"(?=\s|\()", source)
    need(start is not None, "missing runtime helper: " + name)
    depth = 0
    for i in range(start.start(), len(source)):
        depth += (source[i] == "(") - (source[i] == ")")
        if depth == 0:
            return source[start.start():i + 1]
    raise ValueError("unbalanced runtime helper: " + name)


def structural(report):
    root = Path(report["source_root"])
    push = function_body((root / "lisp/nelisp-cc-frame-push.el").read_text(), "nelisp_frame_push_direct")
    runtime = clean_elisp((root / "scripts/nelisp-standalone-build.el").read_text())
    calls = runtime + push + clean_elisp((root / "lisp/nelisp-cc-frame-pop.el").read_text())
    calls += clean_elisp((root / "lisp/nelisp-cc-frame-stack-find.el").read_text())
    need(not re.search(r"\((?:record-make|nl_alloc_record)\b", push),
         "uncaptured frame push still constructs a persistent heap record")
    helpers = ("push", "pop", "promote", "mark", "rewrite")
    for suffix in helpers:
        name = "nl_frame_stack_" + suffix
        function_body(runtime, name)
        need(len(re.findall(r"\(" + name + r"(?=\s|\))", calls)) >= 1,
             "frame-stack helper has no call site: " + name)
    need("nl_frame_stack_push" in push, "ordinary frame push bypasses stack helper")
    adapter = function_body(runtime, "nl_native_frame_v2")
    need(not re.search(r"\(record-make\s+\(data-addr\s+nl_frame_push_sym0\)", adapter),
         "native frame adapter still constructs uncaptured heap frames")
    need("nl_frame_stack_install_captured" in adapter,
         "native callback frames bypass captured-frame installation")
    exported = function_body(runtime, "bf_env_capture_roots_w")
    need("nl_frame_stack_snapshot" in exported, "exported roots expose transient stack metadata")
    inheritance = function_body(runtime, "nl_make_thread_env")
    need("nl_frame_stack_inherit" in inheritance,
         "legacy thread evaluator still shares the parent frame stack")
    for caller, callee in (("nl_gc_mark_recorded_contexts", "nl_frame_stack_mark"),
                           ("nl_compact_rw_roots", "nl_frame_stack_rewrite"),
                           ("nl_thread_run", "nl_frame_stack_retire_env")):
        need(callee in function_body(runtime, caller), "missing active hook: " + caller)
    init = function_body(runtime, "nl_thread_private_frames_init")
    need(re.search(r"\(record-make\s+type_slot\s+3\s+\(\+\s+env\s+32\)\)", init),
         "registered workers do not construct private descriptor-capable stacks")
    # Runtime counters below are mandatory: source names alone are not proof.


def completed(output, case):
    lines = output.decode().splitlines()
    need(len(lines) == 2 and lines[1] == "WH-FRAME-DONE 1" and
         lines[0].startswith("WH-FRAME " + case + " "), "missing, extra or incomplete parity output: " + case)
    return lines[0]


def parity(report, authenticate=True):
    if authenticate:
        authenticate_runs(report)
    expected_keys = {engine + "/" + case for engine in ("gnu", *LINKAGES) for case in CASES}
    need(expected_keys <= report["runs"].keys(), "missing parity cases")
    for case in CASES:
        reference = None
        raw, _, code = replay_run(report["runs"]["gnu/" + case], allow_failure=True)
        defect = code < 0 or code == 124
        if not defect:
            replay_run(report["runs"]["gnu/" + case])
            reference = completed(raw, case)
        for linkage in LINKAGES:
            output, _, _ = replay_run(report["runs"][linkage + "/" + case])
            actual = completed(output, case)
            need(defect or actual == reference,
                 f"{linkage} {case} differs from GNU: {actual!r} != {reference!r}")
        if defect:
            print("GNU defect retained, candidates completed: " + case)


def witness_values(output):
    row = completed(output, "witness")
    values = re.findall(r"\[([0-9 ]+)\]", row)
    need(len(values) == 3, "witness must contain three six-counter vectors")
    snapshots = [[int(value) for value in item.split()] for item in values]
    need(all(len(item) == len(WITNESS_FIELDS) for item in snapshots), "invalid witness counter shape")
    need(row == "WH-FRAME witness (" + " ".join("[" + " ".join(map(str, item)) + "]" for item in snapshots) + ")",
         "unexpected witness data")
    before, after, captured = snapshots
    need(all(after[i] >= before[i] and captured[i] >= after[i] for i in (0, 1, 2, 3, 5)), "counters decreased")
    count = after[0] - before[0]
    need(count >= 200 and after[1] - before[1] == count,
         "uncaptured calls did not all push stack frames")
    need(after[2] == before[2] and after[3] == before[3],
         "uncaptured calls allocated/promoted heap frames")
    need(captured[3] > after[3], "escaping closures did not promote a frame")
    need(captured[2] - after[2] == captured[3] - after[3],
         "heap frame allocations differ from captured-frame promotions")
    need(before[4] == after[4] == captured[4], "frame depth was not restored")
    return dict(zip(WITNESS_FIELDS, [captured[i] - before[i] for i in range(6)]))


def witness(report):
    authenticate_runs(report)
    for linkage in LINKAGES:
        values = witness_values(replay_run(report["runs"][linkage + "/witness"])[0])
        print(linkage + " frame counters: " + json.dumps(values, sort_keys=True))


def ratios_from_costs(costs, gnu):
    ratios, failures = {}, []
    for mode in ("interpreter", "vm", "bytecode"):
        ratios[mode] = {}
        for operation in ("let", "while", "funcall", "builtin"):
            reference = gnu[mode][operation]["ir_per_op"]
            value = costs[mode][operation]["ir_per_op"]
            need(math.isfinite(reference) and reference > 0 and math.isfinite(value) and value >= 0,
                 "invalid/negative performance differential")
            ratio = value / reference
            ratios[mode][operation] = ratio
            if mode != "interpreter" and operation in ("let", "while") and ratio > 5:
                failures.append(f"{mode} {operation} {ratio:.4f}x > 5x GNU")
    return ratios, failures


def same_companions(measured, captured):
    # Source capture additionally records byte counts, verified by provenance().
    return measured == [{key: item[key] for key in ("path", "sha256")} for item in captured]


def floor(report):
    module = load_tool("whf_callfloor", "word-heap-frames-ir.py")
    performance_tag = report.get("performance_tag", report["tag"])
    need(isinstance(performance_tag, str) and bool(re.fullmatch(r"[A-Za-z0-9_-]+", performance_tag)),
         "invalid performance tag")
    module.OUT = OUT / performance_tag / "performance"
    gnu_report, gnu, _ = module.perf("gnu")
    need(gnu_report["qualified_binary"] == module.pin(report["gnu"]["path"]), "performance used a different GNU")
    ratios, failures = {}, []
    for linkage in LINKAGES:
        measured, costs, _ = module.perf(linkage)
        need(measured.get("isolated") and measured["n"] == gnu_report["n"], "performance trials are not matched/isolated")
        need(measured["qualified_binary"] == module.pin(report["binaries"][linkage]["binary"]["path"]),
             linkage + " performance used another candidate")
        need(same_companions(measured["binary_companions"], report["binaries"][linkage]["companions"]),
             linkage + " performance used different binary companions")
        ratios[linkage], failed = ratios_from_costs(costs, gnu)
        failures.extend(linkage + " " + message for message in failed)
    print("callfloor ratios: " + json.dumps(ratios, sort_keys=True))
    need(not failures, "; ".join(failures))


def performance_controls(args):
    module = load_tool("whf_ir_test", "../../test/word-heap-frames-ir-test.py")
    module.run(OUT / args.tag / "performance-controls.json")


def check(args):
    report = json.loads((OUT / args.tag / "evidence.json").read_text())
    failures = []
    for group in ([args.group] if args.group else GROUPS):
        try:
            globals()[group if group != "source" else "structural"](report)
            print("S4.2 " + group + " PASS")
        except (ValueError, OSError, KeyError, TypeError, StopIteration) as error:
            failures.append(group)
            print("S4.2 " + group + " FAIL: " + str(error))
    need(not failures, "failed groups: " + ", ".join(failures))


def controls(args):
    performance_controls(args)
    # Deliberately broken real counter rows must fail before any green is trusted.
    good = b"WH-FRAME witness ([10 10 0 0 4 8] [212 212 0 0 4 8] [220 220 1 1 4 8])\nWH-FRAME-DONE 1\n"
    witness_values(good)
    corrupt = {
        "heap-on-uncaptured": good.replace(b"212 212 0 0", b"212 212 1 0"),
        "no-stack-pushes": good.replace(b"212 212 0 0", b"212 10 0 0"),
        "no-promotion": good.replace(b"220 220 1 1", b"220 220 0 0"),
        "depth-leak": good.replace(b"220 220 1 1 4", b"220 220 1 1 5"),
        "missing-completion": good.replace(b"WH-FRAME-DONE 1\n", b""),
        "extra-output": good + b"unexpected\n",
    }
    for name, data in corrupt.items():
        try:
            witness_values(data)
        except ValueError:
            continue
        raise ValueError("negative control accepted: " + name)
    rejected = list(corrupt)
    reference = {mode: {op: {"ir_per_op": 1.0} for op in ("let", "while", "funcall", "builtin")}
                 for mode in ("interpreter", "vm", "bytecode")}
    boundary = copy.deepcopy(reference)
    for mode in boundary:
        for op in boundary[mode]:
            boundary[mode][op]["ir_per_op"] = 5.0
    need(not ratios_from_costs(boundary, reference)[1], "exact 5x boundary was rejected")
    for mode in ("vm", "bytecode"):
        for op in ("let", "while"):
            broken = copy.deepcopy(boundary)
            broken[mode][op]["ir_per_op"] = 5.000000001
            need(bool(ratios_from_costs(broken, reference)[1]), "5x cap mutation accepted")
            rejected.append("cap-" + mode + "-" + op)
    for number in (float("nan"), float("inf"), -1.0):
        broken = copy.deepcopy(boundary)
        broken["vm"]["while"]["ir_per_op"] = number
        try:
            ratios_from_costs(broken, reference)
        except ValueError:
            rejected.append("invalid-cost-" + str(number))
            continue
        raise ValueError("invalid cost accepted")
    evidence_path = OUT / args.tag / "evidence.json"
    if evidence_path.exists():
        actual = json.loads(evidence_path.read_text())
        provenance(actual)
        for name in ("wrong-binary", "shifted-case", "missing-case-environment"):
            substituted = copy.deepcopy(actual)
            if name == "wrong-binary":
                substituted["runs"]["dynamic/threads"] = copy.deepcopy(actual["runs"]["gnu/threads"])
            elif name == "shifted-case":
                substituted["runs"]["dynamic/threads"] = copy.deepcopy(actual["runs"]["dynamic/public-threads"])
            else:
                del substituted["runs"]["dynamic/threads"]["environment"]["WH_FRAME_CASE"]
            try:
                provenance(substituted)
            except ValueError:
                rejected.append(name)
            else:
                raise ValueError("unauthenticated candidate receipt accepted: " + name)
        fixture = {"runs": {}}
        for case in CASES:
            row = actual["runs"]["gnu/" + case]
            replay_run(row)
            for engine in ("gnu", *LINKAGES):
                fixture["runs"][engine + "/" + case] = copy.deepcopy(row)
        parity(fixture, authenticate=False)
        broken = copy.deepcopy(fixture)
        del broken["runs"]["static/threads"]
        try:
            parity(broken, authenticate=False)
        except ValueError:
            rejected.append("missing-parity-case")
        else:
            raise ValueError("missing parity case accepted")
        directory = OUT / args.tag / "control-raw"
        directory.mkdir(parents=True, exist_ok=True)
        for name, key, suffix, data in (
                ("changed-value", "stdout", ".out", b"WH-FRAME backtrace-locals ((changed))\nWH-FRAME-DONE 1\n"),
                ("successful-stderr", "stderr", ".err", b"unexpected stderr\n"),
                ("nonzero-process", "status", ".rc", b"1\n")):
            path = directory / (name + suffix)
            path.write_bytes(data)
            broken = copy.deepcopy(fixture)
            row = broken["runs"]["dynamic/backtrace-locals"]
            row[key] = pin(path)
            if key == "status":
                row["returncode"] = 1
            try:
                parity(broken, authenticate=False)
            except ValueError:
                rejected.append(name)
            else:
                raise ValueError("raw parity mutation accepted: " + name)
    write_json(OUT / args.tag / "controls.json", {"checker": pin(__file__), "rejected": rejected})
    print(f"S4.2 controls PASS: {len(rejected)} broken inputs rejected")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("capture", "performance", "check", "controls", "performance-controls"))
    parser.add_argument("--tag", default="candidate")
    parser.add_argument("--performance-tag", help="capture against an existing performance campaign")
    parser.add_argument("--dynamic", default=os.getenv("WH_FRAME_DYNAMIC"))
    parser.add_argument("--static", default=os.getenv("WH_FRAME_STATIC"))
    parser.add_argument("--gnu", default=os.getenv("WH_FRAME_GNU", shutil.which("emacs")))
    parser.add_argument("--source-root", default=str(ROOT))
    parser.add_argument("--group", choices=GROUPS)
    parser.add_argument("--n", type=int, default=300)
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--emacs-include", default=os.getenv("PCF_EMACS_INCLUDE"))
    args = parser.parse_args()
    need(bool(re.fullmatch(r"[A-Za-z0-9_-]+", args.tag)), "invalid evidence tag")
    if args.performance_tag is not None:
        need(args.command == "capture" and bool(re.fullmatch(r"[A-Za-z0-9_-]+", args.performance_tag)),
             "performance tag requires capture and a valid evidence tag")
    if args.command in ("capture", "performance"):
        need(args.dynamic and args.static and args.gnu, "supply both binaries and GNU through flags or WH_FRAME_* environment")
    globals()[args.command.replace("-", "_")](args)


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, KeyError, TypeError) as error:
        print("S4.2 FAIL: " + str(error), file=sys.stderr)
        sys.exit(1)
