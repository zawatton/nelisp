#!/usr/bin/env python3
"""S4.2 verifier controls, including immutable real negative/overcap profiles."""
import copy
import importlib.util
import json
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parents[1]


def load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


M = load("wh_frame_ir", ROOT / "tools/ai/word-heap-frames-ir.py")
F = load("wh_frames", ROOT / "tools/ai/word-heap-frames.py")


def legacy_rows(path, mode, layout):
    report = json.loads(path.read_text())
    run = next(r for r in report["runs"] if r["mode"] == mode and r["layout"] == layout)
    rows = []
    for row in run["intervals"]:
        raw = M.read_raw(row["profile"])
        ir = int(re.search(r"^summary: (\d+)$", raw, re.M)[1])
        M.need(ir == row["ir"], "historical raw summary differs")
        rows.append({"op": row["op"], "ir": ir, "seconds": row["seconds"]})
    return report, rows


def run(output=None):
    rejected = []
    def rejects(name, call, reason):
        try:
            call()
        except ValueError as error:
            M.need(reason in str(error), name + " failed for wrong reason: " + str(error))
            rejected.append(name)
            return
        raise ValueError("negative control accepted: " + name)

    symbols = {"entry": "forced_gc", "body": "gc_body"}
    raw = ("part: 2\npid: 42\ncmd: selected-binary --load selected-input\n"
           "desc: Trigger: Client Request: PCF-START\nsummary: 123\n"
           "cfn=forced_gc\ncalls=1 0\n0 10\ncfn=gc_body\ncalls=1 0\n0 20\n")
    M.boundary(raw, "PCF-START", symbols, 2, 42)
    rejects("missing-start", lambda: M.boundary(raw.replace("PCF-START", "wrong"), "PCF-START", symbols, 2, 42), "missing PCF-START")
    rejects("omitted-preparation-gc", lambda: M.boundary(raw.replace("calls=1", "calls=0"), "PCF-START", symbols, 2, 42), "missing executed preparation GC")
    rejects("dictionary-only-gc", lambda: M.boundary(raw.replace("0 20", "0 0"), "PCF-START", symbols, 2, 42), "missing executed preparation GC")
    rejects("wrong-part", lambda: M.boundary(raw, "PCF-START", symbols, 4, 42), "wrong bracket ordering")
    rejects("other-process", lambda: M.boundary(raw, "PCF-START", symbols, 2, 43), "wrong bracket process")
    rejects("zero-instructions", lambda: M.boundary(raw.replace("summary: 123", "summary: 0"), "PCF-START", symbols, 2, 42), "invalid instruction count")
    rejects("missing-end", lambda: M.boundary(raw, "PCF-END", symbols, 3, 42), "missing PCF-END")

    saved = []
    history = ROOT / "target/wh-s4f/frames/final3/performance"
    for runtime, mode, layout, op in (("dynamic", "interpreter", 0, "while"),
                                     ("static", "interpreter", 700, "builtin"),
                                     ("static", "interpreter", 1400, "while")):
        path = history / runtime / "perf.json"
        report, rows = legacy_rows(path, mode, layout)
        rejects(f"genuine-negative-{runtime}-{layout}-{op}", lambda rows=rows: M.costs(rows, report["n"]), "nonpositive acceptance measurement")
        row = next(r for r in rows if r["op"] == op)
        base = rows[0] if op == "while" else next(r for r in rows if r["op"] == "while")
        saved.append({"report": M.pin(path), "mode": mode, "layout": layout, "op": op,
                      "ir_per_op": (row["ir"] - base["ir"]) / report["n"]})

    # A real historical candidate's bytecode LET failed the original 5x cap.
    base = ROOT / "target/wh-s4f/frames/preopt-floor/performance"
    measured, reference = [], []
    for layout in M.LAYOUTS:
        for tag, into in (("actual5", measured), ("gnu", reference)):
            report, rows = legacy_rows(base / tag / "perf.json", "bytecode", layout)
            let = next(r for r in rows if r["op"] == "let")
            empty = next(r for r in rows if r["op"] == "while")
            into.append((let["ir"] - empty["ir"]) / report["n"])
    template = {mode: {op: {"ir_per_op": 1.0} for op in ("let", "while", "funcall", "builtin")} for mode in M.MODES}
    candidate, gnu = copy.deepcopy(template), copy.deepcopy(template)
    candidate["bytecode"]["let"]["ir_per_op"] = sum(measured) / 4
    gnu["bytecode"]["let"]["ir_per_op"] = sum(reference) / 4
    ratios, failures = F.ratios_from_costs(candidate, gnu)
    M.need(bool(failures) and ratios["bytecode"]["let"] > 5, "genuine overcap accepted")
    rejected.append("genuine-actual5-bytecode-let-overcap")
    for mode in ("vm", "bytecode"):
        for op in ("let", "while"):
            costs = copy.deepcopy(template)
            costs[mode][op]["ir_per_op"] = 5.0
            M.need(not F.ratios_from_costs(costs, template)[1], "exact 5x cap rejected")
            costs[mode][op]["ir_per_op"] = 5.000000001
            M.need(bool(F.ratios_from_costs(costs, template)[1]), "strict 5x cap weakened")
            rejected.append(f"strict-cap-{mode}-{op}")
    for value in (float("nan"), float("inf"), -1.0):
        bad = copy.deepcopy(template)
        bad["vm"]["while"]["ir_per_op"] = value
        rejects("bad-cost-" + str(value), lambda: F.ratios_from_costs(bad, template), "invalid/negative performance differential")
    rejects("performance-tag-traversal", lambda: F.floor({"tag": "unused", "performance_tag": "../final4"}),
            "invalid performance tag")
    # Exercise the source/profiler schema adapter using actual final build pins.
    final = ROOT / "target/wh-s4f/frames/final4"
    if (final / "evidence.json").exists() and (final / "performance/dynamic/perf.json").exists():
        captured = json.loads((final / "evidence.json").read_text())["binaries"]["dynamic"]["companions"]
        measured = json.loads((final / "performance/dynamic/perf.json").read_text())["binary_companions"]
        for item in captured:
            F.verify(item)
        M.need(F.same_companions(measured, captured), "authentic companion schemas disagree")
        variants = {"omitted": measured[:-1], "reordered": measured[::-1]}
        for key in ("path", "sha256"):
            variant = copy.deepcopy(measured)
            variant[0][key] += "stale"
            variants[key] = variant
        for name, variant in variants.items():
            M.need(not F.same_companions(variant, captured), "companion adapter accepted " + name)
            rejected.append("live-companion-adapter-" + name)
        wrong_size = dict(captured[0], bytes=captured[0]["bytes"] + 1)
        rejects("live-companion-source-size", lambda: F.verify(wrong_size), "changed/missing input")
    # Use actual labeled profiles for provenance controls when the pilot exists.
    live = ROOT / "target/wh-s4f/session3c/frame-ir/pilot/dynamic"
    if (live / "perf.json").exists():
        report = json.loads((live / "perf.json").read_text())
        M.authenticate_binary(report)
        no_companions = dict(report, binary_companions=[])
        rejects("live-omitted-companions", lambda: M.authenticate_binary(no_companions), "incomplete binary companion set")
        for row in report["runs"]:
            stem = live / f"{row['mode']}-{row['layout']}"
            M.authenticate_paths(row, stem)
        row = report["runs"][0]
        stem = live / f"{row['mode']}-{row['layout']}"
        reused = copy.deepcopy(row)
        reused["stdout"] = report["runs"][1]["stdout"]
        rejects("live-shared-output", lambda: M.authenticate_paths(reused, stem), "wrong process output path")
        reused = copy.deepcopy(row)
        reused["profiles"] = report["runs"][1]["profiles"]
        rejects("live-shared-layout-profiles", lambda: M.authenticate_paths(reused, stem), "wrong numbered bracket profile paths")
        raw = M.read_raw(row["profiles"][1])
        symbols = report["gc_symbols"]
        invocation = M.command(report["binary"]["path"], False)
        _, pid = M.boundary(raw, "PCF-START", symbols, 2, invocation=invocation)
        rejects("live-other-command", lambda: M.boundary(raw, "PCF-START", symbols, 2, pid, ["other-binary"]), "wrong bracket command")
        rejects("live-unexecuted-preparation-gc", lambda: M.boundary(raw.replace("cfn=" + symbols["body"], "cfn=omitted_gc_body"), "PCF-START", symbols, 2, pid, invocation), "missing executed preparation GC")
        rejects("live-missing-start", lambda: M.boundary(raw.replace("PCF-START", "omitted-start"), "PCF-START", symbols, 2, pid, invocation), "missing PCF-START")
        raw_end = M.read_raw(row["profiles"][2])
        rejects("live-missing-end", lambda: M.boundary(raw_end.replace("PCF-END", "omitted-end"), "PCF-END", symbols, 3, pid, invocation), "missing PCF-END")
        M.validate_program(live, report["gnu"]["path"])
    result = {"checker": M.pin(M.__file__), "test": M.pin(__file__),
              "rejected": rejected, "genuine_negatives": saved,
              "genuine_overcap": {"candidate": M.pin(base / "actual5/perf.json"),
                                   "gnu": M.pin(base / "gnu/perf.json"),
                                   "ratio": ratios["bytecode"]["let"]}}
    if output:
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(json.dumps(result, indent=2) + "\n")
    print(f"S4.2 performance controls PASS: {len(rejected)} rejected; genuine old overcap {ratios['bytecode']['let']:.6f}x")
    return result


if __name__ == "__main__":
    run(Path(sys.argv[1]) if len(sys.argv) > 1 else None)
