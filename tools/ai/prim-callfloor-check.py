#!/usr/bin/env python3
"""Retain exact GNU calling evidence; unmeasured acceptance checks fail closed."""
import argparse
import difflib
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import re
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-callfloor"
MODES = ("interpreter", "vm", "bytecode")


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def compile_forms(source, output, gnu):
    expr = f'''(progn (require 'bytecomp)
      (with-temp-buffer (insert-file-contents {json.dumps(str(source))})
       (check-parens) (goto-char (point-min))
       (let (forms) (condition-case nil
        (while t (push (read (current-buffer)) forms)) (end-of-file nil))
        (with-temp-file {json.dumps(str(output))}
         (insert ";;; -*- lexical-binding: t -*-\\n")
         (dolist (form (nreverse forms)) (eval form t)
          (prin1 (if (eq (car form) 'defun)
           (list 'fset (list 'quote (cadr form)) (byte-compile (cadr form))) form)
           (current-buffer)) (insert "\\n"))))))'''
    result = subprocess.run([gnu, "-Q", "--batch", "--eval", expr], cwd=ROOT,
                            capture_output=True, timeout=60, check=True)
    output.with_suffix(".compile.err").write_bytes(result.stderr)


def parity(binary, tag, gnu):
    dest = OUT / tag
    dest.mkdir(parents=True, exist_ok=True)
    source = ROOT / "test/perf/prim-callfloor-parity-functions.el"
    driver = ROOT / "test/perf/prim-callfloor-parity.el"
    compiled = dest / "parity-bytecode.el"
    compile_forms(source, compiled, gnu)
    report = {"binary": pin(binary), "gnu": pin(gnu),
              "inputs": [pin(source), pin(driver), pin(compiled)], "modes": {}}
    version = subprocess.check_output([gnu, "-Q", "--batch", "--eval", "(princ emacs-version)"], text=True)
    if version != "31.1":
        raise ValueError(f"GNU 31.1 required, got {version}")
    report["gnu_version"] = version
    for mode in MODES:
        env = dict(os.environ, PCF_MODE=mode,
                   PCF_SOURCE=str(compiled if mode == "bytecode" else source),
                   TMPDIR=str(ROOT / "target/tmp"),
                   XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
        env.pop("NELISP_FX_DISABLE", None)
        if mode == "interpreter":
            env["NELISP_FX_DISABLE"] = "1"
        rows = []
        for label, exe in (("gnu", gnu), ("nelisp", str(Path(binary).resolve()))):
            command = [exe] + (["-Q", "--batch"] if label == "gnu" else [])
            command += ["--load", str(driver)]
            result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                                    timeout=180)
            stdout, stderr = dest / f"{mode}-{label}.out", dest / f"{mode}-{label}.err"
            stdout.write_bytes(result.stdout)
            stderr.write_bytes(result.stderr)
            rows.append([r for r in result.stdout.decode().splitlines() if r.startswith("PCF ")])
            report["modes"].setdefault(mode, {})[label] = {
                "command": command, "returncode": result.returncode,
                "stdout": pin(stdout), "stderr": pin(stderr),
                "complete": b"PCF-COMPLETE 8\n" in result.stdout,
                "valid": result.returncode == 0 and not result.stderr and
                         b"PCF-COMPLETE 8\n" in result.stdout and len(rows[-1]) == 8}
        diff = list(difflib.unified_diff(*rows, fromfile="GNU31.1", tofile="NeLisp", lineterm=""))
        (dest / f"{mode}.diff").write_text("\n".join(diff) + ("\n" if diff else ""))
        report["modes"][mode]["differences"] = len(diff)
        print(mode, "DIFF" if diff else "MATCH", flush=True)
    (dest / "parity.json").write_text(json.dumps(report, indent=2) + "\n")
    return int(any(mode["differences"] or not mode["gnu"]["valid"]
                   or not mode["nelisp"]["valid"] for mode in report["modes"].values()))


def check(group):
    # Recompute acceptance directly. Cached success receipts cannot substitute
    # for the current budgets, sources, or raw measurements (including reds).
    verify(group)
    print(group + ": PASS")


def compare_floors(before, after):
    """Reject any increase across all 30 S1 operation/mode means."""
    if set(before) != set(MODES) or set(after) != set(MODES):
        raise ValueError("incomplete S1 modes")
    operations = {"form", "funcall", "builtin", "apply", "optional-rest",
                  "let", "let-special", "let-star", "while", "dotimes"}
    regressions = []
    for mode in MODES:
        if set(before[mode]) != operations or set(after[mode]) != operations:
            raise ValueError("incomplete S1 operation comparison")
        for op, old in before[mode].items():
            new = after[mode][op]["ir_per_op"]
            if not math.isfinite(new) or not math.isfinite(old["ir_per_op"]):
                raise ValueError("nonfinite S1 instruction count")
            if new > old["ir_per_op"]:
                regressions.append(f"{mode} {op}: {old['ir_per_op']:.4f} -> {new:.4f} Ir")
    if regressions:
        raise ValueError("S1 operations regressed:\n" + "\n".join(regressions))


def perf(tag):
    """Recompute all costs from authenticated raw intervals, never a PASS flag."""
    path = OUT / tag / "perf.json"
    report = json.loads(path.read_text())
    if type(report["n"]) is not int or report["n"] <= 0:
        raise ValueError("invalid performance iteration count")
    evidence = [pin(path), report["binary"], *report["inputs"], *report.get("binary_companions", [])]
    for item in evidence:
        if pin(item["path"]) != item:
            raise ValueError(f"changed performance input: {item['path']}")
    if "qualified_binary" in report:
        item = report["qualified_binary"]
        if pin(item["path"]) != item or item["sha256"] != report["binary"]["sha256"]:
            raise ValueError("benchmark/qualification binary mismatch")
        evidence.append(item)
    equivalents = report.get("bytecode_equivalents", {})
    if equivalents:
        if equivalents != {"let-special": "while"}:
            raise ValueError("unexpected compiled equivalence")
        program = report["compiled_program"]
        if pin(program["path"]) != program or program not in report["inputs"]:
            raise ValueError("changed compiled equivalence input")
        expr = "(progn (load " + json.dumps(program["path"]) + " nil t) (let ((a (symbol-function 'pcf-let-special)) (b (symbol-function 'pcf-while))) (princ (equal (list (aref a 0) (aref a 1) (aref a 2) (aref a 3)) (list (aref b 0) (aref b 1) (aref b 2) (aref b 3))))))"
        proof = subprocess.run([report["gnu"]["path"], "-Q", "--batch", "--eval", expr], capture_output=True, timeout=30)
        if proof.returncode or proof.stderr or proof.stdout != b"t":
            raise ValueError("compiled programs are not equivalent")
    expected = {(m, p) for m in MODES for p in (0, 700, 1400, 2100)}
    if len(report["runs"]) != 12 or {(r["mode"], r["layout"]) for r in report["runs"]} != expected:
        raise ValueError("incomplete performance coverage")
    means = {m: {} for m in MODES}
    for run in report["runs"]:
        if report.get("isolated"):
            text = ""
            for row in run["intervals"]:
                if row["iterations"] != (0 if row["op"] == "control" else report["n"]):
                    raise ValueError("incorrect isolated iteration count")
                if "same_program_as" in row:
                    if run["mode"] != "bytecode" or equivalents.get(row["op"]) != row["same_program_as"]:
                        raise ValueError("unproved shared program measurement")
                    other = next(r for r in run["intervals"] if r["op"] == row["same_program_as"])
                    if row["profile"] != other["profile"] or row["ir"] != other["ir"]:
                        raise ValueError("incorrect shared program measurement")
                for key in ("stdout", "stderr", "wall_stdout", "wall_stderr",
                            "control_stdout", "control_stderr"):
                    item = row[key]
                    if pin(item["path"]) != item:
                        raise ValueError("changed isolated process output")
                    evidence.append(item)
                output = Path(row["stdout"]["path"]).read_text()
                text += output
                if row["returncode"] or row["wall_returncode"] or output.count("PCF-COMPLETE 1\n") != 1:
                    raise ValueError("incomplete isolated process")
                if "nelisp: uncaught error:" in Path(row["stderr"]["path"]).read_text():
                    raise ValueError("uncaught isolated error")
                if Path(row["wall_stderr"]["path"]).read_bytes():
                    raise ValueError("unexpected isolated wall stderr")
                if "OK." not in Path(row["control_stdout"]["path"]).read_text():
                    raise ValueError("instrumentation was not enabled")
                wall = Path(row["wall_stdout"]["path"]).read_text()
                clock = re.search(r"PCF-TIME ([0-9.]+)", wall)
                if wall.count("PCF-COMPLETE 1\n") != 1 or not clock or float(clock[1]) != row["seconds"]:
                    raise ValueError("incomplete or incorrect isolated clock")
        else:
            stem = OUT / tag / f"{run['mode']}-{run['layout']}"
            text = Path(str(stem) + ".out").read_text()
            if text.splitlines().count("PCF-COMPLETE 11") != 1:
                raise ValueError("incomplete performance process")
            if "nelisp: uncaught error:" in Path(str(stem) + ".err").read_text():
                raise ValueError("uncaught error in profiled process")
            if run["mode"] == "vm" and report["binary"]["sha256"] != pin(shutil.which("emacs"))["sha256"]:
                for name in ("pcf-empty-body", "pcf-one-body", "pcf-funcall", "pcf-builtin", "pcf-let", "pcf-while"):
                    if f"PCF-COVERAGE {name} t" not in text:
                        raise ValueError(f"VM fallback in acceptance operation: {name}")
            for suffix in (".out", ".err", ".wall.out", ".wall.err"):
                evidence.append(pin(str(stem) + suffix))
            if Path(str(stem) + ".wall.err").read_bytes():
                raise ValueError("unexpected wall-process stderr")
        if run["mode"] == "vm" and report["binary"]["sha256"] != pin(shutil.which("emacs"))["sha256"]:
            for name in ("pcf-empty-body", "pcf-one-body", "pcf-funcall", "pcf-builtin", "pcf-let", "pcf-while"):
                if f"PCF-COVERAGE {name} t" not in text:
                    raise ValueError(f"VM fallback in acceptance operation: {name}")
        rows = run["intervals"]
        if len(rows) != 11 or {r["op"] for r in rows} != {
                "control", "form", "funcall", "builtin", "apply", "optional-rest",
                "let", "let-special", "let-star", "while", "dotimes"}:
            raise ValueError("incomplete operation coverage")
        control = next(r for r in rows if r["op"] == "control")
        empty = next(r for r in rows if r["op"] == "while")
        for row in rows:
            if pin(row["profile"]["path"]) != row["profile"]:
                raise ValueError("changed raw profile")
            raw = Path(row["profile"]["path"]).read_text()
            if int(re.search(r"^summary: (\d+)$", raw, re.M)[1]) != row["ir"]:
                raise ValueError("wrong raw instruction count")
            evidence.append(row["profile"])
            if row["op"] != "control":
                base = control if row["op"] in ("while", "dotimes") else empty
                actual = (row["ir"] - base["ir"]) / report["n"]
                if not math.isfinite(row["ir_per_op"]) or abs(actual - row["ir_per_op"]) > 1e-7:
                    raise ValueError("wrong empty-loop subtraction")
                if row["op"] in ("funcall", "builtin", "let", "while") and actual <= 0:
                    raise ValueError("nonpositive acceptance measurement")
                means[run["mode"]].setdefault(row["op"], []).append(row)
    means = {m: {op: {key: sum(r[key] for r in rows) / 4
                       for key in ("ir_per_op", "us_per_op")}
                  for op, rows in ops.items()} for m, ops in means.items()}
    return report, means, evidence


def receipt(name, marker):
    path = OUT / "run2h-qualification-quiet" / (name + ".json")
    report = json.loads(path.read_text())
    files = [pin(path), report["stdout"], report["stderr"], report["binary"], report["static"]]
    if report["binary"] != pin(OUT / "run2h-dynamic") or report["static"] != pin(OUT / "run2h-static"):
        raise ValueError("command used a different qualification binary")
    for item in files:
        if pin(item["path"]) != item:
            raise ValueError("changed command receipt")
    if report["returncode"] or marker not in Path(report["stdout"]["path"]).read_text():
        raise ValueError(f"failed or incomplete command: {name}")
    if Path(report["stderr"]["path"]).read_bytes():
        raise ValueError(f"unexpected command stderr: {name}")
    return files


def entry_witness(before, after):
    """Prove constant callees avoid frames in the configuration being timed."""
    path = ROOT / "tools/ai/prim-callfloor-profile.py"
    spec = importlib.util.spec_from_file_location("callfloor_edges", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    evidence = [pin(path)]
    for report, is_before in ((before, True), (after, False)):
        table = module.symbols(report["binary"]["path"])
        for run in report["runs"]:
            if run["mode"] not in ("vm", "bytecode"):
                continue
            def count(op):
                row = next(r for r in run["intervals"] if r["op"] == op)
                edges = module.call_edges(row["profile"]["path"], table)
                return sum(value[0] for (caller, callee), value in edges.items()
                           if ((run["mode"] == "vm" and caller.startswith("m5_fx_run_body")
                                and callee.startswith("nl_alloc_vector"))
                               or (run["mode"] == "bytecode"
                                   and callee.startswith("wf_bytecode_execute_frame"))))
            delta = count("funcall") - count("while")
            expected = report["n"] if is_before else 0
            if delta != expected:
                raise ValueError(f"{run['mode']} {run['layout']}: frame witness {delta}, expected {expected}")
    return evidence


def verify(group):
    evidence = []
    if group in ("profile", "bytecode-profile", "baseline"):
        base, _, evidence = perf("v4_base")
        gnu, _, files = perf("v4_gnux")
        evidence += files
        path = OUT / "v4_base/profile.json"
        profile = json.loads(path.read_text())
        if len(profile["rows"]) != 120:
            raise ValueError("incomplete S1 operation decomposition")
        measured = {(run["mode"], run["layout"], row["op"]): row["ir_per_op"]
                    for run in base["runs"] for row in run["intervals"] if row["op"] != "control"}
        if len({(r["mode"], r["layout"], r["op"]) for r in profile["rows"]}) != 120:
            raise ValueError("duplicate profile decomposition rows")
        for row in profile["rows"]:
            if abs(sum(row["categories"].values()) - row["Ir"]) > 1e-6:
                raise ValueError("profile categories do not reconcile")
            if abs(row["Ir"] - measured[row["mode"], row["layout"], row["op"]]) > 1e-6:
                raise ValueError("profile decomposition differs from measured total")
        diagnosis = OUT / "diagnosis.json"
        if len(json.loads(diagnosis.read_text())["root_causes"]) < 3:
            raise ValueError("missing root-cause evidence")
        evidence += [pin(path), pin(diagnosis), pin(OUT / "profile-controls.json")]
        if base["binary"] == gnu["binary"]:
            raise ValueError("baseline and reference are the same program")
    elif group in ("arguments", "control", "parity"):
        path = OUT / "run2h/parity.json"
        report = json.loads(path.read_text())
        if report["gnu_version"] != "31.1" or set(report["modes"]) != set(MODES):
            raise ValueError("wrong semantic reference or missing modes")
        if report["binary"] != pin(OUT / "run2h-dynamic"):
            raise ValueError("semantic evidence used a different qualification binary")
        evidence = [pin(path), report["binary"], report["gnu"], *report["inputs"]]
        for mode, data in report["modes"].items():
            if data["differences"] or not data["gnu"]["valid"] or not data["nelisp"]["valid"]:
                raise ValueError(f"semantic difference in {mode}")
            actual = []
            for label in ("gnu", "nelisp"):
                record = data[label]
                evidence += [record["stdout"], record["stderr"]]
                if record["returncode"] or Path(record["stderr"]["path"]).read_bytes():
                    raise ValueError("invalid semantic process")
                lines = Path(record["stdout"]["path"]).read_text().splitlines()
                if lines.count("PCF-COMPLETE 8") != 1:
                    raise ValueError("missing semantic completion marker")
                actual.append([line for line in lines
                               if line.startswith("PCF ")])
            if len(actual[0]) != 8 or actual[0] != actual[1]:
                raise ValueError("wrong semantic rows")
        if group == "parity":
            for mode in ("interpreter", "vm"):
                for case, marker in (("standalone-prim-callfloor-check", "CALLFLOOR checks=16 failures=0 PASS"),
                                     ("standalone-backtrace-locals-check", "failures=0 PASS"),
                                     ("standalone-shallow-binding-check", "failures=0 PASS"),
                                     ("standalone-shallow-binding-dyn-check", "failures=0 PASS")):
                    evidence += receipt(case + "-" + mode, marker)
    elif group in ("funcall", "builtin", "floor"):
        baseline, before, evidence = perf("v4_base")
        _, reference, files = perf("v4_gnux")
        evidence += files
        candidate, after, files = perf("v5_cand")
        if candidate.get("qualified_binary", candidate["binary"]) != pin(OUT / "run2h-dynamic"):
            raise ValueError("performance evidence used a different qualification binary")
        if not baseline.get("isolated") or not candidate.get("isolated") or baseline["n"] != candidate["n"]:
            raise ValueError("floor measurements must use matching independent trials")
        evidence += files
        if group == "funcall":
            evidence += entry_witness(baseline, candidate)
        budget_file = ROOT / "tools/ai/prim-callfloor-budgets.json"
        budgets = json.loads(budget_file.read_text())
        evidence.append(pin(budget_file))
        operations = (group,) if group != "floor" else ("let", "while")
        for mode in MODES:
            if mode == "interpreter" and group == "floor":
                continue
            for op in operations:
                ratio = after[mode][op]["ir_per_op"] / reference[mode][op]["ir_per_op"]
                if ratio > budgets[mode][op]:
                    raise ValueError(f"{mode} {op}: {ratio:.3f}x exceeds {budgets[mode][op]}x GNU")
                if op == "funcall" and after[mode][op]["ir_per_op"] >= before[mode][op]["ir_per_op"]:
                    raise ValueError(f"funcall did not improve in {mode}")
    elif group == "no-regression":
        baseline, before, evidence = perf("v4_base")
        candidate, after, files = perf("v5_cand")
        if candidate.get("qualified_binary", candidate["binary"]) != pin(OUT / "run2h-dynamic"):
            raise ValueError("no-regression used a different qualification binary")
        if not baseline.get("isolated") or not candidate.get("isolated") or baseline["n"] != candidate["n"]:
            raise ValueError("no-regression requires matching independent trials")
        evidence += files
        compare_floors(before, after)
    elif group == "anvil":
        path = OUT / "anvil-run.json"
        report = json.loads(path.read_text())
        if report["returncode"] or report["tools"] != 40 or report["stdin_hold_seconds"] < 25:
            raise ValueError("Anvil tools/list did not meet the contract")
        if pin(OUT / "run2h-dynamic") not in report["inputs"]:
            raise ValueError("Anvil used a different qualification binary")
        evidence = [pin(path), *report["inputs"], *report["outputs"]]
    elif group == "census":
        script = ROOT / "tools/ai/box-census.py"
        spec = importlib.util.spec_from_file_location("box_census", script)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        totals = []
        for name in ("base-census.tsv", "run2h-census.tsv"):
            path = OUT / name
            summary, _ = module.tally(module.load(path))
            if summary["units"] != 114 or summary["defuns"] < 3200:
                raise ValueError("incomplete census")
            if any(summary["idioms"].get(key, 0) for key in
                   ("nl_val_store_word", "nl_abi_rc_to_word", "nl_abi_rc2_to_word", "inline-box-to-word")):
                raise ValueError("S6.2 conversion debt is nonzero")
            totals.append(sum(summary["idioms"].values()))
            evidence.append(pin(path))
        if totals[1] > totals[0]:
            raise ValueError("box census grew")
        evidence += [pin(script), pin(OUT / "run2h-dynamic"), pin(OUT / "base-dynamic")]
        print(f"box census {totals[0]} -> {totals[1]}, S6.2=0")
    elif group == "regression":
        path = ROOT / "target/prim-globals/callfloor-load-run2h/load.json"
        data = json.loads(path.read_text())
        if len(data["rows"]) != 8 or {r["layout"] for r in data["rows"]} != {0, 700, 1400, 2100}:
            raise ValueError("incomplete load layouts")
        means = {}
        evidence.append(pin(path))
        for who in ("base", "candidate"):
            selected = [r for r in data["rows"] if r["who"] == who]
            means[who] = sum(r["ir"] for r in selected) / 4
            for row in selected:
                expected_binary = OUT / ("base-dynamic" if who == "base" else "run2h-dynamic")
                if pin(row["binary"]) != pin(expected_binary):
                    raise ValueError("load used a different qualification binary")
                raw = Path(row["profile"])
                if int(re.search(r"^summary: (\d+)$", raw.read_text(), re.M)[1]) != row["ir"]:
                    raise ValueError("wrong load total")
                stdout = raw.with_suffix(".out")
                if stdout.read_text() != "FIVE-LIBRARIES PASS: 5 loaded\n":
                    raise ValueError("incomplete five-file load")
                evidence += [pin(raw), pin(stdout), pin(raw.with_suffix(".err")), pin(row["binary"])]
        if means["candidate"] >= means["base"]:
            raise ValueError("five-file load did not improve")
        comparisons = []
        for who in ("base-run2", "run2h"):
            path = ROOT / f"target/prim-strcmp/callfloor-{who}.json"
            record = json.loads(path.read_text())
            expected_binary = OUT / ("base-dynamic" if who == "base-run2" else "run2h-dynamic")
            if pin(record["binary"]) != pin(expected_binary) or record["binary_sha256"] != pin(expected_binary)["sha256"]:
                raise ValueError("string benchmark used a different qualification binary")
            for name, digest in record["files"].items():
                item = pin(ROOT / name)
                if item["sha256"] != digest:
                    raise ValueError("changed string benchmark artifact")
                evidence.append(item)
            if record["layouts"] != [0, 700, 1400, 2100] or set(record["operations"]) != {
                    "string-lessp", "string<", "string>", "string-greaterp",
                    "compare-strings", "string=", "string-equal", "string-prefix-p"}:
                raise ValueError("incomplete string benchmark")
            comparisons.append(record)
            evidence.append(pin(path))
        for op, before in comparisons[0]["operations"].items():
            if comparisons[1]["operations"][op]["Ir"] > before["Ir"]:
                raise ValueError(f"string benchmark regressed: {op}")
        # The established globals funcall/dotimes harness has six GC-delimited
        # intervals per run and keeps all work, including call and collection.
        globals_means = []
        for who in ("base-run2", "run2h"):
            path = ROOT / f"target/prim-globals/callfloor-{who}/perf.json"
            record = json.loads(path.read_text())
            expected_binary = OUT / ("base-dynamic" if who == "base-run2" else "run2h-dynamic")
            if record["binary"] != pin(expected_binary):
                raise ValueError("globals benchmark used a different qualification binary")
            if len(record["runs"]) != 12:
                raise ValueError("incomplete globals benchmark")
            evidence += [pin(path), record["binary"], *record["inputs"]]
            values = {}
            for run in record["runs"]:
                if len(run["intervals"]) != 6:
                    raise ValueError("missing globals intervals")
                for row in run["intervals"][1:]:
                    raw = Path(row["profile"]["path"])
                    if int(re.search(r"^summary: (\d+)$", raw.read_text(), re.M)[1]) != row["ir"]:
                        raise ValueError("wrong globals raw count")
                    actual = (row["ir"] - run["intervals"][0]["ir"]) / record["n"]
                    if abs(actual - row["ir_per_op"]) > 1e-7 or actual <= 0:
                        raise ValueError("invalid globals subtraction")
                    values.setdefault((run["mode"], row["op"]), []).append(actual)
                    evidence.append(row["profile"])
                evidence += [run[key] for key in ("stdout", "stderr", "wall_stdout")]
            globals_means.append({key: sum(rows) / 4 for key, rows in values.items()})
        if set(globals_means[0]) != set(globals_means[1]) or len(globals_means[0]) != 15:
            raise ValueError("missing globals operations or modes")
        for key, value in globals_means[0].items():
            if globals_means[1][key] > value:
                raise ValueError(f"globals benchmark regressed: {key}")
    elif group == "final":
        evidence = receipt("final-checks", "GATE-COUNT: 11 cases, 0 failures")
        evidence += receipt("final-standalone", "GATE-COUNT: 71 cases, 0 failures")
        evidence += [pin(OUT / "run2h-dynamic-build.log"), pin(OUT / "run2h-static-build.log")]
        source_path = OUT / "run2h-source.json"
        source = json.loads(source_path.read_text())
        evidence += [pin(source_path), *source["sources"], *source["binaries"]]
    else:
        raise ValueError(f"acceptance producer not implemented: {group}")
    for item in evidence:
        if pin(item["path"]) != item:
            raise ValueError(f"changed acceptance evidence: {item['path']}")
    return evidence


def accept(group):
    evidence = verify(group)
    dest = OUT / "acceptance"
    dest.mkdir(exist_ok=True)
    (dest / (group + ".json")).write_text(json.dumps({"group": group, "returncode": 0,
                                                     "evidence": evidence}, indent=2) + "\n")
    print(group + ": PASS")


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("action", choices=("parity", "check", "accept"))
    p.add_argument("argument")
    p.add_argument("--tag", default="base")
    p.add_argument("--gnu", default=shutil.which("emacs"))
    a = p.parse_args()
    if a.action == "parity":
        raise SystemExit(parity(a.argument, a.tag, a.gnu))
    elif a.action == "accept":
        accept(a.argument)
    else:
        check(a.argument)
