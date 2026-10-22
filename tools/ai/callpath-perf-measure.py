#!/usr/bin/env python3
"""Controlled call-path micro-experiments, reusing the isolated callfloor harness.

Each interval includes the tested loop and its following collection, minus an
independent empty-loop interval. Setup is outside instrumentation on NeLisp.
Four ABI_LAYOUT_PAD environments are averaged; these are environment layouts,
not four separately compiled executables. GNU executes the identical source or
GNU 31.1-compiled bytecode fixture. The GNU 'vm' label repeats interpreted Lisp.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/callpath-perf/study"
GNU = os.environ.get("CALLPATH_GNU") or shutil.which("emacs")
if not GNU:
    raise RuntimeError("set CALLPATH_GNU or put GNU Emacs 31.1 on PATH")
MODES = ("interpreter", "vm", "bytecode")
LAYOUTS = (0, 700, 1400, 2100)
OPS = {
    "control": "", "while": "", "funcall": "(funcall pcf-empty)",
    "nary": "(funcall pcf-four 1 2 3 4)",
    "lexical": "(funcall pcf-lexical)",
    "let-special": "(funcall pcf-dynamic)",
    "subr": "(funcall pcf-subr pcf-pair)",
    "recur-1": "(funcall pcf-recursive 1)",
    "recur-4": "(funcall pcf-recursive 4)",
    "recur-16": "(funcall pcf-recursive 16)",
}
EXTRA = """(defun pcf-four-body (a b c d) nil)
(defun pcf-read-special () pcf-special)
(defun pcf-lexical-body () (let ((pcf-local 17)) (pcf-read-special)))
(defun pcf-dynamic-body () (let ((pcf-special 17)) (pcf-read-special)))
(defun pcf-recursive-body (n)
  (if (> n 0) (pcf-recursive-body (1- n)) nil))
(defvar pcf-four nil)
(defvar pcf-lexical nil)
(defvar pcf-dynamic nil)
(defvar pcf-recursive nil)
(defvar pcf-subr nil)
"""
EXTRA_NAMES = "pcf-four-body pcf-read-special pcf-lexical-body pcf-dynamic-body pcf-recursive-body"
CAPTURE = """(setq pcf-four (symbol-function 'pcf-four-body)
      pcf-lexical (symbol-function 'pcf-lexical-body)
      pcf-dynamic (symbol-function 'pcf-dynamic-body)
      pcf-recursive (symbol-function 'pcf-recursive-body)
      pcf-subr (symbol-function 'car))
"""


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def reference(gnu):
    executable = shutil.which(gnu) or str(Path(gnu).resolve())
    run = subprocess.run([executable, "--version"], capture_output=True,
                         text=True, check=True, timeout=30)
    if run.stdout.splitlines()[0] != "GNU Emacs 31.1":
        raise ValueError("the study reference must be GNU Emacs 31.1")
    return executable


def study_harness(isolated="prim-callfloor-isolated.py"):
    spec = importlib.util.spec_from_file_location("callpath_isolated", ROOT / "tools/ai" / isolated)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.OUT = OUT
    module.P.OPS = OPS
    original = module.P.prepare

    def prepare(dest, gnu):
        source, compiled, driver = original(dest, gnu)
        # The special declaration precedes compilation of pcf-dynamic-body.
        source.write_text(source.read_text().replace("(defvar pcf-special 7)\n", "(defvar pcf-special 7)\n" + EXTRA, 1))
        module.C.compile_forms(source, compiled, gnu)
        text = driver.read_text()
        text = text.replace("pcf-flex pcf-empty-body pcf-one-body", "pcf-flex pcf-empty-body pcf-one-body " + EXTRA_NAMES)
        text = text.replace("pcf-empty-body pcf-one-body))", "pcf-empty-body pcf-one-body " + EXTRA_NAMES + "))")
        text = text.replace("(setq pcf-empty", CAPTURE + "(setq pcf-empty", 1)
        driver.write_text(text)
        return source, compiled, driver

    module.P.prepare = prepare
    return module


def load_tool(name):
    spec = importlib.util.spec_from_file_location("cpp_" + name.replace("-", "_"), ROOT / "tools/ai" / (name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module



MAX_ADDRESS_SPACE = 8 * 1024 ** 3


def memory_policy(jobs=1):
    import resource
    if jobs != 1:
        raise ValueError("heavy campaigns require --jobs 1 under /tmp/notes-heavy.lock")
    soft, hard = resource.getrlimit(resource.RLIMIT_AS)
    if soft == resource.RLIM_INFINITY or not 0 < soft <= MAX_ADDRESS_SPACE:
        raise ValueError("inherit an RLIMIT_AS of at most 8 GiB: ulimit -v 8388608; never raise the cap")
    return {"rlimit_as_soft_bytes": soft,
            "rlimit_as_hard_bytes": None if hard == resource.RLIM_INFINITY else hard,
            "maximum_bytes": MAX_ADDRESS_SPACE, "jobs": jobs,
            "required_heavy_lock": "/tmp/notes-heavy.lock"}

def source_identity(root=ROOT):
    root = Path(root).resolve()
    names = subprocess.check_output(["git", "ls-files", "src", "lisp", "packages", "scripts", "Makefile"], cwd=ROOT, text=True).splitlines()
    rows = [[name, hashlib.sha256((root / name).read_bytes()).hexdigest()] for name in names if (root / name).is_file()]
    return {"count": len(rows), "sha256": hashlib.sha256(json.dumps(rows, separators=(",", ":")).encode()).hexdigest()}


def measure_suite(args):
    global OUT
    import contextlib
    OUT = ROOT / "target/callpath-perf" / (args.suite + ("-queued" if args.protocol == "queued" else ""))
    gnu = reference(args.gnu)
    if shutil.disk_usage(ROOT).free < 50 * 1024 ** 3:
        raise RuntimeError("stop: less than 50 GiB free")
    isolated = "callpath-perf-isolated" if args.protocol == "queued" else "prim-callfloor-isolated"
    module = study_harness(isolated + ".py") if args.suite == "study" else load_tool(isolated)
    module.OUT = OUT
    # Keep the original full fixture and warmup identical to the study/session3a.
    # Select only the required independent intervals after fixture preparation.
    full_ops, original = dict(module.P.OPS), module.P.prepare
    required = ("control", "while", "funcall") if args.suite == "study" else ("control", "while", "builtin", "let")
    def prepare(dest, compiler):
        module.P.OPS = full_ops
        result = original(dest, compiler)
        module.P.OPS = {name: full_ops[name] for name in required}
        return result
    module.P.prepare = prepare
    dest = OUT / args.tag
    dest.mkdir(parents=True, exist_ok=True)
    (dest / "producer.py").write_bytes(Path(__file__).read_bytes())
    campaign = {"schema": 1, "suite": args.suite, "source": source_identity(args.source_root),
                "binary": pin(args.binary), "script": pin(dest / "producer.py"), "n": args.n,
                "modes": MODES, "layouts": LAYOUTS, "operations": required,
                "profile_includes_following_GC": True,
                "protocol": "queued-3-gc-v1" if args.protocol == "queued" else "controller-v1",
                "memory_policy": args.memory_policy}
    (dest / "campaign.json").write_text(json.dumps(campaign, indent=2) + "\n")
    with (dest / "measurement.log").open("w") as log, contextlib.redirect_stdout(log):
        module.measure(args.binary, args.tag, gnu, args.n, LAYOUTS, MODES, args.jobs)
    print(f"{args.suite} {args.tag}: complete modes=3 layouts=4 intervals={12 * len(required)}", flush=True)


def regex_recipe(module):
    import ast
    matches = []
    for node in ast.walk(ast.parse(Path(module.__file__).read_text())):
        if (isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
            and node.func.attr == "write_text" and isinstance(node.func.value, ast.BinOp)
            and isinstance(node.func.value.right, ast.Constant) and node.func.value.right.value == "regex.el"):
            matches.append(ast.literal_eval(node.args[0]))
    if len(matches) != 1 or "ABI-REGEX 6000" not in matches[0]:
        raise ValueError("exact ABI regex recipe missing")
    return matches[0]


def measure_ir(args):
    from concurrent.futures import ThreadPoolExecutor, as_completed
    import threading
    if shutil.disk_usage(ROOT).free < 50 * 1024 ** 3:
        raise RuntimeError("stop: less than 50 GiB free")
    module = load_tool("abi-ir")
    dest = ROOT / "target/callpath-perf/ir" / args.tag
    dest.mkdir(parents=True, exist_ok=False)
    (dest / "regex.el").write_text(regex_recipe(module))
    binary = Path(args.binary).resolve()
    before = source_identity(args.source_root)
    (dest / "producer.py").write_bytes(Path(__file__).read_bytes())
    report = {"schema": 1, "binary": pin(binary), "source": before,
              "inputs": [pin(dest / "producer.py"), pin(module.__file__), *[pin(ROOT / p) for p in module.FILE_SOURCES]],
              "total_includes_GC": True, "memory_policy": args.memory_policy, "runs": []}
    cancel = threading.Event()
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        tasks = {pool.submit(module.run_one, dest, mode, args.tag, binary, layout, cancel): (mode, layout)
                 for mode in ("boot", "files", "regex") for layout in LAYOUTS}
        try:
            for future in as_completed(tasks):
                mode, layout = tasks[future]
                ir, gc = future.result()
                stem = dest / f"{mode}-{args.tag}-{layout}"
                row = {"mode": mode, "layout": layout, "ir": ir, "gc": gc,
                       "profile": pin(str(stem) + ".cg"), "stdout": pin(str(stem) + ".out"),
                       "stderr": pin(str(stem) + ".err")}
                report["runs"].append(row)
                (dest / "report.json").write_text(json.dumps(report, indent=2) + "\n")
                print(f"ir {args.tag}: completed={len(report['runs'])}/12", flush=True)
        finally:
            cancel.set()
    if source_identity(args.source_root) != before:
        raise ValueError("build source changed during measurement")



def semantics(args):
    dest = ROOT / "target/callpath-perf/semantics" / args.tag
    dest.mkdir(parents=True, exist_ok=False)
    gnu = reference(args.gnu)
    callfloor = load_tool("prim-callfloor-check")
    callfloor.OUT = dest / "callfloor"
    frames = load_tool("word-heap-frames")
    envcheck = ROOT / "test/standalone-env-capture-roots-check.el"
    (dest / "producer.py").write_bytes(Path(__file__).read_bytes())
    report = {"schema": 1, "source": source_identity(), "gnu": pin(gnu),
              "inputs": [pin(dest / "producer.py"), pin(envcheck), pin(frames.PROBE),
                         *[pin(frames.THREAD_PROVIDER / p) for p in frames.THREAD_FILES]],
              "binaries": {}, "callfloor": {}, "captures": {}, "env_capture": {},
              "memory_policy": args.memory_policy, "pass": True}
    def run(exe, case, linkage):
        env = dict(os.environ, WH_FRAME_CASE=case, WH_FRAME_THREAD_PROVIDER=str(frames.THREAD_PROVIDER), NELISP_FX_DISABLE="1")
        command = [exe] + (["-Q", "--batch"] if linkage == "gnu" else []) + ["--load", str(frames.PROBE)]
        try:
            proc = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=180)
            code, out, err = proc.returncode, proc.stdout, proc.stderr
        except subprocess.TimeoutExpired as e:
            code, out, err = 124, e.stdout or b"", e.stderr or b""
        prefix = dest / f"{linkage}-{case}"
        Path(str(prefix) + ".out").write_bytes(out)
        Path(str(prefix) + ".err").write_bytes(err)
        return {"command": command, "returncode": code, "stdout": pin(str(prefix) + ".out"), "stderr": pin(str(prefix) + ".err")}
    for case in frames.CASES:
        row = run(gnu, case, "gnu")
        report["captures"]["gnu/" + case] = row
        if row["returncode"] < 0 or row["returncode"] == 124:
            defect = ROOT / "target/callpath-perf/gnu-defects" / args.tag / case
            defect.mkdir(parents=True, exist_ok=True)
            shutil.copy2(frames.PROBE, defect / "input.el")
            (defect / "invocation.json").write_text(json.dumps(row, indent=2) + "\n")
    shadow_source = ROOT / "test/nelisp-shadow-differential-cases.el"
    report["inputs"].append(pin(shadow_source))
    report["shadow_arity"] = {}
    shadow_expression = """(list
      (list (condition-case e (string-search "a") (wrong-number-of-arguments e))
            (condition-case e (string-search "a" "a" nil nil) (wrong-number-of-arguments e)))
      (list (apply '(+ 1 2)) (apply '(list))
            (condition-case e (apply 'list) (wrong-type-argument e))
            (condition-case e (apply nil) (void-function e))
            (condition-case e (apply '(list . 2)) (wrong-type-argument e))
            (condition-case e (apply (cons 'list (vector 1))) (wrong-type-argument e)))
      (let ((cycle (list 'list))) (setcdr cycle cycle)
        (condition-case e (apply cycle) (circular-list (car e)))))"""
    for mode in ("interpreter", "vm"):
        for linkage in ("gnu", "dynamic", "static"):
            binary = gnu if linkage == "gnu" else str(Path(getattr(args, linkage)).resolve())
            env = dict(os.environ)
            env.pop("NELISP_FX_DISABLE", None)
            if mode == "interpreter":
                env["NELISP_FX_DISABLE"] = "1"
            form = '(progn (defun cpp-shadow-arity () ' + shadow_expression + ') '
            if mode == "vm" and linkage != "gnu":
                form += "(nelisp--fx-register 'cpp-shadow-arity) (princ (format \"SHADOW-VM %S\\n\" (vectorp (gethash (nthcdr 3 (symbol-function 'cpp-shadow-arity)) nelisp--fx-table)))) "
            form += '(princ (format "SHADOW-ARITY %S\\n" (cpp-shadow-arity))))'
            command = [binary] + (["-Q", "--batch"] if linkage == "gnu" else []) + ["--eval", form]
            proc = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=120)
            prefix = dest / f"{linkage}-shadow-{mode}"
            Path(str(prefix) + ".out").write_bytes(proc.stdout)
            Path(str(prefix) + ".err").write_bytes(proc.stderr)
            report["shadow_arity"][linkage + "/" + mode] = {"command": command, "returncode": proc.returncode,
                "stdout": pin(str(prefix) + ".out"), "stderr": pin(str(prefix) + ".err")}
            if proc.returncode or proc.stderr or b"SHADOW-ARITY " not in proc.stdout:
                report["pass"] = False
    for linkage in ("dynamic", "static"):
        binary = str(Path(getattr(args, linkage)).resolve())
        report["binaries"][linkage] = pin(binary)
        if callfloor.parity(binary, linkage, gnu):
            report["pass"] = False
        report["callfloor"][linkage] = pin(callfloor.OUT / linkage / "parity.json")
        proc = subprocess.run([binary, "--load", str(envcheck)], cwd=ROOT, capture_output=True, timeout=180)
        prefix = dest / (linkage + "-env-capture")
        Path(str(prefix) + ".out").write_bytes(proc.stdout)
        Path(str(prefix) + ".err").write_bytes(proc.stderr)
        report["env_capture"][linkage] = {"returncode": proc.returncode, "stdout": pin(str(prefix) + ".out"), "stderr": pin(str(prefix) + ".err")}
        if proc.returncode or proc.stderr or b"ENV-CAPTURE-ROOTS checked=" not in proc.stdout or b" PASS\n" not in proc.stdout:
            report["pass"] = False
        for case in frames.CASES:
            row = run(binary, case, linkage)
            report["captures"][linkage + "/" + case] = row
            ref = report["captures"]["gnu/" + case]
            raw = Path(row["stdout"]["path"]).read_bytes()
            refraw = Path(ref["stdout"]["path"]).read_bytes()
            try:
                frames.completed(raw, case)
                if row["returncode"] or Path(row["stderr"]["path"]).read_bytes():
                    report["pass"] = False
                if ref["returncode"] >= 0 and ref["returncode"] != 124 and raw != refraw:
                    report["pass"] = False
            except ValueError:
                report["pass"] = False
    (dest / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"focused semantics: callfloor=48 captures={len(frames.CASES) * 2} env-capture=2 pass={report['pass']}")
    if not report["pass"]:
        raise ValueError("focused semantic checks failed")

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="action", required=True)
    p = sub.add_parser("measure")
    p.add_argument("suite", choices=("study", "floors"))
    p.add_argument("--protocol", choices=("queued", "controller"), default="queued")
    p.add_argument("binary")
    p.add_argument("tag")
    p.add_argument("--gnu", default=GNU)
    p.add_argument("--n", type=int, default=100)
    p.add_argument("--jobs", type=int, default=1)
    p.add_argument("--source-root", default=str(ROOT))
    p = sub.add_parser("ir")
    p.add_argument("binary")
    p.add_argument("tag")
    p.add_argument("--jobs", type=int, default=1)
    p.add_argument("--source-root", default=str(ROOT))
    p = sub.add_parser("semantics")
    p.add_argument("dynamic")
    p.add_argument("static")
    p.add_argument("tag")
    p.add_argument("--gnu", default=GNU)
    args = parser.parse_args()
    args.memory_policy = memory_policy(getattr(args, "jobs", 1))
    if args.action == "measure":
        measure_suite(args)
    elif args.action == "ir":
        measure_ir(args)
    else:
        semantics(args)


if __name__ == "__main__":
    main()
