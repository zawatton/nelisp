#!/usr/bin/env python3
"""Run the plain-globals GNU semantic precondition, retaining raw evidence.

parity BINARY [--tag base]: GNU and NeLisp in interpreter, VM and bytecode.
check GROUP: check the saved parity or a hash-pinned command receipt.
Unrecorded criteria fail; stopping on a baseline defect never implies success.
"""
import argparse
import difflib
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-globals"
FUNCTIONS = ROOT / "test/perf/prim-globals-parity-functions.el"
DRIVER = ROOT / "test/perf/prim-globals-parity.el"
MODES = ("interpreter", "vm", "bytecode")
CORE = ("pgs-get", "pgs-symbol-value", "pgs-setq", "pgs-set",
        "pgs-plain-get")
CASES = 27


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": digest(path)}


def authenticate(item):
    if digest(item["path"]) != item["sha256"]:
        raise ValueError(f"changed evidence: {item['path']}")


def run(command, stem, env):
    started = time.monotonic()
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True,
                            timeout=180)
    stdout, stderr = stem.with_suffix(".out"), stem.with_suffix(".err")
    stdout.write_bytes(result.stdout)
    stderr.write_bytes(result.stderr)
    return {"command": command, "returncode": result.returncode,
            "elapsed_s": time.monotonic() - started,
            "environment": {key: env[key] for key in
                            ("PG_MODE", "PG_BYTECODE_FILE", "NELISP_FX_DISABLE",
                             "TMPDIR", "XDG_CACHE_HOME") if key in env},
            "stdout": pin(stdout), "stderr": pin(stderr)}


def canonical(text):
    rows = [line for line in text.splitlines() if line.startswith("PG ")]
    if len(rows) != CASES or len(set(line.split(" ", 2)[1] for line in rows)) != CASES:
        raise ValueError(f"incomplete parity output: {len(rows)} cases")
    if text.splitlines().count(f"PG-COMPLETE {CASES}") != 1:
        raise ValueError("missing or duplicate completion marker")
    return rows


def inspect_run(record):
    authenticate(record["stdout"])
    authenticate(record["stderr"])
    if record["returncode"] != 0:
        raise ValueError(f"process failed: {record['command']}")
    if Path(record["stderr"]["path"]).read_bytes():
        raise ValueError(f"unexpected stderr: {record['stderr']['path']}")
    return Path(record["stdout"]["path"]).read_text()


def compare(reference, candidate):
    want, got = canonical(inspect_run(reference)), canonical(inspect_run(candidate))
    return list(difflib.unified_diff(want, got, fromfile="GNU31.1", tofile="NeLisp",
                                     lineterm=""))


def parity(binary, tag, gnu):
    binary, gnu = Path(binary).resolve(), Path(shutil.which(gnu) or gnu).resolve()
    dest = OUT / tag
    dest.mkdir(parents=True, exist_ok=True)
    (ROOT / "target/tmp").mkdir(parents=True, exist_ok=True)
    bytecode = dest / "parity-bytecode.el"
    # check-parens distinguishes truncated forms from a normal EOF.
    expression = f'''(progn (require 'bytecomp)
      (with-temp-buffer
        (insert-file-contents {json.dumps(str(FUNCTIONS))})
        (check-parens)
        (goto-char (point-min))
        (let (forms)
          (condition-case nil
              (while t (push (read (current-buffer)) forms)) (end-of-file nil))
          (with-temp-file {json.dumps(str(bytecode))}
            (insert ";;; -*- lexical-binding: t -*-\\n")
            (dolist (form (nreverse forms))
              (eval form t)
              (prin1 (if (eq (car form) 'defun)
                         (let ((fn (byte-compile (cadr form))))
                           (unless (byte-code-function-p fn) (error "not bytecode"))
                           (list 'fset (list 'quote (cadr form)) fn))
                       form) (current-buffer))
              (insert "\\n"))))))'''
    env = dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
               XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
    compilation = run([str(gnu), "-Q", "--batch", "--eval", expression],
                      dest / "compile", env)
    if compilation["returncode"]:
        raise ValueError("bytecode generation failed; see compile.err")
    version = subprocess.check_output([str(gnu), "-Q", "--batch", "--eval",
                                       "(princ emacs-version)"], text=True)
    if version != "31.1":
        raise ValueError(f"GNU reference must be 31.1, got {version}")
    report = {"schema": 1, "head": subprocess.check_output(
        ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "binary": pin(binary), "gnu": pin(gnu), "gnu_version": version,
        "inputs": [pin(FUNCTIONS), pin(DRIVER), pin(bytecode), pin(__file__),
                   *[pin(str(binary) + suffix) for suffix in
                     (".cold", ".native-startup.el")
                     if Path(str(binary) + suffix).exists()]],
        "compilation": compilation, "modes": {}}
    failures = 0
    for mode in MODES:
        mode_env = dict(env, PG_MODE=mode, PG_BYTECODE_FILE=str(bytecode))
        mode_env.pop("NELISP_FX_DISABLE", None)
        if mode == "interpreter":
            mode_env["NELISP_FX_DISABLE"] = "1"
        reference = run([str(gnu), "-Q", "--batch", "--load", str(DRIVER)],
                        dest / f"gnu-{mode}", mode_env)
        candidate = run([str(binary), "--load", str(DRIVER)],
                        dest / f"nelisp-{mode}", mode_env)
        record = {"reference": reference, "candidate": candidate}
        report["modes"][mode] = record
        try:
            diff = compare(reference, candidate)
            if mode == "vm":
                output = inspect_run(candidate)
                for name in CORE:
                    if f"({name} . t)" not in output:
                        raise ValueError(f"VM coverage absent: {name}")
            (dest / f"{mode}.diff").write_text("\n".join(diff) + ("\n" if diff else ""))
            different = sum(1 for line in diff if line.startswith("-") and not line.startswith("---"))
            failures += bool(diff)
            print(f"{mode}: checked={CASES} differing={different} "
                  f"{'FAIL' if diff else 'PASS'}", flush=True)
        except ValueError as error:
            record["error"] = str(error)
            failures += 1
            print(f"{mode}: FAIL {error}", flush=True)
    (dest / "parity.json").write_text(json.dumps(report, indent=2) + "\n")
    return bool(failures)


def check(group, tag):
    if group.startswith("parity-"):
        report = json.loads((OUT / tag / "parity.json").read_text())
        if report["schema"] != 1 or report["gnu_version"] != "31.1":
            raise ValueError("wrong parity report or GNU version")
        for item in [report["binary"], report["gnu"], *report["inputs"]]:
            authenticate(item)
        modes = MODES if group == "parity-all" else (group.removeprefix("parity-"),)
        for mode in modes:
            if compare(report["modes"][mode]["reference"], report["modes"][mode]["candidate"]):
                raise ValueError(f"{mode}: GNU semantic difference; see {OUT / tag / (mode + '.diff')}")
            if mode == "vm":
                text = inspect_run(report["modes"][mode]["candidate"])
                if any(f"({name} . t)" not in text for name in CORE):
                    raise ValueError("VM coverage absent")
        print(f"{group}: PASS ({CASES * len(modes)} cases)")
        return
    # Revalidate raw artifacts without repeating expensive builds/profiles.
    result = subprocess.run([sys.executable, str(ROOT / 'tools/ai/prim-globals-accept.py'), group],
                            cwd=ROOT, capture_output=True, text=True, timeout=45)
    if result.returncode:
        raise ValueError(result.stderr.strip() or result.stdout.strip())
    receipt_path = OUT / "acceptance" / f"{group}.json"
    if not receipt_path.exists():
        raise ValueError(f"{group}: no measurement/verification receipt recorded")
    receipt = json.loads(receipt_path.read_text())
    if (receipt["group"] != group or not receipt["command"]
            or not receipt["inputs"] or not receipt["outputs"]):
        raise ValueError("invalid command receipt")
    for item in receipt["inputs"] + receipt["outputs"]:
        authenticate(item)
    if receipt["returncode"] != 0:
        raise ValueError(f"{group}: command failed")
    print(f"{group}: PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="action", required=True)
    execute = sub.add_parser("parity")
    execute.add_argument("binary")
    execute.add_argument("--gnu", default="emacs")
    execute.add_argument("--tag", default="base")
    verify = sub.add_parser("check")
    verify.add_argument("group")
    verify.add_argument("--tag", default="final3")
    args = parser.parse_args()
    if args.action == "parity":
        return parity(args.binary, args.tag, args.gnu)
    check(args.group, args.tag)
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        sys.exit(1)
