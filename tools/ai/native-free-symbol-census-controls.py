#!/usr/bin/env python3
"""Compile a genuine free-variable fixture and prove the final preflight fails.

The fixtures are linked only to resolve the names read by objdump. They are
never executed: the deliberately uninitialized hidden slots are the defect.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = b""";;; Census controls. -*- lexical-binding: t; -*-
(require 'nelisp-aot-compiler)
(dolist (mode '(free bound))
  ;; The compiler now rejects names unbound on the build host, as GNU
  ;; bytecomp.el's free-variable check does.  A name the host binds but the
  ;; native runtime never binds still takes the hidden-slot lookup, which is
  ;; the defect this post-link census must catch.  Give it a host global value
  ;; (not special) for the free fixture only; the bound fixture is unchanged.
  (if (eq mode 'free) (set 'wh_census_true_unbound 0)
    (makunbound 'wh_census_true_unbound))
  (nelisp-aot-compile-to-object
   `(seq (defun wh_census_fixture ()
           ,(if (eq mode 'free) 'wh_census_true_unbound
              '(let ((wh_census_true_unbound 7)) wh_census_true_unbound))))
   (expand-file-name (concat (symbol-name mode) ".o") (getenv "WH_CENSUS_OUTPUT"))
   :arch 'x86_64 :format 'elf))
"""


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--emacs", default="emacs")
    args = parser.parse_args()
    output = args.output.resolve()
    output.relative_to(ROOT)
    output.mkdir(parents=True, exist_ok=False)
    before = time.monotonic()
    results = {}

    def run(name, command, expected=0, env=None):
        result = subprocess.run(command, cwd=ROOT, capture_output=True, env=env, timeout=120)
        (output / (name + ".out")).write_bytes(result.stdout)
        (output / (name + ".err")).write_bytes(result.stderr)
        results[name] = {"command": command, "exit": result.returncode}
        if result.returncode != expected:
            raise ValueError(f"{name}: expected exit {expected}, got {result.returncode}; see {output}")
        return result.stdout.decode()

    fixture = output / "fixture.el"
    fixture.write_bytes(FIXTURE)
    import os
    run("compile", [args.emacs, "-Q", "--batch", "-L", "lisp", "-L", "src", "-l", str(fixture)],
        env=dict(os.environ, WH_CENSUS_OUTPUT=str(output)))
    for mode, expected in (("free", 1), ("bound", 0)):
        obj = output / (mode + ".o")
        names = [line.split()[-1] for line in run(mode + "-imports", ["nm", "-u", str(obj)]).splitlines()]
        names = sorted(set(names) | {"nl_alloc_symbol", "nelisp_env_lookup_value"})
        if any(not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9]*", name) for name in names):
            raise ValueError("Unexpected fixture import name")
        stubs = output / (mode + "-stubs.c")
        stubs.write_text("".join("long " + name + "(void) { return 0; }\n" for name in names))
        stub_obj = output / (mode + "-stubs.o")
        binary = output / mode
        run(mode + "-stubs", ["cc", "-c", "-fno-builtin", "-o", str(stub_obj), str(stubs)])
        run(mode + "-link", ["ld", "-e", "wh_census_fixture", "-o", str(binary), str(obj), str(stub_obj)])
        text = run(mode + "-census", ["python3", "tools/ai/native-free-symbol-census.py",
                   "check", str(binary), "--max", "0"], expected)
        if f"sites={expected} max=0" not in text:
            raise ValueError("Wrong census count: " + mode)
    rejected = run("final-gate-negative", ["bash", "tools/ai/word-heap-final-checks.sh", str(output / "free"),
        str(output / "bound"), "native-free-symbol-control"], 1)
    if "sites=1 max=0" not in rejected or "FAIL: native free-symbol preflight" not in rejected:
        raise ValueError("Final gate did not reject the free-symbol defect")
    stripped = output / "free-stripped"
    shutil.copy2(output / "free", stripped)
    run("strip", ["strip", "--strip-all", str(stripped)])
    run("stripped-census", ["python3", "tools/ai/native-free-symbol-census.py",
                           "check", str(stripped), "--max", "0"], 1)
    if "missing native symbol coverage" not in (output / "stripped-census.err").read_text():
        raise ValueError("Stripped fixture was not rejected for missing coverage")
    rejected = run("stripped-final-gate", ["bash", "tools/ai/word-heap-final-checks.sh",
                   str(stripped), str(output / "bound"), "native-free-symbol-stripped-control"], 1)
    if "missing native symbol coverage" not in rejected or "FAIL: native free-symbol preflight" not in rejected:
        raise ValueError("Final gate did not reject the stripped free-symbol defect")
    partial = output / "free-partially-stripped"
    shutil.copy2(output / "free", partial)
    run("partial-strip", ["strip", "--strip-all", "--keep-symbol=wh_census_fixture", str(partial)])
    run("partial-census", ["python3", "tools/ai/native-free-symbol-census.py",
                          "check", str(partial), "--max", "0"], 1)
    if "missing native symbol coverage" not in (output / "partial-census.err").read_text():
        raise ValueError("Partial stripping was not rejected for missing detector anchors")
    rejected = run("partial-final-gate", ["bash", "tools/ai/word-heap-final-checks.sh",
                   str(partial), str(output / "bound"), "native-free-symbol-partial-control"], 1)
    if "missing native symbol coverage" not in rejected or "FAIL: native free-symbol preflight" not in rejected:
        raise ValueError("Final gate did not reject the partially stripped defect")
    report = {"fixtures": {mode: hashlib.sha256((output / mode).read_bytes()).hexdigest()
                            for mode in ("free", "bound", "free-stripped", "free-partially-stripped")},
              "inputs": {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest()
                         for path in (Path(__file__), ROOT / "tools/ai/native-free-symbol-census.py",
                                      ROOT / "tools/ai/word-heap-final-checks.sh")},
              "seconds": time.monotonic() - before, "results": results, "verdict": "PASS"}
    (output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(f"FREE-SYMBOL CONTROLS PASS checked=7 rejected=6 seconds={report['seconds']:.3f}")


if __name__ == "__main__":
    main()
