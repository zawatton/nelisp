#!/usr/bin/env python3
"""Preserve a real shared-GNU-bytecode N=1 counterexample; never rebuild."""
import hashlib
import json
import os
from pathlib import Path
import resource
import signal
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "target/bytecode-equal-gateway"
MAIN = ROOT.parent / "resume-4943-20261024"


def need(value, message):
    if not value:
        raise ValueError(message)


def pin(path):
    return {"path": str(path.resolve()), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def child(argv, name, env):
    p = subprocess.Popen(list(map(str, argv)), cwd=OUT, env=env, start_new_session=True,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.DEVNULL)
    timed_out = False
    try:
        stdout, stderr = p.communicate(timeout=15)
    except subprocess.TimeoutExpired:
        timed_out = True
        os.killpg(p.pid, signal.SIGKILL)
        stdout, stderr = p.communicate(timeout=3)
    finally:
        try:
            os.killpg(p.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
    (OUT / (name + ".stdout")).write_bytes(stdout)
    (OUT / (name + ".stderr")).write_bytes(stderr)
    receipt = {"argv": list(map(str, argv)), "rc": p.returncode, "timeout": timed_out,
               "stdout": pin(OUT / (name + ".stdout")), "stderr": pin(OUT / (name + ".stderr"))}
    (OUT / (name + ".json")).write_text(json.dumps(receipt, indent=2) + "\n")
    need(p.returncode == 0 and not stderr and not timed_out, name + " exit/stderr/timeout")
    return stdout.decode("ascii"), receipt


def reproduce():
    need(not (OUT / "red.json").exists(), "preserve existing red evidence")
    gnu = Path(os.environ["GNU_EMACS"]).resolve()
    native = MAIN / "target/resume-split-build/dynamic/nelisp-dynamic"
    inputs = [pin(gnu), pin(native), pin(Path(str(native) + ".cold")), pin(__file__ and Path(__file__))]
    expected = ("7a73e7e5db25275f09753fba330b96ecd44c64b3af9c5298fe343a44084bf5d7",
                "0643c3b909e8292b0d4e2aa6e2c3dbac4dfc552609c53b76d6ca9bb8fa1e5c0f",
                "d0c07da4932178e670f27c69ed704746ee34e65688e38ffb2071a8cc0b6e290d")
    need(tuple(x["sha256"] for x in inputs[:3]) == expected, "unexpected executable/image")
    compiler = OUT / "compile.el"
    compiler.write_text(''';;; -*- lexical-binding: t; -*-
(require 'bytecomp)
(let ((default-directory (file-name-directory load-file-name))
      (fn (byte-compile (lambda (a b) (equal a b)))))
 (unless (byte-code-function-p fn) (error "not genuine GNU bytecode"))
 (with-temp-file "shared.el"
  (insert ";;; -*- lexical-binding: t; -*-\\n")
  (insert (format "(fset 'bce-compiled %S)\\n" fn)))
 (with-temp-buffer
  (disassemble fn (current-buffer))
  (write-region (point-min) (point-max) "disassembly" nil 'silent)))
(kill-emacs 0)
''')
    driver = OUT / "probe.el"
    driver.write_text(''';;; -*- lexical-binding: t; -*-
(load (expand-file-name "shared.el" (file-name-directory load-file-name)) nil t)
(unless (byte-code-function-p (symbol-function 'bce-compiled)) (error "wrong executor object"))
(when (equal (getenv "BCE_ROLE") "native")
 (unless (fboundp 'nelisp--equal-reference) (error "missing private provider")))
(defun bce-source (a b) (equal a b))
(defun bce-probe (name a b)
 (princ (format "BCE-INPUT %s %S %s %s\\n" name (eq a b) (type-of a) (type-of b)))
 (princ (format "BCE-RESULT %s %S %S\\n" name (bce-source a b) (bce-compiled a b))))
(bce-probe 'vector (vector 3 5) (vector 3 5))
(bce-probe 'nested (list (vector 3 5) (list 7)) (list (vector 3 5) (list 7)))
(let ((v (vector 3 5))) (bce-probe 'same v v))
(bce-probe 'scalar 17 17)
(bce-probe 'unequal (vector 3 5) (vector 3 7))
(princ "BCE-DONE N=1\\n")
(kill-emacs 0)
''')
    env = dict(os.environ, NELISP_FX_DISABLE="1", LC_ALL="C.UTF-8", PWD=str(OUT))
    compile_out, compile_receipt = child([gnu, "-Q", "--batch", "--load", compiler], "compile", env)
    need(not compile_out, "unexpected compiler stdout")
    inputs += [pin(compiler), pin(driver), pin(OUT / "shared.el"), pin(OUT / "disassembly")]
    env["BCE_ROLE"] = "gnu"
    gs, gr = child([gnu, "-Q", "--batch", "--load", driver], "gnu", env)
    env["BCE_ROLE"] = "native"
    ns, nr = child([native, "--load", driver], "D6", env)
    good = ("BCE-INPUT vector nil vector vector\nBCE-RESULT vector t t\n"
            "BCE-INPUT nested nil cons cons\nBCE-RESULT nested t t\n"
            "BCE-INPUT same t vector vector\nBCE-RESULT same t t\n"
            "BCE-INPUT scalar t integer integer\nBCE-RESULT scalar t t\n"
            "BCE-INPUT unequal nil vector vector\nBCE-RESULT unequal nil nil\nBCE-DONE N=1\n")
    red = good.replace("BCE-RESULT vector t t", "BCE-RESULT vector t nil").replace("BCE-RESULT nested t t", "BCE-RESULT nested t nil")
    need(gs == good, "GNU fixture/protocol mismatch")
    need(ns == red, "D6 differs from isolated expected counterexample")
    for x in inputs:
        need(pin(Path(x["path"])) == x, "input changed")
    (OUT / "red.json").write_text(json.dumps({"status": "ACTUAL_D6_RED", "inputs": inputs,
        "compile": compile_receipt, "gnu": gr, "D6": nr, "N": 1,
        "limits": "Semantic counterexample only; no actual engine/GC/timing qualification."}, indent=2) + "\n")
    print("ACTUAL_D6_RED vector+nested; same/scalar/unequal controls agree")


def main():
    resource.setrlimit(resource.RLIMIT_AS, (8 * 1024**3, 8 * 1024**3))
    phase = sys.argv[1]
    if phase == "reproduce":
        reproduce()
    elif phase == "check-red":
        r = json.loads((OUT / "red.json").read_text())
        need(r["status"] == "ACTUAL_D6_RED", "missing real red")
        for x in r["inputs"] + [r[k][f] for k in ("gnu", "D6", "compile") for f in ("stdout", "stderr")]:
            need(pin(Path(x["path"])) == x, "changed red evidence")
    elif phase == "host":
        p = subprocess.run([os.environ.get("GNU_EMACS", "emacs"), "-Q", "--batch", "-L", "lisp", "-L", "src", "-L", "scripts", "-l", "test/nelisp-bytecode-equal-gateway-test.el", "-f", "ert-run-tests-batch-and-exit"], cwd=ROOT, timeout=15)
        return p.returncode
    else:
        raise ValueError("unknown phase")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, OSError, subprocess.TimeoutExpired) as error:
        print("BCE FAIL: " + str(error), file=sys.stderr)
        sys.exit(1)
