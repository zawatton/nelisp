#!/usr/bin/env python3
"""Focused GNU/standalone regression checks for aliases and saved bindings.

The runner keeps exact output and binary identities in target/prim-shadow.
Each check requires a nonempty completion marker and rejects old artifacts.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-shadow"
BIN = Path(os.environ.get("NELISP_BIN", OUT / "dynamic/nelisp")).resolve()
NAMES = ("init", "nested", "gc", "default", "mixed", "void", "bound",
         "general-lean", "lean-general", "alias", "vm")


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def run(label, argv, env=None, timeout=180):
    start = time.monotonic()
    result = subprocess.run(list(map(str, argv)), cwd=ROOT, capture_output=True,
                            env=dict(os.environ, TMPDIR=str(ROOT / "target/tmp"),
                                     LC_ALL="C.UTF-8", **(env or {})), timeout=timeout)
    (OUT / f"{label}.stdout").write_bytes(result.stdout)
    (OUT / f"{label}.stderr").write_bytes(result.stderr)
    assert result.returncode == 0, (label, result.returncode, f"{label}.stderr")
    return result.stdout, time.monotonic() - start


def receipt(name, paths, **extra):
    data = {"binary": str(BIN), "binary_sha256": sha(BIN),
            "cold_sha256": sha(str(BIN) + ".cold"), "pass": True,
            "files": {str(Path(p).resolve().relative_to(ROOT)): sha(p) for p in paths}, **extra}
    (OUT / f"{name}.json").write_text(json.dumps(data, indent=2) + "\n")


def auth(name):
    data = json.loads((OUT / f"{name}.json").read_text())
    assert data["pass"] and data["binary_sha256"] == sha(BIN)
    assert data["cold_sha256"] == sha(str(BIN) + ".cold")
    assert data["files"]
    for path, digest in data["files"].items():
        assert sha(ROOT / path) == digest, path
    print(name, "PASS")
    return data


def contract():
    source = ROOT / "test/perf/prim-shadow-corpus.el"
    outputs = []
    for label, cmd in (("gnu-contract", ["emacs", "-Q", "--batch", "-l", source]),
                       ("native-contract", [BIN, "--load", source])):
        output, _ = run(label, cmd)
        assert output.splitlines()[-1] == b"SHADOW-CONTRACT-COMPLETE 12"
        outputs.append(output)
    assert outputs[0] == outputs[1], "contract: exact GNU output differs"
    assert ([x for x in (OUT / "native-contract.stderr").read_bytes().splitlines() if x] ==
            [x for x in (OUT / "gnu-contract.stderr").read_bytes().splitlines() if x])
    receipt("contract", [source, OUT / "gnu-contract.stdout", OUT / "native-contract.stdout"])
    print("SHADOW-CONTRACT PASS checked=12")


def defvar(mode):
    source = ROOT / "test/perf/prim-shadow-functions.el"
    compiled = OUT / "prim-shadow-functions.el"
    compiled.write_bytes(source.read_bytes())
    run("bytecompile", ["emacs", "-Q", "--batch", "-f", "batch-byte-compile", compiled])
    target = compiled.with_suffix(".elc") if mode == "bytecode" else source
    forms = f'(load {json.dumps(str(target))} nil t)\n'
    names = " ".join("ps-shadow-" + n for n in NAMES)
    if mode == "vm":
        forms += f"""(when (fboundp 'nelisp--fx-register)
          (dolist (fn '({names} ps-shadow-inner ps-shadow-initialize)) (nelisp--fx-register fn))
          (unless (vectorp (gethash (nthcdr 3 (symbol-function 'ps-shadow-initialize)) nelisp--fx-table))
            (error "defvar VM helper was not compiled")))\n"""
    forms += f"""(dolist (fn '({names}))
      (makunbound 'ps-shadow)
      (when (eq fn 'ps-shadow-bound) (set 'ps-shadow 99))
      (prin1 (list fn (funcall fn) (boundp 'ps-shadow)
                    (condition-case e ps-shadow (error e)))) (terpri))
    (princ "DEFVAR-COMPLETE {len(NAMES)}\n")
    (kill-emacs 0)
    """
    driver = OUT / f"defvar-{mode}.el"
    driver.write_text(forms)
    gnu, _ = run(f"gnu-defvar-{mode}", ["emacs", "-Q", "--batch", "-l", driver])
    native, seconds = run(f"native-defvar-{mode}", [BIN, "--load", driver],
                          {} if mode == "vm" else {"NELISP_FX_DISABLE": "1"})
    assert gnu.splitlines()[-1] == f"DEFVAR-COMPLETE {len(NAMES)}".encode()
    assert gnu == native, f"defvar {mode}: exact GNU output differs"
    assert not (OUT / f"native-defvar-{mode}.stderr").read_bytes()
    receipt(f"defvar-{mode}", [source, driver, target,
            OUT / f"gnu-defvar-{mode}.stdout", OUT / f"native-defvar-{mode}.stdout"],
            seconds=seconds)
    print(f"DEFVAR PASS mode={mode} checked={len(NAMES)} seconds={seconds:.4f}")


def guard():
    source = ROOT / "test/perf/prim-shadow-guard.el"
    gnu, _ = run("gnu-guard", ["emacs", "-Q", "--batch", "-l", source])
    native, seconds = run("native-guard", [BIN, "--load", source])
    assert gnu.splitlines()[-1] == b"SHADOW-GUARD-COMPLETE 24"
    assert gnu == native, "guard: exact GNU output differs"
    assert ([x for x in (OUT / "native-guard.stderr").read_bytes().splitlines() if x] ==
            [x for x in (OUT / "gnu-guard.stderr").read_bytes().splitlines() if x])
    receipt("guard", [source, OUT / "gnu-guard.stdout", OUT / "native-guard.stdout",
                      OUT / "gnu-guard.stderr", OUT / "native-guard.stderr"],
            seconds=seconds)
    print("SHADOW-GUARD PASS checked=24")


def generation():
    driver = OUT / "generation.el"
    driver.write_text("""(aset nelisp--fx-special-gen 0 0)
    (set 'ps-generation-base 91)
    (defvaralias 'ps-generation-source 'ps-generation-base)
    (setq ps-generation-one (aref nelisp--fx-special-gen 0))
    (makunbound 'ps-generation-source)
    (setq ps-generation-two (aref nelisp--fx-special-gen 0))
    (defvaralias 'ps-generation-source 'ps-generation-base)
    (setq ps-generation-three (aref nelisp--fx-special-gen 0))
    (internal-delete-indirect-variable 'ps-generation-source)
    (setq ps-generation-four (aref nelisp--fx-special-gen 0))
    (unless (and (< 0 ps-generation-one)
                 (< ps-generation-one ps-generation-two)
                 (< ps-generation-two ps-generation-three)
                 (< ps-generation-three ps-generation-four))
      (error "Alias generation did not advance from Lisp integer zero"))
    (princ "GENERATION-PRODUCER PASS checked=4\\n")
    (kill-emacs 0)
    """)
    output, _ = run("generation", [BIN, "--load", driver])
    assert output == b"GENERATION-PRODUCER PASS checked=4\n"
    assert not (OUT / "generation.stderr").read_bytes()
    receipt("generation", [driver, OUT / "generation.stdout"])
    print("GENERATION-PRODUCER PASS checked=4")


def corpus():
    source = ROOT / "tools/nelisp-substrate-parity-corpus.el"
    driver = OUT / "corpus-row65.el"
    driver.write_text(f'''(load {json.dumps(str(source))} nil t)
    (unless (= (eval (nth 2 (assq 65 nelisp-substrate-parity-corpus)) nil) 1)
      (error "Defvar corpus row 65 failed"))
    (princ "CORPUS-ROW65 PASS\\n")
    (kill-emacs 0)
    ''')
    for label, argv in (("gnu-corpus-row65", ["emacs", "-Q", "--batch", "-l", driver]),
                        ("native-corpus-row65", [BIN, "--load", driver])):
        output, _ = run(label, argv)
        assert output == b"CORPUS-ROW65 PASS\n"
    receipt("corpus", [source, driver, OUT / "gnu-corpus-row65.stdout",
                       OUT / "native-corpus-row65.stdout"])
    print("CORPUS-ROW65 PASS")


def reload():
    driver = OUT / "reload.el"
    driver.write_text("""(setq ps-reload-state 73)
    (setq ps-raw-fset (symbol-function 'nelisp--fx-plain-fset))
    (dotimes (_ 2) (load "scripts/nelisp-stdlib-prelude.el" nil t))
    (unless (and (= ps-reload-state 73)
                 (eq ps-raw-fset (symbol-function 'nelisp--fx-plain-fset)))
      (error "reload changed state or captured its wrapper"))
    (defun ps-reload-call () (+ ps-reload-state 1))
    (unless (= (ps-reload-call) 74) (error "afset publication failed"))
    (unless (nelisp--fx-active-p) (error "FX was disabled"))
    (princ "PRELUDE-RELOAD PASS checked=4\n")
    (kill-emacs 0)
    """)
    output, seconds = run("reload", [BIN, "--load", driver])
    assert output == b"PRELUDE-RELOAD PASS checked=4\n"
    assert not (OUT / "reload.stderr").read_bytes()
    receipt("reload", [driver, ROOT / "scripts/nelisp-stdlib-prelude.el",
                       OUT / "reload.stdout"], seconds=seconds)
    print("PRELUDE-RELOAD PASS", seconds)


def alias_module():
    spec = importlib.util.spec_from_file_location("alias", ROOT / "test/perf/prim-alias-check.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main(action):
    OUT.mkdir(parents=True, exist_ok=True)
    if action in ("contract", "gnu-watchers"):
        contract()
    elif action == "guard":
        guard()
        generation()
    elif action == "generation":
        generation()
    elif action == "corpus":
        corpus()
    elif action == "defvar-all":
        for mode in ("interpreter", "vm", "bytecode"):
            defvar(mode)
    elif action.startswith("defvar-"):
        defvar(action.removeprefix("defvar-"))
    elif action.startswith("reload"):
        reload()
    elif action == "dynamic-link":
        assert (ROOT / "target/nelisp").resolve() == BIN
    elif action == "shadow-smoke":
        output, _ = run("shadow-smoke", ["make", "standalone-reader-shadow-smoke",
                                        f"STANDALONE_BIN={BIN}"], timeout=600)
        assert b"[shadow-smoke] PASS:" in output
        receipt("shadow-smoke", [OUT / "shadow-smoke.stdout"])
    elif action == "reference-costs":
        module = alias_module()
        reference = json.loads((OUT / "reference-cost.json").read_text())
        for mode in ("interpreter", "vm", "bytecode"):
            old = reference["modes"][mode]
            cap = old["candidate_alias_reference"] / old["candidate_plain_reference"]
            module.check_cost(mode)
            data = json.loads((module.OUT / "cost.json").read_text())["modes"][mode]
            assert data["candidate_alias_reference"] / data["candidate_plain_reference"] <= cap
    elif action == "census":
        alias_module().check_census()
        spec = importlib.util.spec_from_file_location("census", ROOT / "tools/ai/box-census.py")
        census = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(census)
        summary, _ = census.tally(census.load(BIN.parent / "census.tsv"))
        assert sum(summary["by_unit"].values()) <= 1362
    elif action == "load-five":
        alias_module().check_load()
    elif action in ("builds", "inventory", "final", "delivery",
                    "guard-negative", "witness-negative"):
        auth(action)
    else:
        raise ValueError(action)


if __name__ == "__main__":
    main(sys.argv[1])
