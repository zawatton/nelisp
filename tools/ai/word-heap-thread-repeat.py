#!/usr/bin/env python3
"""Retain GNU parity repeats and a deterministic legacy root isolation probe."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
CORPUS = ROOT / "tools/ai/word-heap-frames-parity.el"
DYNAMIC = ROOT / "tools/ai/word-heap-thread-dynamic.el"
CASES = ("legacy-threads", "threads", "public-threads") + tuple(
    "dynamic-bindings-" + str(index) for index in range(1, 6))


def pin(path):
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def environment(case):
    env = dict(os.environ)
    for name in ("NELISP_READER_DYNAMIC", "NELISP_STANDALONE_TARGET",
                 "NELISP_WORD_HEAP_PHASE", "ABI_LAYOUT_PAD"):
        env.pop(name, None)
    for name in ("WH_THREAD_DEFINITIONS_ONLY", "WH_THREAD_BYTECODE", "WH_THREAD_DYNAMIC_CASE"):
        env.pop(name, None)
    if case.startswith("dynamic-bindings-"):
        env["WH_THREAD_DYNAMIC_CASE"] = case.rsplit("-", 1)[1]
    env.update(WH_FRAME_CASE=case, NELISP_FX_DISABLE="1",
               WH_FRAME_THREAD_PROVIDER=str(ROOT / "packages/nelisp-emacs-foundation/src"))
    return env


def run(directory, name, command, case="legacy-threads", updates=None):
    started = time.monotonic()
    env = {**environment(case), **(updates or {})}
    process = subprocess.Popen(command, cwd=ROOT, env=env,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               start_new_session=True)
    try:
        stdout, stderr = process.communicate(timeout=30)
        code = process.returncode
    except subprocess.TimeoutExpired:
        # Raw clone workers are separate processes. Kill the whole task group,
        # including a worker spinning after its parent timed out.
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
        code = 124
    paths = [directory / (name + suffix) for suffix in (".out", ".err", ".rc")]
    for path, data in zip(paths, (stdout, stderr, (str(code) + "\n").encode())):
        path.write_bytes(data)
    return {"command": command, "returncode": code,
            "environment": {name: value for name, value in env.items()
                            if name.startswith(("WH_FRAME_", "WH_THREAD_")) or name == "NELISP_FX_DISABLE"},
            "seconds": time.monotonic() - started,
            "stdout": pin(paths[0]), "stderr": pin(paths[1]), "status": pin(paths[2])}


def isolation_probe(binary, output):
    # Resolve only the required names; never emit the binary's symbol table.
    names = {"nl_gc_loop_ctx", "nl_make_thread_env", "nl_root_mark",
             "nl_thread_registry_find", "nl_frame_stack_retire_env"}
    symbols = {}
    result = subprocess.run(["nm", "--defined-only", str(binary)],
                            capture_output=True, text=True, check=True)
    for line in result.stdout.splitlines():
        columns = line.split()
        if len(columns) == 3 and columns[2] in names:
            symbols[columns[2]] = int(columns[0], 16)
    if set(symbols) != names:
        raise RuntimeError("binary lacks the bounded isolation probe symbols")
    s = symbols
    # No clone is needed: the actual legacy constructor must give two EvalCtxs
    # distinct root tops before either context executes concurrently.
    output.write_text(f""";;; -*- lexical-binding: t; -*-
(defun whtr-env ()
  (ptr-read-u64 {s['nl_gc_loop_ctx']}
   (+ 64 (* 56 (1- (ptr-read-u64 {s['nl_gc_loop_ctx']} 0))))))
(let* ((parent (whtr-env)) (child 0) (registered nil) (private nil))
  (unless (= (nelisp-thread-gc-inhibit 1) 1) (error "probe barrier"))
  (setq child (ptr-call {s['nl_make_thread_env']} parent 0 0 0 0 0))
  (unless (> child 4096) (error "probe constructor"))
  (setq registered (> (ptr-call {s['nl_thread_registry_find']} child 0 0 0 0 0) 0))
  (setq private (/= (ptr-call {s['nl_root_mark']} parent 0 0 0 0 0)
                    (ptr-call {s['nl_root_mark']} child 0 0 0 0 0)))
  (ptr-call {s['nl_frame_stack_retire_env']} child 0 0 0 0 0)
  (unless (= (nelisp-thread-gc-inhibit 0) 1) (error "probe end"))
  (princ (format "WH-THREAD-ISOLATION %S\\n" (list registered private)))
  (princ "WH-THREAD-ISOLATION-DONE 1\\n")
  (kill-emacs 0))
""")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dynamic", required=True, type=Path)
    parser.add_argument("--static", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--gnu", default="emacs")
    parser.add_argument("--repeats", type=int, default=50)
    parser.add_argument("--expect-broken", action="store_true")
    args = parser.parse_args()
    if args.repeats < 1:
        parser.error("--repeats must be positive")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    if (output / "summary.json").exists():
        parser.error("use a fresh evidence directory")
    gnu = shutil.which(args.gnu)
    if not gnu:
        parser.error("GNU executable not found")
    summary = {"corpus": pin(CORPUS), "dynamic_fixture": pin(DYNAMIC),
               "checker": pin(Path(__file__).resolve()), "repeats": args.repeats,
               "expect_broken": args.expect_broken, "runs": {}, "binaries": {},
               "gnu": pin(Path(gnu).resolve()),
               "providers": [pin(ROOT / "packages/nelisp-emacs-foundation/src" / name)
                             for name in ("emacs-cc-thread-1.el", "emacs-cc-thread-predicates-1.el", "emacs-cc-thread-2.el")]}
    runs = summary["runs"]
    runs["gnu/version"] = run(output, "gnu-version", [gnu, "--version"])
    failures = []
    version = runs["gnu/version"]
    if version["returncode"] or not Path(version["stdout"]["path"]).read_bytes().startswith(b"GNU Emacs 31.1\n"):
        raise RuntimeError("reference must be GNU Emacs 31.1")
    compiled = output / "dynamic-parent-bytecode.el"
    expression = ("(progn (require 'bytecomp) (let ((print-length nil) (print-level nil) "
                  "(print-escape-nonascii t) (print-escape-control-characters t)) "
                  "(with-temp-file (getenv \"WH_THREAD_BYTECODE\") "
                  "(insert \";;; -*- lexical-binding: t; -*-\\n(fset 'whtr-bytecode-parent '\" "
                  "(prin1-to-string (byte-compile (symbol-function 'whtr-bytecode-parent))) \")\\n\"))))")
    updates = {"WH_THREAD_BYTECODE": str(compiled)}
    runs["gnu/compile-parent"] = run(output, "gnu-compile-parent",
        [gnu, "-Q", "--batch", "--load", str(DYNAMIC), "--eval", expression],
        updates={**updates, "WH_THREAD_DEFINITIONS_ONLY": "1"})
    if runs["gnu/compile-parent"]["returncode"] or not compiled.is_file():
        raise RuntimeError("GNU bytecode parent fixture compilation failed")
    summary["compiled_parent"] = pin(compiled)
    expected = {}
    for case in CASES:
        fixture = DYNAMIC if case.startswith("dynamic-bindings-") else CORPUS
        row = run(output, "gnu-" + case, [gnu, "-Q", "--batch", "--load", str(fixture)], case, updates)
        runs["gnu/" + case] = row
        expected[case] = Path(row["stdout"]["path"]).read_bytes()
        marker = b"WH-THREAD-DYNAMIC-DONE 1\n" if case.startswith("dynamic-bindings-") else b"WH-FRAME-DONE 1\n"
        if row["returncode"] or Path(row["stderr"]["path"]).stat().st_size or marker not in expected[case]:
            failures.append("GNU reference " + case)
    for mode in ("dynamic", "static"):
        binary = getattr(args, mode).resolve()
        summary["binaries"][mode] = [pin(Path(str(binary) + suffix))
                                     for suffix in ("", ".cold", ".native-startup.el")]
        probe = output / (mode + "-isolation.el")
        isolation_probe(binary, probe)
        summary.setdefault("isolation_probes", {})[mode] = pin(probe)
        row = run(output, mode + "-isolation", [str(binary), "--load", str(probe)])
        runs[mode + "/isolation"] = row
        want = b"WH-THREAD-ISOLATION " + (b"(nil nil)" if args.expect_broken else b"(t t)") + b"\nWH-THREAD-ISOLATION-DONE 1\n"
        if row["returncode"] or Path(row["stderr"]["path"]).stat().st_size or Path(row["stdout"]["path"]).read_bytes() != want:
            failures.append(mode + " deterministic isolation")
        for case in CASES:
            count = args.repeats if case == "legacy-threads" or case.startswith("dynamic-bindings-") else 1
            fixture = DYNAMIC if case.startswith("dynamic-bindings-") else CORPUS
            passed = 0
            for trial in range(1, count + 1):
                name = f"{mode}-{case}-{trial:03d}"
                row = run(output, name, [str(binary), "--load", str(fixture)], case, updates)
                row["matches_gnu"] = (row["returncode"] == 0 and not Path(row["stderr"]["path"]).stat().st_size
                                      and Path(row["stdout"]["path"]).read_bytes() == expected[case])
                runs[name] = row
                passed += row["matches_gnu"]
                (output / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
            print(f"{mode} {case}: {passed}/{count}", flush=True)
            if passed != count and not args.expect_broken:
                failures.append(mode + " " + case)
    def verify_pins(item):
        if isinstance(item, dict):
            if "path" in item and "sha256" in item:
                if pin(Path(item["path"])) != item:
                    failures.append("changed evidence/artifact: " + item["path"])
            else:
                for value in item.values():
                    verify_pins(value)
        elif isinstance(item, list):
            for value in item:
                verify_pins(value)
    verify_pins(summary)
    summary["failures"] = failures
    summary["pass"] = not failures
    (output / "summary.json").write_text(json.dumps(summary, indent=2) + "\n")
    print("THREAD-REPEAT " + ("PASS" if not failures else "FAIL: " + ", ".join(failures)))
    return bool(failures)


if __name__ == "__main__":
    raise SystemExit(main())
