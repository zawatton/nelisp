#!/usr/bin/env python3
"""Measure each floor in a fresh process; include GC without prior-trial heap history.

Instrumentation starts after setup in a persistent REPL. Every row is one
operation plus its following GC, minus the independently measured empty loop.
The legacy sequential harness remains available for comparison.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import importlib.util
import json
import os
from pathlib import Path
import re
import select
import shutil
import subprocess
import time

SPEC = importlib.util.spec_from_file_location("floor_perf", Path(__file__).with_name("prim-callfloor-perf.py"))
P = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(P)
C, ROOT, OUT = P.C, P.ROOT, P.OUT


def measure(binary, tag, gnu, n=300, layouts=(0, 700, 1400, 2100), modes=C.MODES, jobs=4):
    dest = OUT / tag
    dest.mkdir(parents=True, exist_ok=True)
    source, compiled, driver = P.prepare(dest, gnu)
    text = driver.read_text()
    setup = dest / "setup.el"
    setup.write_text(";;; -*- lexical-binding: t -*-\n(defvar pcf-iterations nil)\n" + text[:text.index("\n(let ((n 0))")] + '\n(princ "PCF-READY\\n")\n')
    binary = str(Path(binary).resolve())
    is_gnu = binary == str(Path(gnu).resolve())
    qualified_binary = C.pin(binary)
    companions = []
    if not is_gnu:
        # Equal-length evidence tags and the same executable basename keep
        # invocation/source-path allocation sizes equal between A and B.
        runtime = dest / "runtime"
        runtime.mkdir(exist_ok=True)
        alias = runtime / "nelisp"
        shutil.copy2(binary, alias)
        for suffix in (".cold", ".native-startup.el"):
            origin = Path(binary + suffix)
            if origin.exists():
                copied = Path(str(alias) + suffix)
                shutil.copy2(origin, copied)
                companions.append(C.pin(copied))
        binary = str(alias)
        if C.pin(binary)["sha256"] != qualified_binary["sha256"]:
            raise ValueError("benchmark copy differs from qualification binary")
    compiled_program = C.pin(compiled)
    equality = "(progn (load " + json.dumps(str(compiled)) + " nil t) (let ((a (symbol-function 'pcf-let-special)) (b (symbol-function 'pcf-while))) (princ (equal (list (aref a 0) (aref a 1) (aref a 2) (aref a 3)) (list (aref b 0) (aref b 1) (aref b 2) (aref b 3))))))"
    proof = subprocess.run([gnu, "-Q", "--batch", "--eval", equality], capture_output=True, check=True, timeout=30)
    equivalents = {"let-special": "while"} if proof.stdout == b"t" and not proof.stderr else {}
    gc = "Fgarbage_collect" if is_gnu else "0x" + re.search(
        r"^([0-9a-f]+) T bf_garbage_collect_w$",
        subprocess.check_output(["nm", binary], text=True), re.M)[1]
    report = {"binary": C.pin(binary), "gnu": C.pin(gnu), "n": n,
              "method": __doc__, "isolated": True, "qualified_binary": qualified_binary,
              "binary_companions": companions, "compiled_program": compiled_program,
              "bytecode_equivalents": equivalents,
              "inputs": [C.pin(__file__), C.pin(P.__file__), C.pin(source), C.pin(compiled), C.pin(setup)],
              "runs": []}

    def trial(mode, layout, op):
        stem = dest / f"{mode}-{layout}-{op}"
        if Path(str(stem) + ".cg.2").exists():
            raise FileExistsError(f"use a fresh evidence tag: {stem}")
        env = dict(os.environ, PCF_MODE=mode, PCF_N=str(n), ABI_LAYOUT_PAD="x" * layout,
                   TMPDIR=str(ROOT / "target/tmp"), XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
        env.pop("NELISP_FX_DISABLE", None)
        env.pop("PCF_PROFILE", None)
        if mode == "interpreter":
            env["NELISP_FX_DISABLE"] = "1"
        prefix = str(stem) + ".vgdb"
        invocation = ([binary, "-Q", "--batch", "--load", str(setup), "--eval",
                       '(let ((input "")) (while (equal input "") (setq input (read-string ""))) (eval (car (read-from-string input))))'] if is_gnu else
                      [binary, "--repl", "--no-prompt", "--no-print"])
        command = ["setarch", os.uname().machine, "-R", "valgrind", "--tool=callgrind", "--instr-atstart=" + ("yes" if is_gnu else "no"), "--error-exitcode=97",
                   "--vgdb-prefix=" + prefix, "--dump-after=" + gc,
                   "--callgrind-out-file=" + str(stem) + ".cg", *invocation]
        count = 0 if op == "control" else n
        select_function = f"(setq pcf-iterations (string-to-number {json.dumps(str(count))}))"
        body = f"(pcf-{op} pcf-iterations)"
        form = f'(progn {select_function} (garbage-collect) {body} (garbage-collect) (princ "PCF-COMPLETE 1\\n") (kill-emacs 0))\n'
        stdout, stderr = Path(str(stem) + ".out"), Path(str(stem) + ".err")
        started = time.monotonic()
        captured = bytearray()
        with stderr.open("wb") as err:
            process = subprocess.Popen(command, cwd=ROOT, env=env, stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE, stderr=err)
            try:
                if not is_gnu:
                    process.stdin.write(f"(load {json.dumps(str(setup))} nil t)\n".encode())
                    process.stdin.flush()
                deadline = time.monotonic() + 120
                while b"PCF-READY\n" not in captured:
                    remaining = deadline - time.monotonic()
                    if remaining <= 0 or not select.select([process.stdout], [], [], remaining)[0]:
                        raise TimeoutError(f"setup: {stem}")
                    chunk = os.read(process.stdout.fileno(), 65536)
                    if not chunk:
                        raise RuntimeError(f"setup exited: {stem}")
                    captured.extend(chunk)
                if is_gnu:
                    # GNU startup is cheap; avoid debugger control while its
                    # minibuffer reader blocks in libc. The first two dumps
                    # are setup GC and the explicit measurement boundary.
                    Path(str(stem) + ".control.out").write_text("OK. instrumentation enabled at start\n")
                    Path(str(stem) + ".control.err").write_text("")
                else:
                    control = subprocess.Popen(["callgrind_control", "--vgdb-prefix=" + prefix,
                                                "--instr=on", str(process.pid)],
                                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                    try:
                        # The control client may start after the first wakeup
                        # under load. Keep servicing it within the same deadline.
                        deadline = time.monotonic() + 15
                        while True:
                            remaining = deadline - time.monotonic()
                            if remaining <= 0:
                                raise subprocess.TimeoutExpired(control.args, 15)
                            process.stdin.write(b"\n")
                            process.stdin.flush()
                            try:
                                co, ce = control.communicate(timeout=min(.1, remaining))
                                break
                            except subprocess.TimeoutExpired:
                                pass
                        Path(str(stem) + ".control.out").write_bytes(co)
                        Path(str(stem) + ".control.err").write_bytes(ce)
                        if control.returncode or b"OK." not in co:
                            raise RuntimeError(f"instrumentation was not enabled: {stem}")
                    finally:
                        if control.poll() is None:
                            control.kill()
                            control.wait()
                process.stdin.write(form.encode())
                process.stdin.flush()
                extra, _ = process.communicate(timeout=120)
                captured.extend(extra)
                stdout.write_bytes(captured)
                if process.returncode or captured.count(b"PCF-COMPLETE 1\n") != 1:
                    raise RuntimeError(f"profile failed: {stem}")
            finally:
                if process.poll() is None:
                    process.kill()
                    process.wait()
        if "nelisp: uncaught error:" in stderr.read_text():
            raise RuntimeError(f"uncaught profile error: {stem}")
        profile = Path(str(stem) + (".cg.3" if is_gnu else ".cg.2"))
        raw = profile.read_text()
        ir = int(re.search(r"^summary: (\d+)$", raw, re.M)[1])
        if ir <= 0 or "Trigger: " not in raw or "Program termination" in raw:
            raise ValueError(f"missing measured GC interval: {stem}")
        wall_form = f'(progn {select_function} (garbage-collect) (let ((started (float-time))) {body} (princ (format "PCF-TIME %.9f\\n" (- (float-time) started)))) (garbage-collect) (princ "PCF-COMPLETE 1\\n") (kill-emacs 0))'
        wall_cmd = ["setarch", os.uname().machine, "-R", binary] + (["-Q", "--batch"] if is_gnu else []) + ["--load", str(setup), "--eval", wall_form]
        wall = subprocess.run(wall_cmd, cwd=ROOT, env=env, capture_output=True, timeout=120)
        wo, we = Path(str(stem) + ".wall.out"), Path(str(stem) + ".wall.err")
        wo.write_bytes(wall.stdout)
        we.write_bytes(wall.stderr)
        clock = re.search(rb"PCF-TIME ([0-9.]+)", wall.stdout)
        if wall.returncode or wall.stderr or not clock or wall.stdout.count(b"PCF-COMPLETE 1\n") != 1:
            raise RuntimeError(f"wall trial failed: {stem}")
        return {"op": op, "iterations": count, "ir": ir, "seconds": float(clock[1]), "profile": C.pin(profile),
                "stdout": C.pin(stdout), "stderr": C.pin(stderr),
                "wall_stdout": C.pin(wo), "wall_stderr": C.pin(we), "command": command,
                "returncode": process.returncode, "wall_returncode": wall.returncode,
                "control_stdout": C.pin(str(stem) + ".control.out"),
                "control_stderr": C.pin(str(stem) + ".control.err"),
                "elapsed_s": time.monotonic() - started}

    with ThreadPoolExecutor(max_workers=jobs) as pool:
        for mode in modes:
            for layout in layouts:
                ops = [op for op in P.OPS if not (mode == "bytecode" and op in equivalents)]
                measured = dict(zip(ops, pool.map(lambda op: trial(mode, layout, op), ops)))
                rows = []
                for op in P.OPS:
                    if mode == "bytecode" and op in equivalents:
                        # The compiler emitted the identical program. Reuse
                        # that exact measurement, with the equivalence explicit.
                        row = dict(measured[equivalents[op]], op=op, same_program_as=equivalents[op])
                    else:
                        row = measured[op]
                    rows.append(row)
                control = rows[0]
                empty = next(row for row in rows if row["op"] == "while")
                for row in rows[1:]:
                    baseline = control if row["op"] in ("while", "dotimes") else empty
                    row["ir_per_op"] = (row["ir"] - baseline["ir"]) / n
                    row["us_per_op"] = 1e6 * (row["seconds"] - baseline["seconds"]) / n
                report["runs"].append({"mode": mode, "layout": layout, "intervals": rows})
                (dest / "perf.json").write_text(json.dumps(report, indent=2) + "\n")
                print(tag, mode, layout, {r["op"]: round(r["ir_per_op"], 3) for r in rows[1:]}, flush=True)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary")
    parser.add_argument("tag")
    parser.add_argument("--gnu", default=shutil.which("emacs"))
    parser.add_argument("--n", type=int, default=300)
    parser.add_argument("--layout", type=int, action="append")
    parser.add_argument("--mode", choices=C.MODES, action="append")
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()
    measure(args.binary, args.tag, args.gnu, args.n, args.layout or (0, 700, 1400, 2100),
            args.mode or C.MODES, args.jobs)
