#!/usr/bin/env python3
"""Measure warmed globals, retaining complete Callgrind intervals including GC.

Each operation has the same loop as a control. Subtract the control interval
to remove loop/measurement overhead and the collection of the preexisting heap;
collections and allocations caused by the operation remain in its marginal Ir.
No private call edges are skipped, so these profiles also support diagnosis.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
OPS = {"control": "17", "varref": "pgb-global",
       "symbol-value": "(symbol-value 'pgb-global)",
       "setq": "(setq pgb-global (1+ pgb-global))",
       "set": "(set 'pgb-global (1+ pgb-global))",
       "funcall": "(funcall pgb-empty)"}
LAYOUTS = (0, 700, 1400, 2100)
DUMP_START = 2


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}


def prepare(dest, gnu):
    source = dest / "functions.el"
    source.write_text(";;; -*- lexical-binding: t -*-\n"
                      "(defvar pgb-global 17)\n(defvar pgb-empty (lambda () nil))\n" +
                      "\n".join(f"(defun pgb-{op} (n) (let ((i 0) (result nil)) "
                                 f"(while (< i n) (setq result {body}) (setq i (1+ i))) result))"
                                 for op, body in OPS.items()) + "\n")
    compiled = dest / "bytecode.el"
    expr = f'''(progn (require 'bytecomp)
      (with-temp-buffer (insert-file-contents {json.dumps(str(source))})
       (check-parens) (goto-char (point-min))
       (let (forms) (condition-case nil
        (while t (push (read (current-buffer)) forms)) (end-of-file nil))
        (with-temp-file {json.dumps(str(compiled))}
         (insert ";;; -*- lexical-binding: t -*-\\n")
         (dolist (form (nreverse forms)) (eval form t)
          (prin1 (if (eq (car form) 'defun)
           (list 'fset (list 'quote (cadr form)) (byte-compile (cadr form))) form)
           (current-buffer)) (insert "\\n"))))))'''
    subprocess.run([gnu, "-Q", "--batch", "--eval", expr], check=True,
                   capture_output=True)
    driver = dest / "driver.el"
    driver.write_text(";;; -*- lexical-binding: t -*-\n" +
        f"(load (if (equal (getenv \"PG_MODE\") \"bytecode\") "
        f"{json.dumps(str(compiled))} {json.dumps(str(source))}) nil t)\n" +
        "(when (and (equal (getenv \"PG_MODE\") \"vm\") "
        "(fboundp 'nelisp--fx-register))\n" +
        " (dolist (name '(" + " ".join("pgb-" + op for op in OPS) +
        ")) (nelisp--fx-register name))\n" +
        " (dolist (name '(" + " ".join("pgb-" + op for op in OPS) +
        ")) (unless (vectorp (gethash (nthcdr 3 (symbol-function name)) nelisp--fx-table)) "
        "(error \"Missing VM coverage: %S\" name))) (princ \"PERF-VM-COVERED 6\\n\"))\n" +
        "\n".join(f"(pgb-{op} 1)" for op in OPS) + "\n" +
        "(garbage-collect)\n" +
        "\n".join(f"(let ((n (string-to-number (getenv \"PG_N\")))) "
                   f"(if (getenv \"PG_PROFILE\") (pgb-{op} n) "
                   f"(let* ((t0 (float-time)) (v (pgb-{op} n))) "
                   f"(princ (format \"PERF {op} %S %.9f\\n\" v "
                   f"(- (float-time) t0))))))\n(garbage-collect)" for op in OPS) +
        "\n(princ \"PERF-COMPLETE 6\\n\")\n(kill-emacs 0)\n")
    return driver


def measure(binary, tag, gnu, n, layouts, jobs=4):
    if not 1 <= jobs <= 8:
        raise ValueError("jobs must be between 1 and 8")
    dest = ROOT / "target/prim-globals" / tag
    dest.mkdir(parents=True, exist_ok=True)
    driver = prepare(dest, gnu)
    binary = str(Path(binary).resolve())
    is_gnu = binary == str(Path(gnu).resolve())
    report = {"binary": pin(binary), "inputs": [pin(__file__), pin(driver),
               pin(dest / "functions.el"), pin(dest / "bytecode.el")],
              "n": n, "method": __doc__, "dump_start": DUMP_START, "runs": []}
    def trial(job):
        mode, layout = job
        stem = dest / f"{mode}-{layout}"
        env = dict(os.environ, PG_MODE=mode, PG_N=str(n), ABI_LAYOUT_PAD="x" * layout,
                   TMPDIR=str(ROOT / "target/tmp"), XDG_CACHE_HOME=str(ROOT / "target/xdg-cache"))
        env.pop("NELISP_FX_DISABLE", None)
        env.pop("PG_PROFILE", None)
        if mode == "interpreter":
            env["NELISP_FX_DISABLE"] = "1"
        gc = "Fgarbage_collect"
        if not is_gnu:
            symbols = subprocess.check_output(["nm", binary], text=True)
            address = re.search(r"^([0-9a-f]+) T bf_garbage_collect_w$", symbols, re.M)
            if not address:
                raise ValueError("missing GC boundary symbol")
            # Generated ELF files expose address names to Valgrind.
            gc = f"0x{int(address[1], 16):016x}"
        command = ["valgrind", "--tool=callgrind", "--error-exitcode=97",
                   f"--dump-after={gc}", f"--callgrind-out-file={stem}.cg",
                   binary, *(["-Q", "--batch"] if is_gnu else []),
                   "--load", str(driver)]
        started = time.monotonic()
        with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
            result = subprocess.run(command, cwd=ROOT, env=dict(env, PG_PROFILE="1"),
                                    stdout=out, stderr=err, timeout=600)
        if result.returncode:
            raise RuntimeError(f"exit {result.returncode}: {stem}.err")
        output = stem.with_suffix(".out").read_text()
        if "PERF-COMPLETE 6" not in output:
            raise ValueError(f"missing completion marker: {stem}")
        wall = subprocess.run([binary, *(["-Q", "--batch"] if is_gnu else []),
                               "--load", str(driver)], cwd=ROOT, env=env,
                              capture_output=True, text=True, timeout=180, check=True)
        if wall.stderr or "PERF-COMPLETE 6" not in wall.stdout:
            raise ValueError(f"invalid wall run: {stem}")
        wall_path = Path(str(stem) + ".wall.out")
        wall_path.write_text(wall.stdout)
        intervals = []
        for i, op in enumerate(OPS, DUMP_START):
            profile = Path(str(stem) + f".cg.{i}")
            match = re.search(r"^summary: (\d+)", profile.read_text(), re.M)
            if not match:
                raise ValueError(f"missing interval: {profile}")
            timing = re.search(rf"^PERF {op} .* ([0-9.]+)$", wall.stdout, re.M)
            intervals.append({"op": op, "ir": int(match[1]),
                              "seconds": float(timing[1]), "profile": pin(profile)})
        control = intervals[0]
        for row in intervals:
            row["ir_per_op"] = (row["ir"] - control["ir"]) / n
            row["us_per_op"] = 1e6 * (row["seconds"] - control["seconds"]) / n
            if DUMP_START > 2 and row['op'] != 'control' and row['ir_per_op'] <= 0:
                raise ValueError(f"unstable zero-iteration control: {stem} {row['op']}")
        record = {"mode": mode, "layout": layout, "command": command,
                  "profile_environment": {"PG_PROFILE": "1", "PG_MODE": mode,
                                          "PG_N": str(n), "ABI_LAYOUT_PAD_BYTES": layout},
                  "elapsed": time.monotonic() - started, "intervals": intervals,
                  "wall_stdout": pin(wall_path),
                  "stdout": pin(stem.with_suffix(".out")), "stderr": pin(stem.with_suffix(".err"))}
        return record
    # Each mode/layout owns its files and process. Preserve report ordering;
    # only the parent publishes complete, validated rows.
    matrix = [(mode, layout) for mode in ("interpreter", "vm", "bytecode") for layout in layouts]
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        for record in pool.map(trial, matrix):
            report["runs"].append(record)
            (dest / "perf.json").write_text(json.dumps(report, indent=2) + "\n")
            print(tag, record["mode"], record["layout"],
                  [(r["op"], round(r["ir_per_op"])) for r in record["intervals"][1:]], flush=True)
    return report


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary")
    parser.add_argument("tag")
    parser.add_argument("--gnu", default=shutil.which("emacs"))
    parser.add_argument("--n", type=int, default=1000)
    parser.add_argument("--layout", type=int, action="append")
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()
    measure(args.binary, args.tag, args.gnu, args.n, args.layout or LAYOUTS, args.jobs)
