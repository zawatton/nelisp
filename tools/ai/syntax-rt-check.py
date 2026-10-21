#!/usr/bin/env python3
"""Checks for tools/ai/syntax-rt-progress.org (Doc 225).

Usage:
  syntax-rt-check.py plan
  syntax-rt-check.py semantics
  syntax-rt-check.py speed char-syntax|string-match
  syntax-rt-check.py final

Binaries: SYNTAX_RT_DYN / SYNTAX_RT_STATIC (default target/syntax-rt/bin/
nelisp-dyn and nelisp-static), SYNTAX_RT_BASE (default the integration-D
dynamic base).  GNU: /usr/local/bin/emacs (31.1).
"""

import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "docs/design/225-syntax-and-match-entry-gnu-parity.org"
GNU_SRC = Path("/home/madblack-21/Downloads/emacs-31.1/src")
GNU_BIN = "/usr/local/bin/emacs"
EVIDENCE = ROOT / "target/syntax-rt"
BASE_BIN = Path(os.environ.get(
    "SYNTAX_RT_BASE",
    ROOT.parent / "integ-20261021d/target/integ/nelisp-dyn"))
DYN_BIN = Path(os.environ.get("SYNTAX_RT_DYN", EVIDENCE / "bin/nelisp-dyn"))
STATIC_BIN = Path(os.environ.get("SYNTAX_RT_STATIC", EVIDENCE / "bin/nelisp-static"))
CPUS = os.environ.get("SYNTAX_RT_CPU", "10")
MEMORY_LIMIT_KB = 8388608


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def fail(message):
    print(f"SYNTAX-RT FAIL: {message}")
    return 1


# ---------------------------------------------------------------- plan

SECTIONS = ("char-syntax ?a", "char-syntax ?(", 'string-match "\\\\sw+"',
            'string-match "alpha"')


def doc_tables(text):
    """Return {section-heading: [row-cells...]} for the §1 owner tables."""
    tables, current = {}, None
    for line in text.splitlines():
        if line.startswith("** "):
            current = line[3:].strip()
            continue
        if current and line.startswith("|") and not line.startswith("|-"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if cells and cells[0] != "Owner":
                tables.setdefault(current, []).append(cells)
    return tables


def gnu_reference_ok(ref, token, shift=0):
    match = re.fullmatch(r"([\w.-]+\.[ch]):(\d+)", ref)
    if not match:
        return False
    source = GNU_SRC / match.group(1)
    if not source.is_file():
        return False
    lines = source.read_text(errors="replace").split("\n")
    line = int(match.group(2)) + shift
    window = lines[max(0, line - 4):line + 3]
    return any(token in candidate for candidate in window)


def check_plan():
    if not DOC.is_file():
        return fail(f"missing {DOC.relative_to(ROOT)}")
    text = DOC.read_text()
    tables = doc_tables(text)
    problems, refs = [], 0
    for wanted in SECTIONS:
        heading = next((h for h in tables if h.startswith(wanted)), None)
        if heading is None:
            problems.append(f"no owner table for {wanted}")
            continue
        total = 0
        for cells in tables[heading]:
            if len(cells) != 7:
                problems.append(f"{wanted}: row has {len(cells)} cells")
                continue
            owner, micros, share, mechanism, ref, token, port = cells
            if not re.fullmatch(r"[\d,]+", micros):
                problems.append(f"{wanted}: bad us/call {micros!r}")
            if not re.fullmatch(r"\d+%", share):
                problems.append(f"{wanted}: bad share {share!r}")
            else:
                total += int(share[:-1])
            if not gnu_reference_ok(ref, token):
                problems.append(f"{wanted}: {ref} does not show {token!r}")
            refs += 1
            if not re.match(r"(ported|adapted|kept|not ported)", port):
                problems.append(f"{wanted}: port decision {port!r}")
        if not 95 <= total <= 105:
            problems.append(f"{wanted}: shares sum to {total}%")
    # Negative control: the same references, shifted 40 lines, must fail.
    shifted_ok = sum(gnu_reference_ok(cells[4], cells[5], 40)
                     for rows in tables.values() for cells in rows
                     if len(cells) == 7)
    if shifted_ok:
        problems.append(f"negative control: {shifted_ok} shifted references still match")
    # The measured runs the shares come from.
    for name in ("split-base-1.txt", "split-base-2.txt"):
        run = EVIDENCE / "s0" / name
        if not run.is_file() or "(char-syntax 97) total" not in run.read_text():
            problems.append(f"missing measurement {run}")
    if BASE_BIN.is_file() and sha256(BASE_BIN) not in text:
        problems.append("base binary sha256 not cited")
    for word in ("§2", "§3", "§4", "codegen", "callpath"):
        if word not in text:
            problems.append(f"doc lacks {word}")
    if problems:
        for problem in problems:
            print(f"  {problem}")
        return fail(f"plan: {len(problems)} problem(s)")
    print(f"SYNTAX-RT plan PASS: {len(SECTIONS)} owner tables, {refs} GNU references, "
          f"negative control rejected all shifted references")
    return 0


# ---------------------------------------------------------------- speed

SPEED_PROBE = r""";; -*- lexical-binding: t; -*-
;; Own cost: the call minus a same-shape trivial subr call, both inline in a
;; dotimes loop (ten copies per iteration).  Same file for GNU and NeLisp.
(defmacro rt-loop (n form)
  `(let ((t0 (float-time)))
     (dotimes (_ ,n) ,form ,form ,form ,form ,form ,form ,form ,form ,form ,form)
     (- (float-time) t0)))
(defvar rt-n (string-to-number (or (getenv "RT_N") "1000")))
(defmacro rt-own (label scale op base)
  `(let ((n (max 1 (/ (* rt-n ,scale) 100))) xs)
     (rt-loop 20 ,op) (rt-loop 20 ,base)
     (dotimes (_ 7)
       (let* ((a (rt-loop n ,op)) (b (rt-loop n ,base)))
         (push (/ (* 1e6 (- a b)) (* 10 n)) xs)
         (push (/ (* 1e6 a) (* 10 n)) xs)
         (push (/ (* 1e6 b) (* 10 n)) xs)))
     (setq xs (nreverse xs))
     (let (own call base)
       (while xs
         (push (pop xs) own) (push (pop xs) call) (push (pop xs) base))
       (setq own (sort own #'<) call (sort call #'<) base (sort base #'<))
       (princ (format "RT %s own=%.4f call=%.4f base=%.4f\n" ,label
                      (nth 3 own) (nth 3 call) (nth 3 base))))))
"""

SPEED_CASES = {
    "char-syntax": [
        ("char-syntax-a", 100, "(char-syntax 97)", "(integerp 97)"),
        ("char-syntax-paren", 100, "(char-syntax 40)", "(integerp 40)"),
    ],
    "string-match": [
        ("string-match-plain", 20, '(string-match "alpha" "alpha")', '(eq "alpha" "alpha")'),
        ("string-match-syntax", 20, '(string-match "\\\\sw+" "alpha")', '(eq "\\\\sw+" "alpha")'),
    ],
}
# Calls per sample: GNU is fast, so it gets more iterations for resolution.
SPEED_N = {"gnu": 20000, "nelisp": 200}


def write_speed_probe(kind):
    directory = EVIDENCE / "speed"
    directory.mkdir(parents=True, exist_ok=True)
    probe = directory / f"{kind}.el"
    body = SPEED_PROBE + "".join(
        f'(rt-own "{label}" {scale} {op} {base})\n'
        for label, scale, op, base in SPEED_CASES[kind])
    probe.write_text(body)
    return probe


def run_limited(command, env=None, timeout=900):
    shell = "ulimit -v %d; exec \"$@\"" % MEMORY_LIMIT_KB
    return subprocess.run(["bash", "-c", shell, "run", *command], capture_output=True,
                          text=True, env=env, timeout=timeout, cwd=ROOT)


def speed_run(runtime, binary, probe):
    env = dict(os.environ, RT_N=str(SPEED_N[runtime]))
    command = (["taskset", "-c", CPUS, str(binary), "--load", str(probe)]
               if runtime == "nelisp"
               else ["taskset", "-c", CPUS, GNU_BIN, "-Q", "--batch", "-l", str(probe)])
    result = run_limited(command, env)
    rows = {}
    for line in result.stdout.splitlines() + result.stderr.splitlines():
        match = re.match(r"RT (\S+) own=(\S+) call=(\S+) base=(\S+)", line)
        if match:
            rows[match.group(1)] = {k: float(v) for k, v in
                                    zip(("own", "call", "base"), match.groups()[1:])}
    return result.returncode, rows


def cpu_freq():
    """The timing CPU's current clock in kHz (COMMON.md: record with every run)."""
    path = Path(f"/sys/devices/system/cpu/cpu{CPUS}/cpufreq/scaling_cur_freq")
    try:
        return int(path.read_text().strip())
    except (OSError, ValueError):
        return None


def paired_ratio(numerator, denominator):
    """NUMERATOR/DENOMINATOR for one round; a non-positive denominator (an
    own cost below the clock's resolution) makes any positive numerator
    infinitely slower and a non-positive one equal."""
    if denominator > 0:
        return numerator / denominator
    return float("inf") if numerator > 0 else 1.0


def check_speed(kind, binary=None):
    if kind not in SPEED_CASES:
        return fail(f"unknown speed case {kind}")
    binary = Path(binary or DYN_BIN)
    for required in (binary, BASE_BIN):
        if not required.is_file():
            return fail(f"missing binary {required}")
    probe = write_speed_probe(kind)
    labels = [case[0] for case in SPEED_CASES[kind]]
    rounds = []
    # Paired ratios (COMMON.md): candidate, GNU and base close together in
    # each round; the decision is the median of the per-round ratios.
    for _ in range(7):
        entry = {}
        for name, runtime, exe in (("candidate", "nelisp", binary), ("gnu", "gnu", None),
                                   ("base", "nelisp", BASE_BIN)):
            freq = cpu_freq()
            code, rows = speed_run(runtime, exe, probe)
            if code != 0 or set(rows) != set(labels):
                return fail(f"speed {kind}: {name} run failed (exit {code}, rows {sorted(rows)})")
            entry[name] = {"freq_khz": freq, "rows": rows}
        rounds.append(entry)
    record = {"kind": kind, "binary": str(binary), "binary_sha256": sha256(binary),
              "base": str(BASE_BIN), "base_sha256": sha256(BASE_BIN), "gnu": GNU_BIN,
              "cpu": CPUS, "rounds": rounds, "cases": {}}
    passed = True
    for label in labels:
        to_gnu = [paired_ratio(r["candidate"]["rows"][label]["own"], r["gnu"]["rows"][label]["own"])
                  for r in rounds]
        to_base = [paired_ratio(r["candidate"]["rows"][label]["own"], r["base"]["rows"][label]["own"])
                   for r in rounds]
        summary = {"ratio_to_gnu": statistics.median(to_gnu),
                   "ratio_to_base": statistics.median(to_base)}
        for name in ("candidate", "gnu", "base"):
            for field in ("own", "call", "base"):
                summary[f"{name}_{field}"] = statistics.median(
                    r[name]["rows"][label][field] for r in rounds)
        summary["ok"] = summary["ratio_to_gnu"] <= 1.0
        passed &= summary["ok"]
        record["cases"][label] = summary
        print(f"  {label}: own {summary['candidate_own']:.4f} us, GNU {summary['gnu_own']:.4f} us, "
              f"base {summary['base_own']:.4f} us; median paired ratio to GNU "
              f"{summary['ratio_to_gnu']:.4g}, to base {summary['ratio_to_base']:.4f} "
              f"{'PASS' if summary['ok'] else 'FAIL'}")
    freqs = [r[n]["freq_khz"] for r in rounds for n in ("candidate", "gnu", "base")
             if r[n]["freq_khz"]]
    if freqs:
        print(f"  CPU {CPUS} scaling_cur_freq {min(freqs)}-{max(freqs)} kHz over {len(freqs)} runs")
    record["passed"] = passed
    (EVIDENCE / "speed" / f"{kind}.json").write_text(json.dumps(record, indent=1) + "\n")
    if passed:
        print(f"SYNTAX-RT speed {kind} PASS")
        return 0
    return fail(f"speed {kind}: NeLisp own cost above GNU's (median paired ratio of 7 rounds)")


def main(argv):
    if argv[1:2] == ["plan"]:
        return check_plan()
    if argv[1:2] == ["speed"] and len(argv) in (3, 4):
        return check_speed(argv[2], argv[3] if len(argv) == 4 else None)
    print(__doc__.strip())
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
