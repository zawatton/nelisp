#!/usr/bin/env python3
"""CPU-14 wall-clock comparisons; use paired ratios, never ratios of medians."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import signal
import sys
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/strings/timing"
FREQUENCY = Path("/sys/devices/system/cpu/cpu14/cpufreq/scaling_cur_freq")


def run(role, command, kind, round_number, pressure=False, in_slot=False):
    env = os.environ.copy()
    env.update(STRINGS_KIND=kind, STRINGS_N=os.environ.get("STRINGS_N", "100000"))
    fixture = "test/unibyte-alloc-pressure80.el" if pressure else "tools/ai/strings-timing.el"
    stem = OUT / f"{kind}-{round_number}-{role}"
    frequency = FREQUENCY.read_text().strip() if FREQUENCY.exists() else None
    invocation = ["taskset", "-c", "14", *command, "--load", fixture]
    # Each paired round owns one heavy slot. Pressure's process timer
    # starts inside that granted slot, so queue wait is never measured.
    if pressure:
        heavy = Path(os.environ.get("NOTES_ROOT", str(Path.home() / "Cowork/Notes"))) / "bin/heavy-run.sh"
        worker = (
            "import subprocess,sys,time,json; from pathlib import Path; "
            "freq=Path('/sys/devices/system/cpu/cpu14/cpufreq/scaling_cur_freq'); "
            "f=freq.read_text().strip() if freq.exists() else None; "
            "t=time.monotonic(); p=subprocess.run(sys.argv[1:],capture_output=True); "
            "elapsed=time.monotonic()-t; sys.stdout.buffer.write(p.stdout); "
            "sys.stdout.flush(); sys.stderr.buffer.write(p.stderr); sys.stderr.flush(); "
            "print(); print('STRINGS-PROCESS '+json.dumps(dict(seconds=elapsed,frequency_khz=f)),flush=True); "
            "sys.exit(p.returncode)"
        )
        invocation = ([sys.executable, "-c", worker, *invocation] if in_slot else
                      [str(heavy), sys.executable, "-c", worker, *invocation])
    started = time.monotonic()
    process = subprocess.Popen(invocation, cwd=ROOT, env=env, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    print(f"start {kind} round={round_number} role={role}", flush=True)
    try:
        stdout, stderr = process.communicate(timeout=840)
    except BaseException:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()
        raise
    result = subprocess.CompletedProcess(invocation, process.returncode, stdout, stderr)
    elapsed = time.monotonic() - started
    if pressure:
        meta = re.findall(rb"^STRINGS-PROCESS (.+)$", stdout, re.M)
        assert len(meta) == 1, f"{role} missing in-slot wall-clock measurement"
        measured = json.loads(meta[0])
        elapsed, frequency = measured["seconds"], measured["frequency_khz"]
    stem.with_suffix(".out").write_bytes(result.stdout)
    stem.with_suffix(".err").write_bytes(result.stderr)
    assert result.returncode == 0, f"{kind}/{role} exit {result.returncode}; see {stem.name}"
    # GNU 31.1 warns about this existing dynamic-binding corpus's missing
    # file cookie. Retain the exact stderr; accept only that four-line warning.
    warning = result.stderr.decode("utf-8", errors="replace").splitlines()
    expected_warning = (role == "gnu" and pressure and len(warning) == 4
                        and warning[0].startswith("Warning (files): Missing ‘lexical-binding’ cookie in ")
                        and warning[0].endswith('/test/unibyte-alloc-pressure80.el".')
                        and warning[1] == "You can add one with ‘M-x elisp-enable-lexical-binding RET’."
                        and warning[2] == "See ‘(elisp)Selecting Lisp Dialect’ and ‘(elisp)Converting to Lexical Binding’"
                        and warning[3] == "for more information.")
    assert not result.stderr or expected_warning, f"{kind}/{role} unexpected stderr"
    if pressure:
        matches = re.findall(rb"^ABI-PRESSURE ([0-9]+ [0-9]+)$", result.stdout, re.M)
        assert len(matches) == 1, f"{role} incomplete pressure result"
        seconds, output = elapsed, matches[0].decode()
    else:
        matches = re.findall(rb"^STRINGS-TIME ([0-9.]+)$", result.stdout, re.M)
        assert len(matches) == 1, f"{role} incomplete timing result"
        seconds, output = float(matches[0]), None
    row = dict(role=role, round=round_number, kind=kind, seconds=seconds,
               process_seconds=elapsed, cpu=14, frequency_khz=frequency, output=output,
               expected_gnu_cookie_warning=expected_warning)
    print(f"{kind} round={round_number} role={role} wall={seconds:.6f}s", flush=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("workload", choices=["producers", "pressure"])
    parser.add_argument("--round", type=int, help=argparse.SUPPRESS)
    parser.add_argument("--kind", choices=["concat", "substring", "format", "pressure"], help=argparse.SUPPRESS)
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    candidate = ROOT / "target/strings/dynamic/nelisp"
    base = ROOT / "target/strings/base/nelisp"
    gnu = shutil.which(os.environ.get("GNU_EMACS", "emacs"))
    assert candidate.exists() and base.exists() and gnu, "missing timing binaries"
    identities = {name: hashlib.sha256(path.read_bytes()).hexdigest()
                  for name, path in [("candidate", candidate), ("base", base), ("gnu", Path(gnu))]}
    assert identities["candidate"] != identities["base"], "candidate and base are the same binary"
    commands = {"candidate": [str(candidate)], "gnu": [gnu, "-Q", "--batch"], "base": [str(base)]}
    if args.round is not None:
        assert args.kind and (args.workload == "pressure") == (args.kind == "pressure")
        rows = []
        for role in ["candidate", "gnu", "base"]:
            rows.append(run(role, commands[role], args.kind, args.round,
                            pressure=args.kind == "pressure", in_slot=True))
            if args.kind != "pressure":
                rows.append(run(role, commands[role], args.kind + "-control", args.round))
        (OUT / f"{args.kind}-round-{args.round}.json").write_text(json.dumps(rows))
        return
    samples, verdicts = [], {}
    for kind in (["pressure"] if args.workload == "pressure" else ["concat", "substring", "format"]):
        pressure = kind == "pressure"
        def paired_round(number):
            print(f"start paired round {kind}/{number}", flush=True)
            subprocess.run([str(Path.home() / "Cowork/Notes/bin/heavy-run.sh"),
                            sys.executable, str(Path(__file__).resolve()), args.workload,
                            "--round", str(number), "--kind", kind], cwd=ROOT, check=True)
            return json.loads((OUT / f"{kind}-round-{number}.json").read_text())
        if pressure:
            paired_round(-1)
        # Primitive fixtures warm inside each process before the timer.
        ratios, base_ratios, own = [], [], []
        for round_number in range(7):
            times = {}
            # Release between rounds; keep candidate/GNU/base close together.
            rows = paired_round(round_number)
            samples.extend(rows)
            if pressure:
                times = {row["role"]: row["seconds"] for row in rows}
            else:
                for role in ["candidate", "gnu", "base"]:
                    call, control = [r["seconds"] for r in rows if r["role"] == role]
                    times[role] = call - control
                    assert times[role] > 0, f"{role}/{kind} own cost has insufficient signal"
            ratios.append(times["candidate"] / times["gnu"])
            base_ratios.append(times["candidate"] / times["base"])
            own.append(times)
        if pressure:
            assert len({s["output"] for s in samples}) == 1, "pressure outputs differ"
        ratio, regression = statistics.median(ratios), statistics.median(base_ratios)
        verdicts[kind] = dict(gnu_ratio=ratio, base_ratio=regression,
                              paired_gnu_ratios=ratios, paired_base_ratios=base_ratios,
                              own_seconds=own, passed=(regression < 1 if pressure else ratio <= 1))
    report = dict(cpu=14, rounds=7, metric="paired wall-clock own cost" if args.workload == "producers"
                  else "paired process wall-clock", identities=identities, samples=samples, verdicts=verdicts,
                  iterations=int(os.environ.get("STRINGS_N", "100000")))
    (OUT / f"{args.workload}.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(verdicts), flush=True)
    raise SystemExit(0 if all(v["passed"] for v in verdicts.values()) else 1)


if __name__ == "__main__":
    main()
