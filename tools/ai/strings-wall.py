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


def run(role, command, kind, round_number, pressure=False):
    env = os.environ.copy()
    env.update(STRINGS_KIND=kind, STRINGS_N=os.environ.get("STRINGS_N", "10000"))
    fixture = "test/unibyte-alloc-pressure80.el" if pressure else "tools/ai/strings-timing.el"
    stem = OUT / f"{kind}-{round_number}-{role}"
    frequency = FREQUENCY.read_text().strip() if FREQUENCY.exists() else None
    invocation = ["taskset", "-c", "14", *command, "--load", fixture]
    # One timed process is one heavy unit. Queue wait is outside the Lisp
    # timer for primitives; pressure's timer starts inside the granted slot.
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
        invocation = [str(heavy), sys.executable, "-c", worker, *invocation]
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
    assert not result.stderr, f"{kind}/{role} unexpected stderr"
    if pressure:
        matches = re.findall(rb"^ABI-PRESSURE ([0-9]+ [0-9]+)$", result.stdout, re.M)
        assert len(matches) == 1, f"{role} incomplete pressure result"
        seconds, output = elapsed, matches[0].decode()
    else:
        matches = re.findall(rb"^STRINGS-TIME ([0-9.]+)$", result.stdout, re.M)
        assert len(matches) == 1, f"{role} incomplete timing result"
        seconds, output = float(matches[0]), None
    row = dict(role=role, round=round_number, kind=kind, seconds=seconds,
               process_seconds=elapsed, cpu=14, frequency_khz=frequency, output=output)
    print(f"{kind} round={round_number} role={role} wall={seconds:.6f}s", flush=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("workload", choices=["producers", "pressure"])
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
    samples, verdicts = [], {}
    for kind in (["pressure"] if args.workload == "pressure" else ["concat", "substring", "format"]):
        pressure = kind == "pressure"
        for role in commands:
            run(role, commands[role], kind, -1, pressure)
        ratios, base_ratios, own = [], [], []
        for round_number in range(7):
            times = {}
            for role in ["candidate", "gnu", "base"]:
                row = run(role, commands[role], kind, round_number, pressure)
                samples.append(row)
                if pressure:
                    times[role] = row["seconds"]
                else:
                    control = run(role, commands[role], kind + "-control", round_number)
                    samples.append(control)
                    times[role] = row["seconds"] - control["seconds"]
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
                  else "paired process wall-clock", identities=identities, samples=samples, verdicts=verdicts)
    (OUT / f"{args.workload}.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(verdicts), flush=True)
    raise SystemExit(0 if all(v["passed"] for v in verdicts.values()) else 1)


if __name__ == "__main__":
    main()
