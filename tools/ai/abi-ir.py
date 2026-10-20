#!/usr/bin/env python3
"""Doc 215 total-Ir comparisons; retain profiles and count actual GC sweeps.

ir/files CAND BASE [--output DIRECTORY] writes directly to a new evidence
directory when requested. Existing evidence directories are never overwritten.
"""
import bisect
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import subprocess
import sys
import tempfile
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
LAYOUTS = (0, 700, 1400, 2100)
IR_MODES = ("boot", "regex", "regex-gc")
GC_CYCLE_FUNCTION = "nl_gc_sweep_merge_chunks"
PROFILE_WORKERS = 8
# Operational limits only: workloads, Ir caps and GC floors stay fixed.
PROFILE_TIMEOUTS = {"boot": 580, "files": 580, "regex": 1780, "regex-gc": 3600}


def fingerprint(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def identities(cand, base):
    paths = [Path(__file__).resolve(), ROOT / "tools/ai/abi-merge-gate.sh"]
    for binary in (cand, base):
        paths.extend(Path(str(Path(binary).resolve()) + suffix)
                     for suffix in ("", ".cold", ".native-startup.el"))
    result = {str(p): fingerprint(p) for p in paths}
    result["candidate-path"] = str(Path(cand).resolve())
    result["baseline-path"] = str(Path(base).resolve())
    for key in ("ABI_DUMP", "ABI_CENSUS"):
        if os.environ.get(key):
            path = Path(os.environ[key]).resolve()
            result[key + "-path"] = str(path)
            result[str(path)] = fingerprint(path)
    # Only the runtime environment matters; do not persist environment values.
    runtime_env = {k: v for k, v in os.environ.items()
                   if k.startswith(("NELISP_", "M5_", "LD_", "LC_")) or k == "LANG"}
    result["runtime-environment"] = hashlib.sha256(
        json.dumps(runtime_env, sort_keys=True).encode()).hexdigest()
    return result


def ir_artifacts():
    return {f"{m}-{who}-{layout}.{suffix}" for m in IR_MODES
            for who in ("base", "cand") for layout in LAYOUTS
            for suffix in ("cg", "out", "err")} | {"regex.el", "regex-gc.el"}


def completed_jobs():
    return {f"{m}-{who}-{layout}": 0 for m in IR_MODES
            for who in ("base", "cand") for layout in LAYOUTS}


def verify_workloads(directory, before):
    after = {name: fingerprint(directory / name) for name in ("regex.el", "regex-gc.el")}
    if before != after:
        raise ValueError("Ir workloads changed during profiling; no record written")


def check_marker(stem, mode):
    marker = {"regex": "ABI-REGEX 6000", "regex-gc": "ABI-REGEX-GC 24000",
              "files": "ABI-FILES (1000 527500"}.get(mode)
    if marker and marker not in stem.with_suffix(".out").read_text():
        raise ValueError(f"missing completion marker: {stem}.out")


def validate_record(directory, current):
    """Reject stale/incomplete inputs before recomputing the raw-profile verdict."""
    record = json.loads((directory / "measurement.json").read_text())
    if (record.get("schema") != 1 or record.get("modes") != list(IR_MODES)
            or record.get("layouts") != list(LAYOUTS)
            or record.get("completed_jobs") != completed_jobs()
            or record.get("identities") != current
            or set(record.get("artifacts", {})) != ir_artifacts()):
        raise ValueError("incomplete or stale Ir measurement provenance")
    for name, digest in record["artifacts"].items():
        if fingerprint(directory / name) != digest:
            raise ValueError(f"changed Ir measurement artifact: {name}")
    for stem in record["completed_jobs"]:
        check_marker(directory / stem, stem.rsplit("-", 2)[0])


def replay_ir(directory, cand, base):
    validate_record(directory, identities(cand, base))
    ok = True
    for mode in IR_MODES:
        samples = [(layout, *(profile_counts(directory / f"{mode}-{who}-{layout}.cg", binary)
                    + ((directory / f"{mode}-{who}-{layout}.out").read_bytes(),)
                    for who, binary in (("base", base), ("cand", cand))))
                   for layout in LAYOUTS]
        ok &= summarize(mode, samples)
    return 0 if ok else 1


def profile_counts(profile, binary):
    symbols = []
    for line in subprocess.check_output(["nm", "-n", str(binary)], text=True).splitlines():
        p = line.split()
        if len(p) == 3 and p[1] in ("t", "T"):
            symbols.append((int(p[0], 16), p[2]))
    symbols.sort()
    addresses = [a for a, _ in symbols]
    names = {}
    target = None
    caller = None
    cycles = 0
    total = None
    for line in profile.read_text().splitlines():
        if line.startswith("summary:"):
            total = int(line.split()[1])
        elif line.startswith(("fn=", "cfn=")):
            tok = line.split("=", 1)[1].strip()
            m = re.fullmatch(r"\((\d+)\)(?:\s+(.*))?", tok)
            name = m[2] if m else tok
            if name and name.startswith("0x"):
                pos = bisect.bisect_right(addresses, int(name.split("'")[0], 16)) - 1
                name = symbols[pos][1] if pos >= 0 else name
            if m:
                if name:
                    names[m[1]] = name
                name = names.get(m[1])
            if name:
                name = name.split("'", 1)[0]
            if line.startswith("cfn="):
                target = name
            else:
                caller = name
                target = None
        elif (line.startswith("calls=") and target == GC_CYCLE_FUNCTION
              and caller != GC_CYCLE_FUNCTION):
            cycles += int(line[6:].split()[0])
    if total is None or total <= 0:
        raise ValueError(f"missing positive total Ir: {profile}")
    return total, cycles


def run_process(args, *, timeout, cancel, **kwargs):
    """Reap the whole isolated profile process group on failure or cancellation."""
    process = subprocess.Popen(args, start_new_session=True, **kwargs)
    deadline = time.monotonic() + timeout
    try:
        while True:
            if cancel.is_set():
                raise concurrent.futures.CancelledError()
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise subprocess.TimeoutExpired(args, timeout)
            try:
                return process.wait(timeout=min(remaining, 0.1))
            except subprocess.TimeoutExpired:
                pass
    finally:
        try:
            os.killpg(process.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        process.wait()


def run_one(tmp, mode, who, binary, layout, cancel):
    if cancel.is_set():
        raise concurrent.futures.CancelledError()
    stem = tmp / f"{mode}-{who}-{layout}"
    cache = tmp / f"{mode}-{who}-{layout}-cache"
    cache.mkdir()  # must be empty for every process, including boot/regex
    env = dict(os.environ, XDG_CACHE_HOME=str(cache), TMPDIR=str(tmp),
               ABI_SOURCE_ROOT=str(ROOT), ABI_LAYOUT_PAD="x" * layout)
    args = ["--eval", "nil"] if mode == "boot" else ["--load", str(
        ROOT / "tools/ai/abi-file-exec.el" if mode == "files" else tmp / f"{mode}.el")]
    with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
        result = run_process(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                                 f"--callgrind-out-file={stem}.cg", str(binary), *args],
                                cwd=ROOT, env=env, stdout=out, stderr=err,
                                timeout=PROFILE_TIMEOUTS[mode], cancel=cancel)
    if result:
        raise RuntimeError(f"{mode} {who} exit {result}: {stem}.err")
    check_marker(stem, mode)
    return profile_counts(stem.with_suffix(".cg"), binary)


def summarize(mode, samples):
    """Judge all four layouts, including their complete outputs and GC work."""
    outputs = set()
    enough = True
    for layout, (bb, bg, bo), (cc, cg, co) in samples:
        outputs.update((bo, co))
        enough &= mode != "regex-gc" or min(bg, cg) >= 10
        print(f"Ir {mode} layout {layout} base {bb} cand {cc}; "
              f"GC base {bg} cand {cg}; output {'identical' if bo == co else 'DIFF'}; "
              f"delta {(cc / bb - 1) * 100:+.3f}%", flush=True)
    same = len(outputs) == 1
    baselines = [b[0] for _, b, _ in samples]
    candidates = [c[0] for _, _, c in samples]
    bb, cc = sum(baselines), sum(candidates)
    passed = ([layout for layout, _, _ in samples] == list(LAYOUTS)
              and cc <= bb and same and enough)
    print(f"Ir {mode} mean base {bb / len(samples):.2f} cand {cc / len(samples):.2f}; "
          f"delta {(cc / bb - 1) * 100:+.3f}%; "
          f"spread base {min(baselines)}..{max(baselines)} "
          f"cand {min(candidates)}..{max(candidates)}; "
          f"output {'identical' if same else 'DIFF'}; {'PASS' if passed else 'FAIL'}", flush=True)
    if not enough:
        print("FAIL: regex-gc requires at least 10 GC cycles per binary in every layout", flush=True)
    return passed


def main():
    started = time.monotonic()
    if len(sys.argv) == 5 and sys.argv[1] == "verify-ir":
        return replay_ir(Path(sys.argv[4]).resolve(), Path(sys.argv[2]).resolve(),
                         Path(sys.argv[3]).resolve())
    if len(sys.argv) not in (4, 6) or (len(sys.argv) == 6 and sys.argv[4] != "--output"):
        raise ValueError("expected ir/files CAND BASE [--output DIRECTORY]")
    mode, cand, base = sys.argv[1:4]
    if mode not in ("ir", "files"):
        raise ValueError("expected ir, files, or verify-ir CAND BASE DIRECTORY")
    before = identities(cand, base) if mode == "ir" else None
    if len(sys.argv) == 6:
        tmp = Path(sys.argv[5]).resolve()
        tmp.mkdir(parents=True)  # Refuse to overwrite any prior evidence.
    else:
        tmp_root = ROOT / "target/abi-ir"
        tmp_root.mkdir(parents=True, exist_ok=True)
        tmp = Path(tempfile.mkdtemp(prefix=f"{mode}-", dir=tmp_root))
    print(f"profiles: {tmp}", flush=True)
    # Same fixed work for both binaries. No GC instructions are subtracted.
    (tmp / "regex.el").write_text(r'''
(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i 20)
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))))
(princ "ABI-REGEX 6000\n")
''')
    # Cold images leave regex6000 with only four sweeps. Keep its independent
    # Ir cap and repeat the original work to measure >=10 automatic sweeps.
    # Counts-only calibration: 60 iterations gave baseline9/candidate12;
    # 80 gave baseline11/candidate17 in every layout. Freeze80 before Ir.
    # No explicit GC or synthetic allocation; retain all instructions.
    (tmp / "regex-gc.el").write_text(r'''
(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i 80)
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))))
(princ "ABI-REGEX-GC 24000\n")
''')
    workloads_before = {name: fingerprint(tmp / name) for name in ("regex.el", "regex-gc.el")}
    modes = IR_MODES if mode == "ir" else ["files"]
    ok = True
    cancel = threading.Event()
    pool = concurrent.futures.ThreadPoolExecutor(max_workers=PROFILE_WORKERS)
    def terminate(signum, _frame):
        # Repeated termination requests must not interrupt process reaping.
        if not cancel.is_set():
            cancel.set()
            raise SystemExit(128 + signum)
    previous_sigterm = signal.signal(signal.SIGTERM, terminate)
    try:
        jobs = {pool.submit(run_one, tmp, m, who, Path(b).resolve(), layout, cancel): (m, who, layout)
                for m in modes for who, b in (("base", base), ("cand", cand)) for layout in LAYOUTS}
        results = {}
        for job in concurrent.futures.as_completed(jobs):
            results[jobs[job]] = job.result()
        for m in modes:
            samples = [(layout, *(results[m, who, layout] +
                        ((tmp / f"{m}-{who}-{layout}.out").read_bytes(),)
                        for who in ("base", "cand"))) for layout in LAYOUTS]
            ok &= summarize(m, samples)
    finally:
        cancel.set()
        try:
            pool.shutdown(wait=True, cancel_futures=True)
        finally:
            signal.signal(signal.SIGTERM, previous_sigterm)
    if mode == "ir":
        verify_workloads(tmp, workloads_before)
        if before != identities(cand, base):
            raise ValueError("Ir inputs changed during profiling; no record written")
        # Written only after every process exits0 with its completion marker.
        # Store raw evidence and identities, never a cached PASS decision.
        (tmp / "measurement.json").write_text(json.dumps({
            "schema": 1, "modes": list(IR_MODES), "layouts": list(LAYOUTS),
            "completed_jobs": completed_jobs(),
            "identities": before,
            "artifacts": {name: fingerprint(tmp / name) for name in sorted(ir_artifacts())},
        }, indent=2) + "\n")
    print(f"elapsed {time.monotonic() - started:.3f}s", flush=True)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
