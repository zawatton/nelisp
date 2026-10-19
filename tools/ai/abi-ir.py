#!/usr/bin/env python3
"""Total-Ir comparisons including the 6000-call ABI regex workload.

Use: abi-ir.py regex CANDIDATE BASELINE. Retain all four required layouts,
whole-process instruction counts and ten or more actual GC sweeps.
"""
import bisect
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
LAYOUTS = {"boot": (0, 700, 1400, 2100), "regex": (0, 700, 1400, 2100),
           "files": (0, 700, 1400, 2100)}
REGEX_ITERATIONS = 20
REGEX_GC_INTERVAL = 2
GC_CYCLE_FUNCTION = "nl_gc_sweep_merge_chunks"


def profile_directory(mode):
    """Honor a durable evidence destination; legacy callers get a temp path."""
    if os.environ.get("ABI_IR_OUTPUT"):
        output = Path(os.environ["ABI_IR_OUTPUT"]).resolve()
        output.mkdir(parents=True, exist_ok=True)
        if any(output.iterdir()):
            raise ValueError(f"profile output must be empty: {output}")
        return output
    parent = ROOT / "target/abi-ir"
    parent.mkdir(parents=True, exist_ok=True)
    return Path(tempfile.mkdtemp(prefix=f"{mode}-", dir=parent))


def callgraph_options(binary):
    """Omit private call edges, retaining every instruction and GC entry."""
    symbols = [line.split() for line in subprocess.check_output(
        ["nm", "-n", str(binary)], text=True).splitlines()]
    functions = [(int(p[0], 16), p[2]) for p in symbols
                 if len(p) == 3 and p[1] in ("t", "T")]
    cycles = {address for address, name in functions if name == GC_CYCLE_FUNCTION}
    if len(cycles) != 1:
        raise ValueError(f"expected one {GC_CYCLE_FUNCTION} symbol: {binary}")
    visible = cycles | {address for address, name in functions if name == "nl_gc_sweep"}
    addresses = {address for address, name in functions
                 if address not in visible
                 and name.startswith(("nl_", "nelisp_", "bf_", "wf_", "m5_"))}
    return [f"--fn-skip=0x{address:016x}" for address in sorted(addresses)]


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


def run_one(tmp, mode, who, binary, layout):
    stem = tmp / f"{mode}-{who}-{layout}"
    cache = tmp / f"{mode}-{who}-{layout}-cache"
    cache.mkdir()  # must be empty for every process, including boot/regex
    env = dict(os.environ, XDG_CACHE_HOME=str(cache), TMPDIR=str(tmp),
               ABI_SOURCE_ROOT=str(ROOT), ABI_LAYOUT_PAD="x" * layout)
    args = ["--eval", "nil"] if mode == "boot" else ["--load", str(
        tmp / "regex.el" if mode == "regex" else ROOT / "tools/ai/abi-file-exec.el")]
    with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
        result = subprocess.run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                                 f"--callgrind-out-file={stem}.cg",
                                 *(callgraph_options(binary) if mode == "regex" else []),
                                 str(binary), *args],
                                cwd=ROOT, env=env, stdout=out, stderr=err, timeout=580)
    if result.returncode:
        raise RuntimeError(f"{mode} {who} exit {result.returncode}: {stem}.err")
    marker = {"regex": f"ABI-REGEX {REGEX_ITERATIONS * 300}",
              "files": "ABI-FILES (1000 527500"}.get(mode)
    if marker and marker not in stem.with_suffix(".out").read_text():
        raise RuntimeError(f"{mode} {who} missing completion marker: {stem}.out")
    return profile_counts(stem.with_suffix(".cg"), binary)


def summarize(mode, samples):
    """Judge every required layout, including complete outputs and GC work."""
    outputs = set()
    enough = True
    for layout, (bb, bg, bo), (cc, cg, co) in samples:
        outputs.update((bo, co))
        enough &= mode != "regex" or min(bg, cg) >= 10
        print(f"Ir {mode} layout {layout} base {bb} cand {cc}; "
              f"GC base {bg} cand {cg}; output {'identical' if bo == co else 'DIFF'}; "
              f"delta {(cc / bb - 1) * 100:+.3f}%", flush=True)
    same = len(outputs) == 1
    baselines = [b[0] for _, b, _ in samples]
    candidates = [c[0] for _, _, c in samples]
    bb, cc = sum(baselines), sum(candidates)
    passed = ([layout for layout, _, _ in samples] == list(LAYOUTS[mode])
              and cc <= bb and same and enough)
    print(f"Ir {mode} mean base {bb / len(samples):.2f} cand {cc / len(samples):.2f}; "
          f"delta {(cc / bb - 1) * 100:+.3f}%; "
          f"spread base {min(baselines)}..{max(baselines)} "
          f"cand {min(candidates)}..{max(candidates)}; "
          f"output {'identical' if same else 'DIFF'}; {'PASS' if passed else 'FAIL'}", flush=True)
    if not enough:
        print("FAIL: regex requires at least 10 GC cycles per binary in every layout", flush=True)
    return passed


def pattern_profiles(candidate, baseline, gnu):
    """Record the same 6000 calls and ten full GCs for each probe pattern.

    These are whole-process instruction counts, including startup and GC.
    The separate `regex' mode retains the exact four-layout ABI acceptance.
    """
    version = subprocess.check_output([str(gnu), "-Q", "--batch", "--eval",
                                       "(princ emacs-version)"], text=True).strip()
    if version != "31.1":
        raise RuntimeError(f"GNU 31.1 required, got {version!r}")
    tmp = profile_directory("patterns")
    patterns = [r"\`\(.*\)\(\)\'", r"\`\(.*\)\(\.el\)\'",
                r"\`\(.*\)\(\.so\|\.elc\|\.elc\.gz\|\.el\|\.el\.gz\)\'", "file"]

    def gnu_one(directory):
        profile = directory / "gnu.cg"
        with (directory / "gnu.out").open("w") as out, (directory / "gnu.err").open("w") as err:
            result = subprocess.run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                                     f"--callgrind-out-file={profile}", str(Path(gnu).resolve()),
                                     "-Q", "--batch", "-l", str(directory / "regex.el")],
                                    cwd=ROOT, env=dict(os.environ, TMPDIR=str(directory)),
                                    stdout=out, stderr=err, timeout=580)
        if result.returncode or "ABI-REGEX 6000" not in (directory / "gnu.out").read_text():
            raise RuntimeError(f"incomplete GNU instruction profile: {directory}")
        total = next((int(line.split()[1]) for line in profile.read_text().splitlines()
                      if line.startswith("summary:")), 0)
        if total <= 0:
            raise RuntimeError(f"empty GNU instruction profile: {profile}")
        return total, 10

    print(f"profiles: {tmp}", flush=True)
    directories = []
    for index, pattern in enumerate(patterns):
        directory = tmp / str(index)
        directory.mkdir()
        directories.append(directory)
        directory.joinpath("regex.el").write_text(f'''
(let ((names (mapcar (lambda (k) (format "some-package-file-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i 20)
    (dolist (s names) (unless (string-match {json.dumps(pattern)} s) (error "Expected match")))
    (setq i (1+ i))
    (when (= (% i 2) 0) (garbage-collect))))
(princ "ABI-REGEX 6000\\n")
''')
    rows = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        jobs = {}
        for index, directory in enumerate(directories):
            for who, binary in (("base", baseline), ("cand", candidate)):
                jobs[index, who] = pool.submit(run_one, directory, "regex", who,
                                                Path(binary).resolve(), 0)
            jobs[index, "gnu"] = pool.submit(gnu_one, directory)
        for index, pattern in enumerate(patterns):
            row = {"pattern": pattern}
            for who in ("base", "cand", "gnu"):
                instructions, cycles = jobs[index, who].result()
                if cycles < 10:
                    raise RuntimeError(f"missing GC work: {index} {who}")
                row[who] = instructions
            rows.append(row)
            print(f"pattern {index}: base {row['base']} cand {row['cand']} GNU {row['gnu']} Ir", flush=True)
    identities = {who: {"path": str(Path(binary).resolve()),
                         "sha256": hashlib.sha256(Path(binary).read_bytes()).hexdigest()}
                  for who, binary in (("base", baseline), ("cand", candidate), ("gnu", gnu))}
    (tmp / "result.json").write_text(json.dumps({"calls": 6000, "gc_requests": 10,
                                                "identities": identities, "patterns": rows}, indent=2) + "\n")
    print(f"PATTERN-IR PASS {tmp / 'result.json'}", flush=True)
    return 0


def main():
    started = time.monotonic()
    if len(sys.argv) == 5 and sys.argv[1] == "patterns":
        return pattern_profiles(*sys.argv[2:])
    mode, cand, base = sys.argv[1:]
    tmp = profile_directory(mode)
    print(f"profiles: {tmp}", flush=True)
    # Same fixed work for both binaries. No GC instructions are subtracted.
    # Keep the original regex work and interleave ten explicit collections.
    # Count actual sweeps, including automatic GC, rather than requests.
    # The acceptance contract requires all four layouts, even with low spread.
    (tmp / "regex.el").write_text(rf'''
(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i {REGEX_ITERATIONS})
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))
    (when (= (% i {REGEX_GC_INTERVAL}) 0) (garbage-collect))))
(princ "ABI-REGEX {REGEX_ITERATIONS * 300}\n")
''')
    modes = ["regex"] if mode == "regex" else (["boot", "regex"] if mode == "ir" else ["files"])
    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        jobs = {(m, who, layout): pool.submit(run_one, tmp, m, who, Path(b).resolve(), layout)
                for m in modes for who, b in (("base", base), ("cand", cand)) for layout in LAYOUTS[m]}
        for m in modes:
            samples = [(layout, *(jobs[m, who, layout].result() +
                        ((tmp / f"{m}-{who}-{layout}.out").read_bytes(),)
                        for who in ("base", "cand"))) for layout in LAYOUTS[m]]
            ok &= summarize(m, samples)
    print(f"elapsed {time.monotonic() - started:.3f}s", flush=True)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
