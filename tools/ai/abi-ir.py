#!/usr/bin/env python3
"""Doc 215 total-Ir comparisons; retain profiles and count actual GC sweeps."""
import bisect
import concurrent.futures
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
LAYOUTS = {"boot": (0, 700, 1400, 2100), "regex": (0, 2100),
           "files": (0, 700, 1400, 2100)}
REGEX_ITERATIONS = 20
REGEX_GC_INTERVAL = 2
GC_CYCLE_FUNCTION = "nl_gc_sweep_merge_chunks"


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


def main():
    started = time.monotonic()
    mode, cand, base = sys.argv[1:]
    tmp_root = ROOT / "target/abi-ir"
    tmp_root.mkdir(parents=True, exist_ok=True)
    tmp = Path(tempfile.mkdtemp(prefix=f"{mode}-", dir=tmp_root))
    print(f"profiles: {tmp}", flush=True)
    # Same fixed work for both binaries. No GC instructions are subtracted.
    # Keep the original regex work and interleave ten explicit collections.
    # Count actual sweeps, including automatic GC, rather than requests.
    # Prior four-layout regex spread was <0.001%; retain endpoint layouts.
    (tmp / "regex.el").write_text(rf'''
(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i {REGEX_ITERATIONS})
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))
    (when (= (% i {REGEX_GC_INTERVAL}) 0) (garbage-collect))))
(princ "ABI-REGEX {REGEX_ITERATIONS * 300}\n")
''')
    modes = ["boot", "regex"] if mode == "ir" else ["files"]
    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=16) as pool:
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
