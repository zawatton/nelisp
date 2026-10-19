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

ROOT = Path(__file__).resolve().parents[2]


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
            if line.startswith("cfn="):
                target = name
        elif line.startswith("calls=") and target == "nl_gc_sweep":
            cycles += int(line[6:].split()[0])
    if total is None or total <= 0:
        raise ValueError(f"missing positive total Ir: {profile}")
    return total, cycles


def run_one(tmp, mode, who, binary):
    stem = tmp / f"{mode}-{who}"
    cache = tmp / f"{mode}-{who}-cache"
    cache.mkdir()  # must be empty for every process, including boot/regex
    env = dict(os.environ, XDG_CACHE_HOME=str(cache), TMPDIR=str(tmp), ABI_SOURCE_ROOT=str(ROOT))
    args = ["--eval", "nil"] if mode == "boot" else ["--load", str(
        tmp / "regex.el" if mode == "regex" else ROOT / "tools/ai/abi-file-exec.el")]
    with stem.with_suffix(".out").open("w") as out, stem.with_suffix(".err").open("w") as err:
        result = subprocess.run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                                 f"--callgrind-out-file={stem}.cg", str(binary), *args],
                                cwd=ROOT, env=env, stdout=out, stderr=err, timeout=1100)
    if result.returncode:
        raise RuntimeError(f"{mode} {who} exit {result.returncode}: {stem}.err")
    return profile_counts(stem.with_suffix(".cg"), binary)


def main():
    mode, cand, base = sys.argv[1:]
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
    modes = ["boot", "regex"] if mode == "ir" else ["files"]
    ratio = float(os.environ.get("ABI_GATE_IR_RATIO", "1.00"))
    ok = True
    with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
        jobs = {(m, who): pool.submit(run_one, tmp, m, who, Path(b).resolve())
                for m in modes for who, b in (("base", base), ("cand", cand))}
        for m in modes:
            bb, bg = jobs[m, "base"].result()
            cc, cg = jobs[m, "cand"].result()
            same = (tmp / f"{m}-base.out").read_bytes() == (tmp / f"{m}-cand.out").read_bytes()
            enough = m != "regex" or min(bg, cg) >= 10
            passed = cc <= bb * ratio and same and enough
            ok &= passed
            print(f"Ir {m} base {bb} cand {cc}; GC base {bg} cand {cg}; "
                  f"output {'identical' if same else 'DIFF'}; "
                  f"{'PASS' if passed else 'FAIL'} ({(cc / bb - 1) * 100:+.3f}%)", flush=True)
            if not enough:
                print("FAIL: regex requires at least 10 GC cycles per binary", flush=True)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
