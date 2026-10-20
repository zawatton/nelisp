#!/usr/bin/env python3
"""Run each object population in a fresh process and retain GNU evidence."""
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
TYPES = ("cons", "float", "string8", "symbol", "vector-slot", "record1", "bignum", "marker")


def main():
    out = Path(sys.argv[1]).resolve()
    out.mkdir(parents=True, exist_ok=True)
    bins = dict(cand=Path(sys.argv[2]).resolve(), base=Path(sys.argv[3]).resolve(),
                gnu=Path(os.environ.get("WH_GNU_EMACS", shutil.which("emacs"))).resolve())
    probe = ROOT / "tools/ai/word-heap-objsize.el"
    started = time.monotonic()

    def run(job):
        who, kind, n = job
        cmd = [str(bins[who]), *(["-Q", "--batch", "-l"] if who == "gnu" else ["--load"]), str(probe)]
        if who != "gnu":
            cmd += ["--eval", "nil"]
        r = subprocess.run(cmd, env=dict(os.environ, WH_TYPE=kind, WH_N=str(n)),
                           capture_output=True, timeout=180, cwd=ROOT)
        stem = out / f"{who}-{kind}-{n}"
        stem.with_suffix(".out").write_bytes(r.stdout)
        stem.with_suffix(".err").write_bytes(r.stderr)
        stem.with_suffix(".rc").write_text(str(r.returncode) + "\n")
        if r.returncode or r.stderr or b"WH-OBJECT-DONE\n" not in r.stdout:
            raise RuntimeError(f"{stem}: exit={r.returncode}, stderr={r.stderr[:100]!r}")
        match = re.search(rb"WH-OBJECT \S+ \d+ (-?\d+)", r.stdout)
        if not match:
            raise ValueError(f"no population measurement: {stem}")
        return [who, kind, n, int(match[1])]

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        rows = list(pool.map(run, [(who, kind, n) for who in bins for kind in TYPES for n in (2000, 4000)]))
    table = {who: {kind: (next(r[3] for r in rows if r[:3] == [who, kind, 4000])
                         - next(r[3] for r in rows if r[:3] == [who, kind, 2000])) / 2000
                   for kind in TYPES} for who in bins}
    def digest(p):
        with Path(p).open("rb") as stream:
            return hashlib.file_digest(stream, "sha256").hexdigest()
    result = dict(table=table, populations=rows, seconds=time.monotonic() - started,
                  probe_sha256=digest(probe), binaries={k: dict(path=str(p), sha256=digest(p)) for k, p in bins.items()},
                  method="(delta4000 - delta2000)/2000; keeper slots excluded; two GCs, conservative scanning disabled",
                  gnu_version=subprocess.check_output([str(bins["gnu"]), "-Q", "--batch", "--eval", "(princ emacs-version)"], text=True))
    (out.parent / "objsize.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(table))


if __name__ == "__main__":
    main()
