#!/usr/bin/env python3
"""Profile where the bootstrap bundle's load time goes, per bundled file.

Usage: ccore-load-profile.py BUNDLE [--top N] [--against OTHER_BUNDLE]

The bundle is copied with a timestamp probe after every `;;; <<< src/FILE'
marker and loaded once by the standalone.  One TSV row per file is printed,
slowest first: SECONDS, FILE.  With --against, the same profile is taken for
OTHER_BUNDLE and the rows are SECONDS, DELTA (this minus other), FILE, sorted
by DELTA; files only in one bundle show the other side as 0.

A file that loads another one from disk is charged for it.  Timings are wall
clock from one run each (about 50 s per bundle): run it on an idle machine and
treat differences under roughly 0.2 s as noise.

Environment: NELISP_BIN (standalone binary).  Runs from the library root.
"""
import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path

PRELUDE = '(setq ccore-prof--last (float-time))\n(setq ccore-prof--start ccore-prof--last)\n'
PROBE = ('(let ((now (float-time))) (princ (format "T\\t%%.3f\\t%s\\n" (- now ccore-prof--last))) '
         '(setq ccore-prof--last now))')


def profile(binary, library, bundle):
    lines = [PRELUDE]
    for line in Path(bundle).read_text(errors="surrogateescape").splitlines():
        lines.append(line)
        if line.startswith(";;; <<< "):
            lines.append(PROBE % line[len(";;; <<< "):].strip().replace("src/", "", 1))
    lines.append(PROBE % "bundle-tail")
    lines.append('(princ (format "TOTAL\\t%.3f\\n" (- (float-time) ccore-prof--start)))')
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False,
                                     errors="surrogateescape") as stream:
        stream.write("\n".join(lines) + "\n")
        traced = stream.name
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as stream:
        stream.write('(load "%s" nil t)\n' % traced)
        runner = stream.name
    try:
        out = subprocess.run([binary, "--load", runner, "--eval", "nil"], cwd=str(library),
                             capture_output=True, timeout=400, text=True, errors="replace")
    finally:
        os.unlink(traced)
        os.unlink(runner)
    times, total = {}, None
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if parts[0] == "T" and len(parts) == 3:
            times[parts[2]] = times.get(parts[2], 0.0) + float(parts[1])
        elif parts[0] == "TOTAL":
            total = float(parts[1])
    if total is None:
        sys.exit("profile incomplete; stderr: " + out.stderr[-300:])
    return times, total


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("bundle")
    parser.add_argument("--top", type=int, default=25)
    parser.add_argument("--against")
    args = parser.parse_args()
    binary = os.environ.get("NELISP_BIN")
    if not binary or not os.access(binary, os.X_OK):
        sys.exit("NELISP_BIN missing or not executable")
    library = Path(__file__).resolve().parents[2]
    times, total = profile(binary, library, os.path.abspath(args.bundle))
    if not args.against:
        print("TOTAL\t%.3f" % total)
        for name, seconds in sorted(times.items(), key=lambda kv: -kv[1])[:args.top]:
            print("%.3f\t%s" % (seconds, name))
        return
    other, other_total = profile(binary, library, os.path.abspath(args.against))
    print("TOTAL\t%.3f\t%+.3f" % (total, total - other_total))
    rows = [(times.get(n, 0.0), times.get(n, 0.0) - other.get(n, 0.0), n)
            for n in set(times) | set(other)]
    for seconds, delta, name in sorted(rows, key=lambda r: -r[1])[:args.top]:
        print("%.3f\t%+.3f\t%s" % (seconds, delta, name))


if __name__ == "__main__":
    main()
