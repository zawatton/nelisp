#!/usr/bin/env python3
"""Verify finished fix lanes independently and integrate the ones that improve.

Usage: ccore-lane-integrate.py [--jobs N] [--dry-run] --prelude-queue DIR LANE...

For every LANE (created by ccore-fix-lanes.py) this re-runs the lane's own
strict check (check.sh) outside the worker's sandbox, never trusting the
worker's report, and classifies the lane:

  clean     zero differing rows and empty standalone stderr
  partial   fewer differing rows than before and no row that newly differs
  rejected  anything else (no improvement, new differing rows, broken run,
            unbalanced parentheses, or a seed that no longer matches the tree)

Clean and partial lanes are integrated: `package' files replace the library
file (only when the library file still equals the lane's seed), `unit' files
are added to packages/nelisp-emacs-foundation/src/, and `prelude' override
files are copied to the prelude queue for the coordinator to splice into the
runtime.  One TSV row per lane is printed: LANE, VERDICT, BEFORE, AFTER, NOTE.

Environment: NELISP_BIN, EMACS (GNU Emacs 31.1 host), LIB is set here.
"""
import argparse
import concurrent.futures
import filecmp
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

LIBRARY = Path(__file__).resolve().parents[2]


def row_keys(path, marker):
    """Return the differing rows (MARKER is `<' or `>') of a diff as a list."""
    if not path.exists():
        return []
    return [line[2:] for line in path.read_text(errors="replace").splitlines()
            if line.startswith(marker + " P| ")]


def package_path(base):
    """Return the library source path of file BASE, or None."""
    found = sorted(LIBRARY.glob("packages/*/src/" + base))
    return found[0] if len(found) == 1 else None


def parens_ok(host, path):
    out = subprocess.run(
        [host, "-Q", "--batch", "--eval",
         '(with-temp-buffer (insert-file-contents "%s") (emacs-lisp-mode) (check-parens))' % path],
        capture_output=True, text=True, timeout=60)
    return out.returncode == 0


def verify(lane):
    """Run LANE's strict check; return (lane, verdict, before, after, note)."""
    lane = Path(lane)
    host = os.environ.get("EMACS", "emacs")
    kind_line = (lane / "KIND.txt").read_text().split()
    kind, files = kind_line[0], kind_line[1:]
    before_rows = row_keys(lane / "DIVERGENCES.txt", "<")
    # In a merged feature lane a new unit file is optional: the worker may
    # have put everything into the existing files.  A file with a seed, or the
    # only file of a single-file lane, must exist.
    missing = [f for f in files if not (lane / "units" / f).exists()
               and ((lane / "seed" / f).exists() or len(files) == 1)]
    files = [f for f in files if (lane / "units" / f).exists()]
    if missing:
        return lane, "rejected", len(before_rows), -1, "unit file missing: " + " ".join(missing)
    for name in files:
        if not parens_ok(host, lane / "units" / name):
            return lane, "rejected", len(before_rows), -1, "unbalanced parentheses in " + name
    env = dict(os.environ, LIB=str(LIBRARY))
    run = subprocess.run(["bash", str(lane / "check.sh")], capture_output=True,
                         text=True, timeout=400, env=env)
    match = re.search(r"LANE-CHECK unit=\S+ rows=(\d+) differing=(\d+) standalone_stderr_bytes=(\d+)",
                      run.stdout)
    if not match:
        return lane, "rejected", len(before_rows), -1, "check did not complete: " + run.stderr[-160:].replace("\n", " ")
    after, stderr_bytes = int(match.group(2)), int(match.group(3))
    after_rows = row_keys(lane / ".lane-check/diff.txt", "<")
    # A row is identified by its GNU line; rows are compared as multisets so a
    # primitive with several identical GNU lines is still counted correctly.
    remaining = list(before_rows)
    new_rows = []
    for row in after_rows:
        if row in remaining:
            remaining.remove(row)
        else:
            new_rows.append(row)
    if stderr_bytes:
        return lane, "rejected", len(before_rows), after, "standalone wrote to stderr"
    if new_rows:
        return lane, "rejected", len(before_rows), after, "newly differing: " + new_rows[0][:80]
    if after == 0:
        return lane, "clean", len(before_rows), 0, ""
    if after < len(before_rows):
        return lane, "partial", len(before_rows), after, ""
    return lane, "rejected", len(before_rows), after, "no improvement"


def integrate(lane, queue, dry_run):
    """Copy LANE's files into the tree; return a note, or raise ValueError."""
    kind_line = (lane / "KIND.txt").read_text().split()
    kind, files = kind_line[0], kind_line[1:]
    plan = []
    for name in files:
        source = lane / "units" / name
        if not source.exists():
            continue
        # A merged feature lane has kind "package" but may add a new unit
        # file, recognisable by having no seed.
        file_kind = kind
        if kind == "package" and not (lane / "seed" / name).exists():
            file_kind = "unit"
        if file_kind == "package":
            target = package_path(name)
            if target is None:
                raise ValueError("no unique library file for " + name)
            if not filecmp.cmp(target, lane / "seed" / name, shallow=False):
                raise ValueError("library file changed since the lane's seed: " + name)
            if filecmp.cmp(target, source, shallow=False):
                continue
        elif file_kind == "unit":
            target = LIBRARY / "packages/nelisp-emacs-foundation/src" / name
            if target.exists():
                raise ValueError("unit already exists: " + name)
        elif file_kind == "prelude":
            target = Path(queue) / name
        else:
            raise ValueError("unknown lane kind " + kind)
        plan.append((source, target, file_kind))
    if not dry_run:
        for source, target, file_kind in plan:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy(source, target)
            if file_kind == "unit":
                # Bulk stubs load before the units; an implemented name must
                # leave the stub list or its guarded definition never installs.
                subprocess.run([sys.executable, str(LIBRARY / "tools/ai/ccore-bulk-remove.py"),
                                str(target)], check=True, stdout=subprocess.DEVNULL)
    return "%d file(s)" % len(plan)


def post_verify(lane, bundle):
    """Re-run LANE's probes against the rebuilt BUNDLE with no unit files."""
    lane = Path(lane)
    verdict = (lane / "VERDICT").read_text().split("\t") if (lane / "VERDICT").exists() else [""]
    if verdict[0] not in ("clean", "partial"):
        return lane, None
    link = lane / "build/nemacs-bootstrap.el"
    if link.is_symlink() or link.exists():
        link.unlink()
    link.symlink_to(bundle)
    env = dict(os.environ, LIB=str(LIBRARY))
    run = subprocess.run(["bash", str(LIBRARY / "tools/ai/ccore-lane.sh"), "check",
                          (lane / "unit.txt").read_text().strip()],
                         capture_output=True, text=True, timeout=400, env=env, cwd=str(lane))
    match = re.search(r"rows=(\d+) differing=(\d+) standalone_stderr_bytes=(\d+)", run.stdout)
    result = ("differing=%s stderr=%s" % (match.group(2), match.group(3)) if match
              else "check did not complete")
    (lane / "POST").write_text("%s\t%s\t%s\n" % (verdict[0], verdict[2] if len(verdict) > 2 else "?", result))
    return lane, (verdict[0], verdict[2] if len(verdict) > 2 else "?", result)


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--prelude-queue")
    parser.add_argument("--post", metavar="BUNDLE",
                        help="instead of integrating, re-run integrated lanes' probes "
                             "against this rebuilt bundle with no unit files")
    parser.add_argument("lanes", nargs="+")
    args = parser.parse_args()
    if args.post:
        bundle = os.path.abspath(args.post)
        with concurrent.futures.ThreadPoolExecutor(args.jobs) as pool:
            for lane, result in pool.map(lambda l: post_verify(l, bundle), args.lanes):
                if result:
                    print("%s\tpost\tlane-after=%s\t%s" % (lane.name, result[1], result[2]))
        return
    if not args.prelude_queue:
        parser.error("--prelude-queue is required when integrating")
    with concurrent.futures.ThreadPoolExecutor(args.jobs) as pool:
        results = list(pool.map(verify, args.lanes))
    # Integrate sequentially: package lanes may not overlap, but the seed
    # comparison must see each earlier copy.
    for lane, verdict, before, after, note in results:
        if verdict in ("clean", "partial"):
            try:
                note = integrate(lane, args.prelude_queue, args.dry_run)
            except ValueError as error:
                verdict, note = "rejected", str(error)
        (lane / "VERDICT").write_text("%s\t%d\t%d\t%s\n" % (verdict, before, after, note))
        print("%s\t%s\t%d\t%d\t%s" % (lane.name, verdict, before, after, note))


if __name__ == "__main__":
    main()
