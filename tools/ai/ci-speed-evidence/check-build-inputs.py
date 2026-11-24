#!/usr/bin/env python3
"""S1.2: every repo file a real `make standalone-reader' opens is covered by the
stamp key (tools/nelisp-standalone-reader-build.sh) or is a listed derived cache.

Two modes.

  check-build-inputs.py [--list FILE] [--root DIR]
      Load the recorded list (default: build-opened-files.txt next to this
      script) and exit 0 only if every in-repo path in it is covered:
        * under lisp/ src/ scripts/ vendor/ packages/ templates/ standalone-compat/ AND present in the live `find' set
          the key hashes (so a recorded input that left the key's file set is
          caught), or a recorded probe of a path that does not exist;
        * the Makefile or the stamp tool itself (both hashed by the key);
        * or under an ALLOWED_DERIVED prefix below.
      Writes into the key's roots are always errors: the tool would see the
      key move during the build and refuse to stamp.
      Exit 1: uncovered path(s).  Exit 2: unusable list.

  check-build-inputs.py --reduce STRACE_LOG [--root DIR] [--out FILE]
      Reduce `strace -f -e trace=openat,open,execve' output to the list format.

List format, one entry per line, sorted, repo-relative, no absolute home paths:
  R path    opened for reading and found
  W path    opened for writing or creating
  M path    open of a path that did not exist (a probe)
  X path    executed program; /usr, /bin and similar are kept as absolute
            system paths, anything inside the repo is repo-relative
Lines starting with # are comments.
"""

import argparse
import os
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
DEFAULT_LIST = HERE / "build-opened-files.txt"

# Directories whose every file the key hashes (the key uses `find', so ignored
# .elc files count too) and the two single files it hashes in addition.
# Keep in sync with KEY_DIRS / KEY_FILES in the stamp tool.  packages/ and
# templates/ and the two test fixtures were added because THIS evidence showed
# the build reads them.
KEY_ROOTS = ("lisp/", "src/", "scripts/", "vendor/", "packages/", "templates/",
             "standalone-compat/")
KEY_FILES = ("Makefile", "tools/nelisp-standalone-reader-build.sh",
             "test/nelisp-prelude-bytecode-parity-fixtures.tsv",
             "test/fixtures/native-bytecode/gnu-31.1-opcodes.json")

# Derived caches and outputs under target/.  Each is an OUTPUT of the build or
# a content-addressed cache whose entries are derived from the key's inputs;
# none is an input the key could be missing.  Keep this list short and
# commented; anything not listed here and not covered above fails the check.
# Entries are string prefixes.
ALLOWED_DERIVED = (
    # The output binary and everything the build writes beside it: .cold image,
    # artifact-runtime cache, boot image, version.json, bytecode reports, the
    # native-startup source.  The binary and the .cold/.nelc companions are
    # checked by the stamp itself; the rest are re-derived on every build.
    "target/nelisp",
    "target/.nelisp-artifact-runtime",
    # Per-unit compiled-object cache: content addressed (source, toolchain,
    # settings), mode-separated directories.  Derived from the key's inputs.
    "target/standalone-units/",
    # mktemp work directories of one build (rooted protocol, F1 compiler,
    # constructor): written and read back within the same run.
    "target/standalone-rooted-protocol-",
    "target/standalone-compiler-",
    "target/compiler-proof-template-",
)


# The cwd itself, and .git: only the git subprocesses read it, and the key
# includes their output (`git rev-parse HEAD', `git describe ... --dirty').
COVERED_BY_GIT_IDENTITY = (".", ".git")


def is_covered(rel, kind, live):
    if rel in COVERED_BY_GIT_IDENTITY or rel.startswith(".git/"):
        return True
    if rel in KEY_FILES:
        return True
    if rel.startswith(KEY_ROOTS):
        if kind == "M":
            return True  # adding the file later changes the key
        return rel in live
    return rel.startswith(ALLOWED_DERIVED)


def live_key_files(root):
    out = subprocess.run(
        ["find", *[r.rstrip("/") for r in KEY_ROOTS], "(", "-type", "f", "-o", "-type", "l", ")", "-print0"],
        cwd=root, capture_output=True, check=True)
    return {p.decode() for p in out.stdout.split(b"\0") if p}


def load(path):
    entries = []
    for n, line in enumerate(Path(path).read_text().splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        m = re.match(r"^([RWMX]) (\S.*)$", line)
        if not m:
            raise ValueError(f"{path}:{n}: unparsable line: {line!r}")
        entries.append((m.group(1), m.group(2)))
    if not entries:
        raise ValueError(f"{path}: no entries")
    return entries


def check(list_path, root):
    try:
        entries = load(list_path)
    except (OSError, ValueError) as e:
        print(f"FAIL: {e}", file=sys.stderr)
        return 2
    live = live_key_files(root)
    bad = []
    covered = 0
    for kind, rel in entries:
        if kind == "X":
            continue
        if kind == "W":
            if rel.startswith(KEY_ROOTS) or rel in KEY_FILES:
                bad.append((kind, rel, "build WRITES into the key's own roots"))
            elif not rel.startswith(ALLOWED_DERIVED):
                bad.append((kind, rel, "written file is not a listed derived output"))
            else:
                covered += 1
            continue
        if is_covered(rel, kind, live):
            covered += 1
        else:
            bad.append((kind, rel, "not under the key's roots and not an allowed derived cache"))
    for kind, rel, why in bad:
        print(f"UNCOVERED {kind} {rel}: {why}", file=sys.stderr)
    execs = sorted({rel for kind, rel in entries if kind == "X"})
    print(f"build-inputs: {len(entries)} entries, {covered} covered, {len(bad)} uncovered, "
          f"{len(execs)} executables")
    return 1 if bad else 0


OPEN_RE = re.compile(
    r'^(?:\[pid\s+\d+\]\s+|\d+\s+)?(?:openat|open)\((?:AT_FDCWD,\s*)?"([^"]*)",\s*([A-Z_|0-9]+)[^)]*\)\s*=\s*(-?\d+)(?:\s+(\w+))?')
OPENAT_DIRFD_RE = re.compile(r'(?:openat)\((\w+),')
EXEC_RE = re.compile(r'^(?:\[pid\s+\d+\]\s+|\d+\s+)?execve\("([^"]*)",.*\)\s*=\s*(-?\d+)')


def reduce_log(log, root, out):
    root = str(Path(root).resolve())
    entries = set()
    for line in Path(log).read_text(errors="replace").splitlines():
        m = EXEC_RE.match(line)
        if m:
            path, rc = m.group(1), int(m.group(2))
            if rc < 0:
                continue
            entries.add(("X", rel_or_abs(path, root)))
            continue
        if "openat(" in line:
            d = OPENAT_DIRFD_RE.search(line)
            if d and d.group(1) != "AT_FDCWD":
                continue  # relative to an fd we do not track
        m = OPEN_RE.match(line)
        if not m:
            continue
        path, flags, rc = m.group(1), m.group(2), int(m.group(3))
        if "O_DIRECTORY" in flags:
            continue
        if not os.path.isabs(path):
            path = os.path.join(root, path)
        path = os.path.normpath(path)
        if not (path == root or path.startswith(root + os.sep)):
            continue
        rel = os.path.relpath(path, root)
        writing = any(f in flags for f in ("O_WRONLY", "O_RDWR", "O_CREAT", "O_TRUNC"))
        if rc < 0:
            if "ENOENT" in line:
                entries.add(("M", rel))
            continue
        entries.add(("W" if writing else "R", rel))
    lines = sorted(f"{k} {p}" for k, p in entries)
    Path(out).write_text(
        "# Reduced from `strace -f -e trace=openat,open,execve make standalone-reader'.\n"
        "# Repo-relative; system executables stay absolute.  See check-build-inputs.py.\n"
        + "\n".join(lines) + "\n")
    print(f"reduced {len(lines)} entries to {out}")


def rel_or_abs(path, root):
    path = os.path.normpath(path)
    if path.startswith(root + os.sep):
        return os.path.relpath(path, root)
    return path


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--list", default=str(DEFAULT_LIST))
    ap.add_argument("--root", default=str(HERE.parent.parent.parent))
    ap.add_argument("--reduce", metavar="STRACE_LOG")
    ap.add_argument("--out", default=str(DEFAULT_LIST))
    a = ap.parse_args()
    if a.reduce:
        reduce_log(a.reduce, a.root, a.out)
        return 0
    return check(a.list, a.root)


if __name__ == "__main__":
    sys.exit(main())
