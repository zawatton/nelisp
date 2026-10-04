#!/usr/bin/env python3
"""Merge fix lanes into one feature lane that owns several files.

Usage: ccore-merge-lanes.py NEW_LANE_DIR SOURCE_LANE...

Single-file lanes cannot fix a feature whose getter, setter and state live in
different files: each lane stops at "the other half is outside this lane".
This merges the SOURCE lanes (created by ccore-fix-lanes.py, not yet
dispatched) into NEW_LANE_DIR: names, probe entries, differing rows, rebind
names and every writable file are combined, check.sh hot-loads all files in
bundle order, and KIND.txt becomes `package FILE...'.  A source lane of kind
`unit' contributes its new unit file name, which the worker creates; the
integrator treats a file without a seed as a new C-core unit.  The source
lane directories are removed.  Prelude lanes cannot be merged.
"""
import os
import re
import shutil
import sys
from pathlib import Path


def main():
    if len(sys.argv) < 4:
        sys.exit(__doc__.split("\n")[2])
    new = Path(sys.argv[1])
    sources = [Path(p) for p in sys.argv[2:]]
    name = new.name
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]*", name):
        sys.exit("unsafe lane name")
    for lane in sources:
        if (lane / "codex.log").exists():
            sys.exit("%s was already dispatched" % lane.name)
        if (lane / "KIND.txt").read_text().split()[0] == "prelude":
            sys.exit("%s is a prelude lane" % lane.name)
    first = sources[0]
    shutil.copytree(first, new, symlinks=True)
    probes = new / "test/nelisp-emacs-lib/c-core-probes"
    for old in probes.glob("*.el"):
        old.unlink()
    names, entries, rows, files = [], [], [], []
    for lane in sources:
        names += (lane / "names.txt").read_text().split()
        unit = (lane / "unit.txt").read_text().strip()
        text = (lane / "test/nelisp-emacs-lib/c-core-probes" / (unit + ".el")).read_text()
        entries.append(text.split("\n", 1)[1])
        rows.append((lane / "DIVERGENCES.txt").read_text())
        for file_name in (lane / "KIND.txt").read_text().split()[1:]:
            if file_name not in files:
                files.append(file_name)
        for sub in ("seed", "units"):
            if (lane / sub).is_dir():
                (new / sub).mkdir(exist_ok=True)
                for path in (lane / sub).glob("*.el"):
                    shutil.copy(path, new / sub / path.name)
    # Existing library files load in bundle order; new unit files load last.
    bundle = os.path.realpath(new / "build/nemacs-bootstrap.el")
    order = {}
    for index, line in enumerate(Path(bundle).read_text(errors="replace").splitlines()):
        if line.startswith(";;; >>> src/"):
            order.setdefault(line[len(";;; >>> src/"):].strip(), index)
    files.sort(key=lambda f: (0, order[f]) if f in order else (1, f))
    (new / "names.txt").write_text("\n".join(names) + "\n")
    (new / "rebind.txt").write_text("\n".join(names) + "\n")
    (new / "unit.txt").write_text(name + "\n")
    (new / "KIND.txt").write_text("package %s\n" % " ".join(files))
    (probes / (name + ".el")).write_text(
        ";;; %s.el --- probes for one feature lane  -*- lexical-binding: t; -*-\n%s"
        % (name, "".join(entries)))
    (new / "DIVERGENCES.txt").write_text("".join(rows))
    (new / "check.sh").write_text(
        "#!/usr/bin/env bash\n# Strict parity check for this lane (about 50 s).\n"
        "cd \"$(dirname \"$0\")\" && exec bash \"$LIB/tools/ai/ccore-lane.sh\" check --strict %s $(for f in %s; do [ -f \"$f\" ] && printf '%%s ' \"$f\"; done)\n"
        % (name, " ".join("units/" + f for f in files)))
    for lane in sources:
        shutil.rmtree(lane)
    print("%s\tpackage\t%s\t%d" % (name, " ".join(files), len(names)))


if __name__ == "__main__":
    main()
