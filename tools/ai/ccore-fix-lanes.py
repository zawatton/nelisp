#!/usr/bin/env python3
"""Generate fix lanes from probe lanes whose rows differ from GNU.

Usage: ccore-fix-lanes.py --wave ID --bundle FILE --out ROOT
                          [--exclude NAMES_FILE]... [--only NAMES_FILE]
                          PROBE_LANE...

Each PROBE_LANE is a lane directory holding one probe unit
(test/nelisp-emacs-lib/c-core-probes/*.el) and its last check result
(.lane-check/diff.txt).  Names with differing rows are assigned by the
MEASURED live definer (ccore-definers.py traces a real load of BUNDLE), so
every lane has exactly one writable file and edits the definition that is
actually live:

  package FILE   the last bundle file that defines the name -> one lane per file
  unit FILE      bulk stub or unbound -> new C-core unit, grouped by area
                 (integration must drop the names from the bulk stub list)
  prelude FILE   the bare runtime's Lisp closure stays live -> override file

Names whose live definition is a native builtin, or whose definer is not a
library package file, are listed in ROOT/unassigned-WAVE.tsv.  A package lane
takes at most MAX_PACKAGE names; the rest are listed there as `deferred'.

Environment: NELISP_BIN (standalone binary), EMACS (GNU Emacs 31.1 host).
"""
import argparse
import collections
import importlib.util
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

MAX_PACKAGE = 16
UNIT_CHUNK = 10
PRELUDE_CHUNK = 8
STUB_FILES = ("emacs-stub.el", "emacs-stub-bulk.el")

ENTRY_DUMP = r'''(progn
 (dolist (file command-line-args-left)
   (with-temp-buffer
     (insert-file-contents file)
     (goto-char (point-min))
     (condition-case nil
         (while t
           (let* ((form (read (current-buffer)))
                  (end (point))
                  (start (scan-sexps end -1)))
             (princ (format "\x1e%s\x1f%s" (car form) (buffer-substring start end)))))
       (end-of-file nil))))
 (setq command-line-args-left nil))'''


def probe_entries(host, files):
    """Return {name: entry text} read from probe FILES with the host reader."""
    out = subprocess.run([host, "-Q", "--batch", "--eval", ENTRY_DUMP] + files,
                         capture_output=True, text=True, timeout=120)
    if out.returncode:
        sys.exit("probe entry dump failed: " + out.stderr[-300:])
    entries = {}
    for chunk in out.stdout.split("\x1e")[1:]:
        name, text = chunk.split("\x1f", 1)
        entries[name] = text
    return entries


def differing_rows(diff_path):
    """Return {name: [diff line...]} from a lane check diff."""
    rows = collections.defaultdict(list)
    for line in Path(diff_path).read_text(errors="replace").splitlines():
        match = re.match(r"^[<>] P\| (\S+) \| ", line)
        if match:
            rows[match.group(1)].append(line)
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--wave", required=True)
    parser.add_argument("--bundle", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--exclude", action="append", default=[],
                        help="file of names already assigned to another lane")
    parser.add_argument("--only", help="file of names to restrict this wave to")
    parser.add_argument("--unit-chunk", type=int, default=UNIT_CHUNK,
                        help="names per new-unit lane")
    parser.add_argument("lanes", nargs="+")
    args = parser.parse_args()
    library = Path(__file__).resolve().parents[2]
    host = os.environ.get("EMACS", "emacs")
    binary = os.environ.get("NELISP_BIN", "")
    root = Path(args.out)
    root.mkdir(parents=True, exist_ok=True)

    entries, rows = {}, collections.defaultdict(list)
    for lane in args.lanes:
        lane = Path(lane)
        probes = sorted((lane / "test/nelisp-emacs-lib/c-core-probes").glob("*.el"))
        diff = lane / ".lane-check/diff.txt"
        if not probes or not diff.exists():
            print("skip %s: no probe file or check result" % lane.name, file=sys.stderr)
            continue
        entries.update(probe_entries(host, [str(p) for p in probes]))
        for name, lines in differing_rows(diff).items():
            rows[name].extend(lines)
    excluded = set()
    for path in args.exclude:
        excluded.update(Path(path).read_text().split())
    only = set(Path(args.only).read_text().split()) if args.only else None
    names = sorted(n for n in rows if n in entries and n not in excluded
                   and (only is None or n in only))
    if not names:
        sys.exit("no differing names found")

    names_file = root / ("names-%s.txt" % args.wave)
    names_file.write_text("\n".join(names) + "\n")
    traced = subprocess.run(
        [sys.executable, str(library / "tools/ai/ccore-definers.py"), args.bundle,
         str(names_file)], capture_output=True, text=True, timeout=900)
    if traced.returncode:
        sys.exit("definer trace failed: " + (traced.stderr or traced.stdout)[-300:])
    (root / ("definers-%s.tsv" % args.wave)).write_text(traced.stdout)
    spec = importlib.util.spec_from_file_location(
        "ccore_owner", str(library / "tools/ai/ccore-owner.py"))
    owner = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(owner)
    bare = owner.bare_kinds(binary, names)

    areas = {}
    for line in (library / "tools/c-core-areas.tsv").read_text().splitlines():
        fields = line.split("\t")
        if len(fields) == 2:
            areas[fields[0]] = fields[1]

    package, unit, prelude, unassigned = (
        collections.defaultdict(list), collections.defaultdict(list), [], [])
    for line in traced.stdout.splitlines():
        row = line.split("\t")
        if len(row) != 5:
            continue
        name, _bare, live, stub, chain = row
        # The stub property outlives the stub: a name is still a bulk stub
        # only when the stub files were its last definer.
        if live == "unbound" or (stub == "bulk" and live in STUB_FILES):
            unit[areas.get(name, "other")].append(name)
        elif live == "-":
            if bare.get(name) == "builtin":
                unassigned.append((name, "native", "native builtin"))
            else:
                prelude.append(name)
        else:
            sources = sorted(library.glob("packages/*/src/" + live))
            if len(sources) == 1:
                package[sources[0]].append(name)
            else:
                unassigned.append((name, "nofile", "%s (%s)" % (live, chain)))

    lanes = []  # (lane name, kind, file names, names, source paths)
    for source, group in sorted(package.items()):
        stem = re.sub(r"[^A-Za-z0-9]+", "-", source.name[:-3]).strip("-")
        group = sorted(group)
        lanes.append(("fix-%s-pkg-%s" % (args.wave, stem), "package", [source.name],
                      group[:MAX_PACKAGE], [source]))
        unassigned.extend((n, "deferred", source.name) for n in group[MAX_PACKAGE:])
    for area, group in sorted(unit.items()):
        for index in range(0, len(group), args.unit_chunk):
            number = index // args.unit_chunk + 1
            lanes.append(("fix-%s-unit-%s-%02d" % (args.wave, area, number), "unit",
                          ["emacs-cc-census-%s-%s%02d.el" % (area, args.wave, number)],
                          group[index:index + args.unit_chunk], []))
    for index in range(0, len(prelude), PRELUDE_CHUNK):
        number = index // PRELUDE_CHUNK + 1
        lanes.append(("fix-%s-prelude-%02d" % (args.wave, number), "prelude",
                      ["prelude-overrides-%s%02d.el" % (args.wave, number)],
                      prelude[index:index + PRELUDE_CHUNK], []))

    brief = (library / "tools/ai/ccore-fix-lane-brief.md").read_text()
    for lane_name, kind, file_names, group, sources in lanes:
        lane = root / lane_name
        subprocess.run(["bash", str(library / "tools/ai/ccore-lane.sh"), "new",
                        str(lane), args.bundle], check=True, stdout=subprocess.DEVNULL)
        (lane / "names.txt").write_text("\n".join(group) + "\n")
        (lane / "unit.txt").write_text(lane_name + "\n")
        (lane / "KIND.txt").write_text("%s %s\n" % (kind, " ".join(file_names)))
        (lane / "check.sh").write_text(
            "#!/usr/bin/env bash\n# Strict parity check for this lane (about 50 s).\n"
            "cd \"$(dirname \"$0\")\" && exec bash \"$LIB/tools/ai/ccore-lane.sh\" check --strict %s %s\n"
            % (lane_name, " ".join("units/" + f for f in file_names)))
        (lane / "rebind.txt").write_text(
            "" if kind == "prelude" else "\n".join(group) + "\n")
        probe = lane / "test/nelisp-emacs-lib/c-core-probes" / (lane_name + ".el")
        probe.write_text(";;; %s.el --- probes for one fix lane  -*- lexical-binding: t; -*-\n%s\n"
                         % (lane_name, "\n".join(entries[n] for n in group)))
        (lane / "DIVERGENCES.txt").write_text(
            "\n".join(line for n in group for line in rows[n]) + "\n")
        for source in sources:
            (lane / "seed").mkdir(exist_ok=True)
            shutil.copy(source, lane / "seed" / source.name)
            shutil.copy(source, lane / "units" / source.name)
        (lane / "BRIEF.md").write_text(brief)
        print("%s\t%s\t%s\t%d" % (lane_name, kind, " ".join(file_names), len(group)))

    with open(root / ("unassigned-%s.tsv" % args.wave), "w") as stream:
        for row in unassigned:
            stream.write("\t".join(row) + "\n")
    print("lanes=%d names=%d unassigned=%d" % (
        len(lanes), sum(len(l[3]) for l in lanes), len(unassigned)), file=sys.stderr)


if __name__ == "__main__":
    main()
