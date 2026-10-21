#!/usr/bin/env python3
"""Checker for tools/ai/symbols-progress.org (Doc 226, GNU struct Lisp_Symbol).

Subcommands (one per ledger criterion):
  plan              S0.1  Doc 226 maps operations to GNU sources, holds the
                          mirror-table census and the LOCALIZED interop plan
  layout            S1.1  header-less symbol objects, bytes per symbol
  census            S1.2  no mirror-table lookups on PLAINVAL/VARALIAS/FORWARDED
  semantics         S1.3  GNU 31.1 oracle fixture on dynamic and static
  speed globals     S2.1  own cost of symbol-value/set/boundp vs GNU
  speed workloads   S2.2  .elc mxlib benchmark and interpreted calls vs base
  final             S3.1  final verification evidence
Helpers:
  census-static [REV]     print the static mirror-table census (working tree,
                          or the git revision REV)
"""
import bisect
import collections
import json
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/symbols"
DOC = ROOT / "docs/design/226-gnu-symbol-port.org"
GNU_SRC = Path("/home/madblack-21/Downloads/emacs-31.1/src")
# The census in Doc 226 is the "before" state: the merge of callpath-perf's
# direct cells onto the symbols base, before any change of this lane.
BEFORE_REV = "915e1a17a"


def need(cond, msg):
    if not cond:
        print("FAIL: " + msg, file=sys.stderr)
        sys.exit(1)


# ---------------------------------------------------------------- census
# A "mirror-table use" is a call site of a function of the env mirror (the
# name-keyed fast-hash-table of symbol-entry records, lisp/nelisp-cc-mirror-*)
# or of the name hash it uses, in a native runtime function outside the
# mirror module.  Callers are grouped by subsystem from their name prefix.
MIRROR_API = re.compile(
    r"\((?:extern-call\s+)?(nelisp_mirror_[a-z0-9_]+|nl_symcell_[a-z_]+|nelisp_fnv1a)\b")
DEFUN = re.compile(r"\(defun\s+([A-Za-z0-9_]+)\s")
SUBSYSTEMS = [
    ("env value/function cells", ("nelisp_env_",)),
    ("interpreter globals ops (setq, defvaralias, watchers)", ("nl_n3_",)),
    ("variable aliases", ("nl_pa_",)),
    ("specbind (shallow binding)", ("nl_sb_",)),
    ("bytecode VM", ("wf_bytecode_",)),
    ("native code (FX/N4)", ("m5_fx_", "nl_fx_", "bf_fx_", "nl_native_", "nl_n4_")),
    ("function cells (fset, install, apply)", ("nl_apply_", "nl_install_", "nl_ei_")),
    ("lexical frames (name hash)", ("nelisp_frame_", "nl_frame_")),
    ("builtins", ("bf_", "m5_sort_")),
    ("hashing (sxhash)", ("nl_jit_",)),
]


def source_files(rev=None):
    """Yield (relative path, text) of the native runtime sources."""
    if rev is None:
        paths = sorted(p.relative_to(ROOT).as_posix()
                       for p in ROOT.glob("lisp/nelisp-cc-*.el"))
        paths.append("scripts/nelisp-standalone-build.el")
        for rel in paths:
            yield rel, (ROOT / rel).read_text(errors="replace")
        return
    names = subprocess.run(["git", "-C", str(ROOT), "ls-tree", "--name-only", rev, "lisp/"],
                           check=True, capture_output=True, text=True).stdout.split()
    paths = sorted(n for n in names if re.match(r"lisp/nelisp-cc-.*\.el$", n))
    paths.append("scripts/nelisp-standalone-build.el")
    for rel in paths:
        text = subprocess.run(["git", "-C", str(ROOT), "show", f"{rev}:{rel}"],
                              check=True, capture_output=True).stdout.decode(errors="replace")
        yield rel, text


def subsystem(caller):
    for name, prefixes in SUBSYSTEMS:
        if caller.startswith(prefixes):
            return name
    return "other"


def census_static(rev=None):
    calls = collections.Counter()
    callers = collections.defaultdict(set)
    for rel, text in source_files(rev):
        if Path(rel).name.startswith("nelisp-cc-mirror-"):
            continue
        defs = [(m.start(), m.group(1)) for m in DEFUN.finditer(text)]
        starts = [d[0] for d in defs]
        for m in MIRROR_API.finditer(text):
            i = bisect.bisect_right(starts, m.start()) - 1
            if i < 0:
                continue          # a definition's own docstring/commentary
            caller = defs[i][1]
            if caller == m.group(1):
                continue
            group = subsystem(caller)
            calls[group] += 1
            callers[group].add(caller)
    rows = []
    for name, _ in SUBSYSTEMS + [("other", ())]:
        rows.append((name, len(callers[name]), calls[name]))
    return rows


def census_table(rows):
    lines = ["| subsystem | functions | call sites |", "|-+-+-|"]
    for name, nfun, ncall in rows:
        lines.append(f"| {name} | {nfun} | {ncall} |")
    lines.append(f"| total | {sum(r[1] for r in rows)} | {sum(r[2] for r in rows)} |")
    return lines


def cmd_census_static(args):
    rows = census_static(args[0] if args else None)
    print("\n".join(census_table(rows)))


# ---------------------------------------------------------------- S0.1 plan
GNU_REFS = re.compile(r"\b(lisp\.h|data\.c|eval\.c|lread\.c|alloc\.c|fns\.c):(\d+)\b")
REQUIRED_OPS = ["symbol-value", "set", "setq", "let", "let*", "defvar", "defconst",
                "defvaralias", "indirect-variable", "makunbound", "boundp",
                "default-value", "fboundp", "fset", "symbol-function", "fmakunbound",
                "intern", "intern-soft", "unintern", "obarray-make", "mapatoms",
                "symbol-plist", "get", "put", "add-variable-watcher"]


def doc_table(text, heading):
    """The rows of the first org table after HEADING, as lists of cells."""
    at = text.find(heading)
    need(at >= 0, f"Doc 226 lacks the heading {heading!r}")
    rows = []
    started = False
    for line in text[at:].splitlines()[1:]:
        if line.startswith("|"):
            started = True
            if not re.match(r"^\|[-+]+", line):
                rows.append([c.strip() for c in line.strip().strip("|").split("|")])
        elif started:
            break
    return rows


def cmd_plan(_args):
    need(DOC.is_file(), "missing " + str(DOC.relative_to(ROOT)))
    text = DOC.read_text()
    # Every GNU reference names an existing line of the 31.1 sources.
    refs = GNU_REFS.findall(text)
    files = {f for f, _ in refs}
    need({"lisp.h", "data.c", "eval.c", "lread.c"} <= files,
         "Doc 226 must cite lisp.h, data.c, eval.c and lread.c with line numbers")
    for f, line in refs:
        n = len((GNU_SRC / f).read_text(errors="replace").splitlines())
        need(1 <= int(line) <= n, f"{f}:{line} is outside the GNU 31.1 source")
    # The operation map covers every operation of the S1.3 fixture.
    ops = doc_table(text, "* Operation map")
    named = {row[0].strip("=~") for row in ops[1:] if row}
    missing = [op for op in REQUIRED_OPS if op not in named]
    need(not missing, "operation map lacks " + ", ".join(missing))
    for row in ops[1:]:
        need(len(row) >= 3 and GNU_REFS.search(row[1]),
             f"operation {row[0]} has no GNU file:line reference")
    # The census table equals the census recomputed at the recorded revision.
    expected = census_table(census_static(BEFORE_REV))
    got = doc_table(text, "* Mirror-table census")
    want = [[c.strip() for c in line.strip("|").split("|")]
            for line in expected if not line.startswith("|-")]
    need(got == want, "census table differs from census-static " + BEFORE_REV)
    need("* LOCALIZED interop" in text and "buffer-native" in text,
         "missing the LOCALIZED interop plan with buffer-native")
    print(f"PLAN PASS refs={len(refs)} ops={len(named)} census-rows={len(want) - 1}")


def not_yet(name):
    def run(_args):
        need(False, f"{name}: implementation or acceptance evidence outstanding")
    return run


COMMANDS = {
    "plan": cmd_plan,
    "census-static": cmd_census_static,
    "layout": not_yet("S1.1"),
    "census": not_yet("S1.2"),
    "semantics": not_yet("S1.3"),
    "speed": not_yet("S2"),
    "final": not_yet("S3.1"),
}


def main():
    if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
        print(__doc__)
        sys.exit(2)
    COMMANDS[sys.argv[1]](sys.argv[2:])


if __name__ == "__main__":
    main()
