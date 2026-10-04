#!/usr/bin/env python3
"""Classify where C-core primitive names are defined, for fix-lane planning.

Usage: ccore-owner.py NAMES_FILE [--runtime-root DIR]

For every name (one per line) print a TSV row: NAME, KIND, FILES.
KIND is the bare standalone's view before any bundle loads:
  builtin   native entry in the runtime binary
  prelude   Lisp closure baked into the runtime (core prelude or lisp/)
  package   not bound in the bare runtime; defined by a library package
  unbound   not bound in the bare runtime and no package definition found
FILES lists syntactic `(defun NAME' / `(defalias 'NAME' sites, relative to the
library root or prefixed with `runtime:'.  The scan is textual: generated or
macro-produced definitions are not found, so an empty FILES is not proof of
absence.

Environment: NELISP_BIN (standalone binary).  The runtime root defaults to
the directory two levels above NELISP_BIN (.../target/nelisp -> ...).
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def bare_kinds(binary, names):
    """Return {name: kind} from one bare standalone run."""
    forms = ["(progn"]
    for name in names:
        forms.append(
            "(princ (format \"%%s\\t%%s\\n\" '%s (condition-case nil "
            "(if (fboundp '%s) (let ((f (symbol-function '%s))) "
            "(cond ((symbolp f) 'alias) ((consp f) (car f)) (t (type-of f)))) 'void) "
            "(error 'err))))" % (name, name, name))
    forms.append(")")
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as stream:
        stream.write("\n".join(forms))
        path = stream.name
    try:
        out = subprocess.run([binary, "--load", path, "--eval", "nil"],
                             capture_output=True, timeout=120, text=True)
    finally:
        os.unlink(path)
    kinds = {}
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) == 2:
            kinds[parts[0]] = parts[1]
    return kinds


def bundle_views(binary, bundle, cwd, names):
    """Return {name: view} comparing definitions before and after BUNDLE loads.

    view is "same" (definition object unchanged), "overridden", "bound"
    (unbound before, bound after) or "unbound" (still unbound).
    """
    forms = ["(setq ccore-owner--before (list"]
    for name in names:
        forms.append("(cons '%s (and (fboundp '%s) (symbol-function '%s)))"
                     % (name, name, name))
    forms.append("))")
    forms.append('(load "%s" nil t)' % bundle)
    forms.append(
        '(dolist (cell ccore-owner--before) (princ (format "V\\t%s\\t%s\\n" (car cell) '
        "(cond ((not (fboundp (car cell))) 'unbound) ((null (cdr cell)) 'bound) "
        "((eq (cdr cell) (symbol-function (car cell))) 'same) (t 'overridden)))))")
    with tempfile.NamedTemporaryFile("w", suffix=".el", delete=False) as stream:
        stream.write("\n".join(forms))
        path = stream.name
    try:
        out = subprocess.run([binary, "--load", path, "--eval", "nil"], cwd=cwd,
                             capture_output=True, timeout=300, text=True)
    finally:
        os.unlink(path)
    views = {}
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) == 3 and parts[0] == "V":
            views[parts[1]] = parts[2]
    return views


def definition_sites(roots, names):
    """Return {name: [site...]} from a textual scan of *.el under ROOTS."""
    wanted = set(names)
    pattern = re.compile(
        r"^\s*\((?:defun|defmacro|defsubst|cl-defun)\s+(\S+)|"
        r"^\s*\(defalias\s+'(\S+)")
    sites = {name: [] for name in names}
    for prefix, root in roots:
        for path in sorted(Path(root).rglob("*.el")):
            relative = os.path.relpath(path, root)
            if relative.startswith(("build/", "target/")):
                continue
            try:
                text = path.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue
            for number, line in enumerate(text.splitlines(), 1):
                match = pattern.match(line)
                if not match:
                    continue
                name = (match.group(1) or match.group(2)).rstrip(")")
                if name in wanted:
                    rel = os.path.relpath(path, root)
                    sites[name].append("%s%s:%d" % (prefix, rel, number))
    return sites


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("names_file")
    parser.add_argument("--runtime-root")
    parser.add_argument("--bundle",
                        help="also print VIEW after this bundle loads (about 45 s)")
    args = parser.parse_args()
    binary = os.environ.get("NELISP_BIN")
    if not binary or not os.access(binary, os.X_OK):
        sys.exit("NELISP_BIN missing or not executable")
    library = Path(__file__).resolve().parents[2]
    runtime = Path(args.runtime_root or Path(binary).resolve().parents[1])
    names = [n for n in Path(args.names_file).read_text().split() if n]
    kinds = bare_kinds(binary, names)
    sites = definition_sites(
        [("", library / "packages"),
         ("runtime:", runtime / "scripts"), ("runtime:", runtime / "lisp")], names)
    views = (bundle_views(binary, os.path.abspath(args.bundle), str(library), names)
             if args.bundle else {})
    for name in names:
        kind = kinds.get(name, "err")
        package_sites = [s for s in sites[name] if not s.startswith("runtime:")]
        if kind == "void":
            label = "package" if package_sites else "unbound"
        elif kind == "builtin":
            label = "builtin"
        elif kind in ("closure", "lambda", "alias", "macro", "byte-code-function",
                      "compiled-function", "interpreted-function"):
            label = "prelude"
        else:
            label = kind
        row = [name, label, " ".join(sites[name])]
        if args.bundle:
            row.append(views.get(name, "err"))
        print("\t".join(row))


if __name__ == "__main__":
    main()
