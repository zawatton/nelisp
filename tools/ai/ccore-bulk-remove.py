#!/usr/bin/env python3
"""Drop implemented primitives from the bulk stub list.

Usage: ccore-bulk-remove.py [--dry-run] UNIT_FILE...

emacs-stub-bulk.el installs nil-returning stubs for the names in its
`--stub-defuns--' list while emacs-stub.el loads, which is before the C-core
units.  A unit's `(unless (fboundp 'NAME) (defun NAME ...))' therefore never
installs for a name that is still in that list.  For every NAME guarded that
way in the given unit files, this removes the exact token NAME from the
`--stub-defuns--' list, and nothing else in the file.  One line per name is
printed: `REMOVED NAME' or `ABSENT NAME' (not in the list).
"""
import re
import sys
from pathlib import Path

LIBRARY = Path(__file__).resolve().parents[2]
BULK = LIBRARY / "packages/nelisp-emacs-foundation/src/emacs-stub-bulk.el"


def guarded_names(path):
    text = Path(path).read_text(encoding="utf-8")
    return re.findall(r"\(unless\s+\(fboundp\s+'([^\s()]+)\)", text)


def main():
    args = sys.argv[1:]
    dry_run = bool(args) and args[0] == "--dry-run"
    if dry_run:
        args = args[1:]
    if not args:
        sys.exit(__doc__.split("\n")[2])
    text = BULK.read_text(encoding="utf-8")
    start = text.index("(let ((--stub-defuns--")
    open_paren = text.index("'(", start) + 1
    depth, end = 0, open_paren
    while True:
        char = text[end]
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
            if depth == 0:
                break
        end += 1
    body = text[open_paren + 1:end]
    for unit in args:
        for name in guarded_names(unit):
            pattern = re.compile(r"(?<![^\s])" + re.escape(name) + r"(?![^\s])")
            if pattern.search(body):
                # Remove the token and one adjacent space, keeping line layout.
                body = re.sub(r"(?<![^\s])" + re.escape(name) + r"(?![^\s]) ?", "", body, count=1)
                print("REMOVED %s" % name)
            else:
                print("ABSENT %s" % name)
    if not dry_run:
        BULK.write_text(text[:open_paren + 1] + body + text[end:], encoding="utf-8")


if __name__ == "__main__":
    main()
