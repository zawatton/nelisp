#!/usr/bin/env python3
"""Measure which bundle file defines each name, by tracing a real bundle load.

Usage: ccore-definers.py BUNDLE NAMES_FILE

The bundle is copied with one probe after every `;;; <<< FILE' marker (package and vendor sections).
Each probe compares the function cell of every watched name with its previous
value, so the output is the measured sequence of definers, not a textual
guess.  One TSV row per name is printed:

  NAME  BARE  LIVE  STUB  CHAIN

BARE   `bound' or `void' before the bundle starts (the bare runtime's view)
LIVE   the last file whose load changed the function cell, `-' when the
       bundle never changes it (the bare runtime definition stays live), or
       `unbound' when the name is still unbound after the bundle
STUB   `bulk' when the live definition carries the `emacs-stub-bulk'
       property (installed by the bulk stub list), else `-'
CHAIN  every definer in load order, `>'-separated

A file that loads another one (emacs-stub.el loads emacs-stub-bulk.el from
disk) is reported under the outer file; STUB tells the two apart.

Environment: NELISP_BIN (standalone binary).  Runs from the library root so
the bundle resolves its vendor paths.  Takes about 50 seconds.
"""
import os
import subprocess
import sys
import tempfile
from pathlib import Path

PRELUDE = r'''(setq ccore-def--names '(%s))
(setq ccore-def--last
      (mapcar (lambda (n) (cons n (and (fboundp n) (symbol-function n)))) ccore-def--names))
(dolist (cell ccore-def--last)
  (princ (format "B\t%%s\t%%s\n" (car cell) (if (cdr cell) 'bound 'void))))
(defun ccore-def--check (file)
  (dolist (cell ccore-def--last)
    (let ((cur (and (fboundp (car cell)) (symbol-function (car cell)))))
      (unless (eq cur (cdr cell))
        (setcdr cell cur)
        (princ (format "D\t%%s\t%%s\t%%s\n" (car cell) file
                       (if (get (car cell) 'emacs-stub-bulk) 'bulk '-)))))))
'''

FINAL = r'''(dolist (cell ccore-def--last)
  (princ (format "F\t%s\t%s\t%s\n" (car cell)
                 (if (fboundp (car cell)) 'bound 'unbound)
                 (if (get (car cell) 'emacs-stub-bulk) 'bulk '-))))
'''


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__.split("\n")[2])
    bundle, names_file = sys.argv[1], sys.argv[2]
    binary = os.environ.get("NELISP_BIN")
    if not binary or not os.access(binary, os.X_OK):
        sys.exit("NELISP_BIN missing or not executable")
    library = Path(__file__).resolve().parents[2]
    names = [n for n in Path(names_file).read_text().split() if n]
    lines = [PRELUDE % " ".join(names)]
    for line in Path(bundle).read_text(errors="surrogateescape").splitlines():
        lines.append(line)
        if line.startswith(";;; <<< "):
            lines.append('(ccore-def--check "%s")' % line[len(";;; <<< "):].strip().replace("src/", "", 1))
    lines.append('(ccore-def--check "bundle-tail")')
    lines.append(FINAL)
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
    bare, chain, stub_at, final = {}, {}, {}, {}
    for line in out.stdout.splitlines():
        parts = line.split("\t")
        if parts[0] == "B" and len(parts) == 3:
            bare[parts[1]] = parts[2]
        elif parts[0] == "D" and len(parts) == 4:
            chain.setdefault(parts[1], []).append(parts[2])
            stub_at[parts[1]] = parts[3]
        elif parts[0] == "F" and len(parts) == 4:
            final[parts[1]] = (parts[2], parts[3])
    if len(final) != len(set(names)):
        sys.exit("trace incomplete: %d of %d names reported; stderr: %s"
                 % (len(final), len(set(names)), out.stderr[-300:]))
    for name in names:
        definers = chain.get(name, [])
        bound, stub = final[name]
        live = "unbound" if bound == "unbound" else (definers[-1] if definers else "-")
        print("\t".join([name, bare.get(name, "?"), live, stub, ">".join(definers) or "-"]))


if __name__ == "__main__":
    main()
