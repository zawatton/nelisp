"""Report top-level (require 'X) deps of the given vendored files that no local source provides.

Usage: tools/gnu-vendor-closure.py FILE...   (prints NAME gnu|NOT-IN-GNU USERS)
Feed the `gnu' names to tools/gnu-vendor-add.py until none remain."""
import os, re, sys
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
GNU = Path(os.environ.get('GNU_LISP_DIR', '/usr/local/share/emacs/31.1/lisp'))
have = set()
for base in [ROOT/'vendor', ROOT/'packages', *[Path(p) for p in os.environ.get('NELISP_RUNTIME_LISP_DIRS', '').split(':') if p]]:
    for p in base.rglob('*.el'):
        have.add(p.stem)
targets = [Path(a) for a in sys.argv[1:]]
missing = {}
for t in targets:
    text = t.read_text(errors='replace')
    for m in re.finditer(r"^\(require '([A-Za-z0-9+-]+)\)", text, re.M):
        name = m.group(1)
        if name not in have:
            missing.setdefault(name, []).append(t.name)
for k, v in sorted(missing.items()):
    gnu = bool(list(GNU.rglob(k + '.el.gz')))
    print(k, 'gnu' if gnu else 'NOT-IN-GNU', ','.join(v))
