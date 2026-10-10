"""Vendor genuine GNU Emacs 31.1 libraries into vendor/emacs-lisp-api.

Usage: tools/gnu-vendor-add.py LIB...   (library names, e.g. reveal tramp)
GNU_LISP_DIR selects the installed GNU lisp tree (default 31.1 under /usr/local).
Decompresses the installed .el.gz verbatim, records ORIGIN and SHA256SUMS.
"""
import gzip, hashlib, os, sys
from pathlib import Path
GNU = Path(os.environ.get('GNU_LISP_DIR', '/usr/local/share/emacs/31.1/lisp'))
ROOT = Path(__file__).resolve().parents[1] / 'vendor'
origin = ROOT / 'ORIGIN'; sums = ROOT / 'SHA256SUMS'
added, skipped = [], []
for lib in sys.argv[1:]:
    hits = sorted(GNU.rglob(lib.split('/')[-1] + '.el.gz')) + sorted(GNU.rglob(lib.split('/')[-1] + '.el'))
    hits = [h for h in hits if '/obsolete/' not in str(h) or 'obsolete' in lib]
    if not hits:
        skipped.append((lib, 'not in GNU')); continue
    src = hits[0]; rel = src.relative_to(GNU).as_posix().removesuffix('.gz')
    for tree in ('emacs-lisp', 'emacs-lisp-api'):
        if (ROOT / tree / rel).exists():
            skipped.append((lib, f'already {tree}/{rel}')); break
    else:
        data = gzip.decompress(src.read_bytes()) if src.suffix == '.gz' else src.read_bytes()
        dest = ROOT / 'emacs-lisp-api' / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(data)
        digest = hashlib.sha256(data).hexdigest()
        with origin.open('a') as f:
            f.write(f'emacs-lisp-api/{rel}\tGNU Emacs 31.1\t{GNU}/{rel}\tsha256={digest}\n')
        with sums.open('a') as f:
            f.write(f'{digest}  vendor/emacs-lisp-api/{rel}\n')
        added.append(rel)
print('ADDED', ' '.join(added)); print('SKIPPED', skipped)
