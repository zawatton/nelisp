#!/usr/bin/env python3
"""Compare the unit-cache files of two builds (or two directories) unit by unit.

usage: aotscale-unitcmp.py DIR_A DIR_B
Units are matched by name (the cache key part of the file name is ignored: it
covers the toolchain digest, which differs whenever the compiler changes).
Each pair is classified as
  same      byte-identical file (code, relocations, symbols, stack maps);
  ctxname   identical once the 64-hex-digit context digests in the names of
            specialized helper clones (`F--nelisp-context-<sha256>-N') are
            masked: code bytes and everything else equal;
  differ    anything else.
Prints one line per non-identical unit and a summary line; exit 0 iff no
`differ' and at least one unit compared."""
import os, re, sys

def units(d):
    out = {}
    for root, _, files in os.walk(d):
        for f in files:
            if f.endswith('.unit'):
                m = re.match(r'(.*?\.o)(\.[0-9a-f]+)?\.unit$', f)
                name = m.group(1) if m else f
                out[(re.sub(r'-[A-Za-z0-9]{6}$', '', os.path.relpath(root, d)), name)] = os.path.join(root, f)
    return out

CTX = re.compile(rb'--nelisp-context-[0-9a-f]{64}-')
a, b = units(sys.argv[1]), units(sys.argv[2])
counts = {'same': 0, 'ctxname': 0, 'differ': 0}
for k in sorted(set(a) & set(b)):
    x, y = open(a[k], 'rb').read(), open(b[k], 'rb').read()
    if x == y:
        c = 'same'
    elif CTX.sub(b'--nelisp-context-X-', x) == CTX.sub(b'--nelisp-context-X-', y):
        c = 'ctxname'
    else:
        c = 'differ'
    counts[c] += 1
    if c != 'same':
        print(c, '/'.join(k))
only_a = sorted(set(a) - set(b)); only_b = sorted(set(b) - set(a))
for k in only_a: print('only-A', '/'.join(k))
for k in only_b: print('only-B', '/'.join(k))
n = sum(counts.values())
print('UNITCMP compared=%d same=%d ctxname=%d differ=%d onlyA=%d onlyB=%d'
      % (n, counts['same'], counts['ctxname'], counts['differ'], len(only_a), len(only_b)))
sys.exit(0 if n and counts['differ'] == 0 else 1)
