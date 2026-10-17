#!/usr/bin/env python3
"""Write a deterministic Inkscape-style SVG for the autocad perf benches.

The real inputs (single-line diagrams, 0.7-9 MB) are private.  This keeps
their shape: one XML attribute per line, `<path>' elements whose d=""
holds absolute coordinates with 13-15 significant digits, and `<text>'
elements with a `<tspan>'.  The defaults give about 710 KB and 17k lines, close to
like the 001 drawing in handoff/nelisp-autocad-perf_dev-nelisp_2026-10-17.md.

Usage: gen-inkscape-svg.py OUT [PATHS TEXTS]
Prints "paths=N texts=M numbers=K bytes=B lines=L" for the benches to check.
"""
import random
import sys

out = sys.argv[1]
paths = int(sys.argv[2]) if len(sys.argv) > 2 else 2000
texts = int(sys.argv[3]) if len(sys.argv) > 3 else 550
rng = random.Random(20261017)
numbers = 0
lines = []
lines += ['<?xml version="1.0" encoding="UTF-8" standalone="no"?>',
          '<svg',
          '   width="841mm"',
          '   height="594mm"',
          '   viewBox="0 0 841 594"',
          '   version="1.1"',
          '   xmlns="http://www.w3.org/2000/svg">',
          '  <g',
          '     id="layer1">']


def num():
    return f"{rng.uniform(0, 1200):.6f}".rstrip("0")


for i in range(paths):
    pts = rng.randint(2, 4)
    d = "M " + num() + "," + num()
    for _ in range(pts):
        d += " L " + num() + "," + num()
    numbers += 2 * (pts + 1)
    lines += ['    <path',
              '       style="fill:none;stroke:#000000;stroke-width:0.26458332"',
              f'       d="{d}"',
              f'       id="path{1000 + i}"',
              '       inkscape:connector-curvature="0"',
              '       sodipodi:nodetypes="cc" />']
for i in range(texts):
    numbers += 2
    lines += ['    <text',
              '       xml:space="preserve"',
              '       style="font-size:3.52777px;font-family:sans-serif"',
              f'       x="{num()}"',
              f'       y="{num()}"',
              '       inkscape:label="label"',
              f'       id="text{2000 + i}"><tspan',
              f'         sodipodi:role="line"',
              f'         id="tspan{2000 + i}">CB-{i:04d} 3φ 6.6kV</tspan></text>']
lines += ['  </g>', '</svg>', '']
data = "\n".join(lines)
with open(out, "w", encoding="utf-8") as f:
    f.write(data)
print(f"paths={paths} texts={texts} numbers={numbers} "
      f"bytes={len(data.encode('utf-8'))} lines={data.count(chr(10))}")
