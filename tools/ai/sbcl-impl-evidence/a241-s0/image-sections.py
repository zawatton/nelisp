#!/usr/bin/env python3
"""Doc 241 S0: section sizes and zero density of a NeLisp cold image (FLAT format).
Usage: image-sections.py IMAGE.cold"""
import re, struct, sys
b = open(sys.argv[1], 'rb').read(); n = len(b)
magic, ctotal, isz, tlen, _g, _f, _u, _ib = struct.unpack_from('<8Q', b, 0)
if magic != 1179407692:
    sys.exit(f"not a FLAT cold image (magic {magic})")
c0 = 64 + tlen * 8
sec = {'header': (0, 64), 'reloc-table': (64, c0), 'content': (c0, c0 + ctotal),
       'intern': (c0 + ctotal, c0 + ctotal + isz), 'trailer': (c0 + ctotal + isz, n)}
print(f"file {n} bytes; tlen {tlen}")
for k, (a, e) in sec.items():
    s = b[a:e]
    print(f"{k:12} {e-a:>10} B {(e-a)/n:6.1%}  zero bytes {s.count(0)/max(1,len(s)):4.0%}")
a, e = sec['intern']; s = b[a:e]
nz = [i for i in range(0, len(s) - len(s) % 8, 8) if s[i:i+8] != bytes(8)]
tail = max((i for i in range(0, len(nz) - 1) if nz[i+1] - nz[i] > 8 and nz[i] < (16 << 20)), default=0)
print(f"intern nonzero words {len(nz)} ({len(nz)*8} B); words below 16 MiB {sum(1 for x in nz if x < (16<<20))}, at/after 16 MiB {sum(1 for x in nz if x >= (16<<20))}")
a, e = sec['content']; runs = [(m.start(), m.end() - m.start()) for m in re.finditer(rb'\x00{65536,}', b[a:e])]
print(f"content zero runs >= 64 KiB: {len(runs)}, total {sum(r[1] for r in runs)} B, at content offsets {[r[0] for r in runs]}")
