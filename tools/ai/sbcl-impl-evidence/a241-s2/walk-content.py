#!/usr/bin/env python3
"""Doc 241 S2: walk the content section of a FLAT cold image block by block
(header word: low 32 bits & ~7 = block total bytes, low 3 bits = mark, bits 32+ = type info),
and report the blocks that contain content zero runs >= 64 KiB.
Usage: walk-content.py IMAGE.cold [--total]   (--total: print only the zero-run total)"""
import re, struct, sys
b = open(sys.argv[1], 'rb').read()
magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
assert magic == 1179407692, "not FLAT"
c0 = 64 + tlen * 8
c = b[c0:c0 + ctotal]
runs = [(m.start(), m.end() - m.start()) for m in re.finditer(rb'\x00{65536,}', c)]
if '--total' in sys.argv:
    print(sum(r[1] for r in runs)); sys.exit(0)
print(f"ctotal {ctotal}; zero runs >= 64 KiB: {runs}; total {sum(r[1] for r in runs)}")
off = 0; n = 0; blocks = []
while off < ctotal:
    h = struct.unpack_from('<Q', c, off)[0]
    bt = h & 0xFFFFFFF8
    if bt < 8 or off + bt > ctotal:
        print(f"walk broke at {off} header {h:#x}"); break
    blocks.append((off, bt, h)); off += bt; n += 1
print(f"walked {n} blocks to offset {off} (ctotal {ctotal})")
for ro, rl in runs:
    for i, (o, bt, h) in enumerate(blocks):
        if o <= ro < o + bt:
            body = c[o + 8:o + bt]
            print(f"run@{ro} len {rl}: block #{i} at {o} header {h:#x} total {bt} mark/low3 {h&7} hi32 {h>>32:#x}; "
                  f"zero bytes in block {body.count(0)}/{len(body)}; run inside block: {min(o+bt, ro+rl)-max(o, ro)}")
            print("  first words:", [hex(x) for x in struct.unpack_from('<8Q', c, o)])
            print("  prev block:", [hex(x) for x in struct.unpack_from('<4Q', c, blocks[i-1][0])], "size", blocks[i-1][1])
            break
    else:
        print(f"run@{ro}: not inside any block")

# ---- string area: [filler][hdr-area: 8 + 32n][data-area: 8 + sum][footer 32] at the end of content ----
fo = ctotal - 32
fsz, fmark, hoff, hend = struct.unpack_from('<4Q', c, fo)
n = (hend - hoff) // 32
print(f"string footer marker {fmark} hoff {hoff} n {n}")
recs = [struct.unpack_from('<4Q', c, hoff + 32 * i) for i in range(n)]   # size, size_byte, multibyte flag, data+8
def nb_of(r): return r[0] if r[1] == 0xFFFFFFFFFFFFFFFF else r[1]   # unibyte: size_byte = -1
by_ptr = sorted(range(n), key=lambda i: recs[i][3])
for ro, rl in runs:
    hit = [i for i in range(n) if recs[i][3] - 8 <= ro < recs[i][3] - 8 + ((recs[i][1] + 8 + 8) & ~7) + 8]
    # simpler: the string whose data (ptr..ptr+size_byte) overlaps the run
    hit = [i for i in range(n) if recs[i][3] < ro + rl and recs[i][3] + max(nb_of(recs[i]), 1) > ro]
    print(f"run@{ro} len {rl}: overlapping strings (index: size, size_byte, mb, data@):")
    for i in hit[:6]:
        s = recs[i]; d = c[s[3]:s[3] + nb_of(s)]
        print(f"  #{i}: {s[0]}, {s[1]}, {s[2]}, @{s[3]}; zero bytes {d.count(0)}/{len(d)}; head {d[:24].hex()} ... tail {d[-16:].hex()}")
