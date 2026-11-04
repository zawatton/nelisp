#!/usr/bin/env python3
"""dumpcmp.py A B: two dense v2 dumps from processes whose intern bases may differ.
Normalizes header word 56 (intern base) and intern word1 of occupied slots by
subtracting each file's own intern base, then compares byte-for-byte except
64-byte windows after an occurrence of NELISP_COLD_FORCE_RELOCATE=."""
import struct, sys
KEY = b"NELISP_COLD_FORCE_RELOCATE="
def norm(b):
    magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
    if magic != 1179407692: sys.exit("not a FLAT dump")
    ib = struct.unpack_from('<Q', b, 56)[0]
    b = bytearray(b); struct.pack_into('<Q', b, 56, 0)
    t = 64 + tlen * 8 + ctotal; n = min(isz, 16 << 20) // 16
    for i in range(n):
        w0, w1 = struct.unpack_from('<2Q', b, t + i * 16)
        if w0: struct.pack_into('<Q', b, t + i * 16 + 8, (w1 - ib) & (2**64 - 1))
    return bytes(b), ib
(a, ia), (b, ib) = (norm(open(p, 'rb').read()) for p in sys.argv[1:3])
if len(a) != len(b): sys.exit(f"size differs: {len(a)} vs {len(b)}")
diff = [i for i in range(len(a)) if a[i] != b[i]]
occ = []
for x in (a, b):
    i = x.find(KEY)
    while i >= 0: occ.append(i); i = x.find(KEY, i + 1)
bad = [i for i in diff if not any(o <= i < o + len(KEY) + 64 for o in occ)]
print(f"intern bases {ia:#x} / {ib:#x}; differing bytes {len(diff)}, outside env-value windows {len(bad)}")
if bad or len(diff) > 128: sys.exit(f"unexplained differences at {bad[:10]}")
