#!/usr/bin/env python3
"""compare.py DECODED_V5 DENSE_V2: header, content, intern, trailer byte-for-byte
(except 64-byte windows after the env value "dense"); reloc table as a sorted multiset."""
import struct, sys
a = open(sys.argv[1], "rb").read(); b = open(sys.argv[2], "rb").read()
if len(a) != len(b): sys.exit(f"size differs: decoded {len(a)} dense {len(b)}")
tlen = struct.unpack_from('<Q', b, 24)[0]; t0, t1 = 64, 64 + tlen * 8
if a[:64] != b[:64]: sys.exit("header differs")
if sorted(struct.unpack_from('<%dQ' % tlen, a, t0)) != sorted(struct.unpack_from('<%dQ' % tlen, b, t0)):
    sys.exit("reloc multiset differs")
print(f"reloc multiset equal ({tlen} entries)")
diff = [i for i in range(t1, len(a)) if a[i] != b[i]]
occ = [i for i in range(len(b)) if b.startswith(b"dense", i)] if diff else []
bad = [i for i in diff if not any(o <= i < o + 64 for o in occ)]
print(f"differing bytes outside the table {len(diff)}, outside env-value windows {len(bad)}")
if bad or len(diff) > 64: sys.exit(f"unexplained differences at {bad[:10]}")
