#!/usr/bin/env python3
"""Doc 241 S3.1 model: measure reloc-table encodings on a v3 cold image.
Usage: model.py IMAGE.cold"""
import struct, sys, collections
b = open(sys.argv[1], 'rb').read()
magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
ents = struct.unpack_from('<%dQ' % tlen, b, 64)
fs = [e & ~7 for e in ents]; tags = [e & 7 for e in ents]
print("tlen", tlen, "dense bytes", tlen * 8, "ctotal", ctotal)
print("tags", dict(collections.Counter(tags)))
print("distinct f", len(set(fs)), "dups", tlen - len(set(fs)))
print("misaligned f", sum(1 for f in fs if f & 7), "max f+8 <= ctotal", max(fs) + 8 <= ctotal)
print("already sorted", ents == tuple(sorted(ents)))
# (a) sort + delta varint with 2-bit tag folded in
def varint_len(v):
    n = 1
    while v >= 128: v >>= 7; n += 1
    return n
srt = sorted(zip(fs, tags)); prev = 0; vb = 0; hist = collections.Counter()
for f, t in srt:
    d = (f >> 3) - prev; prev = f >> 3
    n = varint_len((d << 2) | (t >> 1)); vb += n; hist[n] += 1
print("(a) delta varint bytes", vb, "len hist", dict(hist), "bytes/entry %.3f" % (vb / tlen))
# (b) bitmap over words + 2-bit tag per entry
nwords = max(fs) // 8 + 1
bm = (nwords + 7) // 8; tg = (tlen * 2 + 7) // 8
print("(b) bitmap", bm, "+ tags", tg, "=", bm + tg, "; bits scanned", nwords)
# (c) 4-byte entries (f/8 << 2 | tag/2 fits 32 bits)
print("(c) 4-byte entries", tlen * 4)
