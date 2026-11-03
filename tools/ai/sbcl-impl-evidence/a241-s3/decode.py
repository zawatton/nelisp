#!/usr/bin/env python3
"""Doc 241 S3.1: expand a version-4 FLAT cold image into the version-2 dense
layout (dense relocation table, dense intern section, trailer version 2).
The reloc table comes out SORTED by entry: the dump order is not stored, so
equality with a dense dump is defined as "same multiset of entries".
Usage: decode.py IMAGE [OUT]       decode (OUT defaults to stdout)
       decode.py --check IMAGE     print layout sizes and assert the v4 layout"""
import struct, sys
SENT_INTERN = 1397773107
SENT_RELOC = 1380732980
def parse(b):
    magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
    if magic != 1179407692: raise SystemExit("not a FLAT cold image")
    sent, rsz = struct.unpack_from('<2Q', b, 64)
    if sent != SENT_RELOC: raise SystemExit("reloc section is not version 4 (sentinel %d)" % sent)
    stream = b[80:80 + rsz]
    if len(stream) != rsz: raise SystemExit("truncated reloc stream")
    ents, p, w = [], 0, 0
    while len(ents) < tlen:
        val, sh, nb = 0, 0, 0
        while True:
            if p >= rsz or nb >= 5: raise SystemExit("bad varint")
            x = stream[p]; p += 1; nb += 1
            val |= (x & 127) << sh; sh += 7
            if x < 128: break
        w += val >> 2
        f = w * 8
        if f + 8 > ctotal: raise SystemExit("reloc offset out of range")
        ents.append(f | ((val & 3) * 2))
    if p != rsz: raise SystemExit("trailing bytes in reloc stream")
    c0 = 80 + rsz; ia = c0 + ctotal
    return dict(ctotal=ctotal, isz=isz, tlen=tlen, rsz=rsz, ents=ents, c0=c0, ia=ia)
def decode(b):
    d = parse(b); ia, isz = d['ia'], d['isz']
    sent, nrec = struct.unpack_from('<2Q', b, ia)
    if sent != SENT_INTERN: raise SystemExit("intern section is not sparse (sentinel %d)" % sent)
    nslots = min(isz, 16 << 20) // 16
    nbytes = isz - nslots * 16
    tab = bytearray(nslots * 16)
    p = ia + 16
    for i in range(nrec):
        idx, w0, w1 = struct.unpack_from('<3Q', b, p + i * 24)
        if idx >= nslots: raise SystemExit("record index out of range")
        struct.pack_into('<2Q', tab, idx * 16, w0, w1)
    p += nrec * 24
    names = b[p:p + nbytes]
    if len(names) != nbytes: raise SystemExit("truncated name area")
    trl = bytearray(b[p + nbytes:])
    if len(trl) != 48: raise SystemExit("bad trailer length %d" % len(trl))
    if struct.unpack_from('<Q', trl, 8)[0] != 4: raise SystemExit("trailer version is not 4")
    struct.pack_into('<Q', trl, 8, 2)
    table = struct.pack('<%dQ' % d['tlen'], *d['ents'])
    return bytes(b[:64]) + table + bytes(b[d['c0']:ia]) + bytes(tab) + bytes(names) + bytes(trl)
if __name__ == '__main__':
    if sys.argv[1] == '--check':
        b = open(sys.argv[2], 'rb').read(); d = parse(b)
        sent, nrec = struct.unpack_from('<2Q', b, d['ia'])
        nslots = min(d['isz'], 16 << 20) // 16
        enc = 16 + nrec * 24 + (d['isz'] - nslots * 16)
        tv = struct.unpack_from('<Q', b, d['ia'] + enc + 8)[0]
        print(f"file {len(b)} reloc-section {16 + d['rsz']} entries {d['tlen']} intern {enc} trailer-version {tv}")
        assert sent == SENT_INTERN and tv == 4 and len(b) == d['ia'] + enc + 48, "layout mismatch"
        assert 16 + d['rsz'] <= 2190696, "reloc section > 2,190,696"
        assert enc <= 1000000, "intern section > 1,000,000"
        assert len(b) <= 24000000, "file > 24,000,000"
        print("v4 layout OK"); sys.exit(0)
    out = decode(open(sys.argv[1], 'rb').read())
    if len(sys.argv) > 2: open(sys.argv[2], 'wb').write(out)
    else: sys.stdout.buffer.write(out)
