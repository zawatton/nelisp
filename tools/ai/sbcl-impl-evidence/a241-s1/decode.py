#!/usr/bin/env python3
"""Doc 241 S1: expand a version-3 (sparse intern) FLAT cold image into the
equivalent version-2 dense byte layout (trailer version 2).
Usage: decode.py IMAGE [OUT]   (OUT defaults to stdout)"""
import struct, sys
SENT = 1397773107
def decode(b):
    magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
    if magic != 1179407692: raise SystemExit("not a FLAT cold image")
    c0 = 64 + tlen * 8; ia = c0 + ctotal
    sent, nrec = struct.unpack_from('<2Q', b, ia)
    if sent != SENT: raise SystemExit("intern section is not version 3 (sentinel %d)" % sent)
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
    if struct.unpack_from('<Q', trl, 8)[0] != 3: raise SystemExit("trailer version is not 3")
    struct.pack_into('<Q', trl, 8, 2)
    return bytes(b[:ia]) + bytes(tab) + bytes(names) + bytes(trl)
if __name__ == '__main__':
    out = decode(open(sys.argv[1], 'rb').read())
    if len(sys.argv) > 2: open(sys.argv[2], 'wb').write(out)
    else: sys.stdout.buffer.write(out)
