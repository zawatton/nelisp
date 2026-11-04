#!/usr/bin/env python3
"""Doc 242 S2: expand a version-5 cold image into the version-2 dense layout.
v5 file: 5120-byte header block (64-byte header, sub-header at 64: "SUB5" magic,
recorded chunk-0 base, recorded intern base, content offset 5120), content
(pre-biased to absolute addresses at the recorded bases), sparse intern section
(word1 pre-biased), trailer (version 5), packed relocation section.
Pointer fields are converted back to image offsets with the relocation entries.
The reloc table comes out SORTED (dump order is not stored).
Usage: decode.py IMAGE [OUT]   |   decode.py --selftest"""
import struct, sys
SENT_INTERN = 1397773107
SENT_RELOC = 1380732980
MAGIC5 = 0x35425553
HDR_BLOCK = 5120
class Bad(Exception): pass
def parse(b):
    if len(b) < HDR_BLOCK + 48: raise Bad("file too short")
    magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
    if magic != 1179407692: raise Bad("not a FLAT cold image")
    sub, fixb, fixi, fco = struct.unpack_from('<4Q', b, 64)
    if sub != MAGIC5: raise Bad("bad v5 sub-header magic %#x" % sub)
    if fco != HDR_BLOCK: raise Bad("bad content offset %d" % fco)
    if fixb & 4095 or fixb == 0: raise Bad("bad recorded chunk-0 base")
    ia = fco + ctotal
    if ia + 16 > len(b): raise Bad("truncated content")
    sent, nrec = struct.unpack_from('<2Q', b, ia)
    if sent != SENT_INTERN: raise Bad("intern section is not sparse (sentinel %d)" % sent)
    nslots = min(isz, 16 << 20) // 16
    nbytes = isz - nslots * 16
    enc = 16 + nrec * 24 + nbytes
    ta = ia + enc; ra = ta + 48
    if ra + 16 > len(b): raise Bad("truncated intern section / trailer")
    if struct.unpack_from('<Q', b, ta + 8)[0] != 5: raise Bad("trailer version is not 5")
    rsent, rsz = struct.unpack_from('<2Q', b, ra)
    if rsent != SENT_RELOC: raise Bad("reloc section sentinel %d" % rsent)
    stream = b[ra + 16:ra + 16 + rsz]
    if len(stream) != rsz or ra + 16 + rsz != len(b): raise Bad("truncated or oversized reloc stream")
    ents, p, w = [], 0, 0
    while len(ents) < tlen:
        val, sh, nb = 0, 0, 0
        while True:
            if p >= rsz or nb >= 5: raise Bad("bad varint")
            x = stream[p]; p += 1; nb += 1
            val |= (x & 127) << sh; sh += 7
            if x < 128: break
        w += val >> 2
        f = w * 8
        if f + 8 > ctotal: raise Bad("reloc offset out of range")
        ents.append(f | ((val & 3) * 2))
    if p != rsz: raise Bad("trailing bytes in reloc stream")
    return dict(ctotal=ctotal, isz=isz, tlen=tlen, ents=ents, fixb=fixb, fixi=fixi,
                fco=fco, ia=ia, nrec=nrec, nslots=nslots, nbytes=nbytes, ta=ta)
def decode(b):
    d = parse(b); ct = d['ctotal']
    content = bytearray(b[d['fco']:d['fco'] + ct])
    fixds = d['fixb'] + 1024
    for e in d['ents']:
        ptag = e & 7; f = e - ptag
        o, = struct.unpack_from('<Q', content, f)
        raw = o - ptag
        if fixds <= raw < fixds + ct: o -= fixds
        elif raw >= d['fixi']: o = o - d['fixi'] + ct
        else: raise Bad("pointer %#x at %d outside both recorded regions" % (o, f))
        struct.pack_into('<Q', content, f, o)
    ia = d['ia']; tab = bytearray(d['nslots'] * 16); p = ia + 16
    for i in range(d['nrec']):
        idx, w0, w1 = struct.unpack_from('<3Q', b, p + i * 24)
        if idx >= d['nslots']: raise Bad("record index out of range")
        struct.pack_into('<2Q', tab, idx * 16, w0, w1)
    p += d['nrec'] * 24
    names = b[p:p + d['nbytes']]
    trl = bytearray(b[d['ta']:d['ta'] + 48]); struct.pack_into('<Q', trl, 8, 2)
    table = struct.pack('<%dQ' % d['tlen'], *sorted(d['ents']))
    return bytes(b[:64]) + table + bytes(content) + bytes(tab) + bytes(names) + bytes(trl)
def synth():
    """Tiny v5 image: ctotal 64, one chunk-0 pointer (tag 0) and one intern pointer (tag 2)."""
    fixb, fixi, ct = 0x200000000000, 0x200040000000, 64
    fixds = fixb + 1024
    content = bytearray(ct)
    struct.pack_into('<Q', content, 8, fixds + 24)          # chunk-0 target offset 24
    struct.pack_into('<Q', content, 16, fixi + 32 + 2)      # intern target offset ct+32, tag 2
    isz = (16 << 20) + 8                                    # all slots + 8 name bytes
    nslots = 1 << 20
    recs = struct.pack('<3Q', 5, 7, fixi + 100)
    inter = struct.pack('<2Q', SENT_INTERN, 1) + recs + b'abcdefgh'
    trl = struct.pack('<6Q', 0, 5, 0, 0, 0, 0)
    ents = [8, 16 | 2]
    stream = bytes([(1 << 2) | 0, (1 << 2) | 1])            # words 1 then +1, ptag field = val&3
    reloc = struct.pack('<2Q', SENT_RELOC, len(stream)) + stream
    hdr = bytearray(HDR_BLOCK)
    struct.pack_into('<4Q', hdr, 0, 1179407692, ct, isz, len(ents))
    struct.pack_into('<Q', hdr, 56, fixi)
    struct.pack_into('<4Q', hdr, 64, MAGIC5, fixb, fixi, HDR_BLOCK)
    return bytes(hdr) + bytes(content) + inter + trl + reloc
def selftest():
    img = synth(); out = decode(img)
    ct = 64; t0 = 64 + 2 * 8
    assert struct.unpack_from('<2Q', out, 64) == (8, 18), "sorted table"
    assert struct.unpack_from('<Q', out, t0 + 8)[0] == 24, "chunk-0 pointer -> offset"
    assert struct.unpack_from('<Q', out, t0 + 16)[0] == ct + 32 + 2, "intern pointer -> offset"
    assert struct.unpack_from('<Q', out, t0 + ct + 5 * 16 + 8)[0] == 0x200040000000 + 100, "intern word1 kept"
    assert struct.unpack_from('<Q', out, len(out) - 40)[0] == 2, "trailer version 2"
    bad = []
    bad.append(img[:-1])                                    # truncated stream
    bad.append(img[:5120 + 64 + 20])                        # truncated intern
    x = bytearray(img); struct.pack_into('<Q', x, 64, 1); bad.append(bytes(x))   # bad magic
    x = bytearray(img); struct.pack_into('<Q', x, 5120 + 64 + 16 + 24 + 8 + 8, 4); bad.append(bytes(x))  # trailer v4
    for i, x in enumerate(bad):
        try: decode(x)
        except Bad: continue
        raise SystemExit("selftest: bad input %d accepted" % i)
    print("decode.py selftest OK")
if __name__ == '__main__':
    try:
        if sys.argv[1] == '--selftest': selftest(); sys.exit(0)
        out = decode(open(sys.argv[1], 'rb').read())
    except Bad as e:
        sys.exit("decode.py: " + str(e))
    if len(sys.argv) > 2: open(sys.argv[2], 'wb').write(out)
    else: sys.stdout.buffer.write(out)
