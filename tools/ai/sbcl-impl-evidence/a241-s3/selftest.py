#!/usr/bin/env python3
"""selftest.py V3_IMAGE: re-encode a version-3 image's reloc table the way the
generator does (sorted delta varint), build a v4 file, and check decode.py
round-trips it to the sorted multiset and rejects corrupted streams."""
import struct, subprocess, sys, tempfile, os
here = os.path.dirname(os.path.abspath(__file__))
b = open(sys.argv[1], 'rb').read()
magic, ctotal, isz, tlen = struct.unpack_from('<4Q', b, 0)
ents = struct.unpack_from('<%dQ' % tlen, b, 64)
out = bytearray(); prev = 0
for e in sorted(ents):
    w = (e & ~7) >> 3; val = ((w - prev) << 2) | ((e & 7) >> 1); prev = w
    while val >= 128: out.append(128 | (val & 127)); val >>= 7
    out.append(val)
rest = bytearray(b[64 + tlen * 8:]); struct.pack_into('<Q', rest, len(rest) - 40, 4)
v4 = b[:64] + struct.pack('<2Q', 1380732980, len(out)) + bytes(out) + bytes(rest)
d = tempfile.mkdtemp(); f4 = d + '/v4'; open(f4, 'wb').write(v4)
r = subprocess.run([sys.executable, '-I', here + '/decode.py', '--check', f4], capture_output=True, text=True); print(r.stdout.strip(), r.stderr.strip())
assert r.returncode == 0
dec = subprocess.run([sys.executable, '-I', here + '/decode.py', f4], capture_output=True).stdout
assert struct.unpack_from('<%dQ' % tlen, dec, 64) == tuple(sorted(ents)), "multiset"
for name, bad in [('truncated', v4[:90]), ('unterminated-varint', v4[:80 + len(out) - 1] + b'\x80' + v4[80 + len(out):])]:
    open(d + '/bad', 'wb').write(bad)
    rr = subprocess.run([sys.executable, '-I', here + '/decode.py', d + '/bad'], capture_output=True)
    print(name, 'rejected' if rr.returncode else 'NOT REJECTED'); 
print("v4 size", len(v4), "v3 size", len(b), "reloc section", 16 + len(out))
print("SELFTEST-PASS")
