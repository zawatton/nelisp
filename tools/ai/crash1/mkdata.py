"""Create the B1 data files in DIR and write DIR/expected.txt (name, size, md5 of the file bytes)."""
import hashlib, os, random, sys
d = sys.argv[1]
os.makedirs(d, exist_ok=True)
rnd = random.Random(7)
def text(n):
    # ASCII lines with periodic 2/3-byte UTF-8 characters, cut on a character boundary
    out = bytearray(); i = 0
    while len(out) < n:
        out += (f"line {i} " + ("é日本" if i % 7 == 0 else "abc") + " xyz\n").encode()
        i += 1
    cut = n
    while 0 < cut < len(out) and (out[cut] & 0xC0) == 0x80: cut -= 1
    return bytes(out[:cut])
files = {}
for n in (0, 1, 4095, 4096, 4097, 8388607, 8388608, 8388609, 12000000, 20000000, 33554432, 50000000):
    files[f"ascii-{n}.txt"] = (b"x" * n)
files["utf8-20000000.txt"] = text(20000000)
files["utf8-12000000.txt"] = text(12000000)
files["bin-20000000.bin"] = rnd.randbytes(20000000)
files["bin-9000000.bin"] = rnd.randbytes(9000000)
with open(os.path.join(d, "expected.txt"), "w") as ex:
    for name in sorted(files):
        data = files[name]
        with open(os.path.join(d, name), "wb") as f: f.write(data)
        ex.write(f"{name} {len(data)} {hashlib.md5(data).hexdigest()}\n")
