#!/usr/bin/env python3
"""Build a Bernstein CDB from an SKK dictionary (SKK-JISYO.L.utf8).

Standalone (no Emacs): the bridge runtime's buffer-free cdb reader
(nemacs-runtime-cdb.el) does lookups against the result, so SKK kana-kanji
conversion works on the GUI runtime with no network.  ddskk's own builder
(skk-convert.el dictionary-build-cdb-from-skk-fast) needs a host Emacs
(with-temp-buffer); this is the host-free equivalent.

  python3 skk-jisyo-to-cdb.py SKK-JISYO.L.utf8 SKK-JISYO.L.cdb

Key = yomi (text before the first space), value = the candidate string
(e.g. "/未来/味蕾/").  Uses the standard djb cdb hash.  Verified: a 175,774-
entry dictionary -> cdb-get "みらい" => "/未来/味蕾/" on the bridge GUI runtime.
"""
import struct, sys

def djb(s):
    h = 5381
    for b in s:
        h = ((h * 33) ^ b) & 0xffffffff
    return h

def build(src, out):
    items = []
    with open(src, encoding="utf-8") as f:
        for line in f:
            if line.startswith(";;") or not line.strip():
                continue
            sp = line.find(" ")
            if sp <= 0:
                continue
            yomi = line[:sp]
            cands = line[sp+1:].rstrip("\n")
            if cands:
                items.append((yomi.encode("utf-8"), cands.encode("utf-8")))
    body = bytearray(); tables = [[] for _ in range(256)]; pos = 2048
    for k, v in items:
        h = djb(k)
        body += struct.pack("<II", len(k), len(v)) + k + v
        tables[h & 255].append((h, pos))
        pos += 8 + len(k) + len(v)
    header = bytearray(); ht = bytearray(); tpos = pos
    for i in range(256):
        t = tables[i]; n = len(t) * 2 if t else 0; slots = [(0, 0)] * n
        for h, p in t:
            idx = (h >> 8) % n
            while slots[idx] != (0, 0):
                idx = (idx + 1) % n
            slots[idx] = (h, p)
        header += struct.pack("<II", tpos, n)
        for h, p in slots:
            ht += struct.pack("<II", h, p)
        tpos += n * 8
    with open(out, "wb") as f:
        f.write(header + body + ht)
    return len(items), tpos

if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: skk-jisyo-to-cdb.py SKK-JISYO.L.utf8 out.cdb")
    n, sz = build(sys.argv[1], sys.argv[2])
    print(f"built {sys.argv[2]}: {n} entries, {sz} bytes")
