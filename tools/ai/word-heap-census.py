#!/usr/bin/env python3
"""Read-only allocation census for flat images or captured runtime segments.

Bytes include each allocator's 8-byte BLOCK_TOTAL prefix. Shared backing is
counted once. Header aliases are counted separately from unique payloads.
Unclassified allocations stay explicit; size alone never implies a cons.
Image relocation entries validate pointer fields. Runtime snapshots use exact
allocated block starts, and report structural classification rather than a
reachability oracle. No image is loaded or modified.
"""
import collections
import json
import mmap
from pathlib import Path
import struct
import sys

TAGS = {3: "float", 4: "symbol", 5: "string", 6: "string", 7: "cons",
        8: "vector", 9: "char-table", 10: "bool-vector", 11: "cell",
        12: "record", 13: "bignum", 14: "string", 15: "string",
        16: "symbol", 17: "bytecode", 18: "native-subr", 19: "symbol-pos"}


def census(path):
    path = Path(path)
    handles, segments = [], []
    relfields = None
    intern_range = None
    if path.suffix == ".json":
        meta = json.loads(path.read_text())
        control = (path.parent / "control.bin").read_bytes()
        intern_range = struct.unpack_from("<2Q", control, 832)
        for seg in meta["segments"]:
            f = (path.parent / seg["path"]).open("rb")
            handles.append(f)
            segments.append((seg["address"], mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)))
    else:
        f = path.open("rb")
        handles.append(f)
        raw = mmap.mmap(f.fileno(), 0, access=mmap.ACCESS_READ)
        magic, size, intern, count, *roots = struct.unpack_from("<8Q", raw)
        if magic != 1179407692 or 64 + count * 8 + size + intern not in (len(raw), len(raw) - 48):
            raise ValueError("invalid flat image envelope")
        relfields = {struct.unpack_from("<Q", raw, 64 + i * 8)[0] for i in range(count)}
        intern_range = (size, size + intern)
        segments.append((0, memoryview(raw)[64 + count * 8:64 + count * 8 + size]))
    blocks, words, free_bytes, free_count, all_bytes = {}, {}, 0, 0, 0
    for start, data in segments:
        pos = 0
        while pos < len(data):
            w = struct.unpack_from("<Q", data, pos)[0]
            bt, mark = w & 0xfffffff8, w & 7
            if bt < 16 or pos + bt > len(data):
                raise ValueError(f"bad block at {start + pos:#x}, bt={bt}")
            all_bytes += bt
            if mark == 2:
                free_bytes += bt
                free_count += 1
            else:
                address = start + pos + 8
                blocks[address] = (bt, mark)
                words[address] = struct.unpack_from("<" + "Q" * min(9, (bt - 8) // 8), data, pos + 8)
            pos += bt
    owners, aliases, objects = collections.defaultdict(set), collections.Counter(), collections.defaultdict(set)

    def own(addr, kind):
        if addr in blocks:
            owners[addr].add(kind)

    candidates = []
    for addr, (bt, mark) in blocks.items():
        w = words[addr]
        if bt != 40 or not w or w[0] not in TAGS:
            continue
        tag, payload = w[0], w[1]
        kind = TAGS[tag]
        if relfields is not None:
            off = 16 if tag in (4, 5, 14, 16, 13) else 8
            if tag != 3 and addr + off not in relfields:
                continue
        if tag == 7 and (payload not in blocks or blocks[payload][0] != 32):
            continue
        if tag in (6, 8, 9, 10, 11, 12, 15, 17, 18, 19) and payload not in blocks:
            continue
        if tag in (4, 5, 14, 16, 13):
            if w[2] in blocks:
                if w[3] > blocks[w[2]][0] - 8:
                    continue
            elif not (intern_range[0] <= w[2] and w[2] + w[3] <= intern_range[1]):
                continue
        candidates.append((addr, tag, kind, payload, w))
    # Identify backing before counting headers: a backing array that happens
    # to begin with a small integer is not another Sexp value header.
    backing = set()
    for addr, tag, kind, payload, w in candidates:
        if tag == 3:
            continue
        backing.add(w[2] if tag in (4, 5, 14, 16, 13) else payload)
    for addr, tag, kind, payload, w in candidates:
        if addr in backing:
            continue
        own(addr, kind)
        aliases[kind] += 1
        identity = addr if tag == 3 else (w[1] if tag == 16 else w[2] if tag in (4, 5, 14, 13) else payload)
        objects[kind].add(identity)
        if tag in (4, 5, 14, 16, 13):
            own(w[2], kind)
        elif tag != 3:
            own(payload, kind)
            pw = words[payload]
            # NlVector: buffer @+8, len @+16. NlRecord: buffer @+32,
            # len @+48. NlStr: buffer @+8, bytes @+16.
            if tag in (6, 8, 15) and len(pw) > 1:
                own(pw[1], kind)
            elif tag in (12, 17, 18) and len(pw) > 4:
                own(pw[4], kind)
    rows = collections.defaultdict(lambda: dict(bytes=0, allocations=0))
    for addr, (bt, mark) in blocks.items():
        labels = owners.get(addr, set())
        kind = next(iter(labels)) if len(labels) == 1 else "shared" if labels else "unclassified"
        rows[kind]["bytes"] += bt
        rows[kind]["allocations"] += 1
    total = all_bytes - free_bytes
    for kind, row in rows.items():
        row.update(objects=len(objects[kind]) if kind in objects else None,
                   header_aliases=aliases[kind], percent=100 * row["bytes"] / total)
    return dict(schema=2, path=str(path.resolve()), metric="nonfree BLOCK_TOTAL bytes, structural type classification",
                total=total, all_bytes=all_bytes, free_bytes=free_bytes, free_count=free_count,
                intern_bytes_excluded=intern_range[1] - intern_range[0],
                rows=dict(sorted(rows.items())),
                classification="relocation-validated" if relfields is not None else "runtime block-start-validated")


if __name__ == "__main__":
    result = census(sys.argv[1])
    print(json.dumps(result, indent=2))
