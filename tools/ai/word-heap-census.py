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
import bisect
import json
import mmap
from pathlib import Path
import struct
import sys

# GNU 31.1 special forms (special-form-p).  GNU represents each as a subr
# object with max_args = UNEVALLED (lisp.h struct Lisp_Subr, PVEC_SUBR;
# eval.c eval_sub).  NeLisp's interim encoding is the definition
# (builtin NAME) held in a tag-7 view whose data word is SPECIAL_MARKER
# (nl_apply_do_fset); the census counts those views as special-form subr
# objects, not as cons header aliases (Doc 223).
SPECIAL_FORMS = frozenset((
    "and", "catch", "cond", "condition-case", "defconst", "defvar", "function",
    "if", "inline", "interactive", "let", "let*", "or", "prog1", "progn", "quote",
    "save-current-buffer", "save-excursion", "save-restriction", "setq",
    "unwind-protect", "while"))
SPECIAL_MARKER = 13108

TAGS = {3: "float", 4: "symbol", 5: "string", 6: "string", 7: "cons",
        8: "vector", 9: "char-table", 10: "bool-vector", 11: "cell",
        12: "record", 13: "bignum", 14: "string", 15: "string",
        16: "symbol", 17: "bytecode", 18: "native-subr", 19: "symbol-pos"}


def census(path):
    path = Path(path)
    handles, segments = [], []
    relfields = None
    intern_range = None
    old_intern_range = None
    image_roots = []
    intern_bytes = None
    if path.suffix == ".json":
        meta = json.loads(path.read_text())
        control = (path.parent / "control.bin").read_bytes()
        intern_range = struct.unpack_from("<2Q", control, 832)
        companion = Path(meta["binary"] + ".cold")
        if companion.is_file():
            with companion.open("rb") as f:
                h = struct.unpack("<8Q", f.read(64))
            old_intern_range = (h[7], h[7] + h[2])
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
        old_intern_range = (roots[3], roots[3] + intern)
        image_roots = roots[:3]
        segments.append((0, memoryview(raw)[64 + count * 8:64 + count * 8 + size]))
        intern_bytes = memoryview(raw)[64 + count * 8 + size:64 + count * 8 + size + intern]
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
    starts = sorted(blocks)

    def containing(addr):
        i = bisect.bisect_right(starts, addr) - 1
        if i < 0:
            return None
        a = starts[i]
        return a if addr < a + blocks[a][0] - 8 else None

    def read_words(addr, n):
        for base, data in segments:
            if base <= addr and addr + n * 8 <= base + len(data):
                return struct.unpack_from("<" + "Q" * n, data, addr - base)
        return ()

    def own(addr, kind):
        if addr in blocks:
            owners[addr].add(kind)

    def symbol_name(view):
        w = read_words(view, 4)
        if len(w) < 4 or w[0] not in (4, 16) or w[3] > 256:
            return None
        ptr, n = w[2], w[3]
        if intern_bytes is not None and intern_range[0] <= ptr and ptr + n <= intern_range[1]:
            return bytes(intern_bytes[ptr - intern_range[0]:ptr - intern_range[0] + n]).decode("utf-8", "replace")
        for base, data in segments:
            if base <= ptr and ptr + n <= base + len(data):
                return bytes(data[ptr - base:ptr - base + n]).decode("utf-8", "replace")
        return None

    def cons_cell(word):
        """The cell a cons WORD denotes: direct (payload | 2) or a 000 view."""
        if word & 7 == 2:
            return word - 2
        if word & 7 == 0 and word in blocks and blocks[word][0] == 40 and words[word][0] == 7:
            return words[word][1]
        return None

    def special_definition(payload):
        """PAYLOAD is the cell of (builtin NAME ...) with NAME a GNU special form."""
        first = read_words(payload, 2)
        if len(first) < 2 or symbol_name(first[0]) != "builtin":
            return False
        rest = cons_cell(first[1])
        second = read_words(rest, 2) if rest is not None else ()
        return len(second) == 2 and symbol_name(second[0]) in SPECIAL_FORMS

    # Stale headers can retain absolute pointers. Adjacency is diagnostic only:
    # repeated payload/header runs give equally plausible translations shifted
    # by72 bytes, and a stale target may have been reused. Never assign a payload
    # from this inference without an independent original-address anchor.
    stale_cons = [(a, words[a][1]) for a, (bt, _) in blocks.items()
                  if bt == 40 and words[a][0] == 7 and words[a][1] not in blocks]
    translations = collections.Counter()
    for a, p in stale_cons:
        if p > 4096:
            translations[p - (a + 40)] += 1
            translations[p - (a - 32)] += 1
    recovered = []
    for delta, adjacency in translations.most_common(4):
        targets = [(a, p - delta) for a, p in stale_cons
                   if p - delta in blocks and blocks[p - delta][0] == 32]
        if adjacency >= 100 and len(targets) >= 100:
            recovered.append(dict(delta=delta, adjacent_pairs=adjacency,
                                  valid_pairs=len(targets), samples=targets[:8]))

    # Function cells: slot 1 (+24) of a kind-12 record (a mirror entry).
    function_cells = set()
    if relfields is not None:
        for field in relfields:
            owner = containing(field)
            if owner is not None and field - owner == 24 and words[owner][0] & 63 == 12:
                function_cells.add(read_words(field, 1)[0])
    candidates = []
    embedded_owners = set()
    slots = set(a for a, (bt, _) in blocks.items() if bt == 40)
    # Serialized pointer fields expose embedded Sexp views in record type
    # tags, parse pools and frame arrays without scanning arbitrary byte data.
    if relfields is not None:
        for field in relfields:
            for off in (8, 16):
                a = field - off
                owner = containing(a)
                if owner is not None and a + 32 <= owner + blocks[owner][0] - 8:
                    slots.add(a)
    else:
        # Runtime WORD references also point at embedded views. Only exact
        # aligned Sexp layouts with validated payload edges are accepted below.
        for a, (bt, _) in blocks.items():
            if bt >= 40 and bt <= 1024 * 1024 + 8:
                slots.update(range(a, a + bt - 39, 32))
    def word_slot_object(addr):
        """True when ADDR lies inside a vectorlike object whose slots are 8-byte WORDs
        (a vector or record whose header kind and length match its block exactly).  A
        fixnum WORD such as 13 (the fixnum 3) is then not a tag-13 view header."""
        owner = containing(addr)
        if owner is None or owner == addr:
            return False
        header = read_words(owner, 1)
        return bool(header) and (header[0] & 63) in (8, 12) and \
            blocks[owner][0] - 8 == 8 + 8 * (header[0] >> 8)

    for addr in sorted(slots):
        w = read_words(addr, 4)
        if not w or w[0] not in TAGS:
            continue
        if word_slot_object(addr):
            continue
        if w[3] == 215 and (addr not in blocks or blocks[addr][0] != 40):
            # nl_gc_mark_pool_slots: this slot contains a WORD, not a Sexp.
            continue
        tag, payload = w[0], w[1]
        kind = TAGS[tag]
        if tag == 3 and (addr not in blocks or blocks[addr][0] != 40):
            continue
        if relfields is not None:
            off = 16 if tag in (4, 5, 14, 16, 13) else 8
            if tag != 3 and addr + off not in relfields:
                continue
        if tag == 7 and (payload not in blocks or blocks[payload][0] not in (24, 32)):
            continue
        if tag in (6, 8, 9, 10, 11, 12, 15, 17, 18, 19) and payload not in blocks:
            continue
        minimum = {6: 40, 8: 40, 9: 136, 10: 40, 11: 24,
                   12: 72, 15: 40, 17: 72, 18: 72, 19: 48}.get(tag)
        if minimum and blocks[payload][0] < minimum:
            continue
        if tag in (4, 5, 14, 16, 13):
            if w[2] in blocks:
                if w[3] > blocks[w[2]][0] - 8:
                    continue
            elif not (intern_range[0] <= w[2] and w[2] + w[3] <= intern_range[1]):
                continue
        if (tag == 7 and w[2] == SPECIAL_MARKER and addr in function_cells
                and special_definition(payload)):
            kind = "special-form-subr"
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
        if addr in blocks and blocks[addr][0] == 40:
            own(addr, kind)
        else:
            embedded_owners.add(containing(addr))
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
    for a in embedded_owners:
        if not owners.get(a):
            if blocks[a][0] == 192:
                own(a, "eval-context-storage")
            elif blocks[a][0] == 32768 * 32 + 8:
                own(a, "reader-pool-storage")
            else:
                own(a, "embedded-sexp-storage")
    for a, (bt, _) in blocks.items():
        if bt == 40 and words[a][0] == 7 and not owners.get(a):
            own(a, "cons-unrelocated-view")
    # Leaf views have no pointer relocation. Count their physical storage,
    # separately from immediate WORDs (which allocate nothing).
    for a, (bt, _) in blocks.items():
        if not owners.get(a) and bt == 40 and words[a][0] in (0, 1, 2):
            own(a, {0: "nil-view", 1: "t-view", 2: "integer-view"}[words[a][0]])
        if not owners.get(a) and bt == 40 and words[a][0] in (4, 5, 14, 16):
            w = words[a]
            if old_intern_range[0] <= w[2] and w[2] + w[3] <= old_intern_range[1]:
                own(a, TAGS[w[0]] + "-unrelocated-view")
    # A relocation table is also a complete inventory of serialized incoming
    # edges. Do not invent a Lisp type for detached bootstrap allocations.
    # Distinguish that proven storage class from unresolved reachable owners.
    inbound, outbound = {containing(a) for a in image_roots}, set()
    if relfields is not None:
        for field in relfields:
            # A field entry carries its pointer class (0, 2 or 6) in its low
            # bits; the field itself is the aligned word.  Reading at the
            # tagged offset ran past the span for a last-word field.
            value = read_words(field & ~7, 1)[0]
            inbound.add(containing(value))
            outbound.add(containing(field & ~7))
    for a, (bt, mark) in blocks.items():
        if owners.get(a):
            continue
        w = read_words(a, (bt - 8) // 8)
        if not any(w):
            own(a, "zero-reservation")
        elif bt >= 80:
            raw_bytes = struct.pack("<" + "Q" * len(w), *w).rstrip(b"\0")
            try:
                text = raw_bytes.decode("utf-8")
            except UnicodeDecodeError:
                text = ""
            if len(text) >= 64 and all(c.isprintable() or c in "\t\r\n" for c in text):
                own(a, "raw-text-bytes")
            elif bt >= 1024 * 1024:
                # Reader input reservations need not have a zero-filled tail.
                # Validate the complete NUL-terminated prefix, and report the
                # reservation class without claiming the tail is live text.
                try:
                    prefix = raw_bytes.split(b"\0", 1)[0].decode("utf-8")
                except UnicodeDecodeError:
                    prefix = ""
                if len(prefix) >= 64 and all(c.isprintable() or c in "\t\r\n" for c in prefix):
                    own(a, "text-prefix-reservation")
        if owners.get(a):
            continue
        if bt > 40 and all(v in (0, 2, 3, 7) or v & 3 == 1 or v in blocks for v in w):
            own(a, "word-array-storage")
        elif relfields is not None and a not in inbound and a not in outbound:
            own(a, "image-storage-without-serialized-edges")
        elif relfields is not None:
            own(a, "serialized-runtime-storage")
        else:
            # Exact object provenance is unavailable for these raw runtime
            # blocks; retain their physical storage class and a cons bound.
            own(a, "runtime-auxiliary-storage")
    rows = collections.defaultdict(lambda: dict(bytes=0, allocations=0))
    for addr, (bt, mark) in blocks.items():
        labels = owners.get(addr, set())
        kind = next(iter(labels)) if len(labels) == 1 else "shared" if labels else "unclassified"
        rows[kind]["bytes"] += bt
        rows[kind]["allocations"] += 1
    total = all_bytes - free_bytes
    possible_payload_bytes = possible_view_bytes = possible_view_windows = 0
    for a, (bt, _) in blocks.items():
        if owners.get(a) == {"cons"}:
            continue
        # This bound is independent of semantic owner heuristics. Current
        # cons payloads allocate 24 bytes (BT32); header views occupy 32 bytes
        # starting at any 8-aligned address. Count all possible tag-byte=7
        # windows, including stale pointers and false positives from t WORDs.
        if bt == 32:
            possible_payload_bytes += bt
        elif bt >= 40:
            w = read_words(a, (bt - 8) // 8)
            windows = sum(1 for i in range(len(w) - 3) if w[i] & 255 == 7)
            if windows:
                possible_view_windows += windows
                possible_view_bytes += min(bt, 8 + 32 * windows)
    for kind, row in rows.items():
        row.update(objects=len(objects[kind]) if kind in objects else None,
                   header_aliases=aliases[kind], percent=100 * row["bytes"] / total)
    opaque_kinds = {"image-storage-without-serialized-edges", "serialized-runtime-storage", "runtime-auxiliary-storage", "embedded-sexp-storage", "word-array-storage", "shared"}
    opaque_addresses = [a for a, labels in owners.items()
                        if labels & opaque_kinds or len(labels) > 1]
    opaque = sum(row["bytes"] for kind, row in rows.items() if kind in opaque_kinds)
    cons = rows.get("cons", {}).get("bytes", 0)
    return dict(schema=3, path=str(path.resolve()), metric="nonfree BLOCK_TOTAL bytes; Lisp types plus physical storage classes",
                total=total, all_bytes=all_bytes, free_bytes=free_bytes, free_count=free_count,
                intern_bytes_excluded=intern_range[1] - intern_range[0],
                rows=dict(sorted(rows.items())),
                cons_bytes_lower=cons,
                cons_bytes_upper=cons + possible_payload_bytes + possible_view_bytes,
                cons_percent_upper=100 * (cons + possible_payload_bytes + possible_view_bytes) / total,
                cons_bound=dict(possible_extra_payload_bytes=possible_payload_bytes,
                                possible_extra_view_bytes=possible_view_bytes,
                                possible_extra_view_windows=possible_view_windows,
                                proof="All BT32 non-cons blocks may be payloads. Every 8-aligned, fully contained 32-byte window with low tag byte 7 may be a view; union bounded by min(block bytes, 8+32*windows). Other blocks cannot contain a current cons payload/view. Includes shared owners, stale pointers and t-WORD false positives."),
                semantic_type_unresolved_bytes=opaque,
                fossil_cons=dict(translations=recovered, recovered_header_count=0,
                                 stale_header_count=len(stale_cons),
                                 proof="Stale tag7 headers have a separate physical view class. Adjacency translations are ambiguous and never confer payload ownership; original address anchor and reuse checks would be needed. No reachable-object lower bound is inferred."),
                storage_proof="Pointer-bearing views validated by relocation fields or runtime payload edges; scalar views use exact32-byte layout; old names use saved intern bounds; zero/text/WORD reservations checked in full. Absence of serialized edges is a physical storage class, not proof of semantic detachment. Cons upper bound exhausts every eligible allocation/window independently of owner heuristics. Semantic unresolved bytes remain explicit.",
                classification="relocation-validated" if relfields is not None else "runtime block-start-validated",
                semantic_unresolved_histogram=collections.Counter(blocks[a][0] for a in opaque_addresses).most_common(),
                semantic_unresolved_largest=[dict(address=a, bytes=blocks[a][0], words=words[a])
                                             for a in sorted(opaque_addresses, key=lambda a: blocks[a][0], reverse=True)[:20]],
                semantic_unresolved_prefixes=collections.Counter((blocks[a][0], words[a][:2]) for a in opaque_addresses).most_common(30))


if __name__ == "__main__":
    result = census(sys.argv[1])
    print(json.dumps(result, indent=2))
