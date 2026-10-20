#!/usr/bin/env python3
"""Doc 216 S4.1: retained slopes and an independently replayed heap census.

measure baseline|candidate   capture both linkages (GNU on baseline only)
check                       replay all hash-bound raw inputs, require S4.1
check-baseline              same representation/size checks, expected red
controls                    demonstrate rejecting missing rows and real S1 debt

Evidence lives in target/word-heap-s4v.  No S0/S1 evidence is changed.  Bytes
are arena BLOCK_TOTAL, including its eight-byte prefix; GNU's DWARF minima
and GC-accounted slopes are deliberately reported as distinct measurements.
"""
import argparse
import concurrent.futures
import copy
import hashlib
import json
import os
from pathlib import Path
import re
import select
import shutil
import signal
import struct
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/word-heap-s4v"
PROBE = ROOT / "tools/ai/word-heap-vectorlike.el"
CENSUS_PROBE = ROOT / "tools/ai/word-heap-vectorlike-census.el"
KINDS = ("vector1", "record1", "char-table", "vector-slot")
ROOT_KINDS = KINDS[:3]
SIZES = (2000, 4000)
TAGS = {"vector1": 8, "record1": 12, "char-table": 9}
BASE_REVISION = "b51fe66f4"
GNU_LAYOUT_FILES = ("gnu-version.out", "gnu-layout.out", "gnu-layout.err", "gnu-layout.rc")


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def rel(path):
    return Path(path).resolve().relative_to(ROOT).as_posix()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def symbols(binary):
    raw = subprocess.check_output(["nm", "-n", str(binary)], text=True)
    return {p[2]: int(p[0], 16) for line in raw.splitlines()
            if len(p := line.split()) == 3}


def executable_paths(binary):
    return [Path(str(binary) + suffix) for suffix in ("", ".cold", ".native-startup.el")]


def population(job):
    who, binary, kind, n = job
    directory = OUT / "objects"
    stem = directory / f"{who}-{kind}-{n}"
    cmd = [str(binary), *( ["-Q", "--batch", "-l"] if who == "gnu" else ["--load"]), str(PROBE)]
    if who != "gnu":
        cmd += ["--eval", "nil"]
    run = subprocess.run(cmd, cwd=ROOT, capture_output=True, timeout=180,
                         env=dict(os.environ, WHV_KIND=kind, WHV_N=str(n)))
    for suffix, data in (("out", run.stdout), ("err", run.stderr),
                         ("rc", (str(run.returncode) + "\n").encode())):
        stem.with_suffix("." + suffix).write_bytes(data)
    need(run.returncode == 0 and not run.stderr and b"WHV-OBJECT-DONE\n" in run.stdout,
         f"population failed: {rel(stem)} rc={run.returncode}")
    return rel(stem)


def snapshot(who, binary):
    """Capture actual used arena bytes from a stopped child, without writes."""
    directory = OUT / "census" / who
    directory.mkdir(parents=True, exist_ok=True)
    sym = symbols(binary)
    env = dict(os.environ, WHV_PIN_BEGIN=str(sym["nl_root_pin_begin"]),
               WHV_VECTOR_SLOT=str(sym["nl_vector_slot_word"]))
    with (directory / "stderr").open("wb") as err:
        proc = subprocess.Popen([str(binary), "--repl", "--no-prompt", "--no-print"],
                                cwd=ROOT, env=env, stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE, stderr=err)
        try:
            proc.stdin.write(f'(load "{rel(CENSUS_PROBE)}")\n'.encode())
            proc.stdin.flush()
            output, deadline = b"", time.monotonic() + 120
            while b"WHV-CENSUS-READY\n" not in output:
                need(time.monotonic() < deadline, f"census marker missing: {who}")
                ready, _, _ = select.select([proc.stdout], [], [], 1)
                if ready:
                    part = os.read(proc.stdout.fileno(), 65536)
                    need(part, f"census process exited: {who}, rc={proc.poll()}")
                    output += part
            (directory / "stdout").write_bytes(output)
            proc.send_signal(signal.SIGSTOP)
            os.waitpid(proc.pid, os.WUNTRACED)
            with open(f"/proc/{proc.pid}/mem", "rb", buffering=0) as memory:
                def read(address, size):
                    memory.seek(address)
                    data = memory.read(size)
                    need(len(data) == size, "short snapshot read")
                    return data

                def word(address):
                    return struct.unpack("<Q", read(address, 8))[0]

                arena = word(sym["nl_arena_base"])
                control = read(arena, 1024)
                (directory / "control.bin").write_bytes(control)
                head, segments = word(arena + 704), []
                chunk = head
                while chunk:
                    base, cap, cursor, start, unused, unused2, nxt = struct.unpack("<7Q", read(chunk, 56))
                    if chunk == head:
                        cursor = word(arena)
                    end = base + cursor
                    name = f"chunk-{len(segments)}.bin"
                    (directory / name).write_bytes(read(start, end - start))
                    segments.append(dict(path=name, address=start, bytes=end - start))
                    chunk = nxt
                stats = list(struct.unpack("<8Q", read(sym["nl_gc_stats"], 64)))
                write_json(directory / "snapshot.json",
                           dict(binary=rel(binary), arena=arena, segments=segments,
                                gc_stats=stats, roots=48,
                                discovery="existing pinned view and nl_vector_slot_word; census independently reads raw blocks"))
        finally:
            if proc.poll() is None:
                proc.send_signal(signal.SIGCONT)
                proc.terminate()
                try:
                    proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    proc.wait()
            (directory / "rc").write_text(str(proc.returncode) + "\n")
    need(not (directory / "stderr").read_bytes(), f"census stderr: {who}")


def raw_paths(who):
    paths = []
    for kind in KINDS:
        for n in SIZES:
            paths += [OUT / "objects" / f"{who}-{kind}-{n}.{suffix}"
                      for suffix in ("out", "err", "rc")]
    if who != "gnu":
        paths.extend(p for p in (OUT / "census" / who).iterdir() if p.is_file())
    return paths


def measure(which):
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "objects").mkdir(exist_ok=True)
    base = ROOT / "target/word-heap-s1"
    candidate = ROOT / "target/word-heap-s4v"
    binary_dir = base if which == "baseline" else candidate
    prefix = "base" if which == "baseline" else "candidate"
    bins = {f"{prefix}-{mode}": binary_dir / mode / f"nelisp-{mode}"
            for mode in ("dynamic", "static")}
    if which == "baseline":
        gnu = Path(shutil.which("emacs")).resolve()
        bins["gnu"] = gnu
        version = subprocess.check_output([str(gnu), "-Q", "--batch", "--eval", "(princ emacs-version)"], text=True)
        need(version == "31.1", f"GNU31.1 required, got {version}")
        (OUT / "gnu-version.out").write_text(version + "\n")
        dwarf = subprocess.run(["gdb", "-nx", "-batch", str(gnu),
                                "-ex", "p sizeof (union vectorlike_header)",
                                "-ex", "p sizeof (struct Lisp_Vector)",
                                "-ex", "p sizeof (struct Lisp_Char_Table)",
                                "-ex", "ptype struct Lisp_Char_Table"], capture_output=True, timeout=30)
        (OUT / "gnu-layout.out").write_bytes(dwarf.stdout)
        (OUT / "gnu-layout.err").write_bytes(dwarf.stderr)
        (OUT / "gnu-layout.rc").write_text(str(dwarf.returncode) + "\n")
        need(dwarf.returncode == 0 and not dwarf.stderr, "GNU DWARF extraction failed")
    identities = {rel(p): sha(p) for who, binary in bins.items() if who != "gnu"
                  for p in executable_paths(binary)}
    if "gnu" in bins:
        identities["gnu-executable"] = sha(bins["gnu"])
    inputs = {rel(p): sha(p) for p in (PROBE, CENSUS_PROBE, Path(__file__))}
    jobs = [(who, binary, kind, n) for who, binary in bins.items()
            for kind in KINDS for n in SIZES]
    started = time.monotonic()
    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
        for result in pool.map(population, jobs):
            print(result, flush=True)
    for who, binary in bins.items():
        if who != "gnu":
            snapshot(who, binary)
            print(f"snapshot {who}", flush=True)
    hashes = {rel(p): sha(p) for who in bins for p in raw_paths(who)}
    if "gnu" in bins:
        hashes.update({rel(OUT / name): sha(OUT / name) for name in GNU_LAYOUT_FILES})
    need(inputs == {rel(p): sha(p) for p in (PROBE, CENSUS_PROBE, Path(__file__))}, "probe changed during capture")
    need(all(sha(ROOT / p) == h for p, h in identities.items() if p != "gnu-executable"), "binary changed during capture")
    write_json(OUT / f"{which}-evidence.json",
               dict(schema=1, base_commit=BASE_REVISION, binaries=identities,
                    inputs=inputs, raw=hashes, populations=len(jobs),
                    seconds=time.monotonic() - started))


def verify_inputs(record):
    need(record["schema"] == 1 and record["base_commit"] == BASE_REVISION, "wrong evidence schema/base")
    for section in ("binaries", "inputs", "raw"):
        for name, digest in record[section].items():
            path = Path(shutil.which("emacs")).resolve() if name == "gnu-executable" else ROOT / name
            need(sha(path) == digest, f"changed {section}: {name}")


def slopes(who):
    row = {}
    for kind in KINDS:
        values = {}
        for n in SIZES:
            stem = OUT / "objects" / f"{who}-{kind}-{n}"
            raw = stem.with_suffix(".out").read_bytes()
            need(stem.with_suffix(".rc").read_text().strip() == "0" and
                 not stem.with_suffix(".err").read_bytes(), f"failed population {who}/{kind}/{n}")
            matches = re.findall(rb"^WHV-OBJECT (\S+) (\d+) (-?\d+)$", raw, re.M)
            need(len(matches) == 1 and b"WHV-OBJECT-DONE\n" in raw, "missing/duplicate population")
            k, count, value = matches[0]
            need(k.decode() == kind and int(count) == n, "mislabelled population")
            values[n] = int(value)
        row[kind] = (values[4000] - values[2000]) / 2000
        need(row[kind] > 0, "nonpositive retained slope")
    return row


def census(who, *, directory=None):
    directory = directory or OUT / "census" / who
    meta = json.loads((directory / "snapshot.json").read_text())
    need(meta["roots"] == 48 and meta["gc_stats"][3] >= 2, "census lacks collections/roots")
    need(not (directory / "stderr").read_bytes(), "census stderr")
    raw = (directory / "stdout").read_bytes()
    roots = [(int(i), int(w)) for i, w in re.findall(rb"^WHV-ROOT (\d+) (\d+)$", raw, re.M)]
    need([i for i, w in roots] == list(range(48)) and b"WHV-CENSUS-READY\n" in raw, "incomplete census root discovery")
    blocks = {}
    for seg in meta["segments"]:
        data = (directory / seg["path"]).read_bytes()
        need(len(data) == seg["bytes"], "truncated arena segment")
        pos = 0
        while pos < len(data):
            need(pos + 8 <= len(data), "truncated allocator header")
            header = struct.unpack_from("<Q", data, pos)[0]
            total, mark = header & 0xfffffff8, header & 7
            need(total >= 16 and pos + total <= len(data), "invalid allocator block")
            if mark != 2:
                blocks[seg["address"] + pos + 8] = (total, data, pos + 8)
            pos += total

    def word(owner, offset=0):
        need(owner in blocks, "field owner is not an allocated block start")
        total, data, start = blocks[owner]
        need(0 <= offset and offset % 8 == 0 and offset + 8 <= total - 8,
             "census field read escapes its owning allocation")
        return struct.unpack_from("<Q", data, start + offset)[0]

    def extra_storage(extra):
        need(extra in blocks and blocks[extra][0] >= 8 + 10 * 32,
             "char-table extra allocation must hold ten 32-byte cells")
        need(all(word(extra, i * 32) == 3 for i in range(10)),
             "empty char-table extra cell changed")
        return blocks[extra][0]

    result = {kind: dict(objects=0, legacy_header_boxes=0, legacy_alias_boxes=0, structural_bytes=[],
                        canonical_words=[], header_words=[]) for kind in ROOT_KINDS}
    payload_owners = {}
    for i, value in roots:
        kind = ROOT_KINDS[i % 3]
        row = result[kind]
        row["objects"] += 1
        row["canonical_words"].append(value)
        address, low = value & ~7, value & 7
        need(address in blocks, "root is not an allocated block start")
        total = blocks[address][0]
        header = word(address)
        row["header_words"].append(header)
        if low == 0:
            need(total == 40 and header == TAGS[kind], "malformed legacy root")
            row["legacy_header_boxes"] += 1
            payload = word(address, 8)
            need(payload in blocks, "legacy payload is not allocated")
            payload_owners[payload] = kind
            structural = total + blocks[payload][0]
            if kind in ("vector1", "record1"):
                off, lengthoff = (8, 16) if kind == "vector1" else (32, 48)
                slots = word(payload, off)
                need(slots in blocks and word(payload, lengthoff) == 1, "legacy slot storage malformed")
                need(word(slots) == 3, "legacy nil slot changed")
                structural += blocks[slots][0]
            else:
                extra = word(payload, 96)
                need(extra in blocks and word(payload, 104) == 10 and word(payload, 112) == 10,
                     "legacy char-table extra buffer malformed")
                structural += extra_storage(extra)
        elif low == 6:
            need(header & 255 == TAGS[kind], "wrong direct vectorlike kind")
            payload_owners[address] = kind
            structural = total
            if kind == "vector1":
                need(header >> 8 == 1 and total == 24 and word(address, 8) == 3,
                     "vector1 must have one inline eight-byte slot")
            elif kind == "record1":
                need(header >> 8 == 1 and total == 32 and word(address, 16) == 3,
                     "record1 must have type and one inline user WORD in exactly 32 bytes")
            else:
                # S4 retains the existing interval/extra cell strides.  The
                # empty constructor owns its 80B body and ten extra cells.
                need(total == 88 and word(address, 16) == 3 and
                     all(word(address, off) == 0 for off in (24, 32, 40, 48)),
                     "empty direct char-table body malformed")
                extra = word(address, 56)
                need(extra in blocks and word(address, 64) == 10 and word(address, 72) == 10,
                     "direct char-table extra storage malformed")
                structural += extra_storage(extra)
        else:
            raise ValueError(f"unexpected root pointer tag {low}")
        row["structural_bytes"].append(structural)
    # Canonical direct WORDs alone cannot prove header elimination.  Count
    # every separately allocated old view that aliases one of these exact
    # payload identities, including constructor-path views invisible in the
    # keeper.  FFI's diagnostic keeper view is a different object identity.
    for address, (total, data, off) in blocks.items():
        if total == 40:
            tag, payload = word(address), word(address, 8)
            kind = payload_owners.get(payload)
            if kind is not None and tag == TAGS[kind]:
                result[kind]["legacy_alias_boxes"] += 1
    for kind, row in result.items():
        need(row["objects"] == 16 and len(set(row["canonical_words"])) == 16,
             f"missing/distinct retained {kind} roots")
        need(len(set(row["structural_bytes"])) == 1, "inconsistent structural allocation")
        row["bytes_per_object"] = row["structural_bytes"][0]
    return result


def acceptance(table, counted):
    need(table["vector-slot"] == 8, "vector slots must remain eight bytes")
    need(table["record1"] <= 32, f"retained record1 {table['record1']} exceeds32 arena bytes")
    for kind, row in counted.items():
        need(row["legacy_header_boxes"] == 0, f"{kind} retains {row['legacy_header_boxes']} legacy32-byte header boxes")
        need(row["legacy_alias_boxes"] == 0, f"{kind} retains {row['legacy_alias_boxes']} aliased legacy32-byte header boxes")


def replay(which, enforce=True):
    record = json.loads((OUT / f"{which}-evidence.json").read_text())
    prefix = "base" if which == "baseline" else "candidate"
    whos = [f"{prefix}-{mode}" for mode in ("dynamic", "static")]
    if which == "baseline":
        whos.append("gnu")
    expected_raw = {rel(p) for who in whos for p in raw_paths(who)}
    if which == "baseline":
        expected_raw.update(rel(OUT / name) for name in GNU_LAYOUT_FILES)
    need(set(record["raw"]) == expected_raw, "incomplete raw input hash manifest")
    need(set(record["inputs"]) == {rel(p) for p in (PROBE, CENSUS_PROBE, Path(__file__))},
         "incomplete probe identity manifest")
    directory = ROOT / "target" / ("word-heap-s1" if which == "baseline" else "word-heap-s4v")
    binary_paths = {rel(p) for mode in ("dynamic", "static")
                    for p in executable_paths(directory / mode / f"nelisp-{mode}")}
    need(set(record["binaries"]) == binary_paths | ({"gnu-executable"} if which == "baseline" else set()),
         "incomplete binary identity manifest")
    if which == "baseline":
        original = json.loads((ROOT / "target/word-heap-s1/s1-evidence.json").read_text())["hashes"]
        need(all(record["binaries"][path] == original[path] for path in binary_paths),
             "baseline differs from pinned S1 binaries")
    verify_inputs(record)
    need(record["populations"] == (24 if which == "baseline" else 16), "missing population runs")
    result = {}
    for mode in ("dynamic", "static"):
        who = f"{prefix}-{mode}"
        result[who] = dict(retained=slopes(who), census=census(who))
        if enforce:
            acceptance(result[who]["retained"], result[who]["census"])
    return result


def check():
    baseline, candidate = replay("baseline", False), replay("candidate", True)
    dwarf = (OUT / "gnu-layout.out").read_text()
    need(re.findall(r"^\$\d+ = (\d+)$", dwarf, re.M) == ["8", "8", "552"], "GNU DWARF minimum changed")
    need((OUT / "gnu-layout.rc").read_text().strip() == "0" and not (OUT / "gnu-layout.err").read_bytes(), "GNU layout failed")
    gnu = slopes("gnu")
    for mode in ("dynamic", "static"):
        base, cand = baseline[f"base-{mode}"], candidate[f"candidate-{mode}"]
        for kind in ROOT_KINDS:
            need(base["census"][kind]["legacy_header_boxes"] == 16, "baseline does not expose actual header defect")
            need(cand["retained"][kind] < base["retained"][kind], "no retained allocation reduction")
    result = dict(baseline=baseline, candidate=candidate, gnu_gc_accounted=gnu,
                  gnu_minimum=dict(vector1=16, record1=24, **{"char-table": 552, "vector-slot": 8}),
                  metric="arena retained slope includes eight-byte allocator prefix; GNU minimum excludes allocator overhead")
    write_json(OUT / "vectorlike-result.json", result)
    print(json.dumps({who: row["retained"] for who, row in {**baseline, **candidate}.items()}))
    print("WORD-HEAP S4.1 PASS dyn+static roots=96 legacy-header-boxes=0 vector-slot=8 record1<=32")


def layout_fixture(directory, defect=None):
    """Raw allocated blocks, with 48 distinct roots and one optional defect.

    The short record is followed by a separate nil allocation, so a reader
    bounded only by the arena can mistake that neighbour for its user slot.
    The short extra buffer likewise has a valid first nil but lacks 9 cells.
    """
    directory.mkdir(parents=True, exist_ok=True)
    arena, data, roots = 0x10000000, bytearray(), []

    def block(words):
        address = arena + len(data) + 8
        data.extend(struct.pack("<" + "Q" * (len(words) + 1), 8 + len(words) * 8, *words))
        return address

    for _ in range(16):
        vector = block([8 | (1 << 8), 3])
        if defect == "record-underrun":
            record = block([12 | (1 << 8)])
            block([3])  # An unrelated nil that the old checker read across.
        else:
            record = block([12 | (1 << 8), 3, 3])
        extra = block([3] if defect == "extra-underrun" else [3, 0, 0, 0] * 10)
        table = block([9, 3, 3, 0, 0, 0, 0, extra, 10, 10])
        roots.extend([vector | 6, record | 6, table | 6])
    (directory / "chunk-0.bin").write_bytes(data)
    (directory / "stdout").write_text("".join(f"WHV-ROOT {i} {word}\n" for i, word in enumerate(roots))
                                      + "WHV-CENSUS-READY\n")
    (directory / "stderr").write_bytes(b"")
    write_json(directory / "snapshot.json",
               dict(roots=48, gc_stats=[0, 0, 0, 2],
                    segments=[dict(path="chunk-0.bin", address=arena, bytes=len(data))]))


def layout_controls():
    directory = OUT / "controls" / "census"
    valid = directory / "valid"
    layout_fixture(valid)
    counted = census("valid", directory=valid)
    acceptance(dict(record1=32, **{"vector-slot": 8}), counted)
    need({kind: row["bytes_per_object"] for kind, row in counted.items()}
         == {"vector1": 24, "record1": 32, "char-table": 416},
         "valid raw layout calibration changed")
    failures = {}
    for defect, expected in (("record-underrun", "record1 must have type"),
                             ("extra-underrun", "char-table extra allocation")):
        target = directory / defect
        layout_fixture(target, defect)
        try:
            census(defect, directory=target)
        except ValueError as error:
            need(expected in str(error), f"{defect} failed for an unrelated reason: {error}")
            failures[defect] = str(error)
        else:
            raise ValueError(f"malformed raw layout escaped: {defect}")
    write_json(OUT / "controls" / "layout-controls.json",
               dict(valid_roots=48, rejected=failures, verifier_sha256=sha(Path(__file__)),
                    raw={rel(path): sha(path) for path in directory.rglob("*") if path.is_file()}))
    return len(failures)


def controls():
    baseline = replay("baseline", False)
    rejected = 0
    for row in baseline.values():
        try:
            acceptance(row["retained"], row["census"])
        except ValueError:
            rejected += 1
    need(rejected == 2, "real S1 header defect did not fail both linkages")
    # Each structural condition must reject independently of the size cap.
    for kind in ROOT_KINDS:
        counted = copy.deepcopy(baseline["base-dynamic"]["census"])
        for row in counted.values():
            row["legacy_header_boxes"] = 0
            row["legacy_alias_boxes"] = 0
        counted[kind]["legacy_header_boxes"] = 1
        try:
            acceptance(dict(record1=32, **{"vector-slot": 8}), counted)
        except ValueError:
            rejected += 1
    need(rejected == 5, "typed legacy-header controls escaped")
    for kind in ROOT_KINDS:
        counted = copy.deepcopy(baseline["base-dynamic"]["census"])
        for row in counted.values():
            row["legacy_header_boxes"] = 0
            row["legacy_alias_boxes"] = 0
        counted[kind]["legacy_alias_boxes"] = 1
        try:
            acceptance(dict(record1=32, **{"vector-slot": 8}), counted)
        except ValueError:
            rejected += 1
    need(rejected == 8, "hidden aliased-header controls escaped")
    rejected += layout_controls()
    need(rejected == 10, "raw allocation-boundary controls escaped")
    print("WORD-HEAP S4.1 controls PASS real-base=2 typed-header=3 hidden-alias=3 raw-layout=2 total=10")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("measure", "check", "check-baseline", "controls"))
    parser.add_argument("which", nargs="?", choices=("baseline", "candidate"))
    args = parser.parse_args()
    if args.mode == "measure":
        need(args.which, "measure needs baseline or candidate")
        measure(args.which)
    elif args.mode == "check":
        check()
    elif args.mode == "check-baseline":
        replay("baseline", True)
    else:
        controls()


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        print(f"WORD-HEAP S4.1 FAIL: {error}", file=sys.stderr)
        sys.exit(1)
