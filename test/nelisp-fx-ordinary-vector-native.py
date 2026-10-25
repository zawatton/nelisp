#!/usr/bin/env python3
"""Execute the actual small production-emitted FX unit, with trapped dependencies.

This is an isolated WORD/storage/cache test, not a live evaluator/collector test.
Run emission first with nelisp-fx-vector-emit, then --check to execute/check pins.
"""
import argparse
import ctypes as C
import hashlib
import json
import mmap
import pathlib
import struct
import sys

if not __debug__:
    raise SystemExit("Python optimization disables qualification assertions")
ROOT = pathlib.Path(__file__).resolve().parents[1]
MASK = (1 << 64) - 1


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


class Unit:
    def __init__(self, report, directory):
        self.alive = []
        self.data = {}
        self.report = report
        self.mem = mmap.mmap(-1, 4 * 1024 * 1024,
                             prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
        self.base = C.addressof(C.c_char.from_buffer(self.mem))
        self.code = (directory / report["binary"]).read_bytes()
        assert digest(directory / report["binary"]) == report["sha256"]
        self.mem.write(self.code)
        self.symbols = {r["name"]: self.base + r["offset"] for r in report["symbols"]}
        self.traps = C.c_uint64(0)
        # Every unresolved function is an explicit counter+sentinel trap. No
        # positive/decline/cache test is allowed to execute one of these.
        trap = b"\x49\xbb" + struct.pack("<Q", C.addressof(self.traps)) + b"\x49\xff\x03\x48\xc7\xc0\xf7\xfc\xff\xff\xc3"
        at = (len(self.code) + 15) & -16
        self.mem[at:at + len(trap)] = trap
        self.trap_address = self.base + at
        data_at = (at + len(trap) + 4095) & -4096
        self.unresolved = set()
        for reloc in report["relocs"]:
            name = reloc["symbol"]
            if name in self.symbols:
                target = self.symbols[name]
            elif name in {"nl_arena_base", "nl_thread_registry"}:
                if name not in self.data:
                    self.data[name] = self.base + data_at
                    data_at += 4096
                target = self.data[name]
            else:
                self.unresolved.add(name)
                target = self.trap_address
            offset = reloc["offset"]
            if reloc["type"] in {"pc32", "plt32"}:
                value = target + reloc["addend"] - (self.base + offset + 4)
                self.mem[offset:offset + 4] = struct.pack("<i", value)
            elif reloc["type"] == "abs64":
                self.mem[offset:offset + 8] = struct.pack("<Q", target + reloc["addend"])
            else:
                raise AssertionError(reloc)
        # Actual arena-rebased wf_dirty reads this pointer; keep a complete
        # private page rather than replacing the production dirty operation.
        self.arena = self.block(4096)
        if "nl_arena_base" in self.data:
            self.put(self.data["nl_arena_base"], 0, self.arena)
        self.env = self.block(256)
        self.dirty = self.function("wf_dirty", 0)
        before = C.string_at(self.arena, 4096)
        self.dirty()
        after = C.string_at(self.arena, 4096)
        changed = [i for i in range(0, 4096, 8) if before[i:i+8] != after[i:i+8]]
        assert changed == [88], changed
        self.epoch = self.arena + 88
        self.put(self.epoch, 0, 0)
        assert self.traps.value == 0

    def block(self, size):
        data = C.create_string_buffer(size + 8)
        self.alive.append(data)
        return (C.addressof(data) + 7) & -8

    @staticmethod
    def put(address, offset, value):
        C.c_uint64.from_address(address + offset).value = value & MASK

    @staticmethod
    def get(address, offset=0):
        return C.c_uint64.from_address(address + offset).value

    def function(self, name, argc):
        return C.CFUNCTYPE(C.c_int64, *([C.c_uint64] * argc))(self.symbols[name])

    def vector(self, length, kind=8):
        raw = self.block(8 * (max(length, 0) + 3))
        self.put(raw, 0, 0xAABBCCDDEEFF1122)
        obj = raw + 8
        self.put(obj, 0, (length << 8) | kind)
        for index in range(max(length, 0)):
            self.put(obj, 8 + 8 * index, (index + 10) * 4 + 1)
        self.put(obj, 8 + max(length, 0) * 8, 0x1122334455667788)
        return obj + 6

    def name(self, text):
        data = C.create_string_buffer(text.encode())
        self.alive.append(data)
        view = self.block(32)
        self.put(view, 0, 4)
        self.put(view, 16, C.addressof(data))
        self.put(view, 24, len(text))
        return view

    def cons(self, car, cdr):
        payload = self.block(16)
        self.put(payload, 0, car)
        self.put(payload, 8, cdr)
        return payload + 2

    def builtin(self, name):
        return self.cons(self.name("builtin"), self.cons(self.name(name), 3))


def exercise(unit, expect_new):
    primitive_id = unit.function("m5_fx_primitive_id", 2)
    run = unit.function("m5_fx_primitive_run", 7)
    site = unit.function("m5_fx_site_kind", 4)
    count = 0
    def case(kind, args, expected, value=None, mutate=None):
        nonlocal count
        stack = unit.block(max(8, len(args) * 8))
        for i, arg in enumerate(args):
            unit.put(stack, i * 8, arg)
        result = unit.block(8)
        sentinel = 0xABCDABCDABCDABCD
        unit.put(result, 0, sentinel)
        before_epoch = unit.get(unit.epoch)
        before = C.string_at(args[0] - 14, 8 * 4) if args and args[0] > 4096 and args[0] & 7 == 6 else None
        status = run(kind & MASK, stack, len(args), len(args), result, unit.env, 0)
        assert status == expected, (kind, args, status, expected)
        assert unit.get(result) == (sentinel if value is None else value & MASK)
        assert unit.get(unit.epoch) - before_epoch == (1 if mutate else 0)
        if expected == -3 and before is not None:
            assert C.string_at(args[0] - 14, 8 * 4) == before
        assert unit.traps.value == 0, "unexpected isolated dependency"
        count += 1

    for name, argc, kind in (("aref", 2, -14), ("aset", 3, -15), ("length", 1, -16)):
        word = unit.name(name)
        for n in range(6):
            assert primitive_id(word, n) == (kind if expect_new and n == argc else 0)
            count += 1
    if not expect_new:
        v = unit.vector(2)
        case(-14, [v, 1], -3)
        return count

    for n in (0, 1, 2, 6, 8):
        v = unit.vector(n)
        case(-16, [v], 0, n * 4 + 1)
        for index in range(n):
            case(-14, [v, index * 4 + 1], 0, (index + 10) * 4 + 1)
            for value in (3, 7, 401, unit.vector(1), unit.cons(3, 3)):
                case(-15, [v, index * 4 + 1, value], 0, value, True)
                assert unit.get(v - 6, 8 + index * 8) == value
            # Restore for the next read's independently expected fixture value.
            unit.put(v - 6, 8 + index * 8, (index + 10) * 4 + 1)
        assert unit.get(v - 14) == 0xAABBCCDDEEFF1122
        assert unit.get(v - 6, 8 + n * 8) == 0x1122334455667788

    v = unit.vector(2)
    for tag in (7, 8):
        obj = unit.cons(3, 7) - 2 if tag == 7 else unit.vector(2) - 6
        view = unit.block(32)
        unit.put(view, 0, tag)
        unit.put(view, 8, obj)
        case(-15, [v, 1, view], 0, view, True)
        assert unit.get(v - 6, 8) == obj + (2 if tag == 7 else 6), "noncanonical store"

    boxed_int = unit.block(32)
    unit.put(boxed_int, 0, 2)
    for index in (-3, -7, 3, 7, boxed_int, 2 * 4 + 1, 999 * 4 + 1):
        case(-14, [v, index & MASK], -3)
        case(-15, [v, index & MASK, 401], -3)
    legacy_vector = unit.block(32)
    unit.put(legacy_vector, 0, 8)
    unit.put(legacy_vector, 8, v - 6)
    rejects = [3, 7, 1, 6, 14, legacy_vector, unit.name("str"), unit.block(32) + 4,
               unit.cons(3, 3), unit.vector(7), unit.vector(-1)]
    rejects += [unit.vector(2, kind) for kind in (0, 4, 5, 9, 12, 14, 16, 17, 18)]
    for receiver in rejects:
        case(-16, [receiver], -3)
        case(-14, [receiver, 1], -3)
        case(-15, [receiver, 1, 401], -3)
    for kind, arity in ((-14, 2), (-15, 3), (-16, 1)):
        for argc in range(5):
            if argc != arity:
                case(kind, [v] * argc, -3)

    # Execute actual site-kind producer, signed cached reader, uncached and
    # worker bypass. Cache identity changes model an already resolved fn;
    # live fset/advice requires the final full-runtime acceptance gate.
    for name, argc, kind in (("aref", 2, -14), ("aset", 3, -15), ("length", 1, -16)):
        fn = unit.builtin(name)
        cache = unit.vector(5)
        unit.put(cache - 6, 16, 3)
        assert site(cache, fn, argc, unit.env) == kind
        assert unit.get(cache - 6, 24) == ((kind << 2) + 1) & MASK
        assert site(cache, fn, argc, unit.env) == kind, "signed-cache round trip"
        assert site(0, fn, argc, unit.env) == kind
        unit.put(unit.env, 168, 1)
        unit.put(cache - 6, 24, 12345)
        assert site(cache, fn, argc, unit.env) == kind
        assert unit.get(cache - 6, 24) == 12345, "worker wrote cache"
        unit.put(unit.env, 168, 0)
        replacement = unit.cons(unit.name("lambda"), 3)
        assert site(cache, replacement, argc, unit.env) == 0
        assert unit.traps.value == 0
        count += 6
    return count


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", type=pathlib.Path, required=True)
    args = parser.parse_args()
    destination = args.check.resolve()
    directory = destination.parent
    emission_path = directory / "emission.json"
    emission_pin = digest(emission_path)
    emission = json.loads(emission_path.read_text())
    for path, pin in emission["files"].items():
        assert digest(ROOT / path) == pin, path
    output = {"status": "INCOMPLETE", "emission_sha256": emission_pin}
    destination.write_text(json.dumps(output, indent=2) + "\n")
    seed_manifest_path = directory / "seed-manifest.json"
    seed_pin = digest(seed_manifest_path)
    seed_manifest = json.loads(seed_manifest_path.read_text())
    for path, pin in seed_manifest["files"].items():
        assert digest(ROOT / path) == pin, ("seed source drift", path)
    assert digest(pathlib.Path(seed_manifest["snapshot"]) / "nelisp-cc-fx-call-primitives.el") == seed_manifest["fx_source_sha256"]
    units = {u["variant"]: Unit(u, directory) for u in emission["units"]}
    seed_count = exercise(units["seed"], False)
    try:
        exercise(units["seed"], True)
    except AssertionError:
        seed_red = True
    else:
        raise AssertionError("old seed accepted new-kind expectations")
    candidate_count = exercise(units["candidate"], True)
    try:
        exercise(units["unsigned-cache"], True)
    except AssertionError as error:
        assert "signed-cache" in str(error), str(error)
    else:
        raise AssertionError("unsigned cached-kind mutation accepted")
    for path, pin in emission["files"].items():
        assert digest(ROOT / path) == pin, path
    assert digest(emission_path) == emission_pin
    assert digest(seed_manifest_path) == seed_pin
    for path, pin in seed_manifest["files"].items():
        assert digest(ROOT / path) == pin, ("seed source drift", path)
    output.update(status="ISOLATED_NATIVE_PASS", seed_negative_control=seed_red,
                  seed_cases=seed_count, candidate_cases=candidate_count,
                  unsigned_cache_mutation="REJECTED",
                  seed_manifest_sha256=seed_pin, seed_identity=seed_manifest["source_identity"],
                  files=emission["files"],
                  units=[{"variant": name, "sha256": u.report["sha256"], "frames": u.report["frames"],
                          "unresolved_traps": sorted(u.unresolved), "executed_traps": u.traps.value}
                         for name, u in units.items()],
                  limitations="Private synthetic memory, production kernels and emitted primitives/cache; no live evaluator, advice, fallback error delivery or collector validation.")
    destination.write_text(json.dumps(output, indent=2) + "\n")
    print(json.dumps({k: output[k] for k in ("status", "seed_cases", "candidate_cases", "unsigned_cache_mutation")}))


if __name__ == "__main__":
    main()
