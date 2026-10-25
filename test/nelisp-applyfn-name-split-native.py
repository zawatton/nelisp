#!/usr/bin/env python3
"""Qualify the production-emitted name resolver without a runtime rebuild."""
import argparse
import ctypes
import hashlib
import json
import mmap
import pathlib
import re
import subprocess
import sys
import tempfile

if not __debug__:
    raise SystemExit("Python optimization disables qualification assertions")

ROOT = pathlib.Path(__file__).resolve().parents[1]


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def check_instructions(path):
    completed = subprocess.run(
        ["objdump", "-D", "-w", "-b", "binary", "-m", "i386:x86-64", str(path)],
        capture_output=True, text=True, timeout=15, check=True)
    assert not completed.stderr, completed.stderr
    count = 0
    for line in completed.stdout.splitlines():
        if not re.match(r"\s*[0-9a-f]+:", line):
            continue
        instruction = line.split("\t")[-1].strip()
        mnemonic, _, operand = instruction.partition(" ")
        assert mnemonic not in {"call", "callq", "syscall", "sysenter", "int", "int3", "ud2", "hlt", "(bad)", ".byte"}, instruction
        if mnemonic.startswith("j"):
            assert "*" not in operand, instruction
            target = int(operand.strip().split()[0], 16)
            assert 0 <= target < path.stat().st_size, instruction
        count += 1
    assert count > 0
    return count


def validate(report, directory):
    assert report["status"] == "MECHANICAL_ONLY"
    assert report["outer_initialized"] is True
    assert report["outer_calls"] == 1
    assert report["resolver_relocations"] == 0
    frames = {(r["phase"], r["name"]): r["rt_slots"] for r in report["frames"]}
    old = frames["monolithic", "nelisp_apply_function_w"]
    new = frames["split", "nelisp_apply_function_w"]
    assert 0 < new < old, (old, new)
    assert frames["split", "nl_apply_builtin_name_key"] > 0
    assert report["outer_group_calls"] == 2
    assert {g["name"] for g in report["groups"]} == {"nl_apply_builtin_group_0_w", "nl_apply_builtin_group_1_w"}
    for group in report["groups"]:
        assert group["params"] == 6 and group["rt_slots"] > 0
        for name, expected in group["files"].items():
            assert digest(ROOT / name) == expected, name
    allowed = {"imm", "ref", "arith", "cmp", "logic", "if", "shift", "ptr-read-u8", "ptr-read-u64", "let-rt"}
    assert set(report["resolver_ir_counts"]) <= allowed
    assert digest(directory / "resolver.bin") == report["resolver_sha256"]
    assert digest(directory / "dispatcher-prologue.bin") == report["dispatcher_sha256"]
    assert report["source_after_sha256"] == report["source_sha256"]
    assert set(report["modified_tracked_el"]) <= set(report["source_sha256"])
    assert report["base_head"] == subprocess.check_output(
        ["git", "-C", str(ROOT), "rev-parse", "HEAD"], text=True).strip()
    for path, expected in report["test_sha256"].items():
        assert digest(ROOT / path) == expected, path
    for path, expected in report["source_sha256"].items():
        assert digest(ROOT / path) == expected, path
    return old, new


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", type=pathlib.Path, required=True)
    args = parser.parse_args()
    path = args.check.resolve()
    pinned = digest(path)
    report = json.loads(path.read_text())
    directory = path.parent
    old, new = validate(report, directory)
    instruction_count = check_instructions(directory / "resolver.bin")
    code = (directory / "resolver.bin").read_bytes()
    # A zero-relocation local/indirect call or trap is also forbidden.
    rejected = []
    with tempfile.TemporaryDirectory(prefix="nelisp-name-resolver-") as scratch:
        for name, mutation in (("direct-call", b"\xe8\x00\x00\x00\x00"),
                               ("indirect-jump", b"\xff\xe0"),
                               ("syscall", b"\x0f\x05")):
            bad = pathlib.Path(scratch) / (name + ".bin")
            bad.write_bytes(code + mutation)
            try:
                check_instructions(bad)
            except (AssertionError, ValueError):
                rejected.append(name)
            else:
                raise AssertionError("instruction control accepted: " + name)
    assert len(rejected) == 3
    memory = mmap.mmap(-1, len(code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
    memory.write(code)
    function = ctypes.CFUNCTYPE(ctypes.c_uint64, ctypes.c_uint64, ctypes.c_uint64)(
        ctypes.addressof(ctypes.c_char.from_buffer(memory)))
    cases = 0
    group_results = []
    executable_memories = [memory]
    for group in report["groups"]:
        name = group["name"]
        functions = {}
        for variant in ("control", "initialized"):
            data = (directory / (name + "-" + variant + ".bin")).read_bytes()
            chunk = mmap.mmap(-1, len(data), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
            chunk.write(data)
            executable_memories.append(chunk)
            functions[variant] = ctypes.CFUNCTYPE(ctypes.c_uint64, *([ctypes.c_uint64] * 6))(
                ctypes.addressof(ctypes.c_char.from_buffer(chunk)))
        for variant, entry in functions.items():
            assert entry(2, 11, 22, 33, 44, 55) == 167, (name, variant, "six argument corruption")
            assert entry(1, 11, 22, 33, 44, 55) == 165
            assert entry(0, 11, 22, 33, 44, 55) == (3735928559 if variant == "control" else 0), (name, variant, "fresh slots")
        words = group["rt_slots"] + (group["rt_slots"] & 1)
        loop = (directory / (name + "-loop.bin")).read_bytes()
        shim_code = bytes.fromhex("49 89 e0 48 89 fc") + loop + bytes.fromhex("4c 89 c4 c3")
        shim_memory = mmap.mmap(-1, len(shim_code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
        shim_memory.write(shim_code)
        executable_memories.append(shim_memory)
        shim = ctypes.CFUNCTYPE(None, ctypes.c_void_p)(ctypes.addressof(ctypes.c_char.from_buffer(shim_memory)))
        canary = 0xAABBCCDDEEFF1122
        region = (ctypes.c_uint64 * (words + 2))(*([canary] * (words + 2)))
        shim(ctypes.addressof(region) + 8)
        assert region[0] == region[-1] == canary, (name, "boundary overwrite")
        assert all(value == 0 for value in region[1:-1]), (name, "uncleared fresh slot")
        group_results.append({"name": name, "rt_slots": group["rt_slots"], "zeroed_bytes": words * 8,
                              "six_args": "PASS", "boundary_canaries": "PASS", "uninitialized_control": "RED"})

    def run(name, expected):
        nonlocal cases
        encoded = name.encode("utf-8")
        data = ctypes.create_string_buffer(encoded)
        alive = [data]

        def block(size, alignment=8):
            buffer = ctypes.create_string_buffer(size + alignment)
            alive.append(buffer)
            return (ctypes.addressof(buffer) + alignment - 1) & -alignment

        def put(address, offset, value):
            ctypes.c_uint64.from_address(address + offset).value = value

        representations = []
        for tag in (4, 5, 14):
            view = block(32)
            put(view, 0, tag)
            put(view, 16, ctypes.addressof(data))
            put(view, 24, len(encoded))
            representations.append(("legacy-" + str(tag), view))
        for unibyte in (False, True):
            header = block(32, 32)
            put(header, 0, len(encoded) | (1 << 63 if unibyte else 0))
            put(header, 8, (1 << 64) - 1 if unibyte else len(encoded))
            put(header, 24, ctypes.addressof(data))
            representations.append(("gnu-" + str(unibyte), header + 4))
        for tag in (4, 16):
            for source, word in tuple(representations[:5]):
                symbol = block(48)
                put(symbol, 0, tag)
                put(symbol, 8, word)
                representations.append(("symbol-" + str(tag) + "-" + source, symbol + 6))
        for representation, word in representations:
            result = function(word, len(encoded))
            assert result == expected, (name, representation, expected, result)
            assert function(word, len(encoded) + 1) == 0, (name, representation, "wrong length")
            cases += 2
        invalid = block(32)
        put(invalid, 0, 9)
        put(invalid, 16, ctypes.addressof(data))
        put(invalid, 24, len(encoded))
        assert function(invalid, len(encoded)) == 0
        cases += 1

    names = report["names"]
    assert len(names) > 100
    assert len({r["name"] for r in names}) == len(names)
    for row in names:
        run(row["name"], row["key"])
    for name in ("", "not-a-runtime-builtin", "unknown-λ-name", "x" * 180):
        assert name not in {r["name"] for r in names}
        run(name, 0)
    assert digest(path) == pinned
    validate(report, directory)
    result = {"status": "PASS", "qualification_sha256": pinned,
              "runner_sha256": digest(pathlib.Path(__file__).resolve()),
              "test_sha256": digest(ROOT / "test/nelisp-applyfn-name-split-test.el"),
              "names": len(names), "native_checks": cases,
              "decoded_instructions": instruction_count,
              "instruction_mutations_rejected": rejected,
              "monolithic_rt_slots": old, "split_rt_slots": new,
              "groups": group_results,
              "route_zeroed_bytes": [8 * (new + (new & 1)) + g["zeroed_bytes"] for g in group_results],
              "scope": "mechanical/native resolver qualification; full runtime GC and wall-speed gates remain required"}
    (directory / "native-result.json").write_text(json.dumps(result, sort_keys=True, indent=2) + "\n")
    print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    main()
