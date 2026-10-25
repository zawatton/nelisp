#!/usr/bin/env python3
"""Actual owner-frame prologue controls; full native handlers remain a build gate."""
import ctypes as C
import hashlib
import json
import mmap
import pathlib
import re
import subprocess
import sys

if not __debug__:
    raise SystemExit("Assertions required")
ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "target/transport-split"


def sha(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()


def main():
    if "--sample-only" in sys.argv:
        results = {}
        for name in ("native-controls", "native-controls-wrong-missing"):
            binary = OUT / name
            child = subprocess.run([str(binary)], capture_output=True, timeout=15)
            (OUT / (name + ".stdout")).write_bytes(child.stdout)
            (OUT / (name + ".stderr")).write_bytes(child.stderr)
            accepted = child.returncode == 0 and child.stdout == b"\0" * 8 and child.stderr == b""
            assert accepted == (name == "native-controls"), (name, child.returncode, child.stdout, child.stderr)
            results[name] = {"rc": child.returncode, "stdout_hex": child.stdout.hex(), "stderr_hex": child.stderr.hex(),
                             "sha256": sha(binary), "accepted": accepted}
        report = {"status": "GENERATED_SAMPLE_NATIVE_PASS_OWNER_PENDING", "controls": results,
                  "limits": "Generated list/frame groups with real production argument kernels, exact output/exit/stderr, missing0-to-nil3 mutation rejected. Full production owner preprocessing exceeded60s; actual lowered owner frame sizes/prologues/F1/caps/full runtime/speed are NOT qualified."}
        (OUT / "sample-native.json").write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report))
        return
    report = json.loads((OUT / "owner.json").read_text())
    for path, pin in report["files"].items():
        assert sha(ROOT / path) == pin
    memories = []
    results = []
    for entry in report["frames"]:
        if entry["name"] == "nl_apply_builtin_name_key":
            continue
        argc = entry["params"]
        assert argc in (4, 6) and entry["rt_slots"] > 8
        for selected in (False, True):
            path = OUT / (entry["name"] + (".bin" if selected else ".control.bin"))
            code = path.read_bytes()
            memory = mmap.mmap(-1, len(code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
            memory.write(code); memories.append(memory)
            fn = C.CFUNCTYPE(C.c_uint64, *([C.c_uint64] * argc))(C.addressof(C.c_char.from_buffer(memory)))
            args = [2] + list(range(11, 11 + argc - 1))
            assert fn(*args) == sum(args), (entry["name"], "argument transport")
            args[0] = 1
            assert fn(*args) == sum(args)
            args[0] = 0
            assert fn(*args) == (0 if selected else 3735928559), (entry["name"], selected, "fresh slots")
            if selected:
                pattern = bytes.fromhex("49 89 fa 49 89 cb 31 c0 48 89 e7 b9")
                at = code.index(pattern)
                loop = code[at:at + 26]
                words = int.from_bytes(loop[12:16], "little")
                assert words == entry["rt_slots"] + (entry["rt_slots"] & 1)
                shim = bytes.fromhex("49 89 e0 48 89 fc") + loop + bytes.fromhex("4c 89 c4 c3")
                mm = mmap.mmap(-1, len(shim), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
                mm.write(shim); memories.append(mm)
                clear = C.CFUNCTYPE(None, C.c_void_p)(C.addressof(C.c_char.from_buffer(mm)))
                canary = 0xAABBCCDDEEFF1122
                region = (C.c_uint64 * (words + 2))(*([canary] * (words + 2)))
                clear(C.addressof(region) + 8)
                assert region[0] == region[-1] == canary and all(x == 0 for x in region[1:-1])
        results.append({"name": entry["name"], "rt_slots": entry["rt_slots"], "zeroed_bytes": words * 8,
                        "params_and_boundary_canaries": "PASS", "uninitialized_control": "RED"})
    assert len(results) == 6
    binary = OUT / "native-controls"
    child = subprocess.run([str(binary)], capture_output=True, timeout=15)
    (OUT / "native-controls.stdout").write_bytes(child.stdout)
    (OUT / "native-controls.stderr").write_bytes(child.stderr)
    assert child.returncode == 0 and child.stdout == b"\0" * 8 and child.stderr == b""
    for path, pin in report["files"].items():
        assert sha(ROOT / path) == pin
    result = {"status": "FOCUSED_NATIVE_PASS", "owner_sha256": sha(OUT / "owner.json"), "entries": results,
              "native_binary_sha256": sha(binary), "native_control_stdout_sha256": hashlib.sha256(child.stdout).hexdigest(),
              "limits": "Actual six applyfn-owner RT regions/prologues with substituted witness bodies, plus generated sample groups with real argument kernels. Seventh unchanged EIC selector is checked by host tests; final handler/F1/caps/GC/wall acceptance remains pending."}
    (OUT / "native.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({"status": result["status"], "entries": results}))


if __name__ == "__main__": main()
