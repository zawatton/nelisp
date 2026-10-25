#!/usr/bin/env python3
"""Exercise real emitted FX kernels; reject the seed with identical assertions.

Emit with fxvm-emit first. This private-memory check does not replace live
runtime/collector qualification. Unresolved dependencies are native traps.
"""
import importlib.util
import json
from pathlib import Path
import sys
import time

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("fx_vector", ROOT / "test/nelisp-fx-ordinary-vector-native.py")
base = importlib.util.module_from_spec(spec)
spec.loader.exec_module(base)
MASK = (1 << 64) - 1
if not __debug__:
    raise SystemExit("Assertions must be enabled")


def exercise(unit, arithmetic=True, fresh=True):
    count = 0
    primitive_id = unit.function("m5_fx_primitive_id", 2)
    run = unit.function("m5_fx_primitive_run", 7)
    stack, result = unit.block(32), unit.block(8)
    if arithmetic:
        for name, kind in (("%", -17), ("logand", -18)):
            for argc in range(5):
                assert primitive_id(unit.name(name), argc) == (kind if argc == 2 else 0)
                count += 1
        for x in (-(1 << 61), -17, -1, 0, 1, 17, (1 << 61) - 1):
            for y in (-17, -1, 1, 17):
                for kind in (-17, -18):
                    unit.put(stack, 0, x * 4 + 1)
                    unit.put(stack, 8, y * 4 + 1)
                    assert run(kind & MASK, stack, 2, 2, result, unit.env, 0) == 0
                    remainder = abs(x) % abs(y) * (-1 if x < 0 else 1)
                    value = remainder if kind == -17 else x & y
                    assert unit.get(result) == (value * 4 + 1) & MASK, (kind, x, y)
                    count += 1
        nonfix = unit.block(32)
        for kind in (-17, -18):
            for a, b in ((nonfix, 5), (5, nonfix), (3, 5), (7, 5)):
                unit.put(stack, 0, a)
                unit.put(stack, 8, b)
                unit.put(result, 0, 999)
                assert run(kind & MASK, stack, 2, 2, result, unit.env, 0) == -3
                assert unit.get(result) == 999
                count += 1
            for argc in (0, 1, 3, 4):
                unit.put(result, 0, 999)
                assert run(kind & MASK, stack, argc, argc, result, unit.env, 0) == -3
                assert unit.get(result) == 999
                count += 1
        unit.put(stack, 0, 21)
        unit.put(stack, 8, 1)
        unit.put(result, 0, 999)
        assert run((-17) & MASK, stack, 2, 2, result, unit.env, 0) == -3
        assert unit.get(result) == 999
        count += 1
    if fresh:
        body = unit.function("m5_fx_run_body", 8)
        code, vector = unit.vector(4), unit.vector(4)
        for i, w in enumerate((9, 1, 1, 5)):
            unit.put(code - 6, 8 + 8 * i, w)
        view = unit.block(32)
        unit.put(view, 0, 8)
        unit.put(view, 8, vector - 6)
        argv, frame = unit.block(8), unit.block(128)
        unit.put(argv, 0, view)
        unit.put(unit.env, 200, frame)
        args = unit.cons(view, 3)
        for argw in (argv, 0):
            assert body(code, 1, unit.env, 0, args, 1, argw, result) == -1
            # The bounded entry-normalization fix was withdrawn on weak-GC
            # regression. Preserve an executable cause reproducer.
            assert unit.get(result) == view, "fresh-view investigation drifted"
            count += 1
    assert unit.traps.value == 0, "An unresolved dependency executed"
    return count


def main():
    started = time.monotonic()
    directory = ROOT / "target/fxvm/native"
    emission = json.loads((directory / "emission.json").read_text())
    units = {r["variant"]: base.Unit(r, directory) for r in emission["units"]}
    red = []
    for name, args in (("arithmetic", (True, False)),):
        try:
            exercise(units["seed"], *args)
        except AssertionError as error:
            red.append({"case": name, "result": "REJECTED", "reason": str(error)})
        else:
            raise AssertionError("The seed unexpectedly passed " + name)
    count = exercise(units["candidate"])
    report = {"status": "ISOLATED_NATIVE_PASS", "cases": count, "seed_negative_controls": red,
              "execution_seconds": time.monotonic() - started,
              "emission_seconds": emission.get("emission_seconds"),
              "fresh_vector_status": "CAUSE_REPRODUCED_UNFIXED",
              "emission_sha256": base.digest(directory / "emission.json"),
              "units": [{"variant": name, "sha256": unit.report["sha256"],
                         "executed_traps": unit.traps.value} for name, unit in units.items()],
              "limitations": "Synthetic memory/root allocation; live runtime fixtures qualify error delivery and GC separately."}
    (directory / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report))


if __name__ == "__main__":
    main()
