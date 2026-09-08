#!/usr/bin/env python3
"""Compare standalone T91 decimal conversion against Python binary64 bits.

The standalone reader is built twice because a single 64-bit Lisp integer is
not a reliable transport value in this runtime.  Each run returns one u32
word, and this script joins the words only after reading the process output.
"""

from __future__ import annotations

import os
import random
import re
import struct
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILD_RECIPE = ROOT / "test" / "nelisp-t91-float-oracle-build.el"
TARGET = ROOT / "target"
PROBE = TARGET / "nelisp-t91-float-oracle-probe.el"
BINARY = TARGET / "nelisp"


def corpus() -> list[str]:
    values = [
        "0.0",
        "-0.0",
        "1.0",
        "1.5",
        "0.1",
        "5e-324",
        "2.2250738585072014e-308",
        "1e308",
        "1e309",
        "1e-400",
        # Exact halfway cases for ties-to-even in normal and subnormal ranges.
        "1.00000000000000011102230246251565404236316680908203125",
        "1.000000000000000333066907387546962127685546875",
        "2.47032822920623272088284396434110686182529901307162382212792841250337753635104375932649918180817996189898282347722858865e-324",
        "9" * 100 + "e-100",
        "1" * 1000 + "e-1000",
    ]
    random.seed(91)
    for digits in (100, 1000):
        for _ in range(4):
            mantissa = "".join(str(random.randrange(10)) for _ in range(digits))
            mantissa = mantissa.lstrip("0") or "1"
            split = random.randrange(1, len(mantissa))
            exponent = random.randrange(-308, 309)
            values.append(f"{mantissa[:split]}.{mantissa[split:]}e{exponent}")
    return values


def bits(literal: str) -> tuple[int, int]:
    raw = struct.unpack(">Q", struct.pack(">d", float(literal)))[0]
    return raw >> 32, raw & 0xFFFFFFFF


def run_checked(command: list[str], *, env: dict[str, str] | None = None) -> str:
    result = subprocess.run(command, cwd=ROOT, env=env, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode:
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit(f"command failed ({result.returncode}): {' '.join(command)}")
    return result.stdout.strip()


def restore_production_binary(emacs: str) -> None:
    """Put target/nelisp back into its normal production-reader state."""
    env = os.environ.copy()
    env.pop("NELISP_T91_WORD", None)
    clean = subprocess.run(
        ["make", "standalone-eval-clean"], cwd=ROOT, env=env,
        text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    )
    build = subprocess.run(
        ["make", "standalone-reader"], cwd=ROOT, env=env,
        text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    ) if clean.returncode == 0 else None
    if clean.returncode or build is None or build.returncode:
        for result in (clean, build):
            if result is not None:
                sys.stderr.write(result.stdout)
                sys.stderr.write(result.stderr)
        raise SystemExit("failed to restore production standalone reader")


def parse_words(output: str, count: int) -> list[int]:
    match = re.fullmatch(r"\((?:\s*-?\d+\s*)*\)", output)
    if not match:
        raise SystemExit(f"unexpected standalone output: {output!r}")
    words = [int(word) for word in re.findall(r"-?\d+", output)]
    if len(words) != count or any(word < 0 or word > 0xFFFFFFFF for word in words):
        raise SystemExit(f"invalid u32 output ({len(words)} words): {output!r}")
    return words


def main() -> int:
    values = corpus()
    TARGET.mkdir(parents=True, exist_ok=True)
    PROBE.write_text("(list " + " ".join(values) + ")\n", encoding="utf-8")
    emacs = os.environ.get("EMACS", "emacs")
    common = [emacs, "--batch", "-Q", "-L", "lisp", "-L", "src", "-L", "scripts",
              "-l", str(BUILD_RECIPE.relative_to(ROOT))]
    try:
        outputs: dict[str, list[int]] = {}
        for word in ("hi", "lo"):
            env = os.environ.copy()
            env["NELISP_T91_WORD"] = word
            run_checked(common, env=env)
            outputs[word] = parse_words(
                run_checked([str(BINARY), "--load", str(PROBE.relative_to(ROOT))]),
                len(values),
            )
        actual = list(zip(outputs["hi"], outputs["lo"]))
        expected = [bits(value) for value in values]
        mismatches = [(i, value, got, want)
                      for i, (value, got, want) in enumerate(zip(values, actual, expected))
                      if got != want]
        if mismatches:
            for mismatch in mismatches[:10]:
                print("MISMATCH", mismatch, file=sys.stderr)
            return 1
        PROBE.write_text("(list 1e 1e+ 1e- 1e2e3 1..2)\n", encoding="utf-8")
        malformed = subprocess.run(
            [str(BINARY), "--load", str(PROBE.relative_to(ROOT))],
            cwd=ROOT, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        )
        if malformed.returncode == 0:
            print("MISMATCH malformed literals were accepted", file=sys.stderr)
            return 1
        print(f"T91 oracle: {len(values)} literals, hi/lo u32 mismatches=0")
        print("cases: signed-zero, normal, subnormal, normal/subnormal ties, overflow, underflow, malformed, 100/1000-digit, random")
        return 0
    finally:
        restore_production_binary(emacs)


if __name__ == "__main__":
    raise SystemExit(main())
