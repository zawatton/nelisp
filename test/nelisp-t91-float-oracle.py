#!/usr/bin/env python3
"""Compare standalone T91 decimal conversion against Python binary64 bits.

The standalone reader is built twice because a single 64-bit Lisp integer is
not a reliable transport value in this runtime.  Each run returns one u32
word, and this script joins the words only after reading the process output.
"""

from __future__ import annotations

import errno
import os
import platform
import random
import re
import shutil
import struct
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILD_RECIPE = ROOT / "test" / "nelisp-t91-float-oracle-build.el"
TARGET = ROOT / "target"


class TargetNotRunnable(RuntimeError):
    """The selected standalone target cannot execute on this host."""


def target_image_execution_error(exc: OSError) -> bool:
    """Return whether EXC means an image has the wrong execution format."""
    return exc.errno in {errno.ENOEXEC, errno.EACCES} or getattr(
        exc, "winerror", None
    ) == 193  # ERROR_BAD_EXE_FORMAT


def detected_host_target() -> str:
    """Return the native standalone target for the current host."""
    system = platform.system().lower()
    machine = platform.machine().lower()
    if machine in {"aarch64", "arm64"}:
        arch = "aarch64"
    elif machine in {"x86_64", "amd64", "x64"}:
        arch = "x86_64"
    else:
        raise SystemExit(
            "T91 oracle: unsupported host architecture "
            f"{platform.system()}/{platform.machine()}"
        )

    if system == "darwin":
        return f"macos-{arch}"
    if system == "windows":
        return f"windows-{arch}"
    if system == "linux":
        return f"linux-{arch}"
    raise SystemExit(
        f"T91 oracle: unsupported host platform {platform.system()}/{platform.machine()}"
    )


def host_default_target() -> str:
    """Return the explicit target, or the native target when unset."""
    return os.environ.get("NELISP_STANDALONE_TARGET") or detected_host_target()


def binary_for_target(target: str) -> Path:
    """Return the standalone-reader output path for TARGET."""
    names = {
        "linux-x86_64": "nelisp",
        "linux-aarch64": "nelisp-aarch64",
        "macos-aarch64": "nelisp",
        "macos-x86_64": "nelisp",
        "windows-x86_64": "nelisp.exe",
        "windows-aarch64": "nelisp-aarch64.exe",
    }
    try:
        return TARGET / names[target]
    except KeyError as exc:
        raise SystemExit(f"T91 oracle: unsupported standalone target {target!r}") from exc


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


def run_target(binary: Path, probe: Path, *, env: dict[str, str], skip_image_error: bool) -> str:
    """Run the selected reader, classifying host execution failures as skips."""
    try:
        result = subprocess.run(
            [str(binary), "--load", str(probe.relative_to(ROOT))],
            cwd=ROOT,
            env=env,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except OSError as exc:
        if skip_image_error and target_image_execution_error(exc):
            raise TargetNotRunnable(
                f"target {binary} is not executable on this host: {exc.strerror}"
            ) from exc
        raise SystemExit(
            f"target {binary} could not be executed: {exc.strerror}"
        ) from exc
    if result.returncode:
        sys.stderr.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit(
            f"command failed ({result.returncode}): {binary} --load {probe.relative_to(ROOT)}"
        )
    return result.stdout.strip()


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
    target = host_default_target()
    native_target = detected_host_target()
    skip_image_error = target != native_target
    TARGET.mkdir(parents=True, exist_ok=True)
    scratch = Path(tempfile.mkdtemp(prefix="nelisp-t91-oracle-", dir=TARGET))
    binary = scratch / binary_for_target(target).name
    probe = scratch / "probe.el"
    cache = scratch / "units"
    artifact_source = scratch / "artifact-runtime.el"
    artifact_cache = scratch / "artifact-runtime.el.nelc"
    artifact_enable = scratch / "artifact-runtime.el.nelc.enable"
    probe.write_text("(list " + " ".join(values) + ")\n", encoding="utf-8")
    emacs = os.environ.get("EMACS", "emacs")
    common = [emacs, "--batch", "-Q", "-L", "lisp", "-L", "src", "-L", "scripts",
              "-l", str(BUILD_RECIPE.relative_to(ROOT))]
    try:
        outputs: dict[str, list[int]] = {}
        for word in ("hi", "lo"):
            env = os.environ.copy()
            env["NELISP_T91_WORD"] = word
            env["NELISP_STANDALONE_TARGET"] = target
            env["NELISP_STANDALONE_OUTPUT"] = str(binary)
            env["NELISP_STANDALONE_CACHE_DIR"] = str(cache)
            env["NELISP_T91_ARTIFACT_SOURCE"] = str(artifact_source)
            env["NELISP_T91_ARTIFACT_CACHE"] = str(artifact_cache)
            env["NELISP_T91_ARTIFACT_ENABLE"] = str(artifact_enable)
            run_checked(common, env=env)
            outputs[word] = parse_words(
                run_target(binary, probe, env=env,
                           skip_image_error=skip_image_error),
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
        probe.write_text("(list 1e 1e+ 1e- 1e2e3 1..2)\n", encoding="utf-8")
        try:
            malformed = subprocess.run(
                [str(binary), "--load", str(probe)],
                cwd=ROOT, env={**os.environ, "NELISP_STANDALONE_TARGET": target},
                text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            )
        except OSError as exc:
            if skip_image_error and target_image_execution_error(exc):
                raise TargetNotRunnable(
                    f"target {binary} is not executable on this host: {exc.strerror}"
                ) from exc
            raise SystemExit(
                f"target {binary} could not be executed: {exc.strerror}"
            ) from exc
        if malformed.returncode == 0:
            print("MISMATCH malformed literals were accepted", file=sys.stderr)
            return 1
        print(f"T91 oracle: target={target} binary={binary}")
        print(f"T91 oracle: {len(values)} literals, hi/lo u32 mismatches=0")
        print("cases: signed-zero, normal, subnormal, normal/subnormal ties, overflow, underflow, malformed, 100/1000-digit, random")
        return 0
    except TargetNotRunnable as exc:
        print(f"T91 oracle: SKIP target={target} binary={binary}: {exc}")
        return 77
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


if __name__ == "__main__":
    raise SystemExit(main())
