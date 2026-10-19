#!/usr/bin/env python3
"""Executable acceptance checks for Windows adjacent cold images.

Run W1..W4 under Wine; `native` measures the MSYS2 build directly. W5
validates the complete Linux suite receipt instead of rerunning that suite.
"""
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import struct
import subprocess
import sys
import time
import traceback

ROOT = Path(__file__).resolve().parents[2]
os.chdir(ROOT)
OUT = ROOT / "target/codex/round3"
OUT.mkdir(parents=True, exist_ok=True)
BIN = ROOT / "target/nelisp.exe"
ENV = dict(os.environ, WINEPREFIX=str(ROOT / "target/wine-prefix"), WINEDEBUG="-all")


def run(label, args, binary=BIN, trace=False, cwd=ROOT, native=False, codes=(0,)):
    env = dict(ENV, WINEDEBUG="+file" if trace else "-all")
    if not native:
        # The meter sets LC_ALL=C for numeric output. Wine's Unix launcher
        # needs UTF-8 to decode a non-ASCII executable path before Win32 runs.
        env.update(LC_ALL="C.UTF-8", LANG="C.UTF-8")
    command = ([] if native else ["wine"]) + [str(binary), *args]
    start = time.monotonic()
    with (OUT / (label + ".out")).open("wb") as stdout, (OUT / (label + ".err")).open("wb") as stderr:
        result = subprocess.run(command, env=env, cwd=cwd, stdout=stdout, stderr=stderr, timeout=90)
    elapsed = time.monotonic() - start
    (OUT / (label + ".json")).write_text(json.dumps(dict(command=command, rc=result.returncode, seconds=elapsed)))
    assert result.returncode in codes, (label, result.returncode)
    return (OUT / (label + ".out")).read_text(errors="replace").strip(), elapsed


def opens(label):
    # Only actual open requests, not Wine's path conversion diagnostics.
    return [line for line in (OUT / (label + ".err")).read_text(errors="replace").splitlines()
            if "trace:file:NtCreateFile" in line and "name=" in line]


def restore_trace(trace, image=Path(str(BIN) + ".cold"), explicit=False):
    paths = [line for line in trace.splitlines() if "trace:file:NtCreateFile" in line and "name=" in line]
    assert paths, "Wine trace did not record any open requests"
    companion = next((line for line in paths if '.cold"' in line), None)
    assert companion, "adjacent .cold was never opened"
    assert not any(re.search(r"\.el(?:c)?\"", line) for line in paths), "source opened during cold boot"
    thread = companion.split(":", 1)[0]
    tail = trace[trace.index(companion):]
    # Requests can succeed only partially or fail. Count completed transfers
    # from this thread, never the requested ReadFile lengths.
    reads = [int(n) for n in re.findall(re.escape(thread) + r":trace:file:NtReadFile = SUCCESS \((\d+)\)", tail)]
    with image.open("rb") as source:
        header = struct.unpack("<8Q", source.read(64))
    expected = 64 + header[1] + header[2] + 8 * header[3] + 48
    assert image.stat().st_size == expected, "incomplete companion fixture"
    # Explicit loading rereads the globals header. Auto loading retains that
    # header and performs exactly one complete file transfer.
    assert 64 in reads and 48 in reads and sum(reads) == expected + (64 if explicit else 0), "incomplete image transfer"
    assert not re.search(r"cold.*(?:invalid|reject|digest)", tail, re.I), "image restore rejected"


def trace_controls(trace, image, explicit):
    lengths = set(re.findall(r"NtReadFile = SUCCESS \((\d+)\)", trace))
    mutations = [re.sub(r"NtReadFile = SUCCESS \(\d+\)", "NtReadFile = 0xc0000011 (0)", trace)]
    mutations += [re.sub(r"NtReadFile = SUCCESS \(" + length + r"\)", "NtReadFile = 0xc0000011 (0)", trace)
                  for length in lengths]
    for broken in mutations:
        try:
            restore_trace(broken, image, explicit)
        except AssertionError:
            continue
        raise AssertionError("failed-read mutation passed the restore verifier")


def traced_add(label, binary=BIN, cwd=ROOT, explicit=False):
    assert Path(str(binary) + ".cold").is_file(), "missing companion fixture"
    args = (["--cold-load-from", str(binary) + ".cold"] if explicit else []) + ["--eval", "(+ 1 2)"]
    output, _ = run(label, args, binary, trace=True, cwd=cwd)
    assert output == "3", (label, output)
    trace = (OUT / (label + ".err")).read_text(errors="replace")
    restore_trace(trace, Path(str(binary) + ".cold"), explicit)
    # Limit controls to the tested process thread after the image open, so
    # unrelated Wine loader reads cannot create irrelevant mutations.
    companion = next(line for line in trace.splitlines() if "trace:file:NtCreateFile" in line and '.cold"' in line)
    thread = companion.split(":", 1)[0]
    restore_part = "\n".join(line for line in trace[trace.index(companion):].splitlines() if line.startswith(thread + ":"))
    trace_controls(restore_part, Path(str(binary) + ".cold"), explicit)


def rx_check(label, binary=BIN, cwd=ROOT):
    expression = '(progn (require (quote rx)) (princ (if (string-match-p (rx (+ digit)) "123") "RX-OK" "RX-FAIL")))'
    output, _ = run(label, ["--eval", expression], binary, cwd=cwd)
    assert "RX-OK" in output, (label, output)
    assert not (OUT / (label + ".err")).read_text().strip(), (label, "boot diagnostic")


def plain():
    traced_add("plain-trace")
    rx_check("plain-rx")


def timing(native=False, binary=BIN, prefix="timing"):
    assert Path(str(binary) + ".cold").is_file(), "missing timing companion"
    if not native:
        traced_add(prefix + "-plain-preflight", binary)
        traced_add(prefix + "-explicit-preflight", binary, explicit=True)
    run(prefix + "-warmup", ["--eval", "nil"], binary, native=native)
    values = {"plain": [], "explicit": []}
    for i in range(5):
        for mode in values:
            args = (["--cold-load-from", str(binary) + ".cold"] if mode == "explicit" else []) + ["--eval", "nil"]
            label = f"{prefix}-{mode}-{i}"
            output, seconds = run(label, args, binary, native=native)
            assert output == "nil", (label, output)
            assert not (OUT / (label + ".err")).read_text().strip(), (label, "boot diagnostic")
            values[mode].append(seconds)
    medians = {mode: statistics.median(samples) for mode, samples in values.items()}
    report = dict(samples=values, medians=medians, ratio=medians["plain"] / medians["explicit"])
    (OUT / (prefix + ".json")).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report))
    assert report["ratio"] <= 1.5, report
    if native:
        assert medians["plain"] <= 0.75, "plain native boot exceeded the subsecond cold-start budget"


def copy_bundle(directory, image=True):
    directory.mkdir(parents=True, exist_ok=True)
    binary = directory / BIN.name
    for suffix in ("", ".native-startup.el") + ((".cold",) if image else ()):
        shutil.copy2(str(BIN) + suffix, str(binary) + suffix)
    for dll in (ROOT / "target/wine-dlls").glob("*.dll"):
        shutil.copy2(dll, directory)
    return binary


def fallback():
    binary = copy_bundle(OUT / "fallback", image=False)
    image = Path(str(binary) + ".cold")
    image.unlink(missing_ok=True)
    assert run("missing", ["--eval", "(+ 1 2)"], binary)[0] == "3"
    # The previous lean build is a genuine foreign image, not a mocked header.
    old = OUT / "before/nelisp.exe.cold"
    assert old.is_file(), "preserve the pre-change cold image for the foreign-build control"
    shutil.copy2(old, image)
    assert run("foreign", ["--eval", "(+ 1 2)"], binary)[0] == "3"
    assert "digest" in (OUT / "foreign.err").read_text().lower(), "foreign build was not rejected"
    with Path(str(BIN) + ".cold").open("rb") as src, image.open("wb") as dst:
        dst.write(src.read(4096))
    assert run("truncated", ["--eval", "(+ 1 2)"], binary)[0] == "3"
    assert "cold" in (OUT / "truncated.err").read_text().lower(), "truncated image was not diagnosed"
    # An explicit path must win over a bad adjacent companion.
    assert run("override", ["--cold-load-from", str(BIN) + ".cold", "--eval", "(+ 1 2)"], binary)[0] == "3"
    assert not (OUT / "override.err").read_text().strip(), "adjacent image overrode the explicit path"
    # Restore the valid image: every opt-out must leave it unopened.
    shutil.copy2(str(BIN) + ".cold", image)
    commands = ["--help", "--embedded", "--neln-selftest", "dump-runtime-image",
                "extend-runtime-image", "eval-runtime-image", "exec-runtime-image",
                "compile-elisp-artifact", "compile-elisp-artifacts", "compile-runtime-image",
                "audit-elisp-artifacts", "exec-elisp-artifact", "eval-elisp-artifact",
                "load-elisp-source", "eval-elisp-source", "native-exec-elisp-artifact",
                "inspect-elisp-artifact"]
    for command in commands:
        label = "optout-" + command.lstrip("-")
        # The existing non-Linux neln self-test explicitly returns unsupported
        # (125); discovery must still opt out before reaching that stub.
        codes = (125,) if command == "--neln-selftest" else (0, 1, 2, 42)
        run(label, [command], binary, trace=True, codes=codes)
        paths = opens(label)
        assert paths, (label, "empty trace")
        assert not any(".cold\"" in line for line in paths), (label, "companion opened")
    print("missing, foreign, truncated, explicit override, and 17 opt-outs: PASS")


def relocated():
    for name in ("fresh directory with spaces", "日本語 cold image"):
        directory = OUT / name
        binary = copy_bundle(directory)
        traced_add("relocated-" + ("unicode" if "日本語" in name else "spaces"), binary, directory)
        rx_check("relocated-rx-" + ("unicode" if "日本語" in name else "spaces"), binary, directory)
    print("relocated bundle: spaces and Unicode PASS")


def linux_receipt():
    assert (OUT / "non-windows-identical.rc").read_text().strip() == "0"
    assert (OUT / "non-windows-identical.log").read_text().count(" IDENTICAL") == 6
    assert (OUT / "linux.rc").read_text().strip() == "0"
    log = (OUT / "linux.log").read_text()
    assert "GATE-COUNT: 11 cases, 0 failures" in log
    subprocess.run(["sha256sum", "-c", str(OUT / "linux.sha256")], stdout=subprocess.DEVNULL, check=True)
    print("rebuilt Linux dynamic/static: 11 groups, 0 failures; receipt current")


if __name__ == "__main__":
    actions = {"W1": plain, "W2": timing,
               "W3": fallback, "W4": relocated, "W5": linux_receipt,
               "native": lambda: timing(native=True, prefix="native-timing")}
    assert len(sys.argv) == 2 and sys.argv[1] in actions, "use W1|W2|W3|W4|W5|native"
    failure = OUT / (sys.argv[1] + ".failure.log")
    failure.unlink(missing_ok=True)
    try:
        actions[sys.argv[1]]()
    except Exception:
        # The progress meter reports only exit status. Preserve the reason
        # and a snapshot of per-command outcomes before another run replaces it.
        outcomes = {p.name: p.read_text() for p in OUT.glob("*.json") if p.stat().st_size < 8192}
        failure.write_text(traceback.format_exc() + "\n" + json.dumps(outcomes, indent=2))
        raise
