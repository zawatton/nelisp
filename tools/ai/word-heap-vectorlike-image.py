#!/usr/bin/env python3
"""Hash-bound S4 typed heap-image round trip and fresh-process replay.

run --dynamic BIN --static BIN --output DIR  creates both custom images
verify --output DIR                         rechecks hashes and complete runs
replay --output DIR                         reruns both saved-image restores

Every NeLisp run uses the actual candidate path and its authenticated cold
companion. The stream writer supplies its own build-digest trailer. A flipped
trailer digest is rejected before restoration; an optional S1 image checks
foreign-build rejection through the same public loader.
"""
from __future__ import annotations
import argparse
import hashlib
import json
import os
import shutil
from pathlib import Path
import struct
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "tools/ai/word-heap-vectorlike-image.el"
CHECKS = 32
DIAG = "nelisp: cold-load rejected (build digest mismatch)\n"


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def rel(path):
    path = Path(path).resolve()
    try:
        return path.relative_to(ROOT).as_posix()
    except ValueError:
        return str(path)


def item(path):
    path = Path(path).resolve(strict=True)
    return {"path": rel(path), "sha256": sha(path), "bytes": path.stat().st_size}


def check_item(value):
    path = ROOT / value["path"]
    need(path.is_file() and sha(path) == value["sha256"] and
         path.stat().st_size == value["bytes"], "Changed evidence: " + str(path))
    return path


def trailer(path):
    with Path(path).open("rb") as stream:
        header = stream.read(64)
        need(len(header) == 64, "Truncated flat image header")
        magic, span, intern, count, *_ = struct.unpack("<8Q", header)
        need(magic == 1179407692, "Wrong flat image magic")
        need(span > 0 and intern <= 67108864 and count > 0, "Invalid image sizes")
        need(64 + count * 8 + span + intern + 48 == Path(path).stat().st_size,
             "Image regions/trailer do not cover the exact file")
        direct = 0
        for _ in range(count):
            entry, = struct.unpack("<Q", stream.read(8))
            tag = entry & 7
            # Doc 216: 2 is a direct cons field (S2.1), 6 a vectorlike (S4.1).
            need(tag in (0, 2, 6) and (entry - tag) + 8 <= span,
                 "Invalid tagged relocation field")
            direct += tag == 6
        stream.seek(64 + count * 8 + span + intern)
        raw = stream.read(48)
    magic, version = struct.unpack("<2Q", raw[:16])
    need(magic == 1195655758 and version in (1, 2) and any(raw[16:]),
         "Missing or unstamped build-digest trailer")
    return {"span": span, "intern_bytes": intern, "relocations": count,
            "tag6_relocations": direct, "build_digest": raw[16:].hex()}


def execute(directory, name, command, mode, image=None, stderr=""):
    # Like the cold builder, retain only the task's environment in heap dumps.
    env = {"PATH": os.environ.get("PATH", os.defpath), "LANG": "C.UTF-8",
           "LC_ALL": "C.UTF-8", "TMPDIR": str(directory),
           "XDG_CACHE_HOME": str(directory / "cache"), "WHV_IMAGE_MODE": mode}
    if image is not None:
        env["WHV_IMAGE"] = str(image)
    before = time.perf_counter()
    result = subprocess.run([str(arg) for arg in command], cwd=ROOT, env=env,
                            capture_output=True, timeout=180)
    (directory / (name + ".out")).write_bytes(result.stdout)
    (directory / (name + ".err")).write_bytes(result.stderr)
    (directory / (name + ".rc")).write_text(str(result.returncode) + "\n")
    marker = f"WORD-HEAP-VECTORLIKE-IMAGE {mode.upper()} checks={CHECKS} PASS\n"
    if mode == "reject":
        marker = "WORD-HEAP-VECTORLIKE-IMAGE REJECT PASS\n"
    row = {"command": [str(arg) for arg in command], "mode": mode,
           "exit": result.returncode, "seconds": time.perf_counter() - before,
           "stdout": item(directory / (name + ".out")),
           "stderr": item(directory / (name + ".err")),
           "status": item(directory / (name + ".rc"))}
    need(result.returncode == 0, f"{name}: exit {result.returncode}; see {directory}")
    need(result.stdout.decode() == marker, f"{name}: missing or extra stdout; see {directory}")
    need(result.stderr.decode() == stderr, f"{name}: unexpected stderr; see {directory}")
    return row


def replay_one(directory, label, row):
    binary = check_item(row["artifacts"][""])
    for artifact in row["artifacts"].values():
        check_item(artifact)
    image = check_item(row["image"])
    return execute(directory, label + "-replay",
                   [binary, "--cold-load-from", image, "--load", SOURCE], "restore")


def verify_report(directory, document):
    need(document.get("schema") == 1 and document.get("checks") == CHECKS,
         "Wrong image evidence schema/count")
    check_item(document["source"])
    check_item(document["driver"])
    check_item(document["runtime_source"])
    for label, row in document["linkages"].items():
        need({"dump", "restore", "reject_digest"}.issubset(row["runs"]) and
             set(row["runs"]).issubset({"dump", "restore", "reject_digest", "reject_foreign"}),
             "Missing or unknown image phases: " + label)
        for artifact in row["artifacts"].values():
            check_item(artifact)
        image = check_item(row["image"])
        need(trailer(image) == row["layout"], "Changed image layout: " + label)
        need(row["layout"]["tag6_relocations"] > 0, "No compact image edges: " + label)
        for phase, run in row["runs"].items():
            stdout = check_item(run["stdout"]).read_text()
            stderr = check_item(run["stderr"]).read_text()
            code = check_item(run["status"]).read_text()
            marker = ("WORD-HEAP-VECTORLIKE-IMAGE REJECT PASS\n" if phase.startswith("reject")
                      else f"WORD-HEAP-VECTORLIKE-IMAGE {run['mode'].upper()} checks={CHECKS} PASS\n")
            need(code == "0\n" and run["exit"] == 0 and stdout == marker,
                 "Incomplete image run: " + label + "/" + phase)
            need(stderr == (DIAG if phase.startswith("reject") else ""),
                 "Unexpected stderr: " + label + "/" + phase)
        check_item(row["rejected_image"])
        if "foreign_image" in row:
            check_item(row["foreign_image"])
    gnu = document["gnu"]
    check_item(document["gnu_binary"])
    version = check_item(document["gnu_version"]).read_text()
    need(version.startswith("GNU Emacs 31.1"), "Wrong GNU graph reference")
    for key in ("stdout", "stderr", "status"):
        check_item(gnu[key])
    need(check_item(gnu["stdout"]).read_text() ==
         f"WORD-HEAP-VECTORLIKE-IMAGE GNU checks={CHECKS} PASS\n" and
         check_item(gnu["stderr"]).read_text() == "" and gnu["exit"] == 0 and
         check_item(gnu["status"]).read_text() == "0\n",
         "GNU graph comparison incomplete")
    need(set(document["linkages"]) == {"dynamic", "static"}, "Both linkages are required")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("run", "verify", "replay"))
    parser.add_argument("--dynamic", type=Path)
    parser.add_argument("--static", type=Path)
    parser.add_argument("--foreign-image", type=Path)
    parser.add_argument("--gnu", type=Path, default=Path(shutil.which("emacs") or "emacs"))
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    directory = args.output.resolve()
    directory.mkdir(parents=True, exist_ok=True)
    report = directory / "report.json"
    if args.mode != "run":
        document = json.loads(report.read_text())
        verify_report(directory, document)
        if args.mode == "replay":
            replay = {label: replay_one(directory, label, row)
                      for label, row in document["linkages"].items()}
            verify_report(directory, document)
            (directory / "replay.json").write_text(json.dumps(replay, indent=2) + "\n")
        print("WORD-HEAP-VECTORLIKE-IMAGE " + args.mode.upper() + " checks=32 linkages=2 PASS")
        return
    need(args.dynamic and args.static, "--dynamic and --static are required for run")
    # A failed fresh run must not leave a prior complete manifest at this path.
    report.unlink(missing_ok=True)
    document = {"schema": 1, "checks": CHECKS, "source": item(SOURCE),
                "driver": item(Path(__file__)),
                "runtime_source": item(ROOT / "scripts/nelisp-standalone-build.el"),
                "linkages": {}}
    gnu_binary = args.gnu.resolve(strict=True)
    document["gnu_binary"] = item(gnu_binary)
    version = subprocess.run([str(gnu_binary), "--version"], capture_output=True, check=True)
    need(not version.stderr and version.stdout.startswith(b"GNU Emacs 31.1"),
         "GNU Emacs 31.1 is required")
    (directory / "gnu-version.out").write_bytes(version.stdout)
    document["gnu_version"] = item(directory / "gnu-version.out")
    document["gnu"] = execute(directory, "gnu", [gnu_binary, "-Q", "--batch", "-l", SOURCE], "gnu")
    reject = '(progn (princ "WORD-HEAP-VECTORLIKE-IMAGE REJECT PASS\\n") (kill-emacs 0))'
    for label, value in (("dynamic", args.dynamic), ("static", args.static)):
        binary = value.resolve(strict=True)
        artifacts = {suffix: item(Path(str(binary) + suffix))
                     for suffix in ("", ".cold", ".native-startup.el")}
        companion = trailer(Path(str(binary) + ".cold"))
        image = directory / (label + ".cold")
        runs = {}
        runs["dump"] = execute(directory, label + "-dump", [binary, "--load", SOURCE], "dump", image)
        layout = trailer(image)
        need(layout["build_digest"] == companion["build_digest"], "Image build digest differs from candidate")
        runs["restore"] = execute(directory, label + "-restore",
                                  [binary, "--cold-load-from", image, "--load", SOURCE], "restore")
        rejected = directory / (label + "-bad-digest.cold")
        shutil.copyfile(image, rejected)
        with rejected.open("r+b") as stream:
            stream.seek(-1, 2)
            byte = stream.read(1)
            stream.seek(-1, 2)
            stream.write(bytes((byte[0] ^ 1,)))
        runs["reject_digest"] = execute(directory, label + "-reject-digest",
                                        [binary, "--cold-load-from", rejected, "--eval", reject],
                                        "reject", stderr=DIAG)
        row = {"artifacts": artifacts, "image": item(image), "layout": layout,
               "rejected_image": item(rejected), "runs": runs}
        if args.foreign_image:
            row["foreign_image"] = item(args.foreign_image)
            runs["reject_foreign"] = execute(directory, label + "-reject-foreign",
                                             [binary, "--cold-load-from", args.foreign_image.resolve(),
                                              "--eval", reject], "reject", stderr=DIAG)
        for suffix, artifact in artifacts.items():
            check_item(artifact)
        document["linkages"][label] = row
        report.write_text(json.dumps(document, indent=2) + "\n")
    verify_report(directory, document)
    print("WORD-HEAP-VECTORLIKE-IMAGE RUN checks=32 linkages=2 PASS " + rel(report))


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.TimeoutExpired) as error:
        print("word-heap-vectorlike-image: " + str(error), file=sys.stderr)
        sys.exit(1)
