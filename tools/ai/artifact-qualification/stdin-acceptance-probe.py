#!/usr/bin/env python3
"""Strict stdin isolation probe for one sealed-runner unit directory."""

import argparse
import hashlib
import importlib.util
import json
import os
import sys
from pathlib import Path


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--unit-dir", required=True)
    unit = Path(parser.parse_args().unit_dir).resolve(strict=True)
    fixture = unit / "fixtures/test_sealed_runner.py"
    if not fixture.is_file() or not (unit / "sealed_runner.py").is_file():
        parser.error("--unit-dir must contain sealed_runner.py and fixtures/test_sealed_runner.py")
    sys.path.insert(0, str(unit))
    spec = importlib.util.spec_from_file_location("strict_stdin_fixture", fixture)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    from sealed_runner import run_bundle

    case = module.SealedRunnerTests()
    case.setUp()
    reader = writer = saved_stdin = None
    result = None
    remaining = b""
    try:
        case._write_manifest(
            'if IFS= read -r line; then printf "HOST:%s\\n" "$line"; '
            'else printf "EOF\\n"; fi')
        reader, writer = os.pipe()
        os.write(writer, b"strict-undeclared-stdin-sentinel\n")
        os.close(writer)
        writer = None
        saved_stdin = os.dup(0)
        os.dup2(reader, 0)
        result = run_bundle(case.manifest_path, case.root, case.manifest_hash, timeout_seconds=2)
    finally:
        if saved_stdin is not None:
            os.dup2(saved_stdin, 0)
            os.close(saved_stdin)
        if writer is not None:
            os.close(writer)
        if reader is not None:
            remaining = os.read(reader, 256)
            os.close(reader)
        case.tearDown()
    report = {
        "unit_dir": str(unit),
        "runner_sha256": hashlib.sha256((unit / "sealed_runner.py").read_bytes()).hexdigest(),
        "child_returncode": result.returncode,
        "child_stdout": result.stdout.decode(errors="replace"),
        "pipe_bytes_remaining": remaining.decode(errors="replace"),
        "accepted": result.returncode == 0 and result.stdout == b"EOF\n"
            and remaining == b"strict-undeclared-stdin-sentinel\n",
    }
    print(json.dumps(report, indent=2))
    return 0 if report["accepted"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
