#!/usr/bin/env python3
"""Capture/check final S4 sources without changing historical evidence inputs.

Uses the original source-closure procedure unchanged. Candidate paths are
explicit inputs; defaults follow the final S4 frame build pair. Keep captured
manifests immutable and choose a fresh output directory for every capture.
"""
import argparse
import hashlib
import importlib.util
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]
SOURCE_CHECKER = ROOT / "tools/ai/word-heap-vectorlike-source.py"
SOURCE_CHECKER_SHA = "4e312d693942f0682416ceb428d1008a69b0183af8afa438460cbfdae7a0bdc4"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("capture", "check", "controls"))
    parser.add_argument("--manifest", type=Path,
                        default=Path("target/word-heap-s4v/source-final-evidence.json"))
    parser.add_argument("--output", type=Path)
    for mode in ("dynamic", "static"):
        env = "WH_S4F_DYN" if mode == "dynamic" else "WH_S4F_STATIC"
        parser.add_argument("--" + mode, default=os.environ.get(
            env, f"target/wh-s4f/final4-{mode}/nelisp-{mode}"))
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--timeout", type=float, default=180)
    args = parser.parse_args()
    digest = hashlib.sha256(SOURCE_CHECKER.read_bytes()).hexdigest()
    if digest != SOURCE_CHECKER_SHA:
        raise ValueError("Historical source checker changed; preserve its pinned procedure")
    spec = importlib.util.spec_from_file_location("wh_final_source", SOURCE_CHECKER)
    source = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(source)
    # The unchanged procedure authenticates this wrapper (which pins its
    # implementation) and the selected paths in the manifest it generates.
    source.__file__ = __file__
    source.CANDIDATE_PATHS = {mode: source.local(getattr(args, mode)).as_posix()
                              for mode in ("dynamic", "static")}
    args.output = args.output or Path("target/wh-s4f/session3c/source-census/" +
                                    ("source-controls" if args.mode == "controls" else "source-final"))
    if args.mode == "capture":
        source.capture(args)
    elif args.mode == "controls":
        source.controls(args)
    else:
        import json
        record = json.loads((ROOT / source.local(args.manifest)).read_text())
        count = source.check_record(record, args.emacs)
        print(f"WORD-HEAP FINAL SOURCE CHECK PASS files={count} candidates=2 "
              f"identity={record['source_identity']}")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, KeyError, TypeError, subprocess.SubprocessError) as error:
        print(f"WORD-HEAP FINAL SOURCE FAIL: {error}", file=sys.stderr)
        sys.exit(1)
