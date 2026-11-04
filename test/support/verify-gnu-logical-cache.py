#!/usr/bin/env python3
"""Compare a source cache against separately captured GNU logical resolution."""
import argparse
import hashlib
import json
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--cache-manifest", type=Path, required=True)
parser.add_argument("--resolution-manifest", type=Path, required=True)
parser.add_argument("--library", action="append", required=True)
args = parser.parse_args()

def records(path):
    data = json.loads(path.read_text())
    files = data["files"]
    if isinstance(files, dict):
        files = files.values()
    result = {}
    for record in files:
        if not isinstance(record, dict):
            continue
        name = record["library"]
        if name in result:
            raise ValueError(f"Duplicate logical library record: {name}")
        result[name] = record
    return result

cache = records(args.cache_manifest)
expected = records(args.resolution_manifest)
for name in args.library:
    actual = cache[name]
    reference = expected[name]
    owner = (actual["source-relative"], actual["raw-sha256"])
    if owner != (reference["source-relative"], reference["raw-sha256"]):
        raise SystemExit(f"Logical source owner differs: {name}")
    raw = args.cache_manifest.parent / (name + ".el")
    if hashlib.sha256(raw.read_bytes()).hexdigest() != actual["raw-sha256"]:
        raise SystemExit(f"Cached source bytes differ: {name}")
print("GNU-LOGICAL-CACHE-IDENTITY-PASS")
