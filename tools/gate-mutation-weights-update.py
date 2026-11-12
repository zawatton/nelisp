#!/usr/bin/env python3
"""Regenerate tools/gate-mutation-weights.txt from shard timing sidecars.

Usage: tools/gate-mutation-weights-update.py REPORT_DIR [WEIGHTS_FILE]

REPORT_DIR is searched recursively for gate-mutation-seconds-*.json (the
`row_seconds' sidecars the harness writes next to the row-accounting
reports, e.g. the downloaded gate-reports-mutation-* CI artifacts).  Rows
measured now replace their old weight; rows not measured keep it.
"""
import json
import sys
from pathlib import Path

HEADER = """# Per-row wall seconds used to split tools/gate-mutations.txt across CI shards
# (longest-processing-time first; see tools/gate-mutation-shard.awk).
# Format: key<TAB>seconds, key = gate|file|N (N-th row of that gate+file in the table).
# Regenerate with tools/gate-mutation-weights-update.py <reports dir>.
# Rows absent from this file get the median weight.
"""


def load(path):
    weights = {}
    for line in path.read_text().splitlines():
        if line.strip() and not line.startswith("#") and "\t" in line:
            key, secs = line.split("\t", 1)
            weights[key] = int(secs)
    return weights


def main(argv):
    if not 2 <= len(argv) <= 3:
        print(__doc__)
        return 2
    out = Path(argv[2]) if len(argv) == 3 else Path(__file__).with_name("gate-mutation-weights.txt")
    weights = load(out) if out.exists() else {}
    files = sorted(Path(argv[1]).rglob("gate-mutation-seconds-*.json"))
    if not files:
        print("no gate-mutation-seconds-*.json under", argv[1])
        return 1
    measured = 0
    for f in files:
        for key, secs in json.loads(f.read_text())["row_seconds"].items():
            weights[key] = max(1, int(secs))
            measured += 1
    out.write_text(HEADER + "".join(f"{k}\t{weights[k]}\n" for k in sorted(weights)))
    print(f"{out}: {len(weights)} rows ({measured} measured from {len(files)} files)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
