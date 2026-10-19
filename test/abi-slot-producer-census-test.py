#!/usr/bin/env python3
"""Check the three S6.2 slot producers in an actual pre-inline unit dump.

Unlike a family total, this command rejects missing owned definitions and
checks the complete conversion walk, including let initializers. It reads
only applyfn-reader.o instead of parsing every unit in a full census replay.

Usage: python3 test/abi-slot-producer-census-test.py BUILD/dump.jsonl
"""
import importlib.util
import json
from pathlib import Path
import sys

root = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("box_census", root / "tools/ai/box-census.py")
census = importlib.util.module_from_spec(spec)
spec.loader.exec_module(census)
owned = ("bf_expand_slot_w", "m5_fx_slot_word", "bf_aref_checked_w")
source = None
with open(sys.argv[1], encoding="utf-8") as stream:
    for line in stream:
        # The unit name is the first field in the writer's compact JSONL.
        # Still parse and verify it; substring matching alone is not evidence.
        if '"applyfn-reader.o"' in line:
            unit = json.loads(line)
            if unit["unit"] == "applyfn-reader.o":
                source = unit["source"]
if source is None:
    raise SystemExit("FAIL: missing applyfn-reader.o")
# The two-status spelling is the same reverse adapter debt, even in old
# dumps whose general census did not name it as a separate idiom.
rows = census.conversion_rows(source)
definitions = [x[1] for x in census.forms(source) if x[0] == "defun"]
failed = False
for name in owned:
    if definitions.count(name) != 1:
        raise SystemExit(f"FAIL: expected one definition of {name}")
    counts = [rows.get((name, "nl_val_store_word"), 0),
              sum(rows.get((name, idiom), 0) for idiom in
                  ("nl_abi_rc_to_word", "nl_abi_rc2_to_word")),
              rows.get((name, "inline-box-to-word"), 0)]
    failed |= any(counts)
    print(f"applyfn-reader.o\t{name}\tstore={counts[0]} reverse={counts[1]} inline={counts[2]}")
print(f"SLOT-PRODUCER checked={len(owned)} {'FAIL' if failed else 'PASS'}")
raise SystemExit(1 if failed else 0)
