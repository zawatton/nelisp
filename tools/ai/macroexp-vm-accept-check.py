"""Check measured macro speed and nine actual scenario-frame pairs."""

import argparse
import json
from pathlib import Path
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("directory", type=Path)
parser.add_argument("--section", choices=("macro", "frames", "all"), default="all")
parser.add_argument("--case", choices=("state", "conditional"), action="append")
args = parser.parse_args()
failed = False
try:
    if args.section in ("macro", "all"):
        for case in args.case or ("state", "conditional"):
            report = json.loads((args.directory / f"bench-{case}/report.json").read_text())
            row = report["engines"]["candidate"]
            ok = (str(report["status"]).lower() == "ok"
                  and report["mode"] == "single_pass" and report["reps"] == 7
                  and row["pairs_total"] == 7 and row["pairs_faster"] >= 6
                  and row["ratio_to_baseline_median"] <= 0.95)
            print(f"MACRO-SPEED {'PASS' if ok else 'FAIL'} {case}: "
                  f"ratio={row['ratio_to_baseline_median']:.6f} wins={row['pairs_faster']}/7")
            failed |= not ok
    if args.section in ("frames", "all"):
        ids = ("death-list", "enc-115", "enc-157", "enc-175", "enc-179",
               "enc-32", "enc-52", "shop-126", "shop-302")
        root = args.directory / "ndtw-nineframes"
        good = 0
        for scenario in ids:
            left = root / "gnu" / f"{scenario}-frame.json"
            right = root / "nelisp" / f"{scenario}-frame.json"
            if left.is_file() and right.is_file():
                # Reject empty/non-JSON data before checking exact output.
                json.loads(left.read_text(encoding="utf-8"))
                json.loads(right.read_text(encoding="utf-8"))
                good += left.read_bytes() == right.read_bytes()
        counts = [len(list((root / engine).glob("*-frame.json")))
                  for engine in ("gnu", "nelisp")]
        ok = good == 9 and counts == [9, 9]
        print(f"SCENARIO-FRAMES {'PASS' if ok else 'FAIL'} matched={good}/9 files={counts}")
        failed |= not ok
except (OSError, ValueError, KeyError, TypeError) as error:
    print(f"VM-ACCEPT ERROR {type(error).__name__}: {str(error)[:240]}")
    failed = True
sys.exit(bool(failed))
