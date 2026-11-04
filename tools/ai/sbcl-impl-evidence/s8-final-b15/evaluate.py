#!/usr/bin/env python3
"""Recompute the S8 campaign results from raw samples and apply the frozen protocol.

Usage: evaluate.py [--row ROW] [--self-test]

Reads raw/<row>/raw.json (paired_bench samples), recomputes per-step kernel
medians from the two trip counts of the same repetition, whole-process medians
at the high count, code sizes, and the frozen thresholds in
s8-protocol-frozen.json.  Writes summary.json and exits non-zero if any
integrity check fails.  With --row ROW, exits 0 only if ROW meets the frozen
improvement threshold (>= 10% lower median kernel time than old) and no row
regresses by more than 5%.
"""
import json
import statistics
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ENGINES = ("old", "new", "gnu")


def per_step(raw, engine, key, low, high, reps):
    vals = []
    for rep in range(reps):
        p = {r["n"]: r for r in raw if r["engine"] == engine and r["rep"] == rep}
        vals.append((p[high][key] - p[low][key]) / (high - low))
    return vals


def evaluate(root):
    proto = json.loads((root / "s8-protocol-frozen.json").read_text())
    reps = proto["method"]["reps"]
    rows, errors = {}, []
    for row, spec in proto["rows"].items():
        raw = json.loads((root / "raw" / row / "raw.json").read_text())
        low, high = spec["low"], spec["high"]
        # Integrity: every engine has every (rep, n) sample once, with one result per n.
        for e in ENGINES:
            for rep in range(reps):
                for n in (low, high):
                    hits = [r for r in raw if r["engine"] == e and r["rep"] == rep and r["n"] == n]
                    if len(hits) != 1:
                        errors.append(f"{row}: {e} rep {rep} n {n} has {len(hits)} samples")
        for n in (low, high):
            if len({r["result"] for r in raw if r["n"] == n}) != 1:
                errors.append(f"{row}: result mismatch at n={n}")
        if errors:
            continue
        k = {e: per_step(raw, e, "inner_ns", low, high, reps) for e in ENGINES}
        med = {e: statistics.median(k[e]) for e in ENGINES}
        proc = {e: statistics.median(r["process_ns"] for r in raw
                                     if r["engine"] == e and r["n"] == high) / 1e9
                for e in ENGINES}
        rows[row] = {
            "low": low, "high": high,
            "kernel_ns_per_step_median": med,
            "kernel_ns_per_step_raw": k,
            "kernel_change_pct_new_vs_old": (med["new"] - med["old"]) / med["old"] * 100,
            "new_over_gnu": med["new"] / med["gnu"],
            "pairs_new_faster_than_old": sum(1 for o, n in zip(k["old"], k["new"]) if n < o),
            "whole_process_s_median_high": proc,
            "whole_process_change_pct_new_vs_old": (proc["new"] - proc["old"]) / proc["old"] * 100,
        }
    targets = proto["thresholds"]["target_rows"]
    for row, r in rows.items():
        r["target"] = row in targets
        r["meets_improvement"] = r["kernel_change_pct_new_vs_old"] <= -10.0
        r["kernel_regression_over_5pct"] = r["kernel_change_pct_new_vs_old"] > 5.0
        r["whole_process_regression_over_5pct"] = r["whole_process_change_pct_new_vs_old"] > 5.0
    sizes = json.loads((root / "code-sizes.json").read_text())
    growth = {s: (sizes["new"][s] - sizes["old"][s]) / sizes["old"][s] * 100
              for s in sizes["old"] if s in sizes["new"]}
    summary = {
        "rows": rows,
        "code_growth_pct": growth,
        "code_growth_flagged_over_10pct": sorted(s for s, g in growth.items() if g > 10.0),
        "any_kernel_regression_over_5pct": any(r["kernel_regression_over_5pct"] for r in rows.values()),
        "any_whole_process_regression_over_5pct": any(r["whole_process_regression_over_5pct"] for r in rows.values()),
        "errors": errors,
    }
    return summary


def self_test():
    """A mutated raw sample must be detected (result mismatch or changed medians)."""
    import copy, shutil, tempfile
    tmp = Path(tempfile.mkdtemp())
    try:
        shutil.copytree(HERE, tmp / "e")
        good = evaluate(tmp / "e")
        if good["errors"]:
            print("self-test: baseline has errors", good["errors"][:2]); return 1
        p = tmp / "e" / "raw" / "vector-read" / "raw.json"
        raw = json.loads(p.read_text())
        raw[0]["result"] = raw[0]["result"] + "x"
        p.write_text(json.dumps(raw))
        bad = evaluate(tmp / "e")
        ok = bool(bad["errors"])
        print(f"self-test result-mutation detected: {ok}")
        return 0 if ok else 1
    finally:
        shutil.rmtree(tmp)


def main(argv):
    if argv[1:] == ["--self-test"]:
        return self_test()
    summary = evaluate(HERE)
    (HERE / "summary.json").write_text(json.dumps(summary, indent=1) + "\n")
    if summary["errors"]:
        print("ERRORS:", *summary["errors"][:5], sep="\n  ")
        return 1
    if len(argv) == 3 and argv[1] == "--row":
        r = summary["rows"][argv[2]]
        ok = r["meets_improvement"] and not summary["any_kernel_regression_over_5pct"]
        print(f"{argv[2]}: kernel {r['kernel_change_pct_new_vs_old']:+.1f}% "
              f"({'meets' if ok else 'does not meet'} the frozen >=10% threshold)")
        return 0 if ok else 1
    for row, r in sorted(summary["rows"].items()):
        print(f"{row:<22} kernel {r['kernel_change_pct_new_vs_old']:+7.1f}%  "
              f"whole {r['whole_process_change_pct_new_vs_old']:+6.1f}%  new/gnu {r['new_over_gnu']:.3f}")
    print("code growth >10%:", ", ".join(summary["code_growth_flagged_over_10pct"]))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
