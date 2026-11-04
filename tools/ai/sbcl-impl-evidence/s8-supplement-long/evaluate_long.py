#!/usr/bin/env python3
"""Evaluate the supplementary long-interval S8 rows (s8-supplement-long.json).

Usage: evaluate_long.py RUN_DIR [--self-test]

RUN_DIR holds raw/<row>-short/raw.json and raw/<row>-long/raw.json from
campaign-long.sh.  Reports kernel ns/step from the long interval and, per
engine, the relative gap between the short and long intervals; a gap above the
protocol's cadence_tolerance marks the row cadence_sensitive.  Writes
RUN_DIR/summary-long.json.  Exits 1 on an integrity error (missing or duplicate
sample, result mismatch between engines), 0 otherwise.  --self-test checks the
cadence check on synthetic data (one interval containing an extra collection
must be flagged; a linear one must not) and exits 0 only if both hold.
"""
import json
import statistics
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ENGINES = ("old", "new", "gnu")


def per_step(raw, engine, low, high, reps):
    vals = []
    for rep in range(reps):
        p = {r["n"]: r for r in raw if r["engine"] == engine and r["rep"] == rep}
        vals.append((p[high]["inner_ns"] - p[low]["inner_ns"]) / (high - low))
    return vals


def check_samples(raw, row, low, high, reps, errors):
    for e in ENGINES:
        for rep in range(reps):
            for n in (low, high):
                hits = [r for r in raw if r["engine"] == e and r["rep"] == rep and r["n"] == n]
                if len(hits) != 1:
                    errors.append(f"{row}: {e} rep {rep} n {n} has {len(hits)} samples")
    for n in (low, high):
        if len({r["result"] for r in raw if r["n"] == n}) != 1:
            errors.append(f"{row}: result mismatch at n={n}")


def evaluate_row(row, spec, short_raw, long_raw, reps, tol, errors):
    n1, n2, n4 = spec["n1"], spec["n2"], spec["n4"]
    before = len(errors)
    check_samples(short_raw, row + "-short", n1, n2, reps, errors)
    check_samples(long_raw, row + "-long", n2, n4, reps, errors)
    if len(errors) != before:
        return None
    s = {e: statistics.median(per_step(short_raw, e, n1, n2, reps)) for e in ENGINES}
    k = {e: per_step(long_raw, e, n2, n4, reps) for e in ENGINES}
    m = {e: statistics.median(k[e]) for e in ENGINES}
    gap = {e: abs(s[e] - m[e]) / m[e] for e in ENGINES}
    return {
        "n": [n1, n2, n4],
        "kernel_ns_per_step_median_long": m,
        "kernel_ns_per_step_raw_long": k,
        "kernel_ns_per_step_median_short": s,
        "short_long_gap": gap,
        "cadence_sensitive": any(g > tol for g in gap.values()),
        "kernel_change_pct_new_vs_old": (m["new"] - m["old"]) / m["old"] * 100,
        "new_over_gnu": m["new"] / m["gnu"],
    }


def main(argv):
    proto = json.loads((HERE / "s8-supplement-long.json").read_text())
    reps = proto["method"]["reps"]
    tol = proto["method"]["cadence_tolerance"]
    if "--self-test" in argv:
        return self_test(proto, reps, tol)
    root = Path(argv[1])
    rows, errors = {}, []
    for row, spec in proto["rows"].items():
        paths = [root / "raw" / f"{row}-{iv}" / "raw.json" for iv in ("short", "long")]
        if not all(p.exists() for p in paths):
            errors.append(f"{row}: missing raw.json")
            continue
        r = evaluate_row(row, spec, *(json.loads(p.read_text()) for p in paths), reps, tol, errors)
        if r:
            rows[row] = r
    out = {"rows": rows, "errors": errors}
    (root / "summary-long.json").write_text(json.dumps(out, indent=1) + "\n")
    for row, r in rows.items():
        m = r["kernel_ns_per_step_median_long"]
        print("%-18s old %8.2f new %8.2f gnu %8.2f  new/old %+6.1f%%  new/gnu %.3f  gap %s%s" % (
            row, m["old"], m["new"], m["gnu"], r["kernel_change_pct_new_vs_old"], r["new_over_gnu"],
            " ".join("%s=%.3f" % (e, r["short_long_gap"][e]) for e in ENGINES),
            "  CADENCE-SENSITIVE" if r["cadence_sensitive"] else ""))
    for e in errors:
        print("ERROR", e)
    return 1 if errors else 0


def synth(n1, n2, n4, ns, gc_at=None, gc_ns=0, reps=5):
    raw = []
    for e in ENGINES:
        for rep in range(reps):
            for n in (n1, n2, n4):
                t = n * ns + 10_000_000 + (gc_ns if gc_at is not None and n >= gc_at else 0)
                raw.append({"engine": e, "rep": rep, "n": n, "inner_ns": t, "result": str(n)})
    return raw


def self_test(proto, reps, tol):
    spec = {"n1": 1048576, "n2": 2097152, "n4": 4194304}
    ok = True
    errs = []
    lin = synth(**spec, ns=150.0, reps=reps)
    r = evaluate_row("lin", spec, lin, lin, reps, tol, errs)
    ok &= (r is not None and not r["cadence_sensitive"] and abs(r["kernel_ns_per_step_median_long"]["new"] - 150.0) < 1e-6)
    # One extra 40 ms collection appears only from n2 on: the short interval absorbs it.
    gc = synth(**spec, ns=150.0, gc_at=spec["n2"], gc_ns=40_000_000, reps=reps)
    r = evaluate_row("gc", spec, gc, gc, reps, tol, errs)
    ok &= (r is not None and r["cadence_sensitive"])
    # Integrity: a dropped sample must be an error.
    bad = lin[1:]
    errs2 = []
    ok &= evaluate_row("bad", spec, bad, lin, reps, tol, errs2) is None and bool(errs2)
    print("self-test", "ok" if ok else "FAILED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
