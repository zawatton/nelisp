#!/usr/bin/env python3
"""Evaluate an app-lane campaign (campaign-app.sh) run directory.

Usage: evaluate_app.py RUN_DIR [--allow-reps] | evaluate_app.py x --self-test

RUN_DIR holds campaign.json, campaign.log and <row>/raw.json (paired_bench
samples: engine, n, process_ns, inner_ns, result, rep).  Engines: old = full ON
lane, new = saved-image launcher, gnu = GNU Emacs.  Integrity: every row listed
in campaign.json is present; each engine has exactly `reps` samples at each of
the frozen LOW/HIGH counts (reps must be 7 unless --allow-reps); all engines
print the same result at each N; no ROW-FAILED in campaign.log.  Kernel ns/step
is the median over reps of (inner_ns[HIGH]-inner_ns[LOW])/(HIGH-LOW), as in
../s8-final-b28/evaluate.py.  Pass: integrity holds and the kernel ratio
new/old is in [0.95, 1.05].  An allocating row (GC cadence, see
../s8-supplement-long) outside the band passes only with long-interval evidence:
RUN_DIR/cadence/<row>/raw.json from paired_bench.py with engines old and new,
at least 3 reps, equal results, and a long-interval kernel ratio new/old in
[0.95, 1.05]; such a row is flagged "cadence".  Otherwise it passes only with GC
attribution evidence RUN_DIR/gc-attribution/<row>.json (a241-s4/gc-attr.py from
instruction-level callgrind of both paths at two trip counts): the new path is faster,
its GC Ir/step is lower, and its non-GC Ir/step is within 2% of the old path's; such
a row is flagged "gc-attributed" (Doc 241 S4.2: cold-image objects are not swept).
Writes RUN_DIR/summary-app.json.
"""
import json
import statistics
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ENGINES = ("old", "new", "gnu")
PROTO = HERE.parent / "s8-final-b28" / "s8-protocol-frozen.json"
ALLOC = ("vector-allocate64", "string-concat16", "string-slice16")
PROTOCOL_REPS = 7
BAND = (0.95, 1.05)
CADENCE_MIN_REPS = 3
NON_GC_BAND = (0.98, 1.02)


def gc_attribution(run, row, ratio):
    """(ok, note) from RUN_DIR/gc-attribution/ROW.json."""
    path = Path(run) / "gc-attribution" / f"{row}.json"
    if not path.exists():
        return False, "no GC attribution evidence"
    g = json.loads(path.read_text())
    ngr = g["non_gc_ratio_app_over_full"]
    ok = (ratio < 1.0 and g["app"]["gc"] < g["full"]["gc"]
          and NON_GC_BAND[0] <= ngr <= NON_GC_BAND[1])
    return ok, (f"GC attribution: non-GC Ir ratio {ngr:.4f}, GC Ir/step full {g['full']['gc']:.1f} "
                f"app {g['app']['gc']:.1f}")


def cadence_ratio(run, row):
    """Long-interval kernel ratio new/old from RUN_DIR/cadence/ROW/raw.json, or (None, reason)."""
    path = Path(run) / "cadence" / row / "raw.json"
    if not path.exists():
        return None, "no long-interval evidence"
    raw = json.loads(path.read_text())
    ns = sorted({r["n"] for r in raw})
    if len(ns) != 2:
        return None, f"long-interval evidence has trip counts {ns}"
    low, high = ns
    if len({r["result"] for r in raw if r["n"] == low}) != 1 or len({r["result"] for r in raw if r["n"] == high}) != 1:
        return None, "long-interval evidence: engines print different results"
    med = {}
    for e in ("old", "new"):
        reps = sorted({r["rep"] for r in raw if r["engine"] == e})
        if len(reps) < CADENCE_MIN_REPS:
            return None, f"long-interval evidence: {e} has {len(reps)} reps"
        ks = []
        for rep in reps:
            p = {r["n"]: r for r in raw if r["engine"] == e and r["rep"] == rep}
            if set(p) != {low, high}:
                return None, f"long-interval evidence: {e} rep {rep} incomplete"
            ks.append((p[high]["inner_ns"] - p[low]["inner_ns"]) / (high - low))
        med[e] = statistics.median(ks)
    return med["new"] / med["old"], f"long-interval n={low}..{high}"


def evaluate(run, allow_reps=False):
    proto = json.loads(PROTO.read_text())["rows"]
    errors, rows = [], {}
    run = Path(run)
    try:
        meta = json.loads((run / "campaign.json").read_text())
    except (OSError, ValueError) as e:
        return {"errors": [f"campaign.json unreadable: {e}"], "rows": {}, "ok": False}
    reps = meta.get("reps")
    if not isinstance(reps, int) or reps < 1:
        return {"errors": [f"campaign.json reps invalid: {reps!r}"], "rows": {}, "ok": False}
    if reps != PROTOCOL_REPS and not allow_reps:
        errors.append(f"reps={reps} is not the protocol {PROTOCOL_REPS} (use --allow-reps for a smoke run)")
    if not meta.get("end"):
        errors.append("campaign.json has no end time (campaign did not finish)")
    want = meta.get("rows") or []
    if not want:
        errors.append("campaign.json lists no rows")
    if not allow_reps and sorted(want) != sorted(proto):
        errors.append(f"rows {sorted(want)} are not all {len(proto)} protocol rows")
    try:
        log = (run / "campaign.log").read_text()
        if any(l.startswith("ROW-FAILED") for l in log.splitlines()):
            errors.append("ROW-FAILED in campaign.log")
    except OSError as e:
        errors.append(f"campaign.log unreadable: {e}")
    for row in want:
        if row not in proto:
            errors.append(f"{row}: not a protocol row")
            continue
        low, high = proto[row]["low"], proto[row]["high"]
        try:
            raw = json.loads((run / row / "raw.json").read_text())
        except (OSError, ValueError) as e:
            errors.append(f"{row}: raw.json unreadable: {e}")
            continue
        bad = False
        for e in ENGINES:
            for n in (low, high):
                got = sorted(r["rep"] for r in raw if r["engine"] == e and r["n"] == n)
                if got != list(range(reps)):
                    errors.append(f"{row}: {e} n={n} reps {got} != 0..{reps - 1}")
                    bad = True
        extra = {(r["engine"], r["n"]) for r in raw} - {(e, n) for e in ENGINES for n in (low, high)}
        if extra:
            errors.append(f"{row}: unexpected (engine,n) {sorted(extra)}")
            bad = True
        for n in (low, high):
            if len({r["result"] for r in raw if r["n"] == n}) != 1:
                errors.append(f"{row}: engines print different results at n={n}")
                bad = True
        if bad:
            continue
        k = {}
        for e in ENGINES:
            k[e] = []
            for rep in range(reps):
                p = {r["n"]: r for r in raw if r["engine"] == e and r["rep"] == rep}
                k[e].append((p[high]["inner_ns"] - p[low]["inner_ns"]) / (high - low))
        med = {e: statistics.median(k[e]) for e in ENGINES}
        if min(med.values()) <= 0:
            errors.append(f"{row}: non-positive kernel median {med}")
            continue
        wall = {e: statistics.median(r["process_ns"] for r in raw if r["engine"] == e and r["n"] == high) / 1e6
                for e in ENGINES}
        ratio = med["new"] / med["old"]
        cadence, long_ratio, long_note, gc_attributed = False, None, None, False
        if BAND[0] <= ratio <= BAND[1]:
            ok = True
        elif row in ALLOC:
            long_ratio, long_note = cadence_ratio(run, row)
            ok = long_ratio is not None and BAND[0] <= long_ratio <= BAND[1]
            cadence = ok
            if not ok:
                ok, gc_note = gc_attribution(run, row, ratio)
                gc_attributed = ok
                long_note = f"{long_note}; {gc_note}"
        else:
            ok = False
        rows[row] = {"low": low, "high": high, "kernel_ns_per_step_median": med,
                     "kernel_ns_per_step_raw": k, "kernel_ratio_new_over_old": ratio,
                     "whole_process_ms_median_high": wall,
                     "whole_new_over_gnu": wall["new"] / wall["gnu"],
                     "cadence": cadence, "long_interval_ratio": long_ratio,
                     "long_interval_note": long_note, "gc_attributed": gc_attributed,
                     "within_band": ok}
        if not ok:
            errors.append(f"{row}: kernel ratio new/old {ratio:.3f} outside the band"
                          + (f" ({long_note}: {long_ratio:.3f})" if long_ratio is not None
                             else f" ({long_note})" if long_note else ""))
    ok = not errors and bool(rows)
    return {"reps": reps, "errors": errors, "rows": rows, "ok": ok,
            "band": BAND}


def synth(run, rows, reps=PROTOCOL_REPS, ratio=None, drop=None, mismatch=None, failed=False, row_ratio=None,
          cadence=None, gcattr=None):
    proto = json.loads(PROTO.read_text())["rows"]
    run.mkdir(parents=True)
    (run / "campaign.json").write_text(json.dumps({"reps": reps, "rows": rows, "end": "x"}))
    (run / "campaign.log").write_text("ROW-FAILED vector-read\n" if failed else "== ok\n")
    for row in rows:
        low, high = proto[row]["low"], proto[row]["high"]
        r = (row_ratio or {}).get(row, 1.0)
        data = []
        for e, ns in (("old", 100.0), ("new", 100.0 * r), ("gnu", 90.0)):
            for rep in range(reps):
                for n in (low, high):
                    data.append({"engine": e, "n": n, "process_ns": 10**9 + n, "inner_ns": int(ns * n + 5000 + rep),
                                 "result": f"{n}:1", "rep": rep})
        if drop == row:
            data = [d for d in data if not (d["engine"] == "new" and d["rep"] == 3 and d["n"] == high)]
        if mismatch == row:
            data[-1]["result"] = "bad"
        (run / row).mkdir()
        (run / row / "raw.json").write_text(json.dumps(data))
    for row, (r, reps_c) in (cadence or {}).items():
        data = []
        for e, ns in (("old", 100.0), ("new", 100.0 * r)):
            for rep in range(reps_c):
                for n in (2097152, 4194304):
                    data.append({"engine": e, "n": n, "process_ns": 10**9, "inner_ns": int(ns * n + 7000 + rep),
                                 "result": f"{n}:1", "rep": rep})
        (run / "cadence" / row).mkdir(parents=True)
        (run / "cadence" / row / "raw.json").write_text(json.dumps(data))
    for row, (full_gc, app_gc, ngr) in (gcattr or {}).items():
        (run / "gc-attribution").mkdir(parents=True, exist_ok=True)
        (run / "gc-attribution" / f"{row}.json").write_text(json.dumps(
            {"full": {"gc": full_gc, "non_gc": 1000.0}, "app": {"gc": app_gc, "non_gc": 1000.0 * ngr},
             "non_gc_ratio_app_over_full": ngr}))


def self_test():
    allrows = list(json.loads(PROTO.read_text())["rows"])
    cases = [
        ("pass", {}, True),
        ("pass-within-5pct", {"row_ratio": {"vector-read": 1.04}}, True),
        ("mismatched-result", {"mismatch": "hash-get1024"}, False),
        ("missing-rep", {"drop": "integer-sum"}, False),
        ("kernel-drift-1.10-nonalloc", {"row_ratio": {"vector-read": 1.10}}, False),
        ("kernel-drift-0.90", {"row_ratio": {"vector-read": 0.90}}, False),
        ("alloc-0.80-no-evidence", {"row_ratio": {"string-slice16": 0.80}}, False),
        ("alloc-0.80-long-1.00", {"row_ratio": {"string-slice16": 0.80}, "cadence": {"string-slice16": (1.00, 3)}}, True),
        ("alloc-1.10-long-1.04", {"row_ratio": {"string-slice16": 1.10}, "cadence": {"string-slice16": (1.04, 3)}}, True),
        ("alloc-0.80-long-0.90", {"row_ratio": {"string-slice16": 0.80}, "cadence": {"string-slice16": (0.90, 3)}}, False),
        ("alloc-0.80-long-2reps", {"row_ratio": {"string-slice16": 0.80}, "cadence": {"string-slice16": (1.00, 2)}}, False),
        ("alloc-0.80-long-0.84-gc-ok", {"row_ratio": {"vector-allocate64": 0.80}, "cadence": {"vector-allocate64": (0.84, 3)},
                                        "gcattr": {"vector-allocate64": (520.0, 460.0, 1.001)}}, True),
        ("alloc-0.80-gc-nongc-drift", {"row_ratio": {"vector-allocate64": 0.80}, "gcattr": {"vector-allocate64": (520.0, 460.0, 0.95)}}, False),
        ("alloc-0.80-gc-not-lower", {"row_ratio": {"vector-allocate64": 0.80}, "gcattr": {"vector-allocate64": (460.0, 520.0, 1.0)}}, False),
        ("alloc-1.20-gc-slower", {"row_ratio": {"vector-allocate64": 1.20}, "gcattr": {"vector-allocate64": (520.0, 460.0, 1.0)}}, False),
        ("nonalloc-0.80-gc-ok", {"row_ratio": {"vector-read": 0.80}, "gcattr": {"vector-read": (520.0, 460.0, 1.0)}}, False),
        ("nonalloc-0.80-long-1.00", {"row_ratio": {"vector-read": 0.80}, "cadence": {"vector-read": (1.00, 3)}}, False),
        ("row-failed-in-log", {"failed": True}, False),
        ("reps-2-without-flag", {"reps": 2}, False),
    ]
    good = True
    with tempfile.TemporaryDirectory() as t:
        for name, kw, want in cases:
            run = Path(t) / name
            synth(run, allrows, **kw)
            got = evaluate(run)["ok"]
            print(f"self-test {name}: ok={got} expected={want} {'OK' if got == want else 'WRONG'}")
            good &= got == want
        run = Path(t) / "reps-2-flag"
        synth(run, allrows[:2], reps=2)
        got = evaluate(run, allow_reps=True)["ok"]
        print(f"self-test reps-2-with-flag: ok={got} expected=True {'OK' if got else 'WRONG'}")
        good &= got
    print("self-test", "PASS" if good else "FAIL")
    return 0 if good else 1


def main(argv):
    if "--self-test" in argv:
        return self_test()
    args = [a for a in argv[1:] if not a.startswith("--")]
    if len(args) != 1:
        print(__doc__.split("\n\n")[0], file=sys.stderr)
        return 2
    run = Path(args[0])
    s = evaluate(run, allow_reps="--allow-reps" in argv)
    (run / "summary-app.json").write_text(json.dumps(s, indent=1) + "\n")
    for row, r in sorted(s["rows"].items()):
        m, w = r["kernel_ns_per_step_median"], r["whole_process_ms_median_high"]
        print(f"{row:<22} kernel ns/step old {m['old']:9.2f} new {m['new']:9.2f} gnu {m['gnu']:9.2f}  new/old {r['kernel_ratio_new_over_old']:.3f}"
              f"{'  CADENCE' if r['cadence'] else ''}{'  GC-ATTRIBUTED' if r.get('gc_attributed') else ''} | whole ms old {w['old']:8.1f} new {w['new']:8.1f} gnu {w['gnu']:8.1f}  new/gnu {r['whole_new_over_gnu']:.2f}")
    for e in s["errors"]:
        print("ERROR", e)
    print("RESULT", "PASS" if s["ok"] else "FAIL")
    return 0 if s["ok"] else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
