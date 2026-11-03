#!/usr/bin/env python3
"""Median per-segment startup times from sub-phased runs. Usage: summarize.py OUTDIR"""
import json, re, statistics, sys
from pathlib import Path
O = Path(sys.argv[1]); segs = {}
CHAIN = ["SPAWN", "BOOT_ENTRY", "BOOT_REINIT_COMPLETE", "FIRST_FORM_COMPLETE", "PREP_CHECKS_BEGIN",
         "PREP_PINS_VERIFIED", "PREP_HELPER_LOADED", "PREP_PRELUDE_LOADED", "PREP_PRODUCER_LOADED",
         "PREP_PREFLIGHT_DONE", "PREP_INSTALL_DONE", "PREP_DRIVER_PREP_DONE", "APPLICATION_STATE_READY",
         "KERNEL_BEGIN", "RESULT_VALIDATED", "CHILD_EXIT", "WRAPPER_CLEANUP_COMPLETE"]
n = 0
for ev in sorted(O.glob("phase-out/*.events.sexp")):
    t = ev.read_text(); w = list(json.loads(ev.with_name(ev.name.replace(".events.sexp", ".wrapper.json")).read_text()).values())[0]
    e = {m.group(1): int(m.group(2)) for m in re.finditer(r"\(([A-Z_]+) (\d+) nil\)", t)}
    b = [int(x) for x in re.search(r":boot \[(\d+) (\d+) (\d+)", t).groups()]
    e["BOOT_ENTRY"], e["BOOT_REINIT_COMPLETE"] = b[0], b[2]
    for k in ("SPAWN", "CHILD_EXIT", "WRAPPER_CLEANUP_COMPLETE"):
        v = w.get(k)
        if v is None: sys.exit(f"wrapper lacks {k}: keys {list(w)[:12]}")
        e[k] = int(v)
    for a, c in zip(CHAIN, CHAIN[1:]):
        d = e[c] - e[a]
        if d < 0: sys.exit(f"{ev.name}: {a}->{c} negative")
        segs.setdefault(f"{a} -> {c}", []).append(d / 1e6)
    n += 1
tot = []
print(f"runs={n}")
for k, v in segs.items():
    print(f"{statistics.median(v):9.1f} ms  min {min(v):8.1f}  max {max(v):8.1f}  {k}")
print(f"{sum(statistics.median(v) for v in segs.values()):9.1f} ms  sum of medians")
