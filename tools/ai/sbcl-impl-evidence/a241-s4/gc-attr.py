#!/usr/bin/env python3
"""Doc 241 S4.2: GC attribution of a kernel-ratio difference between the full lane and the app image.
Usage: gc-attr.py BINARY CG_LO_DIR CG_HI_DIR LO HI OUT.json
CG_*_DIR are outputs of cg-instr-s42.sh (cgi.full, cgi.app, syms.txt) at N=LO and N=HI
(2 warmups at N plus the kernel at N, so a run executes 3N steps)."""
import bisect, collections, json, re, sys
binary, d_lo, d_hi, lo, hi, out = sys.argv[1:]; lo, hi = int(lo), int(hi)
GC = re.compile(r"^(nl_gc_|nl_sweep_|nl_typed_registry_lookup$|nl_typed_find$)")
syms = [(int(a, 16), n) for a, n in (l.split() for l in open(f"{d_lo}/syms.txt"))]
addrs = [a for a, _ in syms]
def load(path):
    c = collections.Counter(); inbin = incall = False
    for l in open(path):
        if l.startswith("ob="): inbin = binary in l
        elif l.startswith("calls="): incall = True
        elif l.startswith("0x"):
            if incall: incall = False; continue
            if inbin:
                p = l.split(); i = bisect.bisect_right(addrs, int(p[0], 16)) - 1
                c[syms[i][1] if i >= 0 else "?"] += int(p[-1])
    if not c: sys.exit(f"no cost attributed to {binary} in {path}")
    return c
res = {"binary": binary, "lo": lo, "hi": hi}
for p in ("full", "app"):
    a, b = load(f"{d_lo}/cgi.{p}"), load(f"{d_hi}/cgi.{p}")
    per = {k: (b[k] - a[k]) / (3 * (hi - lo)) for k in set(a) | set(b)}
    res[p] = {"total": sum(per.values()), "gc": sum(v for k, v in per.items() if GC.match(k)),
              "non_gc": sum(v for k, v in per.items() if not GC.match(k))}
res["non_gc_ratio_app_over_full"] = res["app"]["non_gc"] / res["full"]["non_gc"]
json.dump(res, open(out, "w"), indent=1)
print(json.dumps(res, indent=1))
