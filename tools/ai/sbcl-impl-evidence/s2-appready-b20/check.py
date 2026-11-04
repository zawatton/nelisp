#!/usr/bin/env python3
"""Re-validate the S2.5 cohort from raw data; exit 0 only if every rule holds and the
recomputed summary equals summary.json."""
import csv,json,re,statistics,sys,os
d=sys.argv[1] if len(sys.argv)>1 else os.path.dirname(os.path.abspath(__file__))
ROWS=["integer-sum","function-call2","vector-write","vector-read","vector-allocate64","string-slice16","string-concat16","hash-get1024"]
def fail(m): print("FAIL:",m); sys.exit(1)
rs=list(csv.DictReader(open(d+"/runs.csv")))
A=json.load(open(d+"/phaseA.json")); neg=json.load(open(d+"/negctl.json")); rec=json.load(open(d+"/summary.json"))
by={}
for r in rs: by.setdefault((r["row"],r["arm"]),[]).append(r)
summ={"rows":{}}
for row in ROWS:
    ref=None
    for arm in ("normal","restored","direct"):
        L=by.get((row,arm),[])
        if len(L)!=7 or sorted(int(x["pair"]) for x in L)!=list(range(1,8)): fail("%s %s: need 7 pairs"%(row,arm))
        for x in L:
            if x["rc"]!="0": fail("%s %s pair %s rc=%s"%(row,arm,x["pair"],x["rc"]))
            m=re.match(r"BENCH-CHECK n=(\d+) result=(\d+:\d+) elapsed=[0-9.]+$",x["check"])
            if not m: fail("%s %s: bad check line %r"%(row,arm,x["check"]))
            key=(m.group(1),m.group(2))
            if ref is None: ref=key
            elif key!=ref: fail("%s %s: result differs %r vs %r"%(row,arm,key,ref))
            if (arm=="restored")!=(x["reused"]=="1"): fail("%s %s: reuse flag wrong"%(row,arm))
    med={a:round(statistics.median(float(x["wall_s"]) for x in by[(row,a)]),4) for a in ("normal","restored","direct")}
    sv=round(med["normal"]-med["restored"],4); svd=round(med["direct"]-med["restored"],4)
    pm=A["prep_median_wall_s"]
    summ["rows"][row]={"result":"%s result=%s"%ref,"median_wall_s":med,"saving_vs_runner_s":sv,"saving_vs_direct_s":svd,
        "breakeven_runs_vs_runner":(round(pm/sv,2) if sv>0 else None),"breakeven_runs_vs_direct":(round(pm/svd,2) if svd>0 else None)}
for n in ("changed-image-sha","changed-tree-id","no-restore"):
    c=neg.get(n)
    if not c or c["rc"]==0 or "BENCH-CHECK" in c["stdout"] or not c["refused"]: fail("negative control %s not refused"%n)
if len(A["prep"])<1 or not all(p["rc"]==0 for p in A["prep"]) or len(A["app_cold_sha256"])!=64: fail("phase A record")
if round(statistics.median(p["wall_s"] for p in A["prep"]),4)!=A["prep_median_wall_s"]: fail("prep median")
summ["prep_median_wall_s"]=A["prep_median_wall_s"]; summ["app_cold_sha256"]=A["app_cold_sha256"]; summ["app_cold_size"]=A["app_cold_size"]
if summ!=rec: fail("recomputed summary differs from summary.json")
print("PASS: 8 rows x 7 pairs x 3 arms, identical results, 3 refusals, summary reproduced")
