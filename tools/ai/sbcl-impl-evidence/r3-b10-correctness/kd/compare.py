#!/usr/bin/env python3
"""Compare gnu.out vs nelisp.out line sets. Exit 1 on mismatch or missing DONE."""
import os, sys, tempfile

def load(path):
    d, done, order = {}, None, []
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in f.read().split("\n"):
            if line.startswith("DONE n="):
                done = line
            elif " => " in line:
                k, v = line.split(" => ", 1)
                d[k] = v
                order.append(k)
    return d, done, order

def compare(a, b, quiet=False):
    ga, da, oa = load(a)
    gb, db, ob = load(b)
    bad = 0
    if da is None: print("MISSING DONE in", a); bad += 1
    if db is None: print("MISSING DONE in", b); bad += 1
    for k in oa:
        if k not in gb:
            print(f"MISSING-IN-B {k}\n   A: {ga[k]}"); bad += 1
        elif ga[k] != gb[k]:
            print(f"DIFF {k}\n   A: {ga[k]}\n   B: {gb[k]}"); bad += 1
    for k in ob:
        if k not in ga:
            print(f"EXTRA-IN-B {k}\n   B: {gb[k]}"); bad += 1
    print(f"compared A={len(ga)} B={len(gb)} mismatches={bad}")
    return 1 if bad else 0

def self_test(a):
    src = open(a, encoding="utf-8", errors="replace").read()
    lines = src.split("\n")
    # synthetic fallback so the test never depends on real output
    if not any(" => " in l for l in lines) or "DONE n=" not in src:
        lines = ["x => 1", "y => (error foo)", "DONE n=2", ""]
    assert compare_text(lines, lines) == 0, "identical copies must pass"
    mut = list(lines)
    for i, l in enumerate(mut):
        if " => " in l:
            mut[i] = l + "MUT"; break
    r1 = compare_text(lines, mut)
    nodone = [l for l in lines if not l.startswith("DONE n=")]
    r2 = compare_text(lines, nodone)
    ok = r1 == 1 and r2 == 1
    print("SELF-TEST", "PASS" if ok else "FAIL")
    return 0 if ok else 1

def compare_text(la, lb):
    with tempfile.TemporaryDirectory() as t:
        pa, pb = os.path.join(t, "a"), os.path.join(t, "b")
        open(pa, "w").write("\n".join(la)); open(pb, "w").write("\n".join(lb))
        return compare(pa, pb)

if __name__ == "__main__":
    if len(sys.argv) >= 2 and sys.argv[1] == "--self-test":
        sys.exit(self_test(sys.argv[2] if len(sys.argv) > 2 else "gnu.out"))
    if len(sys.argv) != 3:
        print("usage: compare.py A.out B.out | --self-test [A.out]"); sys.exit(2)
    sys.exit(compare(sys.argv[1], sys.argv[2]))
