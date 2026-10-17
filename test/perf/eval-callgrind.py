#!/usr/bin/env python3
"""Record or check the evaluator's instruction cost per loop iteration.

record: run BIN under callgrind on (while (< i n) (setq i (1+ i))) with
n=10 and n=20010, and write the difference per iteration -- instructions
and calls of a few hot natives -- with BIN's sha256 to OUT.json.  Startup
dominates both runs and cancels out.  Takes several minutes; run it by
hand after a change, one at a time.

check: exit 0 iff OUT.json was recorded on BIN (same sha256) and every
--max NAME=LIMIT holds.  This is what tools/ai/evaluator-speed-progress.org
calls, so the ledger stays cheap and still refuses a stale record.

  eval-callgrind.py record BIN OUT.json [--loop eval|fx]
  eval-callgrind.py check BIN OUT.json --max instr=46000 --max nl_alloc_bytes=10
"""
import bisect, hashlib, json, os, re, subprocess, sys, tempfile

HOT = ["nl_alloc_bytes", "nl_sp_eq_lit", "nl_thread_registry_find", "nelisp_fnv1a",
       "nelisp_eval_call_inner"]
N_SMALL, N_BIG = 10, 20010


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def symbols(binary):
    out = subprocess.run(["nm", "-n", binary], capture_output=True, text=True).stdout
    syms = [(int(a, 16), n) for a, t, n in
            (l.split() for l in out.splitlines() if len(l.split()) == 3)
            if t in "Tt"]
    return [a for a, _ in syms], [n for _, n in syms]


LOOPS = {
    "eval": "(let ((i 0) (n {n})) (while (< i n) (setq i (1+ i))))\n",
    # The same loop on the word VM (tools/ai/eval-core-progress.org).
    "fx": "(nelisp--fx-eval '(let ((i 0) (n {n})) (while (< i n) (setq i (1+ i))) i))\n",
}
LOOP = "eval"


def profile(binary, n, workdir, addrs, names):
    src = os.path.join(workdir, f"loop{n}.el")
    with open(src, "w") as f:
        f.write(LOOPS[LOOP].format(n=n))
    out = os.path.join(workdir, f"cg{n}.out")
    subprocess.run(["valgrind", "--tool=callgrind", f"--callgrind-out-file={out}",
                    binary, "--load", src], capture_output=True, check=True)
    def name(addr):
        i = bisect.bisect_right(addrs, addr) - 1
        return names[i] if i >= 0 else "?"
    total = 0
    calls = {}
    ids = {}
    cfn = None
    pending = None
    for line in open(out):
        if line.startswith("summary:"):
            total = int(line.split()[1])
        m = re.match(r"(c?fn)=\((\d+)\)(?:\s+(\S+))?", line)
        if m:
            if m.group(3):
                tok = m.group(3).split("'")[0]
                ids[m.group(2)] = name(int(tok, 16)) if tok.startswith("0x") else tok
            if m.group(1) == "cfn":
                cfn = ids.get(m.group(2), "?")
            continue
        if line.startswith("calls="):
            pending = int(line[6:].split()[0])
        elif pending is not None:
            calls[cfn] = calls.get(cfn, 0) + pending
            pending = None
    return total, calls


def record(binary, out_json):
    addrs, names = symbols(binary)
    with tempfile.TemporaryDirectory(dir=os.path.dirname(os.path.abspath(out_json))) as d:
        t1, c1 = profile(binary, N_SMALL, d, addrs, names)
        t2, c2 = profile(binary, N_BIG, d, addrs, names)
    iters = N_BIG - N_SMALL
    rec = {"binary_sha256": sha256(binary), "iterations": iters, "loop": LOOP,
           "instr": round((t2 - t1) / iters)}
    for k in HOT:
        rec[k] = round((c2.get(k, 0) - c1.get(k, 0)) / iters, 2)
    with open(out_json, "w") as f:
        json.dump(rec, f, indent=1)
    print(json.dumps(rec))


def check(binary, out_json, limits):
    rec = json.load(open(out_json))
    ok = rec["binary_sha256"] == sha256(binary)
    if not ok:
        print("EVAL-CALLGRIND stale record: binary changed since it was recorded")
    for spec in limits:
        k, v = spec.split("=")
        good = rec.get(k) is not None and rec[k] <= float(v)
        print(f"EVAL-CALLGRIND {k} {rec.get(k)} (limit {v}) {'PASS' if good else 'FAIL'}")
        ok = ok and good
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    a = sys.argv[1:]
    if a[0] == "record":
        if "--loop" in a:
            LOOP = a[a.index("--loop") + 1]
        record(a[1], a[2])
    elif a[0] == "check":
        lim = [a[i + 1] for i in range(3, len(a)) if a[i] == "--max"]
        check(a[1], a[2], lim)
    else:
        sys.exit(__doc__)
