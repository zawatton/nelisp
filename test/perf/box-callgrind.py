#!/usr/bin/env python3
"""Record or check allocations and instructions per iteration on three workloads.

Workloads (each run at two sizes so startup cancels out):
  re    -- (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" NAME), per match
  call  -- interpreted (f3 i 2 3) of a 3-arg defun in a while loop, per iteration
           (NELISP_FX_DISABLE=1, so the tree-walking evaluator runs)
  loop  -- (while (< i n) (setq i (1+ i))), per iteration (NELISP_FX_DISABLE=1)

Each program runs (garbage-collect) just before the measured part.  Without
it the free-list state left by startup decides whether nl_alloc_bytes takes
its fast path, and the per-iteration instruction count of the same binary
moved by 20% between runs; with it, repeated runs agree exactly.

record: run BIN under callgrind on all six programs in parallel and write
<wl>.instr / <wl>.alloc (nl_alloc_bytes calls) per iteration, plus BIN's
sha256, to OUT.json.  Takes about ten minutes.

check: exit 0 iff OUT.json was recorded on BIN and every --max KEY=LIMIT holds.
This is what tools/ai/box-review-progress.org calls.

  box-callgrind.py record BIN OUT.json
  box-callgrind.py check BIN OUT.json --max re.alloc=20000 --max call.alloc=80
"""
import bisect, hashlib, json, os, re, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

RE_PROG = (';;; -*- lexical-binding: t -*-\n'
           '(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k))'
           ' (number-sequence 1 {n})))) (garbage-collect) (dolist (s names)'
           ' (string-match "\\\\`\\\\(.*\\\\)\\\\(\\\\.el\\\\)\\\\\'" s)))\n')
WORKLOADS = {
    "re": (RE_PROG, 20, 220, {}),
    "call": ("(defun f3 (a b c) a)\n(garbage-collect)\n"
             "(let ((i 0) (n {n})) (while (< i n) (f3 i 2 3) (setq i (1+ i))))\n",
             10, 5010, {"NELISP_FX_DISABLE": "1"}),
    "loop": ("(garbage-collect)\n(let ((i 0) (n {n})) (while (< i n) (setq i (1+ i))))\n",
             10, 20010, {"NELISP_FX_DISABLE": "1"}),
}


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


def profile(binary, src, env, workdir, addrs, names):
    out = src[:-3] + ".out"
    subprocess.run(["valgrind", "--tool=callgrind", f"--callgrind-out-file={out}",
                    binary, "--load", src], capture_output=True, check=True,
                   env=dict(os.environ, **env))
    def name(addr):
        i = bisect.bisect_right(addrs, addr) - 1
        return names[i] if i >= 0 else "?"
    total, allocs, ids, cfn, pending = 0, 0, {}, None, None
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
            if cfn == "nl_alloc_bytes":
                allocs += pending
            pending = None
    return total, allocs


def record(binary, out_json):
    addrs, names = symbols(binary)
    rec = {"binary_sha256": sha256(binary)}
    with tempfile.TemporaryDirectory(dir=os.path.dirname(os.path.abspath(out_json))) as d:
        jobs = []
        for wl, (prog, n1, n2, env) in WORKLOADS.items():
            for n in (n1, n2):
                src = os.path.join(d, f"{wl}{n}.el")
                with open(src, "w") as f:
                    f.write(prog.format(n=n))
                jobs.append((wl, n, src, env))
        with ThreadPoolExecutor(len(jobs)) as ex:
            res = list(ex.map(lambda j: profile(binary, j[2], j[3], d, addrs, names), jobs))
    by = {(j[0], j[1]): r for j, r in zip(jobs, res)}
    for wl, (_, n1, n2, _) in WORKLOADS.items():
        (t1, a1), (t2, a2) = by[(wl, n1)], by[(wl, n2)]
        rec[f"{wl}.instr"] = round((t2 - t1) / (n2 - n1))
        rec[f"{wl}.alloc"] = round((a2 - a1) / (n2 - n1), 2)
    with open(out_json, "w") as f:
        json.dump(rec, f, indent=1)
    print(json.dumps(rec))


def check(binary, out_json, limits):
    rec = json.load(open(out_json))
    ok = rec["binary_sha256"] == sha256(binary)
    if not ok:
        print("BOX-CALLGRIND stale record: binary changed since it was recorded")
    for spec in limits:
        k, v = spec.split("=")
        good = rec.get(k) is not None and rec[k] <= float(v)
        print(f"BOX-CALLGRIND {k} {rec.get(k)} (limit {v}) {'PASS' if good else 'FAIL'}")
        ok = ok and good
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    a = sys.argv[1:]
    if a and a[0] == "record":
        record(a[1], a[2])
    elif a and a[0] == "check":
        check(a[1], a[2], [a[i + 1] for i in range(3, len(a)) if a[i] == "--max"])
    else:
        sys.exit(__doc__)
