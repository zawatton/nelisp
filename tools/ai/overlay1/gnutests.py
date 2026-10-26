#!/usr/bin/env python3
"""Run a GNU Emacs 31.1 test file on a NeLisp binary through the survey shim.

usage: gnutests.py BINARY TESTFILE(relative to GNU test/) [--expect-pass LIST] [--out TSV]
Environment: EMACS_SRC_DIR (default ~/Downloads/emacs-31.1), TASKSET_CPU (optional).
Prints "PASS n FAIL n ERROR n ..." and, with --expect-pass, exits 1 unless every listed test passes.
"""
import os, sys, subprocess, time, select, signal, tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
SHIM = os.path.join(HERE, "shim.el")
DRV = os.path.join(HERE, "gnutests-drv.el")
SRC = os.environ.get("EMACS_SRC_DIR", os.path.expanduser("~/Downloads/emacs-31.1"))


def run_once(binary, tf, skipf, cwd, timeout=300, per_test=30):
    env = dict(os.environ, SHIM_SRC=SHIM, SHIM_FILE=tf, SHIM_SKIP=skipf, SHIM_SKIPFORMS=skipf + ".forms",
               EMACS_TEST_DIRECTORY=os.path.dirname(tf), HOME=cwd)
    cpu = os.environ.get("TASKSET_CPU")
    pre = "taskset -c %s " % cpu if cpu else ""
    full = "ulimit -v 8388608; exec %s%s --load %s --" % (pre, binary, DRV)
    p = subprocess.Popen(["bash", "-c", full], cwd=cwd, env=env, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, stdin=subprocess.DEVNULL, preexec_fn=os.setsid)
    t0 = last = time.time(); buf = b""; killed = False; fd = p.stdout.fileno()
    while True:
        r, _, _ = select.select([fd], [], [], 1.0)
        if r:
            d = os.read(fd, 65536)
            if not d:
                break
            buf += d
            if b"\n" in d:
                last = time.time()
        now = time.time()
        if now - t0 > timeout or now - last > (per_test if buf else 120):
            os.killpg(p.pid, signal.SIGKILL); killed = True; break
    p.wait()
    return (124 if killed else p.returncode), buf.decode("utf8", "replace")


def parse(out):
    res = {}; order = []; last_start = None; done = False
    for ln in out.split("\n"):
        f = ln.split("\t")
        if ln.startswith("#DONE"):
            done = True
        elif len(f) == 2 and f[1] == "START":
            last_start = f[0]
        elif len(f) >= 3 and f[1] in ("PASS", "FAIL", "ERROR", "SKIP", "XFAIL", "XPASS"):
            res[f[0]] = f[1]; order.append(f[0]); last_start = None
    return res, order, last_start, done


def main():
    args = sys.argv[1:]
    binary, rel = args[0], args[1]
    expect = None
    if "--expect-pass" in args:
        expect = [l.strip() for l in open(args[args.index("--expect-pass") + 1]) if l.strip()]
    tf = os.path.join(SRC, "test", rel)
    cwd = tempfile.mkdtemp(prefix="ov1-gnutests-")
    skipf = os.path.join(cwd, "skip.txt")
    skips = []; allres = {}
    open(skipf + ".forms", "w").write("")
    for attempt in range(60):
        open(skipf, "w").write("\n".join(skips) + "\n")
        rc, out = run_once(binary, tf, skipf, cwd)
        res, order, last_start, done = parse(out)
        allres.update(res); skips += order
        if done:
            break
        if last_start:
            allres[last_start] = "HANG" if rc == 124 else "CRASH"; skips.append(last_start)
        else:
            break
    counts = {}
    for v in allres.values():
        counts[v] = counts.get(v, 0) + 1
    print(" ".join("%s %d" % (k, counts[k]) for k in sorted(counts)))
    if "--out" in args:
        with open(args[args.index("--out") + 1], "w") as fo:
            for k in sorted(allres):
                fo.write("%s\t%s\n" % (k, allres[k]))
    if expect is not None:
        bad = [n for n in expect if allres.get(n) != "PASS"]
        for n in bad[:20]:
            print("NOT PASS:", n, allres.get(n))
        sys.exit(1 if bad else 0)


main()
