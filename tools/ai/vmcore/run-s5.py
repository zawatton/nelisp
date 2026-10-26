import os, sys, subprocess, re, json, time
O = os.environ["S5_OUT"]; S5 = "/home/madblack-21/Cowork/Notes/dev/nelisp/target/agent-worktrees/resume-4943-20261024/target/survey5"
NL = os.environ["NL_BIN"]
E = "/home/madblack-21/Downloads/emacs-31.1/test"
SHIM = S5 + "/shim.el"; DRV = S5 + "/drv.el"
def run_once(engine, tf, skipf, cwd, timeout, per_test=30):
    import select, signal
    env = dict(os.environ, SHIM_SRC=SHIM, SHIM_FILE=tf, SHIM_SKIP=skipf, SHIM_SKIPFORMS=skipf + ".forms", EMACS_TEST_DIRECTORY=os.path.dirname(tf), HOME=cwd)
    if engine == "gnu":
        cmd = "emacs -Q --batch -l %s" % DRV
    else:
        cmd = "%s --load %s --" % (NL, DRV)
    full = "ulimit -v 8388608; exec taskset -c %s %s" % (os.environ.get("S5_CPU", "11"), cmd)
    errf = open(cwd + "/stderr.txt", "w+b")
    p = subprocess.Popen(["bash", "-c", full], cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=errf, stdin=subprocess.DEVNULL, preexec_fn=os.setsid)
    t0 = time.time(); last = t0; buf = b""; killed = False
    fd = p.stdout.fileno()
    while True:
        r, _, _ = select.select([fd], [], [], 1.0)
        if r:
            d = os.read(fd, 65536)
            if not d: break
            buf += d
            if b"\n" in d: last = time.time()
        now = time.time()
        if now - t0 > timeout or now - last > (per_test if buf else 120):
            os.killpg(p.pid, signal.SIGKILL); killed = True; break
    p.wait()
    errf.seek(0); err = errf.read().decode("utf8", "replace"); errf.close()
    rc = 124 if killed else p.returncode
    return rc, buf.decode("utf8", "replace"), err, time.time() - t0
def parse(out):
    res = {}; order = []; last_start = None; last_eval = None; loadfail = None; done = False
    for ln in out.split("\n"):
        f = ln.split("\t")
        if ln.startswith("#READFAIL") or ln.startswith("#FORMFAIL"):
            loadfail = (loadfail + " ;; " if loadfail else "") + " ".join(f[:3])[:400]
        elif ln.startswith("#DONE"): done = True
        elif ln.startswith("#EVAL"): last_eval = (f[1], f[2] if len(f) > 2 else "")
        elif ln.startswith("#LOADED"): last_eval = None
        elif len(f) == 2 and f[1] == "START": last_start = f[0]
        elif len(f) >= 3 and f[1] in ("PASS","FAIL","ERROR","SKIP","XFAIL","XPASS"):
            res[f[0]] = (f[1], f[2], int(f[3]) if len(f) > 3 and f[3].isdigit() else 0); order.append(f[0]); last_start = None
    return res, order, last_start, loadfail, done, last_eval
def run_engine(engine, tf, base):
    cwd = "%s/work/run-%s" % (O, engine); os.makedirs(cwd, exist_ok=True)
    skipf = "%s/work/skip-%s.txt" % (O, engine)
    skips = []; skipforms = []; allres = {}; order = []; loadfail = None; log = ""; wall = 0
    open(skipf + ".forms", "w").write("")
    for attempt in range(60):
        open(skipf, "w").write("\n".join(skips) + "\n")
        rc, out, err, w = run_once(engine, tf, skipf, cwd, 300); wall += w
        log += "--- attempt %d rc=%d wall=%.1f\n%s\n%s\n" % (attempt, rc, w, out[-3000:] if rc else "", err[-1500:])
        res, od, ls, lf, done, le = parse(out)
        allres.update(res); order += od; skips += od
        if lf and not loadfail: loadfail = lf
        if done: break
        if le and not ls:
            loadfail = (loadfail + " ;; " if loadfail else "") + ("#LOAD%s form %s %s rc=%d" % ("HANG" if rc == 124 else "CRASH", le[0], le[1], rc))[:300]
            skipforms.append(le[0]); open(skipf + ".forms", "w").write("\n".join(skipforms) + "\n")
            continue
        if ls:
            kind = "HANG" if rc == 124 else "CRASH"
            allres[ls] = (kind, "rc=%d %s" % (rc, err.strip().split("\n")[-1][:150] if err.strip() else ""), 0)
            order.append(ls); skips.append(ls)
        else:
            if not res:
                allres["#FILE"] = ("NORUN", "rc=%d %s" % (rc, (err.strip() or out.strip())[-300:]), 0)
            break
    open("%s/logs/%s-%s.log" % (O, base, engine), "w").write(log)
    return allres, order, loadfail, wall
def real_ert(tf):
    cwd = O + "/work/run-gnu"
    env = dict(os.environ, EMACS_TEST_DIRECTORY=os.path.dirname(tf), HOME=cwd)
    full = "ulimit -v 8388608; exec timeout 300 taskset -c %s emacs -Q --batch -l ert -l %s -f ert-run-tests-batch" % (os.environ.get("S5_CPU", "11"), tf)
    p = subprocess.run(["bash","-c",full], cwd=cwd, env=env, capture_output=True, stdin=subprocess.DEVNULL)
    out = p.stdout.decode("utf8","replace") + p.stderr.decode("utf8","replace")
    r = {}
    for m in re.finditer(r"^\s+(passed|FAILED|failed|SKIPPED|skipped|XFAIL|XPASS|xfail|xpass)\s+\d+/\d+\s+(\S+)", out, re.M):
        r[m.group(2)] = m.group(1).upper()
    return r
if __name__ == "__main__":
    os.makedirs(O + "/logs", exist_ok=True)
    rows = []
    for rel in sys.argv[1:]:
        tf = rel if rel.startswith("/") else E + "/" + rel
        base = rel.replace("/", "_").replace(".el", "")
        g, go, glf, gw = run_engine("gnu", tf, base)
        n, no, nlf, nw = run_engine("nl", tf, base)
        re_ = real_ert(tf)
        names = list(go) + [x for x in no if x not in g]
        with open(O + "/results.tsv", "a") as fo:
            for nm in names:
                gs = g.get(nm, ("-","",0)); ns = n.get(nm, ("NOTLOADED" if nlf else "MISSING", "",0))
                fo.write("\t".join([rel, nm, gs[0], gs[1][:150], ns[0], ns[1][:150], str(gs[2]), str(ns[2]), re_.get(nm, "-")]) + "\n")
        with open(O + "/files.tsv", "a") as ff:
            ff.write("\t".join([rel, str(len(go)), str(len(no)), (glf or "")[:200], (nlf or "")[:200], "%.1f" % gw, "%.1f" % nw]) + "\n")
        print(rel, len(go), len(no), "gnu_load:", bool(glf), "nl_load:", (nlf or "")[:100], "wall %.0f/%.0f" % (gw, nw), flush=True)
