#!/usr/bin/env python3
"""Run a command and record wall time and peak memory of its process tree.

usage: run-measured.py LOGFILE -- CMD...
Every 5 s the resident set of all descendants is summed (from /proc).  The
kernel's own figure for the largest single descendant (ru_maxrss of the waited
children) is printed at the end.  The sampler is part of this process, so it
cannot outlive the command.
"""
import os, resource, subprocess, sys, time

def children_map():
    m = {}
    for d in os.listdir("/proc"):
        if not d.isdigit():
            continue
        try:
            with open(f"/proc/{d}/stat") as f:
                s = f.read()
            ppid = int(s[s.rindex(")") + 2:].split()[1])
            m.setdefault(ppid, []).append(int(d))
        except (OSError, ValueError):
            continue  # process vanished while scanning
    return m

def rss_kb(pid):
    try:
        with open(f"/proc/{pid}/status") as f:
            for line in f:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1])
    except OSError:
        return 0  # process vanished
    return 0

def comm(pid):
    try:
        with open(f"/proc/{pid}/comm") as f:
            return f.read().strip()
    except OSError:
        return "?"

def main():
    log = sys.argv[1]
    cmd = sys.argv[sys.argv.index("--") + 1:]
    t0 = time.time()
    p = subprocess.Popen(cmd)
    peak_sum = 0; peak_one = 0; peak_one_comm = ""; peak_n = 0; samples = 0
    with open(log, "w") as lf:
        while p.poll() is None:
            cm = children_map(); stack = [p.pid]; tree = []
            while stack:
                x = stack.pop(); tree.append(x); stack += cm.get(x, [])
            rs = [(rss_kb(x), x) for x in tree]
            tot = sum(r for r, _ in rs)
            big = max(rs) if rs else (0, 0)
            n = sum(1 for _, x in rs if comm(x).startswith("emacs"))
            if tot > peak_sum: peak_sum = tot; peak_n = n
            if big[0] > peak_one: peak_one = big[0]; peak_one_comm = comm(big[1])
            samples += 1
            lf.write(f"{time.time()-t0:.0f}s total_kb={tot} procs={len(tree)} emacs={n} largest_kb={big[0]}\n")
            lf.flush()
            try:
                p.wait(timeout=5)
            except subprocess.TimeoutExpired:
                pass  # next sample
        wall = time.time() - t0
        ru = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
        lf.write(f"DONE rc={p.returncode} wall_s={wall:.0f} samples={samples} "
                 f"sampled_peak_total_kb={peak_sum} (emacs procs then: {peak_n}) "
                 f"sampled_peak_single_kb={peak_one} ({peak_one_comm}) kernel_maxrss_single_kb={ru}\n")
    sys.exit(p.returncode)

main()
