#!/usr/bin/env python3
"""Measurements of decision item A against 63ec9640e (tools/ai/symbols-name-word-progress.org, stage S5).

  symbols-name-word-bench.py reader|intern|startup|gc3     measure, write target/snw/logs/bench-MODE.json
  symbols-name-word-bench.py verify reader|intern|startup|gc3|scale   compare with the pre-registered bounds

Baseline and candidate are measured in alternating rounds on one CPU (SNW_CPU, default 13; an idle CPU of
13 12 14 15 10 9 when that one is busy) so a slow stretch of the machine hits both.  The JSON names the sha256
of the four binaries, the CPU and its frequency before and after every run.  The bounds are the ones written
in the ledger header before the first measurement of the candidate; they are ratios candidate / baseline of the
medians of the per-round ratios.  `scale' reads the logs of the at-scale units (scale-dyn, scale-base-dyn,
scale-static, scale-base-static) instead of measuring.
"""
import hashlib
import json
import os
import re
import statistics
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
LOGS = ROOT / "target/snw/logs"
PROBE = ROOT / "tools/ai/symbols-name-word-bench-probe.el"
GNU = os.environ.get("GNU_EMACS", "/usr/local/bin/emacs")
ROUNDS = int(os.environ.get("SNW_ROUNDS", "9"))
STARTUP_RUNS = int(os.environ.get("SNW_STARTUP_RUNS", "31"))


def binaries():
    def get(var, default):
        return Path(os.environ.get(var, str(ROOT / default)))
    return {
        "dyn": get("SNW_DYN", "target/integ/nelisp-dyn"),
        "static": get("SNW_STATIC", "target/integ/nelisp-static"),
        "base-dyn": get("SNW_BASE_DYN", "target/base/nelisp-dyn"),
        "base-static": get("SNW_BASE_STATIC", "target/base/nelisp-static"),
    }


# Pre-registered bounds (ratios candidate / baseline), fixed in the ledger header before any candidate measurement.
BOUNDS = {
    "reader": [("first", 1.10), ("repeat", 1.05)],
    "intern": [("new", 1.10), ("existing", 1.05), ("symbol-name", 1.05)],
    "startup": [("startup", 1.05), ("cold-size", 1.01)],
    "scale": [("scale", 1.10)],
}
GC3_ABSOLUTE = 1.20
GC3_SLACK = 0.05


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def cpu_khz(cpu):
    try:
        return int(Path(f"/sys/devices/system/cpu/cpu{cpu}/cpufreq/scaling_cur_freq").read_text())
    except OSError:
        return None


def cpu_busy(cpu, seconds=1.0):
    """Fraction of the last SECONDS the CPU and its hyperthread sibling spent not idle."""
    def sample():
        rows = {}
        for line in Path("/proc/stat").read_text().splitlines():
            m = re.match(r"cpu(\d+) (.*)", line)
            if m:
                v = [int(x) for x in m.group(2).split()]
                rows[int(m.group(1))] = (sum(v), v[3] + v[4])
        return rows
    a = sample()
    time.sleep(seconds)
    b = sample()
    sibling = (cpu + 16) % 32
    worst = 0.0
    for c in (cpu, sibling):
        if c in a and c in b:
            total = b[c][0] - a[c][0]
            idle = b[c][1] - a[c][1]
            worst = max(worst, 1.0 - idle / total if total else 0.0)
    return worst


def pick_cpu():
    wanted = os.environ.get("SNW_CPU")
    candidates = [int(wanted)] if wanted else [13, 12, 14, 15, 10, 9]
    for cpu in candidates:
        if cpu_busy(cpu) < 0.15:
            return cpu
    sys.exit("FAIL: no idle measurement CPU among %s" % candidates)


def limited():
    import resource
    resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024, 8388608 * 1024))


def probe(binary, mode, cpu):
    env = dict(os.environ, SNW_BENCH=mode, LC_ALL="C.UTF-8")
    before = cpu_khz(cpu)
    proc = subprocess.run(["taskset", "-c", str(cpu), str(binary), "--load", str(PROBE)],
                          env=env, capture_output=True, preexec_fn=limited, timeout=300)
    after = cpu_khz(cpu)
    out = proc.stdout.decode(errors="replace")
    line = next((l for l in out.splitlines() if l.startswith("SNWB " + mode + " ")), None)
    if proc.returncode != 0 or line is None:
        sys.exit("FAIL: %s probe %s rc=%d: %s" % (binary, mode, proc.returncode,
                                                  proc.stderr.decode(errors="replace")[-300:]))
    values = {k: float(v) for k, v in (f.split("=", 1) for f in line.split()[2:])}
    return {"values": values, "khz": [before, after]}


def pinned(bins):
    return {k: {"path": str(v), "sha256": sha(v)} for k, v in bins.items()}


def measure_pairs(mode, cpu):
    bins = binaries()
    rows = []
    for r in range(ROUNDS):
        for linkage in ("dyn", "static"):
            order = [("base-" + linkage), linkage]
            if r % 2:
                order.reverse()
            for which in order:
                row = probe(bins[which], mode, cpu)
                row.update(round=r, binary=which)
                rows.append(row)
    return rows


def measure_startup(cpu):
    bins = binaries()
    rows = []
    for r in range(STARTUP_RUNS):
        for linkage in ("dyn", "static"):
            order = [("base-" + linkage), linkage]
            if r % 2:
                order.reverse()
            for which in order:
                before = cpu_khz(cpu)
                t0 = time.perf_counter()
                proc = subprocess.run(["taskset", "-c", str(cpu), str(bins[which]), "--eval", "1"],
                                      capture_output=True, preexec_fn=limited, timeout=120,
                                      env=dict(os.environ, LC_ALL="C.UTF-8"))
                dt = time.perf_counter() - t0
                if proc.returncode != 0:
                    sys.exit("FAIL: %s --eval 1 rc=%d" % (bins[which], proc.returncode))
                rows.append({"round": r, "binary": which, "seconds": dt, "khz": [before, cpu_khz(cpu)]})
    sizes = {k: {"binary": v.stat().st_size, "cold": Path(str(v) + ".cold").stat().st_size}
             for k, v in bins.items()}
    return rows, sizes


def gc3_worst(binary, cpu):
    """The worst median ratio against GNU of integ-wh-check.sh gc3, on BINARY."""
    env = dict(os.environ, INTEG_WH_DYN=str(binary), GCP_CPU=str(cpu))
    proc = subprocess.run(["bash", str(ROOT / "tools/ai/integ-wh-check.sh"), "gc3"], env=env,
                          capture_output=True, timeout=620)
    log = ROOT / "target/integ-wh/gc3.log"
    text = log.read_text(errors="replace") if log.is_file() else ""
    ratios = [float(x) for x in re.findall(r"median ratio x([0-9.]+)", text)]
    if not ratios:
        sys.exit("FAIL: gc3 on %s printed no ratios: %s" % (binary, proc.stdout.decode(errors="replace")[-300:]))
    return max(ratios), ratios, proc.returncode


def measure_gc3():
    bins = binaries()
    cpu = int(os.environ.get("GCP_CPU", "11"))
    rows = []
    for r in range(int(os.environ.get("SNW_GC3_RUNS", "2"))):
        order = ["base-dyn", "dyn"]
        if r % 2:
            order.reverse()
        for which in order:
            worst, ratios, rc = gc3_worst(bins[which], cpu)
            rows.append({"round": r, "binary": which, "worst": worst, "ratios": ratios, "rc": rc})
            print("gc3 round %d %s worst x%.3f ratios %s rc=%d" % (r, which, worst, ratios, rc), flush=True)
    return rows, cpu


def write_json(mode, payload):
    LOGS.mkdir(parents=True, exist_ok=True)
    payload["mode"] = mode
    payload["bounds"] = BOUNDS.get(mode)
    (LOGS / ("bench-%s.json" % mode)).write_text(json.dumps(payload, indent=1) + "\n")


def run(mode):
    bins = binaries()
    for k, v in bins.items():
        if not v.is_file():
            sys.exit("FAIL: missing binary %s (%s)" % (k, v))
    if mode in ("reader", "intern"):
        cpu = pick_cpu()
        rows = measure_pairs(mode, cpu)
        write_json(mode, {"cpu": cpu, "rounds": ROUNDS, "binaries": pinned(bins), "rows": rows})
        print("measured %s: %d rows on CPU %d" % (mode, len(rows), cpu))
    elif mode == "startup":
        cpu = pick_cpu()
        rows, sizes = measure_startup(cpu)
        write_json(mode, {"cpu": cpu, "runs": STARTUP_RUNS, "binaries": pinned(bins), "rows": rows, "sizes": sizes})
        print("measured startup: %d runs on CPU %d" % (len(rows), cpu))
    elif mode == "gc3":
        rows, cpu = measure_gc3()
        write_json(mode, {"cpu": cpu, "binaries": pinned(bins), "rows": rows})
    else:
        sys.exit("usage: %s reader|intern|startup|gc3 | verify MODE" % sys.argv[0])
    verify(mode)


def paired(rows, key, base, cand, value):
    """Per-round ratios candidate / baseline of VALUE(row)."""
    by = {}
    for row in rows:
        by.setdefault(row["round"], {})[row["binary"]] = row
    out = []
    for r in sorted(by):
        if base in by[r] and cand in by[r]:
            b, c = value(by[r][base]), value(by[r][cand])
            if b > 0:
                out.append(c / b)
    return out


def check_pins(report, bins):
    for k, v in bins.items():
        got = sha(v)
        want = report["binaries"][k]["sha256"]
        if got != want:
            return "%s changed since the measurement (%s, measured %s)" % (k, got[:12], want[:12])
    return None


def verify(mode):
    report_lines = []
    ok = True

    def judge(name, ratios, bound):
        nonlocal ok
        if not ratios:
            report_lines.append("%-22s no data" % name)
            ok = False
            return
        med = statistics.median(ratios)
        good = med <= bound
        ok = ok and good
        report_lines.append("%-22s median x%.3f (min x%.3f max x%.3f, %d rounds) bound x%.2f %s"
                            % (name, med, min(ratios), max(ratios), len(ratios), bound, "ok" if good else "OVER"))

    if mode == "scale":
        bins = binaries()
        for linkage in ("dyn", "static"):
            def total(unit):
                log = LOGS / (unit + ".log")
                if not log.is_file():
                    return None, "no log %s" % log
                text = log.read_text(errors="replace")
                if not text.rstrip().endswith("SNW-UNIT %s PASS" % unit):
                    return None, "%s did not pass" % unit
                head = text.splitlines()[0]
                want = sha(bins[linkage if unit == "scale-" + linkage else "base-" + linkage])
                if (linkage if unit == "scale-" + linkage else "base_" + linkage) + "=" + want not in head:
                    return None, "%s was made with other binaries" % unit
                secs = [float(x.replace(",", ".")) for x in
                        re.findall(r"^(?:generate|check|dump2|verify) rc=0 stderr=0B ([0-9.,]+)s:", text, re.M)]
                if len(secs) != 4:
                    return None, "%s has %d timed phases" % (unit, len(secs))
                return sum(secs), None
            c, why_c = total("scale-" + linkage)
            b, why_b = total("scale-base-" + linkage)
            if c is None or b is None:
                report_lines.append("scale %s: %s" % (linkage, why_c or why_b))
                ok = False
            else:
                judge("scale " + linkage, [c / b], BOUNDS["scale"][0][1])
                report_lines.append("   candidate %.1f s, baseline %.1f s" % (c, b))
    else:
        path = LOGS / ("bench-%s.json" % mode)
        if not path.is_file():
            sys.exit("FAIL: no measurement %s (run: symbols-name-word-check.sh run bench-%s)" % (path, mode))
        report = json.loads(path.read_text())
        bins = binaries()
        why = check_pins(report, bins)
        if why:
            sys.exit("FAIL: " + why)
        report_lines.append("CPU %s, frequency kHz before/after the first and last run: %s / %s" % (
            report["cpu"], report["rows"][0]["khz"], report["rows"][-1]["khz"]))
        for linkage in ("dyn", "static"):
            base, cand = "base-" + linkage, linkage
            rows = report["rows"]
            if mode == "reader":
                judge("reader first " + linkage, paired(rows, "first", base, cand, lambda r: r["values"]["first_ms"]), 1.10)
                judge("reader repeat " + linkage, paired(rows, "repeat", base, cand,
                      lambda r: r["values"]["second_ms"] + r["values"]["third_ms"]), 1.05)
            elif mode == "intern":
                judge("intern new " + linkage, paired(rows, "new", base, cand, lambda r: r["values"]["new_ms"]), 1.10)
                judge("intern existing " + linkage, paired(rows, "existing", base, cand, lambda r: r["values"]["existing_ms"]), 1.05)
                judge("symbol-name " + linkage, paired(rows, "symname", base, cand, lambda r: r["values"]["symname_ms"]), 1.05)
                report_lines.append("   control (symbolp) ratio x%.3f" % statistics.median(
                    paired(rows, "control", base, cand, lambda r: r["values"]["control_ms"])))
            elif mode == "startup":
                judge("startup " + linkage, paired(rows, "startup", base, cand, lambda r: r["seconds"]), 1.05)
                sz = report["sizes"]
                judge("cold image size " + linkage, [sz[cand]["cold"] / sz[base]["cold"]], 1.01)
                report_lines.append("   cold %d B (baseline %d B), binary %d B (baseline %d B)" % (
                    sz[cand]["cold"], sz[base]["cold"], sz[cand]["binary"], sz[base]["binary"]))
            elif mode == "gc3":
                if linkage == "static":
                    continue
                worst_c = max(r["worst"] for r in report["rows"] if r["binary"] == cand)
                worst_b = max(r["worst"] for r in report["rows"] if r["binary"] == base)
                good = worst_c <= GC3_ABSOLUTE and worst_c <= worst_b + GC3_SLACK
                ok = ok and good
                report_lines.append("gc3 worst median ratio against GNU: candidate x%.3f, baseline x%.3f; bounds x%.2f and baseline + %.2f %s"
                                    % (worst_c, worst_b, GC3_ABSOLUTE, GC3_SLACK, "ok" if good else "OVER"))
    LOGS.mkdir(parents=True, exist_ok=True)
    (LOGS / ("bench-%s.verify" % mode)).write_text("\n".join(report_lines) + "\n")
    print("\n".join(report_lines))
    print("BENCH %s %s" % (mode, "PASS" if ok else "FAIL"))
    if not ok:
        sys.exit(1)


def main():
    if len(sys.argv) == 3 and sys.argv[1] == "verify":
        verify(sys.argv[2])
    elif len(sys.argv) == 2:
        run(sys.argv[1])
    else:
        sys.exit(__doc__)


main()
