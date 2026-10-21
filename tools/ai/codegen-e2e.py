#!/usr/bin/env python3
"""End-to-end Ir for tools/ai/codegen-progress.org S2.1.

run WHO BINARY WORKLOAD [--jobs N]
    Profile WORKLOAD under callgrind for BINARY (WHO is base or cand) at
    the four ABI_LAYOUT_PAD layouts and store the totals in
    target/codegen/e2e/WORKLOAD-WHO.json.  WORKLOAD is boot, files or regex
    (the abi-ir.py workloads: `--eval nil', tools/ai/abi-file-exec.el and the
    6000-match regex loop), or regexchar (the Doc 218 per-character class
    pattern at 16 and 128 characters).  Run one workload per heavy slot.
wall CAND BASE [CORPUS_LOG]
    The S2.1 gate (COMMON.md "Measurement", 2026-10-21): wall-clock of
    boot, files, regex and regexchar (16 and 128 characters) for CAND, BASE
    and GNU 31.1 running the same Lisp (`emacs -Q --batch'), pinned to the
    lane's CPU (9) after a warm-up; each of 7 rounds runs the three back to
    back and the ratios are the medians of the per-round ratios, written to
    target/codegen/endtoend.json; regex-per-character is the slope between
    16 and 128 characters.
corpus LOG
    Record in target/codegen/endtoend.json whether the validate.sh run in
    LOG (standalone corpus, bytecode and native suites, AOT differential)
    passed on the measured candidate.
summarize CAND [CORPUS_LOG]
    Combine the stored workloads into target/codegen/endtoend.json, bound
    to CAND by sha256; CORPUS_LOG is a validate.sh output whose result says
    whether the standalone corpus, bytecode and native suites passed.
"""
import concurrent.futures
import hashlib
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/codegen/e2e"
LAYOUTS = (0, 700, 1400, 2100)
TIMEOUTS = {"boot": 600, "files": 900, "regex": 3600, "regexchar": 900}
REGEX_EL = r'''
(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i 20)
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))))
(princ "ABI-REGEX 6000\n")
'''
# Doc 218 "Literal and class cost per character": a repeated class node
# over the whole subject.  200 matches per run; the slope between 16 and
# 128 characters is the per-character cost.
REGEXCHAR_EL = r'''
(let ((s (make-string LEN ?a)) (i 0) (hits 0))
  (while (< i COUNT)
    (when (string-match "\\`[a-z0-9_]*\\'" s) (setq hits (1+ hits)))
    (setq i (1+ i)))
  (princ (format "REGEXCHAR LEN %d\n" hits)))
'''
MARKERS = {"files": "ABI-FILES (1000 527500", "regex": "ABI-REGEX 6000"}


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def profile(binary, args, layout, tmp, stem, timeout, marker):
    cache = tmp / f"{stem}-cache"
    cache.mkdir()
    env = dict(os.environ, XDG_CACHE_HOME=str(cache), TMPDIR=str(tmp),
               ABI_SOURCE_ROOT=str(ROOT), ABI_LAYOUT_PAD="x" * layout)
    cg = tmp / f"{stem}.cg"
    proc = subprocess.run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                           f"--callgrind-out-file={cg}", str(binary), *args],
                          cwd=ROOT, env=env, capture_output=True, text=True,
                          timeout=timeout)
    if proc.returncode != 0:
        raise RuntimeError(f"{stem}: exit {proc.returncode}: {proc.stderr[-400:]}")
    if marker and marker not in proc.stdout:
        raise RuntimeError(f"{stem}: marker {marker!r} missing")
    for line in cg.open():
        if line.startswith("totals:"):
            total = int(line.split()[1])
            cg.unlink()
            return total
    raise RuntimeError(f"{stem}: no totals")


def cmd_run(who, binary, workload, jobs):
    OUT.mkdir(parents=True, exist_ok=True)
    tmp = Path(tempfile.mkdtemp(prefix=f"e2e-{workload}-", dir=ROOT / "target/codegen"))
    tasks = []
    if workload == "boot":
        variants = {"": (["--eval", "nil"], None)}
    elif workload == "files":
        variants = {"": (["--load", str(ROOT / "tools/ai/abi-file-exec.el")], MARKERS["files"])}
    elif workload == "regex":
        (tmp / "regex.el").write_text(REGEX_EL)
        variants = {"": (["--load", str(tmp / "regex.el")], MARKERS["regex"])}
    elif workload == "regexchar":
        variants = {}
        for length in (16, 128):
            f = tmp / f"regexchar-{length}.el"
            f.write_text(REGEXCHAR_EL.replace("LEN", str(length)).replace("COUNT", "200"))
            variants[str(length)] = (["--load", str(f)], f"REGEXCHAR {length} 200")
    else:
        raise SystemExit(f"unknown workload {workload}")
    for var, (args, marker) in variants.items():
        for layout in LAYOUTS:
            stem = f"{workload}{var}-{who}-{layout}"
            tasks.append((who, var, layout, binary, args, stem, marker))
    results = {}
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        futs = {pool.submit(profile, t[3], t[4], t[2], tmp, t[5], TIMEOUTS[workload], t[6]): t
                for t in tasks}
        for fut in concurrent.futures.as_completed(futs):
            who, var, layout = futs[fut][:3]
            results.setdefault(who, {}).setdefault(var, {})[str(layout)] = fut.result()
    rec = {"workload": workload, "layouts": list(LAYOUTS), "who": who,
           "binary": str(Path(binary).resolve()), "sha256": sha256(binary),
           "totals": results[who]}
    (OUT / f"{workload}-{who}.json").write_text(json.dumps(rec, indent=1))
    for var, per in results[who].items():
        print(f"{workload}{var} {who}: {sum(per.values()) / len(per):.1f} Ir (mean of 4 layouts)")
    return 0


def mean(per):
    return sum(per.values()) / len(per)


def load(workload, who, cand):
    rec = json.loads((OUT / f"{workload}-{who}.json").read_text())
    if who == "cand" and rec["sha256"] != sha256(cand):
        raise SystemExit(f"{workload}: measured a different candidate binary")
    return rec["totals"]


def cmd_summarize(cand, corpus_log=None):
    workloads = {}
    for w in ("boot", "files", "regex"):
        workloads[w] = {"base_ir": mean(load(w, "base", cand)[""]),
                        "candidate_ir": mean(load(w, "cand", cand)[""])}
    totals = {who: load("regexchar", who, cand) for who in ("base", "cand")}
    slope = {who: (mean(totals[who]["128"]) - mean(totals[who]["16"]))
             / (112 * 200) for who in ("base", "cand")}
    workloads["regex-per-character"] = {"base_ir": slope["base"],
                                        "candidate_ir": slope["cand"]}
    corpus = None
    if corpus_log:
        text = Path(corpus_log).read_text()
        corpus = "VALIDATE" in text and ": 0 failing groups" in text
    out = {"candidate": str(Path(cand).resolve()), "candidate_sha256": sha256(cand),
           "base_commit": "9d5f4a515", "workloads": workloads,
           "corpus_identical": corpus, "corpus_log": corpus_log}
    (ROOT / "target/codegen/endtoend.json").write_text(json.dumps(out, indent=1))
    for w, m in workloads.items():
        print(f"{w}: {m['base_ir']:.2f} -> {m['candidate_ir']:.2f} Ir "
              f"({100.0 * (m['candidate_ir'] / m['base_ir'] - 1):+.3f}%)")
    print(f"corpus identical: {corpus}")
    return 0


GNU = os.environ.get("CODEGEN_GNU", "/usr/local/bin/emacs")
GNU_REGEXCHAR_COUNT = 200000


def idle_cpu():
    """The codegen lane's measurement CPU (COMMON.md "Measurement": codegen 9;
    heavy jobs stay on 0-7 and 16-23).  CODEGEN_CPU overrides it."""
    return os.environ.get("CODEGEN_CPU", "9")


def wall_args(who, binary, workload, files):
    """Command line for one wall-clock run of WORKLOAD."""
    if who == "gnu":
        base = [GNU, "-Q", "--batch"]
        f = files.get(f"{workload}-gnu", files.get(workload))
        return base + (["--eval", "nil"] if workload == "boot" else ["-l", str(f)])
    return [str(binary)] + (["--eval", "nil"] if workload == "boot"
                            else ["--load", str(files[workload])])


def cmd_wall(cand, base, corpus_log=None, rounds=7):
    import time
    tmp = Path(tempfile.mkdtemp(prefix="e2e-wall-", dir=ROOT / "target/codegen"))
    files = {"files": ROOT / "tools/ai/abi-file-exec.el", "regex": tmp / "regex.el"}
    (tmp / "regex.el").write_text(REGEX_EL)
    for length in (16, 128):
        f = tmp / f"regexchar-{length}.el"
        f.write_text(REGEXCHAR_EL.replace("LEN", str(length)).replace("COUNT", "200"))
        files[f"regexchar{length}"] = f
        # GNU's per-character cost is a few ns: 200 matches would put the
        # 16-to-128 difference inside process noise, so GNU runs 1000 times
        # as many and its slope divides by its own count.
        g = tmp / f"regexchar-{length}-gnu.el"
        g.write_text(REGEXCHAR_EL.replace("LEN", str(length)).replace("COUNT", str(GNU_REGEXCHAR_COUNT)))
        files[f"regexchar{length}-gnu"] = g
    cpu = idle_cpu()
    runtimes = {"base": base, "cand": cand, "gnu": None}
    workloads = ["boot", "files", "regex", "regexchar16", "regexchar128"]
    markers = {"files": "ABI-FILES (1000 527500", "regex": "ABI-REGEX 6000",
               "regexchar16": "REGEXCHAR 16 200", "regexchar128": "REGEXCHAR 128 200",
               "regexchar16-gnu": f"REGEXCHAR 16 {GNU_REGEXCHAR_COUNT}",
               "regexchar128-gnu": f"REGEXCHAR 128 {GNU_REGEXCHAR_COUNT}"}

    def freq():
        try:
            return int(Path(f"/sys/devices/system/cpu/cpu{cpu}/cpufreq/scaling_cur_freq").read_text())
        except (OSError, ValueError):
            return 0

    def one(who, w):
        cache = Path(tempfile.mkdtemp(prefix="c-", dir=tmp))
        env = dict(os.environ, XDG_CACHE_HOME=str(cache), TMPDIR=str(tmp),
                   ABI_SOURCE_ROOT=str(ROOT))
        khz = freq()
        t0 = time.perf_counter()
        proc = subprocess.run(["taskset", "-c", cpu] + wall_args(who, runtimes[who], w, files),
                              cwd=ROOT, env=env, capture_output=True, text=True, timeout=600)
        t = time.perf_counter() - t0
        out = proc.stdout + proc.stderr
        key = f"{w}-gnu" if who == "gnu" and f"{w}-gnu" in markers else w
        if proc.returncode != 0 or (key in markers and markers[key] not in out):
            raise RuntimeError(f"{who} {w}: exit {proc.returncode}: {out[-300:]}")
        return t, khz

    for w in workloads:
        for who in runtimes:
            one(who, w)  # warm-up
    # Paired ratios (COMMON.md "Measurement"): every round times all three
    # runtimes on a workload back to back; the verdict is the median of the
    # per-round ratios, since the clock drops for minutes at a time.
    rows = []
    for _ in range(rounds):
        row = {}
        for w in workloads:
            row[w] = {}
            for who in runtimes:
                t, khz = one(who, w)
                row[w][who] = {"s": t, "khz": khz}
        rows.append(row)

    def med(xs):
        xs = sorted(xs)
        return xs[len(xs) // 2]

    def value(row, w, who):
        if w == "regex-per-character":
            count = GNU_REGEXCHAR_COUNT if who == "gnu" else 200
            return (row["regexchar128"][who]["s"] - row["regexchar16"][who]["s"]) / (112 * count) * 1e9
        return row[w][who]["s"]
    result = {}
    for w, unit in (("boot", "s"), ("files", "s"), ("regex", "s"), ("regex-per-character", "ns/char")):
        result[w] = {
            "base": med([value(r, w, "base") for r in rows]),
            "candidate": med([value(r, w, "cand") for r in rows]),
            "gnu": med([value(r, w, "gnu") for r in rows]),
            "ratio_base": med([value(r, w, "cand") / value(r, w, "base") for r in rows]),
            "ratio_gnu": med([value(r, w, "cand") / value(r, w, "gnu") for r in rows]),
            "unit": unit}
    khz = sorted(r[w][who]["khz"] for r in rows for w in workloads for who in runtimes)
    corpus = None
    if corpus_log:
        text = Path(corpus_log).read_text()
        corpus = ": 0 failing groups" in text
    out = {"candidate": str(Path(cand).resolve()), "candidate_sha256": sha256(cand),
           "base": str(Path(base).resolve()), "base_sha256": sha256(base),
           "gnu": GNU, "cpu": cpu, "rounds": rounds, "base_commit": "9d5f4a515",
           "workloads": result, "rounds_detail": rows,
           "mhz_range": [khz[0] // 1000, khz[-1] // 1000],
           "corpus_identical": corpus, "corpus_log": corpus_log}
    (ROOT / "target/codegen/endtoend.json").write_text(json.dumps(out, indent=1))
    for w, m in result.items():
        print(f"{w}: base {m['base']:.4g} cand {m['candidate']:.4g} gnu {m['gnu']:.4g} {m['unit']}"
              f"  x{m['ratio_base']:.3f} base, x{m['ratio_gnu']:.3f} GNU")
    print(f"corpus identical: {corpus}; cpu {cpu} at {khz[0] // 1000}-{khz[-1] // 1000} MHz; "
          f"medians of {rounds} per-round ratios")
    return 0


def cmd_corpus(log):
    path = ROOT / "target/codegen/endtoend.json"
    rec = json.loads(path.read_text())
    text = Path(log).read_text()
    rec["corpus_identical"] = ": 0 failing groups" in text
    rec["corpus_log"] = str(Path(log).resolve())
    path.write_text(json.dumps(rec, indent=1))
    print(f"corpus identical: {rec['corpus_identical']}")
    return 0


def main(argv):
    if len(argv) >= 5 and argv[1] == "run":
        jobs = int(argv[6]) if len(argv) == 7 and argv[5] == "--jobs" else 4
        if argv[2] not in ("base", "cand"):
            raise SystemExit(__doc__)
        return cmd_run(argv[2], Path(argv[3]), argv[4], jobs)
    if len(argv) == 3 and argv[1] == "corpus":
        return cmd_corpus(argv[2])
    if len(argv) in (4, 5) and argv[1] == "wall":
        return cmd_wall(Path(argv[2]), Path(argv[3]), argv[4] if len(argv) == 5 else None)
    if len(argv) in (3, 4) and argv[1] == "summarize":
        return cmd_summarize(Path(argv[2]), argv[3] if len(argv) == 4 else None)
    raise SystemExit(__doc__)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
