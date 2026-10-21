#!/usr/bin/env python3
"""Checks for tools/ai/codegen-progress.org (native codegen GCC -O2 parity).

wall NAME     the gate (COMMON.md "Measurement", 2026-10-21 16:30): wall-clock
              ns per iteration of the NeLisp, gcc -O2 GNU-layout and gcc -O2
              NeLisp-layout builds (and, informational, NeLisp built by the
              9d5f4a515 compiler when target/codegen/base-src exists): the
              median of 7 alternating runs pinned to the lane's CPU (9), after a
              warm-up, with the repeat count set so a gcc run takes at least
              0.5 s, the zero-iteration run subtracted.  Pass iff NeLisp <=
              gcc -O2 GNU-layout.
kernel NAME   the Ir diagnostic: Ir per iteration with callgrind over the four
              ABI_LAYOUT_PAD layouts (N-iteration run minus the zero-iteration
              run), with the same verdict rule.
measure       Ir for every kernel as one JSON object (no verdict).
plan          Doc 224 exists and covers every kernel, category and mechanism.
endtoend      target/codegen/endtoend.json (codegen-e2e.py wall) records
              boot, files, regex6000 and the regex per-character pattern
              running faster in wall-clock than the base binary (median
              ratio below 1.0), bound to the candidate binary by sha256.
final         target/codegen/final.json records the final verification.

Kernel sources live in test/perf/codegen-kernels/.  Build products and
callgrind outputs go to target/codegen/kernels/.  Set CODEGEN_EMACS to pick
the host Emacs (default: emacs) and CODEGEN_CC (default: gcc-14).
"""
import hashlib
import json
import os
import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "test/perf/codegen-kernels"
OUT = ROOT / "target/codegen/kernels"
KERNELS = ("byte-scan", "list-walk", "fixnum-loop", "class-table", "vector-sum")
LAYOUTS = (0, 700, 1400, 2100)
N = 100000
VARIANTS = ("nl", "gcc-gnu", "gcc-nl")


def run(args, **kw):
    return subprocess.run(args, check=True, capture_output=True, text=True, **kw)


BASE_TREE = ROOT / "target/codegen/base-src"


def build_nl(kernel, exe, lisp_root):
    emacs = os.environ.get("CODEGEN_EMACS", "emacs")
    if exe.exists():
        exe.unlink()
    run([emacs, "--batch", "-Q", "-L", str(lisp_root / "lisp"), "-L", str(lisp_root / "src"),
         "-l", str(SRC / "compile-kernel.el"), "--eval",
         f'(codegen-kernel-compile "{SRC / (kernel + ".nl.el")}" "{exe}")'])
    if not exe.exists():
        raise RuntimeError(f"{kernel}: NeLisp kernel did not build ({lisp_root})")
    return exe


def build(kernel, with_base=False):
    OUT.mkdir(parents=True, exist_ok=True)
    cc = os.environ.get("CODEGEN_CC", "gcc-14")
    bins = {}
    for variant, define in (("gcc-gnu", "-DREPR_GNU"), ("gcc-nl", "-DREPR_NL")):
        exe = OUT / f"{kernel}.{variant}"
        run([cc, "-O2", define, "-o", str(exe), str(SRC / f"{kernel}.c")])
        bins[variant] = exe
    bins["nl"] = build_nl(kernel, OUT / f"{kernel}.nl", ROOT)
    if with_base and (BASE_TREE / "lisp/nelisp-aot-compiler.el").exists():
        bins["nl-base"] = build_nl(kernel, OUT / f"{kernel}.nl-base", BASE_TREE)
    return bins


def callgrind(exe, n, layout):
    stem = OUT / f"{exe.name}.n{n}.l{layout}"
    env = dict(os.environ, ABI_LAYOUT_PAD="x" * layout)
    proc = subprocess.run(
        ["valgrind", "--tool=callgrind", f"--callgrind-out-file={stem}.cg", str(exe)],
        # Same digit count for N and 0, so the harness's stdin parse costs
        # the same in both runs and only the kernel's iterations remain.
        input=f"{n:0{len(str(N))}d} 1\n", capture_output=True, text=True, env=env)
    totals = None
    for line in open(f"{stem}.cg"):
        if line.startswith("totals:"):
            totals = int(line.split()[1])
    if totals is None:
        raise RuntimeError(f"{stem}: no callgrind totals")
    return totals, proc.returncode


def measure(kernel):
    bins = build(kernel)
    result = {"kernel": kernel, "n": N, "layouts": list(LAYOUTS), "variants": {}}
    codes = {}
    for variant in VARIANTS:
        per_layout = []
        for layout in LAYOUTS:
            ir_n, rc_n = callgrind(bins[variant], N, layout)
            ir_0, rc_0 = callgrind(bins[variant], 0, layout)
            codes.setdefault((N, layout), set()).add(rc_n)
            codes.setdefault((0, layout), set()).add(rc_0)
            per_layout.append((ir_n - ir_0) / N)
        result["variants"][variant] = {
            "ir_per_iter": sum(per_layout) / len(per_layout),
            "per_layout": per_layout,
            "sha256": hashlib.sha256(bins[variant].read_bytes()).hexdigest()}
    result["results_agree"] = all(len(v) == 1 for v in codes.values())
    result["exit_codes"] = sorted({c for v in codes.values() for c in v})
    return result


def idle_cpu():
    """The codegen lane's measurement CPU (COMMON.md "Measurement": codegen 9;
    heavy jobs stay on 0-7 and 16-23).  CODEGEN_CPU overrides it."""
    return os.environ.get("CODEGEN_CPU", "9")


def timed(exe, n, r, cpu):
    """Wall-clock seconds of one pinned run and its exit status."""
    import time
    t0 = time.perf_counter()
    proc = subprocess.run(["taskset", "-c", cpu, str(exe)], input=f"{n:0{len(str(N))}d} {r}\n",
                          capture_output=True, text=True)
    return time.perf_counter() - t0, proc.returncode


def median(xs):
    xs = sorted(xs)
    return xs[len(xs) // 2]


def wall(kernel, rounds=7):
    bins = build(kernel, with_base=True)
    cpu = idle_cpu()
    # Repeat count: a gcc -O2 GNU-layout run of at least 0.5 s.
    r = 100
    while True:
        t, _ = timed(bins["gcc-gnu"], N, r, cpu)
        if t >= 0.5:
            break
        r = int(r * max(2.0, 0.6 / max(t, 1e-3)))
    variants = list(bins)
    for v in variants:  # warm-up
        timed(bins[v], N, r, cpu)
        timed(bins[v], 0, r, cpu)
    samples = {v: {"n": [], "zero": []} for v in variants}
    codes = {}
    for _ in range(rounds):
        for v in variants:
            t, rc = timed(bins[v], N, r, cpu)
            samples[v]["n"].append(t)
            codes.setdefault("n", set()).add(rc)
            t, rc = timed(bins[v], 0, r, cpu)
            samples[v]["zero"].append(t)
            codes.setdefault("zero", set()).add(rc)
    result = {"kernel": kernel, "n": N, "repeats": r, "rounds": rounds, "cpu": cpu,
              "results_agree": all(len(c) == 1 for c in codes.values()), "variants": {}}
    for v in variants:
        mn, mz = median(samples[v]["n"]), median(samples[v]["zero"])
        result["variants"][v] = {
            "ns_per_iter": (mn - mz) / (N * r) * 1e9,
            "median_s": mn, "median_zero_s": mz,
            "samples_s": samples[v]["n"], "samples_zero_s": samples[v]["zero"],
            "sha256": hashlib.sha256(bins[v].read_bytes()).hexdigest()}
    return result


def cmd_wall(kernel):
    if kernel not in KERNELS:
        raise SystemExit(f"unknown kernel {kernel}")
    r = wall(kernel)
    (ROOT / "target/codegen/wall").mkdir(parents=True, exist_ok=True)
    (ROOT / f"target/codegen/wall/{kernel}.json").write_text(json.dumps(r, indent=1))
    v = r["variants"]
    base = (f"; NeLisp at 9d5f4a515 {v['nl-base']['ns_per_iter']:.4f}"
            if "nl-base" in v else "")
    print(f"{kernel}: nelisp {v['nl']['ns_per_iter']:.4f} ns/iter; gcc -O2 GNU layout "
          f"{v['gcc-gnu']['ns_per_iter']:.4f}; gcc -O2 NeLisp layout "
          f"{v['gcc-nl']['ns_per_iter']:.4f}{base}  (median of {r['rounds']}, "
          f"{r['repeats']} x {N} iterations, cpu {r['cpu']}); results agree: {r['results_agree']}")
    ok = r["results_agree"] and v["nl"]["ns_per_iter"] <= v["gcc-gnu"]["ns_per_iter"]
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


def cmd_kernel(kernel):
    if kernel not in KERNELS:
        raise SystemExit(f"unknown kernel {kernel}")
    r = measure(kernel)
    v = r["variants"]
    nl, gnu, nlc = (v[k]["ir_per_iter"] for k in ("nl", "gcc-gnu", "gcc-nl"))
    print(f"{kernel}: nelisp {nl:.3f} Ir/iter; gcc -O2 GNU layout {gnu:.3f}; "
          f"gcc -O2 NeLisp layout {nlc:.3f}; results agree: {r['results_agree']}")
    ok = r["results_agree"] and nl <= gnu + 1e-9
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


def cmd_measure():
    print(json.dumps([measure(k) for k in KERNELS], indent=1))
    return 0


DOC = ROOT / "docs/design/224-native-codegen-gcc-parity.org"
CATEGORIES = ("tag check", "boxing", "redundant load", "call overhead",
              "bounds check", "branch structure")
MECHANISMS = ("register allocation", "type inference", "hoist", "inlin")


def cmd_plan():
    if not DOC.exists():
        print(f"FAIL: {DOC.relative_to(ROOT)} missing")
        return 1
    text = DOC.read_text().lower()
    missing = [k for k in KERNELS if k not in text]
    missing += [c for c in CATEGORIES if c not in text]
    missing += [m for m in MECHANISMS if m not in text]
    if "gcc" not in text or not re.search(r"ir per iteration|ir/iter", text):
        missing.append("gcc Ir-per-iteration table")
    if missing:
        print("FAIL: Doc 224 lacks " + ", ".join(missing))
        return 1
    print("PASS: Doc 224 covers 5 kernels, 6 categories and the mechanisms")
    return 0


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def cmd_endtoend():
    """S2.1: target/codegen/endtoend.json (tools/ai/codegen-e2e.py wall).

    Each workload's median wall-clock ratio candidate/base must be below 1.0
    on the candidate binary the record names (by sha256); the record also
    carries each ratio to GNU 31.1 for the NOTE."""
    path = ROOT / "target/codegen/endtoend.json"
    if not path.exists():
        print("FAIL: target/codegen/endtoend.json missing")
        return 1
    rec = json.loads(path.read_text())
    cand = Path(rec["candidate"])
    if not cand.exists() or sha256(cand) != rec["candidate_sha256"]:
        print("FAIL: candidate binary missing or changed since the measurement")
        return 1
    bad = [w for w, m in rec["workloads"].items() if not m["ratio_base"] < 1.0]
    for w, m in rec["workloads"].items():
        print(f"{w}: base {m['base']:.4g} -> candidate {m['candidate']:.4g} "
              f"(x{m['ratio_base']:.3f} base, x{m['ratio_gnu']:.3f} GNU)")
    if not rec.get("corpus_identical") or bad:
        print(f"FAIL: not faster {bad} corpus_identical={rec.get('corpus_identical')}")
        return 1
    print("PASS")
    return 0


def cmd_final():
    path = ROOT / "target/codegen/final.json"
    if not path.exists():
        print("FAIL: target/codegen/final.json missing")
        return 1
    rec = json.loads(path.read_text())
    ok = (rec.get("checks_groups") == 11 and rec.get("checks_failures") == 0
          and rec.get("standalone_checks_rc") == 0 and rec.get("mcp_tools") == 40)
    for key in ("dynamic", "static"):
        p = Path(rec.get(key, ""))
        if not p.exists() or sha256(p) != rec.get(key + "_sha256"):
            ok = False
            print(f"{key} binary missing or changed")
    print(json.dumps({k: rec.get(k) for k in (
        "checks_groups", "checks_failures", "standalone_checks_rc", "mcp_tools")}))
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


def main(argv):
    if len(argv) == 3 and argv[1] == "kernel":
        return cmd_kernel(argv[2])
    if len(argv) == 3 and argv[1] == "wall":
        return cmd_wall(argv[2])
    if len(argv) == 2 and argv[1] in ("measure", "plan", "endtoend", "final"):
        return {"measure": cmd_measure, "plan": cmd_plan,
                "endtoend": cmd_endtoend, "final": cmd_final}[argv[1]]()
    raise SystemExit(__doc__)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
