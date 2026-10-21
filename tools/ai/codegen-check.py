#!/usr/bin/env python3
"""Checks for tools/ai/codegen-progress.org (native codegen GCC -O2 parity).

wall NAME     the gate (COMMON.md "Measurement", 2026-10-21 16:30): wall-clock
              ns per iteration of the NeLisp, gcc -O2 GNU-layout and gcc -O2
              NeLisp-layout builds (and, informational, NeLisp built by the
              9d5f4a515 compiler when target/codegen/base-src exists), pinned
              to the lane's CPU (9) after a warm-up, the repeat count set so a
              gcc run takes at least 0.5 s and the zero-iteration run
              subtracted; each of 7 rounds times every variant back to back
              and divides NeLisp by gcc within the round.  Pass iff the median
              per-round ratio NeLisp / gcc -O2 GNU-layout is at most 1.0.
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
plan229       Doc 229 exists and maps the native value convention to GNU's.
lispvalue S   Doc 229 stage S (1, loader or 2): every control of the stage
              fails with the 9d5f4a515 compiler, loader and binary
              (target/codegen/lispvalue/base.json) and every case of the
              stage passes on this tree (target/codegen/lispvalue/cand.json),
              both runs made on the current corpus and the candidate run on
              the current compiler (tools/ai/codegen-lispvalue.py run).
unit-targets  Doc 229 S4.0: the ERT test
              nelisp-aot-compiler/lisp-same-unit-call-entry-per-target passes
              (under the 8 GiB cap): on x86-64 SysV every same-unit call of a
              native artifact enters an internal entry, on Win64 and AArch64
              none calls a unit defun directly (the dispatcher does).
replay-guard  Doc 229: the native replay guard.  nemacs replays a native
              section in process as soon as the runtime defines its two
              replay primitives; while no runtime source (scripts, lisp, src)
              names either one this passes, and once one does it passes only
              if the S4.3-S4.5 stages (word, overflow, signal) pass and
              nelisp-aot-compiler/no-native-replay-before-lisp-fixes passes.
lispvalue-e2e [STAGE...]
              target/codegen/lispvalue-e2e.json: a build of this tree passes
              validate.sh with the Doc 229 stages (default 1 loader 2, or
              the STAGEs given) in the AOT differential,
              and the five kernels compile byte-identically to 1042eedbc's
              compiler (the dialect is untouched).

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


def cpu_freq(cpu):
    """scaling_cur_freq of CPU in kHz, or 0 when unreadable."""
    try:
        return int(Path(f"/sys/devices/system/cpu/cpu{cpu}/cpufreq/scaling_cur_freq").read_text())
    except (OSError, ValueError):
        return 0


def timed(exe, n, r, cpu):
    """Wall-clock seconds of one pinned run, its exit status and the CPU's
    clock (kHz) just before it."""
    import time
    freq = cpu_freq(cpu)
    t0 = time.perf_counter()
    proc = subprocess.run(["taskset", "-c", cpu, str(exe)],
                          input=f"{n:0{len(str(N))}d} {r}\n".encode(), capture_output=True)
    return time.perf_counter() - t0, proc.returncode, freq


def median(xs):
    xs = sorted(xs)
    return xs[len(xs) // 2]


def wall(kernel, rounds=7):
    """Paired wall-clock ratios (COMMON.md "Measurement", 2026-10-21): each
    round times every variant at N and at 0 iterations back to back, turns
    them into ns per iteration and divides NeLisp by each other variant
    within the round; the verdict is the median of those per-round ratios,
    since this machine's clock drops for minutes at a time."""
    bins = build(kernel, with_base=True)
    cpu = idle_cpu()
    # Repeat count: a gcc -O2 GNU-layout run of at least 0.5 s.
    r = 100
    while True:
        t, _, _ = timed(bins["gcc-gnu"], N, r, cpu)
        if t >= 0.5:
            break
        r = int(r * max(2.0, 0.6 / max(t, 1e-3)))
    variants = list(bins)
    for v in variants:  # warm-up
        timed(bins[v], N, r, cpu)
        timed(bins[v], 0, r, cpu)
    per_round = []
    codes = {}
    for _ in range(rounds):
        row = {}
        for v in variants:
            tn, rc_n, f_n = timed(bins[v], N, r, cpu)
            tz, rc_z, f_z = timed(bins[v], 0, r, cpu)
            codes.setdefault("n", set()).add(rc_n)
            codes.setdefault("zero", set()).add(rc_z)
            row[v] = {"ns_per_iter": (tn - tz) / (N * r) * 1e9, "t_n": tn, "t_zero": tz,
                      "khz_n": f_n, "khz_zero": f_z}
        per_round.append(row)
    ratios = {v: median([row["nl"]["ns_per_iter"] / row[v]["ns_per_iter"] for row in per_round])
              for v in variants if v != "nl"}
    result = {"kernel": kernel, "n": N, "repeats": r, "rounds": rounds, "cpu": cpu,
              "results_agree": all(len(c) == 1 for c in codes.values()),
              "ratio_nl_over": ratios,
              "median_ns_per_iter": {v: median([row[v]["ns_per_iter"] for row in per_round])
                                     for v in variants},
              "rounds_detail": per_round,
              "sha256": {v: hashlib.sha256(bins[v].read_bytes()).hexdigest() for v in variants}}
    return result


def cmd_wall(kernel):
    if kernel not in KERNELS:
        raise SystemExit(f"unknown kernel {kernel}")
    r = wall(kernel)
    (ROOT / "target/codegen/wall").mkdir(parents=True, exist_ok=True)
    (ROOT / f"target/codegen/wall/{kernel}.json").write_text(json.dumps(r, indent=1))
    q, m = r["ratio_nl_over"], r["median_ns_per_iter"]
    khz = sorted(row[v]["khz_n"] for row in r["rounds_detail"] for v in row)
    base = f"; x{q['nl-base']:.3f} its 9d5f4a515 build" if "nl-base" in q else ""
    print(f"{kernel}: NeLisp/gcc -O2 GNU layout x{q['gcc-gnu']:.3f}, NeLisp layout "
          f"x{q['gcc-nl']:.3f}{base} (median of {r['rounds']} per-round ratios; "
          f"ns/iter medians nl {m['nl']:.4f} gcc {m['gcc-gnu']:.4f}; {r['repeats']} x {N} "
          f"iterations; cpu {r['cpu']} at {khz[0] // 1000}-{khz[-1] // 1000} MHz); "
          f"results agree: {r['results_agree']}")
    ok = r["results_agree"] and q["gcc-gnu"] <= 1.0
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


LV_FILES = ["lisp/nelisp-aot-compiler.el", "lisp/nelisp-artifact.el",
            "lisp/nelisp-native-load.el", "test/nelisp-aot-differential-driver.el"]
DOC229 = ROOT / "docs/design/229-native-lisp-values-gnu-object-words.org"


def lv_digest(root):
    h = hashlib.sha256()
    for rel in LV_FILES:
        h.update(rel.encode())
        h.update(sha256(Path(root) / rel).encode())
    return h.hexdigest()


def cmd_plan229():
    if not DOC229.exists():
        print(f"FAIL: {DOC229.relative_to(ROOT)} missing")
        return 1
    text = DOC229.read_text()
    want = ["NILP", "Lisp_Object", "raw-bool", "raw-i64", "sexp-ptr", "lisp.h:",
            "bytecode.c:", "comp.c:", "data.c:", "d_reloc", "nelisp_cons_construct",
            "return-repr", "runtime-entry-params", "control", "guard"]
    missing = [w for w in want if w not in text]
    if missing:
        print("FAIL: Doc 229 lacks " + ", ".join(missing))
        return 1
    print("PASS: Doc 229 maps the native value convention to GNU's")
    return 0


def cmd_lispvalue(stage):
    """S3.2-S3.4: the Doc 229 corpus for STAGE, base controls and candidate."""
    recs = {}
    for tag in ("base", "cand"):
        path = ROOT / f"target/codegen/lispvalue/{tag}.json"
        if not path.exists():
            print(f"FAIL: {path.relative_to(ROOT)} missing")
            return 1
        recs[tag] = json.loads(path.read_text())
    base, cand = recs["base"], recs["cand"]
    base_root = ROOT / "target/codegen/base-src"
    text = (ROOT / "scripts/nelisp-aot-differential-cases.el").read_text(encoding="utf-8")
    start = text.index("(defconst nelisp-aot-differential-lisp-value-cases")
    end = text.index("\n", text.index('"Doc 229 controls and guards', start))
    cases_sha = hashlib.sha256(text[start:end].encode("utf-8")).hexdigest()
    problems = []
    try:
        ref = subprocess.run(["git", "-C", str(ROOT), "show",
                              "9d5f4a515:lisp/nelisp-aot-compiler.el"],
                             capture_output=True, check=True).stdout
        if hashlib.sha256(ref).hexdigest() != sha256(base_root / "lisp/nelisp-aot-compiler.el"):
            problems.append("base-src is not 9d5f4a515")
    except subprocess.CalledProcessError:
        problems.append("cannot read 9d5f4a515")
    if Path(base["root"]) != base_root.resolve() or base["compiler_digest"] != lv_digest(base_root):
        problems.append("base run not on base-src")
    if Path(cand["root"]) != ROOT.resolve() or cand["compiler_digest"] != lv_digest(ROOT):
        problems.append("candidate run stale: compiler or loader changed since")
    if base["cases_sha256"] != cases_sha or cand["cases_sha256"] != cases_sha:
        problems.append("corpus changed since a run")
    by_name = {c["name"]: c for c in cand["cases"]}
    mine = [c for c in base["cases"] if str(c["stage"]) == stage]
    if not mine:
        problems.append(f"no cases for stage {stage}")
    controls_ok = guards_ok = cand_ok = 0
    for c in mine:
        got = by_name.get(c["name"], {}).get("status")
        if c["kind"] == "control":
            if c["status"] in ("pass", "not-native"):
                problems.append(f"{c['name']} does not fail on the base ({c['status']})")
            else:
                controls_ok += 1
        elif c["status"] != "pass":
            problems.append(f"guard {c['name']} fails on the base ({c['status']})")
        else:
            guards_ok += 1
        if got == "pass":
            cand_ok += 1
        else:
            problems.append(f"{c['name']} on the candidate: {got}")
    n_controls = sum(1 for c in mine if c["kind"] == "control")
    print(f"stage {stage}: {controls_ok}/{n_controls} controls fail on 9d5f4a515, "
          f"{guards_ok} guards pass there, {cand_ok}/{len(mine)} cases pass on the candidate")
    for p in problems[:12]:
        print("  " + p)
    print("PASS" if not problems else "FAIL")
    return 0 if not problems else 1


def cmd_lispvalue_e2e(required=None):
    path = ROOT / "target/codegen/lispvalue-e2e.json"
    if not path.exists():
        print("FAIL: target/codegen/lispvalue-e2e.json missing")
        return 1
    rec = json.loads(path.read_text())
    ok = True
    binary = Path(rec.get("binary", ""))
    if not binary.exists() or sha256(binary) != rec.get("binary_sha256"):
        print("binary missing or changed since validation")
        ok = False
    if rec.get("tree_digest") != lv_digest(ROOT):
        print("compiler or loader changed since validation")
        ok = False
    for key in ("validate_failing_groups", "standalone_checks_failures"):
        if rec.get(key) != 0:
            print(f"{key} = {rec.get(key)}")
            ok = False
    need = required or ["1", "loader", "2"]
    missing = [st for st in need if st not in (rec.get("lisp_value_stages") or [])]
    if missing:
        print(f"Doc 229 stages missing from the differential: {missing}")
        ok = False
    if rec.get("kernels_identical") is not True:
        print(f"kernel text differs: {rec.get('kernels_identical')}")
        ok = False
    print(json.dumps({k: rec.get(k) for k in (
        "validate_failing_groups", "standalone_checks_failures", "lisp_value_stages",
        "kernels_identical")}))
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


REPLAY_PRIMITIVES = ("nelisp--native-" + "call-boundary",
                     "nelisp--runtime-" + "symbol-address")


def cmd_replay_guard():
    hits = []
    for d in ("scripts", "lisp", "src"):
        for f in sorted((ROOT / d).glob("*.el")):
            text = f.read_text(errors="replace")
            hits += [(p, f.name) for p in REPLAY_PRIMITIVES if p in text]
    if not hits:
        print("no runtime source names a native replay primitive")
        print("PASS")
        return 0
    print("replay primitives named: %s" % ", ".join("%s in %s" % h for h in hits[:4]))
    ok = all(cmd_lispvalue(stage) == 0 for stage in ("word", "overflow", "signal"))
    test = "nelisp-aot-compiler/no-native-replay-before-lisp-fixes"
    cmd = ("ulimit -v 8388608; exec emacs --batch -Q -L lisp -L src "
           "--eval '(setq load-prefer-newer t)' -l ert "
           "-l test/nelisp-aot-compiler-test.el "
           "--eval '(ert-run-tests-batch-and-exit \"%s\")'" % test)
    r = subprocess.run(["bash", "-c", cmd], cwd=ROOT, capture_output=True,
                       text=True, timeout=600)
    ok = ok and r.returncode == 0
    print("PASS" if ok else "FAIL")
    return 0 if ok else 1


def cmd_unit_targets():
    test = "nelisp-aot-compiler/lisp-same-unit-call-entry-per-target"
    cmd = ("ulimit -v 8388608; exec emacs --batch -Q -L lisp -L src "
           "--eval '(setq load-prefer-newer t)' -l ert "
           "-l test/nelisp-aot-compiler-test.el "
           "--eval '(ert-run-tests-batch-and-exit \"%s\")'" % test)
    r = subprocess.run(["bash", "-c", cmd], cwd=ROOT, capture_output=True,
                       text=True, timeout=600)
    ran = [l for l in (r.stdout + r.stderr).splitlines() if l.startswith("Ran ")]
    print(ran[-1] if ran else "no ERT summary")
    print("PASS" if r.returncode == 0 and ran else "FAIL")
    return 0 if r.returncode == 0 and ran else 1


def main(argv):
    if len(argv) == 3 and argv[1] == "kernel":
        return cmd_kernel(argv[2])
    if len(argv) == 3 and argv[1] == "wall":
        return cmd_wall(argv[2])
    if len(argv) == 3 and argv[1] == "lispvalue":
        return cmd_lispvalue(argv[2])
    if len(argv) > 2 and argv[1] == "lispvalue-e2e":
        return cmd_lispvalue_e2e(argv[2:])
    if len(argv) == 2 and argv[1] in ("measure", "plan", "endtoend", "final",
                                      "plan229", "lispvalue-e2e", "unit-targets",
                                      "replay-guard"):
        return {"measure": cmd_measure, "plan": cmd_plan,
                "endtoend": cmd_endtoend, "final": cmd_final,
                "plan229": cmd_plan229, "lispvalue-e2e": cmd_lispvalue_e2e,
                "unit-targets": cmd_unit_targets,
                "replay-guard": cmd_replay_guard}[argv[1]]()
    raise SystemExit(__doc__)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
