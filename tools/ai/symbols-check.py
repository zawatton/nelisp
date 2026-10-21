#!/usr/bin/env python3
"""Checker for tools/ai/symbols-progress.org (Doc 226, GNU struct Lisp_Symbol).

Subcommands (one per ledger criterion):
  plan              S0.1  Doc 226 maps operations to GNU sources, holds the
                          mirror-table census and the LOCALIZED interop plan
  layout            S1.1  header-less symbol objects, bytes per symbol
  census            S1.2  no mirror-table lookups on PLAINVAL/VARALIAS/FORWARDED
  semantics         S1.3  GNU 31.1 oracle fixture on dynamic and static
  speed globals     S2.1  own cost of symbol-value/set/boundp vs GNU
  speed workloads   S2.2  .elc mxlib benchmark and interpreted calls vs base
  final             S3.1  final verification evidence
Helpers:
  census-static [REV]     print the static mirror-table census (working tree,
                          or the git revision REV)
"""
import bisect
import collections
import hashlib
import json
import os
import re
import resource
import statistics
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/symbols"
DOC = ROOT / "docs/design/226-gnu-symbol-port.org"
GNU_SRC = Path("/home/madblack-21/Downloads/emacs-31.1/src")
# The census in Doc 226 is the "before" state: the merge of callpath-perf's
# direct cells onto the symbols base, before any change of this lane.
BEFORE_REV = "915e1a17a"


def need(cond, msg):
    if not cond:
        print("FAIL: " + msg, file=sys.stderr)
        sys.exit(1)


# ---------------------------------------------------------------- census
# A "mirror-table use" is a call site of a function of the env mirror (the
# name-keyed fast-hash-table of symbol-entry records, lisp/nelisp-cc-mirror-*)
# or of the name hash it uses, in a native runtime function outside the
# mirror module.  Callers are grouped by subsystem from their name prefix.
MIRROR_API = re.compile(
    r"\((?:extern-call\s+)?(nelisp_mirror_[a-z0-9_]+|nl_symcell_[a-z_]+|nelisp_fnv1a)\b")
DEFUN = re.compile(r"\(defun\s+([A-Za-z0-9_]+)\s")
SUBSYSTEMS = [
    ("env value/function cells", ("nelisp_env_",)),
    ("interpreter globals ops (setq, defvaralias, watchers)", ("nl_n3_",)),
    ("variable aliases", ("nl_pa_",)),
    ("specbind (shallow binding)", ("nl_sb_",)),
    ("bytecode VM", ("wf_bytecode_",)),
    ("native code (FX/N4)", ("m5_fx_", "nl_fx_", "bf_fx_", "nl_native_", "nl_n4_")),
    ("function cells (fset, install, apply)", ("nl_apply_", "nl_install_", "nl_ei_")),
    ("lexical frames (name hash)", ("nelisp_frame_", "nl_frame_")),
    ("builtins", ("bf_", "m5_sort_")),
    ("hashing (sxhash)", ("nl_jit_",)),
]


def source_files(rev=None):
    """Yield (relative path, text) of the native runtime sources."""
    if rev is None:
        paths = sorted(p.relative_to(ROOT).as_posix()
                       for p in ROOT.glob("lisp/nelisp-cc-*.el"))
        paths.append("scripts/nelisp-standalone-build.el")
        for rel in paths:
            yield rel, (ROOT / rel).read_text(errors="replace")
        return
    names = subprocess.run(["git", "-C", str(ROOT), "ls-tree", "--name-only", rev, "lisp/"],
                           check=True, capture_output=True, text=True).stdout.split()
    paths = sorted(n for n in names if re.match(r"lisp/nelisp-cc-.*\.el$", n))
    paths.append("scripts/nelisp-standalone-build.el")
    for rel in paths:
        text = subprocess.run(["git", "-C", str(ROOT), "show", f"{rev}:{rel}"],
                              check=True, capture_output=True).stdout.decode(errors="replace")
        yield rel, text


def subsystem(caller):
    for name, prefixes in SUBSYSTEMS:
        if caller.startswith(prefixes):
            return name
    return "other"


def census_static(rev=None):
    calls = collections.Counter()
    callers = collections.defaultdict(set)
    for rel, text in source_files(rev):
        if Path(rel).name.startswith("nelisp-cc-mirror-"):
            continue
        defs = [(m.start(), m.group(1)) for m in DEFUN.finditer(text)]
        starts = [d[0] for d in defs]
        for m in MIRROR_API.finditer(text):
            i = bisect.bisect_right(starts, m.start()) - 1
            if i < 0:
                continue          # a definition's own docstring/commentary
            caller = defs[i][1]
            if caller == m.group(1):
                continue
            group = subsystem(caller)
            calls[group] += 1
            callers[group].add(caller)
    rows = []
    for name, _ in SUBSYSTEMS + [("other", ())]:
        rows.append((name, len(callers[name]), calls[name]))
    return rows


def census_table(rows):
    lines = ["| subsystem | functions | call sites |", "|-+-+-|"]
    for name, nfun, ncall in rows:
        lines.append(f"| {name} | {nfun} | {ncall} |")
    lines.append(f"| total | {sum(r[1] for r in rows)} | {sum(r[2] for r in rows)} |")
    return lines


def cmd_census_static(args):
    rows = census_static(args[0] if args else None)
    print("\n".join(census_table(rows)))


# ---------------------------------------------------------------- S0.1 plan
GNU_REFS = re.compile(r"\b(lisp\.h|data\.c|eval\.c|lread\.c|alloc\.c|fns\.c):(\d+)\b")
REQUIRED_OPS = ["symbol-value", "set", "setq", "let", "let*", "defvar", "defconst",
                "defvaralias", "indirect-variable", "makunbound", "boundp",
                "default-value", "fboundp", "fset", "symbol-function", "fmakunbound",
                "intern", "intern-soft", "unintern", "obarray-make", "mapatoms",
                "symbol-plist", "get", "put", "add-variable-watcher"]


def doc_table(text, heading):
    """The rows of the first org table after HEADING, as lists of cells."""
    at = text.find(heading)
    need(at >= 0, f"Doc 226 lacks the heading {heading!r}")
    rows = []
    started = False
    for line in text[at:].splitlines()[1:]:
        if line.startswith("|"):
            started = True
            if not re.match(r"^\|[-+]+", line):
                rows.append([c.strip() for c in line.strip().strip("|").split("|")])
        elif started:
            break
    return rows


def cmd_plan(_args):
    need(DOC.is_file(), "missing " + str(DOC.relative_to(ROOT)))
    text = DOC.read_text()
    # Every GNU reference names an existing line of the 31.1 sources.
    refs = GNU_REFS.findall(text)
    files = {f for f, _ in refs}
    need({"lisp.h", "data.c", "eval.c", "lread.c"} <= files,
         "Doc 226 must cite lisp.h, data.c, eval.c and lread.c with line numbers")
    for f, line in refs:
        n = len((GNU_SRC / f).read_text(errors="replace").splitlines())
        need(1 <= int(line) <= n, f"{f}:{line} is outside the GNU 31.1 source")
    # The operation map covers every operation of the S1.3 fixture.
    ops = doc_table(text, "* Operation map")
    named = {row[0].strip("=~") for row in ops[1:] if row}
    missing = [op for op in REQUIRED_OPS if op not in named]
    need(not missing, "operation map lacks " + ", ".join(missing))
    for row in ops[1:]:
        need(len(row) >= 3 and GNU_REFS.search(row[1]),
             f"operation {row[0]} has no GNU file:line reference")
    # The census table equals the census recomputed at the recorded revision.
    expected = census_table(census_static(BEFORE_REV))
    got = doc_table(text, "* Mirror-table census")
    want = [[c.strip() for c in line.strip("|").split("|")]
            for line in expected if not line.startswith("|-")]
    need(got == want, "census table differs from census-static " + BEFORE_REV)
    need("* LOCALIZED interop" in text and "buffer-native" in text,
         "missing the LOCALIZED interop plan with buffer-native")
    print(f"PLAN PASS refs={len(refs)} ops={len(named)} census-rows={len(want) - 1}")


# ---------------------------------------------------------------- S1.1 layout
OBJSIZE = ROOT / "tools/ai/word-heap-objsize.el"
GNU_SYMBOL_BYTES = 48            # sizeof (struct Lisp_Symbol), lisp.h:786-829


def objsize(command, n):
    """Retained live bytes after N make-symbol calls (word-heap-objsize.el)."""
    env = dict(os.environ, WH_TYPE="symbol", WH_N=str(n))
    proc = subprocess.run(command, env=env, capture_output=True, preexec_fn=cap_memory,
                          timeout=600)
    m = re.search(rb"WH-OBJECT symbol (\d+) (-?\d+)", proc.stdout)
    need(proc.returncode == 0 and m, "objsize probe failed: " + " ".join(map(str, command)))
    return int(m.group(2))


def cmd_layout(_args):
    """Bytes per uninterned symbol (make-symbol slope, shared name) vs GNU."""
    # The per-symbol cost is the slope between two populations, which
    # removes the probe's constant overhead (word-heap S0 method).
    def slope(command):
        a, b = objsize(command, 4000), objsize(command, 16000)
        return (b - a) / 12000
    gnu = slope([GNU, "-Q", "--batch", "-l", str(OBJSIZE)])
    print(f"GNU 31.1: {gnu:.1f} B per make-symbol (sizeof struct Lisp_Symbol {GNU_SYMBOL_BYTES})")
    failures = []
    for kind, binary in head_builds().items():
        s = slope([str(binary), "--load", str(OBJSIZE)])
        print(f"{kind}: {s:.1f} B per make-symbol (slope N=4000..16000)")
        if s > GNU_SYMBOL_BYTES:
            failures.append(f"{kind} {s:.1f} B > {GNU_SYMBOL_BYTES} B")
    need(not failures, "; ".join(failures))
    print("LAYOUT PASS")


# ---------------------------------------------------------------- S3.1 final
def cmd_final(_args):
    """COMMON.md final verification evidence for builds of the current sources."""
    builds = head_builds()
    tag = "symbols-final"
    log = ROOT / "target/final-verify" / ("checks-" + tag)
    summary = OUT / "final" / "summary.json"
    need(summary.is_file(), "no final verification recorded (target/symbols/final/summary.json)")
    s = json.loads(summary.read_text())
    need(s.get("tree") == tree_hash(), "final verification is not from the current sources")
    need(Path(s["dyn"]).resolve() == builds["dyn"].resolve()
         and Path(s["static"]).resolve() == builds["static"].resolve(),
         "final verification used other binaries")
    text = (log / "summary.log").read_text() if (log / "summary.log").is_file() else s.get("gate", "")
    need("GATE-COUNT: 11 cases, 0 failures" in text, "COMMON checks: not 11 cases with 0 failures")
    need(s.get("mcp_tools") == 40, "MCP smoke did not list 40 tools")
    need(s.get("standalone_checks_failures") == 0, "standalone-checks failed")
    print("COMMON 11 groups, 0 failures; standalone-checks 0 failures; MCP 40 tools")
    # The criterion also names a wine smoke: tools/ai/win-smoke.sh builds a
    # Win64 PE from a git-archive snapshot of a commit and runs it under wine;
    # its smoke.log ends in "WIN-SMOKE PASS" (the retired run-win.sh printed
    # "WINE-SMOKE PASS").  wine.log is that log and wine.commit the commit
    # (target/win/<name>/commit): the commit's build inputs must be the
    # current ones.
    wine = OUT / "final" / "wine.log"
    lines = [l.strip() for l in wine.read_text().splitlines() if l.strip()] if wine.is_file() else []
    need(lines and lines[-1] == "WIN-SMOKE PASS",
         "wine smoke pending (tools/ai/win-smoke.sh smoke OUT; copy OUT/smoke.log to "
         "target/symbols/final/wine.log and OUT/commit to wine.commit)")
    commit_file = OUT / "final" / "wine.commit"
    need(commit_file.is_file(), "wine smoke has no recorded commit (target/symbols/final/wine.commit)")
    commit = commit_file.read_text().strip()
    need(tree_hash_of_rev(commit) == tree_hash(),
         f"wine smoke is not from the current sources (built from {commit[:9]})")
    print("WIN-SMOKE PASS from " + commit[:9])
    print("FINAL PASS")


# ---------------------------------------------------------------- S1.3 semantics
FIXTURE = ROOT / "test/standalone-symbols-check.el"
EXPECTED = ROOT / "test/standalone-symbols-check.expected"


def tree_hash():
    """Content hash of the build inputs (target/symbols/tree-hash.sh)."""
    files = subprocess.run(["git", "-C", str(ROOT), "ls-files", "-z", "lisp", "scripts", "src"],
                           capture_output=True, check=True).stdout.split(b"\0")
    lines = []
    for f in sorted(x for x in files if x):
        lines.append(f"{sha(ROOT / f.decode())}  {f.decode()}\n")
    return hashlib.sha256("".join(lines).encode()).hexdigest()


def tree_hash_of_rev(rev):
    """tree_hash() of the build inputs as committed at REV."""
    names = subprocess.run(["git", "-C", str(ROOT), "ls-tree", "-r", "-z", "--name-only", rev,
                            "lisp", "scripts", "src"], capture_output=True, check=True).stdout.split(b"\0")
    lines = []
    for f in sorted(x for x in names if x):
        data = subprocess.run(["git", "-C", str(ROOT), "show", f"{rev}:{f.decode()}"],
                              capture_output=True, check=True).stdout
        lines.append(f"{hashlib.sha256(data).hexdigest()}  {f.decode()}\n")
    return hashlib.sha256("".join(lines).encode()).hexdigest()


def head_builds():
    """The newest dynamic and static builds of the current build inputs."""
    tree = tree_hash()
    found = {}
    for kind in ("dyn", "static"):
        best = None
        for d in sorted((OUT / "bin").glob("*-" + kind)):
            h = d / "SOURCE-TREE"
            if not (d / "nelisp").is_file() or not h.is_file():
                continue
            if h.read_text().strip() != tree:
                continue
            if best is None or (d / "nelisp").stat().st_mtime > (best / "nelisp").stat().st_mtime:
                best = d
        need(best is not None, f"no {kind} build of the current sources ({tree[:12]})")
        found[kind] = best / "nelisp"
    return found


def fixture_lines(command):
    proc = subprocess.run(command, capture_output=True, preexec_fn=cap_memory, timeout=600)
    return proc.returncode, [l for l in proc.stdout.decode(errors="replace").splitlines()
                             if l.startswith("SYM")]


def cmd_semantics(_args):
    expected = EXPECTED.read_text().splitlines()
    rc, gnu = fixture_lines([GNU, "-Q", "--batch", "-l", str(FIXTURE)])
    need(rc == 0 and gnu == expected, "GNU 31.1 output differs from the .expected file")
    failures = []
    for kind, binary in head_builds().items():
        rc, got = fixture_lines([str(binary), "--load", str(FIXTURE)])
        bad = [e.split(" ", 2)[1] for e, g in zip(expected, got) if e != g]
        if len(got) != len(expected):
            bad.append(f"{len(got)}/{len(expected)} lines")
        print(f"{kind}: {len(expected) - len(bad)}/{len(expected)} lines equal GNU 31.1"
              + ("" if not bad else " -- differ: " + ", ".join(bad)))
        if bad:
            failures.append(kind)
    need(not failures, "fixture differs from GNU 31.1 on " + ", ".join(failures))
    print("SEMANTICS PASS")


# ---------------------------------------------------------------- S1.2 census
CENSUS = OUT / "census"
CENSUS_PATHS = ("plain-read", "plain-write", "plain-bind", "alias-read", "alias-write",
                "alias-bind", "fwd-read", "fwd-write", "fwd-bind", "uninterned-read")
CENSUS_TARGETS = ("nelisp_mirror_lookup_hashed", "nelisp_mirror_bucket_prepend_unchecked")
CENSUS_N = 1000


def callgrind_calls(path, targets):
    """Total calls into each target function in a callgrind profile."""
    names, counts = {}, collections.Counter()
    callee = None
    for line in Path(path).read_text(errors="replace").splitlines():
        if line.startswith(("fn=", "cfn=", "cfi=", "cob=")) is False and not line.startswith("calls="):
            continue
        if line.startswith("cfn="):
            m = re.match(r"cfn=\((\d+)\)(?: (.*))?", line)
            if m:
                if m.group(2):
                    names[m.group(1)] = m.group(2)
                callee = names.get(m.group(1))
            else:
                callee = line[4:]
        elif line.startswith("fn="):
            m = re.match(r"fn=\((\d+)\)(?: (.*))?", line)
            if m and m.group(2):
                names[m.group(1)] = m.group(2)
            callee = None
        elif line.startswith("calls=") and callee in targets:
            counts[callee] += int(line[6:].split()[0])
    return counts


def symbol_addresses(binary, names):
    """The callgrind function labels of NAMES: callgrind cannot read the
    runtime's ELF symbols, so it labels each function by its entry address."""
    out = subprocess.run(["nm", binary], capture_output=True, text=True, check=True).stdout
    found = {}
    for line in out.splitlines():
        parts = line.split()
        if len(parts) == 3 and parts[2] in names:
            found["0x%016x" % int(parts[0], 16)] = parts[2]
    need(set(found.values()) == set(names), "missing census symbols in " + str(binary))
    return found


def census_run(binary, path, n, tag):
    dest = CENSUS / tag
    dest.mkdir(parents=True, exist_ok=True)
    prefix = dest / f"{path}-{n}"
    command = ["taskset", "-c", CPU, "valgrind", "--tool=callgrind",
               f"--callgrind-out-file={prefix}.cg", binary, "--load",
               str(ROOT / "test/perf/symbols-census-probe.el")]
    proc = subprocess.run(command, env=dict(os.environ, SYMC_N=str(n), SYMC_PATH=path),
                          capture_output=True, preexec_fn=cap_memory, timeout=1800)
    Path(f"{prefix}.out").write_bytes(proc.stdout)
    Path(f"{prefix}.err").write_bytes(proc.stderr)
    need(proc.returncode == 0 and f"SYMC {path} {n} ".encode() in proc.stdout,
         f"census run {path} N={n} failed")
    labels = symbol_addresses(binary, CENSUS_TARGETS)
    counts = callgrind_calls(f"{prefix}.cg", set(labels) | set(CENSUS_TARGETS))
    result = collections.Counter()
    for label, count in counts.items():
        result[labels.get(label, label)] += count
    return result


def measure_census(binary, tag):
    rows = {}
    for path in CENSUS_PATHS:
        zero = census_run(binary, path, 0, tag)
        full = census_run(binary, path, CENSUS_N, tag)
        rows[path] = {t: (full[t] - zero[t]) / CENSUS_N for t in CENSUS_TARGETS}
    report = {"schema": 1, "tag": tag, "binary": pin(binary), "source": source_head(binary),
              "n": CENSUS_N, "probe": pin(ROOT / "test/perf/symbols-census-probe.el"),
              "per_iteration": rows}
    CENSUS.mkdir(parents=True, exist_ok=True)
    for name in ("census-" + tag + ".json", "census.json"):
        (CENSUS / name).write_text(json.dumps(report, indent=1) + "\n")
    return report


def cmd_census(args):
    if args and args[0] == "--measure":
        report = measure_census(args[1], args[2])
    else:
        need((CENSUS / "census.json").is_file(), "no census recorded")
        report = json.loads((CENSUS / "census.json").read_text())
        need(len(report["source"]) > 2 and report["source"][2] == tree_hash(),
             "census evidence is not from a build of the current sources")
    print("| path | hashed walks/iter | insertions/iter |")
    failures = []
    for path, row in report["per_iteration"].items():
        a, b = (row[t] for t in CENSUS_TARGETS)
        print(f"| {path} | {a:.3f} | {b:.3f} |")
        # Every path counts, the uninterned symbol's PLAINVAL value too
        # (ledger S1.2 NOTE: claimed only once uninterned symbols reach
        # their object without the hash).
        if a > 0.001 or b > 0.001:
            failures.append(path)
    # Negative control: the base build hashes on these paths, so the
    # census must count there (target/symbols/census/census-m0.json).
    base = CENSUS / "census-m0.json"
    need(base.is_file(), "missing the base census (negative control)")
    counted = [p for p, r in json.loads(base.read_text())["per_iteration"].items()
               if r["nelisp_mirror_lookup_hashed"] > 0.5]
    need(len(counted) >= 9, "the census does not count hashed walks on the base build")
    need(not failures, "mirror-table lookups on " + ", ".join(failures))
    print("CENSUS PASS")


# ---------------------------------------------------------------- S2 speed
# COMMON.md "Measurement": wall-clock, seven alternating rounds (candidate,
# GNU, base; reversed on odd rounds), pinned to the lane's CPU, warmed up,
# GC included, decided on per-round paired comparisons.  An own cost is
# (operation loop - same-shape trivial subr loop) / N in one process.
GNU = os.environ.get("SYMBOLS_GNU", "/usr/local/bin/emacs")
CPU = os.environ.get("SYMBOLS_CPU", "13")
ROUNDS = 7
SPEED = OUT / "speed"
OWN = {"symbol-value": ("symp-value", "symp-trivial1"),
       "set": ("symp-set", "symp-trivial2"),
       "boundp": ("symp-boundp", "symp-trivial1")}
# Iterations per engine and mode: long enough that each loop takes tenths
# of a second on that engine.
GLOBALS_N = {("gnu", "interpreter"): 2000000, ("gnu", "bytecode"): 5000000,
             ("nelisp", "interpreter"): 200000, ("nelisp", "bytecode"): 1000000}
MXLIB_N = 100010
BYTECODE_GEN = r"""
(let* ((src (getenv "SYM_SRC")) (out (getenv "SYM_OUT")) (forms nil))
  (setq byte-compile-warnings nil)
  (with-temp-buffer
    (insert-file-contents src)
    (goto-char (point-min))
    (condition-case nil
        (while t (push (read (current-buffer)) forms))
      (end-of-file nil)))
  (setq forms (nreverse forms))
  (load src nil t)
  (with-temp-file out
    (insert ";;; -*- lexical-binding: t -*-\n")
    (dolist (f forms)
      (cond
       ((eq (car-safe f) 'defun)
        (byte-compile (cadr f))
        (insert (format "(fset '%s %S)\n" (cadr f) (symbol-function (cadr f)))))
       ((memq (car-safe f) '(defvar defconst))
        (insert (format "%S\n" f)))))))
"""


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def pin(path):
    path = Path(path).resolve()
    return {"path": str(path), "sha256": sha(path)}


def cap_memory():
    resource.setrlimit(resource.RLIMIT_AS, (8388608 * 1024, 8388608 * 1024))


def cpu_khz():
    try:
        return int(Path(f"/sys/devices/system/cpu/cpu{CPU}/cpufreq/scaling_cur_freq").read_text())
    except OSError:
        return None


def bytecode_fixture(name, sources):
    """GNU 31.1 bytecode of SOURCES, printed as fset forms NeLisp reads."""
    SPEED.mkdir(parents=True, exist_ok=True)
    src = SPEED / (name + ".el")
    src.write_text(";;; -*- lexical-binding: t; -*-\n"
                   + "".join(Path(s).read_text() for s in sources))
    out = SPEED / (name + "-bytecode.el")
    proc = subprocess.run([GNU, "-Q", "--batch", "--eval", BYTECODE_GEN],
                          env=dict(os.environ, SYM_SRC=str(src), SYM_OUT=str(out)),
                          capture_output=True, preexec_fn=cap_memory, timeout=300)
    need(proc.returncode == 0 and out.is_file(), "GNU bytecode generation failed: "
         + proc.stderr.decode(errors="replace")[-400:])
    return out


def run_probe(engine, binary, probe, env, marker):
    command = ["taskset", "-c", CPU, binary]
    command += ["-Q", "--batch", "-l", str(probe)] if engine == "gnu" else ["--load", str(probe)]
    before = cpu_khz()
    proc = subprocess.run(command, env=dict(os.environ, **env), capture_output=True,
                          preexec_fn=cap_memory, timeout=3600)
    after = cpu_khz()
    line = next((l for l in proc.stdout.decode(errors="replace").splitlines()
                 if l.startswith(marker + " ")), None)
    need(proc.returncode == 0 and line, f"{engine} {binary} probe failed rc={proc.returncode}: "
         + proc.stderr.decode(errors="replace")[-300:])
    fields = line.split()
    times = dict(f.split("=", 1) for f in fields if "=" in f)
    return {"command": command, "env": env, "n": int(fields[1]), "khz": [before, after],
            "times": {k: float(v) for k, v in times.items()}}


def source_head(binary):
    """(HEAD, diff sha, build-input tree hash) recorded by build.sh."""
    d = Path(binary).resolve().parent
    def read(name, split=False):
        f = d / name
        if not f.is_file():
            return None
        text = f.read_text()
        return text.split()[0] if split else text.strip()
    return (read("SOURCE-HEAD"), read("SOURCE-DIFF-SHA", True), read("SOURCE-TREE"))


def measure_globals(cand, base, tag):
    probe = ROOT / "test/perf/symbols-globals-probe.el"
    functions = ROOT / "test/perf/symbols-globals-functions.el"
    bytecode = bytecode_fixture("globals", [functions])
    engines = {"candidate": cand, "gnu": GNU, "base": base}
    runs = []
    for r in range(ROUNDS):
        order = ["candidate", "gnu", "base"]
        if r % 2:
            order.reverse()
        for mode in ("interpreter", "bytecode"):
            for name in order:
                kind = "gnu" if name == "gnu" else "nelisp"
                env = {"SYMP_N": str(GLOBALS_N[(kind, mode)])}
                if mode == "bytecode":
                    env["SYMP_ELC"] = str(bytecode)
                elif kind == "nelisp":
                    env["NELISP_FX_DISABLE"] = "1"
                row = run_probe(kind, engines[name], probe, env, "SYMP")
                row.update(round=r, mode=mode, engine=name)
                runs.append(row)
    report = {"schema": 1, "tag": tag, "cpu": CPU, "rounds": ROUNDS,
              "binaries": {k: pin(v) for k, v in engines.items()},
              "source": {k: source_head(v) for k, v in (("candidate", cand), ("base", base))},
              "inputs": [pin(probe), pin(functions), pin(bytecode)], "runs": runs}
    SPEED.mkdir(parents=True, exist_ok=True)
    for name in ("globals-" + tag + ".json", "globals.json"):
        (SPEED / name).write_text(json.dumps(report, indent=1) + "\n")
    return report


def own_costs(report):
    """{(mode, op): {engine: [ns per round]}} and the workload per-call times."""
    own = collections.defaultdict(lambda: collections.defaultdict(list))
    for row in report["runs"]:
        t, n = row["times"], row["n"]
        for op, (loop, trivial) in OWN.items():
            own[(row["mode"], op)][row["engine"]].append((t[loop] - t[trivial]) / n * 1e9)
        own[(row["mode"], "call-global")][row["engine"]].append(t["symp-call-global"] / n * 1e9)
    return own


def globals_table(report):
    rows = []
    own = own_costs(report)
    for (mode, op), by in sorted(own.items()):
        c, g, b = by["candidate"], by["gnu"], by["base"]
        diff = statistics.median(x - y for x, y in zip(c, g))
        ratio_g = statistics.median(x / y for x, y in zip(c, g)) if min(g) > 0 else None
        ratio_b = statistics.median(x / y for x, y in zip(c, b)) if min(b) > 0 else None
        rows.append(dict(mode=mode, op=op, candidate=statistics.median(c),
                         gnu=statistics.median(g), base=statistics.median(b),
                         diff_gnu=diff, ratio_gnu=ratio_g, ratio_base=ratio_b))
    return rows


def measure_mxlib(cand, base, tag):
    corpus = OUT / "corpus/mxlib.el"
    need(corpus.is_file(), "missing target/symbols/corpus/mxlib.el (notes-65 copy)")
    loops = ROOT / "test/perf/symbols-mxlib-loops.el"
    probe = ROOT / "test/perf/symbols-mxlib-probe.el"
    bytecode = bytecode_fixture("mxlib", [corpus, loops])
    engines = {"candidate": cand, "gnu": GNU, "base": base}
    runs = []
    for r in range(ROUNDS):
        order = ["candidate", "gnu", "base"]
        if r % 2:
            order.reverse()
        for name in order:
            kind = "gnu" if name == "gnu" else "nelisp"
            row = run_probe(kind, engines[name], probe,
                            {"SYMX_N": str(MXLIB_N), "SYMP_ELC": str(bytecode)}, "SYMX")
            row.update(round=r, engine=name)
            runs.append(row)
    report = {"schema": 1, "tag": tag, "cpu": CPU, "rounds": ROUNDS,
              "binaries": {k: pin(v) for k, v in engines.items()},
              "source": {k: source_head(v) for k, v in (("candidate", cand), ("base", base))},
              "inputs": [pin(corpus), pin(loops), pin(probe), pin(bytecode)], "runs": runs}
    for name in ("mxlib-" + tag + ".json", "mxlib.json"):
        (SPEED / name).write_text(json.dumps(report, indent=1) + "\n")
    return report


def fmt(x):
    return "-" if x is None else f"{x:.3f}"


def current_build_ok(report):
    src = report["source"]["candidate"]
    return len(src) > 2 and src[2] == tree_hash()


def cmd_speed(args):
    need(args and args[0] in ("globals", "workloads"), "usage: speed globals|workloads [--measure CAND BASE TAG]")
    what = args[0]
    if len(args) > 1 and args[1] == "--measure":
        cand, base, tag = args[2:5]
        if what == "globals":
            report = measure_globals(cand, base, tag)
        else:
            report = measure_mxlib(cand, base, tag)
    else:
        name = "globals.json" if what == "globals" else "mxlib.json"
        need((SPEED / name).is_file(), f"no {what} measurement recorded")
        report = json.loads((SPEED / name).read_text())
        need(current_build_ok(report), f"{what} evidence is not from a build of the current sources")
    failures = []
    if what == "globals":
        print("| mode | op | cand ns | GNU ns | base ns | median(cand-GNU) | cand/GNU | cand/base |")
        for row in globals_table(report):
            if row["op"] == "call-global":
                continue
            print(f"| {row['mode']} | {row['op']} | {fmt(row['candidate'])} | {fmt(row['gnu'])} | "
                  f"{fmt(row['base'])} | {fmt(row['diff_gnu'])} | {fmt(row['ratio_gnu'])} | {fmt(row['ratio_base'])} |")
            if row["diff_gnu"] > 0:
                failures.append(f"{row['mode']}/{row['op']} own cost {row['candidate']:.2f} ns > GNU {row['gnu']:.2f} ns")
    else:
        need((SPEED / "globals.json").is_file(), "no globals measurement (call-global) recorded")
        greport = json.loads((SPEED / "globals.json").read_text())
        rows = [r for r in globals_table(greport) if r["op"] == "call-global" and r["mode"] == "interpreter"]
        times = collections.defaultdict(lambda: collections.defaultdict(list))
        for row in report["runs"]:
            for loop, sec in row["times"].items():
                times[loop][row["engine"]].append(sec / row["n"] * 1e9)
        print("| workload | cand ns/call | GNU ns/call | base ns/call | cand/base | cand/GNU |")
        for loop, by in sorted(times.items()):
            rb = statistics.median(x / y for x, y in zip(by["candidate"], by["base"]))
            rg = statistics.median(x / y for x, y in zip(by["candidate"], by["gnu"]))
            print(f"| {loop} (bytecode) | {fmt(statistics.median(by['candidate']))} | "
                  f"{fmt(statistics.median(by['gnu']))} | {fmt(statistics.median(by['base']))} | {fmt(rb)} | {fmt(rg)} |")
            if rb >= 1.0:
                failures.append(f"{loop} cand/base {rb:.3f} >= 1.0")
        for row in rows:
            print(f"| call-global (interpreter) | {fmt(row['candidate'])} | {fmt(row['gnu'])} | "
                  f"{fmt(row['base'])} | {fmt(row['ratio_base'])} | {fmt(row['ratio_gnu'])} |")
            if row["ratio_base"] is None or row["ratio_base"] >= 1.0:
                failures.append(f"call-global cand/base {fmt(row['ratio_base'])} >= 1.0")
    need(not failures, "; ".join(failures))
    print(f"SPEED {what.upper()} PASS")


def not_yet(name):
    def run(_args):
        need(False, f"{name}: implementation or acceptance evidence outstanding")
    return run


COMMANDS = {
    "plan": cmd_plan,
    "census-static": cmd_census_static,
    "layout": cmd_layout,
    "census": cmd_census,
    "semantics": cmd_semantics,
    "speed": cmd_speed,
    "final": cmd_final,
}


def main():
    if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
        print(__doc__)
        sys.exit(2)
    COMMANDS[sys.argv[1]](sys.argv[2:])


if __name__ == "__main__":
    main()
